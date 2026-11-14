//! `tedi web`: the server fns' bodies, a GitHub App webhook, the periodic pull, and the client bundle.
//! One process serves one store; every write goes through `tedi_task_operations`, which takes the store
//! lock itself.

use std::{
	collections::BTreeMap,
	net::SocketAddr,
	path::PathBuf,
	sync::{Arc, LazyLock, OnceLock},
	time::Duration,
};

use color_eyre::eyre::{Result, bail, eyre};
use dioxus::server::{
	DioxusRouterExt as _, FullstackState,
	axum::{self, Router, body::Bytes, routing::post},
	http::{HeaderMap, StatusCode},
};
use dioxus_fullstack::ServerEvents;
use tedi_core::{
	IssueLink, RepoInfo, TaskView,
	board::{Board, Scope, View},
	conflict::{Conflict, FieldEdit},
};
use tedi_task_operations::open_interactions::SyncMode;
use tokio::sync::{Mutex, broadcast, mpsc};

use crate::api::{Edited, Setup, Sprint};

pub struct Settings {
	pub addr: SocketAddr,
	/// The client bundle (`dx bundle` output); without it only the API is served, as under `dx serve`.
	pub public: Option<PathBuf>,
	pub mode: SyncMode,
	pub archive_after: jiff::SignedDuration,
	pub views: Vec<View>,
	pub scopes: BTreeMap<String, Scope>,
	/// Where the sprint milestones live.
	pub milestones: Option<RepoInfo>,
	/// What to pull how often, catching whatever no webhook announced.
	pub pull: Option<(Duration, Vec<String>)>,
	/// Mounts `/webhook`, checking each delivery's `X-Hub-Signature-256` against it.
	pub webhook_secret: Option<String>,
}

static SETTINGS: OnceLock<Settings> = OnceLock::new();
static CHANGES: LazyLock<broadcast::Sender<()>> = LazyLock::new(|| broadcast::channel(16).0);
/// Loaded once per change rather than once per open board.
static BOARD: Mutex<Option<Arc<Board>>> = Mutex::const_new(None);

fn settings() -> &'static Settings {
	SETTINGS.get().expect("set by `serve` before the router exists")
}

pub async fn serve(config: Settings) -> Result<()> {
	use tower_http::services::{ServeDir, ServeFile};

	let addr = config.addr;
	SETTINGS.set(config).map_err(|_| eyre!("one server per process"))?;
	let (announce, announced) = mpsc::unbounded_channel::<String>();

	let mut router = Router::new().register_server_functions().with_state(FullstackState::headless());
	if settings().webhook_secret.is_some() {
		let announce = announce.clone();
		router = router.route("/webhook", post(move |headers: HeaderMap, body: Bytes| webhook(headers, body, announce.clone())));
	}
	if let Some(public) = &settings().public {
		router = router.fallback_service(ServeDir::new(public).fallback(ServeFile::new(public.join("index.html"))));
	}
	let listener = tokio::net::TcpListener::bind(addr).await?;
	tracing::info!("serving the board on {addr}");
	tokio::try_join!(async { Ok(axum::serve(listener, router).await?) }, puller(announced))?;
	drop(announce); // held until here, so the puller's channel never closes under it
	Ok(())
}

/// Runs every pull, one at a time: what a webhook announced, and the whole `watch` every `pull_every`.
/// Owned by `serve`, so a client hanging up never cancels a pull midway.
async fn puller(mut announced: mpsc::UnboundedReceiver<String>) -> Result<()> {
	let mut every = settings().pull.as_ref().map(|(every, _)| tokio::time::interval(*every));
	loop {
		let targets = tokio::select! {
			repo = announced.recv() => {
				let mut repos = vec![repo.expect("`serve` holds a sender until it returns")];
				while let Ok(more) = announced.try_recv() {
					repos.push(more);
				}
				repos.sort();
				repos.dedup();
				repos
			}
			_ = async { every.as_mut().expect("guarded").tick().await }, if every.is_some() => settings().pull.as_ref().expect("ticks only with it").1.clone(),
		};
		if let Err(e) = tedi_task_operations::pull::pull_repos(&targets).await {
			tracing::warn!("pull of {targets:?}: {e:#}"); // GitHub unreachable or an issue unstorable: the cursor holds, the next pull retries
		}
		changed().await;
	}
}

async fn webhook(headers: HeaderMap, body: Bytes, announce: mpsc::UnboundedSender<String>) -> StatusCode {
	use hmac::Mac as _;

	let secret = settings().webhook_secret.as_ref().expect("mounted only with a secret");
	let Some(signature) = headers
		.get("x-hub-signature-256")
		.and_then(|v| v.to_str().ok())
		.and_then(|v| v.strip_prefix("sha256="))
		.and_then(|v| hex::decode(v).ok())
	else {
		return StatusCode::UNAUTHORIZED;
	};
	let mut mac = hmac::Hmac::<sha2::Sha256>::new_from_slice(secret.as_bytes()).expect("HMAC takes a key of any length");
	mac.update(&body);
	if mac.verify_slice(&signature).is_err() {
		return StatusCode::UNAUTHORIZED;
	}

	#[derive(serde::Deserialize)]
	struct Delivery {
		repository: Option<Repository>,
	}
	#[derive(serde::Deserialize)]
	struct Repository {
		full_name: String,
	}
	match serde_json::from_slice::<Delivery>(&body) {
		Ok(Delivery { repository: Some(repo) }) => {
			announce.send(repo.full_name).expect("the puller lives as long as the server");
			StatusCode::ACCEPTED
		}
		Ok(Delivery { repository: None }) => StatusCode::NO_CONTENT, // an org- or app-level event: no repo's issues moved
		Err(_) => StatusCode::BAD_REQUEST,
	}
}

async fn changed() {
	*BOARD.lock().await = None;
	let _ = CHANGES.send(()); // errs only while no board is open: nobody to tell
}

pub fn changes() -> ServerEvents<()> {
	use tokio_stream::StreamExt as _;
	// a lagging board missed ticks, which still says the same thing: reload
	ServerEvents::from_stream(tokio_stream::wrappers::BroadcastStream::new(CHANGES.subscribe()).map(|_| Ok(())))
}

/// Who edits: the instance's GitHub user until editors sign in, then the request's.
fn editor() -> Option<String> {
	tedi_task_operations::current_user::get()
}

pub async fn setup() -> Result<Setup> {
	let s = settings();
	let me = editor();
	Ok(Setup {
		personal: match &me {
			Some(me) => personal_views(me)?,
			None => Vec::new(),
		},
		me,
		team: s.views.clone(),
		scopes: s.scopes.clone(),
		sprints: match s.milestones {
			Some(repo) => sprints(repo).await?,
			None => Vec::new(),
		},
	})
}

async fn sprints(repo: RepoInfo) -> Result<Vec<Sprint>> {
	let milestones = tedi_task_operations::github::client::get()?.list_milestones(repo).await?;
	Ok(milestones
		.into_iter()
		.filter_map(|m| {
			let view = TaskView::parse(m.description.as_deref()?);
			Some(Sprint {
				title: m.title,
				links: view.issue_links(),
			})
		})
		.collect())
}

pub async fn board() -> Result<Arc<Board>> {
	let mut cached = BOARD.lock().await;
	if let Some(board) = &*cached {
		return Ok(board.clone());
	}
	let board = Arc::new(tedi_task_operations::board::load(settings().archive_after, jiff::Timestamp::now()).await?);
	*cached = Some(board.clone());
	Ok(board)
}

pub async fn edit(link: IssueLink, edits: Vec<FieldEdit>) -> Result<Edited> {
	let edited = tedi_task_operations::board::edit(&link, edits, settings().mode).await;
	changed().await; // a failed edit may still have pulled
	match edited {
		Ok(()) => Ok(Edited::Landed),
		Err(e) => match e.downcast::<Conflict>() {
			Ok(conflict) => Ok(Edited::Conflict(conflict)),
			Err(e) => Err(e),
		},
	}
}

pub async fn pending_conflict(link: &IssueLink) -> Result<Option<String>> {
	match tedi_task_operations::board::pending_conflict(link).await? {
		Some(path) => Ok(Some(std::fs::read_to_string(path)?)),
		None => Ok(None),
	}
}

pub async fn resolve(link: &IssueLink, resolution: String) -> Result<Option<String>> {
	let Some(path) = tedi_task_operations::board::pending_conflict(link).await? else {
		bail!("nothing is pending on {link}: someone resolved it first");
	};
	std::fs::write(&path, resolution)?;
	let still = pending_conflict(link).await;
	changed().await;
	still
}

fn views_file(me: &str) -> PathBuf {
	tedi_task_operations::paths::data_dir("web").join(format!("{me}.json"))
}

fn personal_views(me: &str) -> Result<Vec<View>> {
	match std::fs::read_to_string(views_file(me)) {
		Ok(json) => Ok(serde_json::from_str(&json)?),
		Err(e) if e.kind() == std::io::ErrorKind::NotFound => Ok(Vec::new()),
		Err(e) => Err(e.into()),
	}
}

fn write_views(me: &str, views: &[View]) -> Result<()> {
	std::fs::write(views_file(me), serde_json::to_string_pretty(views)?)?;
	Ok(())
}

fn signed_in() -> Result<String> {
	editor().ok_or_else(|| eyre!("personal views belong to an editor, and this instance doesn't know its GitHub user"))
}

pub fn save_view(view: View) -> Result<()> {
	let me = signed_in()?;
	let mut views = personal_views(&me)?;
	match views.iter_mut().find(|v| v.name == view.name) {
		Some(old) => *old = view,
		None => views.push(view),
	}
	write_views(&me, &views)
}

pub fn delete_view(name: &str) -> Result<()> {
	let me = signed_in()?;
	let mut views = personal_views(&me)?;
	views.retain(|v| v.name != name);
	write_views(&me, &views)
}
