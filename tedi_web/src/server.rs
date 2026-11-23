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

use color_eyre::eyre::{Result, WrapErr as _, bail, eyre};
use dioxus::server::{
	DioxusRouterExt as _, FullstackState,
	axum::{self, Router, body::Bytes, routing::post},
	http::{HeaderMap, StatusCode},
};
use dioxus_fullstack::ServerEvents;
use tedi_core::{
	IssueLink, RepoInfo, TaskView,
	board::{Board, Scope, View, scope_at},
	conflict::{Conflict, FieldEdit},
	workflow::State,
};
use tedi_task_operations::{
	open_interactions::SyncMode,
	workflow::{MovedOn, Mover, Rejected, Workflows},
};
use tokio::sync::{Mutex, broadcast, mpsc};

use crate::api::{Dock, Edited, Favorite, Kept, Library, LibraryEdit, Setup, Shelf, Sprint};

pub struct Settings {
	pub addr: SocketAddr,
	/// The client bundle (`dx bundle` output); without it only the API is served, as under `dx serve`.
	pub public: Option<PathBuf>,
	pub mode: SyncMode,
	pub workflows: Workflows,
	pub views: Vec<View>,
	pub scopes: BTreeMap<String, Scope>,
	pub columns: BTreeMap<String, crate::api::Columns>,
	/// Where the sprint milestones live.
	pub milestones: Option<RepoInfo>,
	/// Owners and `owner/repo`s: what the periodic pull pulls, and the only repos a webhook delivery is pulled for.
	pub watch: Vec<String>,
	/// Pull all of `watch` this often, catching whatever no webhook announced.
	pub pull_every: Option<Duration>,
	/// Mounts `/webhook`, checking each delivery's `X-Hub-Signature-256` against it.
	pub webhook_secret: Option<String>,
	pub viewer: Viewer,
}

/// Whose library a request reads and edits.
pub enum Viewer {
	/// Behind a gate naming the signed-in user in this header; a request without it is refused.
	Header(String),
	/// Ungated: whoever runs this instance, as its GitHub user.
	Operator,
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

pub fn changes() -> ServerEvents<()> {
	use tokio_stream::StreamExt as _;
	// a lagging board missed ticks, which still says the same thing: reload
	ServerEvents::from_stream(tokio_stream::wrappers::BroadcastStream::new(CHANGES.subscribe()).map(|_| Ok(())))
}

pub async fn setup() -> Result<Setup> {
	let s = settings();
	Ok(Setup {
		me: editor(),
		workflows: s.workflows.iter().map(|(name, w)| (name.clone(), w.workflow.clone())).collect(),
		views: s.views.clone(),
		scopes: s.scopes.clone(),
		columns: s.columns.clone(),
		sprints: match s.milestones {
			Some(repo) => sprints(repo).await?,
			None => Vec::new(),
		},
	})
}

pub async fn board() -> Result<Arc<Board>> {
	let mut cached = BOARD.lock().await;
	if let Some(board) = &*cached {
		return Ok(board.clone());
	}
	let board = Arc::new(tedi_task_operations::board::load(&settings().workflows).await?);
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

pub async fn move_card(link: IssueLink, workflow: String, to: State, seen: State) -> Result<Edited> {
	let Some(me) = editor() else {
		bail!("who moves it is unknown: this instance never learned its GitHub user");
	};
	let s = settings();
	let moved = tedi_task_operations::workflow::move_card(&link, &workflow, &to, Some(&seen), &Mover::Person(me), &s.workflows, s.mode, jiff::Timestamp::now()).await;
	changed().await; // a failed move may still have pulled
	match moved {
		Ok(()) => Ok(Edited::Landed),
		Err(e) => match e.downcast::<Conflict>() {
			Ok(conflict) => Ok(Edited::Conflict(conflict)),
			Err(e) if e.downcast_ref::<Rejected>().is_some() || e.downcast_ref::<MovedOn>().is_some() => Ok(Edited::Rejected(e.to_string())),
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

pub async fn library(headers: &HeaderMap) -> Result<Library> {
	let key = viewer(headers)?;
	let _held = LIBRARIES.lock().await;
	read_library(&key)
}

pub async fn edit_library(headers: &HeaderMap, edit: LibraryEdit) -> Result<Library> {
	let key = viewer(headers)?;
	let _held = LIBRARIES.lock().await;
	let mut library = read_library(&key)?;
	apply(&mut library, edit)?;
	let file = library_file(&key);
	let fresh = file.with_extension("json.new");
	std::fs::write(&fresh, serde_json::to_string_pretty(&library)?)?;
	std::fs::rename(&fresh, &file)?; // whole or not at all: a crash mid-write keeps the last library
	Ok(library)
}

static SETTINGS: OnceLock<Settings> = OnceLock::new();
static CHANGES: LazyLock<broadcast::Sender<()>> = LazyLock::new(|| broadcast::channel(16).0);
/// Loaded once per change rather than once per open board.
static BOARD: Mutex<Option<Arc<Board>>> = Mutex::const_new(None);
/// Held across each library's read-modify-write.
static LIBRARIES: Mutex<()> = Mutex::const_new(());

fn settings() -> &'static Settings {
	SETTINGS.get().expect("set by `serve` before the router exists")
}

/// Runs every pull, one at a time: what a webhook announced, and the whole `watch` every `pull_every`.
/// Owned by `serve`, so a client hanging up never cancels a pull midway.
async fn puller(mut announced: mpsc::UnboundedReceiver<String>) -> Result<()> {
	let mut every = settings().pull_every.map(tokio::time::interval);
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
			_ = async { every.as_mut().expect("guarded").tick().await }, if every.is_some() => settings().watch.clone(),
		};
		if let Err(e) = tedi_task_operations::pull::pull_repos(&targets, &settings().workflows, jiff::Timestamp::now()).await {
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
			let owner = repo.full_name.split_once('/').map_or(repo.full_name.as_str(), |(owner, _)| owner);
			if !settings().watch.iter().any(|w| w.eq_ignore_ascii_case(owner) || w.eq_ignore_ascii_case(&repo.full_name)) {
				return StatusCode::NO_CONTENT; // the App is public: anyone's install delivers here
			}
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

/// Who edits: the instance's GitHub user until editors sign in, then the request's.
fn editor() -> Option<String> {
	tedi_task_operations::current_user::get()
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

/// The viewer's key, as their library's file stem.
fn viewer(headers: &HeaderMap) -> Result<String> {
	let key = match &settings().viewer {
		Viewer::Header(name) => headers
			.get(name)
			.ok_or_else(|| eyre!("the request carries no `{name}`: the gate in front of this board names the signed-in user in it"))?
			.to_str()?
			.to_string(),
		Viewer::Operator => editor().ok_or_else(|| eyre!("a library belongs to a viewer, and this ungated instance doesn't know its GitHub user"))?,
	};
	let plain = !key.is_empty() && !key.starts_with('.') && key.chars().all(|c| c.is_ascii_alphanumeric() || "._@[]-".contains(c));
	if !plain {
		bail!("{key:?} is not a viewer this board keeps a library for");
	}
	Ok(key)
}

fn library_file(key: &str) -> PathBuf {
	tedi_task_operations::paths::data_dir("web").join(format!("{key}.json"))
}

fn read_library(key: &str) -> Result<Library> {
	let file = library_file(key);
	match std::fs::read_to_string(&file) {
		Ok(json) => serde_json::from_str(&json).wrap_err_with(|| format!("{} doesn't read as a library", file.display())),
		Err(e) if e.kind() == std::io::ErrorKind::NotFound => Ok(Library::default()), // a viewer who never kept anything
		Err(e) => Err(e.into()),
	}
}

fn apply(library: &mut Library, edit: LibraryEdit) -> Result<()> {
	match edit {
		LibraryEdit::Arranged { dock: Dock::Tiles, band, layout } => _ = library.on_screen.tiles.insert(band, layout),
		LibraryEdit::Arranged {
			dock: Dock::Columns { view, by },
			band,
			layout,
		} => {
			library.on_screen.columns.entry(view).or_default().entry(by).or_default().insert(band, layout);
		}
		LibraryEdit::Viewed(view) => _ = library.on_screen.views.insert(view.name.clone(), view),
		LibraryEdit::Tinted { view, by, tints } => _ = library.on_screen.tints.entry(view).or_default().insert(by, tints),
		LibraryEdit::Keep(Kept::Filter(view)) => {
			if view.name.trim().is_empty() {
				bail!("a filter is kept under a name");
			}
			if team_view(&view.name) {
				bail!("{} is a team view's name: keep yours under another", view.name);
			}
			library.filters.insert(view.name.clone(), view);
		}
		LibraryEdit::Keep(Kept::Workspace { name, band, layout }) => _ = library.workspaces.entry(named(name)?).or_default().insert(band, layout),
		LibraryEdit::Keep(Kept::Columns { name, by, band, layout, tints }) => {
			if settings().columns.contains_key(&name) {
				bail!("{name} are the team's columns: keep yours under another name");
			}
			let kept = library.columns.entry(named(name.clone())?).or_insert_with(|| crate::api::Columns {
				by: by.clone(),
				layout: BTreeMap::new(),
				tints: BTreeMap::new(),
			});
			if kept.by != by {
				bail!("the columns {name} are by {}, not {by}", kept.by);
			}
			kept.layout.insert(band, layout);
			kept.tints = tints;
		}
		LibraryEdit::Keep(Kept::Scope { name, scope }) => {
			let name = named(name)?;
			if name.contains(" / ") {
				bail!("` / ` joins a scope's path: name it without one");
			}
			if settings().scopes.contains_key(&name) {
				bail!("{name} is a team scope's name: keep yours under another");
			}
			if scope.repos.is_empty() && scope.children.is_empty() {
				bail!("the scope {name} names no repo");
			}
			library.scopes.insert(name, scope);
		}
		LibraryEdit::Rename { shelf, from, to } => {
			let to = named(to)?;
			let taken = match shelf {
				Shelf::Filters => library.filters.contains_key(&to) || team_view(&to),
				Shelf::Workspaces => library.workspaces.contains_key(&to),
				Shelf::Columns => library.columns.contains_key(&to) || settings().columns.contains_key(&to),
				Shelf::Scopes => library.scopes.contains_key(&to) || settings().scopes.contains_key(&to) || to.contains(" / "),
			};
			if taken {
				bail!("{to} is taken");
			}
			let missing = || eyre!("{from} is not kept");
			match shelf {
				Shelf::Filters => {
					let mut view = library.filters.remove(&from).ok_or_else(missing)?;
					view.name = to.clone();
					library.filters.insert(to.clone(), view);
					if let Some(columns) = library.on_screen.columns.remove(&from) {
						library.on_screen.columns.insert(to.clone(), columns);
					}
					if let Some(tints) = library.on_screen.tints.remove(&from) {
						library.on_screen.tints.insert(to.clone(), tints);
					}
					if let Some(mut shown) = library.on_screen.views.remove(&from) {
						shown.name = to.clone();
						library.on_screen.views.insert(to.clone(), shown);
					}
				}
				Shelf::Workspaces => {
					let kept = library.workspaces.remove(&from).ok_or_else(missing)?;
					library.workspaces.insert(to.clone(), kept);
				}
				Shelf::Columns => {
					let kept = library.columns.remove(&from).ok_or_else(missing)?;
					library.columns.insert(to.clone(), kept);
				}
				Shelf::Scopes => {
					let kept = library.scopes.remove(&from).ok_or_else(missing)?;
					library.scopes.insert(to.clone(), kept);
					if let Some(path) = &library.scope
						&& let Some(rest) = within(path, &from)
					{
						library.scope = Some(format!("{to}{rest}"));
					}
				}
			}
			if library.favorites.remove(&favorite(shelf, from)) {
				library.favorites.insert(favorite(shelf, to));
			}
		}
		LibraryEdit::Drop { shelf, name } => {
			let dropped = match shelf {
				Shelf::Filters => library.filters.remove(&name).is_some(),
				Shelf::Workspaces => library.workspaces.remove(&name).is_some(),
				Shelf::Columns => library.columns.remove(&name).is_some(),
				Shelf::Scopes => library.scopes.remove(&name).is_some(),
			};
			if shelf == Shelf::Scopes && library.scope.as_deref().is_some_and(|p| within(p, &name).is_some()) {
				library.scope = None;
			}
			if !dropped {
				bail!("{name} is not kept");
			}
			library.favorites.remove(&favorite(shelf, name));
		}
		LibraryEdit::Scope(path) => {
			if let Some(path) = &path
				&& scope_at(&library.scopes(&settings().scopes), path).is_none()
			{
				bail!("no scope {path}");
			}
			library.scope = path;
		}
		LibraryEdit::Favorite { what, on: false } =>
			if !library.favorites.remove(&what) {
				bail!("{what:?} is not a favorite");
			},
		LibraryEdit::Favorite { what, on: true } => {
			let known = match &what {
				Favorite::View(name) => library.filters.contains_key(name) || team_view(name),
				Favorite::Scope(path) => scope_at(&library.scopes(&settings().scopes), path).is_some(),
				Favorite::Workspace(name) => library.workspaces.contains_key(name),
				Favorite::Columns(name) => library.columns.contains_key(name) || settings().columns.contains_key(name),
			};
			if !known {
				bail!("no {what:?} to favorite");
			}
			library.favorites.insert(what);
		}
	}
	Ok(())
}

fn team_view(name: &str) -> bool {
	settings().views.iter().any(|v| v.name == name)
}

fn named(name: String) -> Result<String> {
	match name.trim().is_empty() {
		true => bail!("kept under a name, and this one is blank"),
		false => Ok(name),
	}
}

fn favorite(shelf: Shelf, name: String) -> Favorite {
	match shelf {
		Shelf::Filters => Favorite::View(name),
		Shelf::Workspaces => Favorite::Workspace(name),
		Shelf::Columns => Favorite::Columns(name),
		Shelf::Scopes => Favorite::Scope(name),
	}
}

/// What follows `top` in `path` when `path` is `top` or nested in it.
fn within<'a>(path: &'a str, top: &str) -> Option<&'a str> {
	path.strip_prefix(top).filter(|rest| rest.is_empty() || rest.starts_with(" / "))
}
