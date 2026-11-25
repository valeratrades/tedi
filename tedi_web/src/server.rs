//! `tedi web`: the server fns' bodies, a GitHub App webhook, the periodic pull, and the client bundle.
//! One process serves one store; every write goes through `tedi_task_operations`, which takes the store
//! lock itself.

mod accounts;
mod migrate;

use std::{
	collections::{BTreeMap, BTreeSet, HashMap},
	net::SocketAddr,
	path::PathBuf,
	sync::{Arc, LazyLock, OnceLock},
	time::Duration,
};

use accounts::Caller;
pub use accounts::{account, edit_account};
use color_eyre::eyre::{Result, WrapErr as _, bail, eyre};
use dioxus::server::{
	DioxusRouterExt as _, FullstackState,
	axum::{self, Router, body::Bytes, routing::post},
	http::{HeaderMap, StatusCode},
};
use dioxus_fullstack::ServerEvents;
use futures::{Stream, StreamExt as _};
use tedi_core::{
	IssueLink, MilestoneLink, RepoInfo,
	board::{Answer, Board, GroupBy, Hit, Holds, Listing, Part, Query, Scope, Source as Held, Thread, scope_at},
	conflict::{CommentEdit, Conflict, Edited, FieldEdit},
	workflow::State,
};
use tedi_task_operations::{
	board::Reading,
	github::client,
	open_interactions::SyncMode,
	reach::{Reach, Reached},
	workflow::{MovedOn, Mover, Rejected, Workflows},
};
use tokio::sync::{Mutex, Notify, OnceCell, broadcast, mpsc};

use crate::api::{
	Catalog, Change, Construction, Entry as Cataloged, Fork, Forked, Item, Kept, Library, LibraryEdit, Onto, Origin, Owner, Setup, ShareLink, Shared, Shelf, Upstream, Usage, Version, View,
	ViewPart, Workspace, diff,
};

/// Each team object's versions, oldest first, with what each holds: a version per commit that changed it.
type Versions = BTreeMap<Item, Vec<(Version, Construction)>>;

pub struct Settings {
	pub addr: SocketAddr,
	/// The client bundle (`dx bundle` output); without it only the API is served, as under `dx serve`.
	pub public: Option<PathBuf>,
	pub mode: SyncMode,
	pub workflows: Workflows,
	pub views: Vec<View>,
	pub scopes: BTreeMap<String, Scope>,
	pub columns: BTreeMap<String, crate::api::Columns>,
	pub workspaces: BTreeMap<String, Workspace>,
	pub source: Option<Source>,
	/// Owners and `owner/repo`s: what the periodic pull pulls, and the only repos a webhook delivery is pulled for.
	pub watch: Vec<String>,
	/// Pull all of `watch` this often, catching whatever no webhook announced.
	pub pull_every: Option<Duration>,
	/// Mounts `/webhook`, checking each delivery's `X-Hub-Signature-256` against it.
	pub webhook_secret: Option<String>,
	pub viewer: Viewer,
}

/// Where the team's config lives in git.
#[derive(Clone, Debug, serde::Deserialize, serde::Serialize)]
pub struct Source {
	/// `owner/repo`
	pub repo: String,
	pub path: String,
	pub branch: String,
}

/// Whose library a request reads and edits.
pub enum Viewer {
	/// Behind a gate naming the signed-in user in this header; a request without it is refused.
	Header(String),
	/// Ungated: whoever runs this instance, as its GitHub user.
	Operator,
}

pub async fn serve(config: Settings) -> Result<()> {
	use dioxus::server::axum::{
		extract::Request,
		middleware::{Next, from_fn},
	};
	use tower_http::services::{ServeDir, ServeFile};

	let addr = config.addr;
	SETTINGS.set(config).map_err(|_| eyre!("one server per process"))?;
	migrate::migrate(settings()).await?;
	//UNBOUNDED announcements are repo names, deduplicated by the puller
	let (announce, announced) = mpsc::unbounded_channel::<Pull>();
	ANNOUNCE.set(announce).map_err(|_| eyre!("one server per process"))?;

	let mut router = Router::new().register_server_functions().with_state(FullstackState::headless());
	if settings().webhook_secret.is_some() {
		router = router.route("/webhook", post(webhook));
	}
	if let Some(public) = &settings().public {
		let files = ServeDir::new(public).precompressed_br().fallback(ServeFile::new(public.join("index.html")));
		let served = Router::new().fallback_service(files).layer(from_fn(async |request: Request, next: Next| {
			let file = public.join(request.uri().path().trim_start_matches('/'));
			let mut response = next.run(request).await;
			// the client's length uncompressed, which its Content-Length isn't
			if response.status().is_success()
				&& file.extension().is_some_and(|e| e == "wasm")
				&& let Ok(m) = std::fs::metadata(&file)
			// the page itself, served in place of a file there isn't
			{
				response.headers_mut().insert("x-bytes", m.len().into());
			}
			response
		}));
		router = router.fallback_service(served);
	}
	let listener = tokio::net::TcpListener::bind(addr).await?;
	tracing::info!("serving the board on {addr}");
	tokio::try_join!(async { Ok(axum::serve(listener, router).await?) }, puller(announced), Box::pin(reader()))?; // main polls this whole future off its stack
	Ok(())
}

/// Every watched repo and the viewer's own pulled now, after any pull already running.
pub fn refresh(headers: &HeaderMap) -> Result<()> {
	let key = viewer(headers)?;
	for w in &settings().watch {
		announce(Pull::Watched(w.clone()));
	}
	announce(Pull::Account(key));
	Ok(())
}

pub fn changes() -> ServerEvents<()> {
	// a lagging board missed ticks, which still says the same thing: reload
	ServerEvents::from_stream(tokio_stream::wrappers::BroadcastStream::new(CHANGES.subscribe()).map(|_| Ok(())))
}

pub async fn setup(headers: &HeaderMap) -> Result<Setup> {
	let s = settings();
	Ok(Setup {
		me: Caller::of(headers)?.act(async { client::login() }).await,
		accounts: matches!(s.viewer, Viewer::Header(_)),
		workflows: s.workflows.trees.clone(),
		views: s.views.clone(),
		scopes: s.scopes.clone(),
		columns: s.columns.clone(),
		workspaces: s.workspaces.clone(),
		ids: addressed(team_items(s).into_keys()).map(|item| (id("team", &item), item)).collect(),
	})
}

/// Its parts, a line each, as they are read: those the queries `first` show before the others.
pub async fn board(headers: &HeaderMap, first: Vec<String>) -> Result<impl Stream<Item = Result<Bytes>> + use<>> {
	let reach = Caller::of(headers)?.reach;
	let first: Vec<Query> = first.iter().map(|q| q.parse()).collect::<Result<_, _>>()?;
	let store = current().await?;
	let steps = steps(&store, &reach, &first);
	Ok(futures::stream::try_unfold((store, reach, steps.into_iter()), |(store, reach, mut steps)| async move {
		Ok(match steps.next() {
			Some(step) => Some((store.lines(step, &reach).await?, (store, reach, steps))),
			None => None,
		})
	}))
}

/// What a page meant to show at 100% it showed at `percent`, having stood full for `late` percent of the wait.
pub fn off(what: &str, percent: u64, late: u64) {
	tracing::warn!(what, percent, late, "a view's progress was off");
}

pub async fn query(headers: &HeaderMap, asked: String) -> Result<Answer> {
	let caller = Caller::of(headers)?;
	let login = caller.act(async { client::login() }).await;
	let board = whole(caller.reach).await?;
	let hits = asked.parse::<Query>()?.run(&board.cards, login.as_deref())?;
	Ok(Answer {
		trees: settings().workflows.trees.clone(),
		hits: hits
			.into_iter()
			.map(|i| Hit {
				link: board.cards[i].link.clone(),
				states: board.cards[i].states.clone(),
			})
			.collect(),
		login,
	})
}

pub async fn view(headers: &HeaderMap, query: String, group: GroupBy) -> Result<Listing> {
	let caller = Caller::of(headers)?;
	let login = caller.act(async { client::login() }).await;
	tedi_task_operations::board::listing(&whole(caller.reach).await?, query, &group, &settings().workflows.trees, login.as_deref())
}

pub async fn edit(headers: &HeaderMap, link: IssueLink, edits: Vec<FieldEdit>) -> Result<Edited> {
	let caller = Caller::of(headers)?;
	let link = caller.reach.issue(link)?;
	let edited = caller.act(tedi_task_operations::board::edit(&link, edits, settings().mode)).await;
	changed().await; // a failed edit may still have pulled
	refused(edited)
}

pub async fn move_card(headers: &HeaderMap, link: IssueLink, workflow: String, to: State, seen: Option<State>) -> Result<Edited> {
	let caller = Caller::of(headers)?;
	let link = caller.reach.issue(link)?;
	let Some(me) = caller.act(async { client::login() }).await else {
		bail!("who moves it is unknown: bring a key of yours to GitHub, under your account");
	};
	let (s, mover) = (settings(), Mover::Person(me));
	let moved = caller
		.act(tedi_task_operations::workflow::move_card(
			&link,
			&workflow,
			&to,
			seen.as_ref(),
			&mover,
			&s.workflows,
			s.mode,
			jiff::Timestamp::now(),
		))
		.await;
	changed().await; // a failed move may still have pulled
	refused(moved)
}

pub async fn set_milestone(headers: &HeaderMap, link: IssueLink, repo: String, seen: Option<MilestoneLink>, new: Option<MilestoneLink>) -> Result<Edited> {
	let caller = Caller::of(headers)?;
	let (owner, name) = repo.split_once('/').ok_or_else(|| eyre!("a milestone's repo is `owner/repo`, not {repo}"))?;
	let (link, repo) = (caller.reach.issue(link)?, caller.reach.repo(RepoInfo::new(owner, name))?);
	let set = caller
		.act(tedi_task_operations::board::set_milestone(&link, &repo, seen.as_ref(), new.as_ref(), settings().mode))
		.await;
	changed().await; // a failed one may still have pulled
	refused(set)
}

pub async fn thread(headers: &HeaderMap, link: IssueLink) -> Result<Thread> {
	tedi_task_operations::board::thread(&Caller::of(headers)?.reach.issue(link)?).await
}

pub async fn comment(headers: &HeaderMap, link: IssueLink, edit: CommentEdit) -> Result<Edited> {
	let caller = Caller::of(headers)?;
	let link = caller.reach.issue(link)?;
	let said = caller.act(tedi_task_operations::board::comment(&link, edit, settings().mode)).await;
	changed().await; // a failed one may still have pulled
	refused(said)
}

pub async fn request_review(headers: &HeaderMap, link: IssueLink, reviewer: String, on: bool) -> Result<()> {
	let caller = Caller::of(headers)?;
	let link = caller.reach.issue(link)?;
	caller.act(tedi_task_operations::board::request_review(&link, &reviewer, on)).await?;
	changed().await;
	Ok(())
}

pub async fn pending_conflict(headers: &HeaderMap, link: IssueLink) -> Result<Option<String>> {
	let caller = Caller::of(headers)?;
	let link = caller.reach.issue(link)?;
	caller.act(pending(&link)).await
}

pub async fn resolve(headers: &HeaderMap, link: IssueLink, resolution: String) -> Result<Option<String>> {
	let caller = Caller::of(headers)?;
	let link = caller.reach.issue(link)?;
	let resolved = caller.act(async {
		let Some(path) = tedi_task_operations::board::pending_conflict(&link).await? else {
			bail!("nothing is pending on {}: someone resolved it first", *link);
		};
		std::fs::write(&path, resolution)?;
		pending(&link).await
	});
	let still = resolved.await;
	changed().await;
	still
}

pub async fn library(headers: &HeaderMap) -> Result<Library> {
	let key = viewer(headers)?;
	let _held = LIBRARIES.lock().await;
	let library = read_library(&key)?;
	library
		.scopes(&settings().scopes)
		.map_err(|e| eyre!("{e}: rename or drop it in {}", library_file(&key).display()))?;
	Ok(library)
}

pub async fn edit_library(headers: &HeaderMap, edit: LibraryEdit) -> Result<Library> {
	let key = viewer(headers)?;
	let _held = LIBRARIES.lock().await;
	let library = edited(&read_library(&key)?, edit).await?;
	write_library(&key, &library)?;
	Ok(library)
}

pub async fn leave_library(headers: &HeaderMap, edits: Vec<LibraryEdit>) -> Result<()> {
	let key = viewer(headers)?;
	let _held = LIBRARIES.lock().await;
	let mut library = read_library(&key)?;
	for edit in edits {
		match edited(&library, edit).await {
			Ok(after) => library = after,
			Err(e) => tracing::warn!("{key} left behind an edit that doesn't apply: {e:#}"),
		}
	}
	write_library(&key, &library)
}

pub async fn catalog(headers: &HeaderMap) -> Result<Catalog> {
	let me = viewer(headers)?;
	let versions = versions().await?;
	let libraries = {
		let _held = LIBRARIES.lock().await;
		libraries()?
	};
	let team = team_items(settings());
	let owner_of = |viewer: &str, item: &Item| if team.contains_key(item) { Owner::Team } else { Owner::Viewer(viewer.to_string()) };
	let shares = |owner: &str| read_shares(owner);

	let mut usage: BTreeMap<(Owner, Item), Usage> = BTreeMap::new();
	for (viewer, library) in &libraries {
		let mut refers: BTreeSet<(Owner, Item)> = BTreeSet::new();
		for item in kept_items(library) {
			refers.insert((Owner::Viewer(viewer.clone()), item));
		}
		for item in &library.favorites {
			let owner = owner_of(viewer, item);
			usage.entry((owner.clone(), item.clone())).or_default().favorites += 1;
			refers.insert((owner, item.clone()));
		}
		for f in &library.forks {
			let upstream = (f.upstream.owner.clone(), Item::new(f.item.shelf(), f.upstream.name.clone()));
			usage.entry(upstream.clone()).or_default().forks += 1;
			refers.insert(upstream);
		}
		let workspaces = library.workspaces.values().chain(library.drafts.workspaces.values());
		for layout in workspaces.flat_map(|w| w.tiles.values()) {
			for view in viewed(layout)? {
				let item = Item::View(view);
				refers.insert((owner_of(viewer, &item), item));
			}
		}
		if let Some(name) = &library.workspace {
			let item = Item::Workspace(name.clone());
			refers.insert((owner_of(viewer, &item), item));
		}
		for key in refers {
			usage.entry(key).or_default().viewers += 1;
		}
	}

	let behind = |shelf: Shelf, upstream: &Upstream| -> Result<Option<u32>> {
		let latest = match &upstream.owner {
			Owner::Team => versions.as_ref().and_then(|v| v.get(&Item::new(shelf, upstream.name.clone()))).map(|v| v.len() as u32),
			Owner::Viewer(owner) => shares(owner)?.latest(&Item::new(shelf, upstream.name.clone())),
		};
		Ok(match (latest, upstream.version) {
			(Some(latest), Some(at)) => Some(latest - at),
			_ => None,
		})
	};
	let forks_of = |owner: &Owner, item: &Item| -> Result<Vec<Fork>> {
		let mut out = Vec::new();
		for (viewer, library) in &libraries {
			for f in library
				.forks
				.iter()
				.filter(|f| &f.upstream.owner == owner && f.item.shelf() == item.shelf() && f.upstream.name == item.name())
			{
				out.push(Fork {
					viewer: viewer.clone(),
					name: f.item.name().to_string(),
					ahead: f.upstream.ahead,
					behind: behind(f.item.shelf(), &f.upstream)?,
				});
			}
		}
		Ok(out)
	};

	let mut entries = Vec::new();
	for (item, running) in &team {
		let history: Vec<(Version, Construction)> = versions.as_ref().and_then(|v| v.get(item)).cloned().unwrap_or_default(); // an object absent from every commit has no history
		entries.push(Cataloged {
			owner: Owner::Team,
			usage: usage.get(&(Owner::Team, item.clone())).cloned().unwrap_or_default(), // nobody refers to it
			version: history.iter().rev().find(|(_, c)| c == running).map(|(v, _)| v.v),
			history: history.into_iter().map(|(v, _)| v).collect(),
			forks: forks_of(&Owner::Team, item)?,
			behind: None,
			item: item.clone(),
		});
	}
	let mine = libraries.iter().find(|(v, _)| *v == me).map(|(_, l)| l.clone()).unwrap_or_default(); // a viewer who never kept anything
	let owner = Owner::Viewer(me.clone());
	for item in kept_items(&mine) {
		entries.push(Cataloged {
			usage: usage.get(&(owner.clone(), item.clone())).cloned().unwrap_or_default(), // nobody refers to it
			history: Vec::new(),
			version: None,
			forks: forks_of(&owner, &item)?,
			behind: match mine.forks.iter().find(|f| f.item == item) {
				Some(f) => behind(item.shelf(), &f.upstream)?,
				None => None,
			},
			owner: owner.clone(),
			item,
		});
	}
	Ok(Catalog { entries })
}

pub async fn share(headers: &HeaderMap, item: Item, bands: Vec<tilers_core::Band>) -> Result<ShareLink> {
	let me = viewer(headers)?;
	let workspace = matches!(item, Item::Workspace(_));
	if workspace == bands.is_empty() {
		bail!("a workspace is shared for the bands it carries, and nothing else is");
	}
	let team = team_items(settings());
	if let Some(running) = team.get(&item) {
		let versions = versions()
			.await?
			.ok_or_else(|| eyre!("the team's config names no `source`: its objects have no versions to pin"))?;
		let version = versions
			.get(&item)
			.and_then(|h| h.iter().rev().find(|(_, c)| c == running))
			.map(|(v, _)| v.v)
			.ok_or_else(|| eyre!("the team's {} is unpublished: no commit of the config holds it as the board runs it", item.name()))?;
		if let Construction::Workspace(workspace) = running
			&& let Some(band) = bands.iter().find(|b| !workspace.tiles.contains_key(b))
		{
			bail!("the team's workspace {} has no {} arrangement", item.name(), crate::api::band_name(*band));
		}
		return Ok(ShareLink {
			item,
			owner: Owner::Team,
			version,
			bands,
		});
	}
	let _held = LIBRARIES.lock().await;
	let library = read_library(&me)?;
	let mut construction = kept(&library, &item).ok_or_else(|| eyre!("{} is not kept", item.name()))?;
	let mut shares = read_shares(&me)?;
	let mut carried = BTreeMap::new();
	if let Construction::Workspace(Workspace { tiles, .. }) = &mut construction {
		if let Some(band) = bands.iter().find(|b| !tiles.contains_key(b)) {
			bail!("{} has no {} arrangement", item.name(), crate::api::band_name(*band));
		}
		tiles.retain(|band, _| bands.contains(band));
		for layout in tiles.values() {
			for name in viewed(layout)? {
				if let Some(view) = library.views.get(&name) {
					let v = shares.pin(Item::View(name.clone()), Construction::View(view.clone()), BTreeMap::new());
					carried.insert(name, v);
				}
			}
		}
	}
	let version = shares.pin(item.clone(), construction, carried);
	write_shares(&me, &shares)?;
	Ok(ShareLink {
		item,
		owner: Owner::Viewer(me),
		version,
		bands,
	})
}

pub async fn shared(headers: &HeaderMap, link: ShareLink) -> Result<Shared> {
	let me = viewer(headers)?;
	let shared = pinned(&link).await?;
	let _held = LIBRARIES.lock().await;
	let mut library = read_library(&me)?;
	library.received.retain(|l| *l != link);
	library.received.insert(0, link);
	write_library(&me, &library)?;
	Ok(shared)
}

pub async fn propose(headers: &HeaderMap, item: Item, onto: Onto, message: String) -> Result<String> {
	let me = viewer(headers)?;
	let s = settings();
	let source = s
		.source
		.as_ref()
		.ok_or_else(|| eyre!("this board names no `source` for its team config: there is nothing to propose to"))?;
	let construction = {
		let _held = LIBRARIES.lock().await;
		kept(&read_library(&me)?, &item).ok_or_else(|| eyre!("{} is not kept", item.name()))?
	};
	let team = team_items(s);
	let (target, update) = match &onto {
		Onto::Update(name) => (name.clone(), true),
		Onto::New(name) => (name.clone(), false),
	};
	let target_item = Item::new(item.shelf(), target.clone());
	match (update, team.get(&target_item)) {
		(true, None) => bail!("the team has no {target} to update"),
		(false, Some(_)) => bail!("the team already has a {target}: update it, or name the new one otherwise"),
		_ => {}
	}
	let client = tedi_task_operations::github::client::get()?;
	let repo = source_repo(source)?;
	let history = client.file_history(repo, &source.path, &source.branch).await?;
	let head = history
		.last()
		.and_then(|c| c.content.clone())
		.ok_or_else(|| eyre!("{} holds no {} on {}", source.repo, source.path, source.branch))?;
	let content = proposed(&head, &target, update, &construction)?;
	let lines: Vec<Change> = match team.get(&target_item) {
		Some(theirs) => diff(theirs, &construction),
		None => Vec::new(),
	};
	let kind = match item.shelf() {
		Shelf::Views => "view",
		Shelf::Scopes => "scope",
		Shelf::Workspaces => "workspace",
		Shelf::Columns => "column layout",
	};
	let title = format!("board: {} the {kind} {target}", if update { "update" } else { "add" });
	let mut body = message.trim().to_string();
	if !lines.is_empty() {
		body.push_str("\n\n```diff\n");
		for c in &lines {
			body.push_str(&format!("{} {}\n", c.sign, c.text));
		}
		body.push_str("```");
	}
	body.push_str(&format!("\n\nProposed from the board by {me}."));
	let slug: String = target.chars().map(|c| if c.is_ascii_alphanumeric() { c.to_ascii_lowercase() } else { '-' }).collect();
	let proposal = tedi_task_operations::github::FileProposal {
		base: source.branch.clone(),
		head: format!("board/{me}/{slug}-{}", jiff::Timestamp::now().as_second()),
		path: source.path.clone(),
		content,
		title,
		body,
	};
	Ok(client.propose_file(repo, &proposal).await?)
}

async fn pending(link: &Reached<IssueLink>) -> Result<Option<String>> {
	match tedi_task_operations::board::pending_conflict(link).await? {
		Some(path) => Ok(Some(std::fs::read_to_string(path)?)),
		None => Ok(None),
	}
}

/// The board within `reach`.
async fn whole(reach: Reach) -> Result<Board> {
	let store = current().await?;
	let mut board = Board::default();
	for step in steps(&store, &reach, &[]) {
		for line in store.lines(step, &reach).await?.split_inclusive(|b| *b == b'\n') {
			board.extend([serde_json::from_slice(line)?]);
		}
	}
	Ok(board)
}

#[derive(Clone, Copy)]
enum Step {
	Head,
	Unit(usize),
	Ranks,
}

/// All there is of `store` within `reach`, what `first` shows before the rest; what isn't within it has no step.
fn steps(store: &Store, reach: &Reach, first: &[Query]) -> Vec<Step> {
	let late = |(_, source): &(usize, &Held)| !first.iter().any(|q| q.needs(source));
	let (mut open, mut rest): (Vec<_>, Vec<_>) = store.reading.units(reach).partition(|(_, source)| source.holds == Holds::OpenIssues);
	open.sort_by_key(late); // stable
	rest.sort_by_key(late);
	let units = |of: Vec<(usize, &Held)>| of.into_iter().map(|(i, _)| Step::Unit(i)).collect::<Vec<_>>();
	[Step::Head].into_iter().chain(units(open)).chain([Step::Ranks]).chain(units(rest)).collect()
}

/// The store as it stands, its units' lines kept as they are first read: by whoever asks, or ahead of them.
struct Store {
	reading: Reading<'static>,
	units: HashMap<usize, OnceCell<Bytes>>,
}
impl Store {
	async fn lines(&self, step: Step, reach: &Reach) -> Result<Bytes> {
		match step {
			Step::Head => line(&self.reading.head(reach)),
			Step::Unit(i) => self.unit(i).await,
			Step::Ranks => {
				for (i, _) in self.reading.units(reach).filter(|(_, source)| source.holds == Holds::OpenIssues) {
					self.unit(i).await?;
				}
				line(&self.reading.ranks(reach))
			}
		}
	}

	async fn unit(&self, i: usize) -> Result<Bytes> {
		let read = async {
			let mut lines = Vec::new();
			for part in self.reading.read(i).await? {
				lines.extend_from_slice(&line(&part)?);
			}
			tokio::task::yield_now().await; // reading never waits: what is read leaves before the next is
			Ok(Bytes::from(lines))
		};
		self.units[&i].get_or_try_init(|| read).await.cloned()
	}
}

fn line(part: &Part) -> Result<Bytes> {
	let mut line = serde_json::to_vec(part)?;
	line.push(b'\n');
	Ok(line.into())
}

/// The store since the last change.
async fn current() -> Result<Arc<Store>> {
	let mut held = STORE.lock().await;
	if let Some(store) = &*held {
		return Ok(store.clone());
	}
	let reading = Reading::begin(&settings().workflows.trees)?;
	let units = reading.units(&Reach::All).map(|(i, _)| (i, OnceCell::new())).collect();
	let store = Arc::new(Store { reading, units });
	*held = Some(store.clone());
	Ok(store)
}

/// Reads the store after every change, so that a page opening later finds it read.
async fn reader() -> Result<()> {
	let read = async || -> Result<()> {
		let store = current().await?;
		for (i, _) in store.reading.units(&Reach::All) {
			store.unit(i).await?;
		}
		Ok(())
	};
	//LOOP follows the store for the life of the server
	loop {
		let read = tokio::select! {
			() = STALE.notified() => continue,
			read = read() => read,
		};
		if let Err(e) = read {
			tracing::warn!("reading the board ahead: {e:#}"); // whoever asks for it is told
		}
		STALE.notified().await;
	}
}

static SETTINGS: OnceLock<Settings> = OnceLock::new();
/// What the puller pulls next; never dropped, so its channel never closes.
static ANNOUNCE: OnceLock<mpsc::UnboundedSender<Pull>> = OnceLock::new();
static CHANGES: LazyLock<broadcast::Sender<()>> = LazyLock::new(|| broadcast::channel(16).0);
/// Read once per change rather than once per open board; each is served its reach of it.
static STORE: Mutex<Option<Arc<Store>>> = Mutex::const_new(None);
/// The store changed since it was last read ahead.
static STALE: Notify = Notify::const_new();
/// Held across each library's read-modify-write.
static LIBRARIES: Mutex<()> = Mutex::const_new(());

fn settings() -> &'static Settings {
	SETTINGS.get().expect("set by `serve` before the router exists")
}

/// What the puller is asked for.
#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
enum Pull {
	/// An entry of `watch`, or a repo under one.
	Watched(String),
	/// A repo under accounts' scopes.
	Repo(String),
	/// All of an account's scopes.
	Account(String),
}

/// Runs every pull, one at a time: what a webhook announced, and the whole `watch` and every account's scopes
/// each `pull_every`. Owned by `serve`, so a client hanging up never cancels a pull midway.
async fn puller(mut announced: mpsc::UnboundedReceiver<Pull>) -> Result<()> {
	let mut every = settings().pull_every.map(tokio::time::interval);
	//LOOP serves announcements for the life of the server
	loop {
		let pulls: BTreeSet<Pull> = tokio::select! {
			first = announced.recv() => {
				let mut pulls = BTreeSet::from([first.expect("`ANNOUNCE` holds a sender for good")]);
				while let Ok(more) = announced.try_recv() {
					pulls.insert(more);
				}
				pulls
			}
			_ = async { every.as_mut().expect("guarded").tick().await }, if every.is_some() => {
				let accounts = accounts::keys()?.into_iter().map(Pull::Account);
				settings().watch.iter().cloned().map(Pull::Watched).chain(accounts).collect()
			}
		};
		let watched: Vec<String> = pulls.iter().filter_map(|p| if let Pull::Watched(w) = p { Some(w.clone()) } else { None }).collect();
		if let Err(e) = tedi_task_operations::pull::pull_repos(&watched, &settings().workflows, jiff::Timestamp::now()).await {
			tracing::warn!("pull of {watched:?}: {e:#}"); // GitHub unreachable or an issue unstorable: the cursor holds, the next pull retries
		}
		for pull in pulls {
			let pulled = match &pull {
				Pull::Watched(_) => continue,
				Pull::Account(key) => accounts::pull(key, None).await,
				Pull::Repo(repo) => match accounts::naming(repo)?.first() {
					Some(key) => accounts::pull(key, Some(repo)).await, // the store is one: whoever of them sees it
					None => continue,                                   // no scope names it any more
				},
			};
			if let Err(e) = pulled {
				tracing::warn!("{pull:?}: {e:#}");
			}
		}
		changed().await;
	}
}

fn announce(pull: Pull) {
	ANNOUNCE
		.get()
		.expect("set by `serve` before the router exists")
		.send(pull)
		.expect("the puller lives as long as the server");
}

async fn webhook(headers: HeaderMap, body: Bytes) -> StatusCode {
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
			if settings().watch.iter().any(|w| w.eq_ignore_ascii_case(owner) || w.eq_ignore_ascii_case(&repo.full_name)) {
				announce(Pull::Watched(repo.full_name));
				return StatusCode::ACCEPTED;
			}
			match accounts::naming(&repo.full_name) {
				Ok(accounts) if accounts.is_empty() => StatusCode::NO_CONTENT, // the App is public: anyone's install delivers here
				Ok(_) => {
					announce(Pull::Repo(repo.full_name));
					StatusCode::ACCEPTED
				}
				Err(e) => {
					tracing::error!("a delivery of {}: {e:#}", repo.full_name);
					StatusCode::INTERNAL_SERVER_ERROR
				}
			}
		}
		Ok(Delivery { repository: None }) => StatusCode::NO_CONTENT, // an org- or app-level event: no repo's issues moved
		Err(_) => StatusCode::BAD_REQUEST,
	}
}

async fn changed() {
	*STORE.lock().await = None;
	STALE.notify_one();
	let _ = CHANGES.send(()); // errs only while no board is open: nobody to tell
}

/// How an edit ended, when nothing broke.
fn refused(edited: Result<()>) -> Result<Edited> {
	match edited {
		Ok(()) => Ok(Edited::Landed),
		Err(e) => match e.downcast::<Conflict>() {
			Ok(conflict) => Ok(Edited::Conflict(conflict)),
			Err(e) if e.downcast_ref::<Rejected>().is_some() || e.downcast_ref::<MovedOn>().is_some() => Ok(Edited::Rejected(e.to_string())),
			Err(e) => Err(e),
		},
	}
}

/// The viewer's key, as their library's file stem.
fn viewer(headers: &HeaderMap) -> Result<String> {
	let key = match &settings().viewer {
		Viewer::Header(name) => headers
			.get(name)
			.ok_or_else(|| eyre!("the request carries no `{name}`: the gate in front of this board names the signed-in user in it"))?
			.to_str()?
			.to_string(),
		Viewer::Operator => tedi_task_operations::current_user::get().ok_or_else(|| eyre!("a library belongs to a viewer, and this ungated instance doesn't know its GitHub user"))?,
	};
	let plain = key != "team" && !key.is_empty() && !key.starts_with('.') && key.chars().all(|c| c.is_ascii_alphanumeric() || "._@[]-".contains(c));
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
	let mut library: Library = match std::fs::read_to_string(&file) {
		Ok(json) => serde_json::from_str(&json).wrap_err_with(|| format!("{} doesn't read as a library", file.display()))?,
		Err(e) if e.kind() == std::io::ErrorKind::NotFound => Library::default(), // a viewer who never kept anything
		Err(e) => return Err(e.into()),
	};
	for item in addressed(kept_items(&library).into_iter()) {
		if !library.ids.values().any(|held| *held == item) {
			library.ids.insert(id(key, &item), item); // held by no file yet; an edit writes it down
		}
	}
	Ok(library)
}

async fn edited(library: &Library, edit: LibraryEdit) -> Result<Library> {
	let fetched = match &edit {
		LibraryEdit::Fork { from, .. } => Some(origin(from).await?),
		LibraryEdit::Pull(item) => {
			let forked = library.forks.iter().find(|f| &f.item == item).ok_or_else(|| eyre!("{item:?} is no fork"))?;
			Some(latest(item.shelf(), &forked.upstream).await?)
		}
		_ => None,
	};
	let mut edited = library.clone();
	apply(&mut edited, edit, fetched)?;
	Ok(edited)
}

/// `fetched`: a fork's or pull's upstream, read before the edit.
fn apply(library: &mut Library, edit: LibraryEdit, fetched: Option<Fetched>) -> Result<()> {
	let team = team_items(settings());
	match edit {
		LibraryEdit::Workspace { workspace, part } => {
			let mut edited = library.workspace(&settings().workspaces, &workspace).ok_or_else(|| eyre!("no workspace {workspace}"))?;
			let was = edited.clone();
			edited.set(part);
			match settings().workspaces.get(&workspace) {
				_ if edited == was => {}
				Some(theirs) if *theirs == edited => _ = library.drafts.workspaces.remove(&workspace),
				Some(_) => _ = library.drafts.workspaces.insert(workspace, edited),
				None => {
					library.autosaved.insert(workspace.clone(), jiff::Timestamp::now());
					save(library, Item::Workspace(workspace), Construction::Workspace(edited));
				}
			}
		}
		LibraryEdit::View { view, part } => {
			let mut edited = library.view(&settings().views, &view).ok_or_else(|| eyre!("no view {view}"))?;
			if let ViewPart::GroupBy(by) = &part
				&& *by != edited.group_by
			{
				library.laid.remove(&view);
			}
			edited.set(part);
			draft(library, edited);
		}
		LibraryEdit::Revert(Item::View(name)) => {
			library.laid.remove(&name);
			library.drafts.views.remove(&name).ok_or_else(|| eyre!("{name} is as kept"))?;
		}
		LibraryEdit::Revert(Item::Workspace(name)) => _ = library.drafts.workspaces.remove(&name).ok_or_else(|| eyre!("{name} is as the team has it"))?,
		LibraryEdit::Revert(item) => bail!("{item:?} is never drafted"),
		LibraryEdit::Lay { view, columns } => {
			let laid = library
				.columns
				.get(&columns)
				.or_else(|| settings().columns.get(&columns))
				.cloned()
				.ok_or_else(|| eyre!("no column layout {columns}"))?;
			let mut edited = library.view(&settings().views, &view).ok_or_else(|| eyre!("no view {view}"))?;
			if edited.group_by != laid.by {
				bail!("the columns {columns} are by {}, and {view} groups by {}", laid.by, edited.group_by);
			}
			edited.boxes = laid.boxes;
			draft(library, edited);
			library.laid.insert(view, columns);
		}
		LibraryEdit::Locked { view, locked: true } => _ = library.locked.insert(view),
		LibraryEdit::Locked { view, locked: false } => _ = library.locked.remove(&view),
		LibraryEdit::Keep(Kept::View(view)) => {
			let item = Item::View(named(view.name.clone())?);
			refuse_team(&team, &item)?;
			library.drafts.views.remove(&view.name);
			save(library, item, Construction::View(view));
		}
		LibraryEdit::Keep(Kept::Workspace { name, workspace }) => {
			let item = Item::Workspace(named(name)?);
			refuse_team(&team, &item)?;
			save(library, item, Construction::Workspace(workspace));
		}
		LibraryEdit::Keep(Kept::Columns { name, columns }) => {
			let item = Item::Columns(named(name)?);
			refuse_team(&team, &item)?;
			save(library, item, Construction::Columns(columns));
		}
		LibraryEdit::Keep(Kept::Scope { name, scope }) => {
			let name = named(name)?;
			if scope.is_empty() {
				bail!("the scope {name} narrows by nothing");
			}
			if !library.scopes.contains_key(&name) {
				placeable(library, &name)?;
			}
			save(library, Item::Scope(name), Construction::Scope(scope));
		}
		LibraryEdit::Rename { shelf, from, to } => {
			let to = named(to)?;
			let (old, new) = (Item::new(shelf, from.clone()), Item::new(shelf, to.clone()));
			if shelf == Shelf::Scopes {
				return rename_scope(library, &from, &to);
			}
			if kept(library, &new).is_some() || team.contains_key(&new) {
				bail!("{to} is taken");
			}
			let construction = kept(library, &old).ok_or_else(|| eyre!("{from} is not kept"))?;
			let id = library
				.ids
				.iter()
				.find(|(_, held)| **held == old)
				.map(|(id, _)| id.clone())
				.expect("every kept object but a scope has one");
			drop_kept(library, &old);
			library.ids.insert(id, new.clone());
			put(library, &new, construction);
			match shelf {
				Shelf::Views => {
					if let Some(mut draft) = library.drafts.views.remove(&from) {
						draft.name = to.clone();
						library.drafts.views.insert(to.clone(), draft);
					}
					if let Some(laid) = library.laid.remove(&from) {
						library.laid.insert(to.clone(), laid);
					}
					if library.locked.remove(&from) {
						library.locked.insert(to.clone());
					}
				}
				Shelf::Workspaces => {
					if library.workspace.as_deref() == Some(&from) {
						library.workspace = Some(to.clone());
					}
					if let Some(at) = library.autosaved.remove(&from) {
						library.autosaved.insert(to.clone(), at);
					}
					for name in library.recent.iter_mut().filter(|n| **n == from) {
						*name = to.clone();
					}
				}
				Shelf::Columns =>
					for name in library.laid.values_mut().filter(|n| **n == from) {
						*name = to.clone();
					},
				Shelf::Scopes => unreachable!("renamed above"),
			}
			if library.favorites.remove(&old) {
				library.favorites.insert(new.clone());
			}
			for f in library.forks.iter_mut().filter(|f| f.item == old) {
				f.item = new.clone();
			}
		}
		LibraryEdit::Drop { shelf, name } => {
			let item = Item::new(shelf, name.clone());
			if !drop_kept(library, &item) {
				bail!("{name} is not kept");
			}
			if shelf == Shelf::Scopes {
				let held: Vec<String> = library.scopes.keys().filter(|p| within(p, &name).is_some()).cloned().collect();
				for path in held {
					library.scopes.remove(&path); // what it holds goes with it
					library.favorites.remove(&Item::Scope(path.clone()));
					library.forks.retain(|f| f.item != Item::Scope(path.clone()));
				}
			}
			match shelf {
				Shelf::Views => {
					library.drafts.views.remove(&name);
					library.laid.remove(&name);
					library.locked.remove(&name);
				}
				Shelf::Workspaces => {
					if library.workspace.as_deref() == Some(&name) {
						library.workspace = None;
					}
					library.autosaved.remove(&name);
					library.recent.retain(|n| *n != name);
				}
				Shelf::Columns => library.laid.retain(|_, n| *n != name),
				Shelf::Scopes => {}
			}
			library.favorites.remove(&item);
			library.forks.retain(|f| f.item != item);
		}
		LibraryEdit::Open(name) => {
			let item = Item::Workspace(name.clone());
			if kept(library, &item).is_none() && !team.contains_key(&item) {
				bail!("no workspace {name}");
			}
			select(library, name);
		}
		LibraryEdit::NewWorkspace => {
			let free = |name: &String| !library.workspaces.contains_key(name) && !settings().workspaces.contains_key(name);
			let name = (1..).map(|n| format!("ws {n}")).find(free).expect("only so many are kept");
			put(library, &Item::Workspace(name.clone()), Construction::Workspace(Workspace::default()));
			select(library, name);
		}
		LibraryEdit::Favorite { what, on: false } =>
			if !library.favorites.remove(&what) {
				bail!("{what:?} is not a favorite");
			},
		LibraryEdit::Favorite { what, on: true } => {
			let known = match &what {
				Item::Scope(path) => scope_at(&library.scopes(&settings().scopes).map_err(|e| eyre!(e))?, path).is_some(),
				item => kept(library, item).is_some() || team.contains_key(item),
			};
			if !known {
				bail!("no {what:?} to favorite");
			}
			library.favorites.insert(what);
		}
		LibraryEdit::Fork { from, name } => {
			let Fetched { construction, upstream, carried } = fetched.expect("fetched for a fork");
			let shelf = match &from {
				Origin::Team(item) => item.shelf(),
				Origin::Link(link) => link.item.shelf(),
			};
			let item = Item::new(shelf, named(name.clone())?);
			if shelf == Shelf::Scopes {
				placeable(library, &name)?;
			} else if kept(library, &item).is_some() || team.contains_key(&item) {
				bail!("{name} is taken");
			}
			for (view, (version, held)) in carried {
				let carried = Item::View(view.clone());
				if kept(library, &carried).is_some() || team.contains_key(&carried) {
					continue; // the receiver's own view of that name stands
				}
				put(library, &carried, Construction::View(held));
				library.forks.push(Forked {
					item: carried,
					upstream: Upstream {
						owner: upstream.owner.clone(),
						name: view,
						version: Some(version),
						ahead: 0,
					},
				});
			}
			let construction = match construction {
				Construction::View(mut view) => {
					view.name = name;
					Construction::View(view)
				}
				other => other,
			};
			put(library, &item, construction);
			library.forks.push(Forked { item, upstream });
		}
		LibraryEdit::Pull(item) => {
			let Fetched { construction, upstream, .. } = fetched.expect("fetched for a pull");
			let forked = library.forks.iter_mut().find(|f| f.item == item).expect("found before fetching");
			if forked.upstream.ahead > 0 {
				bail!("{} is {} ahead of its upstream: pulling would drop that", item.name(), forked.upstream.ahead);
			}
			forked.upstream = upstream;
			let construction = match construction {
				Construction::View(mut view) => {
					view.name = item.name().to_string();
					Construction::View(view)
				}
				other => other,
			};
			put(library, &item, construction);
		}
	}
	Ok(())
}

/// A new scope can sit at `path`: nothing holds that path yet, and its parent, team's or own, exists.
fn placeable(library: &Library, path: &str) -> Result<()> {
	let tree = library.scopes(&settings().scopes).map_err(|e| eyre!(e))?;
	if scope_at(&tree, path).is_some() {
		bail!("{path} is taken");
	}
	if let Some((parent, _)) = path.rsplit_once(" / ")
		&& scope_at(&tree, parent).is_none()
	{
		bail!("no scope {parent} to keep {path} in");
	}
	Ok(())
}

//#[codestyle::skip(swappable-args)]
/// The viewer's scope at `from` and every one of theirs inside it moved to `to`, with what refers to them.
fn rename_scope(library: &mut Library, from: &str, to: &str) -> Result<()> {
	if !library.scopes.contains_key(from) {
		bail!("{from} is not kept");
	}
	placeable(library, to)?;
	if within(to, from).is_some() {
		bail!("{from} can't move inside itself");
	}
	let moved = |path: &str| within(path, from).map(|rest| format!("{to}{rest}"));
	library.scopes = std::mem::take(&mut library.scopes).into_iter().map(|(p, s)| (moved(&p).unwrap_or(p), s)).collect(); // only those under `from` move
	library.favorites = std::mem::take(&mut library.favorites)
		.into_iter()
		.map(|f| match &f {
			Item::Scope(p) => moved(p).map_or(f, Item::Scope),
			_ => f,
		})
		.collect();
	for f in &mut library.forks {
		if let Item::Scope(p) = &f.item
			&& let Some(to) = moved(p)
		{
			f.item = Item::Scope(to);
		}
	}
	Ok(())
}

/// `name`, the workspace last selected.
fn select(library: &mut Library, name: String) {
	library.recent.retain(|n| *n != name);
	library.recent.insert(0, name.clone());
	library.recent.truncate(9);
	library.workspace = Some(name);
}

/// `view` as last edited, unless it is as kept.
fn draft(library: &mut Library, view: View) {
	let kept = library.views.get(&view.name).or_else(|| settings().views.iter().find(|v| v.name == view.name));
	match kept == Some(&view) {
		true => _ = library.drafts.views.remove(&view.name),
		false => _ = library.drafts.views.insert(view.name.clone(), view),
	}
}

/// `construction` kept as `item`; a fork's `ahead` counts it when it changed anything.
fn save(library: &mut Library, item: Item, construction: Construction) {
	if kept(library, &item).as_ref() != Some(&construction)
		&& let Some(f) = library.forks.iter_mut().find(|f| f.item == item)
	{
		f.upstream.ahead += 1;
	}
	put(library, &item, construction);
}

fn refuse_team(team: &BTreeMap<Item, Construction>, item: &Item) -> Result<()> {
	match team.contains_key(item) {
		true => bail!("{} is the team's: keep yours under another name", item.name()),
		false => Ok(()),
	}
}

fn kept(library: &Library, item: &Item) -> Option<Construction> {
	match item {
		Item::View(n) => library.views.get(n).cloned().map(Construction::View),
		Item::Scope(n) => library.scopes.get(n).cloned().map(Construction::Scope),
		Item::Workspace(n) => library.workspaces.get(n).cloned().map(Construction::Workspace),
		Item::Columns(n) => library.columns.get(n).cloned().map(Construction::Columns),
	}
}

/// What an address names by id: all but scopes, which it names by path.
fn addressed(items: impl Iterator<Item = Item>) -> impl Iterator<Item = Item> {
	items.filter(|item| item.shelf() != Shelf::Scopes)
}

/// Eight letters and digits for `item`, the same for the same `seed`.
fn id(seed: &str, item: &Item) -> String {
	use sha2::Digest as _;
	const DIGITS: &[u8] = b"0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";
	let hash = sha2::Sha256::digest(format!("{seed}\n{}\n{}", item.shelf(), item.name()));
	hash.iter().take(8).map(|b| DIGITS[usize::from(*b) % DIGITS.len()] as char).collect()
}

fn put(library: &mut Library, item: &Item, construction: Construction) {
	if item.shelf() == Shelf::Views && kept(library, item).is_none() {
		library.drafts.views.remove(item.name()); // of another by its name, gone since
	}
	if item.shelf() != Shelf::Scopes && !library.ids.values().any(|held| held == item) {
		// seeded by the moment: the name's last holder may still hold its id, renamed
		library.ids.insert(id(&jiff::Timestamp::now().as_nanosecond().to_string(), item), item.clone());
	}
	let name = item.name().to_string();
	match (item, construction) {
		(Item::View(_), Construction::View(v)) => _ = library.views.insert(name, v),
		(Item::Scope(_), Construction::Scope(s)) => _ = library.scopes.insert(name, s),
		(Item::Workspace(_), Construction::Workspace(w)) => _ = library.workspaces.insert(name, w),
		(Item::Columns(_), Construction::Columns(c)) => _ = library.columns.insert(name, c),
		(item, construction) => panic!("{item:?} holds no {construction:?}"),
	}
}

/// Whether it was kept.
fn drop_kept(library: &mut Library, item: &Item) -> bool {
	library.ids.retain(|_, held| held != item);
	let name = item.name();
	match item {
		Item::View(_) => library.views.remove(name).is_some(),
		Item::Scope(_) => library.scopes.remove(name).is_some(),
		Item::Workspace(_) => library.workspaces.remove(name).is_some(),
		Item::Columns(_) => library.columns.remove(name).is_some(),
	}
}

fn kept_items(library: &Library) -> Vec<Item> {
	let names = |shelf: Shelf, names: Vec<&String>| names.into_iter().map(|n| Item::new(shelf, n.clone())).collect::<Vec<_>>();
	[
		names(Shelf::Views, library.views.keys().collect()),
		names(Shelf::Scopes, library.scopes.keys().collect()),
		names(Shelf::Workspaces, library.workspaces.keys().collect()),
		names(Shelf::Columns, library.columns.keys().collect()),
	]
	.concat()
}

/// The team's objects as `board.toml` lays them out: `[[board.views]]`, `[board.scopes…]`, `[board.columns.…]`, `[board.workspaces.…]`.
#[derive(serde::Deserialize)]
struct Team {
	#[serde(default)] // a commit may hold none
	views: Vec<View>,
	#[serde(default)] // a commit may hold none
	scopes: BTreeMap<String, Scope>,
	#[serde(default)] // a commit may hold none
	columns: BTreeMap<String, crate::api::Columns>,
	#[serde(default)] // a commit may hold none
	workspaces: BTreeMap<String, Workspace>,
}

fn team_of(config: &str) -> Result<Team> {
	#[derive(serde::Deserialize)]
	struct File {
		board: Team,
	}
	Ok(toml::from_str::<File>(config)?.board)
}

fn items(team: &Team) -> BTreeMap<Item, Construction> {
	crate::api::constructions(&team.views, &team.scopes, &team.columns, &team.workspaces)
}

/// As this board runs them.
fn team_items(s: &Settings) -> BTreeMap<Item, Construction> {
	items(&Team {
		views: s.views.clone(),
		scopes: s.scopes.clone(),
		columns: s.columns.clone(),
		workspaces: s.workspaces.clone(),
	})
}

/// None: the board names no `source`.
async fn versions() -> Result<Option<Versions>> {
	let Some(source) = &settings().source else {
		return Ok(None);
	};
	let commits = tedi_task_operations::github::client::get()?
		.file_history(source_repo(source)?, &source.path, &source.branch)
		.await?;
	let mut versions: Versions = BTreeMap::new();
	for commit in commits {
		let held = match &commit.content {
			Some(text) => items(&team_of(text).wrap_err_with(|| format!("{} as of {}", source.path, commit.sha))?),
			None => BTreeMap::new(),
		};
		for (item, construction) in held {
			let history = versions.entry(item).or_default();
			let changed = match history.last() {
				Some((_, last)) if *last == construction => continue,
				Some((_, last)) => diff(last, &construction),
				None => Vec::new(),
			};
			let v = history.len() as u32 + 1;
			history.push((
				Version {
					v,
					author: commit.author.clone(),
					at: commit.at,
					changes: changed,
				},
				construction,
			));
		}
	}
	Ok(Some(versions))
}

fn source_repo(source: &Source) -> Result<RepoInfo> {
	let (owner, repo) = source.repo.split_once('/').ok_or_else(|| eyre!("`source.repo` is `owner/repo`, not {}", source.repo))?;
	Ok(RepoInfo::new(owner, repo))
}

/// The views a dock layout holds.
fn viewed(layout: &str) -> Result<Vec<String>> {
	let grid = tilers_core::model::serial::load(layout).map_err(|e| eyre!("a kept layout doesn't load: {e:?}"))?;
	Ok(grid
		.cells
		.iter()
		.flat_map(|c| c.group.tabs.iter())
		.filter_map(|t| t.0.strip_prefix("view:"))
		.map(str::to_owned)
		.collect())
}

/// Every viewer's, by key.
fn libraries() -> Result<Vec<(String, Library)>> {
	let dir = tedi_task_operations::paths::data_dir("web");
	let entries = match std::fs::read_dir(&dir) {
		Ok(entries) => entries,
		Err(e) if e.kind() == std::io::ErrorKind::NotFound => return Ok(Vec::new()), // nobody kept anything yet
		Err(e) => return Err(e.into()),
	};
	let mut out = Vec::new();
	for entry in entries {
		let path = entry?.path();
		if path.extension().is_some_and(|e| e == "json")
			&& let Some(key) = path.file_stem().and_then(|s| s.to_str())
		{
			out.push((key.to_string(), read_library(key)?));
		}
	}
	Ok(out)
}

fn write_library(key: &str, library: &Library) -> Result<()> {
	let file = library_file(key);
	let fresh = file.with_extension("json.new");
	std::fs::write(&fresh, serde_json::to_string_pretty(library)?)?;
	std::fs::rename(&fresh, &file)?; // whole or not at all: a crash mid-write keeps the last library
	Ok(())
}

/// A fork's or pull's source, as read before the edit.
struct Fetched {
	construction: Construction,
	upstream: Upstream,
	carried: BTreeMap<String, (u32, View)>,
}

async fn origin(from: &Origin) -> Result<Fetched> {
	match from {
		Origin::Team(item) => {
			let running = team_items(settings()).remove(item).ok_or_else(|| eyre!("the team has no {}", item.name()))?;
			let version = versions()
				.await?
				.and_then(|v| v.get(item).and_then(|h| h.iter().rev().find(|(_, c)| *c == running)).map(|(v, _)| v.v));
			Ok(Fetched {
				upstream: Upstream {
					owner: Owner::Team,
					name: item.name().to_string(),
					version,
					ahead: 0,
				},
				construction: running,
				carried: BTreeMap::new(),
			})
		}
		Origin::Link(link) => {
			let shared = pinned(link).await?;
			Ok(Fetched {
				upstream: Upstream {
					owner: link.owner.clone(),
					name: link.item.name().to_string(),
					version: Some(link.version),
					ahead: 0,
				},
				construction: shared.construction,
				carried: shared.carried,
			})
		}
	}
}

/// `upstream`'s latest version.
async fn latest(shelf: Shelf, upstream: &Upstream) -> Result<Fetched> {
	let item = Item::new(shelf, upstream.name.clone());
	let (version, construction) = match &upstream.owner {
		Owner::Team => {
			let versions = versions().await?.ok_or_else(|| eyre!("the team's config names no `source`: it has no versions to pull"))?;
			let (v, c) = versions
				.get(&item)
				.and_then(|h| h.last())
				.ok_or_else(|| eyre!("no commit of the team's config holds {}", item.name()))?;
			(v.v, c.clone())
		}
		Owner::Viewer(owner) => {
			let shares = read_shares(owner)?;
			let v = shares.latest(&item).ok_or_else(|| eyre!("{owner} never shared {}", item.name()))?;
			(v, shares.at(&item, v).expect("its latest").construction.clone())
		}
	};
	Ok(Fetched {
		construction,
		upstream: Upstream {
			owner: upstream.owner.clone(),
			name: upstream.name.clone(),
			version: Some(version),
			ahead: 0,
		},
		carried: BTreeMap::new(),
	})
}

async fn pinned(link: &ShareLink) -> Result<Shared> {
	let missing = || eyre!("{link} pins nothing: no such version");
	let (mut construction, carried) = match &link.owner {
		Owner::Team => {
			let versions = versions().await?.ok_or_else(|| eyre!("the team's config names no `source`: its versions are unknown"))?;
			let (_, c) = versions.get(&link.item).and_then(|h| h.get((link.version as usize).checked_sub(1)?)).ok_or_else(missing)?;
			(c.clone(), BTreeMap::new())
		}
		Owner::Viewer(owner) => {
			let shares = read_shares(owner)?;
			let snapshot = shares.at(&link.item, link.version).ok_or_else(missing)?;
			let mut carried = BTreeMap::new();
			for (name, v) in &snapshot.carried {
				let Some(Snapshot {
					construction: Construction::View(view),
					..
				}) = shares.at(&Item::View(name.clone()), *v)
				else {
					bail!("{owner}'s shares lost the view {name}@{v} that {link} carries");
				};
				carried.insert(name.clone(), (*v, view.clone()));
			}
			(snapshot.construction.clone(), carried)
		}
	};
	if let Construction::Workspace(Workspace { tiles, .. }) = &mut construction {
		tiles.retain(|band, _| link.bands.contains(band));
		if tiles.is_empty() {
			bail!("{link} carries no band its workspace has");
		}
	}
	Ok(Shared {
		link: link.clone(),
		construction,
		carried,
	})
}

/// What a viewer shared, by object: each version as it was when its link was copied.
#[derive(Default, serde::Deserialize, serde::Serialize)]
struct Shares(Vec<Pinned>);
impl Shares {
	fn latest(&self, item: &Item) -> Option<u32> {
		self.0.iter().find(|p| p.item == *item).map(|p| p.versions.len() as u32)
	}

	fn at(&self, item: &Item, version: u32) -> Option<&Snapshot> {
		self.0.iter().find(|p| p.item == *item)?.versions.get((version as usize).checked_sub(1)?)
	}

	/// Its version: the last one again when nothing changed since.
	fn pin(&mut self, item: Item, construction: Construction, carried: BTreeMap<String, u32>) -> u32 {
		let snapshot = Snapshot { construction, carried };
		let at = match self.0.iter().position(|p| p.item == item) {
			Some(at) => at,
			None => {
				self.0.push(Pinned { item, versions: Vec::new() });
				self.0.len() - 1
			}
		};
		let versions = &mut self.0[at].versions;
		if versions.last() != Some(&snapshot) {
			versions.push(snapshot);
		}
		versions.len() as u32
	}
}

#[derive(serde::Deserialize, serde::Serialize)]
struct Pinned {
	item: Item,
	versions: Vec<Snapshot>,
}
#[derive(Clone, PartialEq, serde::Deserialize, serde::Serialize)]
struct Snapshot {
	construction: Construction,
	/// The sharer's own views a workspace's tiles name, at the versions they were pinned at.
	carried: BTreeMap<String, u32>,
}

fn shares_file(owner: &str) -> PathBuf {
	tedi_task_operations::paths::data_dir("web").join("shared").join(format!("{owner}.json"))
}

fn read_shares(owner: &str) -> Result<Shares> {
	let file = shares_file(owner);
	match std::fs::read_to_string(&file) {
		Ok(json) => serde_json::from_str(&json).wrap_err_with(|| format!("{} doesn't read as shares", file.display())),
		Err(e) if e.kind() == std::io::ErrorKind::NotFound => Ok(Shares::default()), // never shared anything
		Err(e) => Err(e.into()),
	}
}

fn write_shares(owner: &str, shares: &Shares) -> Result<()> {
	let file = shares_file(owner);
	std::fs::create_dir_all(file.parent().expect("under the data dir"))?;
	let fresh = file.with_extension("json.new");
	std::fs::write(&fresh, serde_json::to_string_pretty(shares)?)?;
	std::fs::rename(&fresh, &file)?;
	Ok(())
}

//#[codestyle::skip(swappable-args)]
/// `head` with `construction` written as the team's `target`, everything else as it was.
fn proposed(head: &str, target: &str, update: bool, construction: &Construction) -> Result<String> {
	use toml_edit::{DocumentMut, Item as Toml, Table};
	fn table(value: &impl serde::Serialize) -> Result<Table> {
		Ok(toml::to_string(value)?.parse::<DocumentMut>()?.as_table().clone())
	}
	/// `held` made `new`, keeping where each key that stays sits and how it is written around.
	fn merge(held: &mut Table, new: Table) {
		let keys: Vec<String> = new.iter().map(|(k, _)| k.to_string()).collect();
		held.retain(|k, _| keys.iter().any(|n| n == k));
		for (key, item) in new {
			match (held.get_mut(&key).and_then(Toml::as_table_mut), item) {
				(Some(held), Toml::Table(new)) => merge(held, new),
				(_, item) => _ = held.insert(&key, item),
			}
		}
	}
	fn named<'a>(board: &'a mut Table, key: &str) -> Result<&'a mut Table> {
		board.entry(key).or_insert(toml_edit::table()).as_table_mut().ok_or_else(|| eyre!("`board.{key}` is not a table"))
	}
	fn put_table(at: &mut Table, key: &str, new: Table) {
		match at.get_mut(key).and_then(Toml::as_table_mut) {
			Some(held) => merge(held, new),
			None => _ = at.insert(key, Toml::Table(new)),
		}
	}
	let held = |holds: bool| -> Result<()> {
		match (update, holds) {
			(true, false) => bail!("the config's head holds no {target}: it moved on since this board was deployed"),
			(false, true) => bail!("the config's head already holds a {target}"),
			_ => Ok(()),
		}
	};
	let mut doc: DocumentMut = head.parse()?;
	let board = doc.entry("board").or_insert(toml_edit::table()).as_table_mut().ok_or_else(|| eyre!("`board` is not a table"))?;
	match construction {
		Construction::View(view) => {
			let mut view = view.clone();
			view.name = target.to_string();
			let written = table(&view)?;
			let views = board
				.entry("views")
				.or_insert(Toml::ArrayOfTables(Default::default()))
				.as_array_of_tables_mut()
				.ok_or_else(|| eyre!("`board.views` is not an array of tables"))?;
			let at = views.iter().position(|v| v.get("name").and_then(|n| n.as_str()) == Some(target));
			held(at.is_some())?;
			match at {
				Some(at) => merge(views.get_mut(at).expect("found above"), written),
				None => views.push(written),
			}
		}
		Construction::Scope(scope) => {
			let mut at = named(board, "scopes")?;
			let path: Vec<&str> = target.split(" / ").collect();
			let (last, parents) = path.split_last().expect("split yields one");
			for name in parents {
				at = at.get_mut(name).and_then(Toml::as_table_mut).ok_or_else(|| eyre!("the config's head holds no scope {name}"))?;
			}
			held(at.contains_key(last))?;
			put_table(at, last, table(scope)?);
		}
		Construction::Columns(columns) => {
			let at = named(board, "columns")?;
			held(at.contains_key(target))?;
			put_table(at, target, table(columns)?);
		}
		Construction::Workspace(workspace) => {
			let at = named(board, "workspaces")?;
			held(at.contains_key(target))?;
			put_table(at, target, table(workspace)?);
		}
	}
	Ok(doc.to_string())
}

fn named(name: String) -> Result<String> {
	match name.trim().is_empty() {
		true => bail!("kept under a name, and this one is blank"),
		false => Ok(name),
	}
}

//#[codestyle::skip(swappable-args)]
/// What follows `top` in `path` when `path` is `top` or nested in it.
fn within<'a>(path: &'a str, top: &str) -> Option<&'a str> {
	path.strip_prefix(top).filter(|rest| rest.is_empty() || rest.starts_with(" / "))
}
