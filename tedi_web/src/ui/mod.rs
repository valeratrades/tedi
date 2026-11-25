//! The client: a sidebar of workspaces, views, scopes and sprints over the open workspace: its lens, then its
//! tiles, every one a view or an issue. Cards arrive whole and are queried here; edits leave as server fns. What
//! is on screen, and what the viewer keeps, is their library on the server.

mod account;
mod card;
mod chain;
mod facets;
mod filter;
mod icons;
mod issue;
mod kit;
mod library;
mod palette;
mod route;
mod scope;
mod shell;
mod sync;
mod toast;
mod toolbar;
mod view;
mod workspace;

use std::{
	collections::{BTreeMap, HashMap, HashSet},
	rc::Rc,
};

use dioxus::{
	core::{Runtime, spawn_forever},
	prelude::*,
};
use ev_lib::uikit_dioxus::*;
use futures::StreamExt as _;
use icons::{Glyph, Icon};
use tedi_core::{
	IssueLink, MilestoneLink,
	board::{Board, Card, Holds, Part, Query, Scope, Shown, Slice, Source, Target, Within},
	chain::Point,
	conflict::{CommentEdit, Conflict, Edited},
	workflow::Workflow,
};
use tilers_dioxus::{
	Band, DockPanel, Group, GroupId, MinSize, PackedApi, PackedArea, PackedGrid, PanelId, Tile,
	model::{packed::GridState, serial},
};

use crate::api::{self, Arrangement, Item, Lens, Library, LibraryEdit, Setup, View, ViewPart, Workspace, WorkspacePart};

const HELP: [(&str, &str); 7] = [
	("⌘K", "jump to a view, scope, sprint or issue"),
	("⇧⏎", "in the palette: open in a new tile"),
	("⇧-click", "a facet value or a card's chip: rule it out of the workspace"),
	("⌥-click", "an importance or hours bound: the exact value"),
	("drag", "a sidebar row onto the dock: open it there"),
	("drag", "a card onto a column: move it there"),
	("drag", "a sort label: what orders first; a click reverses it"),
];

#[wasm_bindgen::prelude::wasm_bindgen(js_namespace = loading)]
extern "C" {
	/// The client runs: the frame, and what doesn't load from here on, are its to show.
	fn started();
}

/// How long what settles is let settle.
const SETTLES: std::time::Duration = std::time::Duration::from_millis(800);
/// A wait of so many ms is one.
const NOTICED: f64 = 200.0;
const PANEL_MIN: MinSize = MinSize::Rem { w: 16.0, h: 8.0 };

/// The most a browser carries for a page that is gone.
const CARRIED: usize = 64 * 1024;

#[component]
pub fn App() -> Element {
	rsx! {
		Fonts {}
		Shell {}
	}
}

/// Shared by every tile.
#[derive(Clone, Copy)]
struct Ctx {
	setup: Signal<Option<Setup>>,
	/// The server's, as last read.
	library: Signal<Option<Library>>,
	/// Edits it may not hold yet, oldest first: everything shown is `library` with these over it.
	unsaved: Signal<Vec<Unsaved>>,
	/// What sends them once they settle.
	saving: Signal<Option<dioxus::core::Task>>,
	/// Held by whoever sends: edits land in the order they were made.
	wire: Signal<Rc<futures::lock::Mutex<()>>>,
	/// The workspace this tab shows, by id.
	workspace: Signal<Option<String>>,
	/// What every tile runs: the open workspace's. None: none is open.
	lens: Memo<Option<Lens>>,
	/// The server's, as last read.
	stored: Signal<Board>,
	/// Edits it may not hold yet, oldest first.
	pending: Signal<Vec<sync::Pending>>,
	/// Per what is set, the edits made of it that can be undone, oldest first, each against what the one before left.
	made: Signal<HashMap<String, Vec<sync::Op>>>,
	reading: Signal<Reading>,
	/// What is shown: `stored` with `pending` over it.
	board: Memo<Board>,
	/// Edits that came back clashing, shown on their issue until answered.
	conflicts: Signal<HashMap<IssueLink, Conflict>>,
	panels: Signal<Vec<DockPanel>>,
	api: Signal<Option<PackedApi>>,
	/// The dock as it was last arranged or kept. None: what it shows isn't the viewer's to keep.
	seen: Signal<Option<Seen>>,
	/// What of the first board is in `stored`, and what has come by now.
	loading: Signal<Loading>,
	progress: Signal<Loading>,
	/// What each tile on screen shows, and its name.
	wanted: Signal<BTreeMap<u64, (String, Need)>>,
	/// The queries the board is read in the order of.
	order: Signal<Vec<String>>,
	/// Why setup or the library didn't arrive: nothing renders without both.
	failure: Signal<Option<String>>,
	/// The dialog over the screen.
	modal: Signal<Option<Modal>>,
	/// The command palette, open.
	palette: Signal<bool>,
	/// The tile the palette's pick lands in as a tab, when its + opened it.
	into: Signal<Option<GroupId>>,
	/// On a phone, the shelves' sheet, open.
	sheet: Signal<bool>,
	/// The outcomes on screen, oldest first.
	notes: Signal<Vec<toast::Note>>,
	/// The address the page was loaded at is yet to be reached.
	landing: Signal<bool>,
	/// A shared workspace on the dock in place of the viewer's, read-only.
	viewing: Signal<Option<api::Shared>>,
	/// What the address bar names.
	at: Signal<route::Address>,
	/// The panel last brought up, and what was up then.
	main: Signal<Option<PanelId>>,
	up: Signal<Vec<PanelId>>,
	/// Moves to an address under way; after the last, the bar corrects rather than steps.
	moving: Signal<usize>,
	settling: Signal<bool>,
	/// How often each view was reverted: its columns remount on it.
	relaid: Signal<HashMap<String, usize>>,
}
impl Ctx {
	/// The band the dock is laid out for: `Sm` is a phone, one tile at a time.
	pub(super) fn band(&self) -> Option<Band> {
		(*self.api.read()).map(|a| a.band())
	}

	/// An outcome, replacing any on screen under `key`.
	pub(super) fn say(&self, key: impl Into<String>, tone: toast::Tone, text: impl Into<String>, action: Option<(&'static str, Callback<()>)>) {
		let mut notes = self.notes;
		let key = key.into();
		let id = notes.peek().iter().map(|n| n.id).max().map_or(0, |m| m + 1);
		let mut notes = notes.write();
		notes.retain(|n| n.key != key);
		notes.push(toast::Note {
			id,
			key,
			tone,
			text: text.into(),
			action,
		});
	}

	pub(super) fn unsay(&self, id: u64) {
		let mut notes = self.notes;
		notes.write().retain(|n| n.id != id);
	}

	fn card(&self, link: &IssueLink) -> Option<Card> {
		self.board.read().cards.iter().find(|c| &c.link == link).cloned()
	}

	/// What cards sit in, by name.
	fn workflows(&self) -> BTreeMap<String, Workflow> {
		self.setup.read().as_ref().expect("tiles render once setup arrived").workflows.clone()
	}

	/// Whether some of what it shows isn't here yet.
	fn awaits(&self, need: &Need) -> bool {
		need.percent(&self.loading.read(), &self.board.read()).is_some()
	}

	/// What every view shows: the open workspace's lens.
	fn need(&self) -> Option<Need> {
		let Lens { mut search, scope, .. } = self.lens.read().clone()?;
		search.narrow(&scope.terms(&self.orgs()));
		Some(Need::Query(search))
	}

	fn me(&self) -> Option<String> {
		self.setup.read().as_ref()?.me.clone()
	}

	/// Each as kept: the team's, then the viewer's own.
	fn views(&self) -> Vec<View> {
		let setup = self.setup.read();
		let library = self.library.read();
		let team = setup.as_ref().map(|s| s.views.clone()).into_iter().flatten();
		let mut views: Vec<View> = team
			.chain(library.as_ref().map(|l| l.views.values().cloned().collect::<Vec<_>>()).into_iter().flatten())
			.collect();
		if let Some(shared) = &*self.viewing.read() {
			views.extend(
				shared
					.carried
					.values()
					.filter(|(_, v)| !views.iter().any(|w| w.name == v.name))
					.map(|(_, v)| v.clone())
					.collect::<Vec<_>>(),
			);
		}
		views
	}

	/// The view `name` as last edited.
	fn view(&self, name: &str) -> Option<View> {
		let mut view = {
			let (setup, library) = (self.setup.read(), self.library.read());
			match library.as_ref()?.view(&setup.as_ref()?.views, name) {
				Some(view) => view,
				None => {
					let viewing = self.viewing.read();
					return viewing.as_ref().and_then(|s| s.carried.get(name)).map(|(_, v)| v.clone());
				}
			}
		};
		for edit in self.unsaved.read().iter() {
			match &edit.edit {
				LibraryEdit::View { view: edited, part } if edited == name => view.set(part.clone()),
				_ => {}
			}
		}
		Some(view)
	}

	/// What the open workspace's scope leaves in; the error: nothing of it is on the board, as far as that is here.
	fn within(&self) -> Result<Within, String> {
		match &*self.lens.read() {
			None => Ok(Within::default()),
			Some(Lens { scope, .. }) => self.left_in(scope).ok_or_else(|| format!("nothing of {scope} is on the board: pick another scope")),
		}
	}

	fn left_in(&self, scope: &Slice) -> Option<Within> {
		scope.within(&self.board.read(), &self.orgs())
	}

	/// The sprints of the open workspace's scope: what is planned, the shortest period first, then the bottlenecks.
	fn sprints(&self) -> Vec<Target> {
		let within = self.within().unwrap_or_default(); // nothing of it is on the board: shown without one
		let board = self.board.read();
		let mut sprints: Vec<Target> = board.targets.iter().filter(|t| t.sprint().is_some() && within.offers(&t.link)).cloned().collect();
		sprints.sort_by_key(|t| (t.sprint(), t.link.repo_info().to_string()));
		sprints
	}

	/// The open workspace's name.
	fn open_workspace(&self) -> Option<String> {
		match self.named(self.workspace.read().as_ref()?)? {
			Item::Workspace(name) => Some(name),
			other => panic!("only a workspace is open: {other:?}"),
		}
	}

	fn workspace_named(&self, name: &str) -> Option<Workspace> {
		workspace_named(&self.setup.read(), &self.library.read(), &self.unsaved.read(), name)
	}

	/// What a card can show.
	fn fields(&self) -> impl Iterator<Item = Shown> {
		let states = self.workflows().into_keys().map(Shown::State);
		[Shown::Repo, Shown::Number, Shown::Assignees].into_iter().chain(states).chain([
			Shown::Importance,
			Shown::Hours,
			Shown::Type,
			Shown::Labels,
			Shown::Blockers,
			Shown::Prs,
			Shown::Sprint,
			Shown::Updated,
		])
	}

	/// The view `name` as kept again, its columns mounted anew.
	async fn revert(self, name: String) {
		if self.keep(LibraryEdit::Revert(Item::View(name.clone()))).await {
			let mut relaid = self.relaid;
			*relaid.write().entry(name).or_default() += 1;
		}
	}

	fn edit_view(self, view: &str, part: ViewPart) {
		let kept = {
			let (setup, library) = (self.setup.peek(), self.library.peek());
			let (setup, library) = (setup.as_ref().expect("views render once setup arrived"), library.as_ref().expect("it arrives with setup"));
			library.view(&setup.views, view).is_some()
		};
		match kept {
			true => self.settle(LibraryEdit::View { view: view.to_string(), part }),
			false => self.say("view", toast::Tone::Info, format!("{view} is shared with you: fork it to change it"), None),
		}
	}

	/// A part of the open workspace's lens; of a shared one, as it is viewed.
	fn edit_lens(self, part: WorkspacePart) {
		let mut viewing = self.viewing;
		if viewing.peek().is_some() {
			return match &mut viewing.write().as_mut().expect("peeked").construction {
				api::Construction::Workspace(workspace) => workspace.set(part),
				other => panic!("only a workspace takes the dock: {other:?}"),
			};
		}
		match self.open_workspace() {
			Some(workspace) => self.settle(LibraryEdit::Workspace { workspace, part }),
			None => self.none_open(),
		}
	}

	/// What `lens` is, or is about to be: a memo peeked at may be a change behind.
	fn lens_now(&self) -> Option<Lens> {
		lens(&self.setup.peek(), &self.library.peek(), &self.unsaved.peek(), &self.workspace.peek(), &self.viewing.peek())
	}

	fn none_open(&self) {
		self.say("workspace", toast::Tone::Info, "no workspace is open: pick one first", None);
	}

	/// The named scope's terms copied into the open workspace.
	fn start_from(self, path: Option<String>) {
		let (terms, what) = match &path {
			None => (Default::default(), "all repos"),
			Some(path) => {
				let at = self.scopes().into_iter().find(|s| s.path == *path).expect("offered from these scopes");
				(at.scope.filter(), path.as_str())
			}
		};
		self.set_scope(Slice::Terms(terms), format!("Started from {what}"));
	}

	fn scope_to(self, point: Point) {
		let said = format!("Scoped to {}", self.path(&point));
		self.set_scope(Slice::Point(point), said);
	}

	/// The open workspace narrowed to `scope`, and `said` so.
	fn set_scope(self, scope: Slice, said: String) {
		let Some(Lens { scope: was, .. }) = self.lens_now() else {
			return self.none_open();
		};
		if was == scope {
			return;
		}
		self.edit_lens(WorkspacePart::Scope(scope));
		let kept = self.viewing.peek().is_none().then(|| self.open_workspace()).flatten();
		let undo = kept.map(|workspace| {
			let back = LibraryEdit::Workspace {
				workspace,
				part: WorkspacePart::Scope(was),
			};
			// what it is picked from leaves the screen before its note does
			Runtime::current().in_scope(ScopeId::ROOT, || Callback::new(move |()| self.settle(back.clone())))
		});
		self.say("scope", toast::Tone::Info, said, undo.map(|undo| ("Undo", undo)));
	}

	/// The team's with the viewer's own in them, parents before what they hold.
	fn scopes(&self) -> Vec<ScopeAt> {
		fn walk(prefix: &str, level: BTreeMap<String, Scope>, depth: usize, out: &mut Vec<ScopeAt>) {
			for (name, scope) in level {
				let path = if prefix.is_empty() { name } else { format!("{prefix} / {name}") };
				let children = scope.children.clone();
				out.push(ScopeAt { path: path.clone(), depth, scope });
				walk(&path, children, depth + 1, out);
			}
		}
		let (library, setup) = (self.library.read(), self.setup.read());
		let tree = library
			.as_ref()
			.expect("read once the library arrived")
			.scopes(&setup.as_ref().expect("arrives with the library").scopes)
			.expect("the server serves no library whose scope lost its parent");
		let mut out = Vec::new();
		walk("", tree, 0, &mut out);
		out
	}

	fn favorite(&self, what: &Item) -> bool {
		self.library.read().as_ref().is_some_and(|l| l.favorites.contains(what))
	}

	fn open_issue(&self, link: IssueLink) {
		self.open(PanelId(format!("issue:{link}")));
	}

	/// Shows `id`, hosting it first if it isn't: as a tab of the largest tile holding its kind (issues one,
	/// views and sprints the other), else beside the widest tile, which gives up the room.
	fn open(&self, id: PanelId) {
		if self.lens_now().is_none() {
			return self.none_open();
		}
		let mut api = (*self.api.peek()).expect("a band is entered before anything can be opened");
		let library = |p: &PanelId| p.0.starts_with("library:");
		if library(&id)
			&& let Some(held) = serial::load(&api.save())
				.expect("a layout loads back from its own save")
				.cells
				.iter()
				.flat_map(|c| c.group.tabs.iter())
				.find(|t| library(t) && **t != id)
				.cloned()
		{
			self.retitle(&held, id.clone()); // one Library, refocused
		}
		self.host(&id);
		if !id.0.starts_with("issue:") {
			let mut main = self.main;
			main.set(Some(id.clone()));
		}
		let mut grid = serial::load(&api.save()).expect("a layout loads back from its own save");
		if let Some(cell) = grid.cells.iter_mut().find(|c| c.group.tabs.contains(&id)) {
			cell.group.active = cell.group.tabs.iter().position(|t| *t == id).expect("found in it");
			api.load(&serial::save(&grid)).expect("its own save, another tab shown");
			return;
		}
		if let Some(float) = grid.floats.iter_mut().find(|f| f.group.tabs.contains(&id)) {
			float.group.active = float.group.tabs.iter().position(|t| *t == id).expect("found in it");
			api.load(&serial::save(&grid)).expect("its own save, another tab shown");
			return;
		}
		let issue = |p: &PanelId| p.0.starts_with("issue:");
		if issue(&id) {
			// an issue opens above the board, never squeezing it: one float holds them as tabs
			match grid.floats.iter().find(|f| f.group.tabs.iter().any(issue)) {
				Some(float) => {
					api.add_tab(float.group.id, id.clone());
					return self.open(id);
				}
				None => {
					let group = Group::new(api.mint_group_id(), id);
					let (cols, rows) = (api.cols(), api.rows());
					return api.float(group, (cols * 5).div_euclid(8), (rows * 4).div_euclid(5)); // room for the conversation and its properties beside it
				}
			}
		}
		if let Some(cell) = grid.cells.iter().filter(|c| c.group.tabs.iter().any(|t| !issue(t))).max_by_key(|c| c.w * c.h) {
			api.add_tab(cell.group.id, id.clone());
			return self.open(id);
		}
		let group = Group::new(api.mint_group_id(), id.clone());
		let (cols, rows) = (api.cols(), api.rows());
		match grid.cells.iter().enumerate().max_by_key(|(_, c)| c.w) {
			None => api.place(group, cols, rows),
			Some((i, widest)) => {
				let w = widest.w.div_euclid(2);
				if widest.w > w {
					api.resize(i, widest.w - w, widest.h);
				}
				api.place(group, w, widest.h);
			}
		}
	}

	/// Shows `id` in a tile of its own beside the widest, which gives up the room.
	pub(super) fn open_beside(&self, id: PanelId) {
		if self.lens_now().is_none() {
			return self.none_open();
		}
		let mut api = (*self.api.peek()).expect("a band is entered before anything can be opened");
		self.host(&id);
		let grid = serial::load(&api.save()).expect("a layout loads back from its own save");
		let group = Group::new(api.mint_group_id(), id.clone());
		match grid.cells.iter().enumerate().max_by_key(|(_, c)| c.w) {
			None => api.place(group, api.cols(), api.rows()),
			Some((i, widest)) => {
				let w = widest.w.div_euclid(2);
				api.resize(i, widest.w - w, widest.h);
				api.place(group, w, widest.h);
			}
		}
	}

	/// The dock's tabs `from` renamed `to`: a view kept under another name.
	fn retitle(&self, from: &PanelId, to: PanelId) {
		let mut api = (*self.api.peek()).expect("a band is entered before anything can be renamed");
		let mut grid = serial::load(&api.save()).expect("a layout loads back from its own save");
		let Some(tab) = grid.cells.iter_mut().flat_map(|c| c.group.tabs.iter_mut()).find(|t| *t == from) else {
			return; // not on screen
		};
		*tab = to.clone();
		self.host(&to);
		api.load(&serial::save(&grid)).expect("its own save, one tab renamed");
	}

	/// A sidebar item started a drag: wherever it drops on the dock, `id` lands.
	fn carry(&self, id: PanelId) {
		if self.lens_now().is_none() {
			return self.none_open();
		}
		let mut api = (*self.api.peek()).expect("a band is entered before the sidebar can be dragged from");
		self.host(&id);
		let (w, h) = size(&id);
		api.carry(id, w, h);
	}

	fn host(&self, id: &PanelId) {
		let mut panels = self.panels;
		if !panels.peek().iter().any(|p| &p.id == id) {
			let panel = self.panel(id.clone());
			panels.write().push(panel);
		}
	}

	/// The panel `id` names, its content looked up afresh: a layout holds ids only.
	fn panel(&self, id: PanelId) -> DockPanel {
		let (glyph, title, content) = match id.0.split_once(':') {
			Some(("view", name)) => (Glyph::View, name.to_string(), rsx! { view::ViewPanel { key: "{name}", name: name.to_string() } }),
			Some(("library", at)) => (Glyph::Search, "Library".to_string(), rsx! { library::LibraryPanel { at: at.to_string() } }),
			Some(("sprint", at)) => match sprint_at(at) {
				Some(link) => {
					return DockPanel {
						label: Some(rsx! { view::SprintTab { link: link.clone() } }),
						title: at.to_string(),
						content: rsx! { view::SprintPanel { link: link.clone() } },
						id,
						min: PANEL_MIN,
					};
				}
				None => (Glyph::Alert, id.0.clone(), rsx! { "Not a sprint: {at}" }),
			},
			Some(("issue", link)) => match IssueLink::parse(link) {
				Some(link) => {
					return DockPanel {
						label: Some(rsx! { issue::IssueTab { link: link.clone() } }),
						id,
						title: format!("#{}", link.number()),
						content: rsx! { issue::IssuePanel { link } },
						min: PANEL_MIN,
					};
				}
				None => (Glyph::Alert, id.0.clone(), rsx! { "Not an issue: {link}" }),
			},
			_ => (Glyph::Alert, id.0.clone(), rsx! { "Unknown panel {id.0}" }),
		};
		DockPanel {
			label: Some(self.tab_label(glyph, &title)),
			id,
			title,
			content,
			min: PANEL_MIN,
		}
	}

	/// A tab as the design draws it (Figma: Tab): what it holds, and its title.
	pub(super) fn tab_label(&self, glyph: Glyph, title: &str) -> Element {
		rsx! {
			span { class: "flex min-w-0 items-center gap-1.5",
				Icon { glyph, class: "size-3.5 shrink-0 text-ink-soft" }
				span { class: "truncate text-body", "{title}" }
			}
		}
	}

	/// Whether it landed; why not is toasted.
	async fn comment(self, link: IssueLink, edit: CommentEdit) -> bool {
		let key = format!("comment:{link}");
		match api::comment(link.clone(), edit).await {
			Ok(Edited::Landed) => return true,
			Ok(Edited::Conflict(conflict)) => self.clashed(link, conflict),
			Ok(Edited::Rejected(why)) => self.refused(key, link, format!("Comment refused: {why}")),
			Err(e) => self.say(key, toast::Tone::Error, e.to_string(), None),
		}
		false
	}

	fn clashed(self, link: IssueLink, conflict: Conflict) {
		let fields = conflict.clashes.iter().map(|c| c.field.to_string()).collect::<Vec<_>>().join(", ");
		let mut conflicts = self.conflicts;
		conflicts.write().insert(link.clone(), conflict);
		let resolve = Callback::new({
			let link = link.clone();
			move |()| self.open_issue(link.clone())
		});
		self.say(
			format!("clash:{link}"),
			toast::Tone::Error,
			format!("{fields} on #{} didn't go through: changed on GitHub meanwhile", link.number()),
			Some(("Resolve", resolve)),
		);
		self.open_issue(link);
	}

	//#[codestyle::skip(swappable-args)]
	fn refused(self, key: String, link: IssueLink, why: String) {
		let open = Callback::new(move |()| self.open_issue(link.clone()));
		self.say(key, toast::Tone::Error, why, Some(("Open", open)));
	}

	/// Shown at once, sent as it settles, in place of any edit of the same not sent yet.
	fn settle(self, edit: LibraryEdit) {
		let (mut unsaved, mut saving) = (self.unsaved, self.saving);
		{
			let mut unsaved = unsaved.write();
			unsaved.retain(|held| held.sent || !supersedes(&edit, &held.edit));
			unsaved.push(Unsaved { edit, sent: false });
		}
		if let Some(task) = saving.take() {
			task.cancel();
		}
		saving.set(Some(spawn_forever(async move {
			gloo_timers::future::sleep(SETTLES).await;
			spawn_forever(self.flush()); // what is being sent outlives the next edit's cancel
		})));
	}

	/// Everything unsaved, sent.
	async fn flush(self) {
		let wire = self.wire.peek().clone();
		let _sending = wire.lock().await;
		self.drain().await;
	}

	/// Whether it landed; why not is toasted. What was unsaved lands before it.
	async fn keep(self, edit: LibraryEdit) -> bool {
		let wire = self.wire.peek().clone();
		let _sending = wire.lock().await;
		self.drain().await;
		self.send_edit(edit).await
	}

	/// What is unsaved, handed to the browser as the page goes: it outlives the page, as no request of the page's own does.
	fn leave(self) {
		if self.unsaved.peek().is_empty() {
			return;
		}
		let body = web_sys::js_sys::Array::of1(&leaving(&self.unsaved.peek()).into());
		let json = web_sys::BlobPropertyBag::new();
		json.set_type("application/json");
		let body = web_sys::Blob::new_with_str_sequence_and_options(&body, &json).expect("a blob of one string");
		let url = format!("{}/api/library/left", dioxus_fullstack::get_server_url());
		let navigator = web_sys::window().expect("a browser window").navigator();
		navigator.send_beacon_with_opt_blob(&url, Some(&body)).expect("a same-origin url");
	}

	async fn drain(self) {
		let mut unsaved = self.unsaved;
		while let Some(edit) = self.next_unsaved() {
			self.send_edit(edit).await;
			unsaved.write().remove(0); // only the wire's holder takes one off, and what is sent stays first
		}
	}

	fn next_unsaved(self) -> Option<LibraryEdit> {
		let mut unsaved = self.unsaved;
		if unsaved.peek().is_empty() {
			return None;
		}
		let mut unsaved = unsaved.write();
		let next = &mut unsaved[0];
		next.sent = true;
		Some(next.edit.clone())
	}

	async fn send_edit(self, edit: LibraryEdit) -> bool {
		match api::edit_library(edit).await {
			Ok(library) => {
				let mut l = self.library;
				l.set(Some(library));
				if self.workspace.peek().is_some() && self.open_workspace().is_none() {
					let (mut workspace, mut seen) = (self.workspace, self.seen);
					workspace.set(None); // dropped
					seen.set(None);
					if let Some(mut api) = *self.api.peek() {
						api.reset();
					}
				}
				true
			}
			Err(e) => {
				self.say("library", toast::Tone::Error, e.to_string(), None);
				false
			}
		}
	}

	/// What the viewer did to the dock's tiles, to the workspace it was done in.
	fn keep_tiles(self, (band, layout): (Band, String)) {
		let workspace = self.open_workspace().expect("a dock is kept only while a workspace is open");
		self.settle(LibraryEdit::Workspace {
			workspace,
			part: WorkspacePart::Tiles { band, layout },
		});
	}

	/// The open workspace's tiles onto the dock: the one the address names, else the one last selected.
	fn arrange(self, api: PackedApi) {
		let (true, Some(library)) = (self.setup.peek().is_some(), self.library.peek().clone()) else {
			return; // `Live` arranges once both arrived
		};
		if let Some(shared) = self.viewing.peek().clone() {
			return workspace::view_shared(self, shared); // a band change keeps the link on the dock
		}
		let (mut landing, mut workspace) = (self.landing, self.workspace);
		let to = self.at.peek().clone();
		if *landing.peek() {
			let named = to.workspace().filter(|id| matches!(self.named(id), Some(Item::Workspace(_))));
			workspace.set(named.or_else(|| library.workspace.and_then(|name| self.id_of(&Item::Workspace(name)))));
		}
		let open = self.open_workspace().and_then(|name| self.workspace_named(&name));
		self.lay_out(api, open.map(|w| w.tiles));
		if landing.take() {
			if to != route::Address::default() {
				spawn_forever(self.reach(to));
			}
		} else {
			spawn_forever(self.reach(to.issues_only())); // they float above any band's tiles
		}
	}

	/// The band's of `tiles` on the dock, an empty one when it has none; none: no workspace's, nothing of it kept.
	fn lay_out(self, mut api: PackedApi, tiles: Option<Arrangement>) {
		let mut seen = self.seen;
		seen.set(None);
		api.reset();
		let Some(tiles) = tiles else { return };
		if tiles.get(&api.band()).is_none_or(|layout| self.show(api, layout)) {
			seen.set(Some(Seen::of(api)));
		}
	}

	/// `layout` on the dock, its panels hosted first, cut to the rows this window shows: a row is a fixed height,
	/// so a layout kept on a taller window would scroll here.
	fn show(self, api: PackedApi, layout: &str) -> bool {
		self.load(api, layout, |grid| {
			without_floating_issues(grid);
			for id in grid.cells.iter().flat_map(|c| c.group.tabs.iter()) {
				self.host(id);
			}
			cut(grid, api.rows());
		})
	}

	/// `layout`, as `fit` leaves it, on `api`'s dock; said so when it doesn't load.
	fn load(self, mut api: PackedApi, layout: &str, fit: impl FnOnce(&mut PackedGrid)) -> bool {
		match serial::load(layout) {
			Ok(mut grid) => {
				fit(&mut grid);
				api.load(&serial::save(&grid)).expect("parsed above");
				true
			}
			Err(e) => {
				self.say(
					"layout",
					toast::Tone::Error,
					format!("the kept layout doesn't load ({e:?}); nothing kept is overwritten until another is shown"),
					None,
				);
				false
			}
		}
	}

	/// Workspace `name` in place of the one open.
	async fn switch(self, name: &str) {
		let api = (*self.api.peek()).expect("a band is entered before a workspace can be switched to");
		notice(api, self.seen, |made| self.keep_tiles(made)); // before the dock is another's
		let (mut viewing, mut workspace) = (self.viewing, self.workspace);
		viewing.set(None);
		workspace.set(Some(self.id_of(&Item::Workspace(name.to_string())).expect("offered from the library or the team's")));
		let Workspace { tiles, .. } = self.workspace_named(name).expect("it has an id");
		self.lay_out(api, Some(tiles));
		if self.library.peek().as_ref().expect("read above").workspace.as_deref() != Some(name) {
			self.keep(LibraryEdit::Open(name.to_string())).await;
		}
	}

	async fn new_workspace(self) {
		if self.keep(LibraryEdit::NewWorkspace).await {
			let library = self.library.peek().clone().expect("its edit just landed");
			self.go(self.at_workspace(&library.workspace.expect("it selected the new one")));
		}
	}
}

/// A scope, and where it sits.
struct ScopeAt {
	path: String,
	depth: usize,
	scope: Scope,
}
impl ScopeAt {
	fn name(&self) -> &str {
		self.path.rsplit(" / ").next().expect("split yields one")
	}
}

/// A library edit shown over the library until the server holds it.
#[derive(Clone, PartialEq)]
struct Unsaved {
	edit: LibraryEdit,
	sent: bool,
}

/// What a page leaves: an edit in flight with it, as the page's end may cut it off.
fn leaving(unsaved: &[Unsaved]) -> String {
	let edits: Vec<&LibraryEdit> = unsaved.iter().map(|u| &u.edit).collect();
	serde_json::json!({ "edits": edits }).to_string()
}

/// `edit` sets all that `held` did.
fn supersedes(edit: &LibraryEdit, held: &LibraryEdit) -> bool {
	use std::mem::discriminant;
	match (edit, held) {
		(LibraryEdit::Workspace { workspace, part }, LibraryEdit::Workspace { workspace: w, part: p }) =>
			workspace == w
				&& match (part, p) {
					(WorkspacePart::Tiles { band, .. }, WorkspacePart::Tiles { band: b, .. }) => band == b,
					_ => discriminant(part) == discriminant(p),
				},
		(LibraryEdit::View { view, part }, LibraryEdit::View { view: v, part: p }) =>
			view == v
				&& match (part, p) {
					(ViewPart::Arrangement { of, band, .. }, ViewPart::Arrangement { of: o, band: b, .. }) => (of, band) == (o, b),
					_ => discriminant(part) == discriminant(p),
				},
		_ => false,
	}
}

/// The workspace `name` as last edited.
fn workspace_named(setup: &Option<Setup>, library: &Option<Library>, unsaved: &[Unsaved], name: &str) -> Option<Workspace> {
	let mut workspace = library.as_ref()?.workspace(&setup.as_ref()?.workspaces, name)?;
	for edit in unsaved {
		match &edit.edit {
			LibraryEdit::Workspace { workspace: edited, part } if edited == name => workspace.set(part.clone()),
			_ => {}
		}
	}
	Some(workspace)
}

#[derive(Clone, Debug, PartialEq)]
enum Modal {
	Account,
	Scope(Editing),
	/// The stored issues the board couldn't read, and why.
	Unreadable,
	/// The keys, the dock's and the board's.
	Help,
	/// The viewer's object as a pull request on the team's config.
	Propose(Item),
}

#[derive(Clone, Debug, PartialEq)]
enum Editing {
	/// Starting as this.
	New(Scope),
	/// One of the viewer's, by name.
	Own(String),
	/// The team's, by path: saved, it becomes the viewer's own.
	Team(String),
}

/// A sprint's panel, named by its milestone: titles repeat across repos.
fn sprint_panel(link: &MilestoneLink) -> PanelId {
	PanelId(format!("sprint:{}/{}", link.repo_info(), link.number()))
}

/// The milestone an `owner/repo/n` names.
fn sprint_at(at: &str) -> Option<MilestoneLink> {
	let [owner, repo, n] = at.split('/').collect::<Vec<_>>().try_into().ok()?; //IGNORED_ERROR: it names no milestone
	MilestoneLink::parse(&format!("https://github.com/{owner}/{repo}/milestone/{n}"))
}

/// What a tile wants when it opens.
fn size(id: &PanelId) -> (u32, u32) {
	match id.0.split_once(':').map(|(kind, _)| kind) {
		Some("sprint") => (24, 18),
		Some("issue") => (40, 14),
		_ => (40, 18),
	}
}

/// The issues the address holds: those floating above the dock.
fn floating_issues(grid: &PackedGrid) -> Vec<PanelId> {
	let tabs = grid.floats.iter().flat_map(|f| f.group.tabs.iter());
	tabs.filter(|p| p.0.starts_with("issue:")).cloned().collect()
}

fn without_floating_issues(grid: &mut PackedGrid) {
	for id in floating_issues(grid) {
		grid.close(&id);
	}
}

/// The dock as a library keeps it.
fn kept(api: PackedApi) -> PackedGrid {
	let mut grid = serial::load(&api.save()).expect("a layout loads back from its own save");
	without_floating_issues(&mut grid);
	grid
}

/// Its tiles no lower than `rows`.
fn cut(grid: &mut PackedGrid, rows: u32) {
	for cell in grid.cells.iter_mut().filter(|c| c.y < rows && c.y + c.h > rows) {
		cell.h = rows - cell.y;
	}
}

/// A dock as the viewer laid it out: free of what this window's size alone decides.
#[derive(Clone, PartialEq)]
struct Seen {
	band: Band,
	grid: PackedGrid,
}
impl Seen {
	fn of(api: PackedApi) -> Self {
		let mut grid = kept(api);
		for c in &mut grid.cells {
			c.w = c.desired_w.take().unwrap_or(c.w); // unclamped: as wanted
			c.x = c.desired_x.take().unwrap_or(c.x);
			c.h = c.desired_h.take().unwrap_or(c.h);
			c.y = 0; // follows from the rest
		}
		Self { band: api.band(), grid }
	}
}

/// What `api`'s dock shows, when the viewer changed it since it was last `seen`, at rest.
fn notice(api: PackedApi, mut seen: Signal<Option<Seen>>, made: impl FnOnce((Band, String))) {
	let grid = kept(api);
	if grid.state != GridState::Settled {
		return;
	}
	let now = Seen::of(api);
	let changed = seen.peek().as_ref().is_some_and(|was| was.band == now.band && was.grid != now.grid);
	if changed {
		seen.set(Some(now));
		made((api.band(), serial::save(&grid)));
	}
}

/// The dock's heights as `kept` has them, cut to the rows it shows now: the bars above it and the window come and go.
fn recut(mut api: PackedApi, mut seen: Signal<Option<Seen>>, kept: Option<String>) {
	let rows = api.rows();
	if rows == 0 || *seen.peek() != Some(Seen::of(api)) {
		return; // out of sight, or as the viewer is making it
	}
	let held = kept.map(|layout| serial::load(&layout).expect("it was shown"));
	let mut grid = serial::load(&api.save()).expect("a layout loads back from its own save");
	let was = grid.clone();
	for cell in &mut grid.cells {
		let held = held.iter().flat_map(|g| &g.cells).find(|c| c.group.id == cell.group.id);
		cell.h = held.map_or(rows, |c| c.desired_h.unwrap_or(c.h)); // not among those kept: seeded, at full height
		cell.desired_h = None;
	}
	cut(&mut grid, rows);
	if grid != was {
		api.load(&serial::save(&grid)).expect("its own save, heights changed");
		seen.set(Some(Seen::of(api)));
	}
}

/// What the viewer does to `api`'s dock, once `seen`, goes to `made`; until then it shows what is `kept` for its band.
fn use_kept(api: Signal<Option<PackedApi>>, seen: Signal<Option<Seen>>, kept: Callback<Band, Option<String>>, made: Callback<(Band, String)>) {
	use_effect(move || {
		if let Some(api) = *api.read() {
			notice(api, seen, |layout| made.call(layout));
		}
	});
	let rows = use_memo(move || (*api.read()).map(|a| a.rows()));
	use_effect(move || {
		if rows().is_some() {
			let api = (*api.peek()).expect("its rows were read");
			notice(api, seen, |layout| made.call(layout));
			recut(api, seen, kept.call(api.band()));
		}
	});
}

#[component]
fn Shell() -> Element {
	// the root's: what settles outlives whoever made it
	let ctx = use_context_provider(|| {
		Runtime::current().in_scope(ScopeId::ROOT, || {
			let (stored, pending) = (Signal::new(Board::default()), Signal::new(Vec::new()));
			let (setup, library, unsaved) = (Signal::new(None), Signal::new(None), Signal::new(Vec::new()));
			let (workspace, viewing) = (Signal::new(None), Signal::new(None));
			Ctx {
				setup,
				library,
				unsaved,
				saving: Signal::new(None),
				wire: Signal::new(Rc::default()),
				workspace,
				lens: Memo::new(move || lens(&setup.read(), &library.read(), &unsaved.read(), &workspace.read(), &viewing.read())),
				stored,
				pending,
				made: Signal::new(HashMap::new()),
				reading: Signal::new(Reading::Idle),
				board: Memo::new(move || sync::shown(&stored.read(), &pending.read())),
				conflicts: Signal::new(HashMap::new()),
				panels: Signal::new(Vec::new()),
				api: Signal::new(None),
				seen: Signal::new(None),
				loading: Signal::new(Loading::default()),
				progress: Signal::new(Loading::default()),
				wanted: Signal::new(BTreeMap::new()),
				order: Signal::new(Vec::new()),
				failure: Signal::new(None),
				modal: Signal::new(None),
				palette: Signal::new(false),
				into: Signal::new(None),
				sheet: Signal::new(false),
				notes: Signal::new(Vec::new()),
				landing: Signal::new(true),
				viewing,
				at: Signal::new(route::Address::default()),
				main: Signal::new(None),
				up: Signal::new(Vec::new()),
				moving: Signal::new(0),
				settling: Signal::new(true),
				relaid: Signal::new(HashMap::new()),
			}
		})
	});
	use_hook(|| match route::landed() {
		Ok(to) => {
			let mut at = ctx.at;
			at.set(to);
		}
		Err(e) => ctx.say("address", toast::Tone::Error, e, None),
	});
	route::use_address(ctx);
	use_hook(move || {
		use wasm_bindgen::{JsCast as _, closure::Closure};
		let window = web_sys::window().expect("a browser window");
		let document = window.document().expect("a page has one");
		let runtime = Runtime::current();
		let gone = Closure::<dyn FnMut(web_sys::Event)>::new(move |e: web_sys::Event| {
			let shown = !web_sys::window().expect("a browser window").document().expect("a page has one").hidden();
			if e.type_() == "visibilitychange" && shown {
				return;
			}
			ctx.leave();
			if e.type_() == "visibilitychange" {
				// a phone ends a hidden page without a word; one that comes back must not send what is old by then
				runtime.in_scope(ScopeId::ROOT, || spawn_forever(ctx.flush()));
			}
		});
		window
			.add_event_listener_with_callback("pagehide", gone.as_ref().unchecked_ref())
			.expect("a window takes listeners");
		document
			.add_event_listener_with_callback("visibilitychange", gone.as_ref().unchecked_ref())
			.expect("a document takes listeners");
		gone.forget(); // the shell never unmounts
	});
	let unsent = use_memo(move || sync::unsent(&ctx.pending.read()) || leaving(&ctx.unsaved.read()).len() > CARRIED);
	use_effect(move || {
		document::eval(match unsent() {
			true => "window.onbeforeunload = (e) => e.preventDefault()",
			false => "window.onbeforeunload = null",
		});
	});
	let kept = use_callback(move |band| ctx.open_workspace().and_then(|name| ctx.workspace_named(&name)?.tiles.remove(&band)));
	use_kept(ctx.api, ctx.seen, kept, use_callback(move |made| ctx.keep_tiles(made)));
	use_hook(started);
	rsx! {
		Live {}
		div { class: "flex h-svh overflow-hidden bg-background text-ink",
			shell::Nav {}
			div { class: "flex min-w-0 flex-1 flex-col",
				shell::TopBar {}
				workspace::Banner {}
				filter::FilterBar {}
				div { class: "relative min-h-0 flex-1",
					if let Some(why) = ctx.failure.read().clone() {
						div { class: "absolute inset-0 z-10 flex items-start justify-center bg-background p-8",
							view::Empty { glyph: icons::Glyph::Alert, error: true, title: "The board didn't load", body: why }
						}
					}
					div { class: "absolute inset-0",
						PackedArea {
							panels: ctx.panels,
							on_band: Some(Callback::new(move |api: PackedApi| {
								let (mut handle, mut seen) = (ctx.api, ctx.seen);
								handle.set(Some(api));
								seen.set(None);
								ctx.arrange(api);
							})),
							config: None,
							actions: Some(Callback::new(|tile: Tile| rsx! { TileActions { tile } })),
							on_add: Some(Callback::new(move |group: GroupId| {
								let (mut into, mut p) = (ctx.into, ctx.palette);
								into.set(Some(group));
								p.set(true);
							})),
						}
					}
				}
				shell::ThumbBar {}
			}
		}
		Modals {}
		palette::Palette {}
		toast::Toasts {}
	}
}

fn lens(setup: &Option<Setup>, library: &Option<Library>, unsaved: &[Unsaved], workspace: &Option<String>, viewing: &Option<api::Shared>) -> Option<Lens> {
	if let Some(shared) = viewing {
		let api::Construction::Workspace(shared) = &shared.construction else {
			panic!("only a workspace takes the dock: {:?}", shared.link)
		};
		return Some(shared.lens.clone());
	}
	let id = workspace.as_ref()?;
	match library.as_ref()?.ids.get(id).or_else(|| setup.as_ref()?.ids.get(id))? {
		Item::Workspace(name) => Some(workspace_named(setup, library, unsaved, name)?.lens),
		other => panic!("only a workspace is open: {other:?}"),
	}
}

/// Setup and the library, then the board, reloaded on every change the server announces.
#[component]
fn Live() -> Element {
	let ctx = use_context::<Ctx>();
	use_future(move || async move {
		let mut failure = ctx.failure;
		match futures::join!(api::setup(), api::library()) {
			(Ok(setup), Ok(library)) => {
				let (mut s, mut l) = (ctx.setup, ctx.library);
				s.set(Some(setup));
				l.set(Some(library));
				gloo_timers::future::sleep(std::time::Duration::ZERO).await; // the shelves show before the tiles are drawn
				if let Some(api) = *ctx.api.peek() {
					ctx.arrange(api);
				}
			}
			(Err(e), _) => return failure.set(Some(format!("setup: {e}"))),
			(_, Err(e)) => return failure.set(Some(format!("your library: {e}"))),
		}
		gloo_timers::future::sleep(std::time::Duration::ZERO).await; // the tiles say what they show as they mount
		loop {
			match api::changes().await {
				Ok(mut ticks) => {
					reload(ctx).await;
					while let Some(tick) = ticks.next().await {
						match tick {
							Ok(()) => reload(ctx).await,
							Err(e) => ctx.say("live", toast::Tone::Error, format!("live updates: {e}"), None),
						}
					}
				}
				Err(e) => ctx.say("live", toast::Tone::Error, format!("live updates: {e}"), None),
			}
			gloo_timers::future::sleep(std::time::Duration::from_secs(3)).await; // the server restarted or the network dropped: reconnect
		}
	});
	rsx! {}
}

/// The server's board being read: one read at a time, so none older replaces a newer.
#[derive(Clone, Copy, PartialEq)]
enum Reading {
	Idle,
	Once,
	/// It may have changed after the read began.
	Again,
}

async fn reload(ctx: Ctx) {
	let (mut stored, mut pending, mut reading, mut loading) = (ctx.stored, ctx.pending, ctx.reading, ctx.loading);
	if *reading.peek() != Reading::Idle {
		return reading.set(Reading::Again);
	}
	//LOOP rereads while a refresh was requested mid-read
	loop {
		reading.set(Reading::Once);
		let held = sync::landed(&pending.peek());
		let first = !loading.peek().whole();
		match read(ctx, first).await {
			Ok((board, loaded)) => {
				stored.set(board);
				if first {
					let mut progress = ctx.progress;
					progress.set(loaded.clone());
					loading.set(loaded);
				}
				pending.write().retain(|p| !held.contains(&p.id));
			}
			Err(e) if first => loading.write().failed = Some(e),
			Err(e) => ctx.say("board", toast::Tone::Error, format!("board: {e}"), None),
		}
		if *reading.peek() == Reading::Once {
			return reading.set(Reading::Idle);
		}
	}
}

/// What of the first board is on this page, in bytes of the store it is read from.
#[derive(Clone, Debug, Default, PartialEq)]
struct Loading {
	sources: BTreeMap<Source, Share>,
	ranked: bool,
	failed: Option<String>,
}
impl Loading {
	fn whole(&self) -> bool {
		self.ranked && self.sources.values().all(|s| s.loaded == s.of)
	}

	/// Of the sources `needed`: how much is here, or `None` once all is.
	fn percent(&self, needed: impl Fn(&Source) -> bool) -> Option<u64> {
		let (mut loaded, mut of, mut ranked) = (0, 0, true);
		for (source, share) in self.sources.iter().filter(|(s, _)| needed(s)) {
			loaded += share.loaded;
			of += share.of;
			ranked &= self.ranked || source.holds != Holds::OpenIssues;
		}
		match self.sources.is_empty() || loaded < of || !ranked {
			true => Some((100 * loaded).checked_div(of).unwrap_or(0).min(99)), // nothing sized yet; 100 is the view
			false => None,
		}
	}

	fn add(&mut self, part: &Part) {
		match part {
			Part::Head { sizes, .. } => self.sources = sizes.iter().map(|(s, of)| (s.clone(), Share { loaded: 0, of: *of })).collect(),
			Part::Read { source, bytes, .. } => self.sources.get_mut(source).expect("the head sized every source").loaded += bytes,
			Part::Ranks(_) => self.ranked = true,
		}
	}
}

#[derive(Clone, Copy, Debug, Default, PartialEq)]
struct Share {
	loaded: u64,
	of: u64,
}

/// What a tile shows.
#[derive(Clone, Debug, PartialEq)]
enum Need {
	Query(Query),
	Cards(Vec<IssueLink>),
}
impl Need {
	fn covers(&self, source: &Source) -> bool {
		match self {
			Need::Query(q) => q.needs(source),
			Need::Cards(links) => links.iter().any(|l| l.project().to_string() == source.repo),
		}
	}

	/// How much of it is in `board`, of which `loaded` is; `None` once all of it.
	fn percent(&self, loaded: &Loading, board: &Board) -> Option<u64> {
		match self {
			Need::Query(_) => loaded.percent(|s| self.covers(s)),
			Need::Cards(links) => {
				let missing = links.iter().filter(|l| !board.cards.iter().any(|c| c.link == **l)).count();
				let of = links.len();
				(missing > 0 && loaded.percent(|s| self.covers(s)).is_some()).then(|| (100 * (of - missing)).div_euclid(of) as u64)
			}
		}
	}
}

/// The server's board, what the screen shows before the rest. The `first` read shows it as it comes: each tile's as
/// soon as it is all here, the rest as often as showing it takes little of the time.
async fn read(ctx: Ctx, first: bool) -> Result<(Board, Loading), String> {
	let (mut stored, mut loading, mut progress, mut order) = (ctx.stored, ctx.loading, ctx.progress, ctx.order);
	let window = web_sys::window().expect("runs in a browser");
	let performance = window.performance().expect("a browser's window has one");
	let document = window.document().expect("a page has one");
	if first {
		let wanted = ctx.wanted.peek();
		let on_screen = wanted.values().flat_map(|(_, need)| match need {
			Need::Query(q) => vec![q.to_string()],
			Need::Cards(links) => links.iter().map(|l| format!("repo:{}", l.project())).collect(),
		});
		order.set(on_screen.collect());
	}
	let mut chunks = api::board(order.peek().clone()).await.map_err(|e| e.to_string())?; // the same on each read: so is the cards' order
	let (mut board, mut loaded) = (Board::default(), Loading::default());
	let mut pending = Vec::new(); // a line not all here yet
	let began = performance.now();
	let (mut drawn, mut next) = (began, began + 200.0);
	let (mut at, mut done) = (HashMap::<u64, u64>::new(), HashSet::new());
	while let Some(chunk) = chunks.next().await {
		pending.extend_from_slice(&chunk.map_err(|e| e.to_string())?);
		let end = pending.iter().rposition(|b| *b == b'\n').map_or(0, |at| at + 1); // no line ended in it
		for line in pending.drain(..end).collect::<Vec<u8>>().split_inclusive(|b| *b == b'\n') {
			let part: Part = serde_json::from_slice(line).map_err(|e| e.to_string())?;
			loaded.add(&part);
			let came: Vec<IssueLink> = match &part {
				Part::Read { cards, .. } => cards.iter().map(|c| c.link.clone()).collect(),
				Part::Head { .. } | Part::Ranks(_) => Vec::new(),
			};
			board.extend([part]);
			if !first {
				continue;
			}
			let mut ready = Vec::new();
			for (id, (what, need)) in ctx.wanted.peek().iter() {
				let touched = match need {
					Need::Query(_) => true,
					Need::Cards(links) => links.iter().any(|l| came.contains(l)),
				};
				if !touched || done.contains(id) {
					continue;
				}
				match need.percent(&loaded, &board) {
					Some(percent) => _ = at.insert(*id, percent),
					None => {
						done.insert(*id);
						let counted = matches!(need, Need::Cards(_)); // a card is a step of its own, however few they are
						ready.extend(at.remove(id).map(|percent| (what.clone(), percent, counted)));
					}
				}
			}
			let now = performance.now();
			if !ready.is_empty() || now > next {
				stored.set(board.clone());
				loading.set(loaded.clone());
				progress.set(loaded.clone());
			} else if now - drawn > 16.0 {
				progress.set(loaded.clone());
			} else {
				continue;
			}
			if document.hidden() {
				continue; // its timers are slowed, and nothing is drawn
			}
			gloo_timers::future::sleep(std::time::Duration::ZERO).await;
			drawn = performance.now();
			if !ready.is_empty() || now > next {
				next = drawn + 4.0 * (drawn - now);
			}
			for (what, percent, counted) in ready {
				let late = (100.0 * (drawn - now) / (drawn - began)) as u64;
				let early = 99 - percent > 5 && !counted;
				if drawn - began > NOTICED && early || drawn - now > NOTICED && late > 5 {
					spawn_forever(async move { _ = api::off(what, percent, late).await }); // a report that doesn't reach the server changes nothing here
				}
			}
		}
		if first {
			progress.set(loaded.clone()); // the next may be long in coming
		}
	}
	match loaded.whole() {
		true => Ok((board, loaded)),
		false => Err("it was cut short".to_string()),
	}
}

/// Buttons a tile's header adds beside tilers' own (Figma: TileBar): split the shown panel off, maximize.
#[component]
fn TileActions(tile: Tile) -> Element {
	let ctx = use_context::<Ctx>();
	let Tile {
		group, active, maximized, floating, ..
	} = tile;
	if ctx.band() == Some(Band::Sm) || floating {
		return rsx! {}; // one tile at a time, or above them: nothing to split or maximize beside
	}
	let tabs = {
		let api = (*ctx.api.read()).expect("a tile shows on an entered dock");
		let grid = serial::load(&api.save()).expect("a layout loads back from its own save");
		grid.cells.into_iter().find(|c| c.group.id == group).map(|c| c.group.tabs).expect("a docked tile is a cell")
	};
	let api = move || (*ctx.api.peek()).expect("a tile shows on an entered dock");
	rsx! {
		if tabs.len() > 1 {
			kit::IconBtn {
				glyph: Glyph::Split,
				title: "Split it off beside",
				onclick: {
					let active = active.clone();
					move |_| {
						let mut a = api();
						a.close(&active);
						let (w, h) = size(&active);
						let g = a.mint_group_id();
						a.place(Group::new(g, active.clone()), w, h);
					}
				},
			}
		}
		kit::IconBtn { glyph: Glyph::Maximize, title: if maximized { "Restore" } else { "Maximize" }, active: maximized, onclick: move |_| api().maximize(group) }
	}
}

/// Over everything the dock draws, its grips included.
#[component]
fn Modals() -> Element {
	let ctx = use_context::<Ctx>();
	let close = move |v: bool| {
		if !v {
			let mut m = ctx.modal;
			m.set(None);
		}
	};
	let modal = ctx.modal.read().clone();
	rsx! {
		div { class: "relative z-[100]",
			match modal {
				None => rsx! {},
				Some(Modal::Account) => rsx! { account::AccountPanel {} },
				Some(Modal::Scope(editing)) => rsx! { scope::ScopeEditor { key: "{editing:?}", editing } },
				Some(Modal::Propose(item)) => rsx! { library::Propose { key: "{item:?}", item } },
				Some(Modal::Help) => rsx! {
					Dialog { open: true, on_open_change: close,
						DialogContent { class: "flex flex-col gap-3 sm:max-w-md",
							DialogHeader { DialogTitle { "Keys" } }
							for (keys , what) in HELP {
								div { key: "{keys}", class: "flex items-center gap-3 text-body",
									span { class: "flex-1 text-ink-mid", "{what}" }
									kit::Kbd { "{keys}" }
								}
							}
						}
					}
				},
				Some(Modal::Unreadable) => {
					let board = ctx.board.read();
					let files = board.unreadable.clone();
					rsx! {
						Dialog { open: true, on_open_change: close,
							DialogContent { class: "max-h-[80vh] overflow-y-auto sm:max-w-4xl",
								DialogHeader { DialogTitle { "Stored issues that don't parse" } }
								for (path , why) in files {
									div { key: "{path.display()}", class: "flex flex-col gap-1 border-t pt-2",
										code { class: "break-all text-xs", "{path.display()}" }
										pre { class: "min-w-0 overflow-x-auto whitespace-pre text-xs text-ink-soft", "{plain(&why)}" }
									}
								}
							}
						}
					}
				}
			}
		}
	}
}

/// `text` without its terminal colors.
fn plain(text: &str) -> String {
	let mut out = String::with_capacity(text.len());
	let mut chars = text.chars();
	while let Some(c) = chars.next() {
		if c == '\x1b' {
			chars.by_ref().find(|c| c.is_ascii_alphabetic());
		} else {
			out.push(c);
		}
	}
	out
}
