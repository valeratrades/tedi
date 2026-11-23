//! The client: a sidebar of workspaces, views, scopes and sprints over a tiled workspace, every tile a view or
//! an issue. Cards arrive whole and are queried here; edits leave as server fns. What is on screen, and what
//! the viewer keeps, is their library on the server.

mod card;
mod facets;
mod icons;
mod issue;
mod kit;
mod library;
mod toolbar;
mod view;

use std::{
	collections::{BTreeMap, HashMap},
	str::FromStr as _,
};

use dioxus::prelude::*;
use ev_lib::uikit_dioxus::*;
use futures::StreamExt as _;
use tedi_core::{
	IssueLink,
	board::{Board, Card, GroupBy, Layout, Query, Scope, Shown, Sort, SortKey, View, scope_at},
	conflict::{Conflict, FieldEdit},
	workflow::{State, Workflow},
};
use tilers_dioxus::{Band, DockPanel, Group, GroupId, MinSize, PackedApi, PackedArea, PanelId, model::serial};

use crate::api::{self, Dock, Edited, Favorite, Kept, Library, LibraryEdit, Setup, Shelf};

#[component]
pub fn App() -> Element {
	rsx! {
		Fonts {}
		ToasterProvider {
			Toaster { position: ToastPosition::BottomRight }
			Shell {}
		}
	}
}

/// Shared by every tile.
#[derive(Clone, Copy)]
struct Ctx {
	setup: Signal<Option<Setup>>,
	library: Signal<Option<Library>>,
	board: Signal<Option<Board>>,
	/// Edits that came back clashing, shown on their issue until answered.
	conflicts: Signal<HashMap<IssueLink, Conflict>>,
	panels: Signal<Vec<DockPanel>>,
	api: Signal<Option<PackedApi>>,
	/// The band the dock was last arranged for from the library: until then, what it shows isn't the viewer's to keep.
	arranged: Signal<Option<Band>>,
	/// Why setup or the library didn't arrive: nothing renders without both.
	failure: Signal<Option<String>>,
	/// The dialog over the screen.
	modal: Signal<Option<Modal>>,
	toaster: ToasterHandle,
}
impl Ctx {
	fn card(&self, link: &IssueLink) -> Option<Card> {
		self.board.read().as_ref()?.cards.iter().find(|c| &c.link == link).cloned()
	}

	/// What cards sit in, by name.
	fn workflows(&self) -> BTreeMap<String, Workflow> {
		self.setup.read().as_ref().expect("tiles render once setup arrived").workflows.clone()
	}

	fn me(&self) -> Option<String> {
		self.setup.read().as_ref()?.me.clone()
	}

	/// The team views, then the viewer's own.
	fn views(&self) -> Vec<View> {
		let setup = self.setup.read();
		let library = self.library.read();
		let team = setup.as_ref().map(|s| s.views.clone()).into_iter().flatten();
		team.chain(library.as_ref().map(|l| l.filters.values().cloned().collect::<Vec<_>>()).into_iter().flatten())
			.collect()
	}

	/// The scope everything on screen is narrowed to: its path, and its filter while it is still configured.
	fn scope(&self) -> Option<(String, Option<String>)> {
		let path = self.library.read().as_ref()?.scope.clone()?;
		let at = scope_at(&self.scopes(), &path);
		Some((path, at))
	}

	/// The team's, then the viewer's own.
	fn scopes(&self) -> BTreeMap<String, Scope> {
		let library = self.library.read();
		let setup = self.setup.read();
		library
			.as_ref()
			.expect("read once the library arrived")
			.scopes(&setup.as_ref().expect("arrives with the library").scopes)
	}

	fn favorite(&self, what: &Favorite) -> bool {
		self.library.read().as_ref().is_some_and(|l| l.favorites.contains(what))
	}

	fn open_issue(&self, link: IssueLink) {
		self.open(PanelId(format!("issue:{link}")));
	}

	/// Shows `id`, hosting it first if it isn't: as a tab of the largest tile holding its kind (issues one,
	/// views and sprints the other), else beside the widest tile, which gives up the room.
	fn open(&self, id: PanelId) {
		let mut api = (*self.api.peek()).expect("a band is entered before anything can be opened");
		self.host(&id);
		let mut grid = serial::load(&api.save()).expect("a layout loads back from its own save");
		if let Some(cell) = grid.cells.iter_mut().find(|c| c.group.tabs.contains(&id)) {
			cell.group.active = cell.group.tabs.iter().position(|t| *t == id).expect("found in it");
			api.load(&serial::save(&grid)).expect("its own save, another tab shown");
			return;
		}
		let issue = |p: &PanelId| p.0.starts_with("issue:");
		if let Some(cell) = grid.cells.iter().filter(|c| c.group.tabs.iter().any(|t| issue(t) == issue(&id))).max_by_key(|c| c.w * c.h) {
			api.add_tab(cell.group.id, id.clone());
			return self.open(id);
		}
		let group = Group::new(api.mint_group_id(), id.clone());
		let (cols, rows) = (api.cols(), api.rows());
		match grid.cells.iter().enumerate().max_by_key(|(_, c)| c.w) {
			None => api.place(group, cols, rows),
			Some((i, widest)) => {
				let w = if issue(&id) { cols * 3 / 8 } else { widest.w / 2 };
				if widest.w > w {
					api.resize(i, widest.w - w, widest.h);
				}
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
		let setup = self.setup.peek();
		let setup = setup.as_ref().expect("panels are hosted once setup arrived");
		let (title, content) = match id.0.split_once(':') {
			Some(("view", name)) => (name.to_string(), rsx! { view::ViewPanel { name: name.to_string() } }),
			Some(("sprint", title)) => match setup.sprints.iter().find(|s| s.title == title) {
				Some(sprint) => (title.to_string(), rsx! { view::SprintPanel { sprint: sprint.clone() } }),
				None => (title.to_string(), rsx! { "The sprint {title} is closed" }),
			},
			Some(("issue", link)) => match IssueLink::parse(link) {
				Some(link) => {
					let title = self.card(&link).map_or_else(|| format!("#{}", link.number()), |c| format!("#{} {}", link.number(), c.title));
					(title, rsx! { issue::IssuePanel { link } })
				}
				None => (id.0.clone(), rsx! { "Not an issue: {link}" }),
			},
			_ => (id.0.clone(), rsx! { "Unknown panel {id.0}" }),
		};
		DockPanel {
			id,
			title,
			content,
			min: MinSize::Rem { w: 16.0, h: 8.0 },
		}
	}

	/// `edits` to `link`, as seen on its card: a clash lands on the issue's tab, inline.
	async fn edit(self, link: IssueLink, edits: Vec<FieldEdit>) {
		match api::edit(link.clone(), edits).await {
			Ok(Edited::Landed) => {
				let mut conflicts = self.conflicts;
				conflicts.write().remove(&link);
			}
			Ok(Edited::Conflict(conflict)) => {
				let mut conflicts = self.conflicts;
				conflicts.write().insert(link.clone(), conflict);
				self.open_issue(link);
			}
			Ok(Edited::Rejected(why)) => _ = self.toaster.info(why),
			Err(e) => _ = self.toaster.error(e.to_string()),
		}
	}

	/// `card` to `to` of `workflow`, from where it shows sitting: a hook's refusal is toasted, a clash lands on the issue's tab.
	async fn move_card(self, card: Card, workflow: String, to: State) {
		let seen = card.states[&workflow].clone();
		match api::move_card(card.link.clone(), workflow, to, seen).await {
			Ok(Edited::Landed) => {
				let mut conflicts = self.conflicts;
				conflicts.write().remove(&card.link);
			}
			Ok(Edited::Conflict(conflict)) => {
				let mut conflicts = self.conflicts;
				conflicts.write().insert(card.link.clone(), conflict);
				self.open_issue(card.link);
			}
			Ok(Edited::Rejected(why)) => _ = self.toaster.info(why),
			Err(e) => _ = self.toaster.error(e.to_string()),
		}
	}

	/// Whether it landed; why not is toasted.
	async fn keep(self, edit: LibraryEdit) -> bool {
		match api::edit_library(edit).await {
			Ok(library) => {
				let mut l = self.library;
				l.set(Some(library));
				true
			}
			Err(e) => {
				self.toaster.error(e.to_string());
				false
			}
		}
	}

	/// The band's tiles as the library has them, else the first view.
	fn arrange(self, api: PackedApi) {
		let (true, Some(library)) = (self.setup.peek().is_some(), self.library.peek().clone()) else {
			return; // `Live` arranges once both arrived
		};
		let band = api.band();
		let shown = match library.on_screen.tiles.get(&band) {
			Some(layout) => self.show(api, layout),
			None => {
				if let Some(view) = self.views().into_iter().next() {
					self.open(PanelId(format!("view:{}", view.name)));
				}
				true
			}
		};
		if shown {
			let mut arranged = self.arranged;
			arranged.set(Some(band));
		}
	}

	/// `layout` on the dock, its panels hosted first, cut to the rows this window shows: a row is a fixed height,
	/// so a layout kept on a taller window would scroll here.
	fn show(self, mut api: PackedApi, layout: &str) -> bool {
		match serial::load(layout) {
			Ok(mut grid) => {
				for id in grid.cells.iter().flat_map(|c| c.group.tabs.iter()) {
					self.host(id);
				}
				let rows = api.rows();
				for cell in grid.cells.iter_mut().filter(|c| c.y < rows && c.y + c.h > rows) {
					cell.h = rows - cell.y;
				}
				api.load(&serial::save(&grid)).expect("parsed above, heights cut");
				true
			}
			Err(e) => {
				self.toaster
					.error(format!("the kept layout doesn't load ({e:?}); nothing kept is overwritten until another is shown"));
				false
			}
		}
	}

	/// Workspace `name` onto the dock, if it has this band's.
	fn switch(self, name: &str) {
		let api = (*self.api.peek()).expect("a band is entered before a workspace can be switched to");
		let band = api.band();
		let library = self.library.peek();
		let workspace = library.as_ref().and_then(|l| l.workspaces.get(name)).expect("offered from the library");
		match workspace.get(&band) {
			Some(layout) => {
				let layout = layout.clone();
				drop(library);
				if self.show(api, &layout) {
					let mut arranged = self.arranged;
					arranged.set(Some(band));
				}
			}
			None => _ = self.toaster.info(format!("{name} keeps no {band} arrangement: arrange this screen, then save it as {name}")),
		}
	}
}

#[derive(Clone, Debug, PartialEq)]
enum Modal {
	Scope(Editing),
	/// The stored issues the board couldn't read, and why.
	Unreadable,
}

#[derive(Clone, Debug, PartialEq)]
enum Editing {
	New,
	/// One of the viewer's, by name.
	Own(String),
	/// The team's, by path: saved, it becomes the viewer's own.
	Team(String),
}

/// What a tile wants when it opens.
fn size(id: &PanelId) -> (u32, u32) {
	match id.0.split_once(':').map(|(kind, _)| kind) {
		Some("sprint") => (24, 18),
		Some("issue") => (40, 14),
		_ => (40, 18),
	}
}

/// Sends what `api`'s dock shows as `dock`'s, once it settles, from the moment it was arranged for its band.
fn use_autosave(ctx: Ctx, api: Signal<Option<PackedApi>>, arranged: Signal<Option<Band>>, dock: Dock) {
	let mut pending = use_signal(|| None::<dioxus::core::Task>);
	let mut sent = use_signal(|| None::<(Band, String)>);
	use_effect(move || {
		let Some(api) = *api.read() else { return };
		let (band, layout) = (api.band(), api.save());
		if *arranged.read() != Some(band) || sent.peek().as_ref() == Some(&(band, layout.clone())) {
			return;
		}
		if let Some(task) = pending.take() {
			task.cancel();
		}
		let dock = dock.clone();
		pending.set(Some(spawn(async move {
			gloo_timers::future::sleep(std::time::Duration::from_millis(800)).await;
			sent.set(Some((band, layout.clone())));
			ctx.keep(LibraryEdit::Arranged { dock, band, layout }).await;
		})));
	});
}

#[component]
fn Shell() -> Element {
	let toaster = use_toaster();
	let ctx = use_context_provider(|| Ctx {
		setup: Signal::new(None),
		library: Signal::new(None),
		board: Signal::new(None),
		conflicts: Signal::new(HashMap::new()),
		panels: Signal::new(Vec::new()),
		api: Signal::new(None),
		arranged: Signal::new(None),
		failure: Signal::new(None),
		modal: Signal::new(None),
		toaster,
	});
	use_context_provider(|| Callback::new(|_: GroupId| {})); // tilers' `+`: new tabs come from the sidebar and palette
	use_autosave(ctx, ctx.api, ctx.arranged, Dock::Tiles);
	rsx! {
		Live {}
		SidebarProvider {
			Nav {}
			SidebarInset { class: "flex h-svh flex-col overflow-hidden",
				TopBar {}
				div { class: "relative min-h-0 flex-1",
					if let Some(why) = ctx.failure.read().clone() {
						div { class: "absolute inset-0 z-10 flex items-start justify-center bg-surface p-8",
							Alert { variant: AlertVariant::Destructive, class: "max-w-2xl",
								AlertTitle { "The board didn't load" }
								AlertDescription { pre { class: "whitespace-pre-wrap text-xs", "{why}" } }
							}
						}
					}
					div { class: "absolute inset-0",
						PackedArea {
							panels: ctx.panels,
							on_band: Some(Callback::new(move |api: PackedApi| {
								let (mut handle, mut arranged) = (ctx.api, ctx.arranged);
								handle.set(Some(api));
								arranged.set(None);
								ctx.arrange(api);
							})),
							config: None,
						}
					}
				}
			}
		}
		Modals {}
		Palette {}
	}
}

/// The scope everything on screen is narrowed to, what it narrows by, and its editor.
#[component]
fn TopBar() -> Element {
	let ctx = use_context::<Ctx>();
	if ctx.library.read().is_none() {
		return rsx! { div { class: "h-10 shrink-0 border-b" } };
	}
	let scopes = ctx.scopes();
	let own = ctx.library.read().as_ref().expect("checked above").scopes.clone();
	let mut paths = Vec::new();
	fn walk(prefix: &str, level: &BTreeMap<String, Scope>, depth: usize, out: &mut Vec<(String, String, usize)>) {
		for (name, scope) in level {
			let path = if prefix.is_empty() { name.clone() } else { format!("{prefix} / {name}") };
			out.push((path.clone(), name.clone(), depth));
			walk(&path, &scope.children, depth + 1, out);
		}
	}
	walk("", &scopes, 0, &mut paths);
	let current = ctx.scope();
	let pick = move |path: Option<String>| {
		spawn(async move {
			_ = ctx.keep(LibraryEdit::Scope(path)).await;
		});
	};
	rsx! {
		div { class: "flex h-10 shrink-0 items-center gap-2 border-b px-3 text-sm",
			span { class: "text-ink-soft", "scope" }
			Select {
				value: current.as_ref().map_or_else(String::new, |(p, _)| p.clone()),
				on_value_change: move |p: String| pick((!p.is_empty()).then_some(p)),
				SelectTrigger { size: Size::Sm, class: "min-w-40", SelectValue { placeholder: "the whole store" } }
				SelectContent {
					SelectItem { value: "", text_value: "the whole store", "the whole store" }
					for (path , name , depth) in paths {
						SelectItem { key: "{path}", value: path.clone(), text_value: path.clone(),
							span { style: "padding-left: {depth * 12}px", "{name}" }
						}
					}
				}
			}
			match current {
				None => rsx! { span { class: "text-ink-soft", "every stored issue and pull request" } },
				Some((path, None)) => rsx! {
					Badge { variant: BadgeVariant::Error, "{path} is no longer configured" }
				},
				Some((path, Some(filter))) => {
					let top = path.split(" / ").next().expect("a path names at least its top scope").to_string();
					let editing = if own.contains_key(&top) && !path.contains(" / ") { Editing::Own(path.clone()) } else { Editing::Team(path.clone()) };
					let also = editing.clone();
					rsx! {
						button {
							r#type: "button",
							class: "min-w-0 truncate rounded px-1.5 py-0.5 font-mono text-xs text-ink-soft hover:bg-surface-sunken hover:text-ink",
							title: "What every view here is narrowed by — click to edit",
							onclick: move |_| { let mut m = ctx.modal; m.set(Some(Modal::Scope(also.clone()))); },
							"{filter}"
						}
						Button { variant: ButtonVariant::Ghost, size: Size::Xs, r#type: "button", onclick: move |_| { let mut m = ctx.modal; m.set(Some(Modal::Scope(editing.clone()))); }, "Edit" }
					}
				}
			}
			div { class: "flex-1" }
			Button { variant: ButtonVariant::Ghost, size: Size::Xs, r#type: "button", title: "A scope of your own", onclick: move |_| { let mut m = ctx.modal; m.set(Some(Modal::Scope(Editing::New))); }, "+ scope" }
		}
	}
}

/// Setup and the library, then the board, reloaded on every change the server announces.
#[component]
fn Live() -> Element {
	let ctx = use_context::<Ctx>();
	let toaster = ctx.toaster;
	use_future(move || async move {
		let mut failure = ctx.failure;
		match futures::join!(api::setup(), api::library()) {
			(Ok(setup), Ok(library)) => {
				let (mut s, mut l) = (ctx.setup, ctx.library);
				s.set(Some(setup));
				l.set(Some(library));
				if let Some(api) = *ctx.api.peek() {
					ctx.arrange(api);
				}
			}
			(Err(e), _) => return failure.set(Some(format!("setup: {e}"))),
			(_, Err(e)) => return failure.set(Some(format!("your library: {e}"))),
		}
		loop {
			match api::changes().await {
				Ok(mut ticks) => {
					reload(ctx).await;
					while let Some(tick) = ticks.next().await {
						match tick {
							Ok(()) => reload(ctx).await,
							Err(e) => _ = toaster.error(format!("live updates: {e}")),
						}
					}
				}
				Err(e) => _ = toaster.error(format!("live updates: {e}")),
			}
			gloo_timers::future::sleep(std::time::Duration::from_secs(3)).await; // the server restarted or the network dropped: reconnect
		}
	});
	rsx! {}
}

async fn reload(ctx: Ctx) {
	match api::board().await {
		Ok(board) => {
			let mut b = ctx.board;
			b.set(Some(board));
		}
		Err(e) => _ = ctx.toaster.error(format!("board: {e}")),
	}
}

/// The sidebar; on a phone, where it doesn't fit, the same menu in a sheet behind a button.
#[component]
fn Nav() -> Element {
	let mut open = use_signal(|| false);
	rsx! {
		Sidebar { class: "w-(--sidebar-width) shrink-0", NavMenu {} }
		button { class: "fixed bottom-3 left-3 z-40 rounded-md border bg-card px-3 py-2 md:hidden", onclick: move |_| open.set(true), "☰" }
		Sheet { open: open(), on_open_change: move |v| open.set(v),
			SheetContent { side: ev_lib::uikit_dioxus::primitives::Side::Left, aria_label: "Views",
				// a choice made in the menu also closes it
				div { class: "flex h-full flex-col", onclick: move |_| open.set(false), NavMenu {} }
			}
		}
	}
}

#[component]
fn NavMenu() -> Element {
	let ctx = use_context::<Ctx>();
	let setup = ctx.setup.read();
	let library = ctx.library.read();
	let (Some(setup), Some(library)) = (setup.as_ref(), library.as_ref()) else {
		return rsx! {
			SidebarHeader { class: "font-semibold px-3", "tedi" }
			SidebarContent { if ctx.failure.read().is_none() { Spinner {} } }
		};
	};
	let views = ctx.views();
	let scopes = library.scopes(&setup.scopes);
	let current_scope = library.scope.clone();
	let starred = |f: &Favorite| library.favorites.contains(f);

	let mut workspaces: Vec<Entry> = library
		.workspaces
		.keys()
		.map(|name| Entry {
			label: name.clone(),
			depth: 0,
			open: Open::Workspace(name.clone()),
			favorite: Some(Favorite::Workspace(name.clone())),
			own: Some(Shelf::Workspaces),
			current: false,
		})
		.collect();
	workspaces.sort_by_key(|e| !e.favorite.as_ref().is_some_and(starred)); // stable: favorites first, else in order

	let mut listed: Vec<Entry> = ctx
		.views()
		.into_iter()
		.map(|v| Entry {
			open: Open::Panel(PanelId(format!("view:{}", v.name))),
			favorite: Some(Favorite::View(v.name.clone())),
			own: library.filters.contains_key(&v.name).then_some(Shelf::Filters),
			current: false,
			depth: 0,
			label: v.name,
		})
		.collect();
	listed.sort_by_key(|e| !e.favorite.as_ref().is_some_and(starred));

	let mut scoped = Vec::new();
	fn walk(prefix: &str, level: &BTreeMap<String, Scope>, depth: usize, own: &BTreeMap<String, Scope>, current: &Option<String>, out: &mut Vec<Entry>) {
		for (name, scope) in level {
			let path = if prefix.is_empty() { name.clone() } else { format!("{prefix} / {name}") };
			out.push(Entry {
				label: name.clone(),
				depth,
				open: Open::Scope(path.clone()),
				favorite: Some(Favorite::Scope(path.clone())),
				own: (depth == 0 && own.contains_key(name)).then_some(Shelf::Scopes),
				current: current.as_ref() == Some(&path),
			});
			walk(&path, &scope.children, depth + 1, own, current, out);
		}
	}
	walk("", &scopes, 0, &library.scopes, &current_scope, &mut scoped);

	let sprints: Vec<Entry> = setup
		.sprints
		.iter()
		.map(|s| Entry {
			label: s.title.clone(),
			depth: 0,
			open: Open::Panel(PanelId(format!("sprint:{}", s.title))),
			favorite: None,
			own: None,
			current: false,
		})
		.collect();

	// a favorite whose view or scope is gone stays listed until unfavorited
	let stale: Vec<Favorite> = library
		.favorites
		.iter()
		.filter(|f| match f {
			Favorite::View(name) => !views.iter().any(|v| &v.name == name),
			Favorite::Scope(path) => scope_at(&scopes, path).is_none(),
			Favorite::Workspace(_) | Favorite::Columns(_) => false, // dropped with their entry
		})
		.cloned()
		.collect();
	let unreadable = ctx.board.read().as_ref().map(|b| b.unreadable.clone()).unwrap_or_default(); // the board arrives after the sidebar
	rsx! {
		SidebarHeader { class: "font-semibold px-3", "tedi" }
		SidebarContent { class: "overflow-x-hidden",
			Section {
				label: "Workspaces",
				entries: workspaces,
				empty: "Arrange the screen, then + to keep it",
				add: Add::Named {
					placeholder: "save this screen as…".into(),
					on_name: EventHandler::new(move |name: String| {
						let api = (*ctx.api.peek()).expect("a band is entered before the screen can be saved");
						let edit = LibraryEdit::Keep(Kept::Workspace { name, band: api.band(), layout: api.save() });
						spawn(async move { _ = ctx.keep(edit).await });
					}),
				},
			}
			Section {
				label: "Views",
				entries: listed,
				empty: "No views",
				add: Add::Named {
					placeholder: "new view's name…".into(),
					on_name: EventHandler::new(move |name: String| {
						let view = View {
							name: name.clone(),
							query: "is:open".into(),
							layout: Layout::List,
							group_by: GroupBy::Nothing,
							sort: vec![Sort { by: SortKey::Rank, reversed: false }].into(),
							shown: vec![Shown::Repo, Shown::Assignees, Shown::Importance, Shown::Hours],
						};
						spawn(async move {
							if ctx.keep(LibraryEdit::Keep(Kept::Filter(view))).await {
								ctx.open(PanelId(format!("view:{name}")));
							}
						});
					}),
				},
			}
			Section {
				label: "Scopes",
				entries: scoped,
				empty: "No scopes",
				add: Add::Dialog(EventHandler::new(move |()| { let mut m = ctx.modal; m.set(Some(Modal::Scope(Editing::New))); })),
			}
			if !sprints.is_empty() {
				Section { label: "Sprints", entries: sprints, empty: "", add: Add::None }
			}
			if !stale.is_empty() {
				SidebarGroup {
					SidebarGroupLabel { "Favorites gone" }
					SidebarGroupContent {
						SidebarMenu {
							for what in stale {
								SidebarMenuItem { key: "{what:?}",
									SidebarMenuButton {
										class: "line-through",
										onclick: { let what = what.clone(); move |_| { let what = what.clone(); spawn(async move { _ = ctx.keep(LibraryEdit::Favorite { what, on: false }).await; }); } },
										"{gone(&what)}"
									}
								}
							}
						}
					}
				}
			}
		}
		if !unreadable.is_empty() {
			SidebarFooter { Unreadable { files: unreadable } }
		}
	}
}

fn gone(what: &Favorite) -> String {
	match what {
		Favorite::View(n) | Favorite::Scope(n) | Favorite::Workspace(n) | Favorite::Columns(n) => format!("{n} (click to unfavorite)"),
	}
}

/// A sidebar entry.
#[derive(Clone, PartialEq)]
struct Entry {
	label: String,
	depth: usize,
	open: Open,
	favorite: Option<Favorite>,
	/// Kept by the viewer, on this shelf: renamed and deleted in place.
	own: Option<Shelf>,
	current: bool,
}

#[derive(Clone, PartialEq)]
enum Open {
	/// Opened on the dock, or dragged onto it.
	Panel(PanelId),
	Workspace(String),
	Scope(String),
}

#[derive(Clone, PartialEq)]
enum Add {
	None,
	/// A name typed in place.
	Named {
		placeholder: String,
		on_name: EventHandler<String>,
	},
	Dialog(EventHandler<()>),
}

/// A sidebar group: its entries, and `+` to add one.
#[component]
fn Section(label: String, entries: Vec<Entry>, empty: String, add: Add) -> Element {
	let mut adding = use_signal(|| false);
	rsx! {
		SidebarGroup {
			div { class: "flex items-center justify-between pr-1",
				SidebarGroupLabel { "{label}" }
				match &add {
					Add::None => rsx! {},
					Add::Named { .. } => rsx! { AddButton { label: label.clone(), onclick: move |_| adding.set(true) } },
					Add::Dialog(open) => {
						let open = *open;
						rsx! { AddButton { label: label.clone(), onclick: move |_| open.call(()) } }
					}
				}
			}
			SidebarGroupContent {
				SidebarMenu {
					if let (true, Add::Named { placeholder, on_name }) = (adding(), &add) {
						div { class: "px-2 py-1",
							library::NameForm {
								placeholder: placeholder.clone(),
								on_name: { let on_name = *on_name; move |name: String| { adding.set(false); on_name.call(name); } },
								on_cancel: move |()| adding.set(false),
							}
						}
					}
					if entries.is_empty() && !adding() && !empty.is_empty() {
						span { class: "px-3 py-1 text-xs text-ink-soft", "{empty}" }
					}
					for entry in entries {
						Row { key: "{entry.label}-{entry.depth}-{open_key(&entry.open)}", entry }
					}
				}
			}
		}
	}
}

fn open_key(open: &Open) -> String {
	match open {
		Open::Panel(id) => id.0.clone(),
		Open::Workspace(n) => format!("workspace:{n}"),
		Open::Scope(p) => format!("scope:{p}"),
	}
}

#[component]
fn AddButton(label: String, onclick: EventHandler<MouseEvent>) -> Element {
	rsx! {
		button {
			r#type: "button",
			class: "flex size-5 items-center justify-center rounded text-ink-soft hover:bg-surface-sunken hover:text-ink",
			title: "Add to {label}",
			aria_label: "Add to {label}",
			onclick: move |e| onclick.call(e),
			"+"
		}
	}
}

/// A click opens it, a drag of a panel drops it on the dock; ★ favorites it; hovered, the viewer's own
/// shows ✎ and ✕, a scope ✎ for its editor.
#[component]
fn Row(entry: Entry) -> Element {
	let ctx = use_context::<Ctx>();
	let mut renaming = use_signal(|| false);
	let mut deleting = use_signal(|| false);
	let starred = entry.favorite.as_ref().is_some_and(|f| ctx.favorite(f));
	let Entry {
		label,
		depth,
		open,
		favorite,
		own,
		current,
	} = entry;
	let go = {
		let open = open.clone();
		move |_| match open.clone() {
			Open::Panel(id) => ctx.open(id),
			Open::Workspace(name) => ctx.switch(&name),
			Open::Scope(path) => {
				spawn(async move {
					_ = ctx.keep(LibraryEdit::Scope(Some(path))).await;
				});
			}
		}
	};
	let rename = {
		let from = label.clone();
		move |to: String| {
			renaming.set(false);
			let Some(shelf) = own else { return };
			if to == from {
				return;
			}
			let from = from.clone();
			spawn(async move {
				if ctx
					.keep(LibraryEdit::Rename {
						shelf,
						from: from.clone(),
						to: to.clone(),
					})
					.await && shelf == Shelf::Filters
				{
					ctx.retitle(&PanelId(format!("view:{from}")), PanelId(format!("view:{to}")));
				}
			});
		}
	};
	let edit = match (&open, own) {
		(Open::Scope(path), Some(_)) => Some(Editing::Own(path.clone())),
		(Open::Scope(path), None) => Some(Editing::Team(path.clone())),
		_ => None,
	};
	rsx! {
		SidebarMenuItem {
			div {
				class: "group/row flex items-center gap-0.5 pr-1",
				style: "padding-left: {depth * 12}px",
				draggable: if matches!(open, Open::Panel(_)) { "true" } else { "false" },
				ondragstart: {
					let open = open.clone();
					move |e: DragEvent| {
						let Open::Panel(id) = &open else { return };
						e.data_transfer().set_data("text/plain", &id.0).expect("a dragstart's transfer is writable"); // Firefox starts no drag without data
						ctx.carry(id.clone());
					}
				},
				match favorite.clone() {
					Some(what) => rsx! {
						button {
							r#type: "button",
							class: if starred { "w-5 shrink-0 text-accent" } else { "w-5 shrink-0 text-ink-soft opacity-0 group-hover/row:opacity-60 hover:!opacity-100 [@media(hover:none)]:opacity-40" },
							title: if starred { "Unfavorite" } else { "Favorite" },
							onclick: move |e: MouseEvent| {
								e.stop_propagation();
								let what = what.clone();
								spawn(async move { _ = ctx.keep(LibraryEdit::Favorite { what, on: !starred }).await; });
							},
							if starred { "★" } else { "☆" }
						}
					},
					None => rsx! { span { class: "w-5 shrink-0" } },
				}
				if renaming() {
					div { class: "min-w-0 flex-1 py-0.5",
						library::NameForm { initial: label.clone(), placeholder: "name", on_name: rename, on_cancel: move |()| renaming.set(false) }
					}
				} else {
					SidebarMenuButton { class: if current { "min-w-0 flex-1 truncate font-semibold text-ink" } else { "min-w-0 flex-1 truncate" }, is_active: current, onclick: go, "{label}" }
					if let Some(editing) = edit {
						RowAction { title: "Edit scope", onclick: move |_| { let mut m = ctx.modal; m.set(Some(Modal::Scope(editing.clone()))); }, "✎" }
					} else if own.is_some() {
						RowAction { title: "Rename", onclick: move |_| renaming.set(true), "✎" }
					}
					if let Some(shelf) = own {
						if deleting() {
							button {
								r#type: "button",
								class: "shrink-0 rounded px-1 text-xs text-accent-error hover:bg-surface-sunken",
								onclick: { let name = label.clone(); move |_| { let name = name.clone(); deleting.set(false); spawn(async move { _ = ctx.keep(LibraryEdit::Drop { shelf, name }).await; }); } },
								onmouseleave: move |_| deleting.set(false),
								"delete?"
							}
						} else {
							RowAction { title: "Delete", onclick: move |_| deleting.set(true), "✕" }
						}
					}
				}
			}
		}
	}
}

#[component]
fn RowAction(title: String, onclick: EventHandler<MouseEvent>, children: Element) -> Element {
	rsx! {
		button {
			r#type: "button",
			class: "shrink-0 rounded px-1 text-xs text-ink-soft opacity-0 group-hover/row:opacity-100 hover:bg-surface-sunken hover:text-ink [@media(hover:none)]:opacity-60",
			title: "{title}",
			aria_label: "{title}",
			onclick: move |e: MouseEvent| {
				e.stop_propagation();
				onclick.call(e);
			},
			{children}
		}
	}
}

#[component]
fn Unreadable(files: Vec<(std::path::PathBuf, String)>) -> Element {
	let ctx = use_context::<Ctx>();
	let n = files.len();
	rsx! {
		button { r#type: "button", onclick: move |_| { let mut m = ctx.modal; m.set(Some(Modal::Unreadable)); },
			Badge { variant: BadgeVariant::Warn, if n == 1 { "1 stored issue doesn't parse" } else { "{n} stored issues don't parse" } }
		}
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
				Some(Modal::Scope(editing)) => rsx! { ScopeForm { key: "{editing:?}", editing } },
				Some(Modal::Unreadable) => {
					let board = ctx.board.read();
					let files = board.as_ref().map(|b| b.unreadable.clone()).expect("offered from the board's");
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

/// A scope's name and the repos it covers; a team scope saves as the viewer's own.
#[component]
fn ScopeForm(editing: Editing) -> Element {
	let ctx = use_context::<Ctx>();
	let (initial_name, initial) = {
		let library = ctx.library.read();
		let library = library.as_ref().expect("the editor opens from a loaded screen");
		match &editing {
			Editing::New => (
				String::new(),
				Scope {
					repos: Vec::new(),
					children: BTreeMap::new(),
				},
			),
			Editing::Own(name) => (name.clone(), library.scopes[name].clone()),
			Editing::Team(path) => {
				let setup = ctx.setup.read();
				let team = &setup.as_ref().expect("arrives with the library").scopes;
				// the copy covers, flat, what the team scope and every one nested in it cover
				let filter = scope_at(team, path).expect("offered from the team's");
				let repos = filter.strip_prefix("repo:").expect("a scope's filter is a repo clause").split(',').map(str::to_string).collect();
				let name = path.rsplit(" / ").next().expect("a path names at least one scope");
				(format!("my {name}"), Scope { repos, children: BTreeMap::new() })
			}
		}
	};
	let mut name = use_signal(|| initial_name.clone());
	let mut repos = use_signal(|| initial.repos.clone());
	let mut typed = use_signal(String::new);
	let close = move || {
		let mut m = ctx.modal;
		m.set(None);
	};
	let board = ctx.board.read();
	let cards = board.as_ref().map(|b| b.cards.as_slice()).unwrap_or_default(); // before the board arrives nothing is offered
	let mut known: Vec<String> = cards.iter().map(|c| c.link.project().to_string()).collect();
	known.extend(cards.iter().map(|c| format!("{}/*", c.link.project().owner().expect("a carded repo is a real one, owned"))));
	known.sort();
	known.dedup();
	let needle = typed.read().trim().to_lowercase();
	let offered: Vec<String> = known.into_iter().filter(|r| !repos.read().contains(r) && r.to_lowercase().contains(&needle)).take(12).collect();
	let filter = format!("repo:{}", repos.read().join(","));
	let covers = match repos.read().is_empty() {
		true => Err("names no repo yet".to_string()),
		false => Query::from_str(&format!("{filter} is:open"))
			.map_err(|e| e.to_string())
			.and_then(|q| q.run(cards, ctx.me().as_deref()).map_err(|e| e.to_string()))
			.map(|hits| hits.len()),
	};
	let team = matches!(editing, Editing::Team(_));
	let save = {
		let editing = editing.clone();
		move |_| {
			let (to, scope) = (
				name.read().trim().to_string(),
				Scope {
					repos: repos.read().clone(),
					children: BTreeMap::new(),
				},
			);
			let editing = editing.clone();
			spawn(async move {
				if let Editing::Own(from) = &editing
					&& *from != to && !ctx
					.keep(LibraryEdit::Rename {
						shelf: Shelf::Scopes,
						from: from.clone(),
						to: to.clone(),
					})
					.await
				{
					return;
				}
				if !ctx.keep(LibraryEdit::Keep(Kept::Scope { name: to.clone(), scope })).await {
					return;
				}
				if !matches!(editing, Editing::Own(_)) {
					ctx.keep(LibraryEdit::Scope(Some(to))).await;
				}
				close();
			});
		}
	};
	rsx! {
		Dialog { open: true, on_open_change: move |v: bool| if !v { close() },
			DialogContent { class: "flex flex-col gap-4 sm:max-w-xl",
				DialogHeader {
					DialogTitle {
						match &editing {
							Editing::New => "New scope".to_string(),
							Editing::Own(n) => format!("Scope {n}"),
							Editing::Team(p) => format!("Scope {p}"),
						}
					}
					if team {
						DialogDescription { "The team's, set in the board config. Saving keeps a copy of your own, which you can change." }
					}
				}
				label { class: "flex flex-col gap-1 text-sm",
					span { class: "text-ink-soft", "Name" }
					Input { value: name(), placeholder: "e.g. backend", oninput: move |e: FormEvent| name.set(e.value()) }
				}
				div { class: "flex flex-col gap-2 text-sm",
					span { class: "text-ink-soft", "Repos" }
					div { class: "flex flex-wrap gap-1",
						if repos.read().is_empty() {
							span { class: "text-xs text-ink-soft", "none yet: pick below or type `owner/repo`, `owner/*`" }
						}
						for r in repos.read().clone() {
							span { key: "{r}", class: "inline-flex items-center gap-1 rounded border px-1.5 py-0.5 font-mono text-xs",
								"{r}"
								button { r#type: "button", class: "text-ink-soft hover:text-ink", title: "Drop {r}", onclick: { let r = r.clone(); move |_| repos.write().retain(|x| *x != r) }, "×" }
							}
						}
					}
					form {
						onsubmit: move |e: FormEvent| {
							e.prevent_default();
							let r = typed.read().trim().to_string();
							if !r.is_empty() && !repos.read().contains(&r) {
								repos.write().push(r);
							}
							typed.set(String::new());
						},
						Input { size: Size::Sm, class: "font-mono", value: typed(), placeholder: "filter, or type a pattern and press Enter", oninput: move |e: FormEvent| typed.set(e.value()) }
					}
					div { class: "flex max-h-40 flex-wrap gap-1 overflow-y-auto",
						for r in offered {
							button {
								key: "{r}",
								r#type: "button",
								class: "rounded bg-surface-sunken px-1.5 py-0.5 font-mono text-xs text-ink-soft hover:text-ink",
								onclick: { let r = r.clone(); move |_| { repos.write().push(r.clone()); typed.set(String::new()); } },
								"+ {r}"
							}
						}
					}
					div { class: "flex items-center gap-2 text-xs",
						code { class: "truncate text-ink-soft", "{filter}" }
						match &covers {
							Ok(n) => rsx! { span { class: "shrink-0", "· {n} open" } },
							Err(e) => rsx! { span { class: "shrink-0 text-accent-error", "· {e}" } },
						}
					}
				}
				DialogFooter { class: "flex items-center gap-2",
					if let Editing::Own(n) = &editing {
						Button {
							variant: ButtonVariant::Ghost,
							size: Size::Sm,
							r#type: "button",
							class: "text-accent-error",
							onclick: { let n = n.clone(); move |_| { let name = n.clone(); spawn(async move { if ctx.keep(LibraryEdit::Drop { shelf: Shelf::Scopes, name }).await { close(); } }); } },
							"Delete"
						}
					}
					div { class: "flex-1" }
					Button { variant: ButtonVariant::Ghost, size: Size::Sm, r#type: "button", onclick: move |_| close(), "Cancel" }
					Button {
						size: Size::Sm,
						r#type: "button",
						disabled: name.read().trim().is_empty() || repos.read().is_empty() || covers.is_err(),
						onclick: save,
						if team { "Save as mine" } else { "Save" }
					}
				}
			}
		}
	}
}

/// ⌘K: every view, scope, sprint and open issue by name.
#[component]
fn Palette() -> Element {
	let ctx = use_context::<Ctx>();
	let mut open = use_signal(|| false);
	use_command_palette_shortcut(move || open.toggle(), true);
	let item = |id: PanelId, label: String| CommandPaletteItem {
		id: id.0.clone(),
		label,
		icon: None,
		shortcut: None,
		keywords: Vec::new(),
		disabled: false,
		on_select: EventHandler::new(move |_| ctx.open(id.clone())),
	};
	let mut groups = Vec::new();
	if let Some(setup) = ctx.setup.read().as_ref() {
		let mut views: Vec<CommandPaletteItem> = ctx.views().into_iter().map(|v| item(PanelId(format!("view:{}", v.name)), v.name)).collect();
		views.extend(setup.sprints.iter().map(|s| item(PanelId(format!("sprint:{}", s.title)), format!("Sprint: {}", s.title))));
		groups.push(CommandPaletteGroup {
			id: "views".into(),
			heading: Some("Views".into()),
			items: views,
		});
	}
	if let Some(board) = ctx.board.read().as_ref() {
		groups.push(CommandPaletteGroup {
			id: "issues".into(),
			heading: Some("Open issues".into()),
			items: board
				.cards
				.iter()
				.filter(|c| c.open())
				.map(|c| item(PanelId(format!("issue:{}", c.link)), format!("{} #{} {}", c.link.project(), c.link.number(), c.title)))
				.collect(),
		});
	}
	rsx! {
		CommandPalette { open: open(), on_open_change: move |v| open.set(v), groups }
	}
}
