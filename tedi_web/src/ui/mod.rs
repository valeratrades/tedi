//! The client: a sidebar of workspaces, views, scopes and sprints over a tiled workspace, every tile a view or
//! an issue. Cards arrive whole and are queried here; edits leave as server fns. What is on screen, and what
//! the viewer keeps, is their library on the server.

mod facets;
mod issue;
mod library;
mod view;

use std::collections::{BTreeMap, HashMap};

use dioxus::prelude::*;
use ev_lib::uikit_dioxus::*;
use futures::StreamExt as _;
use tedi_core::{
	IssueLink,
	board::{Board, Card, Scope, View, scope_at},
	conflict::{Conflict, FieldEdit},
	workflow::{State, Workflow},
};
use tilers_dioxus::{Band, DockPanel, Group, GroupId, MinSize, PackedApi, PackedArea, PanelId};

use crate::api::{self, Dock, Edited, Favorite, Library, LibraryEdit, Setup, Shelf};

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

	/// The views the scope on screen names (every team view, without one), then the viewer's own.
	fn listed(&self) -> Vec<View> {
		let names = match self.scope() {
			Some((_, Some((_, names)))) => Some(names),
			Some((_, None)) => Some(Vec::new()),
			None => None,
		};
		let own = self.library.read().as_ref().map(|l| l.filters.keys().cloned().collect::<Vec<_>>()).unwrap_or_default();
		self.views()
			.into_iter()
			.filter(|v| own.contains(&v.name) || names.as_ref().is_none_or(|n| n.contains(&v.name)))
			.collect()
	}

	/// The scope everything on screen is narrowed to: its path, and its filter and views while it is still configured.
	fn scope(&self) -> Option<(String, Option<(String, Vec<String>)>)> {
		let path = self.library.read().as_ref()?.scope.clone()?;
		let at = scope_at(&self.setup.read().as_ref()?.scopes, &path);
		Some((path, at))
	}

	fn favorite(&self, what: &Favorite) -> bool {
		self.library.read().as_ref().is_some_and(|l| l.favorites.contains(what))
	}

	fn open_issue(&self, link: IssueLink) {
		self.open(PanelId(format!("issue:{link}")));
	}

	/// Shows `id`, hosting it first if it isn't: an issue as a tab of the tile holding another, anything else in a tile of its own.
	fn open(&self, id: PanelId) {
		let mut api = (*self.api.peek()).expect("a band is entered before anything can be opened");
		if api.tab_ids().contains(&id) {
			return;
		}
		self.host(&id);
		let host = id.0.starts_with("issue:").then(|| {
			let grid = tilers_dioxus::model::serial::load(&api.save()).expect("a layout loads back from its own save");
			grid.cells.iter().find(|c| c.group.tabs.iter().any(|t| t.0.starts_with("issue:"))).map(|c| c.group.id)
		});
		match host.flatten() {
			Some(group) => api.add_tab(group, id),
			None => {
				let (w, h) = size(&id);
				let group = api.mint_group_id();
				api.place(Group::new(group, id), w, h);
			}
		}
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
		let library = self.library.peek();
		let on_screen = &library.as_ref().expect("arrives with setup").on_screen.views;
		let shown = |defined: View| rsx! { view::ViewPanel { view: on_screen.get(&defined.name).cloned().unwrap_or_else(|| defined.clone()), defined } };
		let (title, content) = match id.0.split_once(':') {
			Some(("view", name)) => match self.views().into_iter().find(|v| v.name == name) {
				Some(view) => (name.to_string(), shown(view)),
				None => (name.to_string(), rsx! { "The view {name} is no longer saved" }),
			},
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
				if let Some(view) = self.listed().into_iter().next() {
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

	/// `layout` on the dock, its panels hosted first.
	fn show(self, mut api: PackedApi, layout: &str) -> bool {
		match tilers_dioxus::model::serial::load(layout) {
			Ok(grid) => {
				for id in grid.cells.iter().flat_map(|c| c.group.tabs.iter()) {
					self.host(id);
				}
				api.load(layout).expect("parsed above");
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
pub fn App() -> Element {
	rsx! {
		Fonts {}
		ToasterProvider {
			Toaster { position: ToastPosition::BottomRight }
			Shell {}
		}
	}
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
		toaster,
	});
	use_context_provider(|| Callback::new(|_: GroupId| {})); // tilers' `+`: new tabs come from the sidebar and palette
	use_autosave(ctx, ctx.api, ctx.arranged, Dock::Tiles);
	rsx! {
		Live {}
		SidebarProvider {
			Nav {}
			SidebarInset { class: "flex h-svh flex-col",
				TopBar {}
				div { class: "relative flex-1",
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
		Palette {}
	}
}

/// The scope everything on screen is narrowed to.
#[component]
fn TopBar() -> Element {
	let ctx = use_context::<Ctx>();
	rsx! {
		div { class: "flex h-9 shrink-0 items-center gap-2 border-b px-3 text-sm",
			span { class: "text-ink-soft", "scope" }
			match ctx.scope() {
				None => rsx! { span { class: "font-medium", "the whole store" } },
				Some((path, at)) => rsx! {
					span { class: if at.is_some() { "font-medium" } else { "font-medium line-through" }, title: if at.is_none() { "no longer configured" }, "{path}" }
					button {
						class: "px-1 text-ink-soft hover:text-ink",
						title: "Widen to the whole store",
						onclick: move |_| { spawn(async move { _ = ctx.keep(LibraryEdit::Scope(None)).await; }); },
						"×"
					}
				},
			}
		}
	}
}

/// Setup and the library, then the board, reloaded on every change the server announces.
#[component]
fn Live() -> Element {
	let ctx = use_context::<Ctx>();
	let toaster = ctx.toaster;
	use_future(move || async move {
		match futures::join!(api::setup(), api::library()) {
			(Ok(setup), Ok(library)) => {
				let (mut s, mut l) = (ctx.setup, ctx.library);
				s.set(Some(setup));
				l.set(Some(library));
				if let Some(api) = *ctx.api.peek() {
					ctx.arrange(api);
				}
			}
			(Err(e), _) => _ = toaster.error(format!("setup: {e}")),
			(_, Err(e)) => _ = toaster.error(format!("your library: {e}")),
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
		Sidebar { NavMenu {} }
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
		return rsx! { SidebarContent { Spinner {} } };
	};
	let unreadable = ctx.board.read().as_ref().map_or(0, |b| b.unreadable.len());
	let favorite_workspaces: Vec<String> = library
		.workspaces
		.keys()
		.filter(|n| library.favorites.contains(&Favorite::Workspace((*n).clone())))
		.cloned()
		.collect();
	let views = ctx.views();
	let mut listed = ctx.listed();
	listed.sort_by_key(|v| !library.favorites.contains(&Favorite::View(v.name.clone()))); // stable: favorites first, else in order
	let mut scopes: Vec<(String, Scope)> = setup.scopes.clone().into_iter().collect();
	scopes.sort_by_key(|(name, _)| !library.favorites.contains(&Favorite::Scope(name.clone())));
	// a favorite whose view or scope is gone stays listed until unfavorited
	let stale: Vec<Favorite> = library
		.favorites
		.iter()
		.filter(|f| match f {
			Favorite::View(name) => !views.iter().any(|v| &v.name == name),
			Favorite::Scope(path) => scope_at(&setup.scopes, path).is_none(),
			Favorite::Workspace(_) | Favorite::Columns(_) => false, // dropped with their entry
		})
		.cloned()
		.collect();
	let workspaces: Vec<String> = library.workspaces.keys().cloned().collect();
	let filters: Vec<String> = library.filters.keys().cloned().collect();
	rsx! {
		SidebarHeader { class: "font-semibold px-3", "tedi" }
		SidebarContent {
			SidebarGroup {
				div { class: "flex items-center justify-between",
					SidebarGroupLabel { "Workspaces" }
					library::Menu {
						shelf: Shelf::Workspaces,
						names: workspaces,
						on_pick: move |name: String| ctx.switch(&name),
						on_save: Some(EventHandler::new(move |name: String| {
							let api = (*ctx.api.peek()).expect("a band is entered before the screen can be saved");
							let edit = LibraryEdit::Keep(api::Kept::Workspace { name, band: api.band(), layout: api.save() });
							spawn(async move { _ = ctx.keep(edit).await });
						})),
					}
				}
				SidebarGroupContent {
					SidebarMenu {
						for name in favorite_workspaces {
							SidebarMenuItem { key: "{name}",
								SidebarMenuButton { onclick: { let name = name.clone(); move |_| ctx.switch(&name) }, "★ {name}" }
							}
						}
					}
				}
			}
			SidebarGroup {
				div { class: "flex items-center justify-between",
					SidebarGroupLabel { "Views" }
					library::Menu {
						shelf: Shelf::Filters,
						names: filters,
						on_pick: move |name: String| ctx.open(PanelId(format!("view:{name}"))),
						on_save: None,
					}
				}
				SidebarGroupContent {
					SidebarMenu {
						for view in listed {
							Item { key: "{view.name}", id: PanelId(format!("view:{}", view.name)), label: view.name.clone(), favorite: Some(Favorite::View(view.name.clone())), depth: 0 }
						}
					}
				}
			}
			if !setup.scopes.is_empty() {
				SidebarGroup {
					SidebarGroupLabel { "Scopes" }
					SidebarGroupContent {
						SidebarMenu {
							for (name, scope) in scopes {
								ScopeItem { key: "{name}", path: name.clone(), name, scope, depth: 0 }
							}
						}
					}
				}
			}
			if !setup.sprints.is_empty() {
				SidebarGroup {
					SidebarGroupLabel { "Sprints" }
					SidebarGroupContent {
						SidebarMenu {
							for sprint in setup.sprints.clone() {
								Item { key: "{sprint.title}", id: PanelId(format!("sprint:{}", sprint.title)), label: sprint.title.clone(), favorite: None, depth: 0 }
							}
						}
					}
				}
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
		if unreadable > 0 {
			SidebarFooter {
				Badge { variant: BadgeVariant::Warn, "{unreadable} stored issues don't parse" }
			}
		}
	}
}

fn gone(what: &Favorite) -> String {
	match what {
		Favorite::View(n) | Favorite::Scope(n) | Favorite::Workspace(n) | Favorite::Columns(n) => format!("{n} (click to unfavorite)"),
	}
}

/// A sidebar entry: a click opens `id`, a drag drops it on the dock where it lands; `favorite` gets a ★ toggle.
#[component]
fn Item(id: PanelId, label: String, favorite: Option<Favorite>, depth: usize) -> Element {
	let ctx = use_context::<Ctx>();
	let starred = favorite.as_ref().is_some_and(|f| ctx.favorite(f));
	rsx! {
		SidebarMenuItem {
			div {
				class: "flex items-center gap-1",
				style: "padding-left: {depth * 12}px",
				draggable: "true",
				ondragstart: {
					let id = id.clone();
					move |e: DragEvent| {
						e.data_transfer().set_data("text/plain", &id.0).expect("a dragstart's transfer is writable"); // Firefox starts no drag without data
						ctx.carry(id.clone());
					}
				},
				if let Some(what) = favorite.clone() {
					button {
						class: if starred { "px-1 text-accent" } else { "px-1 text-ink-soft opacity-50 hover:opacity-100" },
						title: if starred { "Unfavorite" } else { "Favorite" },
						onclick: move |e: MouseEvent| {
							e.stop_propagation();
							let what = what.clone();
							spawn(async move { _ = ctx.keep(LibraryEdit::Favorite { what, on: !starred }).await; });
						},
						if starred { "★" } else { "☆" }
					}
				}
				SidebarMenuButton { class: "flex-1", onclick: { let id = id.clone(); move |_| ctx.open(id.clone()) }, "{label}" }
			}
		}
	}
}

/// A scope, picked as the one everything on screen is narrowed to, then the scopes nested in it.
#[component]
fn ScopeItem(path: String, name: String, scope: Scope, depth: usize) -> Element {
	let ctx = use_context::<Ctx>();
	let what = Favorite::Scope(path.clone());
	let starred = ctx.favorite(&what);
	let current = ctx.scope().is_some_and(|(p, _)| p == path);
	rsx! {
		SidebarMenuItem {
			div { class: "flex items-center gap-1", style: "padding-left: {depth * 12}px",
				button {
					class: if starred { "px-1 text-accent" } else { "px-1 text-ink-soft opacity-50 hover:opacity-100" },
					title: if starred { "Unfavorite" } else { "Favorite" },
					onclick: move |_| {
						let what = what.clone();
						spawn(async move { _ = ctx.keep(LibraryEdit::Favorite { what, on: !starred }).await; });
					},
					if starred { "★" } else { "☆" }
				}
				SidebarMenuButton {
					class: if current { "flex-1 font-semibold" } else { "flex-1" },
					onclick: { let path = path.clone(); move |_| { let path = path.clone(); spawn(async move { _ = ctx.keep(LibraryEdit::Scope(Some(path))).await; }); } },
					"{name}"
				}
			}
		}
		for (child, nested) in scope.children.clone() {
			ScopeItem { key: "{child}", path: format!("{path} / {child}"), name: child, scope: nested, depth: depth + 1 }
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
		let mut views: Vec<CommandPaletteItem> = ctx.listed().into_iter().map(|v| item(PanelId(format!("view:{}", v.name)), v.name)).collect();
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
