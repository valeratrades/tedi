//! The client: a sidebar of views, scopes and sprints over a tiled workspace, every tile a view or an
//! issue. Cards arrive whole and are queried here; edits leave as server fns.

mod issue;
mod view;

use std::collections::{BTreeMap, HashMap};

use dioxus::prelude::*;
use ev_lib::uikit_dioxus::*;
use futures::StreamExt as _;
use tedi_core::{
	IssueLink,
	board::{Board, Card, GroupBy, Layout, Scope, Shown, Sort, View},
	conflict::{Conflict, FieldEdit},
};
use tilers_dioxus::{DockPanel, Group, GroupId, MinSize, PackedApi, PackedArea, PanelId};

use crate::api::{self, Edited, Setup, Sprint};

/// Shared by every tile.
#[derive(Clone, Copy)]
struct Ctx {
	setup: Signal<Option<Setup>>,
	board: Signal<Option<Board>>,
	/// Edits that came back clashing, shown on their issue until answered.
	conflicts: Signal<HashMap<IssueLink, Conflict>>,
	panels: Signal<Vec<DockPanel>>,
	api: Signal<Option<PackedApi>>,
	toaster: ToasterHandle,
}
impl Ctx {
	fn card(&self, link: &IssueLink) -> Option<Card> {
		self.board.read().as_ref()?.cards.iter().find(|c| &c.link == link).cloned()
	}

	fn me(&self) -> Option<String> {
		self.setup.read().as_ref()?.me.clone()
	}

	fn open_view(&self, view: View) {
		let title = view.name.clone();
		self.open(PanelId(format!("view:{}", view.name)), title, rsx! { view::ViewPanel { view } }, (40, 18), None);
	}

	/// `path`: the scope's name under each scope it nests in, ` / `-joined.
	fn open_scope(&self, path: String, scope: &Scope) {
		let view = scope_view(&path, scope);
		self.open(PanelId(format!("scope:{path}")), path, rsx! { view::ViewPanel { view } }, (40, 18), None);
	}

	fn open_sprint(&self, sprint: Sprint) {
		let title = sprint.title.clone();
		self.open(PanelId(format!("sprint:{}", sprint.title)), title, rsx! { view::SprintPanel { sprint } }, (24, 18), None);
	}

	/// Issues share one tile, a tab each.
	fn open_issue(&self, link: IssueLink) {
		let title = self.card(&link).map_or_else(|| link.to_string(), |c| format!("#{} {}", link.number(), c.title));
		self.open(PanelId(format!("issue:{link}")), title, rsx! { issue::IssuePanel { link } }, (40, 14), Some("issue:"));
	}

	/// Shows `id`, hosting it first if it isn't: as a tab of the tile holding a `join`-prefixed panel, else in a tile of its own.
	fn open(&self, id: PanelId, title: String, content: Element, (w, h): (u32, u32), join: Option<&str>) {
		let mut api = (*self.api.peek()).expect("a band is entered before anything can be opened");
		if api.tab_ids().contains(&id) {
			return;
		}
		let mut panels = self.panels;
		if !panels.peek().iter().any(|p| p.id == id) {
			panels.write().push(DockPanel {
				id: id.clone(),
				title,
				content,
				min: MinSize::Rem { w: 16.0, h: 8.0 },
			});
		}
		let host = join.and_then(|prefix| {
			let grid = tilers_dioxus::model::serial::load(&api.save()).expect("a layout loads back from its own save");
			grid.cells.iter().find(|c| c.group.tabs.iter().any(|t| t.0.starts_with(prefix))).map(|c| c.group.id)
		});
		match host {
			Some(group) => api.add_tab(group, id),
			None => {
				let group = api.mint_group_id();
				api.place(Group::new(group, id), w, h);
			}
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
			Err(e) => _ = self.toaster.error(e.to_string()),
		}
	}
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
		board: Signal::new(None),
		conflicts: Signal::new(HashMap::new()),
		panels: Signal::new(Vec::new()),
		api: Signal::new(None),
		toaster,
	});
	use_context_provider(|| Callback::new(|_: GroupId| {})); // tilers' `+`: new tabs come from the sidebar and palette
	rsx! {
		Live {}
		SidebarProvider {
			Nav {}
			SidebarInset { class: "relative min-h-svh",
				div { class: "absolute inset-0",
					PackedArea {
						panels: ctx.panels,
						on_band: Some(Callback::new(move |api: PackedApi| ctx.entered(api))),
						config: Some(tilers_dioxus::Config { storage_key: Some("tedi-board".into()), ..Default::default() }),
					}
				}
			}
		}
		Palette {}
	}
}

impl Ctx {
	/// Each band keeps its own layout (a phone's is not a desktop's): a restored one gets its panels back
	/// from their ids, a fresh one opens the first team view.
	fn entered(self, api: PackedApi) {
		let mut handle = self.api;
		handle.set(Some(api));
		let Some(setup) = self.setup.peek().clone() else { return }; // `Live` opens the first view once setup arrives
		match api.restored() {
			true => self.rehost(&setup, api.tab_ids()),
			false => self.open_first(&setup),
		}
	}

	fn open_first(&self, setup: &Setup) {
		if let Some(view) = setup.team.first().or(setup.personal.first()) {
			self.open_view(view.clone());
		}
	}

	/// Panels a restored layout hosts, rebuilt from their ids.
	fn rehost(&self, setup: &Setup, ids: Vec<PanelId>) {
		let mut panels = self.panels;
		for id in ids {
			if panels.peek().iter().any(|p| p.id == id) {
				continue;
			}
			let (title, content) = match id.0.split_once(':') {
				Some(("view", name)) => match setup.team.iter().chain(&setup.personal).find(|v| v.name == name) {
					Some(view) => (name.to_string(), rsx! { view::ViewPanel { view: view.clone() } }),
					None => (name.to_string(), rsx! { "The view {name} is no longer saved" }),
				},
				Some(("scope", path)) => match find_scope(&setup.scopes, path) {
					Some(scope) => (path.to_string(), rsx! { view::ViewPanel { view: scope_view(path, scope) } }),
					None => (path.to_string(), rsx! { "The scope {path} is no longer configured" }),
				},
				Some(("sprint", title)) => match setup.sprints.iter().find(|s| s.title == title) {
					Some(sprint) => (title.to_string(), rsx! { view::SprintPanel { sprint: sprint.clone() } }),
					None => (title.to_string(), rsx! { "The sprint {title} is closed" }),
				},
				Some(("issue", link)) => match IssueLink::parse(link) {
					Some(link) => (format!("#{}", link.number()), rsx! { issue::IssuePanel { link } }),
					None => (id.0.clone(), rsx! { "Not an issue: {link}" }),
				},
				_ => (id.0.clone(), rsx! { "Unknown panel {id.0}" }),
			};
			panels.write().push(DockPanel {
				id,
				title,
				content,
				min: MinSize::Rem { w: 16.0, h: 8.0 },
			});
		}
	}
}

/// Setup, then the board, reloaded on every change the server announces.
#[component]
fn Live() -> Element {
	let ctx = use_context::<Ctx>();
	let toaster = ctx.toaster;
	use_future(move || async move {
		match api::setup().await {
			Ok(setup) => {
				let mut s = ctx.setup;
				s.set(Some(setup.clone()));
				if let Some(api) = *ctx.api.peek() {
					match api.restored() {
						true => ctx.rehost(&setup, api.tab_ids()),
						false => ctx.open_first(&setup),
					}
				}
			}
			Err(e) => _ = toaster.error(format!("setup: {e}")),
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
	let Some(setup) = setup.as_ref() else {
		return rsx! { SidebarContent { Spinner {} } };
	};
	let unreadable = ctx.board.read().as_ref().map_or(0, |b| b.unreadable.len());
	rsx! {
		SidebarHeader { class: "font-semibold px-3", "tedi" }
		SidebarContent {
			SidebarGroup {
				SidebarGroupLabel { "Views" }
				SidebarGroupContent {
					SidebarMenu {
						for view in setup.team.iter().chain(&setup.personal).cloned() {
							SidebarMenuItem { key: "{view.name}",
								SidebarMenuButton { onclick: { let view = view.clone(); move |_| ctx.open_view(view.clone()) }, "★ {view.name}" }
							}
						}
					}
				}
			}
			if !setup.scopes.is_empty() {
				SidebarGroup {
					SidebarGroupLabel { "Scopes" }
					SidebarGroupContent {
						SidebarMenu {
							for (name, scope) in setup.scopes.clone() {
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
								SidebarMenuItem { key: "{sprint.title}",
									SidebarMenuButton { onclick: { let sprint = sprint.clone(); move |_| ctx.open_sprint(sprint.clone()) }, "{sprint.title}" }
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

#[component]
fn ScopeItem(path: String, name: String, scope: Scope, depth: usize) -> Element {
	let ctx = use_context::<Ctx>();
	rsx! {
		SidebarMenuItem {
			div { style: "padding-left: {depth * 12}px",
				SidebarMenuButton { onclick: { let (path, scope) = (path.clone(), scope.clone()); move |_| ctx.open_scope(path.clone(), &scope) }, "{name}" }
			}
		}
		for (child, nested) in scope.children.clone() {
			ScopeItem { key: "{child}", path: format!("{path} / {child}"), name: child, scope: nested, depth: depth + 1 }
		}
	}
}

fn find_scope<'a>(scopes: &'a BTreeMap<String, Scope>, path: &str) -> Option<&'a Scope> {
	let mut names = path.split(" / ");
	let mut scope = scopes.get(names.next()?)?;
	for name in names {
		scope = scope.children.get(name)?;
	}
	Some(scope)
}

/// A scope opens as the open work of its repos, by stage.
fn scope_view(name: &str, scope: &Scope) -> View {
	View {
		name: name.to_string(),
		query: format!("{} is:open", scope.filter()),
		layout: Layout::Board,
		group_by: GroupBy::Stage,
		sort: Sort::Rank,
		shown: vec![Shown::Repo, Shown::Assignees, Shown::Importance, Shown::Hours, Shown::Type, Shown::Blockers],
	}
}

/// ⌘K: every view, scope, sprint and open issue by name.
#[component]
fn Palette() -> Element {
	let ctx = use_context::<Ctx>();
	let mut open = use_signal(|| false);
	use_command_palette_shortcut(move || open.toggle(), true);
	let item = |id: String, label: String, on_select: EventHandler<()>| CommandPaletteItem {
		id,
		label,
		icon: None,
		shortcut: None,
		keywords: Vec::new(),
		disabled: false,
		on_select,
	};
	let mut groups = Vec::new();
	if let Some(setup) = ctx.setup.read().as_ref() {
		let mut views: Vec<CommandPaletteItem> = setup
			.team
			.iter()
			.chain(&setup.personal)
			.cloned()
			.map(|v| item(format!("view:{}", v.name), v.name.clone(), EventHandler::new(move |_| ctx.open_view(v.clone()))))
			.collect();
		fn scopes(prefix: &str, all: &BTreeMap<String, Scope>, out: &mut Vec<(String, Scope)>) {
			for (name, scope) in all {
				let path = format!("{prefix}{name}");
				out.push((path.clone(), scope.clone()));
				scopes(&format!("{path} / "), &scope.children, out);
			}
		}
		let mut all = Vec::new();
		scopes("", &setup.scopes, &mut all);
		views.extend(all.into_iter().map(|(path, scope)| {
			item(
				format!("scope:{path}"),
				format!("Scope: {path}"),
				EventHandler::new(move |_| ctx.open_scope(path.clone(), &scope)),
			)
		}));
		views.extend(setup.sprints.iter().cloned().map(|s| {
			item(
				format!("sprint:{}", s.title),
				format!("Sprint: {}", s.title),
				EventHandler::new(move |_| ctx.open_sprint(s.clone())),
			)
		}));
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
				.map(|c| {
					let link = c.link.clone();
					item(
						format!("issue:{link}"),
						format!("{} #{} {}", c.link.project(), c.link.number(), c.title),
						EventHandler::new(move |_| ctx.open_issue(link.clone())),
					)
				})
				.collect(),
		});
	}
	rsx! {
		CommandPalette { open: open(), on_open_change: move |v| open.set(v), groups }
	}
}
