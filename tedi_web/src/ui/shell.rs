//! The screen's frame (Figma: Sidebar, SidebarSection, SidebarRow, TopBar, ScopeBar): the viewer's shelves
//! on the left, the scope and the workspaces above the dock.

use std::collections::BTreeMap;

use dioxus::prelude::*;
use ev_lib::uikit_dioxus::{Popover, PopoverContent, PopoverTrigger, Sheet, SheetContent};
use tedi_core::board::{Query, Scope, View, scope_at};
use tilers_dioxus::PanelId;

use super::{
	Ctx, Editing, Modal,
	card::Avatar,
	icons::{Glyph, Icon},
	kit::{IconBtn, Kbd, MenuItem, Picker},
	library::NameForm,
	workspace,
};
use crate::api::{Item, Kept, LibraryEdit, Shelf};

/// Open cards a query holds within the scope on screen; \`None\` before the board arrives or while it doesn't parse.
fn count(ctx: &Ctx, query: &str) -> Option<usize> {
	let board = ctx.board.read();
	let board = board.as_ref()?;
	let mut q: Query = query.parse().ok()?;
	if let Some((_, Some(filter))) = ctx.scope() {
		q.narrow(&filter.parse().expect("a scope's filter is a repo clause"));
	}
	Some(q.run(&board.cards, ctx.me().as_deref()).ok()?.len())
}

/// What a view's query is about, as the design draws it.
pub(super) fn glyph_of(view: &View) -> Glyph {
	let q = &view.query;
	match () {
		_ if q.contains("is:pr") => Glyph::Pr,
		_ if q.contains("assignee:@me") => Glyph::User,
		_ if q.contains("is:blocked") => Glyph::Blocker,
		_ => Glyph::View,
	}
}

/// The shelves; on a phone the same in a sheet behind ☰.
#[component]
pub(super) fn Nav() -> Element {
	let ctx = use_context::<Ctx>();
	let mut sheet = ctx.sheet;
	rsx! {
		aside { class: "hidden w-(--size-rail) shrink-0 border-r border-border-subtle bg-card md:flex", Shelves {} }
		Sheet { open: sheet(), on_open_change: move |v| sheet.set(v),
			SheetContent { side: ev_lib::uikit_dioxus::primitives::Side::Left, aria_label: "Shelves", class: "w-[312px] bg-card p-0",
				// a choice made in the sheet also closes it
				div { class: "flex h-full", onclick: move |_| sheet.set(false), Shelves {} }
			}
		}
	}
}

#[component]
fn Shelves() -> Element {
	let ctx = use_context::<Ctx>();
	let setup = ctx.setup.read();
	let library = ctx.library.read();
	let (Some(setup), Some(library)) = (setup.as_ref(), library.as_ref()) else {
		return rsx! {
			div { class: "flex flex-1 flex-col gap-1 p-2", Brand {} }
		};
	};
	let starred = |f: &Item| library.favorites.contains(f);
	let mut workspaces: Vec<Row> = library
		.workspaces
		.iter()
		.map(|(name, kept)| Row {
			glyph: Glyph::Workspace,
			label: name.clone(),
			depth: 0,
			go: Go::Workspace(name.clone()),
			favorite: Some(Item::Workspace(name.clone())),
			own: Some(Shelf::Workspaces),
			current: library.workspace.as_ref() == Some(name),
			meta: kept.keys().map(ToString::to_string).collect::<Vec<_>>().join("·"),
		})
		.collect();
	workspaces.sort_by_key(|r| !r.favorite.as_ref().is_some_and(starred)); // stable: favorites first, else in order

	let mut views: Vec<Row> = ctx
		.views()
		.into_iter()
		.map(|v| Row {
			glyph: glyph_of(&v),
			favorite: Some(Item::View(v.name.clone())),
			own: library.filters.contains_key(&v.name).then_some(Shelf::Filters),
			current: false,
			depth: 0,
			meta: count(&ctx, &v.query).map(|n| n.to_string()).unwrap_or_default(),
			go: Go::Panel(PanelId(format!("view:{}", v.name))),
			label: v.name,
		})
		.collect();
	views.sort_by_key(|r| !r.favorite.as_ref().is_some_and(starred));

	let scopes = library.scopes(&setup.scopes).expect("the server serves no library whose scope lost its parent");
	let mut scoped = Vec::new();
	fn walk(ctx: &Ctx, prefix: &str, level: &BTreeMap<String, Scope>, depth: usize, own: &BTreeMap<String, Scope>, current: Option<&String>, out: &mut Vec<Row>) {
		for (name, scope) in level {
			let path = if prefix.is_empty() { name.clone() } else { format!("{prefix} / {name}") };
			out.push(Row {
				glyph: Glyph::Scope,
				label: name.clone(),
				depth,
				favorite: Some(Item::Scope(path.clone())),
				own: own.contains_key(&path).then_some(Shelf::Scopes),
				current: current == Some(&path),
				meta: count(ctx, &format!("{} is:open", scope.filter())).map(|n| n.to_string()).unwrap_or_default(),
				go: Go::Scope(path.clone()),
			});
			walk(ctx, &path, &scope.children, depth + 1, own, current, out);
		}
	}
	walk(&ctx, "", &scopes, 0, &library.scopes, library.scope.as_ref(), &mut scoped);

	let columns: Vec<Row> = setup
		.columns
		.iter()
		.map(|(name, c)| (name, c, None))
		.chain(library.columns.iter().map(|(name, c)| (name, c, Some(Shelf::Columns))))
		.map(|(name, c, own)| Row {
			glyph: Glyph::Columns,
			label: name.clone(),
			depth: 0,
			go: Go::Columns(name.clone()),
			favorite: Some(Item::Columns(name.clone())),
			own,
			current: false,
			meta: c.tints.len().to_string(),
		})
		.collect();

	let sprints: Vec<Row> = setup
		.sprints
		.iter()
		.map(|s| Row {
			glyph: if s.title == "urgent" { Glyph::Alert } else { Glyph::Sprint },
			label: s.title.clone(),
			depth: 0,
			go: Go::Panel(PanelId(format!("sprint:{}", s.title))),
			favorite: None,
			own: None,
			current: false,
			meta: s.links.len().to_string(),
		})
		.collect();

	let unreadable = ctx.board.read().as_ref().map_or(0, |b| b.unreadable.len()); // the board arrives after the shelves
	let me = ctx.me();
	rsx! {
		div { class: "flex min-h-0 flex-1 flex-col gap-1 p-2",
			Brand {}
			div { class: "flex min-h-0 flex-1 flex-col gap-1 overflow-y-auto overflow-x-hidden",
				Section { title: "Workspaces", kind: "workspaces", rows: workspaces, empty: "Arrange the screen, then + to keep it", add: Add::Named("save this screen as…") }
				Section { title: "Views", kind: "views", rows: views, empty: "No views", add: Add::Builder }
				Section { title: "Scopes", kind: "scopes", rows: scoped, empty: "No scopes", add: Add::Dialog }
				Section { title: "Column layouts", kind: "columns", rows: columns, empty: "Tint a board's columns, then keep them", add: Add::None }
				if !sprints.is_empty() {
					Section { title: "Sprints", kind: "", rows: sprints, empty: "", add: Add::None }
				}
			}
			div { class: "flex h-9 shrink-0 items-center gap-2 border-t border-border-subtle px-2 pt-1",
				if unreadable > 0 {
					button {
						r#type: "button",
						class: "inline-flex h-[22px] items-center gap-1 rounded-[var(--control-radius)] border border-chip-warn-border px-1.5 text-caption text-accent-warn hover:bg-hover",
						onclick: move |e: MouseEvent| { e.stop_propagation(); let mut m = ctx.modal; m.set(Some(Modal::Unreadable)); },
						Icon { glyph: Glyph::Alert, class: "size-3" }
						if unreadable == 1 { "1 issue doesn't parse" } else { "{unreadable} issues don't parse" }
					}
				}
				div { class: "flex-1" }
				if let Some(me) = me {
					Avatar { login: me }
				}
			}
		}
	}
}

/// "tedi", and the command palette behind "Jump… ⌘K".
#[component]
fn Brand() -> Element {
	let ctx = use_context::<Ctx>();
	rsx! {
		div { class: "flex h-9 shrink-0 items-center gap-2 pr-1 pl-2",
			span { class: "text-h3 text-ink", "tedi" }
			div { class: "flex-1" }
			button {
				r#type: "button",
				class: "flex h-6 items-center gap-1.5 rounded-[var(--control-radius)] border border-border-subtle px-1.5 text-caption text-ink-faint hover:bg-hover",
				onclick: move |e: MouseEvent| { e.stop_propagation(); let mut p = ctx.palette; p.set(true); },
				Icon { glyph: Glyph::Search, class: "size-3" }
				"Jump…"
				Kbd { "⌘K" }
			}
		}
	}
}

#[derive(Clone, PartialEq)]
struct Row {
	glyph: Glyph,
	label: String,
	depth: usize,
	go: Go,
	favorite: Option<Item>,
	/// Kept by the viewer, on this shelf: renamed and deleted in place. None: the team's, read-only.
	own: Option<Shelf>,
	current: bool,
	/// A count of cards in the scope, or the bands a workspace has arranged.
	meta: String,
}

#[derive(Clone, PartialEq)]
enum Go {
	/// Opened on the dock, or dragged onto it.
	Panel(PanelId),
	Workspace(String),
	Scope(String),
	Columns(String),
}

#[derive(Clone, Copy, PartialEq)]
enum Add {
	None,
	/// A name typed in place.
	Named(&'static str),
	Dialog,
	/// The view builder, on a blank draft.
	Builder,
}

/// \`VIEWS 🔍 +\`: collapse, browse this kind in the library, + new from the screen.
#[component]
fn Section(title: &'static str, kind: &'static str, rows: Vec<Row>, empty: &'static str, add: Add) -> Element {
	let ctx = use_context::<Ctx>();
	let mut open = use_signal(|| true);
	let mut adding = use_signal(|| false);
	let named = move |name: String| {
		adding.set(false);
		match kind {
			"workspaces" => {
				let api = (*ctx.api.peek()).expect("a band is entered before the screen can be saved");
				let edit = LibraryEdit::Keep(Kept::Workspace {
					name,
					tiles: BTreeMap::from([(api.band(), api.save())]),
				});
				spawn(async move { _ = ctx.keep(edit).await });
			}
			_ => unreachable!("only workspaces are named in place"),
		}
	};
	rsx! {
		div { class: "flex flex-col",
			div { class: "group/sec flex h-7 shrink-0 items-center gap-1 pr-1 pl-2",
				button { r#type: "button", class: "flex min-w-0 flex-1 items-center gap-1", onclick: move |e: MouseEvent| { e.stop_propagation(); open.toggle(); },
					Icon { glyph: if open() { Glyph::ChevronDown } else { Glyph::ChevronRight }, class: "size-3 text-ink-faint" }
					span { class: "truncate text-overline text-ink-soft", "{title}" }
				}
				if !kind.is_empty() {
					IconBtn { glyph: Glyph::Search, title: "Browse {kind} in the library", onclick: move |_| ctx.open(PanelId(format!("library:{kind}"))) }
				}
				match add {
					Add::None => rsx! {},
					Add::Named(_) => rsx! { IconBtn { glyph: Glyph::Plus, title: "New, from this screen", onclick: move |_| adding.set(true) } },
					Add::Dialog => rsx! { IconBtn { glyph: Glyph::Plus, title: "New", onclick: move |_| { let mut m = ctx.modal; m.set(Some(Modal::Scope(Editing::New))); } } },
					Add::Builder => rsx! { IconBtn { glyph: Glyph::Plus, title: "Build a view", onclick: move |_| ctx.open(PanelId("builder:".into())) } },
				}
			}
			if open() {
				if let (true, Add::Named(placeholder)) = (adding(), add) {
					div { class: "px-2 py-1", onclick: |e: MouseEvent| e.stop_propagation(),
						NameForm { placeholder, on_name: named, on_cancel: move |()| adding.set(false) }
					}
				}
				if rows.is_empty() && !adding() && !empty.is_empty() {
					span { class: "px-2 py-1 text-caption text-ink-faint", "{empty}" }
				}
				for row in rows {
					ShelfRow { key: "{row.label}-{row.depth}-{key_of(&row.go)}", row }
				}
			}
		}
	}
}

fn key_of(go: &Go) -> String {
	match go {
		Go::Panel(id) => id.0.clone(),
		Go::Workspace(n) => format!("workspace:{n}"),
		Go::Scope(p) => format!("scope:{p}"),
		Go::Columns(n) => format!("columns:{n}"),
	}
}

/// A click opens it; a panel drags onto the dock; hovered, the viewer's own offer rename and delete, the
/// team's a fork; ★ favorites it.
#[component]
fn ShelfRow(row: Row) -> Element {
	let ctx = use_context::<Ctx>();
	let mut renaming = use_signal(|| false);
	let mut deleting = use_signal(|| false);
	let Row {
		glyph,
		label,
		depth,
		go,
		favorite,
		own,
		current,
		meta,
	} = row;
	let starred = favorite.as_ref().is_some_and(|f| ctx.favorite(f));
	let activate = {
		let go = go.clone();
		move |_| match go.clone() {
			Go::Panel(id) => ctx.open(id),
			Go::Workspace(name) => ctx.switch(&name),
			Go::Scope(path) => {
				spawn(async move { _ = ctx.keep(LibraryEdit::Scope(Some(path))).await });
			}
			Go::Columns(name) => ctx.open(PanelId(format!("library:columns:{name}"))),
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
	let fork = {
		let (go, label) = (go.clone(), label.clone());
		move |_| match &go {
			Go::Scope(path) => {
				let mut m = ctx.modal;
				m.set(Some(Modal::Scope(if own.is_some() { Editing::Own(path.clone()) } else { Editing::Team(path.clone()) })));
			}
			Go::Panel(id) if id.0.starts_with("view:") => ctx.open(PanelId(format!("builder:{label}"))),
			_ => ctx.open(PanelId(format!("library:{}", key_of(&go)))),
		}
	};
	let face = if current { "bg-selected text-ink" } else { "text-ink-mid hover:bg-hover hover:text-ink" };
	let pad = 8 + depth * 12;
	rsx! {
		div {
			class: "group/row relative flex h-7 shrink-0 cursor-pointer items-center gap-1.5 rounded-[var(--control-radius)] pr-1.5 {face}",
			style: "padding-left: {pad}px",
			draggable: if matches!(go, Go::Panel(_)) { "true" } else { "false" },
			ondragstart: {
				let go = go.clone();
				move |e: DragEvent| {
					let Go::Panel(id) = &go else { return };
					e.data_transfer().set_data("text/plain", &id.0).expect("a dragstart's transfer is writable"); // Firefox starts no drag without data
					ctx.carry(id.clone());
				}
			},
			onclick: activate,
			Icon { glyph, class: if current { "size-3.5 text-primary-ink" } else { "size-3.5 text-ink-soft" } }
			if renaming() {
				div { class: "min-w-0 flex-1", onclick: |e: MouseEvent| e.stop_propagation(),
					NameForm { initial: label.clone(), placeholder: "name", on_name: rename, on_cancel: move |()| renaming.set(false) }
				}
			} else {
				span { class: "min-w-0 flex-1 truncate text-body", "{label}" }
				if starred {
					Icon { glyph: Glyph::Star, class: "size-3 fill-accent-warn text-accent-warn group-hover/row:hidden" }
				}
				if own.is_none() && !matches!(go, Go::Panel(ref id) if id.0.starts_with("sprint:")) {
					span { class: "group-hover/row:hidden", title: "the team's, read-only: fork to edit", Icon { glyph: Glyph::Lock, class: "size-3 text-ink-faint" } }
				}
				if !meta.is_empty() {
					span { class: "text-mono-s text-ink-faint group-hover/row:hidden", "{meta}" }
				}
				div { class: "hidden items-center group-hover/row:flex",
					if let Some(what) = favorite.clone() {
						IconBtn { glyph: Glyph::Star, title: if starred { "Unfavorite" } else { "Favorite" }, active: starred, onclick: move |_| { let what = what.clone(); spawn(async move { _ = ctx.keep(LibraryEdit::Favorite { what, on: !starred }).await; }); } }
					}
					match own {
						Some(shelf) => rsx! {
							if shelf == Shelf::Filters {
								IconBtn { glyph: Glyph::Fields, title: "Edit in the builder", onclick: { let label = label.clone(); move |_| ctx.open(PanelId(format!("builder:{label}"))) } }
							}
							IconBtn { glyph: Glyph::Pencil, title: "Rename", onclick: move |_| renaming.set(true) }
							if deleting() {
								button {
									r#type: "button",
									class: "rounded px-1 text-caption text-accent-error hover:bg-hover",
									onclick: { let name = label.clone(); move |e: MouseEvent| { e.stop_propagation(); let name = name.clone(); deleting.set(false); spawn(async move { _ = ctx.keep(LibraryEdit::Drop { shelf, name }).await; }); } },
									onmouseleave: move |_| deleting.set(false),
									"delete?"
								}
							} else {
								IconBtn { glyph: Glyph::X, title: "Delete", onclick: move |_| deleting.set(true) }
							}
						},
						None if !matches!(go, Go::Panel(ref id) if id.0.starts_with("sprint:")) => rsx! { IconBtn { glyph: Glyph::Fork, title: "Fork into your library", onclick: fork } },
						None => rsx! {},
					}
				}
			}
		}
	}
}

/// The scope everything on screen is narrowed to, the repos that is, and its editor; then the workspaces.
#[component]
pub(super) fn TopBar() -> Element {
	let ctx = use_context::<Ctx>();
	let library = ctx.library.read();
	let Some(library) = library.as_ref() else {
		return rsx! { div { class: "h-(--size-topbar) shrink-0 border-b border-border-subtle bg-muted" } };
	};
	let current = ctx.scope();
	let mut workspaces: Vec<String> = library.workspaces.keys().cloned().collect();
	workspaces.sort_by_key(|w| !library.favorites.contains(&Item::Workspace(w.clone()))); // stable: as the shelf lists them
	let at = library.workspace.clone();
	let band = ctx.band();
	let mut sheet = ctx.sheet;
	if band == Some(tilers_dioxus::Band::Sm) {
		return rsx! {
			div { class: "flex h-12 shrink-0 items-center gap-2 border-b border-border-subtle bg-muted px-2",
				IconBtn { glyph: Glyph::Menu, title: "Shelves", onclick: move |_| sheet.set(true) }
				ScopePicker {}
				div { class: "flex-1" }
				IconBtn { glyph: Glyph::Search, title: "Jump to…", onclick: move |_| { let mut p = ctx.palette; p.set(true); } }
				if let Some(me) = ctx.me() { Avatar { login: me } }
			}
		};
	}
	rsx! {
		div { class: "flex h-(--size-topbar) shrink-0 items-center gap-2 border-b border-border-subtle bg-muted pr-2",
			button { r#type: "button", class: "ml-2 flex size-7 items-center justify-center text-ink-soft md:hidden", aria_label: "Shelves", onclick: move |_| sheet.set(true), Icon { glyph: Glyph::Menu } }
			div { class: "flex shrink-0 items-center gap-2 pl-2",
				ScopePicker {}
				match &current {
					None => rsx! {},
					Some((path, None)) => rsx! { span { class: "text-caption text-accent-error", "{path} is no longer configured" } },
					Some((path, Some(filter))) => {
						let editing = if library.scopes.contains_key(path) { Editing::Own(path.clone()) } else { Editing::Team(path.clone()) };
						rsx! {
							span { class: "hidden h-5 max-w-56 items-center truncate rounded-[var(--control-radius)] bg-secondary px-1.5 text-mono-s text-ink-soft lg:inline-flex", title: "What every view, facet and count here is narrowed by", "{filter}" }
							button {
								r#type: "button",
								class: "inline-flex h-6 items-center gap-1 rounded-[var(--control-radius)] px-2 text-body-s font-medium text-ink-mid hover:bg-hover hover:text-ink",
								onclick: move |_| { let mut m = ctx.modal; m.set(Some(Modal::Scope(editing.clone()))); },
								Icon { glyph: Glyph::Pencil }
								"Edit"
							}
						}
					}
				}
			}
			div { class: "hidden h-5 w-px bg-border md:block" }
			div { class: "hidden h-full min-w-0 items-center md:flex",
				for name in workspaces {
					button {
						key: "{name}",
						r#type: "button",
						class: if at.as_ref() == Some(&name) { "flex h-full shrink-0 items-center gap-1.5 border-t-2 border-primary bg-card pr-2 pl-3 text-body text-ink" } else { "flex h-full shrink-0 items-center gap-1.5 border-r border-border-subtle pr-2 pl-3 text-body text-ink-mid hover:text-ink" },
						onclick: { let name = name.clone(); move |_| ctx.switch(&name) },
						Icon { glyph: Glyph::Workspace, class: "size-3.5 text-ink-soft" }
						"{name}"
					}
				}
				IconBtn { glyph: Glyph::Plus, title: "New workspace from this screen", onclick: move |_| ctx.open(PanelId("library:workspaces".into())) }
				workspace::WorkspaceMenu {}
			}
			div { class: "flex-1" }
			if let Some(band) = band {
				span { class: "hidden h-5 items-center rounded-[var(--control-radius)] border border-border-subtle px-1.5 text-mono-s text-ink-soft md:inline-flex", title: "the arrangement live for this window's width", "{band}" }
			}
			IconBtn { glyph: Glyph::Share, title: "Copy a link to this screen", onclick: move |_| workspace::share_screen(ctx) }
			IconBtn { glyph: Glyph::Help, title: "Keys and help", onclick: move |_| { let mut m = ctx.modal; m.set(Some(Modal::Help)); } }
		}
	}
}

/// \`EV / EV-Service-Arb ▾\`: the scope tree with open-card counts, starred first.
#[component]
fn ScopePicker() -> Element {
	let ctx = use_context::<Ctx>();
	let mut open = use_signal(|| false);
	let scopes = ctx.scopes();
	let library = ctx.library.read();
	let library = library.as_ref().expect("the top bar renders once the library arrived");
	let mut rows: Vec<(String, String, usize, bool)> = Vec::new();
	fn walk(prefix: &str, level: &BTreeMap<String, Scope>, depth: usize, own: &BTreeMap<String, Scope>, out: &mut Vec<(String, String, usize, bool)>) {
		for (name, scope) in level {
			let path = if prefix.is_empty() { name.clone() } else { format!("{prefix} / {name}") };
			out.push((path.clone(), name.clone(), depth, own.contains_key(&path)));
			walk(&path, &scope.children, depth + 1, own, out);
		}
	}
	walk("", &scopes, 0, &library.scopes, &mut rows);
	rows.sort_by_key(|(path, ..)| !library.favorites.contains(&Item::Scope(path.clone()))); // stable: the tree's order within each
	let current = library.scope.clone();
	let (parent, leaf) = match current.as_deref().map(|p| p.rsplit_once(" / ").unwrap_or(("", p))) {
		Some((parent, leaf)) => (parent.to_string(), leaf.to_string()),
		None => (String::new(), "All repos".to_string()),
	};
	let mut pick = move |path: Option<String>| {
		open.set(false);
		spawn(async move { _ = ctx.keep(LibraryEdit::Scope(path)).await });
	};
	rsx! {
		Popover { open: open(), on_open_change: move |v| open.set(v),
			PopoverTrigger { class: if open() { "flex h-7 items-center gap-1.5 rounded-[var(--control-radius)] border border-ring px-2" } else { "flex h-7 items-center gap-1.5 rounded-[var(--control-radius)] border border-border px-2 hover:bg-hover" },
				Icon { glyph: Glyph::Scope, class: "size-3.5 text-chart-3" }
				if !parent.is_empty() {
					span { class: "hidden text-body text-ink-soft sm:inline", "{parent}" }
					span { class: "hidden text-body text-ink-faint sm:inline", "/" }
				}
				span { class: "text-body font-medium text-ink", "{leaf}" }
				Icon { glyph: Glyph::ChevronDown, class: "size-3.5 text-ink-soft" }
			}
			PopoverContent { align: "start", class: "flex w-[260px] flex-col gap-0.5 rounded-[var(--radius)] border border-border bg-popover p-1 text-ink",
				span { class: "px-2 pt-1.5 pb-0.5 text-overline text-ink-soft", "scope…" }
				button { r#type: "button", class: if current.is_none() { "flex h-7 items-center gap-2 rounded-[var(--control-radius)] bg-selected px-2" } else { "flex h-7 items-center gap-2 rounded-[var(--control-radius)] px-2 hover:bg-hover" }, onclick: move |_| pick(None),
					span { class: "flex-1 text-left text-body text-ink", "All repos" }
					span { class: "text-mono-s text-ink-soft", {count(&ctx, "is:open").map(|n| n.to_string()).unwrap_or_default()} }
				}
				for (path , name , depth , own) in rows {
					{
						let filter = scope_at(&scopes, &path).expect("walked from these scopes");
						let n = count(&ctx, &format!("{filter} is:open")).map(|n| n.to_string()).unwrap_or_default();
						let starred = library.favorites.contains(&Item::Scope(path.clone()));
						let pad = 8 + depth * 12;
						rsx! {
							button {
								key: "{path}",
								r#type: "button",
								class: if current.as_ref() == Some(&path) { "flex h-7 items-center gap-2 rounded-[var(--control-radius)] bg-selected pr-2" } else { "flex h-7 items-center gap-2 rounded-[var(--control-radius)] pr-2 hover:bg-hover" },
								style: "padding-left: {pad}px",
								onclick: { let path = path.clone(); move |_| pick(Some(path.clone())) },
								span { class: "flex-1 truncate text-left text-body text-ink", "{name}" if own { span { class: "text-ink-soft", " · mine" } } }
								if starred { Icon { glyph: Glyph::Star, class: "size-3 fill-accent-warn text-accent-warn" } }
								span { class: "text-mono-s text-ink-soft", "{n}" }
							}
						}
					}
				}
				div { class: "my-0.5 h-px bg-border-subtle" }
				button { r#type: "button", class: "flex h-7 items-center gap-2 rounded-[var(--control-radius)] px-2 hover:bg-hover",
					onclick: move |_| { open.set(false); ctx.open(PanelId("library:scopes".into())); },
					span { class: "flex-1 text-left text-body text-ink", "Edit scopes…" }
					Kbd { "⌘⇧P" }
				}
			}
		}
	}
}

/// On a phone, the thumb zone: which workspace this is, and the band it is arranged for.
#[component]
pub(super) fn ThumbBar() -> Element {
	let ctx = use_context::<Ctx>();
	if ctx.band() != Some(tilers_dioxus::Band::Sm) {
		return rsx! {};
	}
	let library = ctx.library.read();
	let Some(library) = library.as_ref() else { return rsx! {} };
	let names: Vec<String> = library.workspaces.keys().cloned().collect();
	let current = library.workspace.clone();
	rsx! {
		div { class: "flex h-14 shrink-0 items-center gap-2 border-t border-border-subtle bg-muted px-3",
			Picker { glyph: Glyph::Workspace, label: current.clone().unwrap_or_else(|| "no workspace".into()),
				for name in names {
					MenuItem { key: "{name}", selected: current.as_ref() == Some(&name), label: name.clone(), onclick: { let name = name.clone(); move |_| ctx.switch(&name) } }
				}
			}
			div { class: "flex-1" }
			span { class: "inline-flex h-5 items-center rounded-[var(--control-radius)] border border-border-subtle px-1.5 text-mono-s text-ink-soft", "sm" }
		}
	}
}
