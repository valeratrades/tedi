//! The screen's frame (Figma: Sidebar, SidebarSection, SidebarRow, TopBar): the viewer's shelves on the left, the
//! open workspace's name above the dock.

use dioxus::prelude::*;
use ev_lib::uikit_dioxus::{Popover, PopoverContent, PopoverTrigger, Sheet, SheetContent};
use tedi_core::board::Slice;
use tilers_dioxus::{Band, PanelId};

use super::{
	Ctx, Editing, Modal,
	card::Avatar,
	icons::{Glyph, Icon},
	kit::{IconBtn, Kbd},
	library::NameForm,
	route::Go as Link,
	sprint_panel,
	toolbar::{FieldsMenu, GroupMenu, Keep},
	view::{count, outcome},
	workspace,
};
use crate::api::{Item, LibraryEdit, Shelf, Workspace};

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
	let team = setup.workspaces.iter().map(|(name, w)| (name, w, None));
	let own = library.workspaces.iter().map(|(name, w)| (name, w, Some(Shelf::Workspaces)));
	let open = ctx.open_workspace();
	let mut workspaces: Vec<Row> = team
		.chain(own)
		.map(|(name, Workspace { tiles, .. }, own)| Row {
			glyph: Glyph::Workspace,
			label: name.clone(),
			depth: 0,
			go: Go::Workspace(name.clone()),
			favorite: Some(Item::Workspace(name.clone())),
			own,
			current: open.as_ref() == Some(name),
			meta: tiles.keys().map(ToString::to_string).collect::<Vec<_>>().join("·"),
		})
		.collect();
	workspaces.sort_by_key(|r| !r.favorite.as_ref().is_some_and(starred)); // stable: favorites first, else in order

	let mut views: Vec<Row> = ctx
		.views()
		.into_iter()
		.map(|v| Row {
			glyph: Glyph::View,
			favorite: Some(Item::View(v.name.clone())),
			own: library.views.contains_key(&v.name).then_some(Shelf::Views),
			current: ctx.shows(&PanelId(format!("view:{}", v.name))),
			depth: 0,
			meta: String::new(),
			go: Go::Panel(PanelId(format!("view:{}", v.name))),
			label: v.name,
		})
		.collect();
	views.sort_by_key(|r| !r.favorite.as_ref().is_some_and(starred));

	let scopes: Vec<Row> = ctx
		.scopes()
		.into_iter()
		.map(|s| Row {
			glyph: Glyph::Scope,
			label: s.name().to_string(),
			depth: s.depth,
			favorite: Some(Item::Scope(s.path.clone())),
			own: library.scopes.contains_key(&s.path).then_some(Shelf::Scopes),
			current: false,                                                                    // applied, it is its terms
			meta: ctx.open_in(&Slice::Terms(s.scope.filter())).map(|n| n.to_string()).unwrap_or_default(), // before the board arrives
			go: Go::Scope(s.path),
		})
		.collect();

	let view = ctx.current_view();
	let (laid, by) = (view.as_ref().and_then(|v| ctx.laid(v)), view.as_ref().and_then(|v| ctx.view(v)).map(|v| v.group_by));
	let columns: Vec<Row> = setup
		.columns
		.iter()
		.map(|(name, c)| (name, c, None))
		.chain(library.columns.iter().map(|(name, c)| (name, c, Some(Shelf::Columns))))
		.map(|(name, c, own)| Row {
			glyph: Glyph::Columns,
			label: name.clone(),
			depth: 0,
			go: Go::Columns {
				name: name.clone(),
				onto: view.clone().filter(|_| by.as_ref() == Some(&c.by)),
			},
			favorite: Some(Item::Columns(name.clone())),
			own,
			current: laid.as_ref() == Some(name),
			meta: c.boxes.tints.len().to_string(),
		})
		.collect();

	let held = ctx.sprints();
	let sprints: Vec<Row> = held
		.iter()
		.map(|s| Row {
			glyph: Glyph::Sprint,
			label: s.title.clone(),
			depth: 0,
			go: Go::Panel(sprint_panel(&s.link)),
			favorite: None,
			own: None,
			current: ctx.shows(&sprint_panel(&s.link)),
			meta: match held.iter().filter(|o| o.title == s.title).count() {
				1 => s.nodes.len().to_string(),
				_ => format!("{} {}", s.link.repo_info().repo(), s.nodes.len()),
			},
		})
		.collect();

	let unreadable = ctx.board.read().unreadable.len();
	let me = ctx.me();
	rsx! {
		div { class: "flex min-h-0 flex-1 flex-col gap-1 p-2",
			Brand {}
			div { class: "flex min-h-0 flex-1 flex-col gap-1 overflow-y-auto overflow-x-hidden",
				Section { title: "Workspaces", kind: "workspaces", rows: workspaces, empty: "+ makes one", add: Add::Workspace }
				Section { title: "Views", kind: "views", rows: views, empty: "No views", add: Add::Builder }
				Section { title: "Scopes", kind: "scopes", rows: scopes, empty: "No scopes", add: Add::Dialog }
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
				if setup.accounts {
					button {
						r#type: "button",
						class: "inline-flex h-6 items-center gap-1 rounded-[var(--control-radius)] px-1 text-body-s text-ink-mid hover:bg-hover hover:text-ink",
						title: "Account",
						aria_label: "Account",
						onclick: move |e: MouseEvent| { e.stop_propagation(); let mut m = ctx.modal; m.set(Some(Modal::Account)); },
						match me {
							Some(me) => rsx! { Avatar { login: me } },
							None => rsx! { Icon { glyph: Glyph::User, class: "size-4" } "Bring your keys" },
						}
					}
				} else if let Some(me) = me {
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
	/// A scope's open cards, or the bands a workspace has arranged.
	meta: String,
}

#[derive(Clone, PartialEq)]
enum Go {
	/// Opened on the dock, or dragged onto it.
	Panel(PanelId),
	Workspace(String),
	/// Copied into the open workspace.
	Scope(String),
	Columns {
		name: String,
		/// The view on screen, when it groups as they do; else they open in the library.
		onto: Option<String>,
	},
}

#[derive(Clone, Copy, PartialEq)]
enum Add {
	None,
	/// An empty one, opened.
	Workspace,
	Dialog,
	/// The view builder, on a blank draft.
	Builder,
}

//#[codestyle::skip(swappable-args)]
/// `VIEWS 🔍 +`: collapse, browse this kind in the library, + a new one.
#[component]
fn Section(title: &'static str, kind: &'static str, rows: Vec<Row>, empty: &'static str, add: Add) -> Element {
	let ctx = use_context::<Ctx>();
	let mut open = use_signal(|| true);
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
					Add::Workspace => rsx! { IconBtn { glyph: Glyph::Plus, title: "New workspace", onclick: move |_| { spawn(ctx.new_workspace()); } } },
					Add::Dialog => rsx! { IconBtn { glyph: Glyph::Plus, title: "New", onclick: move |_| { let mut m = ctx.modal; m.set(Some(Modal::Scope(Editing::New(Default::default())))); } } },
					Add::Builder => rsx! { IconBtn { glyph: Glyph::Plus, title: "Build a view", onclick: move |_| ctx.open(PanelId("builder:".into())) } },
				}
			}
			if open() {
				if rows.is_empty() && !empty.is_empty() {
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
		Go::Columns { name, .. } => format!("columns:{name}"),
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
	let to = match &go {
		Go::Panel(id) => Some(ctx.at_panel(id)),
		Go::Workspace(name) => Some(ctx.at_workspace(name)),
		Go::Scope(_) => None, // an edit of the workspace, not a move
		Go::Columns { name, onto: Some(view) } => Some(ctx.at_columns(view, name)),
		Go::Columns { name, onto: None } => Some(ctx.at_panel(&PanelId(format!("library:columns:{name}")))),
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
					.await && shelf == Shelf::Views
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
			Icon { glyph, class: if current { "size-3.5 text-primary-ink" } else { "size-3.5 text-ink-soft" } }
			if renaming() {
				div { class: "min-w-0 flex-1", onclick: |e: MouseEvent| e.stop_propagation(),
					NameForm { initial: label.clone(), placeholder: "name", on_name: rename, on_cancel: move |()| renaming.set(false) }
				}
			} else {
				match (to, &go) {
					(Some(to), _) => rsx! { Link { to, class: "min-w-0 flex-1 truncate text-body after:absolute after:inset-0", "{label}" } },
					(None, Go::Scope(path)) => rsx! {
						button {
							r#type: "button",
							class: "min-w-0 flex-1 cursor-pointer truncate text-left text-body after:absolute after:inset-0",
							title: "Start from {path}",
							onclick: { let path = path.clone(); move |_| ctx.start_from(Some(path.clone())) },
							"{label}"
						}
					},
					(None, go) => panic!("{} leads nowhere", key_of(go)),
				}
				if starred {
					Icon { glyph: Glyph::Star, class: "size-3 fill-accent-warn text-accent-warn group-hover/row:hidden" }
				}
				if own.is_none() && !matches!(go, Go::Panel(ref id) if id.0.starts_with("sprint:")) {
					span { class: "relative group-hover/row:hidden", title: "the team's, read-only: fork to edit", Icon { glyph: Glyph::Lock, class: "size-3 text-ink-faint" } }
				}
				if !meta.is_empty() {
					span { class: "text-mono-s text-ink-faint group-hover/row:hidden", "{meta}" }
				}
				div { class: "relative hidden items-center group-hover/row:flex",
					if let Some(what) = favorite.clone() {
						IconBtn { glyph: Glyph::Star, title: if starred { "Unfavorite" } else { "Favorite" }, active: starred, onclick: move |_| { let what = what.clone(); spawn(async move { _ = ctx.keep(LibraryEdit::Favorite { what, on: !starred }).await; }); } }
					}
					match own {
						Some(shelf) => rsx! {
							if shelf == Shelf::Views {
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

/// The open workspace's name and menu; on a phone the shelves and the palette beside it.
#[component]
pub(super) fn TopBar() -> Element {
	let ctx = use_context::<Ctx>();
	if ctx.library.read().is_none() {
		return rsx! { div { class: "h-(--size-topbar) shrink-0 border-b border-border-subtle bg-muted" } };
	}
	let band = ctx.band();
	let mut sheet = ctx.sheet;
	if band == Some(Band::Sm) {
		return rsx! {
			div { class: "flex h-12 shrink-0 items-center gap-2 border-b border-border-subtle bg-muted px-2",
				IconBtn { glyph: Glyph::Menu, title: "Shelves", onclick: move |_| sheet.set(true) }
				workspace::WorkspaceSelect {}
				div { class: "flex-1" }
				IconBtn { glyph: Glyph::Search, title: "Jump to…", onclick: move |_| { let mut p = ctx.palette; p.set(true); } }
				if let Some(me) = ctx.me() { Avatar { login: me } }
			}
		};
	}
	rsx! {
		div { class: "flex h-(--size-topbar) shrink-0 items-center gap-2 border-b border-border-subtle bg-muted px-2",
			button { r#type: "button", class: "flex size-7 items-center justify-center text-ink-soft md:hidden", aria_label: "Shelves", onclick: move |_| sheet.set(true), Icon { glyph: Glyph::Menu } }
			workspace::WorkspaceSelect {}
			div { class: "flex-1" }
			if let Some(band) = band {
				span { class: "hidden h-5 items-center rounded-[var(--control-radius)] border border-border-subtle px-1.5 text-mono-s text-ink-soft md:inline-flex", title: "the arrangement live for this window's width", "{band}" }
			}
			IconBtn { glyph: Glyph::Share, title: "Copy a link to this workspace", onclick: move |_| workspace::share_screen(ctx) }
			IconBtn { glyph: Glyph::Help, title: "Keys and help", onclick: move |_| { let mut m = ctx.modal; m.set(Some(Modal::Help)); } }
		}
	}
}

/// On a phone, the thumb zone: the band the workspace is arranged for, and the shown view's fields and grouping.
#[component]
pub(super) fn ThumbBar() -> Element {
	let ctx = use_context::<Ctx>();
	if ctx.band() != Some(Band::Sm) {
		return rsx! {};
	}
	let name = ctx.current_view();
	rsx! {
		div { class: "flex h-14 shrink-0 items-center justify-end gap-2 border-t border-border-subtle bg-muted px-3",
			span { class: "inline-flex h-5 items-center rounded-[var(--control-radius)] border border-border-subtle px-1.5 text-mono-s text-ink-soft", "sm" }
			if let Some(name) = name {
				ThumbView { key: "{name}", name }
			}
		}
	}
}

#[component]
fn ThumbView(name: String) -> Element {
	const BUTTON: &str = "inline-flex h-7 items-center gap-1.5 rounded-[var(--control-radius)] border border-border-strong px-3 text-body font-medium text-ink";
	const MENU: &str = "top-auto bottom-full mb-1 flex max-h-96 w-72 flex-col gap-0.5 overflow-y-auto rounded-[var(--radius)] border border-border bg-popover p-1 text-ink";
	let ctx = use_context::<Ctx>();
	let mut fields = use_signal(|| false);
	let mut grouping = use_signal(|| false);
	let edit = use_callback({
		let name = name.clone();
		move |part| ctx.edit_view(&name, part)
	});
	let defined = ctx.views().into_iter().find(|v| v.name == name);
	let (Some(view), Some(defined)) = (ctx.view(&name), defined) else {
		return rsx! {}; // dropped
	};
	let groups = outcome(&ctx, &view.group_by).and_then(Result::ok); // why it doesn't run is the tile's to say
	let cards = groups.map(|g| format!("{} cards", count(&g, &view.boxes.hidden))).unwrap_or_default();
	rsx! {
		Popover { open: fields(), on_open_change: move |v| fields.set(v),
			PopoverTrigger { class: BUTTON, Icon { glyph: Glyph::Fields } "Fields" }
			PopoverContent { align: "end", class: MENU,
				FieldsMenu { shown: view.shown.clone(), edit }
				div { class: "my-0.5 h-px bg-border-subtle" }
				div { class: "flex flex-wrap items-center gap-1 px-1",
					span { class: "flex-1 text-mono-s text-ink-soft", "{cards}" }
					Keep { view: view.clone(), defined }
				}
			}
		}
		Popover { open: grouping(), on_open_change: move |v| grouping.set(v),
			PopoverTrigger { class: BUTTON, Icon { glyph: Glyph::Group } "Group" }
			PopoverContent { align: "end", class: MENU, GroupMenu { by: view.group_by, edit } }
		}
	}
}
