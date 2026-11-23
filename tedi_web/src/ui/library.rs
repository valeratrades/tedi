//! The Library (Figma 03 · Library browser): every object the viewer can put on the screen — the team's, their
//! own, their favorites, those shared with them — as cards, one open beside them with its definition, history
//! and forks. Moving one anywhere is a copy: Open onto the screen, Fork into the library, Propose to the team.

use dioxus::prelude::*;
use ev_lib::uikit_dioxus::*;
use tedi_core::board::View;
use tilers_dioxus::{PanelId, model::serial};

use super::{
	Ctx, Modal,
	card::{Chip, Tone, ago},
	icons::{Glyph, Icon},
	kit::{Btn, IconBtn, MenuItem as Row, Picker, Style},
	toast::Tone as Said,
	workspace,
};
use crate::api::{self, Catalog, Construction, Dock, Entry, Item, LibraryEdit, Onto, Origin, Owner, ShareLink, Shelf, band_name, constructions, diff};

/// A name typed in place: Enter takes it, Escape or leaving the field drops it.
#[component]
pub fn NameForm(#[props(default)] initial: String, placeholder: String, on_name: EventHandler<String>, on_cancel: EventHandler<()>) -> Element {
	let mut typed = use_signal(|| initial.clone());
	rsx! {
		form {
			onsubmit: move |e: FormEvent| {
				e.prevent_default();
				let name = typed.read().trim().to_string();
				if name.is_empty() {
					on_cancel.call(());
				} else {
					on_name.call(name);
				}
			},
			input {
				class: "h-7 w-full min-w-0 rounded-md border border-border bg-transparent px-2 text-sm outline-none focus:border-ink-soft",
				value: typed(),
				placeholder,
				onmounted: move |e| async move {
					_ = e.set_focus(true).await; // focus is a nicety: a field gone before it lands needs none
				},
				oninput: move |e| typed.set(e.value()),
				onkeydown: move |e: KeyboardEvent| {
					if e.key() == Key::Escape {
						on_cancel.call(());
					}
				},
				onblur: move |_| on_cancel.call(()),
			}
		}
	}
}

#[derive(Clone, Copy, Debug, PartialEq)]
enum Tab {
	Team,
	Mine,
	Favorites,
	Shared,
}

#[derive(Clone, Debug, PartialEq)]
enum Pick {
	Obj(Owner, Item),
	Link(ShareLink),
}

/// What a panel id after `library:` asks for: `<kind>`, `<kind>:<name>`, `link:<share link>`, or nothing.
fn focus(at: &str) -> Result<(Option<Shelf>, Option<String>), String> {
	if let Some(link) = at.strip_prefix("link:") {
		let link: ShareLink = link.parse()?;
		return Ok((Some(link.item.shelf()), None));
	}
	let (kind, name) = match at.split_once(':') {
		Some((kind, name)) => (kind, Some(name.to_string())),
		None => (at, None),
	};
	let shelf = match kind {
		"" => None,
		"views" | "view" => Some(Shelf::Filters),
		"scopes" | "scope" => Some(Shelf::Scopes),
		"workspaces" | "workspace" => Some(Shelf::Workspaces),
		"columns" => Some(Shelf::Columns),
		other => return Err(format!("{other} is no kind the library holds")),
	};
	Ok((shelf, name))
}

fn kind_name(shelf: Shelf) -> &'static str {
	match shelf {
		Shelf::Filters => "View",
		Shelf::Scopes => "Scope",
		Shelf::Workspaces => "Workspace",
		Shelf::Columns => "Column layout",
	}
}

fn kind_glyph(shelf: Shelf) -> Glyph {
	match shelf {
		Shelf::Filters => Glyph::View,
		Shelf::Scopes => Glyph::Scope,
		Shelf::Workspaces => Glyph::Workspace,
		Shelf::Columns => Glyph::Columns,
	}
}

/// Figma: KindTag. A fixed hue per kind, so a mixed list stays scannable.
#[component]
fn KindTag(shelf: Shelf) -> Element {
	let tone = match shelf {
		Shelf::Filters => Tone::Info,
		Shelf::Scopes => Tone::Positive,
		Shelf::Workspaces => Tone::Purple,
		Shelf::Columns => Tone::Pink,
	};
	rsx! {
		Chip { tone,
			Icon { glyph: kind_glyph(shelf), class: "size-3" }
			"{kind_name(shelf)}"
		}
	}
}

/// Figma: OwnerBadge. Team = config, read-only here; mine = the viewer's; fork = mine, remembering its upstream.
#[component]
fn OwnerBadge(owner: Owner, forked: bool) -> Element {
	let (glyph, label, ink) = match (&owner, forked) {
		(Owner::Team, _) => (Glyph::Users, "team".to_string(), "text-primary-ink"),
		(Owner::Viewer(_), true) => (Glyph::Fork, "fork".to_string(), "text-accent-info-ink"),
		(Owner::Viewer(v), false) if v.is_empty() => (Glyph::User, "mine".to_string(), "text-positive-ink"),
		(Owner::Viewer(v), false) => (Glyph::User, format!("@{v}"), "text-ink-mid"),
	};
	rsx! {
		span { class: "inline-flex items-center gap-1 text-caption {ink}",
			Icon { glyph, class: "size-3" }
			"{label}"
		}
	}
}

/// A live picture of what it constructs: a view's columns, a scope's repo tree, a workspace's tiles, a column layout's tints.
#[component]
fn Thumb(construction: Construction, #[props(default)] large: bool) -> Element {
	const HUES: [&str; 5] = ["var(--ink-faint)", "var(--tint-yellow)", "var(--tint-blue)", "var(--tint-green)", "var(--tint-purple)"];
	let ctx = use_context::<Ctx>();
	let height = if large { "h-36" } else { "h-24" };
	let body = match &construction {
		Construction::View(view) => {
			let columns: Vec<String> = match &view.group_by {
				tedi_core::board::GroupBy::State(w) => ctx.workflows().get(w).map(|w| w.leaves().iter().map(|s| s.names().last().cloned().expect("a state names its leaf")).collect()).unwrap_or_default(), // a workflow this board lacks: drawn uncoloured
				_ => Vec::new(),
			};
			let tints = ctx.library.read().as_ref().and_then(|l| l.on_screen.tints.get(&view.name).and_then(|t| t.get(&view.group_by)).cloned()).unwrap_or_default(); // never tinted
			let hues: Vec<String> = (0..5)
				.map(|i| match columns.get(i).and_then(|c| tints.get(c)) {
					Some(t) => format!("var(--tint-{t})"),
					None => HUES[i].to_string(),
				})
				.collect();
			let board = view.layout == tedi_core::board::Layout::Board;
			rsx! {
				div { class: "grid h-full grid-cols-5 gap-1",
					for (i , hue) in hues.into_iter().enumerate() {
						div { key: "{i}", class: "flex flex-col gap-1",
							div { class: "h-1 rounded-full", style: "background: {hue}" }
							for row in 0..(if board { 4 - i % 3 } else { 5 }) {
								div { key: "{row}", class: "h-2.5 rounded-sm bg-secondary" }
							}
						}
					}
				}
			}
		}
		Construction::Scope(scope) => {
			let mut lines = Vec::new();
			fn walk(scope: &tedi_core::board::Scope, depth: usize, lines: &mut Vec<(usize, String, bool)>) {
				for repo in &scope.repos {
					lines.push((depth, repo.clone(), false));
				}
				for (name, child) in &scope.children {
					lines.push((depth, name.clone(), true));
					walk(child, depth + 1, lines);
				}
			}
			walk(scope, 0, &mut lines);
			rsx! {
				div { class: "flex flex-col gap-0.5 overflow-hidden",
					for (i , (depth , text , folder)) in lines.into_iter().take(7).enumerate() {
						div { key: "{i}", class: "flex items-center gap-1 truncate text-mono-s text-ink-soft", style: "padding-left: {depth * 10}px",
							Icon { glyph: if folder { Glyph::Scope } else { Glyph::ChevronRight }, class: "size-3 shrink-0" }
							"{text}"
						}
					}
				}
			}
		}
		Construction::Workspace(tiles) => {
			let layout = tiles.get(&tilers_dioxus::Band::Xl).or_else(|| tiles.values().next());
			let cells = layout.and_then(|l| serial::load(l).ok()).map(|g| g.cells).unwrap_or_default(); // an unreadable layout draws empty: Open says why
			let (cols, rows) = (cells.iter().map(|c| c.x + c.w).max().unwrap_or(1).max(1), cells.iter().map(|c| c.y + c.h).max().unwrap_or(1).max(1));
			rsx! {
				div { class: "relative h-full w-full",
					for (i , c) in cells.into_iter().enumerate() {
						div {
							key: "{i}",
							class: "absolute rounded-sm border border-border bg-secondary",
							style: "left: {c.x * 100 / cols}%; top: {c.y * 100 / rows}%; width: calc({c.w * 100 / cols}% - 3px); height: calc({c.h * 100 / rows}% - 3px)",
						}
					}
				}
			}
		}
		Construction::Columns(columns) => rsx! {
			div { class: "flex h-full gap-1",
				for (group , tint) in columns.tints.iter() {
					div { key: "{group}", class: "flex flex-1 flex-col gap-1",
						div { class: "h-full rounded-sm", style: "background: color-mix(in srgb, var(--tint-{tint}) 40%, var(--card))" }
						span { class: "truncate text-mono-s text-ink-soft", "{group}" }
					}
				}
			}
		},
	};
	rsx! {
		div { class: "{height} overflow-hidden rounded-[var(--radius)] bg-muted p-2", {body} }
	}
}

/// What the screen holds of the library: the team's, then the viewer's own.
fn objects(ctx: &Ctx) -> Vec<(Owner, Item, Construction)> {
	let setup = ctx.setup.read();
	let library = ctx.library.read();
	let (Some(setup), Some(library)) = (setup.as_ref(), library.as_ref()) else {
		return Vec::new();
	};
	let team = constructions(&setup.views, &setup.scopes, &setup.columns, &setup.workspaces).into_iter().map(|(i, c)| (Owner::Team, i, c));
	let filters: Vec<View> = library.filters.values().cloned().collect();
	let mine = constructions(&filters, &library.scopes, &library.columns, &library.workspaces).into_iter().map(|(i, c)| (Owner::Viewer(String::new()), i, c));
	team.chain(mine).collect()
}

fn entry<'a>(catalog: Option<&'a Catalog>, owner: &Owner, item: &Item) -> Option<&'a Entry> {
	catalog?.entries.iter().find(|e| &e.item == item && (e.owner == Owner::Team) == (*owner == Owner::Team))
}

/// Figma 03: tabs, kind filter, search, sort over cards; the picked one in detail.
#[component]
pub(super) fn LibraryPanel(at: String) -> Element {
	let ctx = use_context::<Ctx>();
	let mut tab = use_signal(|| Tab::Team);
	let mut kind = use_signal(|| None::<Shelf>);
	let mut search = use_signal(String::new);
	let mut by_name = use_signal(|| false);
	let mut picked = use_signal(|| None::<Pick>);
	let focused = focus(&at);
	use_effect(use_reactive!(|at| {
		let Ok((shelf, name)) = focus(&at) else { return };
		kind.set(shelf);
		if let Some(link) = at.strip_prefix("link:").and_then(|l| l.parse::<ShareLink>().ok()) {
			tab.set(Tab::Shared);
			picked.set(Some(Pick::Link(link)));
		} else if let (Some(shelf), Some(name)) = (shelf, name) {
			let item = Item::new(shelf, name);
			let team = objects(&ctx).into_iter().any(|(o, i, _)| o == Owner::Team && i == item);
			tab.set(if team { Tab::Team } else { Tab::Mine });
			picked.set(Some(Pick::Obj(if team { Owner::Team } else { Owner::Viewer(String::new()) }, item)));
		}
	}));
	let catalog = use_resource(move || {
		let _ = ctx.library.read(); // a fork, a save or a share moves the counts
		async move { api::catalog().await }
	});
	if let Err(why) = focused {
		return rsx! { div { class: "p-4 text-body text-ink-soft", "{why}" } };
	}
	let catalog_read = catalog.read();
	let catalog = match &*catalog_read {
		Some(Ok(c)) => Some(c.clone()),
		Some(Err(e)) => {
			let e = e.to_string();
			return rsx! { div { class: "p-4 text-body text-accent-error", "The library's counts didn't load: {e}" } };
		}
		None => None,
	};
	drop(catalog_read);
	let library = ctx.library.read().clone().expect("panels render once the library arrived");
	let all = objects(&ctx);
	let received = library.received.clone();
	let in_tab = |t: Tab, owner: &Owner, item: &Item| match t {
		Tab::Team => *owner == Owner::Team,
		Tab::Mine => *owner != Owner::Team,
		Tab::Favorites => library.favorites.contains(item),
		Tab::Shared => false,
	};
	let count = |t: Tab| match t {
		Tab::Shared => received.len(),
		t => all.iter().filter(|(o, i, _)| in_tab(t, o, i)).count(),
	};
	let needle = search.read().to_lowercase();
	let mut shown: Vec<(Owner, Item, Construction)> = all
		.iter()
		.filter(|(o, i, _)| in_tab(tab(), o, i) && kind().is_none_or(|k| i.shelf() == k) && i.name().to_lowercase().contains(&needle))
		.cloned()
		.collect();
	let viewers = |o: &Owner, i: &Item| entry(catalog.as_ref(), o, i).map_or(0, |e| e.usage.viewers);
	match by_name() {
		true => shown.sort_by(|a, b| a.1.name().cmp(b.1.name())),
		false => shown.sort_by_key(|(o, i, _)| std::cmp::Reverse(viewers(o, i))),
	}
	let links: Vec<ShareLink> = received.iter().filter(|l| kind().is_none_or(|k| l.item.shelf() == k) && l.item.name().to_lowercase().contains(&needle)).cloned().collect();
	let kinds_in_tab = |k: Shelf| all.iter().filter(|(o, i, _)| in_tab(tab(), o, i) && i.shelf() == k).count();
	rsx! {
		div { class: "flex h-full min-h-0",
			div { class: "flex min-w-0 flex-1 flex-col gap-3 overflow-y-auto p-4",
				div { class: "flex flex-wrap items-center gap-3",
					h1 { class: "text-[22px] font-semibold", "Library" }
					div { class: "flex items-center gap-0.5 rounded-[var(--control-radius)] bg-secondary p-0.5",
						for (t , glyph , label) in [(Tab::Team, Glyph::Users, "Team"), (Tab::Mine, Glyph::User, "Mine"), (Tab::Favorites, Glyph::Star, "Favorites"), (Tab::Shared, Glyph::Share, "Shared with me")] {
							button {
								key: "{label}",
								r#type: "button",
								class: if tab() == t { "flex h-6 items-center gap-1 rounded-[var(--control-radius)] bg-card px-2 text-body-s text-ink" } else { "flex h-6 items-center gap-1 rounded-[var(--control-radius)] px-2 text-body-s text-ink-mid hover:text-ink" },
								onclick: move |_| tab.set(t),
								Icon { glyph, class: "size-3.5" }
								"{label} · {count(t)}"
							}
						}
					}
				}
				div { class: "flex flex-wrap items-center gap-2",
					div { class: "flex h-7 min-w-48 flex-1 items-center gap-1.5 rounded-[var(--control-radius)] border border-border px-2",
						Icon { glyph: Glyph::Search, class: "size-3.5 text-ink-soft" }
						input {
							class: "min-w-0 flex-1 bg-transparent text-body outline-none",
							placeholder: "search by name",
							value: search(),
							oninput: move |e| search.set(e.value()),
						}
					}
					for k in [Shelf::Filters, Shelf::Scopes, Shelf::Workspaces, Shelf::Columns] {
						button {
							key: "{k:?}",
							r#type: "button",
							class: if kind() == Some(k) { "h-7 rounded-[var(--control-radius)] border border-ring bg-selected px-2 text-body-s text-ink" } else { "h-7 rounded-[var(--control-radius)] border border-border px-2 text-body-s text-ink-mid hover:bg-hover" },
							onclick: move |_| kind.set(if kind() == Some(k) { None } else { Some(k) }),
							"{kind_name(k)}s {kinds_in_tab(k)}"
						}
					}
					Picker { glyph: Glyph::Sort, label: if by_name() { "Name" } else { "Most used" },
						Row { selected: !by_name(), label: "Most used", onclick: move |_| by_name.set(false) }
						Row { selected: by_name(), label: "Name", onclick: move |_| by_name.set(true) }
					}
				}
				if tab() == Tab::Shared {
					if links.is_empty() {
						span { class: "text-body text-ink-faint", "Nothing shared with you yet: a link someone sends lands here once opened." }
					}
					div { class: "grid grid-cols-[repeat(auto-fill,minmax(14rem,1fr))] gap-3",
						for link in links {
							LinkCard { key: "{link}", link: link.clone(), picked: picked() == Some(Pick::Link(link.clone())), on_pick: move |l| picked.set(Some(Pick::Link(l))) }
						}
					}
				} else {
					if shown.is_empty() {
						span { class: "text-body text-ink-faint", "Nothing here matches." }
					}
					div { class: "grid grid-cols-[repeat(auto-fill,minmax(14rem,1fr))] gap-3",
						for (owner , item , construction) in shown {
							LibraryCard {
								key: "{owner}-{item:?}",
								picked: picked() == Some(Pick::Obj(owner.clone(), item.clone())),
								entry: entry(catalog.as_ref(), &owner, &item).cloned(),
								owner,
								item,
								construction,
								on_pick: move |(o, i)| picked.set(Some(Pick::Obj(o, i))),
							}
						}
					}
				}
			}
			match picked() {
				Some(Pick::Obj(owner, item)) => match all.iter().find(|(o, i, _)| *o == owner && *i == item) {
					Some((_, _, construction)) => rsx! {
						Detail { key: "{owner}-{item:?}", entry: entry(catalog.as_ref(), &owner, &item).cloned(), owner, item, construction: construction.clone(), on_pick: move |(o, i)| { tab.set(if o == Owner::Team { Tab::Team } else { Tab::Mine }); picked.set(Some(Pick::Obj(o, i))); } }
					},
					None => rsx! {},
				},
				Some(Pick::Link(link)) => rsx! { SharedDetail { key: "{link}", link, on_pick: move |(o, i)| { tab.set(Tab::Mine); picked.set(Some(Pick::Obj(o, i))); } } },
				None => rsx! {},
			}
		}
	}
}

/// Figma: LibraryCard. Hovered: Open, Fork, link, ★.
#[component]
fn LibraryCard(owner: Owner, item: Item, construction: Construction, entry: Option<Entry>, picked: bool, on_pick: EventHandler<(Owner, Item)>) -> Element {
	let ctx = use_context::<Ctx>();
	let starred = ctx.favorite(&item);
	let forked = ctx.library.read().as_ref().is_some_and(|l| l.forks.iter().any(|f| f.item == item)) && owner != Owner::Team;
	let meta = match &entry {
		Some(e) if owner == Owner::Team && !e.history.is_empty() => {
			let v = e.version.map_or("unpublished".to_string(), |v| format!("v{v} · {}", ago(e.history[v as usize - 1].at)));
			format!("used by {} · {v}", e.usage.viewers)
		}
		Some(e) => format!("used by {}", e.usage.viewers),
		None => String::new(),
	};
	let frame = if picked { "border-ring" } else { "border-border hover:border-border-strong" };
	rsx! {
		div {
			class: "group/card flex cursor-pointer flex-col gap-2 rounded-[var(--radius)] border bg-card p-2 {frame}",
			onclick: { let (o, i) = (owner.clone(), item.clone()); move |_| on_pick.call((o.clone(), i.clone())) },
			Thumb { construction: construction.clone() }
			div { class: "flex items-center gap-2",
				KindTag { shelf: item.shelf() }
				div { class: "flex-1" }
				if starred {
					Icon { glyph: Glyph::Star, class: "size-3.5 fill-accent-warn text-accent-warn" }
				}
			}
			span { class: "truncate text-body font-semibold", "{item.name()}" }
			div { class: "flex items-center gap-2",
				OwnerBadge { owner: owner.clone(), forked }
				span { class: "ml-auto truncate text-mono-s text-ink-faint", "{meta}" }
			}
			div { class: "hidden items-center gap-1 group-hover/card:flex",
				Btn { style: Style::Primary, onclick: { let (o, i, c) = (owner.clone(), item.clone(), construction.clone()); move |e: MouseEvent| { e.stop_propagation(); open(ctx, &o, &i, &c); } }, "Open" }
				if owner == Owner::Team {
					Btn { style: Style::Outline, glyph: Glyph::Fork, onclick: { let i = item.clone(); move |e: MouseEvent| { e.stop_propagation(); fork(ctx, Origin::Team(i.clone()), i.name()); } }, "Fork" }
				}
				div { class: "flex-1" }
				IconBtn { glyph: Glyph::Link, title: "Copy a share link", onclick: { let (i, c) = (item.clone(), construction.clone()); move |_| share(ctx, i.clone(), &c) } }
				IconBtn { glyph: Glyph::Star, title: if starred { "Unfavorite" } else { "Favorite" }, active: starred, onclick: { let i = item.clone(); move |_| { let what = i.clone(); spawn(async move { _ = ctx.keep(LibraryEdit::Favorite { what, on: !starred }).await; }); } } }
			}
		}
	}
}

#[component]
fn LinkCard(link: ShareLink, picked: bool, on_pick: EventHandler<ShareLink>) -> Element {
	let frame = if picked { "border-ring" } else { "border-border hover:border-border-strong" };
	rsx! {
		div {
			class: "flex cursor-pointer flex-col gap-2 rounded-[var(--radius)] border bg-card p-3 {frame}",
			onclick: { let l = link.clone(); move |_| on_pick.call(l.clone()) },
			KindTag { shelf: link.item.shelf() }
			span { class: "truncate text-body font-semibold", "{link.item.name()}" }
			div { class: "flex items-center gap-2",
				OwnerBadge { owner: link.owner.clone(), forked: false }
				span { class: "ml-auto text-mono-s text-ink-faint", "v{link.version}" }
			}
		}
	}
}

/// Copies `construction` onto the screen.
fn open(ctx: Ctx, owner: &Owner, item: &Item, construction: &Construction) {
	match (item, construction) {
		(Item::View(name), _) => ctx.open(PanelId(format!("view:{name}"))),
		(Item::Workspace(name), _) => ctx.switch(name),
		(Item::Scope(path), _) => {
			let path = path.clone();
			spawn(async move { _ = ctx.keep(LibraryEdit::Scope(Some(path))).await });
		}
		(Item::Columns(name), Construction::Columns(columns)) => {
			let onto: Vec<String> = ctx.views().into_iter().filter(|v| v.group_by == columns.by).map(|v| v.name).collect();
			match onto.as_slice() {
				[] => ctx.say("library", Said::Info, format!("no view groups by {}: {name} colours none", columns.by), None),
				_ => {
					let mut m = ctx.modal;
					m.set(Some(Modal::ApplyColumns { name: name.clone(), owner: owner.clone() }));
				}
			}
		}
		(item, construction) => panic!("{item:?} holds no {construction:?}"),
	}
}

/// `columns` onto `view`'s columns: its tints, and its arrangement for every band it keeps one for.
pub(super) fn apply_columns(ctx: Ctx, view: String, columns: api::Columns) {
	spawn(async move {
		let by = columns.by.clone();
		if !ctx.keep(LibraryEdit::Tinted { view: view.clone(), by: by.clone(), tints: columns.tints.clone() }).await {
			return;
		}
		for (band, layout) in columns.layout {
			if !ctx.keep(LibraryEdit::Arranged { dock: Dock::Columns { view: view.clone(), by: by.clone() }, band, layout }).await {
				return;
			}
		}
		ctx.open(PanelId(format!("view:{view}")));
	});
}

fn fork(ctx: Ctx, from: Origin, name: &str) {
	let name = format!("{name} (mine)");
	spawn(async move {
		if ctx.keep(LibraryEdit::Fork { from, name: name.clone() }).await {
			ctx.say("library", Said::Success, format!("Forked into your library as {name}"), None);
		}
	});
}

/// A pinned link to `item` as it stands, onto the clipboard; a workspace's carries every band it arranged.
fn share(ctx: Ctx, item: Item, construction: &Construction) {
	let bands = match construction {
		Construction::Workspace(tiles) => tiles.keys().copied().collect(),
		_ => Vec::new(),
	};
	let scope = ctx.library.read().as_ref().and_then(|l| l.scope.clone());
	spawn(async move {
		match api::share(item, bands, scope).await {
			Ok(link) => workspace::copy_link(ctx, &link),
			Err(e) => ctx.say("share", Said::Error, e.to_string(), None),
		}
	});
}

/// The picked object: preview, definition, counts, its moves, its link, history and forks.
#[component]
fn Detail(owner: Owner, item: Item, construction: Construction, entry: Option<Entry>, on_pick: EventHandler<(Owner, Item)>) -> Element {
	let ctx = use_context::<Ctx>();
	let mut comparing = use_signal(|| None::<String>);
	let library = ctx.library.read().clone().expect("panels render once the library arrived");
	let upstream = (owner != Owner::Team).then(|| library.forks.iter().find(|f| f.item == item).map(|f| f.upstream.clone())).flatten();
	let team = objects(&ctx);
	let theirs = |name: &str| team.iter().find(|(o, i, _)| *o == Owner::Team && i.shelf() == item.shelf() && i.name() == name).map(|(_, _, c)| c.clone());
	let meta = match (&owner, &entry, &upstream) {
		(Owner::Team, Some(e), _) if !e.history.is_empty() => match e.version {
			Some(v) => {
				let h = &e.history[v as usize - 1];
				format!("v{v} · published {} by @{} · from board.toml", ago(h.at), h.author)
			}
			None => "unpublished: no commit of board.toml holds it as the board runs it".to_string(),
		},
		(Owner::Team, ..) => "the team's config: read-only here, fork it to edit".to_string(),
		(_, e, Some(up)) => {
			let v = up.version.map_or(String::new(), |v| format!("@v{v}")); // the team's config has no history to number it by
			let behind = e.as_ref().and_then(|e| e.behind).map_or(String::new(), |b| format!(" · {b} behind")); // its upstream has no history
			format!("fork of {}/{}{v} · {} ahead{behind}", up.owner, up.name, up.ahead)
		}
		(_, _, None) => "yours".to_string(),
	};
	let definition: Vec<(&str, String)> = match &construction {
		Construction::View(v) => vec![
			("query", v.query.clone()),
			("layout", v.layout.to_string()),
			("group_by", v.group_by.to_string()),
			("sort", v.sort.to_string()),
			("fields", v.shown.iter().cloned().map(String::from).collect::<Vec<_>>().join(" ")),
		],
		Construction::Scope(s) => vec![("repos", s.repos.join(" ")), ("nested", s.children.keys().cloned().collect::<Vec<_>>().join(" "))],
		Construction::Workspace(tiles) => vec![("arranged for", tiles.keys().map(|b| band_name(*b)).collect::<Vec<_>>().join(" "))],
		Construction::Columns(c) => vec![("by", c.by.to_string()), ("tints", c.tints.iter().map(|(g, t)| format!("{g} {t}")).collect::<Vec<_>>().join(" "))],
	};
	let usage = entry.as_ref().map(|e| e.usage.clone()).unwrap_or_default(); // the counts are still loading
	let behind_only = entry.as_ref().and_then(|e| e.behind).is_some_and(|b| b > 0) && upstream.as_ref().is_some_and(|u| u.ahead == 0);
	rsx! {
		div { class: "flex w-[26rem] shrink-0 flex-col gap-3 overflow-y-auto border-l border-border-subtle p-4",
			Thumb { construction: construction.clone(), large: true }
			div { class: "flex items-center gap-2",
				h2 { class: "truncate text-heading font-semibold", "{item.name()}" }
				KindTag { shelf: item.shelf() }
				OwnerBadge { owner: owner.clone(), forked: upstream.is_some() }
			}
			if !meta.is_empty() {
				span { class: "text-caption text-ink-soft", "{meta}" }
			}
			div { class: "flex flex-col gap-1 rounded-[var(--radius)] bg-muted p-2",
				for (key , value) in definition {
					div { key: "{key}", class: "flex gap-3 text-mono-s",
						span { class: "w-20 shrink-0 text-ink-faint", "{key}" }
						span { class: "min-w-0 break-words text-ink", "{value}" }
					}
				}
			}
			div { class: "flex gap-6",
				for (n , label) in [(usage.viewers, "viewers"), (usage.forks, "forks"), (usage.favorites, "★")] {
					div { key: "{label}", class: "flex flex-col",
						span { class: "text-heading", "{n}" }
						span { class: "text-caption text-ink-soft", "{label}" }
					}
				}
			}
			div { class: "flex flex-wrap items-center gap-2",
				Btn { style: Style::Primary, md: true, onclick: { let (o, i, c) = (owner.clone(), item.clone(), construction.clone()); move |_| open(ctx, &o, &i, &c) }, "Open" }
				if owner == Owner::Team {
					Btn { style: Style::Outline, md: true, glyph: Glyph::Fork, onclick: { let i = item.clone(); move |_| fork(ctx, Origin::Team(i.clone()), i.name()) }, "Fork to mine" }
				} else {
					Btn { style: Style::Ghost, md: true, glyph: Glyph::Publish, onclick: { let i = item.clone(); move |_| { let mut m = ctx.modal; m.set(Some(Modal::Propose(i.clone()))); } }, "Propose change" }
				}
				if behind_only {
					Btn { style: Style::Outline, md: true, glyph: Glyph::Sync, onclick: { let i = item.clone(); move |_| { let i = i.clone(); spawn(async move { _ = ctx.keep(LibraryEdit::Pull(i)).await; }); } }, "Pull" }
				}
			}
			Btn { style: Style::Outline, glyph: Glyph::Link, onclick: { let (i, c) = (item.clone(), construction.clone()); move |_| share(ctx, i.clone(), &c) }, "Copy share link" }
			if let Some(e) = &entry {
				if !e.history.is_empty() {
					span { class: "flex items-center gap-1 text-label text-ink-faint", Icon { glyph: Glyph::History, class: "size-3" } "HISTORY" }
					for h in e.history.iter().rev() {
						div { key: "{h.v}", class: "flex items-center gap-2 text-body-s",
							span { class: "text-mono-s text-primary-ink", "v{h.v}" }
							span { class: "min-w-0 flex-1 truncate",
								match h.changes.first() {
									Some(c) => format!("{} {}", c.sign, c.text),
									None => "created".to_string(),
								}
							}
							span { class: "text-mono-s text-ink-faint", "@{h.author} · {ago(h.at)}" }
						}
					}
				}
				if !e.forks.is_empty() {
					span { class: "text-label text-ink-faint", "FORKS" }
					for f in e.forks.iter() {
						div { key: "{f.viewer}-{f.name}", class: "flex items-center gap-2 text-body-s",
							Icon { glyph: Glyph::Fork, class: "size-3.5 text-ink-soft" }
							span { class: "min-w-0 flex-1 truncate", "{f.name} ", span { class: "text-caption text-ink-faint", "@{f.viewer}" } }
							if f.ahead > 0 {
								span { class: "text-mono-s text-positive-ink", "{f.ahead} ahead" }
							}
							if let Some(b) = f.behind.filter(|b| *b > 0) {
								span { class: "text-mono-s text-accent-warn", "{b} behind" }
							}
							if library.forks.iter().any(|m| m.item.name() == f.name && m.item.shelf() == item.shelf()) && owner == Owner::Team {
								button {
									r#type: "button",
									class: "text-body-s text-ink-mid hover:text-ink",
									onclick: { let n = f.name.clone(); move |_| comparing.set(if comparing() == Some(n.clone()) { None } else { Some(n.clone()) }) },
									"Compare"
								}
								button {
									r#type: "button",
									class: "text-body-s text-ink-mid hover:text-ink",
									onclick: { let n = f.name.clone(); let shelf = item.shelf(); move |_| on_pick.call((Owner::Viewer(String::new()), Item::new(shelf, n.clone()))) },
									"Show"
								}
							}
						}
					}
				}
			}
			if let (Some(name), Owner::Team) = (comparing(), &owner) {
				if let Some(Construction::View(_) | Construction::Scope(_) | Construction::Workspace(_) | Construction::Columns(_)) = Some(&construction) {
					{
						let mine = objects(&ctx).into_iter().find(|(o, i, _)| *o != Owner::Team && i.shelf() == item.shelf() && i.name() == name).map(|(_, _, c)| c);
						match mine {
							Some(mine) => rsx! { Changes { from: construction.clone(), to: mine } },
							None => rsx! {},
						}
					}
				}
			}
			if let Some(up) = upstream.filter(|u| u.owner == Owner::Team) {
				if let Some(theirs) = theirs(&up.name) {
					span { class: "text-label text-ink-faint", "AGAINST THE TEAM'S {up.name}" }
					Changes { from: theirs, to: construction.clone() }
				}
			}
		}
	}
}

/// `from` → `to`, line by line.
#[component]
fn Changes(from: Construction, to: Construction) -> Element {
	let lines = diff(&from, &to);
	rsx! {
		div { class: "flex flex-col gap-0.5 rounded-[var(--radius)] bg-muted p-2 text-mono-s",
			if lines.is_empty() {
				span { class: "text-ink-faint", "no difference" }
			}
			for (i , c) in lines.into_iter().enumerate() {
				div {
					key: "{i}",
					class: match c.sign {
						api::Sign::Added => "text-positive-ink",
						api::Sign::Removed => "text-accent-error",
						api::Sign::Changed => "text-accent-warn",
					},
					"{c.sign} {c.text}"
				}
			}
		}
	}
}

/// A link shared with the viewer: read-only until forked.
#[component]
fn SharedDetail(link: ShareLink, on_pick: EventHandler<(Owner, Item)>) -> Element {
	let ctx = use_context::<Ctx>();
	let shared = use_resource({
		let link = link.clone();
		move || {
			let link = link.clone();
			async move { api::shared(link).await }
		}
	});
	let read = shared.read();
	let shared = match &*read {
		None => return rsx! { div { class: "w-[26rem] shrink-0 border-l border-border-subtle p-4 text-body text-ink-soft", "loading {link}…" } },
		Some(Err(e)) => return rsx! { div { class: "w-[26rem] shrink-0 border-l border-border-subtle p-4 text-body text-accent-error", "{e}" } },
		Some(Ok(s)) => s.clone(),
	};
	drop(read);
	let name = link.item.name().to_string();
	rsx! {
		div { class: "flex w-[26rem] shrink-0 flex-col gap-3 overflow-y-auto border-l border-border-subtle p-4",
			Thumb { construction: shared.construction.clone(), large: true }
			div { class: "flex items-center gap-2",
				h2 { class: "truncate text-heading font-semibold", "{name}" }
				KindTag { shelf: link.item.shelf() }
				OwnerBadge { owner: link.owner.clone(), forked: false }
			}
			span { class: "text-caption text-ink-soft", "v{link.version} as {link.owner} shared it · read-only until forked" }
			if !shared.carried.is_empty() {
				span { class: "text-caption text-ink-soft", "brings its views: {shared.carried.keys().cloned().collect::<Vec<_>>().join(\", \")}" }
			}
			div { class: "flex gap-2",
				if matches!(shared.construction, Construction::Workspace(_)) {
					Btn { style: Style::Primary, md: true, onclick: { let s = shared.clone(); move |_| workspace::view_shared(ctx, s.clone()) }, "Open" }
				}
				Btn {
					style: Style::Outline,
					md: true,
					glyph: Glyph::Fork,
					onclick: {
						let (link, name) = (link.clone(), name.clone());
						move |_| {
							let (from, name) = (Origin::Link(link.clone()), name.clone());
							let shelf = link.item.shelf();
							spawn(async move {
								if ctx.keep(LibraryEdit::Fork { from, name: name.clone() }).await {
									on_pick.call((Owner::Viewer(String::new()), Item::new(shelf, name)));
								}
							});
						}
					},
					"Fork to mine"
				}
			}
		}
	}
}

/// Flow 1 step 4: the viewer's `item` as a pull request on the team's config — over the team's object it
/// forked, or as a new one.
#[component]
pub(super) fn Propose(item: Item) -> Element {
	let ctx = use_context::<Ctx>();
	let library = ctx.library.read().clone().expect("proposed from a loaded screen");
	let upstream = library.forks.iter().find(|f| f.item == item && f.upstream.owner == Owner::Team).map(|f| f.upstream.name.clone());
	let mut update = use_signal(|| upstream.is_some());
	let mut name = use_signal(|| item.name().trim_end_matches(" (mine)").to_string());
	let mut message = use_signal(String::new);
	let mut sending = use_signal(|| false);
	let close = move || {
		let mut m = ctx.modal;
		m.set(None);
	};
	let all = objects(&ctx);
	let mine = all.iter().find(|(o, i, _)| *o != Owner::Team && *i == item).map(|(_, _, c)| c.clone()).expect("proposed from the library's own");
	let target = if update() { upstream.clone().expect("update is offered only with an upstream") } else { name() };
	let theirs = all.iter().find(|(o, i, _)| *o == Owner::Team && i.shelf() == item.shelf() && i.name() == target).map(|(_, _, c)| c.clone());
	let taken = !update() && theirs.is_some();
	rsx! {
		Dialog { open: true, on_open_change: move |v: bool| if !v { close() },
			DialogContent { class: "flex flex-col gap-3 sm:max-w-lg",
				DialogHeader { DialogTitle { "Propose to team" } }
				if let Some(up) = upstream.clone() {
					label { class: "flex items-center gap-2 text-body",
						input { r#type: "radio", checked: update(), onchange: move |_| update.set(true) }
						"Update team/{up}"
					}
				}
				label { class: "flex items-center gap-2 text-body",
					input { r#type: "radio", checked: !update(), onchange: move |_| update.set(false) }
					"Add as new"
					if !update() {
						input {
							class: "h-7 min-w-0 flex-1 rounded-md border border-border bg-transparent px-2 text-body outline-none",
							value: name(),
							oninput: move |e| name.set(e.value()),
						}
					}
				}
				if taken {
					span { class: "text-caption text-accent-error", "the team already has a {kind_name(item.shelf()).to_lowercase()} {target}" }
				}
				match &theirs {
					Some(theirs) if update() => rsx! { Changes { from: theirs.clone(), to: mine.clone() } },
					_ => rsx! {},
				}
				Textarea { placeholder: "why: the reviewers read this first", value: message(), rows: 3, oninput: move |e: FormEvent| message.set(e.value()) }
				DialogFooter {
					Btn { style: Style::Ghost, md: true, onclick: move |_| close(), "Cancel" }
					Btn {
						style: Style::Primary,
						md: true,
						disabled: sending() || taken || target.trim().is_empty(),
						onclick: {
							let (item, target) = (item.clone(), target.clone());
							move |_| {
								let onto = if update() { Onto::Update(target.clone()) } else { Onto::New(target.clone()) };
								let (item, text) = (item.clone(), message());
								sending.set(true);
								spawn(async move {
									match api::propose(item, onto, text).await {
										Ok(url) => {
											let pr = url.rsplit_once("/pull/").map_or(url.clone(), |(repo, n)| format!("{}#{n}", repo.rsplit('/').next().expect("rsplit yields one")));
											let view = Callback::new(move |()| {
												if let Err(e) = web_sys::window().expect("a browser window").open_with_url_and_target(&url, "_blank") {
													ctx.say("propose", Said::Error, format!("the browser didn't open {url}: {e:?}"), None);
												}
											});
											ctx.say("propose", Said::Success, format!("Proposal opened · {pr}"), Some(("View", view)));
											close();
										}
										Err(e) => {
											ctx.say("propose", Said::Error, e.to_string(), None);
											sending.set(false);
										}
									}
								});
							}
						},
						"Open config PR"
					}
				}
			}
		}
	}
}

/// A column layout onto one of the views grouping as it does.
#[component]
pub(super) fn ApplyColumns(name: String, owner: Owner) -> Element {
	let ctx = use_context::<Ctx>();
	let item = Item::Columns(name.clone());
	let columns = objects(&ctx).into_iter().find(|(o, i, _)| (*o == Owner::Team) == (owner == Owner::Team) && *i == item).map(|(_, _, c)| c);
	let Some(Construction::Columns(columns)) = columns else {
		return rsx! {};
	};
	let close = move || {
		let mut m = ctx.modal;
		m.set(None);
	};
	let onto: Vec<String> = ctx.views().into_iter().filter(|v| v.group_by == columns.by).map(|v| v.name).collect();
	rsx! {
		Dialog { open: true, on_open_change: move |v: bool| if !v { close() },
			DialogContent { class: "flex flex-col gap-1 sm:max-w-sm",
				DialogHeader { DialogTitle { "Colour a view's columns as {name}" } }
				for view in onto {
					Row {
						key: "{view}",
						selected: false,
						label: view.clone(),
						glyph: Glyph::View,
						onclick: { let (view, columns) = (view.clone(), columns.clone()); move |_| { apply_columns(ctx, view.clone(), columns.clone()); close(); } },
					}
				}
			}
		}
	}
}

