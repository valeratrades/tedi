//! A view tile: a query, laid out as columns, a table or a list, grouped and sorted as the view says.

use std::str::FromStr as _;

use dioxus::prelude::*;
use ev_lib::uikit_dioxus::*;
use strum::IntoEnumIterator as _;
use tedi_core::{
	board::{Facet, GroupBy, Layout, Order, Query, Shown, Sort, SortKey, View, group},
	workflow::State,
};
use tilers_dioxus::{Band, DockPanel, Group, MinSize, PackedApi, PackedArea, PanelId};

pub(super) use super::card::CardView;
use super::{
	Ctx,
	card::Field,
	facets::{FacetMenu, InView},
	icons::{Glyph, Icon},
	kit::{Btn, Style},
	toolbar::ViewToolbar,
};
use crate::api::{self, Dock, LibraryEdit, Shelf, Sprint, Tint, Tints};

/// The view `name`: as last edited on screen, against how it is kept; an edit is saved as it settles, and `↺` goes back.
#[component]
pub fn ViewPanel(name: String) -> Element {
	let ctx = use_context::<Ctx>();
	match ctx.views().into_iter().find(|v| v.name == name) {
		Some(defined) => rsx! { ViewBody { defined } },
		None => rsx! { div { class: "p-3 text-sm text-ink-soft", "The view {name} is no longer kept: close this tab, or keep a view under its name." } },
	}
}

/// A sprint's issues, in its order.
#[component]
pub fn SprintPanel(sprint: Sprint) -> Element {
	let ctx = use_context::<Ctx>();
	let board = ctx.board.read();
	let Some(board) = board.as_ref() else {
		return rsx! { Spinner {} };
	};
	let shown: Vec<Shown> = ctx
		.workflows()
		.into_keys()
		.map(Shown::State)
		.chain([Shown::Repo, Shown::Assignees, Shown::Importance, Shown::Hours])
		.collect();
	rsx! {
		div { class: "flex flex-col gap-1 overflow-auto p-2",
			for link in sprint.links.iter() {
				match board.cards.iter().find(|c| &c.link == link) {
					Some(card) => rsx! { CardView { key: "{link}", card: card.clone(), shown: shown.clone() } },
					None => rsx! { div { key: "{link}", class: "text-xs text-ink-soft", "{link} is not stored here" } },
				}
			}
		}
	}
}

#[component]
fn ViewBody(defined: View) -> Element {
	let ctx = use_context::<Ctx>();
	let view = use_signal(|| {
		let library = ctx.library.peek();
		let on_screen = &library.as_ref().expect("views render once the library arrived").on_screen.views;
		on_screen.get(&defined.name).cloned().unwrap_or_else(|| defined.clone()) // never edited on screen: as kept
	});
	use_context_provider(|| InView(view));
	// a query mid-edit that doesn't parse leaves the board on the last one that did
	let mut good = use_signal(|| view.peek().clone());
	let mut sent = use_signal(|| view.peek().clone());
	let mut pending = use_signal(|| None::<dioxus::core::Task>);
	use_effect(move || {
		let now = view.read().clone();
		if now.query.parse::<Query>().is_ok() && *good.peek() != now {
			good.set(now.clone());
		}
		if *sent.peek() == now {
			return;
		}
		if let Some(task) = pending.take() {
			task.cancel();
		}
		pending.set(Some(spawn(async move {
			gloo_timers::future::sleep(std::time::Duration::from_millis(800)).await;
			sent.set(now.clone());
			ctx.keep(LibraryEdit::Viewed(now)).await;
		})));
	});
	let board = ctx.board.read();
	let Some(board) = board.as_ref() else {
		return rsx! { Spinner {} };
	};
	let v = view.read().clone();
	let error = v.query.parse::<Query>().err().map(|e| e.to_string());
	let ran = if error.is_none() { v.clone() } else { good.read().clone() };
	let within = match ctx.scope() {
		None => Query::default(),
		Some((path, None)) =>
			return rsx! { Empty { glyph: Glyph::Alert, error: true, title: "Scope gone", body: "The scope {path} is no longer configured: pick another in the scope bar." } },
		Some((_, Some(filter))) => filter.parse().expect("a scope's filter is a repo clause"),
	};
	let workflows = ctx.workflows();
	let query: Query = ran.query.parse().expect("the view that ran parsed");
	let me = ctx.me();
	let run = |q: &Query| -> Result<Vec<usize>, String> {
		let mut scoped = q.clone();
		scoped.narrow(&within);
		scoped.run(&board.cards, me.as_deref()).map_err(|e| e.to_string())
	};
	let shown = (|| -> Result<_, String> {
		let mut hits = run(&query)?;
		ran.sort.apply(&board.cards, &mut hits, &workflows).map_err(|e| e.to_string())?;
		let groups = group(&board.cards, &hits, &ran.group_by, &workflows).map_err(|e| e.to_string())?;
		Ok((hits.len(), groups))
	})();
	let (count, mut groups) = match shown {
		Ok(shown) => shown,
		Err(e) => return rsx! { Empty { glyph: Glyph::Alert, error: true, title: "The view doesn't run", body: e } },
	};
	if ran.layout != Layout::Board {
		groups.retain(|(_, members)| !members.is_empty()); // a workflow's empty buckets are the board's columns only
	}
	// what, dropped, would let the most cards back in
	let loosen = (count == 0)
		.then(|| {
			Facet::iter()
				.filter_map(|f| {
					let mut lifted = query.clone();
					lifted.clear(f);
					(lifted != query).then(|| run(&lifted).map(|h| (f, h.len())).ok()).flatten()
				})
				.filter(|(_, n)| *n > 0)
				.max_by_key(|(_, n)| *n)
		})
		.flatten();
	let scope_name = ctx.scope().map(|(path, _)| path);
	rsx! {
		div { class: "flex h-full flex-col",
			ViewToolbar { view, defined, query: query.clone(), error, count }
			if count == 0 && ran.layout != Layout::Board {
				Empty {
					glyph: Glyph::Search,
					title: "No cards match",
					body: match &scope_name { Some(s) => format!("{} in {s}", ran.query), None => ran.query.clone() },
					if let Some((facet, n)) = loosen {
						Btn { style: Style::Outline, title: "{n} cards come back", onclick: move |_| InView(view).narrow(|q| q.clear(facet)), "Drop {facet}:" }
					}
				}
			} else {
				div { class: "min-h-0 flex-1 p-2",
					match ran.layout {
						Layout::Board => rsx! { Columns { key: "{ran.group_by}", view: ran.name.clone(), groups, by: ran.group_by.clone(), shown: ran.shown.clone() } },
						Layout::Table => rsx! { Rows { groups, shown: ran.shown.clone(), sort: ran.sort.clone(), query } },
						Layout::List => rsx! {
							div { class: "flex h-full flex-col gap-1.5 overflow-auto",
								for (key , members) in groups {
									if !key.is_empty() { h3 { class: "mt-2 text-h3 text-ink", "{key}" } }
									for i in members {
										CardView { key: "{board.cards[i].link}", card: board.cards[i].clone(), shown: ran.shown.clone() }
									}
								}
							}
						},
					}
				}
			}
		}
	}
}

/// Says why it's empty and what fills it (Figma: EmptyState).
#[component]
pub(super) fn Empty(glyph: Glyph, #[props(default)] error: bool, title: String, body: String, children: Element) -> Element {
	rsx! {
		div { class: "flex flex-1 items-center justify-center p-6",
			div { class: "flex w-80 flex-col items-center gap-2 rounded-[var(--radius)] border border-border-subtle p-6 text-center",
				Icon { glyph, class: if error { "size-6 text-accent-error" } else { "size-6 text-ink-soft" } }
				span { class: if error { "text-h3 text-accent-error" } else { "text-h3 text-ink" }, "{title}" }
				span { class: "text-body-s text-ink-soft", "{body}" }
				{children}
			}
		}
	}
}

/// What every column of a view reads: its tiles are hosted by a dock of their own, so they don't take props from the view.
#[derive(Clone, Copy)]
struct Lanes {
	groups: Signal<Vec<(String, Vec<usize>)>>,
	by: Signal<GroupBy>,
	shown: Signal<Vec<Shown>>,
	dragged: Signal<Option<tedi_core::IssueLink>>,
	tints: Signal<Tints>,
}

/// Columns, one per group, each a tile of a dock nested in the view's: resized, reordered, tabbed together,
/// kept per view and grouping in the viewer's library. Grouped by a workflow, a card dropped on a column moves there.
#[component]
fn Columns(view: String, groups: Vec<(String, Vec<usize>)>, by: GroupBy, shown: Vec<Shown>) -> Element {
	let ctx = use_context::<Ctx>();
	let tints: Tints = {
		let library = ctx.library.read();
		let on_screen = &library.as_ref().expect("views render once the library arrived").on_screen.tints;
		on_screen.get(&view).and_then(|t| t.get(&by)).cloned().unwrap_or_default() // never tinted
	};
	let lanes = use_context_provider(|| Lanes {
		groups: Signal::new(groups.clone()),
		by: Signal::new(by.clone()),
		shown: Signal::new(shown.clone()),
		dragged: Signal::new(None),
		tints: Signal::new(tints.clone()),
	});
	let mut panels = use_signal(Vec::<DockPanel>::new);
	let mut api = use_signal(|| None::<PackedApi>);
	let mut arranged = use_signal(|| None::<Band>);
	super::use_autosave(ctx, api, arranged, Dock::Columns { view: view.clone(), by: by.clone() });
	// `layout`'s columns hosted, then shown; the effect below adds any group it lacks
	let show = move |mut a: PackedApi, layout: &str| -> bool {
		let mut grid = match tilers_dioxus::model::serial::load(layout) {
			Ok(grid) => grid,
			Err(e) => {
				ctx.toaster
					.error(format!("the kept columns don't load ({e:?}); nothing kept is overwritten until others are shown"));
				return false;
			}
		};
		let (groups, mut panels) = (lanes.groups.peek(), panels);
		for id in grid.cells.iter().flat_map(|c| c.group.tabs.iter()) {
			if !panels.peek().iter().any(|p| &p.id == id) {
				let key = id.0.strip_prefix("col:").expect("a column dock hosts only columns").to_string();
				let count = groups.iter().find(|(k, _)| *k == key).map_or(0, |(_, m)| m.len());
				panels.write().push(column(id.clone(), key.clone(), format!("{key} · {count}")));
			}
		}
		let rows = a.rows(); // a row is a fixed height: columns kept on a taller view are cut to this one
		for cell in grid.cells.iter_mut().filter(|c| c.y < rows && c.y + c.h > rows) {
			cell.h = rows - cell.y;
		}
		a.load(&tilers_dioxus::model::serial::save(&grid)).expect("parsed above, heights cut");
		true
	};
	use_effect(use_reactive!(|groups, shown, tints| {
		let (mut g, mut s, mut t) = (lanes.groups, lanes.shown, lanes.tints);
		if *g.peek() != groups {
			g.set(groups);
		}
		if *s.peek() != shown {
			s.set(shown);
		}
		if *t.peek() != tints {
			t.set(tints);
		}
	}));
	// every group hosted, its count in the title; a group the query no longer yields keeps its tile, empty
	use_effect(move || {
		let groups = lanes.groups.read();
		let Some(mut api) = *api.read() else { return };
		let hosted = api.tab_ids();
		let w = (api.cols() / groups.len().max(1) as u32).max(1);
		let h = api.rows();
		for (key, members) in groups.iter() {
			let id = PanelId(format!("col:{key}"));
			let title = format!("{key} · {}", members.len());
			let at = panels.peek().iter().position(|p| p.id == id);
			match at {
				Some(i) if panels.peek()[i].title != title => panels.write()[i].title = title,
				Some(_) => {}
				None => panels.write().push(column(id.clone(), key.clone(), title)),
			}
			if !hosted.contains(&id) {
				let group = api.mint_group_id();
				api.place(Group::new(group, id), w, h);
			}
		}
	});
	let library = ctx.library.read();
	let library = library.as_ref().expect("views render once the library arrived");
	let kept: Vec<String> = library.columns.iter().filter(|(_, c)| c.by == by).map(|(n, _)| n.clone()).collect();
	let favorites: Vec<String> = kept
		.iter()
		.filter(|n| library.favorites.contains(&crate::api::Favorite::Columns((*n).clone())))
		.cloned()
		.collect();
	let team: Vec<String> = {
		let setup = ctx.setup.read();
		let setup = setup.as_ref().expect("views render once setup arrived");
		setup.columns.iter().filter(|(_, c)| c.by == by).map(|(n, _)| n.clone()).collect()
	};
	let tint = {
		let (view, by) = (view.clone(), by.clone());
		move |tints: Tints| {
			let edit = LibraryEdit::Tinted {
				view: view.clone(),
				by: by.clone(),
				tints,
			};
			spawn(async move { _ = ctx.keep(edit).await });
		}
	};
	let apply = {
		let (show, tint) = (show.clone(), tint.clone());
		move |name: String| {
			let (mut api, mut arranged) = (api, arranged);
			let a = (*api.peek()).expect("the column dock entered a band before its menu shows");
			let band = a.band();
			let mine = ctx.library.peek().as_ref().expect("arrived before the menu").columns.get(&name).cloned();
			let kept = mine
				.or_else(|| ctx.setup.peek().as_ref().expect("arrived before the menu").columns.get(&name).cloned())
				.expect("offered from the viewer's columns or the team's");
			match kept.layout.get(&band) {
				Some(layout) =>
					if show(a, layout) {
						arranged.set(Some(band));
						api.set(Some(a)); // re-runs the placing of groups it lacks
					},
				None if kept.tints.is_empty() => _ = ctx.toaster.info(format!("{name} keeps no columns for a {band} view: arrange these, then save them as {name}")),
				None => {}
			}
			if *lanes.tints.peek() != kept.tints {
				tint(kept.tints);
			}
		}
	};
	rsx! {
		div { class: "flex items-center gap-1 text-xs",
			for name in team {
				Button { key: "team-{name}", variant: ButtonVariant::Ghost, size: Size::Xs, r#type: "button", onclick: { let (apply, name) = (apply.clone(), name.clone()); move |_| apply(name.clone()) }, "{name}" }
			}
			for name in favorites {
				Button { key: "{name}", variant: ButtonVariant::Ghost, size: Size::Xs, r#type: "button", onclick: { let (apply, name) = (apply.clone(), name.clone()); move |_| apply(name.clone()) }, "★ {name}" }
			}
			Popover {
				PopoverTrigger { class: "px-1 text-ink-soft hover:text-ink", aria_label: "column colors", "colors▾" }
				PopoverContent { align: "start", class: "flex flex-col gap-1 p-2 text-xs",
					for (key , _) in lanes.groups.read().iter() {
						div { key: "{key}", class: "flex items-center gap-1",
							span { class: "w-24 truncate", "{key}" }
							for choice in std::iter::once(None).chain(Tint::iter().map(Some)) {
								button {
									r#type: "button",
									title: choice.map_or_else(|| "none".to_string(), |t| t.to_string()),
									class: if lanes.tints.read().get(key).copied() == choice { "size-4 rounded-full border border-ink" } else { "size-4 rounded-full border border-border" },
									style: choice.map_or_else(String::new, |t| format!("background-color: var(--tint-{t})")),
									onclick: {
										let (key, tint) = (key.clone(), tint.clone());
										move |_| {
											let mut tints = lanes.tints.peek().clone();
											match choice {
												Some(t) => _ = tints.insert(key.clone(), t),
												None => _ = tints.remove(&key),
											}
											tint(tints);
										}
									},
								}
							}
						}
					}
				}
			}
			super::library::Menu {
				shelf: Shelf::Columns,
				label: "layouts",
				names: kept,
				on_pick: { let apply = apply.clone(); move |name: String| apply(name) },
				on_save: Some(EventHandler::new(move |name: String| {
					let a = (*api.peek()).expect("the column dock entered a band before its menu shows");
					let by = lanes.by.peek().clone();
					let edit = LibraryEdit::Keep(api::Kept::Columns { name, by, band: a.band(), layout: a.save(), tints: lanes.tints.peek().clone() });
					spawn(async move { _ = ctx.keep(edit).await });
				})),
			}
		}
		div { class: "relative min-h-0 flex-1",
			div { class: "absolute inset-0",
				PackedArea {
					panels,
					on_band: Some(Callback::new({
						let view = view.clone();
						move |a: PackedApi| {
							let band = a.band();
							arranged.set(None);
							let kept = ctx.library.peek().as_ref().and_then(|l| l.on_screen.columns.get(&view)?.get(&*lanes.by.peek())?.get(&band).cloned());
							// kept on a wider view of this band, it would reflow into a jumble: fresh columns instead
							let kept = kept.filter(|layout| tilers_dioxus::model::serial::load(layout).is_ok_and(|g| g.cells.iter().all(|c| c.x + c.w <= a.cols())));
							let shown = match kept {
								Some(layout) => show(a, &layout),
								None => true, // the effect below seeds a tile per group
							};
							if shown {
								arranged.set(Some(band));
							}
							api.set(Some(a));
						}
					})),
					config: None,
				}
			}
		}
	}
}

fn column(id: PanelId, key: String, title: String) -> DockPanel {
	DockPanel {
		id,
		title,
		content: rsx! { Column { key: "{key}", group: key.clone() } },
		min: MinSize::Rem { w: 12.0, h: 8.0 },
	}
}

#[component]
fn Column(group: String) -> Element {
	let ctx = use_context::<Ctx>();
	let toaster = use_toaster();
	let lanes = use_context::<Lanes>();
	let mut dragged = lanes.dragged;
	let by = lanes.by;
	let draggable = matches!(*by.read(), GroupBy::State(_));
	let board = ctx.board.read();
	let cards = &board.as_ref().expect("views render once the board loaded").cards;
	let groups = lanes.groups.read();
	let members = groups.iter().find(|(k, _)| *k == group).map_or(&[][..], |(_, m)| m.as_slice());
	let shown = lanes.shown.read();
	let tint = lanes.tints.read().get(&group).copied();
	rsx! {
		div {
			class: "flex h-full flex-col gap-1.5 overflow-y-auto bg-muted p-1",
			ondragover: move |e| e.prevent_default(),
			ondrop: {
				let key = group.clone();
				move |e: DragEvent| {
					e.prevent_default();
					let Some(link) = dragged.take() else { return };
					let GroupBy::State(workflow) = by.peek().clone() else {
						toaster.info(format!("Only a workflow's columns take a drop; this view groups by {}", by.peek()));
						return;
					};
					let to = State::from_str(&key).expect("a workflow's columns are keyed by its leaves");
					let Some(card) = ctx.card(&link) else { return }; // gone from the board since the drag began
					if card.states[&workflow] == to {
						return;
					}
					spawn(ctx.move_card(card, workflow, to));
				}
			},
			for &i in members {
				div {
					key: "{cards[i].link}",
					draggable: if draggable { "true" } else { "false" },
					ondragstart: { let link = cards[i].link.clone(); move |_| dragged.set(Some(link.clone())) },
					CardView { card: cards[i].clone(), shown: shown.clone() }
				}
			}
		}
	}
}

#[component]
fn Rows(groups: Vec<(String, Vec<usize>)>, shown: Vec<Shown>, sort: Order, query: Query) -> Element {
	let ctx = use_context::<Ctx>();
	let mut in_view = use_context::<InView>();
	// a header leads the order, then leads it reversed, then leaves it; the other keys follow as they were
	let mut cycle = move |by: SortKey| {
		let now = in_view.0.peek().sort.keys().to_vec();
		let lead = match now.first() {
			Some(Sort { by: at, reversed: false }) if *at == by => Some(true),
			Some(Sort { by: at, reversed: true }) if *at == by => None,
			_ => Some(false),
		};
		let mut keys: Vec<Sort> = lead.map(|reversed| Sort { by: by.clone(), reversed }).into_iter().collect();
		keys.extend(now.into_iter().filter(|s| s.by != by));
		if keys.is_empty() {
			keys.push(Sort { by: SortKey::Rank, reversed: false });
		}
		in_view.0.write().sort = keys.into();
	};
	let arrow = |by: &SortKey| match sort.keys().first() {
		Some(Sort { by: at, reversed }) if at == by =>
			if *reversed {
				" ↑"
			} else {
				" ↓"
			},
		_ => "",
	};
	let board = ctx.board.read();
	let cards = &board.as_ref().expect("views render once the board loaded").cards;
	rsx! {
		div { class: "overflow-auto",
			Table {
				TableHeader {
					TableRow {
						TableHead {
							button { r#type: "button", class: "hover:text-ink", onclick: move |_| cycle(SortKey::Title), "Issue{arrow(&SortKey::Title)}" }
						}
						for s in shown.iter().cloned() {
							TableHead { key: "{s}",
								span { class: "inline-flex items-center gap-0.5",
									match SortKey::try_from(s.clone()) {
										Ok(by) => rsx! { button { r#type: "button", class: "hover:text-ink", onclick: { let by = by.clone(); move |_| cycle(by.clone()) }, "{s}{arrow(&by)}" } },
										Err(_) => rsx! { "{s}" },
									}
									if let Ok(facet) = Facet::try_from(s.clone()) {
										FacetMenu { facet, query: query.clone() }
									}
								}
							}
						}
					}
				}
				TableBody {
					for (key , members) in groups {
						if !key.is_empty() {
							TableRow { key: "group-{key}", TableCell { class: "font-semibold", "{key}" } }
						}
						for i in members {
							TableRow { key: "{cards[i].link}",
								TableCell {
									button { class: "text-left hover:underline", onclick: { let link = cards[i].link.clone(); move |_| ctx.open_issue(link.clone()) },
										"#{cards[i].link.number()} {cards[i].title}"
									}
								}
								for s in shown.iter() {
									TableCell { Field { card: cards[i].clone(), shown: s.clone() } }
								}
							}
						}
					}
				}
			}
		}
	}
}
