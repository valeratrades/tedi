//! A view tile: a query, laid out as columns, grouped and sorted as the view says.

use std::str::FromStr as _;

use dioxus::prelude::*;
use ev_lib::uikit_dioxus::*;
use strum::IntoEnumIterator as _;
use tedi_core::{
	board::{GroupBy, Order, Query, Shown, View, group},
	workflow::State,
};
use tilers_dioxus::{Band, Config, DockPanel, Group, LockScope, MinSize, PackedApi, PackedArea, PanelId, Tile};

pub(super) use super::card::CardView;
use super::{
	Ctx,
	facets::InView,
	icons::{Glyph, Icon},
	kit::MenuItem,
	toolbar::ViewToolbar,
};
use crate::api::{self, Dock, LibraryEdit, Sprint, Tint, Tints};

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
	let locked = use_signal(|| {
		let library = ctx.library.peek();
		library.as_ref().expect("views render once the library arrived").on_screen.locked.contains(&defined.name)
	});
	let mut lock_sent = use_signal(|| *locked.peek());
	use_effect({
		let name = defined.name.clone();
		move || {
			let now = locked();
			if *lock_sent.peek() != now {
				lock_sent.set(now);
				let edit = LibraryEdit::Locked { view: name.clone(), locked: now };
				spawn(async move { _ = ctx.keep(edit).await });
			}
		}
	});
	if ctx.board.read().is_none() {
		return rsx! { Spinner {} };
	}
	let v = view.read().clone();
	let error = v.query.parse::<Query>().err().map(|e| e.to_string());
	let ran = if error.is_none() { v.clone() } else { good.read().clone() };
	let Outcome { query, count, groups } = match outcome(&ctx, &ran) {
		Ok(o) => o,
		Err(why) => return rsx! { Empty { glyph: Glyph::Alert, error: true, title: "The view doesn't run", body: why } },
	};
	rsx! {
		LockScope { locked,
			div { class: "flex h-full flex-col",
				ViewToolbar { view, defined, query, error, count }
				Results { ran, groups }
			}
		}
	}
}

/// What a view shows: its query narrowed by the scope, ordered and grouped.
pub(super) struct Outcome {
	pub(super) query: Query,
	pub(super) count: usize,
	pub(super) groups: Vec<(String, Vec<usize>)>,
}

pub(super) fn outcome(ctx: &Ctx, ran: &View) -> Result<Outcome, String> {
	let board = ctx.board.read();
	let board = board.as_ref().expect("views run once the board loaded");
	let within: Query = match ctx.scope() {
		None => Query::default(),
		Some((path, None)) => return Err(format!("the scope {path} is no longer configured: pick another in the scope bar")),
		Some((_, Some(filter))) => filter.parse().expect("a scope's filter is a repo clause"),
	};
	let workflows = ctx.workflows();
	let query: Query = ran.query.parse().map_err(|e: tedi_core::board::QueryError| e.to_string())?;
	let me = ctx.me();
	let run = |q: &Query| -> Result<Vec<usize>, String> {
		let mut scoped = q.clone();
		scoped.narrow(&within);
		scoped.run(&board.cards, me.as_deref()).map_err(|e| e.to_string())
	};
	let mut hits = run(&query)?;
	ran.sort.apply(&board.cards, &mut hits, &workflows).map_err(|e| e.to_string())?;
	let groups = group(&board.cards, &hits, &ran.group_by, &workflows).map_err(|e| e.to_string())?;
	Ok(Outcome { query, count: hits.len(), groups })
}

/// The view's cards, a column per group.
#[component]
pub(super) fn Results(ran: View, groups: Vec<(String, Vec<usize>)>) -> Element {
	let ctx = use_context::<Ctx>();
	rsx! {
		div { class: "flex min-h-0 flex-1 flex-col p-2",
			if ctx.band() == Some(Band::Sm) {
				Stacked { view: ran.name.clone(), by: ran.group_by.clone(), groups, shown: ran.shown.clone() }
			} else {
				Columns { key: "{ran.group_by}", view: ran.name.clone(), groups, by: ran.group_by.clone(), shown: ran.shown.clone(), order: ran.sort.clone() }
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
	order: Signal<Order>,
	dragged: Signal<Option<tedi_core::IssueLink>>,
	tints: Signal<Tints>,
	api: Signal<Option<PackedApi>>,
}

/// Columns, one per group, each a tile of a dock nested in the view's: resized, reordered, tabbed together,
/// collapsed to a spine, kept per view and grouping in the viewer's library. Grouped by a workflow, a card
/// dropped on a column moves there.
#[component]
fn Columns(view: String, groups: Vec<(String, Vec<usize>)>, by: GroupBy, shown: Vec<Shown>, order: Order) -> Element {
	let ctx = use_context::<Ctx>();
	let tints: Tints = {
		let library = ctx.library.read();
		let on_screen = &library.as_ref().expect("views render once the library arrived").on_screen.tints;
		on_screen.get(&view).and_then(|t| t.get(&by)).cloned().unwrap_or_default() // never tinted
	};
	let mut panels = use_signal(Vec::<DockPanel>::new);
	let mut api = use_signal(|| None::<PackedApi>);
	let lanes = use_context_provider(|| Lanes {
		groups: Signal::new(groups.clone()),
		by: Signal::new(by.clone()),
		shown: Signal::new(shown.clone()),
		order: Signal::new(order.clone()),
		dragged: Signal::new(None),
		tints: Signal::new(tints.clone()),
		api,
	});
	use_context_provider(|| ColumnsOf(view.clone()));
	let mut arranged = use_signal(|| None::<Band>);
	super::use_autosave(ctx, api, arranged, Dock::Columns { view: view.clone(), by: by.clone() });
	// `layout`'s columns hosted, then shown; the effect below adds any group it lacks
	let show = move |mut a: PackedApi, layout: &str| -> bool {
		let mut grid = match tilers_dioxus::model::serial::load(layout) {
			Ok(grid) => grid,
			Err(e) => {
				ctx.say(
					"columns",
					super::toast::Tone::Error,
					format!("the kept columns don't load ({e:?}); nothing kept is overwritten until others are shown"),
					None,
				);
				return false;
			}
		};
		let mut panels = panels;
		for id in grid.cells.iter().flat_map(|c| c.group.tabs.iter()) {
			if !panels.peek().iter().any(|p| &p.id == id) {
				let key = id.0.strip_prefix("col:").expect("a column dock hosts only columns").to_string();
				panels.write().push(column(id.clone(), key));
			}
		}
		let rows = a.rows(); // a row is a fixed height: columns kept on a taller view are cut to this one
		for cell in grid.cells.iter_mut().filter(|c| c.y < rows && c.y + c.h > rows) {
			cell.h = rows - cell.y;
		}
		a.load(&tilers_dioxus::model::serial::save(&grid)).expect("parsed above, heights cut");
		true
	};
	use_effect(use_reactive!(|groups, shown, tints, order| {
		let (mut g, mut s, mut t, mut o) = (lanes.groups, lanes.shown, lanes.tints, lanes.order);
		if *g.peek() != groups {
			g.set(groups);
		}
		if *s.peek() != shown {
			s.set(shown);
		}
		if *t.peek() != tints {
			t.set(tints);
		}
		if *o.peek() != order {
			o.set(order);
		}
	}));
	// every group hosted; a group the query no longer yields keeps its tile, empty
	use_effect(move || {
		let groups = lanes.groups.read();
		let Some(mut api) = *api.read() else { return };
		let hosted = api.tab_ids();
		let h = api.rows();
		for (key, members) in groups.iter() {
			let id = PanelId(format!("col:{key}"));
			if !panels.peek().iter().any(|p| p.id == id) {
				panels.write().push(column(id.clone(), key.clone()));
			}
			if !hosted.contains(&id) {
				let group = api.mint_group_id();
				api.place(Group::new(group, id), 1, h); // at its least width: the row scrolls sideways past the view
				if members.is_empty() {
					api.collapse(group); // an empty bucket is a spine, still a drop target
				}
			}
		}
	});
	rsx! {
		div { class: "cols relative min-h-0 flex-1",
			div { class: "absolute inset-0",
				PackedArea {
					panels,
					on_band: Some(Callback::new({
						let view = view.clone();
						move |a: PackedApi| {
							let band = a.band();
							arranged.set(None);
							let kept = ctx.library.peek().as_ref().and_then(|l| l.on_screen.columns.get(&view)?.get(&*lanes.by.peek())?.get(&band).cloned());
							let shown = match kept {
								Some(layout) => show(a, &layout),
								None => true, // the effect above seeds a tile per group
							};
							if shown {
								arranged.set(Some(band));
							}
							api.set(Some(a));
						}
					})),
					config: Some(Config { scroll_x: true, ..Config::default() }),
					actions: Some(Callback::new(|tile: Tile| rsx! { ColumnActions { tile } })),
				}
			}
		}
	}
}

/// Which view a column dock belongs to.
#[derive(Clone)]
struct ColumnsOf(String);

fn column(id: PanelId, key: String) -> DockPanel {
	DockPanel {
		id,
		title: key.clone(),
		label: Some(rsx! { ColumnHead { key: "{key}", group: key.clone() } }),
		content: rsx! { Column { key: "{key}", group: key } },
		min: MinSize::Px { w: 248.0, h: 120.0 },
	}
}

/// `● triage 86`: the tint's dot, the group, its count. A card dropped on a collapsed column lands here.
#[component]
fn ColumnHead(group: String) -> Element {
	let lanes = use_context::<Lanes>();
	let tint = lanes.tints.read().get(&group).copied();
	let count = lanes.groups.read().iter().find(|(k, _)| *k == group).map_or(0, |(_, m)| m.len());
	let drop = use_drop(group.clone());
	rsx! {
		span { class: "flex items-center gap-1.5", ondragover: move |e| e.prevent_default(), ondrop: drop,
			span { class: "size-2 shrink-0 rounded-full", style: match tint { Some(t) => format!("background: var(--tint-{t})"), None => "background: var(--ink-faint)".into() } }
			span { class: "text-body font-medium text-ink", "{group}" }
			span { class: "text-mono-s text-ink-soft", "{count}" }
		}
	}
}

/// `⋯`: the column's tint and the column layouts kept for this grouping.
#[component]
fn ColumnActions(tile: Tile) -> Element {
	let ctx = use_context::<Ctx>();
	let lanes = use_context::<Lanes>();
	let ColumnsOf(view) = use_context::<ColumnsOf>();
	let group = tile.active.0.strip_prefix("col:").expect("a column dock hosts only columns").to_string();
	let by = lanes.by.read().clone();
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
	let library = ctx.library.read();
	let library = library.as_ref().expect("views render once the library arrived");
	let mine: Vec<String> = library.columns.iter().filter(|(_, c)| c.by == by).map(|(n, _)| n.clone()).collect();
	let team: Vec<String> = {
		let setup = ctx.setup.read();
		setup
			.as_ref()
			.expect("views render once setup arrived")
			.columns
			.iter()
			.filter(|(_, c)| c.by == by)
			.map(|(n, _)| n.clone())
			.collect()
	};
	let mut naming = use_signal(|| false);
	let apply = {
		let tint = tint.clone();
		move |name: String| {
			let a = (*lanes.api.peek()).expect("the column dock entered a band before its menu shows");
			let band = a.band();
			let kept = ctx.library.peek().as_ref().expect("arrived before the menu").columns.get(&name).cloned();
			let kept = kept
				.or_else(|| ctx.setup.peek().as_ref().expect("arrived before the menu").columns.get(&name).cloned())
				.expect("offered from the viewer's columns or the team's");
			if let Some(layout) = kept.layout.get(&band) {
				let mut a = a;
				if let Err(e) = a.load(layout) {
					ctx.say("columns", super::toast::Tone::Error, format!("{name}'s {band} columns don't load ({e:?})"), None);
				}
			}
			if *lanes.tints.peek() != kept.tints {
				tint(kept.tints);
			}
		}
	};
	rsx! {
		super::kit::Menu { glyph: Glyph::More, title: "Column menu",
				span { class: "px-2 pt-1.5 pb-0.5 text-overline text-ink-soft", "Tint" }
				div { class: "flex flex-wrap gap-1 px-2 pb-1",
					for choice in std::iter::once(None).chain(Tint::iter().map(Some)) {
						button {
							key: "{choice:?}",
							r#type: "button",
							title: choice.map_or_else(|| "none".to_string(), |t| t.to_string()),
							class: if lanes.tints.read().get(&group).copied() == choice { "size-5 rounded-full border-2 border-ink" } else { "size-5 rounded-full border border-border hover:border-ink-soft" },
							style: choice.map_or_else(|| "background: var(--card)".to_string(), |t| format!("background: var(--tint-{t})")),
							onclick: {
								let (group, tint) = (group.clone(), tint.clone());
								move |_| {
									let mut tints = lanes.tints.peek().clone();
									match choice {
										Some(t) => _ = tints.insert(group.clone(), t),
										None => _ = tints.remove(&group),
									}
									tint(tints);
								}
							},
						}
					}
				}
				div { class: "my-0.5 h-px bg-border-subtle" }
				span { class: "px-2 pt-1.5 pb-0.5 text-overline text-ink-soft", "Column layouts" }
				for name in team.into_iter().chain(mine) {
					MenuItem { key: "{name}", selected: false, label: name.clone(), onclick: { let (apply, name) = (apply.clone(), name.clone()); move |_| apply(name.clone()) } }
				}
				if naming() {
					div { class: "px-1 py-1", onclick: |e: MouseEvent| e.stop_propagation(),
						super::library::NameForm {
							placeholder: "keep these columns as…",
							on_name: move |name: String| {
								naming.set(false);
													let a = (*lanes.api.peek()).expect("the column dock entered a band before its menu shows");
								let edit = LibraryEdit::Keep(api::Kept::Columns { name, by: lanes.by.peek().clone(), band: a.band(), layout: a.save(), tints: lanes.tints.peek().clone() });
								spawn(async move { _ = ctx.keep(edit).await });
							},
							on_cancel: move |()| naming.set(false),
						}
					}
				} else {
					MenuItem { selected: false, glyph: Glyph::Save, label: "Keep these columns as…", onclick: move |_| naming.set(true) }
				}
		}
	}
}

/// A drop of the card being dragged onto `group`: a move, when the columns are a workflow's.
fn use_drop(group: String) -> impl FnMut(DragEvent) + 'static {
	let ctx = use_context::<Ctx>();
	let lanes = use_context::<Lanes>();
	let mut dragged = lanes.dragged;
	let by = lanes.by;
	move |e: DragEvent| {
		e.prevent_default();
		let Some(link) = dragged.take() else { return };
		let GroupBy::State(workflow) = by.peek().clone() else {
			ctx.say(
				"drop",
				super::toast::Tone::Warn,
				format!("Only a workflow's columns take a drop; this view groups by {}", by.peek()),
				None,
			);
			return;
		};
		let to = State::from_str(&group).expect("a workflow's columns are keyed by its leaves");
		let Some(card) = ctx.card(&link) else { return }; // gone from the board since the drag began
		if card.states[&workflow] == to {
			return;
		}
		ctx.move_card(card, workflow, to);
	}
}

#[component]
fn Column(group: String) -> Element {
	let ctx = use_context::<Ctx>();
	let lanes = use_context::<Lanes>();
	let mut dragged = lanes.dragged;
	let mut over = use_signal(|| false);
	let draggable = matches!(*lanes.by.read(), GroupBy::State(_));
	let board = ctx.board.read();
	let cards = &board.as_ref().expect("views render once the board loaded").cards;
	let groups = lanes.groups.read();
	let members = groups.iter().find(|(k, _)| *k == group).map_or(&[][..], |(_, m)| m.as_slice());
	let shown = lanes.shown.read();
	let mut drop = use_drop(group.clone());
	// where the dragged card would land: its place in this column under the view's order
	let landing = match (over(), dragged.read().as_ref()) {
		(true, Some(link)) if !members.iter().any(|&i| &cards[i].link == link) => cards.iter().position(|c| &c.link == link).map(|at| {
			let mut with: Vec<usize> = members.to_vec();
			with.push(at);
			lanes.order.read().apply(cards, &mut with, &ctx.workflows()).expect("the view's order ran on these cards");
			with.iter().position(|&i| i == at).expect("pushed above")
		}),
		_ => None,
	};
	let slot = rsx! {
		div { class: "flex h-14 shrink-0 items-center justify-center rounded-[var(--radius)] border border-ring bg-primary/14 text-caption text-primary-ink",
			if draggable { "Drop → {group}  ·  runs on_enter hooks" } else { "Only a workflow's columns take a drop" }
		}
	};
	rsx! {
		div {
			class: if over() { "flex h-full flex-col gap-1.5 overflow-y-auto rounded-[var(--radius)] outline outline-1 outline-ring" } else { "flex h-full flex-col gap-1.5 overflow-y-auto" },
			ondragover: move |e| {
				e.prevent_default();
				if !over() {
					over.set(true);
				}
			},
			ondragleave: move |_| over.set(false),
			ondrop: move |e| {
				over.set(false);
				drop(e);
			},
			for (n , &i) in members.iter().enumerate() {
				Fragment { key: "{cards[i].link}",
				if landing == Some(n) {
					{slot.clone()}
				}
				div {
					class: if dragged.read().as_ref() == Some(&cards[i].link) { "opacity-40" } else { "" },
					draggable: if draggable { "true" } else { "false" },
					ondragstart: { let link = cards[i].link.clone(); move |_| dragged.set(Some(link.clone())) },
					ondragend: move |_| dragged.set(None),
					CardView { card: cards[i].clone(), shown: shown.clone() }
				}
				}
			}
			if landing == Some(members.len()) {
				{slot.clone()}
			}
		}
	}
}

/// A board on a phone (Figma: 06a): its groups stacked, headers sticky; a long press on a card moves it.
#[component]
fn Stacked(view: String, by: GroupBy, groups: Vec<(String, Vec<usize>)>, shown: Vec<Shown>) -> Element {
	let ctx = use_context::<Ctx>();
	let mut moving = use_signal(|| None::<tedi_core::board::Card>);
	let board = ctx.board.read();
	let cards = &board.as_ref().expect("views render once the board loaded").cards;
	let workflow = match &by {
		GroupBy::State(w) => Some(w.clone()),
		_ => None,
	};
	rsx! {
		div { class: "flex h-full flex-col gap-1.5 overflow-auto",
			for (key , members) in groups.into_iter().filter(|(_, m)| !m.is_empty()) {
				div { key: "head-{key}", class: "sticky top-0 z-[5] bg-background pb-0.5", GroupHead { view: view.clone(), by: by.clone(), group: key.clone(), count: members.len() } }
				for i in members {
					div {
						key: "{key}-{cards[i].link}",
						oncontextmenu: {
							let card = cards[i].clone();
							move |e: MouseEvent| {
								e.prevent_default();
								moving.set(Some(card.clone()));
							}
						},
						CardView { card: cards[i].clone(), shown: shown.clone() }
					}
				}
			}
		}
		if let (Some(card), Some(workflow)) = (moving(), workflow) {
			div { class: "fixed inset-0 z-[100] flex items-end bg-scrim", onclick: move |_| moving.set(None),
				div { class: "flex w-full flex-col gap-0.5 rounded-t-[10px] border-t border-border bg-popover p-2 pb-6", onclick: |e: MouseEvent| e.stop_propagation(),
					span { class: "px-2 pt-1 pb-1.5 text-overline text-ink-soft", "Move #{card.link.number()} to…" }
					for leaf in ctx.workflows()[&workflow].leaves() {
						MenuItem {
							key: "{leaf}",
							selected: card.states[&workflow] == leaf,
							label: leaf.to_string(),
							onclick: {
								let (card, workflow, leaf) = (card.clone(), workflow.clone(), leaf.clone());
								move |_| {
									moving.set(None);
									if card.states[&workflow] != leaf {
										ctx.move_card(card.clone(), workflow.clone(), leaf.clone());
									}
								}
							},
						}
					}
				}
			}
		}
	}
}

/// A group's header, full width above its rows (Figma: ColumnHeader): the tint's dot and wash, the group, its count.
#[component]
fn GroupHead(view: String, by: GroupBy, group: String, count: usize) -> Element {
	let ctx = use_context::<Ctx>();
	let tint = ctx
		.library
		.read()
		.as_ref()
		.expect("views render once the library arrived")
		.on_screen
		.tints
		.get(&view)
		.and_then(|t| t.get(&by)?.get(&group).copied());
	let wash = tint.map_or_else(String::new, |t| format!("background: color-mix(in srgb, var(--tint-{t}) 14%, var(--card))"));
	rsx! {
		div { class: "flex h-8 shrink-0 items-center gap-1.5 rounded-[var(--radius)] bg-card pr-1 pl-2", style: wash,
			span { class: "size-2 shrink-0 rounded-full", style: match tint { Some(t) => format!("background: var(--tint-{t})"), None => "background: var(--ink-faint)".into() } }
			span { class: "text-body font-medium text-ink", "{group}" }
			span { class: "text-mono-s text-ink-soft", "{count}" }
		}
	}
}
