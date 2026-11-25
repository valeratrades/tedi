//! A view tile: the workspace's cards, boxed as the view says.

use std::{collections::BTreeSet, str::FromStr as _};

use dioxus::{core::spawn_forever, prelude::*};
use ev_lib::uikit_dioxus::Logo;
use strum::IntoEnumIterator as _;
use tedi_core::{
	IssueLink, MilestoneLink, NodeLink,
	board::{GroupBy, Lift, Order, Placed, Shown, Target, Within, group},
	workflow::State,
};
use tilers_dioxus::{Band, Config, DockPanel, Group, LockScope, MinSize, PackedApi, PackedArea, PanelId, Tile};

pub(super) use super::card::CardView;
use super::{
	Ctx, Need, Seen,
	icons::{Glyph, Icon},
	issue::Markdown,
	kit::MenuItem,
	route::Go,
	sprint_panel,
	toolbar::ViewToolbar,
};
use crate::api::{Arrangement, Boxing, Columns as Layout, Kept, LibraryEdit, Tint, Tints, View, ViewPart};

pub(super) type Groups = Vec<(String, Vec<Placed>)>;

/// The view `name`: as last edited, against how it is kept.
#[component]
pub fn ViewPanel(name: String) -> Element {
	let ctx = use_context::<Ctx>();
	match ctx.views().into_iter().find(|v| v.name == name) {
		Some(defined) => rsx! { ViewBody { defined } },
		None => rsx! { div { class: "p-3 text-sm text-ink-soft", "The view {name} is no longer kept: close this tab, or keep a view under its name." } },
	}
}

/// What a sprint is for, and its musts, in its order.
#[component]
pub fn SprintPanel(link: MilestoneLink) -> Element {
	let ctx = use_context::<Ctx>();
	let board = ctx.board.read();
	let Some(sprint) = board.targets.iter().find(|t| t.link == link) else {
		return rsx! {
			Behind { what: "a sprint", need: Need::Cards(Vec::new()),
				div { class: "p-3 text-sm text-ink-soft", "{link.as_str()} is no open sprint here" }
			}
		};
	};
	let links: Vec<IssueLink> = sprint.nodes.iter().filter_map(|n| if let NodeLink::Issue(l) = n { Some(l.clone()) } else { None }).collect();
	rsx! { Behind { what: "sprint {sprint.title}", need: Need::Cards(links), Musts { sprint: sprint.clone() } } }
}

/// A sprint's tab: its title, once the board arrived.
#[component]
pub fn SprintTab(link: MilestoneLink) -> Element {
	let ctx = use_context::<Ctx>();
	let board = ctx.board.read();
	let title = board.targets.iter().find(|t| t.link == link).map(|t| t.title.clone());
	ctx.tab_label(Glyph::Sprint, &title.unwrap_or_else(|| format!("{}/{}", link.repo_info(), link.number()))) // not here yet, or closed
}

#[component]
fn Musts(sprint: Target) -> Element {
	let ctx = use_context::<Ctx>();
	let board = ctx.board.read();
	let shown: Vec<Shown> = ctx
		.workflows()
		.into_keys()
		.map(Shown::State)
		.chain([Shown::Repo, Shown::Assignees, Shown::Importance, Shown::Hours])
		.collect();
	rsx! {
		div { class: "flex flex-col gap-1 overflow-auto p-2",
			if !sprint.description.is_empty() {
				div { class: "px-1 pb-1 text-body text-ink-soft", Markdown { text: sprint.description.clone(), ontick: None } }
			}
			for node in sprint.nodes.iter() {
				match node {
					NodeLink::Issue(link) => match board.cards.iter().find(|c| &c.link == link) {
						Some(card) => rsx! { CardView { key: "{link}", card: card.clone(), shown: shown.clone() } },
						None => rsx! { div { key: "{link}", class: "text-xs text-ink-soft", "{link} is not stored here" } },
					},
					NodeLink::Milestone(link) => rsx! { Must { key: "{link.as_str()}", link: link.clone() } },
				}
			}
		}
	}
}

/// A milestone a sprint holds: a sprint opens here, any other on GitHub.
#[component]
fn Must(link: MilestoneLink) -> Element {
	let ctx = use_context::<Ctx>();
	let board = ctx.board.read();
	let Some(held) = board.targets.iter().find(|t| t.link == link) else {
		return rsx! { div { class: "text-xs text-ink-soft", "{link.as_str()} is closed, or not stored here" } };
	};
	let line = rsx! {
		Icon { glyph: Glyph::Sprint, class: "size-3.5 shrink-0 text-ink-soft" }
		span { class: "truncate", "{held.title}" }
		span { class: "shrink-0 text-mono-s text-ink-faint", "{link.repo_info().repo()}" }
	};
	let class = "flex min-w-0 items-center gap-1.5 rounded-[var(--control-radius)] px-1 py-1 text-body text-ink hover:bg-hover";
	match held.sprint() {
		Some(_) => rsx! { Go { to: ctx.at_panel(&sprint_panel(&link)), class, {line} } },
		None => rsx! { a { class, href: "{link.as_str()}", target: "_blank", {line} } },
	}
}

/// `children`, drawn from the start, behind how much of what they show is here until all of it is.
#[component]
pub(super) fn Behind(what: String, need: Need, children: Element) -> Element {
	let ctx = use_context::<Ctx>();
	let mut wanted = ctx.wanted;
	let id = use_hook(|| wanted.peek().keys().last().map_or(0, |id| id + 1));
	if wanted.peek().get(&id) != Some(&(what.clone(), need.clone())) {
		wanted.write().insert(id, (what, need.clone()));
	}
	dioxus::prelude::use_drop(move || _ = wanted.write().remove(&id));
	let awaited = ctx.awaits(&need);
	rsx! {
		div { class: "relative flex h-full min-h-0 flex-1 flex-col",
			div { class: if awaited { "isolate flex min-h-0 flex-1 flex-col" } else { "flex min-h-0 flex-1 flex-col" }, inert: if awaited { "true" }, {children} }
			if awaited {
				div { class: "absolute inset-0 flex bg-card", Loader { need } }
			}
		}
	}
}

/// How much of it is here, or why it isn't.
#[component]
fn Loader(need: Need) -> Element {
	let ctx = use_context::<Ctx>();
	if let Some(why) = &ctx.loading.read().failed {
		return rsx! { Empty { glyph: Glyph::Alert, error: true, title: "The board didn't load", body: "{why}" } };
	}
	let percent = need.percent(&ctx.progress.read(), &ctx.board.read()).unwrap_or(99); // all here, and drawn with the next
	rsx! {
		div { class: "flex flex-1 flex-col items-center justify-center gap-8",
			div { class: "hidden text-primary-ink [[data-mark]_&]:block", Logo { class: "block h-11" } }
			div { role: "progressbar", aria_label: "Loading", aria_valuenow: "{percent}", class: "flex w-48 max-w-[60%] flex-col items-center gap-6",
				div { class: "h-px w-full overflow-hidden bg-border", div { class: "h-full bg-primary-ink", style: "width: {percent}%" } }
				span { class: "pl-[0.2em] text-mono-s tracking-[0.2em] text-ink-soft", "{percent}%" }
			}
		}
	}
}

#[component]
fn ViewBody(defined: View) -> Element {
	let ctx = use_context::<Ctx>();
	let name = use_hook(|| defined.name.clone());
	let view = use_memo({
		let name = name.clone();
		move || ctx.view(&name)
	});
	let edit = use_callback({
		let name = name.clone();
		move |part| ctx.edit_view(&name, part)
	});
	let locked = use_signal(|| {
		let library = ctx.library.peek();
		library.as_ref().expect("views render once the library arrived").locked.contains(&defined.name)
	});
	let mut lock_sent = use_signal(|| *locked.peek());
	use_effect({
		let name = name.clone();
		move || {
			let now = locked();
			if *lock_sent.peek() != now {
				lock_sent.set(now);
				let edit = LibraryEdit::Locked { view: name.clone(), locked: now };
				spawn(async move { _ = ctx.keep(edit).await });
			}
		}
	});
	let by = use_memo(move || view.read().as_ref().map(|v| v.group_by.clone()));
	// off the dock its tile stays mounted, out of sight: another workspace's
	let docked = use_memo({
		let id = PanelId(format!("view:{name}"));
		move || ctx.api.read().is_some_and(|a| a.tab_ids().contains(&id))
	});
	let outcome = use_memo(move || docked().then(|| outcome(&ctx, by.read().as_ref()?)).flatten());
	let relaid = ctx.relaid.read().get(&name).copied().unwrap_or(0); // never laid out anew
	let Some(view) = view.read().clone().filter(|_| docked()) else {
		return rsx! {};
	};
	let (Some(need), Some(outcome)) = (ctx.need(), outcome.read().clone()) else {
		return rsx! {}; // none is open
	};
	let groups = match outcome {
		Ok(groups) => groups,
		Err(why) => return rsx! { Empty { glyph: Glyph::Alert, error: true, title: "The view doesn't run", body: why } },
	};
	let count = (!ctx.awaits(&need)).then(|| count(&groups, &view.boxes.hidden));
	rsx! {
		LockScope { locked,
			div { class: "flex h-full flex-col",
				if ctx.band() != Some(Band::Sm) {
					ViewToolbar { view: view.clone(), defined, groups: groups.iter().map(|(g, _)| g.clone()).collect::<Vec<_>>(), count, edit }
				}
				Behind { what: "view {name}", need,
					Results { view, groups, relaid, edit, onlay: { let name = name.clone(); move |columns| { spawn_forever(ctx.lay(name.clone(), columns)); } } }
				}
			}
		}
	}
}

/// The open workspace's cards as its lens has them, of those here, grouped `by`, the chain's in one; none while none is open.
pub(super) fn outcome(ctx: &Ctx, by: &Boxing) -> Option<Result<Groups, String>> {
	let by = match by {
		Boxing::Groups(by) => by,
		Boxing::Chain => &GroupBy::Nothing,
	};
	let lens = ctx.lens.read().clone()?;
	let board = ctx.board.read();
	let cards = &board.cards;
	let workflows = ctx.workflows();
	let within = match ctx.within() {
		Ok(within) => within,
		Err(_) if ctx.need().is_some_and(|need| ctx.awaits(&need)) => return Some(Ok(Vec::new())), // what it holds may be yet to come
		Err(why) => return Some(Err(why)),
	};
	let run = || -> Result<Groups, String> {
		let mut hits = lens.search.run(&within, cards, ctx.me().as_deref()).map_err(|e| e.to_string())?;
		lens.sort.apply(cards, &mut hits, &workflows).map_err(|e| e.to_string())?;
		group(cards, &hits, by, &workflows, &within).map_err(|e| e.to_string())
	};
	Some(run())
}

/// The cards in the boxes shown.
pub(super) fn count(groups: &Groups, hidden: &BTreeSet<String>) -> usize {
	let shown = groups.iter().filter(|(group, _)| !hidden.contains(group));
	shown.flat_map(|(_, members)| members.iter().map(|p| p.card)).collect::<BTreeSet<_>>().len()
}

/// The view's cards, a column per group shown.
#[component]
pub(super) fn Results(view: View, groups: Groups, relaid: usize, edit: Callback<ViewPart>, onlay: Callback<String>) -> Element {
	let ctx = use_context::<Ctx>();
	rsx! {
		div { class: "flex min-h-0 flex-1 flex-col p-2",
			match &view.group_by {
				Boxing::Chain => rsx! { super::chain::Chain { key: "{relaid}", view: view.clone(), groups, edit } },
				Boxing::Groups(_) if ctx.band() == Some(Band::Sm) => rsx! { Stacked { view: view.clone(), groups } },
				Boxing::Groups(by) => rsx! { Columns { key: "{by}-{relaid}", view: view.clone(), groups, edit, onlay } },
			}
		}
	}
}

//#[codestyle::skip(swappable-args)]
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
	view: Signal<View>,
	groups: Signal<Groups>,
	dragged: Signal<Option<tedi_core::IssueLink>>,
	api: Signal<Option<PackedApi>>,
	edit: Callback<ViewPart>,
	onlay: Callback<String>,
}
impl Lanes {
	fn by(&self) -> GroupBy {
		grouping(&self.view.read())
	}

	fn tint(&self, group: &str) -> Option<Tint> {
		self.view.read().boxes.tints.get(group).copied()
	}
}

/// What a view of columns groups by.
fn grouping(view: &View) -> GroupBy {
	match &view.group_by {
		Boxing::Groups(by) => by.clone(),
		Boxing::Chain => panic!("a view of the chain has no columns: {}", view.name),
	}
}

/// The groups `api`'s dock has a column for.
fn columns_of(api: PackedApi) -> BTreeSet<String> {
	let key = |id: PanelId| id.0.strip_prefix("col:").expect("a column dock hosts only columns").to_string();
	api.tab_ids().into_iter().map(key).collect()
}

/// Columns, one per group shown, each a tile of a dock nested in the view's: resized, reordered, tabbed together.
/// Grouped by a workflow, a card dropped on a column moves there.
#[component]
fn Columns(view: View, groups: Groups, edit: Callback<ViewPart>, onlay: Callback<String>) -> Element {
	let ctx = use_context::<Ctx>();
	let mut panels = use_signal(Vec::<DockPanel>::new);
	let mut api = use_signal(|| None::<PackedApi>);
	let lanes = use_context_provider(|| Lanes {
		view: Signal::new(view.clone()),
		groups: Signal::new(groups.clone()),
		dragged: Signal::new(None),
		api,
		edit,
		onlay,
	});
	let mut seen = use_signal(|| None::<Seen>);
	// the columns the dock had when last looked at: what is there since, the viewer closed or brought back
	let mut placed = use_signal(BTreeSet::<String>::new);
	let made = use_callback(move |(band, layout)| edit.call(ViewPart::Arrangement { of: None, band, layout }));
	let kept = use_callback(move |band| lanes.view.peek().boxes.arrangement.get(&band).cloned());
	super::use_kept(api, seen, kept, made);
	let mut host = move |key: &str| {
		let id = PanelId(format!("col:{key}"));
		if !panels.peek().iter().any(|p| p.id == id) {
			panels.write().push(column(id, key.to_string()));
		}
	};
	let mut show = move |a: PackedApi, layout: &str| -> bool {
		ctx.load(a, layout, |grid| {
			let kept: Vec<PanelId> = grid.cells.iter().flat_map(|c| c.group.tabs.iter()).cloned().collect();
			for id in kept {
				let key = id.0.strip_prefix("col:").expect("a column dock hosts only columns");
				match lanes.view.peek().boxes.hidden.contains(key) {
					true => grid.close(&id), // arranged before it was hidden, in another band
					false => host(key),
				}
			}
			super::cut(grid, a.rows());
		})
	};
	use_effect(use_reactive!(|view, groups| {
		let (mut v, mut g) = (lanes.view, lanes.groups);
		if *v.peek() != view {
			v.set(view);
		}
		if *g.peek() != groups {
			g.set(groups);
		}
	}));
	// the dock and what is hidden follow each other; a group no card is in any more keeps its tile, empty
	use_effect(move || {
		let Some(mut api) = *api.read() else { return };
		let groups = lanes.groups.read();
		let was_hidden = lanes.view.read().boxes.hidden.clone();
		let on_dock = columns_of(api);
		let hidden: BTreeSet<String> = {
			let was = placed.peek();
			let closed = was.difference(&on_dock).cloned();
			let back: BTreeSet<&String> = on_dock.difference(&was).collect();
			was_hidden.iter().cloned().chain(closed).filter(|g| !back.contains(g)).collect()
		};
		let extra: Vec<&String> = on_dock.intersection(&hidden).collect();
		let missing: Vec<&String> = groups.iter().map(|(g, _)| g).filter(|g| !hidden.contains(*g) && !on_dock.contains(*g)).collect();
		if !extra.is_empty() || !missing.is_empty() {
			super::notice(api, seen, |layout| made.call(layout)); // before the dock holds more than what the viewer did
			let h = api.rows();
			for group in extra {
				api.close(&PanelId(format!("col:{group}")));
			}
			for group in missing {
				host(group);
				let id = api.mint_group_id();
				api.place(Group::new(id, PanelId(format!("col:{group}"))), 1, h); // at its least width: the row scrolls sideways past the view
			}
			if seen.peek().is_some() {
				seen.set(Some(Seen::of(api)));
			}
		}
		placed.set(columns_of(api));
		if hidden != was_hidden {
			let mut view = lanes.view;
			view.write().boxes.hidden = hidden.clone();
			edit.call(ViewPart::Hidden(hidden));
		}
	});
	rsx! {
		div { class: "cols relative min-h-0 flex-1",
			div { class: "absolute inset-0",
				PackedArea {
					panels,
					on_band: Some(Callback::new(move |a: PackedApi| {
						seen.set(None);
						let kept = lanes.view.peek().boxes.arrangement.get(&a.band()).cloned();
						if kept.is_none_or(|layout| show(a, &layout)) {
							seen.set(Some(Seen::of(a)));
						}
						placed.set(columns_of(a));
						api.set(Some(a));
					})),
					config: Some(Config { scroll_x: true, ..Config::default() }),
					actions: Some(Callback::new(|tile: Tile| rsx! { ColumnActions { tile } })),
				}
			}
		}
	}
}

fn column(id: PanelId, key: String) -> DockPanel {
	DockPanel {
		id,
		title: key.clone(),
		label: Some(rsx! { ColumnHead { key: "{key}", group: key.clone() } }),
		content: rsx! { Column { key: "{key}", group: key } },
		min: MinSize::Px { w: 248.0, h: 120.0 },
	}
}

/// `● triage 86`: the tint's dot, the group, its count. A card dropped on it lands in the column.
#[component]
fn ColumnHead(group: String) -> Element {
	let lanes = use_context::<Lanes>();
	let tint = lanes.tint(&group);
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
	let group = tile.active.0.strip_prefix("col:").expect("a column dock hosts only columns").to_string();
	let by = lanes.by();
	let (setup, library) = (ctx.setup.read(), ctx.library.read());
	let team = &setup.as_ref().expect("views render once setup arrived").columns;
	let mine = &library.as_ref().expect("views render once the library arrived").columns;
	let layouts: Vec<String> = team.iter().chain(mine).filter(|(_, c)| c.by == by).map(|(n, _)| n.clone()).collect();
	let mut naming = use_signal(|| false);
	let laid = ctx.laid(&lanes.view.read().name);
	rsx! {
		super::kit::Menu { glyph: Glyph::More, title: "Column menu",
				span { class: "px-2 pt-1.5 pb-0.5 text-overline text-ink-soft", "Tint" }
				div { class: "flex flex-wrap gap-1 px-2 pb-1",
					for choice in std::iter::once(None).chain(Tint::iter().map(Some)) {
						button {
							key: "{choice:?}",
							r#type: "button",
							title: choice.map_or_else(|| "none".to_string(), |t| t.to_string()),
							class: if lanes.tint(&group) == choice { "size-5 rounded-full border-2 border-ink" } else { "size-5 rounded-full border border-border hover:border-ink-soft" },
							style: choice.map_or_else(|| "background: var(--card)".to_string(), |t| format!("background: var(--tint-{t})")),
							onclick: {
								let group = group.clone();
								move |_| {
									let mut tints: Tints = lanes.view.peek().boxes.tints.clone();
									match choice {
										Some(t) => _ = tints.insert(group.clone(), t),
										None => _ = tints.remove(&group),
									}
									lanes.edit.call(ViewPart::Tints(tints));
								}
							},
						}
					}
				}
				div { class: "my-0.5 h-px bg-border-subtle" }
				span { class: "px-2 pt-1.5 pb-0.5 text-overline text-ink-soft", "Column layouts" }
				for name in layouts {
					MenuItem { key: "{name}", selected: laid.as_ref() == Some(&name), label: name.clone(), onclick: move |_| lanes.onlay.call(name.clone()) }
				}
				if naming() {
					div { class: "px-1 py-1", onclick: |e: MouseEvent| e.stop_propagation(),
						super::library::NameForm {
							placeholder: "keep these columns as…",
							on_name: move |name: String| {
								naming.set(false);
								let a = (*lanes.api.peek()).expect("the column dock entered a band before its menu shows");
								let (by, mut boxes) = (lanes.by(), lanes.view.peek().boxes.clone());
								boxes.arrangement.extend(Arrangement::from([(a.band(), a.save())])); // as it stands, dragged or not
								let edit = LibraryEdit::Keep(Kept::Columns { name: name.clone(), columns: Layout { by, boxes } });
								spawn_forever(async move {
									if ctx.keep(edit).await {
										lanes.onlay.call(name);
									}
								});
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

/// Whether `card` can't be shown as early as `to`, a Bottlenecks pressing on it; said so.
fn refused(ctx: &Ctx, card: &tedi_core::board::Card, workflow: &str, to: &State) -> bool {
	let workflow = &ctx.workflows()[workflow];
	let within = ctx.within().unwrap_or_default(); // under a scope not on the board, no card shows
	let (Some(floor), Some(bottlenecks), true) = (&workflow.bottlenecks, within.bottlenecks(&card.link), card.open()) else {
		return false;
	};
	if workflow.order(to) >= workflow.order(floor) {
		return false;
	}
	let of = bottlenecks.repo_info();
	let org = of.owner().expect("milestones are github-only");
	ctx.say(
		"drop",
		super::toast::Tone::Warn,
		format!("#{} is in {org}'s Bottlenecks: it stands no earlier than {floor}", card.link.number()),
		None,
	);
	true
}

/// A drop of the card being dragged onto `group`: a move, when the columns are a workflow's.
fn use_drop(group: String) -> impl FnMut(DragEvent) + 'static {
	let ctx = use_context::<Ctx>();
	let lanes = use_context::<Lanes>();
	let mut dragged = lanes.dragged;
	move |e: DragEvent| {
		e.prevent_default();
		let Some(link) = dragged.take() else { return };
		let by = grouping(&lanes.view.peek());
		let GroupBy::State(workflow) = by else {
			ctx.say("drop", super::toast::Tone::Warn, format!("Only a workflow's columns take a drop; this view groups by {by}"), None);
			return;
		};
		let to = State::from_str(&group).expect("a workflow's columns are keyed by its leaves");
		let Some(card) = ctx.card(&link) else { return }; // gone from the board since the drag began
		if refused(&ctx, &card, &workflow, &to) || card.states[&workflow] == to {
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
	let by = lanes.by();
	let draggable = matches!(by, GroupBy::State(_));
	let board = ctx.board.read();
	let cards = &board.cards;
	let order: Option<Order> = ctx.lens.read().as_ref().map(|l| l.sort.clone());
	let groups = lanes.groups.read();
	let members = groups.iter().find(|(k, _)| *k == group).map_or(&[][..], |(_, m)| m.as_slice());
	let shown = lanes.view.read().shown.clone();
	let drawn = use_drawn(members.len());
	let mut drop = use_drop(group.clone());
	let here = |c: &tedi_core::board::Card| matches!(&by, GroupBy::State(w) if c.states[w].to_string() == group);
	// where the dragged card would land: its place in this column under the workspace's order
	let landing = match (over(), dragged.read().as_ref(), &order) {
		(true, Some(link), Some(order)) => cards.iter().position(|c| &c.link == link && !here(c)).map(|at| {
			let mut with: Vec<usize> = members.iter().filter(|p| p.lifted != Some(Lift::Blocks)).map(|p| p.card).collect(); // what it lifts, it does again
			with.push(at);
			let workflows = ctx.workflows();
			order.apply(cards, &mut with, &workflows).expect("the lens's order ran on these cards");
			let groups = tedi_core::board::group(cards, &with, &GroupBy::Nothing, &workflows, &Within::default()).expect("no workflow to be unknown");
			let mut below = groups[0].1.iter().skip_while(|p| p.card != at).skip(1);
			below.find_map(|next| members.iter().position(|p| p.card == next.card)).unwrap_or(members.len()) // none below it: last
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
			for (n , &Placed { card: i , lifted }) in members.iter().enumerate().take(drawn) {
				Fragment { key: "{cards[i].link}",
				if landing == Some(n) {
					{slot.clone()}
				}
				div {
					class: if dragged.read().as_ref() == Some(&cards[i].link) { "opacity-40" } else { "" },
					draggable: if draggable { "true" } else { "false" },
					ondragstart: { let link = cards[i].link.clone(); move |_| dragged.set(Some(link.clone())) },
					ondragend: move |_| dragged.set(None),
					CardView { card: cards[i].clone(), shown: shown.clone(), lifted }
				}
				}
			}
			if landing == Some(members.len()) {
				{slot.clone()}
			}
		}
	}
}

/// How many of so many rows to draw: what a screen holds at once, the rest while the page is free.
fn use_drawn(of: usize) -> usize {
	let mut drawn = use_signal(|| 20);
	use_effect(use_reactive!(|of| {
		if drawn() < of {
			spawn(async move {
				gloo_timers::future::sleep(std::time::Duration::ZERO).await;
				let now = *drawn.peek();
				drawn.set(now + now.min(100));
			});
		}
	}));
	drawn()
}

/// A board on a phone (Figma: 06a): its groups stacked, headers sticky; a long press on a card moves it.
#[component]
fn Stacked(view: View, groups: Groups) -> Element {
	let ctx = use_context::<Ctx>();
	let mut moving = use_signal(|| None::<tedi_core::board::Card>);
	let board = ctx.board.read();
	let cards = &board.cards;
	let workflow = match grouping(&view) {
		GroupBy::State(w) => Some(w),
		_ => None,
	};
	let drawn = use_drawn(groups.iter().map(|(_, m)| m.len()).max().unwrap_or(0)); // a view with no group
	rsx! {
		div { class: "flex h-full flex-col gap-1.5 overflow-auto",
			for (key , members) in groups.into_iter().filter(|(g, m)| !m.is_empty() && !view.boxes.hidden.contains(g)) {
				div { key: "head-{key}", class: "sticky top-0 z-[5] bg-background pb-0.5", GroupHead { tint: view.boxes.tints.get(&key).copied(), group: key.clone(), count: members.len() } }
				for Placed { card: i , lifted } in members.into_iter().take(drawn) {
					div {
						key: "{key}-{cards[i].link}",
						oncontextmenu: {
							let card = cards[i].clone();
							move |e: MouseEvent| {
								e.prevent_default();
								moving.set(Some(card.clone()));
							}
						},
						CardView { card: cards[i].clone(), shown: view.shown.clone(), lifted }
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
									if !refused(&ctx, &card, &workflow, &leaf) && card.states[&workflow] != leaf {
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

//#[codestyle::skip(swappable-args)]
/// A group's header, full width above its rows (Figma: ColumnHeader): the tint's dot and wash, the group, its count.
#[component]
fn GroupHead(tint: Option<Tint>, group: String, count: usize) -> Element {
	let wash = tint.map_or_else(String::new, |t| format!("background: color-mix(in srgb, var(--tint-{t}) 14%, var(--card))"));
	rsx! {
		div { class: "flex h-8 shrink-0 items-center gap-1.5 rounded-[var(--radius)] bg-card pr-1 pl-2", style: wash,
			span { class: "size-2 shrink-0 rounded-full", style: match tint { Some(t) => format!("background: var(--tint-{t})"), None => "background: var(--ink-faint)".into() } }
			span { class: "text-body font-medium text-ink", "{group}" }
			span { class: "text-mono-s text-ink-soft", "{count}" }
		}
	}
}
