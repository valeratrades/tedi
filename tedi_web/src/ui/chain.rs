//! A view boxed by the chain (Figma: 09 · Chain): what the workspace's scope is, the leaf issues right in it as a dock,
//! and what else it holds as tabs, the selected one the same again. A tab's inside is made as it is first opened, and kept.

use std::collections::{HashMap, HashSet};

use dioxus::prelude::*;
use tedi_core::{
	IssueLink, MilestoneLink,
	board::{Card, Shown, Slice},
	chain::Point,
};
use tilers_dioxus::{Band, DockPanel, Group, MinSize, PackedApi, PackedArea, PanelId, model::packed::GridState};

use super::{
	Ctx, Seen,
	card::{CardView, Chip, Field, KindIcon, Tone},
	icons::{Glyph, Icon},
	issue::{Markdown, Number},
	kit::IconBtn,
	route::Go,
	scope::glyph_at,
	view::{Empty, Groups},
};
use crate::api::{View, ViewPart};

/// What every node reads: the docks host its leaves, so they take no props from the view.
#[derive(Clone, Copy)]
struct Nest {
	view: Signal<View>,
	/// The cards the lens leaves in, in its order.
	hits: Signal<Vec<IssueLink>>,
	edit: Callback<ViewPart>,
}

#[component]
pub(super) fn Chain(view: View, groups: Groups, edit: Callback<ViewPart>) -> Element {
	let ctx = use_context::<Ctx>();
	let hits: Vec<IssueLink> = {
		let board = ctx.board.read();
		groups.iter().flat_map(|(_, members)| members).map(|p| board.cards[p.card].link.clone()).collect()
	};
	let nest = use_context_provider(|| Nest {
		view: Signal::new(view.clone()),
		hits: Signal::new(hits.clone()),
		edit,
	});
	use_effect(use_reactive!(|view, hits| {
		let (mut v, mut h) = (nest.view, nest.hits);
		if *v.peek() != view {
			v.set(view);
		}
		if *h.peek() != hits {
			h.set(hits);
		}
	}));
	let lens = ctx.lens.read();
	match lens.as_ref().map(|l| &l.scope) {
		Some(Slice::Point(point)) => rsx! {
			div { class: "flex min-h-0 flex-1 overflow-auto", Node { key: "{point}", path: vec![point.clone()] } }
		},
		Some(Slice::Terms(_)) | None => rsx! {
			Empty {
				glyph: Glyph::Scope,
				title: "No point to nest from",
				body: "A chain view nests what one point of the chain holds: scope the workspace to an org, a repo, a milestone or an issue.",
			}
		},
	}
}

/// What a node holds right in it.
#[derive(Clone, Default, PartialEq)]
struct Held {
	/// Those that hold more, issues last.
	nodes: Vec<Point>,
	leaves: Vec<IssueLink>,
	/// Closed, or not stored here.
	away: usize,
}

/// The last of `path`, the points from the scope down to it.
#[component]
fn Node(path: Vec<Point>) -> Element {
	let ctx = use_context::<Ctx>();
	let nest = use_context::<Nest>();
	let at = path.last().expect("a path holds what it leads to").clone();
	let held = use_memo({
		let at = at.clone();
		move || held(&ctx, &nest, &at)
	});
	let mut selected = use_signal(|| None::<Point>);
	let mut opened = use_hook(|| CopyValue::new(Vec::<Point>::new()));
	if path[..path.len() - 1].contains(&at) {
		return rsx! { Empty { glyph: Glyph::Revert, error: true, title: "It is in itself", body: "{ctx.called(&at)} is owned by what it owns." } };
	}
	let Held { nodes, leaves, away } = held();
	let shown = selected().filter(|p| nodes.contains(p)).or_else(|| nodes.first().cloned());
	opened.write().retain(|p| nodes.contains(p));
	if let Some(shown) = &shown
		&& !opened.read().contains(shown)
	{
		opened.write().push(shown.clone());
	}
	let (node, left) = match (ctx.band(), nodes.is_empty()) {
		(Some(Band::Sm), _) => ("flex min-h-fit min-w-0 flex-1 flex-col gap-2", "flex min-w-0 flex-col gap-2"),
		(_, true) => ("flex min-h-fit flex-1 gap-2", "flex min-w-72 flex-1 flex-col gap-2"),
		(_, false) => ("flex min-h-fit flex-1 gap-2", "flex w-72 shrink-0 flex-col gap-2"),
	};
	rsx! {
		section { class: node, aria_label: ctx.called(&at),
			div { class: left,
				Head { at: at.clone(), away, root: path.len() == 1 }
				if !leaves.is_empty() {
					Leaves { at: at.clone(), leaves }
				}
			}
			if let Some(shown) = shown {
				div { class: "flex flex-1 flex-col",
					div { role: "tablist", class: "flex flex-wrap",
						for point in nodes {
							button {
								key: "{point}",
								r#type: "button",
								role: "tab",
								aria_selected: if point == shown { "true" } else { "false" },
								class: if point == shown { "flex h-8 max-w-64 min-w-0 items-center gap-1.5 border-t-2 border-primary-ink bg-card px-3 text-body text-ink" } else { "flex h-8 max-w-64 min-w-0 items-center gap-1.5 border-t-2 border-transparent px-3 text-body text-ink-mid hover:bg-hover" },
								onclick: { let point = point.clone(); move |_| selected.set(Some(point.clone())) },
								Icon { glyph: glyph_at(&point), class: "size-3.5 shrink-0 text-ink-soft" }
								span { class: "truncate",
									if let (Point::Milestone(link), false) = (&point, matches!(at, Point::Repo(_))) { "{link.repo_info().repo()} · " }
									"{ctx.called(&point)}"
								}
							}
						}
					}
					div { role: "tabpanel", class: "flex flex-1 flex-col rounded-[var(--radius)] border border-border-subtle p-2",
						for point in opened.read().iter().cloned() {
							div { key: "{point}", class: if point == shown { "flex flex-1 flex-col" } else { "hidden" },
								Node { path: path.iter().cloned().chain([point.clone()]).collect::<Vec<_>>() }
							}
						}
					}
				}
			}
		}
	}
}

fn held(ctx: &Ctx, nest: &Nest, at: &Point) -> Held {
	let (board, hits) = (ctx.board.read(), nest.hits.read());
	let place: HashMap<&IssueLink, usize> = hits.iter().enumerate().map(|(n, link)| (link, n)).collect();
	let cards: HashMap<&IssueLink, &Card> = board.cards.iter().map(|c| (&c.link, c)).collect();
	let parents: HashSet<&IssueLink> = board.cards.iter().filter_map(|c| c.parent.as_ref()).collect();
	let (mut held, mut epics) = (Held::default(), Vec::new());
	for point in board.below(at, &ctx.orgs()) {
		match point {
			Point::Org(_) | Point::Repo(_) => held.nodes.push(point),
			Point::Milestone(ref link) if board.targets.iter().any(|t| t.link == *link) => held.nodes.push(point),
			Point::Milestone(_) => held.away += 1,
			Point::Issue(link) => match cards.get(&link) {
				None => held.away += 1,
				// what the lens leaves in may be anywhere below it
				Some(card) if parents.contains(&link) && (card.open() || place.contains_key(&link)) => epics.push(link),
				Some(_) if !parents.contains(&link) && place.contains_key(&link) => held.leaves.push(link),
				Some(_) => {}
			},
		}
	}
	let placed = |link: &IssueLink| place.get(link).map_or((true, 0), |n| (false, *n)); // not a hit itself: last
	epics.sort_by_key(placed);
	held.leaves.sort_by_key(placed);
	held.nodes.extend(epics.into_iter().map(Point::Issue));
	held
}

/// What a node is: name, description, the rest as chips.
#[component]
fn Head(at: Point, away: usize, root: bool) -> Element {
	let ctx = use_context::<Ctx>();
	let (repo, body) = match &at {
		Point::Org(_) | Point::Repo(_) => (None, rsx! { span { class: "text-h3 text-ink", "{ctx.called(&at)}" } }),
		Point::Milestone(link) => (Some(link.repo_info()), rsx! { MilestoneHead { link: link.clone() } }),
		Point::Issue(link) => (Some(link.project()), rsx! { IssueHead { link: link.clone() } }),
	};
	let scope_to = at.clone();
	rsx! {
		div { class: "flex max-h-80 shrink-0 flex-col gap-2 overflow-y-auto rounded-[var(--radius)] border border-border-subtle bg-card px-3 py-2",
			div { class: "flex h-6 shrink-0 items-center gap-1.5",
				match &at {
					Point::Issue(link) => match ctx.card(link) {
						Some(card) => rsx! { KindIcon { card } },
						None => rsx! { Icon { glyph: Glyph::Issue, class: "size-3 shrink-0 text-ink-soft" } },
					},
					_ => rsx! { Icon { glyph: glyph_at(&at), class: "size-3.5 shrink-0 text-ink-soft" } },
				}
				a { class: "truncate text-mono-s text-ink-soft hover:underline", href: "{at}", target: "_blank",
					match repo {
						Some(repo) => rsx! { "{repo}" },
						None => rsx! { "GitHub" },
					}
				}
				if let Point::Issue(link) = &at {
					Number { link: link.clone() }
				}
				div { class: "flex-1" }
				if !root {
					IconBtn { glyph: Glyph::Scope, title: "Scope to it", onclick: move |_| ctx.scope_to(scope_to.clone()) }
				}
			}
			{body}
			if away > 0 {
				span { class: "text-caption text-ink-faint", "{away} more it holds: closed, or not stored here" }
			}
		}
	}
}

#[component]
fn MilestoneHead(link: MilestoneLink) -> Element {
	let ctx = use_context::<Ctx>();
	let board = ctx.board.read();
	let Some(target) = board.targets.iter().find(|t| t.link == link) else {
		return rsx! { span { class: "text-body text-ink-soft", "{link.as_str()} is closed, or not stored here" } };
	};
	let hosted: Vec<&Card> = board.cards.iter().filter(|c| target.hosts(&c.link)).collect();
	let closed = hosted.iter().filter(|c| !c.open()).count();
	let due = target.due_on.map(|at| at.strftime("%Y-%m-%d").to_string());
	rsx! {
		span { class: "text-h3 text-ink", "{target.title}" }
		div { class: "flex flex-wrap items-center gap-1",
			Chip { tone: Tone::Positive, "open" }
			if let Some(due) = due {
				Chip { tone: Tone::Warn, "due {due}" }
			}
			if !hosted.is_empty() {
				Chip { tone: Tone::Neutral, "{closed} of {hosted.len()} closed" }
			}
		}
		if !target.description.is_empty() {
			div { class: "text-body-s text-ink-mid", Markdown { text: target.description.clone() } }
		}
	}
}

#[component]
fn IssueHead(link: IssueLink) -> Element {
	let ctx = use_context::<Ctx>();
	let Some(card) = ctx.card(&link) else {
		return rsx! { span { class: "text-body text-ink-soft", "{link} is not on the board: not stored here" } };
	};
	rsx! {
		Go { to: ctx.at.read().with_issue(&link), class: "text-left text-h3 text-ink hover:underline", "{card.title}" }
		div { class: "flex flex-wrap items-center gap-1",
			for shown in [Shown::Type, Shown::Importance, Shown::Hours, Shown::Labels, Shown::Assignees] {
				Field { key: "{shown}", card: card.clone(), shown }
			}
		}
		if !card.body.is_empty() {
			div { class: "text-body-s text-ink-mid", Markdown { text: card.body.clone() } }
		}
	}
}

/// A dock of the leaf issues right in `at`, a tile each; as arranged, else in the workspace's order.
#[component]
fn Leaves(at: Point, leaves: Vec<IssueLink>) -> Element {
	let ctx = use_context::<Ctx>();
	let nest = use_context::<Nest>();
	let mut panels = use_signal(Vec::<DockPanel>::new);
	let mut api = use_signal(|| None::<PackedApi>);
	let mut seen = use_signal(|| None::<Seen>);
	let mut wanted = use_signal(|| leaves.clone());
	// what it was last laid out for
	let mut placed = use_signal(Vec::<IssueLink>::new);
	let kept = use_callback({
		let at = at.clone();
		move |band: Band| nest.view.peek().boxes.nodes.get(&at).and_then(|bands| bands.get(&band).cloned())
	});
	let made = use_callback(move |(band, layout)| nest.edit.call(ViewPart::Arrangement { of: Some(at.clone()), band, layout }));
	let mut host = move |id: &PanelId| {
		if !panels.peek().iter().any(|p| p.id == *id) {
			panels.write().push(leaf(id));
		}
	};
	use_effect(use_reactive!(|leaves| {
		if *wanted.peek() != leaves {
			wanted.set(leaves);
		}
	}));
	// a tree misses no leaf: one whose tile is closed comes back last
	use_effect(move || {
		let Some(mut api) = *api.read() else { return };
		if super::kept(api).state != GridState::Settled {
			return; // in the viewer's hand: what it comes to is noticed first
		}
		super::notice(api, seen, |layout| made.call(layout)); // before the dock holds other than what the viewer did
		let wanted = wanted.read();
		let ids: Vec<PanelId> = wanted.iter().map(|link| PanelId(format!("leaf:{link}"))).collect();
		let was = api.tab_ids();
		if *placed.peek() != *wanted && kept.call(api.band()).is_none() {
			api.reset(); // in their order
		}
		for id in api.tab_ids().iter().filter(|id| !ids.contains(id)) {
			api.close(id);
		}
		let on_dock: HashSet<PanelId> = api.tab_ids().into_iter().collect();
		let across = match api.band() {
			Band::Sm => 1,
			Band::Md => 2,
			Band::Xl => 3,
		};
		for id in ids.iter().filter(|id| !on_dock.contains(id)) {
			host(id);
			let group = Group::new(api.mint_group_id(), id.clone());
			api.place(group, api.cols().div_euclid(across), 1);
		}
		if *placed.peek() != *wanted {
			placed.set(wanted.clone());
		}
		if api.tab_ids() != was && seen.peek().is_some() {
			seen.set(Some(Seen::of(api)));
		}
	});
	rsx! {
		div { class: "relative min-h-48 flex-1",
			div { class: "absolute inset-0",
				PackedArea {
					panels,
					on_band: Some(Callback::new(move |a: PackedApi| {
						seen.set(None);
						placed.set(Vec::new());
						let loads = kept.call(a.band()).is_none_or(|layout| ctx.load(a, &layout, |grid| {
							for id in grid.cells.iter().flat_map(|c| c.group.tabs.iter()) {
								host(id);
							}
						}));
						if loads {
							seen.set(Some(Seen::of(a)));
						}
						api.set(Some(a));
					})),
				}
			}
		}
	}
}

fn leaf(id: &PanelId) -> DockPanel {
	let link = IssueLink::parse(id.0.strip_prefix("leaf:").expect("a node's dock hosts only leaves")).expect("its id is its link");
	let title = format!("#{}", link.number());
	DockPanel {
		id: id.clone(),
		label: Some(rsx! {
			span { class: "flex items-center gap-1.5",
				Icon { glyph: Glyph::Issue, class: "size-3.5 shrink-0 text-ink-soft" }
				span { class: "text-body", "{title}" }
			}
		}),
		title: title.clone(),
		content: rsx! { Leaf { link } },
		min: MinSize::Px { w: 224.0, h: 96.0 },
	}
}

#[component]
fn Leaf(link: IssueLink) -> Element {
	let ctx = use_context::<Ctx>();
	let nest = use_context::<Nest>();
	let Some(card) = ctx.card(&link) else {
		return rsx! {}; // gone from the board: its tile goes next
	};
	rsx! {
		div { class: "h-full overflow-y-auto", CardView { card, shown: nest.view.read().shown.clone() } }
	}
}
