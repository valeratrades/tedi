//! A view tile: a query, laid out as columns, a table or a list, grouped and sorted as the view says.

use std::str::FromStr as _;

use dioxus::prelude::*;
use ev_lib::uikit_dioxus::{Card as Tile, *};
use tedi_core::{
	board::{Card, GroupBy, Layout, Query, Shown, View, group, restage},
	conflict::{Field, FieldEdit, list},
	labels::{HOURS, Scheme},
	stage::Stage,
};
use tilers_dioxus::{DockPanel, Group, MinSize, PackedApi, PackedArea, PanelId, keys::Keymap};

use super::Ctx;
use crate::api::{self, Sprint};

#[component]
pub fn ViewPanel(view: View) -> Element {
	let ctx = use_context::<Ctx>();
	let toaster = use_toaster();
	let mut view = use_signal(|| view);
	let board = ctx.board.read();
	let Some(board) = board.as_ref() else {
		return rsx! { Spinner {} };
	};
	let v = view.read().clone();
	let hits = Query::from_str(&v.query).and_then(|q| q.run(&board.cards, ctx.me().as_deref())).map(|mut hits| {
		v.sort.apply(&board.cards, &mut hits);
		hits
	});
	let save = move |_| {
		let view = view.read().clone();
		spawn(async move {
			match api::save_view(view.clone()).await {
				Ok(()) => _ = toaster.positive(format!("Saved {} to your views", view.name)),
				Err(e) => _ = toaster.error(e.to_string()),
			}
		});
	};
	rsx! {
		div { class: "flex h-full flex-col gap-2 p-2",
			div { class: "flex flex-wrap items-center gap-2",
				Input {
					class: "min-w-64 flex-1 font-mono",
					value: v.query.clone(),
					placeholder: "stage:triage,selected repo:EV-Service-Arb/* t:bug i:>=5 text",
					oninput: move |e: FormEvent| view.write().query = e.value(),
				}
				Choice { value: v.layout, on_change: move |l| view.write().layout = l }
				Choice { value: v.group_by, on_change: move |g| view.write().group_by = g }
				Choice { value: v.sort, on_change: move |s| view.write().sort = s }
				Button { variant: ButtonVariant::Outline, size: Size::Sm, r#type: "button", onclick: save, "Save as mine" }
			}
			match hits {
				Err(e) => rsx! { Badge { variant: BadgeVariant::Error, "{e}" } },
				Ok(hits) => {
					let groups = group(&board.cards, &hits, v.group_by);
					match v.layout {
						Layout::Board => rsx! { Columns { key: "{v.group_by}", view: v.name.clone(), groups, by: v.group_by, shown: v.shown.clone() } },
						Layout::Table => rsx! { Rows { groups, shown: v.shown.clone() } },
						Layout::List => rsx! {
							div { class: "flex flex-col gap-1 overflow-auto",
								for (key , members) in groups {
									if !key.is_empty() { h3 { class: "text-sm font-semibold mt-2", "{key}" } }
									for i in members {
										CardView { key: "{board.cards[i].link}", card: board.cards[i].clone(), shown: v.shown.clone() }
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

/// One of a closed set (layout, grouping, order), shown by its name.
#[component]
fn Choice<T: strum::IntoEnumIterator + std::fmt::Display + std::str::FromStr + Copy + PartialEq + 'static>(value: T, on_change: EventHandler<T>) -> Element {
	rsx! {
		Select {
			value: value.to_string(),
			on_value_change: move |s: String| on_change.call(T::from_str(&s).ok().expect("offered from the same set")),
			SelectTrigger { size: Size::Sm, SelectValue {} }
			SelectContent {
				for option in T::iter() {
					SelectItem { value: option.to_string(), text_value: option.to_string(), "{option}" }
				}
			}
		}
	}
}

/// What every column of a view reads: its tiles are hosted by a dock of their own, so they don't take props from the view.
#[derive(Clone, Copy)]
struct Lanes {
	groups: Signal<Vec<(String, Vec<usize>)>>,
	by: GroupBy,
	shown: Signal<Vec<Shown>>,
	dragged: Signal<Option<tedi_core::IssueLink>>,
}

/// Columns, one per group, each a tile of a dock nested in the view's: resized, reordered, tabbed together
/// and cached per view and grouping. Grouped by stage, a card dropped on a column moves there.
#[component]
fn Columns(view: String, groups: Vec<(String, Vec<usize>)>, by: GroupBy, shown: Vec<Shown>) -> Element {
	let lanes = use_context_provider(|| Lanes {
		groups: Signal::new(groups.clone()),
		by,
		shown: Signal::new(shown.clone()),
		dragged: Signal::new(None),
	});
	let mut panels = use_signal(Vec::<DockPanel>::new);
	let mut api = use_signal(|| None::<PackedApi>);
	use_effect(use_reactive!(|groups, shown| {
		let (mut g, mut s) = (lanes.groups, lanes.shown);
		if *g.peek() != groups {
			g.set(groups);
		}
		if *s.peek() != shown {
			s.set(shown);
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
	let config = tilers_dioxus::Config {
		storage_key: Some(format!("tedi-columns-{view}-{by}")),
		keymap: Keymap {
			scope: Vec::new(),
			root: Vec::new(),
			..Default::default()
		}, // the view's dock holds the keys; two docks would both answer them
		..Default::default()
	};
	rsx! {
		div { class: "relative min-h-0 flex-1",
			div { class: "absolute inset-0",
				PackedArea {
					panels,
					on_band: Some(Callback::new(move |a: PackedApi| {
						let groups = lanes.groups.peek();
						let mut panels = panels;
						for id in a.tab_ids() {
							if !panels.peek().iter().any(|p| p.id == id) {
								let key = id.0.strip_prefix("col:").expect("this dock hosts only columns").to_string();
								let count = groups.iter().find(|(k, _)| *k == key).map_or(0, |(_, m)| m.len());
								panels.write().push(column(id, key.clone(), format!("{key} · {count}")));
							}
						}
						api.set(Some(a));
					})),
					config: Some(config),
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
		min: MinSize::Rem { w: 14.0, h: 8.0 },
	}
}

#[component]
fn Column(group: String) -> Element {
	let ctx = use_context::<Ctx>();
	let toaster = use_toaster();
	let lanes = use_context::<Lanes>();
	let mut dragged = lanes.dragged;
	let by = lanes.by;
	let board = ctx.board.read();
	let cards = &board.as_ref().expect("views render once the board loaded").cards;
	let groups = lanes.groups.read();
	let members = groups.iter().find(|(k, _)| *k == group).map_or(&[][..], |(_, m)| m.as_slice());
	let shown = lanes.shown.read();
	rsx! {
		div {
			class: "flex h-full flex-col gap-2 overflow-y-auto p-1",
			ondragover: move |e| e.prevent_default(),
			ondrop: {
				let key = group.clone();
				move |e: DragEvent| {
					e.prevent_default();
					let Some(link) = dragged.take() else { return };
					if by != GroupBy::Stage {
						toaster.info(format!("Only stage columns take a drop; this view groups by {by}"));
						return;
					}
					let to = Stage::from_str(&key).expect("stage columns are keyed by stage");
					let Some(card) = ctx.card(&link) else { return }; // gone from the board since the drag began
					if card.stage == to {
						return;
					}
					match restage(&card.labels, to) {
						Ok((labels, state)) => {
							let mut edits = vec![FieldEdit { field: Field::Labels, seen: list(&card.labels), new: list(&labels) }];
							if state != card.state {
								edits.push(FieldEdit {
									field: Field::State,
									seen: card.state.to_checkbox_contents(),
									new: state.to_checkbox_contents(),
								});
							}
							spawn(ctx.edit(card.link.clone(), edits));
						}
						Err(e) => _ = toaster.info(e.to_string()),
					}
				}
			},
			for &i in members {
				div {
					key: "{cards[i].link}",
					draggable: "true",
					ondragstart: { let link = cards[i].link.clone(); move |_| dragged.set(Some(link.clone())) },
					CardView { card: cards[i].clone(), shown: shown.clone() }
				}
			}
		}
	}
}

#[component]
fn Rows(groups: Vec<(String, Vec<usize>)>, shown: Vec<Shown>) -> Element {
	let ctx = use_context::<Ctx>();
	let board = ctx.board.read();
	let cards = &board.as_ref().expect("views render once the board loaded").cards;
	rsx! {
		div { class: "overflow-auto",
			Table {
				TableHeader {
					TableRow {
						TableHead { "Issue" }
						for s in shown.iter() { TableHead { "{s}" } }
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
									TableCell { Facet { card: cards[i].clone(), shown: *s } }
								}
							}
						}
					}
				}
			}
		}
	}
}

/// title · repo · @who · i[5]± h[3]± ⛓2 — what `shown` asks for, in its order.
#[component]
pub fn CardView(card: Card, shown: Vec<Shown>) -> Element {
	let ctx = use_context::<Ctx>();
	let link = card.link.clone();
	rsx! {
		Tile { class: if card.conflicted { "gap-1 p-2 border-accent-error" } else { "gap-1 p-2" },
			button { class: "text-left text-sm font-medium hover:underline", onclick: move |_| ctx.open_issue(link.clone()), "{card.title}" }
			div { class: "flex flex-wrap items-center gap-1 text-xs text-ink-soft",
				for s in shown {
					Facet { card: card.clone(), shown: s }
				}
				if card.conflicted { Badge { variant: BadgeVariant::Error, "clash" } }
			}
		}
	}
}

#[component]
fn Facet(card: Card, shown: Shown) -> Element {
	let ctx = use_context::<Ctx>();
	match shown {
		Shown::Repo => rsx! { span { "{card.link.project().repo()}" } },
		Shown::Assignees => rsx! { for a in card.assignees.iter() { span { "@{a}" } } },
		Shown::Stage => rsx! { Badge { "{card.stage}" } },
		Shown::Type => rsx! { if let Some(k) = card.kind { Badge { variant: BadgeVariant::Info, "{k}" } } },
		Shown::Labels => rsx! { for l in card.labels.iter() { Badge { "{l}" } } },
		Shown::Importance => rsx! { Stepper { card: card.clone(), axis: Axis::Importance } },
		Shown::Hours => rsx! { Stepper { card: card.clone(), axis: Axis::Hours } },
		Shown::Blockers => {
			let board = ctx.board.read();
			let cards = &board.as_ref().expect("cards render from the board").cards;
			let blocking = card.blocked_by.iter().filter(|b| cards.iter().any(|c| &c.link == *b && c.open())).count();
			rsx! { if blocking > 0 { span { title: "blocked by {blocking} open", "⛓{blocking}" } } }
		}
		Shown::Prs => rsx! { for pr in card.prs.iter() { a { href: "{pr.url}", target: "_blank", Badge { "PR {pr.state}" } } } },
	}
}

#[derive(Clone, Copy, PartialEq)]
enum Axis {
	Importance,
	Hours,
}

/// `i[5]±`: a step up or down is a labels edit, made against the labels the card shows.
#[component]
fn Stepper(card: Card, axis: Axis) -> Element {
	let ctx = use_context::<Ctx>();
	let (prefix, value, counts_as) = match axis {
		Axis::Importance => ("i", card.importance.map(u16::from), Some(3)), // `labels.nix`: "`i:3` is the default"
		Axis::Hours => ("h", card.hours, None),
	};
	let steps: Vec<u16> = match axis {
		Axis::Importance => (0..=9).collect(),
		Axis::Hours => HOURS.to_vec(),
	};
	let step = move |up: bool| {
		let at = value.or(counts_as).map(|v| steps.iter().position(|s| *s == v).expect("the scheme's own values"));
		let next = match (at, up) {
			(None, true) => Some(steps[0]),
			(None, false) => None,
			(Some(i), true) => steps.get(i + 1).copied(),
			(Some(i), false) => i.checked_sub(1).map(|i| steps[i]),
		};
		let Some(next) = next else { return };
		let mut labels: Vec<String> = card
			.labels
			.iter()
			.filter(|l| {
				let scheme = Scheme::from(std::slice::from_ref(*l));
				match axis {
					Axis::Importance => scheme.importance.is_none(),
					Axis::Hours => scheme.hours.is_none(),
				}
			})
			.cloned()
			.collect();
		labels.push(format!("{prefix}:{next}"));
		spawn(ctx.edit(
			card.link.clone(),
			vec![FieldEdit {
				field: Field::Labels,
				seen: list(&card.labels),
				new: list(&labels),
			}],
		));
	};
	let shown = value.map_or_else(|| "?".to_string(), |v| v.to_string());
	rsx! {
		span { class: "inline-flex items-center gap-0.5 rounded border px-1",
			"{prefix}[{shown}]"
			button { class: "px-0.5 hover:text-ink", title: "lower", onclick: { let step = step.clone(); move |_| step(false) }, "−" }
			button { class: "px-0.5 hover:text-ink", title: "raise", onclick: move |_| step(true), "+" }
		}
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
	let shown = vec![Shown::Stage, Shown::Repo, Shown::Assignees, Shown::Importance, Shown::Hours];
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
