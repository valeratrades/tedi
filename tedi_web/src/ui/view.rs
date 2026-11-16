//! A view tile: a query, laid out as columns, a table or a list, grouped and sorted as the view says.

use std::str::FromStr as _;

use dioxus::prelude::*;
use ev_lib::uikit_dioxus::{Card as Tile, *};
use strum::IntoEnumIterator as _;
use tedi_core::{
	board::{Card, Facet, GroupBy, Layout, Query, Shown, Sort, SortKey, View, group, restage},
	conflict::{Field, FieldEdit, list},
	labels::{Color, HOURS, Scheme},
	stage::Stage,
};
use tilers_dioxus::{Band, DockPanel, Group, MinSize, PackedApi, PackedArea, PanelId};

use super::{
	Ctx,
	facets::{FacetBar, FacetMenu, InView, Pick},
};
use crate::api::{Dock, Kept, LibraryEdit, Shelf, Sprint, Tint, Tints};

/// `view` as last edited on screen, `defined` as it is kept: an edit is saved as it settles, and `↺` goes back.
#[component]
pub fn ViewPanel(view: View, defined: View) -> Element {
	let ctx = use_context::<Ctx>();
	let toaster = use_toaster();
	let mut view = use_signal(|| view);
	use_context_provider(|| InView(view));
	let mut sent = use_signal(|| view.peek().clone());
	let mut pending = use_signal(|| None::<dioxus::core::Task>);
	use_effect(move || {
		let now = view.read().clone();
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
	let hits = Query::from_str(&v.query).and_then(|q| {
		let mut hits = q.run(&board.cards, ctx.me().as_deref())?;
		v.sort.apply(&board.cards, &mut hits);
		Ok((q, hits))
	});
	let save = EventHandler::new(move |name: String| {
		let view = View {
			name: name.clone(),
			..view.read().clone()
		};
		spawn(async move {
			if ctx.keep(LibraryEdit::Keep(Kept::Filter(view))).await {
				toaster.positive(format!("Saved {name} to your views"));
			}
		});
	});
	let mine: Vec<String> = ctx.library.read().as_ref().expect("views render once the library arrived").filters.keys().cloned().collect();
	let sorts: Vec<Sort> = SortKey::iter().flat_map(|by| [false, true].map(|reversed| Sort { by, reversed })).collect();
	rsx! {
		div { class: "flex h-full flex-col gap-2 p-2",
			div { class: "flex flex-wrap items-center gap-2",
				Input {
					class: "min-w-64 flex-1 font-mono",
					value: v.query.clone(),
					placeholder: "stage:triage,selected repo:EV-Service-Arb/* -t:chore i:>=5,none text",
					oninput: move |e: FormEvent| view.write().query = e.value(),
				}
				Choice { value: v.layout, options: Layout::iter().collect(), on_change: move |l| view.write().layout = l }
				Choice { value: v.group_by, options: GroupBy::iter().collect(), on_change: move |g| view.write().group_by = g }
				Choice { value: v.sort, options: sorts, on_change: move |s| view.write().sort = s }
				if v != defined {
					Button {
						variant: ButtonVariant::Ghost,
						size: Size::Sm,
						r#type: "button",
						title: "back to {defined.name} as kept",
						onclick: { let defined = defined.clone(); move |_| view.set(defined.clone()) },
						"↺"
					}
				}
				super::library::Menu { shelf: Shelf::Filters, names: mine, on_pick: move |name: String| ctx.open(PanelId(format!("view:{name}"))), on_save: Some(save) }
			}
			match hits {
				Err(e) => rsx! { Badge { variant: BadgeVariant::Error, "{e}" } },
				Ok((query, hits)) => {
					let groups = group(&board.cards, &hits, v.group_by);
					rsx! {
						FacetBar { query: query.clone() }
						match v.layout {
							Layout::Board => rsx! { Columns { key: "{v.group_by}", view: v.name.clone(), groups, by: v.group_by, shown: v.shown.clone() } },
							Layout::Table => rsx! { Rows { groups, shown: v.shown.clone(), sort: v.sort, query } },
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
}

/// One of a closed set (layout, grouping, order), shown by its name.
#[component]
fn Choice<T: std::fmt::Display + std::str::FromStr + Copy + PartialEq + 'static>(value: T, options: Vec<T>, on_change: EventHandler<T>) -> Element {
	rsx! {
		Select {
			value: value.to_string(),
			on_value_change: move |s: String| on_change.call(T::from_str(&s).ok().expect("offered from the same set")),
			SelectTrigger { size: Size::Sm, SelectValue {} }
			SelectContent {
				for option in options {
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
	tints: Signal<Tints>,
}

/// Columns, one per group, each a tile of a dock nested in the view's: resized, reordered, tabbed together,
/// kept per view and grouping in the viewer's library. Grouped by stage, a card dropped on a column moves there.
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
		by,
		shown: Signal::new(shown.clone()),
		dragged: Signal::new(None),
		tints: Signal::new(tints.clone()),
	});
	let mut panels = use_signal(Vec::<DockPanel>::new);
	let mut api = use_signal(|| None::<PackedApi>);
	let mut arranged = use_signal(|| None::<Band>);
	super::use_autosave(ctx, api, arranged, Dock::Columns { view: view.clone(), by });
	// `layout`'s columns hosted, then shown; the effect below adds any group it lacks
	let show = move |mut a: PackedApi, layout: &str| -> bool {
		let grid = match tilers_dioxus::model::serial::load(layout) {
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
		a.load(layout).expect("parsed above");
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
		let view = view.clone();
		move |tints: Tints| {
			let edit = LibraryEdit::Tinted { view: view.clone(), by, tints };
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
									style: choice.map_or_else(String::new, |t| format!("background-color: {}", hue(t))),
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
				names: kept,
				on_pick: { let apply = apply.clone(); move |name: String| apply(name) },
				on_save: Some(EventHandler::new(move |name: String| {
					let a = (*api.peek()).expect("the column dock entered a band before its menu shows");
					let edit = LibraryEdit::Keep(Kept::Columns { name, by, band: a.band(), layout: a.save(), tints: lanes.tints.peek().clone() });
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
							let kept = ctx.library.peek().as_ref().and_then(|l| l.on_screen.columns.get(&view)?.get(&by)?.get(&band).cloned());
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
	let tint = lanes.tints.read().get(&group).copied();
	// `--card` can't be mixed from itself on one element: the column mixes it, each card takes the mix
	let (style, card_style) = match tint {
		Some(t) => (
			format!(
				"background-color: color-mix(in srgb, {0} 4%, transparent); --tinted-card: color-mix(in srgb, {0} 14%, var(--card))",
				hue(t)
			),
			"--card: var(--tinted-card)",
		),
		None => (String::new(), ""),
	};
	rsx! {
		div {
			class: "flex h-full flex-col gap-2 overflow-y-auto p-1",
			style,
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
					style: card_style,
					ondragstart: { let link = cards[i].link.clone(); move |_| dragged.set(Some(link.clone())) },
					CardView { card: cards[i].clone(), shown: shown.clone() }
				}
			}
		}
	}
}

/// Notion's palette, as its dark theme writes text: a column takes a little of it, its cards more.
fn hue(tint: Tint) -> &'static str {
	match tint {
		Tint::Gray => "#9b9b9b",
		Tint::Brown => "#ba856f",
		Tint::Orange => "#c77d48",
		Tint::Yellow => "#ca9849",
		Tint::Green => "#529e72",
		Tint::Blue => "#379ad3",
		Tint::Purple => "#9d68d3",
		Tint::Pink => "#d15796",
		Tint::Red => "#df5452",
	}
}

#[component]
fn Rows(groups: Vec<(String, Vec<usize>)>, shown: Vec<Shown>, sort: Sort, query: Query) -> Element {
	let ctx = use_context::<Ctx>();
	let mut in_view = use_context::<InView>();
	// natural, then reversed, then back to rank
	let mut cycle = move |by: SortKey| {
		in_view.0.write().sort = match sort {
			Sort { by: at, reversed: false } if at == by => Sort { by, reversed: true },
			Sort { by: at, reversed: true } if at == by => Sort { by: SortKey::Rank, reversed: false },
			_ => Sort { by, reversed: false },
		}
	};
	let arrow = move |by: SortKey| match sort {
		Sort { by: at, reversed } if at == by =>
			if reversed {
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
							button { r#type: "button", class: "hover:text-ink", onclick: move |_| cycle(SortKey::Title), "Issue{arrow(SortKey::Title)}" }
						}
						for s in shown.iter().copied() {
							TableHead { key: "{s}",
								span { class: "inline-flex items-center gap-0.5",
									match SortKey::try_from(s) {
										Ok(by) => rsx! { button { r#type: "button", class: "hover:text-ink", onclick: move |_| cycle(by), "{s}{arrow(by)}" } },
										Err(_) => rsx! { "{s}" },
									}
									if let Ok(facet) = Facet::try_from(s) {
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
									TableCell { Shows { card: cards[i].clone(), shown: *s } }
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
					Shows { card: card.clone(), shown: s }
				}
				if card.conflicted { Badge { variant: BadgeVariant::Error, "clash" } }
			}
		}
	}
}

#[component]
fn Shows(card: Card, shown: Shown) -> Element {
	let ctx = use_context::<Ctx>();
	match shown {
		Shown::Repo => rsx! { Pick { facet: Facet::Repo, value: card.link.project().to_string(), "{card.link.project().repo()}" } },
		Shown::Assignees => rsx! { for a in card.assignees.iter() { Pick { facet: Facet::Assignee, value: a.clone(), "@{a}" } } },
		Shown::Stage => rsx! { Pick { facet: Facet::Stage, value: card.stage.to_string(), Badge { "{card.stage}" } } },
		Shown::Type => {
			let color = card.kind.and_then(|k| label_color(&ctx, &card, &k.to_string()));
			rsx! { if let Some(k) = card.kind { Pick { facet: Facet::Type, value: k.to_string().trim_start_matches("t:").to_string(), Chip { color, variant: BadgeVariant::Info, "{k}" } } } }
		}
		Shown::Labels => {
			let own_facet = |l: &String| {
				let scheme = Scheme::from(std::slice::from_ref(l));
				scheme.kind.is_some() || scheme.importance.is_some() || scheme.hours.is_some() || scheme.pin.is_some()
			};
			let labels: Vec<(String, Option<Color>)> = card.labels.iter().filter(|l| !own_facet(l)).map(|l| (l.clone(), label_color(&ctx, &card, l))).collect();
			rsx! { for (l, color) in labels { Pick { facet: Facet::Label, value: l.clone(), Chip { color, "{l}" } } } }
		}
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

fn label_color(ctx: &Ctx, card: &Card, label: &str) -> Option<Color> {
	let board = ctx.board.read();
	let colors = &board.as_ref().expect("cards render from the board").colors;
	colors.get(&card.link.project().to_string())?.get(label).copied()
}

/// A label in its GitHub colour, as GitHub's dark theme wears one: a faint fill and edge, the text lifted
/// toward white as far as the colour is dark.
#[component]
fn Chip(color: Option<Color>, #[props(default)] variant: BadgeVariant, children: Element) -> Element {
	let Some(hex) = color else {
		return rsx! { Badge { variant, {children} } };
	};
	let Color([r, g, b]) = hex;
	let perceived = (0.2126 * f32::from(r) + 0.7152 * f32::from(g) + 0.0722 * f32::from(b)) / 255.;
	let lift = ((0.6 - perceived) * 100.).max(0.);
	rsx! {
		span {
			class: "inline-flex h-5 items-center rounded-[var(--chip-radius)] border px-1.5 text-caption font-medium whitespace-nowrap",
			style: "background-color: #{hex}2e; border-color: hsl(from #{hex} h s calc(l + {lift}) / 0.3); color: hsl(from #{hex} h s calc(l + {lift}))",
			{children}
		}
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
	let (prefix, facet, value, counts_as) = match axis {
		Axis::Importance => ("i", Facet::Importance, card.importance.map(u16::from), Some(3)), // `labels.nix`: "`i:3` is the default"
		Axis::Hours => ("h", Facet::Hours, card.hours, None),
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
			Pick { facet, value: value.map_or_else(|| "none".to_string(), |v| v.to_string()), "{prefix}[{shown}]" }
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
