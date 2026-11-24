//! Quick filters: a facet's chip opens its values with live counts; a pick rewrites the query, ⇧ rules a
//! value out, a chip's × drops its clause. They only ever rewrite the query string, so it stays the whole of
//! what a view shows. Importance and hours pick a bound (\`i:>=5\`, \`h:<=3\`), ⌥ the exact value.

use dioxus::prelude::*;
use ev_lib::uikit_dioxus::*;
use strum::IntoEnumIterator as _;
use tedi_core::board::{Facet, Query, Tally, View};

use super::{
	Ctx,
	icons::{Glyph, Icon},
	kit::{Btn, Style},
};

/// The view a card sits in, when it does.
#[derive(Clone, Copy)]
pub(super) struct InView(pub(super) Signal<View>);
impl InView {
	pub(super) fn narrow(mut self, edit: impl FnOnce(&mut Query)) {
		let mut query: Query = self.0.peek().query.parse().expect("values are offered only while their view's query parses");
		edit(&mut query);
		self.0.write().query = query.to_string();
	}

	fn toggle(self, facet: Facet, value: &str, exclude: bool) {
		self.narrow(|q| q.toggle(facet, value, exclude).expect("offered values are spelled as the query reads them"));
	}
}

/// A chip per facet, the ones the query narrows on first.
#[component]
pub(super) fn FacetBar(query: Query, wrap: bool, #[props(default)] children: Element) -> Element {
	let facets: Vec<Facet> = Facet::iter().collect();
	rsx! {
		div { class: if wrap { "flex min-w-0 flex-1 flex-wrap items-center gap-1" } else { "flex min-w-0 shrink-0 items-center gap-1 overflow-x-auto [scrollbar-width:none]" },
			for facet in facets {
				FacetChip { key: "{facet}", facet, query: query.clone() }
			}
			{children}
		}
	}
}

/// \`state triage, selected ▾ ×\`: active, its values print inline and × drops the clause.
#[component]
pub(super) fn FacetChip(facet: Facet, query: Query) -> Element {
	let in_view = use_context::<InView>();
	let mut open = use_signal(|| false);
	let picked: Vec<String> = query
		.picked(facet)
		.into_iter()
		.map(|v| match (facet, v.strip_prefix('-')) {
			(Facet::State, out) => {
				let (sign, at) = out.map_or(("", v.as_str()), |at| ("-", at));
				format!("{sign}{}", at.split_once('/').map_or(at, |(_, bucket)| bucket)) // a bucket reads without its workflow
			}
			(Facet::Importance | Facet::Hours, _) => v.replace(">=", "≥ ").replace("<=", "≤ "),
			_ => v,
		})
		.collect();
	let active = !picked.is_empty();
	let pad = if active { "pr-1" } else { "pr-1.5" };
	let frame = match (active, open()) {
		(_, true) => "border-ring",
		(true, false) => "border-primary-border bg-selected",
		(false, false) => "border-border-subtle hover:bg-hover",
	};
	rsx! {
		Popover { open: open(), on_open_change: move |v| open.set(v),
			span { class: "inline-flex h-[22px] items-center gap-1 rounded-[var(--control-radius)] border pl-2 {pad} {frame}",
				PopoverTrigger { class: "inline-flex items-center gap-1", aria_label: format!("filter by {facet}"),
					span { class: if active || open() { "text-body-s font-medium text-ink" } else { "text-body-s font-medium text-ink-soft" }, "{facet}" }
					if active {
						span { class: "max-w-32 truncate text-mono-s text-primary-ink", {picked.join(", ")} }
					}
					Icon { glyph: Glyph::ChevronDown, class: "size-3 text-ink-soft" }
				}
				if active {
					button {
						r#type: "button",
						class: "text-ink-soft hover:text-ink",
						title: "drop the {facet} filter",
						onclick: move |_| in_view.narrow(|q| q.clear(facet)),
						Icon { glyph: Glyph::X, class: "size-3" }
					}
				}
			}
			PopoverContent { align: "start", class: "w-[260px] rounded-[var(--radius)] border border-border bg-popover p-1 text-ink",
				FacetMenu { facet, query }
			}
		}
	}
}

/// Every value the rest of the query leaves, with its count. Computed only while open.
#[component]
pub(super) fn FacetMenu(facet: Facet, query: Query) -> Element {
	let ctx = use_context::<Ctx>();
	let in_view = use_context::<InView>();
	let mut needle = use_signal(String::new);
	let board = ctx.board.read();
	let cards = &board.as_ref().expect("views render once the board loaded").cards;
	let within = match ctx.scope() {
		Some((_, Some(filter))) => filter.parse().expect("a scope's filter is a repo clause"),
		Some((path, None)) => unreachable!("a view under the gone scope {path} shows only that"),
		None => Query::default(),
	};
	let tallies = match query.tally(facet, &within, cards, ctx.me().as_deref()) {
		Ok(t) => t,
		Err(e) => return rsx! { span { class: "p-2 text-caption text-accent-error", "{e}" } },
	};
	// the direction its picked bound has, else the one the facet is mostly read in
	let held = query.picked(facet).into_iter().find_map(|v| [">=", "<="].into_iter().find(|d| v.starts_with(d)));
	let mut dir = use_signal(|| held.unwrap_or(if facet == Facet::Hours { "<=" } else { ">=" }));
	let bound = matches!(facet, Facet::Importance | Facet::Hours).then(|| dir());
	let flip = move |to: &'static str| {
		dir.set(to);
		let picked = in_view
			.0
			.peek()
			.query
			.parse::<Query>()
			.expect("a facet's menu is open only over a query that parses")
			.picked(facet)
			.into_iter()
			.find(|v| v.starts_with(">=") || v.starts_with("<="));
		if let Some(was) = picked {
			let at = format!("{to}{}", &was[2..]);
			in_view.narrow(|q| {
				q.clear(facet);
				q.toggle(facet, &at, false).expect("a bound the facet reads");
			});
		}
	};
	rsx! {
		div { class: "flex flex-col gap-0.5",
			if bound.is_some() {
				div { class: "flex items-center gap-0.5 rounded-[var(--control-radius)] border border-border-subtle p-0.5",
					for (d , label) in [(">=", "≥ at least"), ("<=", "≤ at most")] {
						button {
							key: "{d}",
							r#type: "button",
							class: if dir() == d { "h-6 flex-1 rounded-[var(--control-radius)] bg-active text-body-s font-medium text-ink" } else { "h-6 flex-1 rounded-[var(--control-radius)] text-body-s font-medium text-ink-soft hover:text-ink" },
							onclick: { let mut flip = flip; move |_| flip(d) },
							"{label}"
						}
					}
				}
			}
			div { class: if bound.is_some() { "hidden" } else { "flex h-7 items-center gap-1.5 rounded-[var(--control-radius)] border border-border-subtle px-2" },
				Icon { glyph: Glyph::Search, class: "size-3.5 text-ink-faint" }
				input {
					class: "min-w-0 flex-1 bg-transparent text-body text-ink outline-none placeholder:text-ink-faint",
					placeholder: "filter {facet}…",
					value: needle(),
					oninput: move |e| needle.set(e.value()),
				}
			}
			div { class: "flex max-h-72 flex-col gap-0.5 overflow-y-auto",
				match bound {
					Some(bound) => rsx! { Bounds { facet, bound, tallies, needle: needle() } },
					None => rsx! {
						if tallies.is_empty() {
							span { class: "px-2 py-1 text-caption text-ink-soft", "No values" }
						}
						for t in tallies.into_iter().filter(|t| t.value.to_lowercase().contains(&needle.read().to_lowercase())) {
							Value { key: "{t.value}", facet, value: t.value.clone(), label: t.value.clone(), count: t.count, picked: t.picked, bar: None }
						}
					},
				}
			}
			div { class: "my-0.5 h-px bg-border-subtle" }
			div { class: "flex items-center gap-2 px-2 py-1",
				span { class: "flex-1 text-caption text-ink-soft",
					match bound {
						Some(_) => format!("type {facet}:5 for exactly 5"),
						None => format!("⇧-click excludes  ·  -{facet}:"),
					}
				}
				Btn { style: Style::Ghost, onclick: move |_| in_view.narrow(|q| q.clear(facet)), "Clear" }
			}
		}
	}
}

/// \`≥ 5\` with how many reach it, over a bar of how many sit exactly there: the distribution behind the bound.
#[component]
fn Bounds(facet: Facet, bound: &'static str, tallies: Vec<Tally>, needle: String) -> Element {
	let mut exact: Vec<(u16, usize, Option<bool>)> = tallies.iter().filter_map(|t| Some((t.value.parse().ok()?, t.count, t.picked))).collect();
	exact.sort_by_key(|(v, ..)| *v);
	if bound == ">=" {
		exact.reverse(); // the most important first, each bound counting what reaches it
	}
	let most = exact.iter().map(|(_, n, _)| *n).max().unwrap_or(1).max(1); // no value tallied: no bar to scale
	let mut reached = 0;
	let rows: Vec<(String, String, usize, f32, Option<bool>)> = exact
		.into_iter()
		.map(|(v, n, _)| {
			reached += n;
			let value = format!("{bound}{v}");
			let picked = tallies.iter().find(|t| t.value == value).and_then(|t| t.picked);
			(value, format!("{} {v}", if bound == ">=" { "≥" } else { "≤" }), reached, n as f32 / most as f32, picked)
		})
		.collect();
	let unset = tallies.iter().find(|t| t.value == "none").cloned();
	rsx! {
		for (value , label , count , bar , picked) in rows.into_iter().filter(|r| r.1.contains(&needle)) {
			Value { key: "{value}", facet, value, label, count, picked, bar: Some(bar) }
		}
		if let Some(t) = unset {
			Value { facet, value: "none", label: "also unset", count: t.count, picked: t.picked, bar: None }
		}
	}
}

//#[codestyle::skip(swappable-args)]
#[component]
fn Value(facet: Facet, value: String, label: String, count: usize, picked: Option<bool>, bar: Option<f32>) -> Element {
	let in_view = use_context::<InView>();
	rsx! {
		button {
			r#type: "button",
			class: if picked.is_some() { "relative flex h-7 items-center gap-2 rounded-[var(--control-radius)] bg-selected px-2 text-left" } else { "relative flex h-7 items-center gap-2 rounded-[var(--control-radius)] px-2 text-left hover:bg-hover" },
			onclick: move |e: MouseEvent| {
				match bar {
					Some(_) if picked != Some(true) => in_view.narrow(|q| {
						q.clear(facet);
						q.toggle(facet, &value, false).expect("a bound the facet reads");
					}),
					Some(_) => in_view.narrow(|q| q.clear(facet)),
					None => in_view.toggle(facet, &value, e.modifiers().shift()),
				}
			},
			if let Some(bar) = bar {
				span { class: "absolute inset-y-1 left-0 rounded-r-sm bg-hover", style: "width: {bar * 100.}%" }
			}
			span { class: "relative flex w-3.5 justify-center",
				match picked {
					Some(true) => rsx! { Icon { glyph: Glyph::Check, class: "size-3.5 text-primary-ink" } },
					Some(false) => rsx! { Icon { glyph: Glyph::Blocker, class: "size-3.5 text-accent-error" } },
					None => rsx! { Icon { glyph: Glyph::Check, class: "size-3.5 text-ink-faint opacity-0" } },
				}
			}
			span { class: if picked == Some(false) { "relative flex-1 truncate text-body text-ink line-through" } else { "relative flex-1 truncate text-body text-ink" }, "{label}" }
			span { class: "relative text-mono-s text-ink-soft tabular-nums", "{count}" }
		}
	}
}

/// A value a card shows, which narrows its view to it when it sits in one.
#[component]
pub(super) fn Pick(facet: Facet, value: String, children: Element) -> Element {
	match try_use_context::<InView>() {
		None => rsx! { {children} },
		Some(in_view) => rsx! {
			button {
				r#type: "button",
				class: "cursor-pointer",
				title: "{facet}:{value} · ⇧: rule out",
				onclick: move |e: MouseEvent| {
					e.stop_propagation();
					in_view.toggle(facet, &value, e.modifiers().shift());
				},
				{children}
			}
		},
	}
}
