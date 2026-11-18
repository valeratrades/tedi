//! Quick filters: a value's click includes it in its view's query, alt-click rules it out, again drops it.
//! They only ever rewrite the query string, so it stays the whole of what a view shows.

use dioxus::prelude::*;
use ev_lib::uikit_dioxus::*;
use strum::IntoEnumIterator as _;
use tedi_core::board::{Facet, Query, View};

use super::Ctx;

/// The view a card sits in, when it does.
#[derive(Clone, Copy)]
pub(super) struct InView(pub(super) Signal<View>);
impl InView {
	fn narrow(mut self, edit: impl FnOnce(&mut Query)) {
		let mut query: Query = self.0.peek().query.parse().expect("values are offered only while their view's query parses");
		edit(&mut query);
		self.0.write().query = query.to_string();
	}

	fn toggle(self, facet: Facet, value: &str, exclude: bool) {
		self.narrow(|q| q.toggle(facet, value, exclude).expect("offered values are spelled as the query reads them"));
	}
}

/// One menu per facet, under the query.
#[component]
pub(super) fn FacetBar(query: Query) -> Element {
	rsx! {
		div { class: "flex flex-wrap items-center gap-1 text-xs",
			for facet in Facet::iter() {
				FacetMenu { key: "{facet}", facet, query: query.clone(), label: facet.to_string() }
			}
		}
	}
}

/// `state▾`: the values the view's other clauses leave, each with its count; `×` when the query narrows on it.
#[component]
pub(super) fn FacetMenu(facet: Facet, query: Query, #[props(default)] label: String) -> Element {
	let in_view = use_context::<InView>();
	let mut open = use_signal(|| false);
	let active = {
		let mut lifted = query.clone();
		lifted.clear(facet);
		lifted != query
	};
	rsx! {
		span { class: "inline-flex items-center",
			Popover { open: open(), on_open_change: move |v| open.set(v),
				PopoverTrigger {
					class: if active { "rounded px-1 font-semibold text-ink bg-surface-sunken" } else { "rounded px-1 text-ink-soft hover:text-ink" },
					aria_label: format!("filter by {facet}"),
					"{label}▾"
				}
				PopoverContent { align: "start", class: "flex max-h-80 w-60 flex-col overflow-y-auto p-1 text-sm font-normal",
					Values { facet, query: query.clone() }
				}
			}
			if active {
				button { r#type: "button", class: "px-0.5 text-ink-soft hover:text-ink", title: "drop the {facet} filter", onclick: move |_| in_view.narrow(|q| q.clear(facet)), "×" }
			}
		}
	}
}

/// Computed only while its menu is open.
#[component]
fn Values(facet: Facet, query: Query) -> Element {
	let ctx = use_context::<Ctx>();
	let in_view = use_context::<InView>();
	let board = ctx.board.read();
	let cards = &board.as_ref().expect("views render once the board loaded").cards;
	let tallies = match query.tally(facet, cards, ctx.me().as_deref()) {
		Ok(t) => t,
		Err(e) => return rsx! { span { class: "p-1 text-xs text-accent-error", "{e}" } },
	};
	rsx! {
		if tallies.is_empty() {
			span { class: "p-1 text-xs text-ink-soft", "No values" }
		}
		for t in tallies {
			button {
				key: "{t.value}",
				r#type: "button",
				class: "flex items-center gap-2 rounded px-1 py-0.5 text-left hover:bg-surface-sunken",
				title: "click: only these · alt-click: rule out",
				onclick: { let value = t.value.clone(); move |e: MouseEvent| in_view.toggle(facet, &value, e.modifiers().alt()) },
				span { class: "w-3 text-center",
					match t.picked { Some(true) => "✓", Some(false) => "⊘", None => "" }
				}
				span { class: if t.picked == Some(false) { "flex-1 truncate line-through" } else { "flex-1 truncate" }, "{t.value}" }
				span { class: "text-xs tabular-nums text-ink-soft", "{t.count}" }
			}
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
				class: "cursor-pointer hover:underline",
				title: "{facet}:{value} · alt-click: rule out",
				onclick: move |e: MouseEvent| {
					e.stop_propagation();
					in_view.toggle(facet, &value, e.modifiers().alt());
				},
				{children}
			}
		},
	}
}
