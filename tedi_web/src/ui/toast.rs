//! Outcomes, bottom-right (Figma: Toast): an accent bar for the tone, ink text, and the one action that
//! follows from it (Undo, View, Open, Resolve).

use dioxus::prelude::*;

use super::{
	Ctx,
	icons::{Glyph, Icon},
	kit::{Btn, IconBtn, Style},
};

/// How long a note stays, unless hovered.
const SHOWN_MS: u64 = 6_000;

#[derive(Clone, Copy, Debug, PartialEq)]
pub(super) enum Tone {
	Info,
	Success,
	Warn,
	Error,
}

#[derive(Clone, PartialEq)]
pub(super) struct Note {
	pub(super) id: u64,
	/// Notes of one key replace each other: a stepper's clicks leave one note, not a stack.
	pub(super) key: String,
	pub(super) tone: Tone,
	pub(super) text: String,
	pub(super) action: Option<(&'static str, Callback<()>)>,
}

#[component]
pub(super) fn Toasts() -> Element {
	let ctx = use_context::<Ctx>();
	let notes = ctx.notes.read().clone();
	rsx! {
		div { class: "pointer-events-none fixed right-4 bottom-4 z-[115] flex w-[420px] max-w-[calc(100vw-2rem)] flex-col gap-2",
			for note in notes {
				Toast { key: "{note.id}", note }
			}
		}
	}
}

#[component]
fn Toast(note: Note) -> Element {
	let ctx = use_context::<Ctx>();
	let mut hovered = use_signal(|| false);
	let id = note.id;
	use_future(move || async move {
		loop {
			gloo_timers::future::sleep(std::time::Duration::from_millis(SHOWN_MS)).await;
			if !*hovered.peek() {
				ctx.unsay(id);
				return;
			}
		}
	});
	let (bar, glyph) = match note.tone {
		Tone::Info => ("bg-accent-info-ink", Glyph::Sync),
		Tone::Success => ("bg-positive-ink", Glyph::Check),
		Tone::Warn => ("bg-accent-warn", Glyph::Alert),
		Tone::Error => ("bg-accent-error", Glyph::Alert),
	};
	rsx! {
		div {
			class: "pointer-events-auto flex min-h-10 items-center gap-2 rounded-[var(--radius)] border border-border bg-popover py-1.5 pr-2 pl-3",
			onmouseenter: move |_| hovered.set(true),
			onmouseleave: move |_| hovered.set(false),
			span { class: "h-5 w-[3px] shrink-0 rounded-sm {bar}" }
			Icon { glyph, class: "size-3.5 shrink-0 text-ink-soft" }
			span { class: "min-w-0 flex-1 text-body text-ink", "{note.text}" }
			if let Some((label, act)) = note.action {
				Btn { style: Style::Ghost, onclick: move |_| { act.call(()); ctx.unsay(id); }, "{label}" }
			}
			IconBtn { glyph: Glyph::X, title: "Dismiss", onclick: move |_| ctx.unsay(id) }
		}
	}
}
