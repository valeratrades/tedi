//! The design's controls where the kit's differ from them (Figma: Button, IconButton, Kbd, Select, MenuItem):
//! u2 sizes (24 and 28 px), outline without a fill.

use dioxus::prelude::*;
use ev_lib::uikit_dioxus::{Popover, PopoverContent, PopoverTrigger};

use super::icons::{Glyph, Icon};

#[derive(Clone, Copy, Debug, PartialEq)]
pub(super) enum Style {
	/// The one commit action on a surface.
	Primary,
	Outline,
	/// Toolbar and inline.
	Ghost,
	/// Destructive.
	Danger,
}

#[component]
pub(super) fn Btn(
	style: Style,
	#[props(default)] md: bool,
	glyph: Option<Glyph>,
	#[props(default)] disabled: bool,
	#[props(default)] title: String,
	onclick: EventHandler<MouseEvent>,
	children: Element,
) -> Element {
	let face = match style {
		Style::Primary => "bg-primary text-on-primary hover:bg-primary-hover",
		Style::Outline => "border border-border-strong text-ink hover:bg-hover",
		Style::Ghost => "text-ink-mid hover:bg-hover hover:text-ink",
		Style::Danger => "border border-chip-error-border text-accent-error hover:bg-hover",
	};
	let size = if md {
		"h-7 gap-1.5 px-3 text-body font-medium"
	} else {
		"h-6 gap-1 px-2 text-body-s font-medium"
	};
	rsx! {
		button {
			r#type: "button",
			class: "inline-flex shrink-0 items-center justify-center rounded-[var(--control-radius)] whitespace-nowrap disabled:pointer-events-none disabled:opacity-50 {size} {face}",
			disabled,
			title,
			onclick: move |e| onclick.call(e),
			if let Some(glyph) = glyph {
				Icon { glyph }
			}
			{children}
		}
	}
}

#[component]
pub(super) fn IconBtn(glyph: Glyph, title: String, #[props(default)] active: bool, onclick: EventHandler<MouseEvent>) -> Element {
	rsx! {
		button {
			r#type: "button",
			class: if active { "inline-flex size-6 shrink-0 items-center justify-center rounded-[var(--control-radius)] bg-active text-ink" } else { "inline-flex size-6 shrink-0 items-center justify-center rounded-[var(--control-radius)] text-ink-soft hover:bg-hover hover:text-ink" },
			title: "{title}",
			aria_label: "{title}",
			onclick: move |e: MouseEvent| {
				e.stop_propagation();
				onclick.call(e);
			},
			Icon { glyph }
		}
	}
}

#[component]
pub(super) fn Kbd(children: Element) -> Element {
	rsx! {
		kbd { class: "inline-flex h-4 shrink-0 items-center rounded-[var(--control-radius)] border border-border bg-secondary px-1 text-mono-s text-ink-soft", {children} }
	}
}

/// A select whose leading icon names its axis, so a row of them reads without labels.
#[component]
pub(super) fn Picker(glyph: Glyph, label: String, children: Element) -> Element {
	let mut open = use_signal(|| false);
	rsx! {
		Popover { open: open(), on_open_change: move |v| open.set(v),
			PopoverTrigger {
				class: if open() { "flex h-7 items-center gap-1.5 rounded-[var(--control-radius)] border border-ring px-2 text-body text-ink" } else { "flex h-7 items-center gap-1.5 rounded-[var(--control-radius)] border border-border px-2 text-body text-ink hover:bg-hover" },
				Icon { glyph, class: "size-4 text-ink-soft" }
				"{label}"
				Icon { glyph: Glyph::ChevronDown, class: "size-3.5 text-ink-soft" }
			}
			PopoverContent { align: "start", class: "flex max-h-96 min-w-56 flex-col gap-0.5 overflow-y-auto rounded-[var(--radius)] border border-border bg-popover p-1 text-ink",
				{children}
			}
		}
	}
}

/// A menu row: label, and a count or shortcut on the right.
#[component]
pub(super) fn MenuItem(selected: bool, label: String, #[props(default)] meta: String, glyph: Option<Glyph>, onclick: EventHandler<MouseEvent>) -> Element {
	rsx! {
		button {
			r#type: "button",
			class: if selected { "flex h-7 shrink-0 items-center gap-2 rounded-[var(--control-radius)] bg-selected px-2 text-left" } else { "flex h-7 shrink-0 items-center gap-2 rounded-[var(--control-radius)] px-2 text-left hover:bg-hover" },
			onclick: move |e| onclick.call(e),
			match glyph {
				Some(glyph) => rsx! { Icon { glyph, class: "size-4 text-ink-soft" } },
				None => rsx! { span { class: "flex w-3.5 justify-center", if selected { Icon { glyph: Glyph::Check, class: "size-3.5 text-primary-ink" } } } },
			}
			span { class: "flex-1 truncate text-body text-ink", "{label}" }
			if !meta.is_empty() {
				span { class: "text-mono-s text-ink-soft", "{meta}" }
			}
		}
	}
}
