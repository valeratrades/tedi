//! A view's toolbar (Figma: ViewToolbar): its grouping, the fields a card shows, the columns shown; counts what the
//! boxes show, and keeps the view.

use std::collections::BTreeSet;

use dioxus::{core::spawn_forever, prelude::*};
use ev_lib::uikit_dioxus::Checkbox;
use tedi_core::board::{GroupBy, Shown};

use super::{
	Ctx,
	icons::{Glyph, Icon},
	kit::{Btn, MenuItem, Picker, Style},
	library::NameForm,
};
use crate::api::{Kept, LibraryEdit, View, ViewPart};

/// `view` as last edited, against how it is kept; `groups` are those it could show; no `count`: what it shows isn't here yet.
#[component]
pub(super) fn ViewToolbar(view: View, defined: View, groups: Vec<String>, count: Option<usize>, edit: Callback<ViewPart>) -> Element {
	rsx! {
		div { class: "flex shrink-0 flex-wrap items-center gap-2 border-b border-border-subtle bg-card px-3 py-2",
			Picker { glyph: Glyph::Group, label: view.group_by.to_string(), GroupMenu { by: view.group_by.clone(), edit } }
			Picker { glyph: Glyph::Fields, label: format!("{} fields", view.shown.len()), FieldsMenu { shown: view.shown.clone(), edit } }
			ColumnsPicker { groups, hidden: view.boxes.hidden.clone(), edit }
			div { class: "flex-1" }
			span { class: "shrink-0 px-1 text-mono-s text-ink-soft", if let Some(count) = count { "{count} cards" } }
			tilers_dioxus::LockToggle {}
			Keep { view, defined }
		}
	}
}

#[component]
pub(super) fn GroupMenu(by: GroupBy, edit: Callback<ViewPart>) -> Element {
	let ctx = use_context::<Ctx>();
	rsx! {
		for g in ctx.groupings() {
			MenuItem { key: "{g}", selected: g == by, label: g.to_string(), onclick: move |_| edit.call(ViewPart::GroupBy(g.clone())) }
		}
	}
}

/// Which groups have a box.
#[component]
pub(super) fn ColumnsPicker(groups: Vec<String>, hidden: BTreeSet<String>, edit: Callback<ViewPart>) -> Element {
	let gone: Vec<String> = hidden.iter().filter(|h| !groups.contains(h)).cloned().collect(); // hidden, and no card is in it now
	let shown = groups.iter().filter(|g| !hidden.contains(*g)).count();
	let label = if shown == groups.len() {
		format!("{shown} columns")
	} else {
		format!("{shown} of {}", groups.len())
	};
	rsx! {
		Picker { glyph: Glyph::Columns, label,
			for group in groups.into_iter().chain(gone) {
				label { key: "{group}", class: "flex h-7 shrink-0 items-center gap-2 rounded-[var(--control-radius)] px-2 hover:bg-hover",
					Checkbox {
						checked: !hidden.contains(&group),
						on_checked_change: {
							let (group, hidden) = (group.clone(), hidden.clone());
							move |on: bool| {
								let mut hidden = hidden.clone();
								match on {
									true => _ = hidden.remove(&group),
									false => _ = hidden.insert(group.clone()),
								}
								edit.call(ViewPart::Hidden(hidden));
							}
						},
					}
					span { class: "flex-1 truncate text-body text-ink", "{group}" }
				}
			}
		}
	}
}

/// Every field a card can show; the shown ones in their order, dragged to reorder.
#[component]
pub(super) fn FieldsMenu(shown: Vec<Shown>, edit: Callback<ViewPart>) -> Element {
	let ctx = use_context::<Ctx>();
	let hidden: Vec<Shown> = ctx.fields().filter(|s| !shown.contains(s)).collect();
	let mut dragged = use_signal(|| None::<usize>);
	let set = {
		let shown = shown.clone();
		move |change: &dyn Fn(&mut Vec<Shown>)| {
			let mut shown = shown.clone();
			change(&mut shown);
			edit.call(ViewPart::Shown(shown));
		}
	};
	rsx! {
		span { class: "px-2 pt-1.5 pb-0.5 text-overline text-ink-soft", "Shown · drag to order" }
		for (i , s) in shown.iter().cloned().enumerate() {
			div {
				key: "{s}",
				draggable: "true",
				ondragstart: move |_| dragged.set(Some(i)),
				ondragover: move |e| e.prevent_default(),
				ondrop: {
					let set = set.clone();
					move |e: DragEvent| {
						e.prevent_default();
						let Some(from) = dragged.take() else { return };
						set(&|shown| {
							let moved = shown.remove(from);
							shown.insert(i, moved);
						});
					}
				},
				class: "flex h-7 cursor-grab items-center gap-2 rounded-[var(--control-radius)] px-2 hover:bg-hover",
				Icon { glyph: Glyph::Grip, class: "size-3.5 text-ink-faint" }
				Checkbox { checked: true, on_checked_change: { let (s, set) = (s.clone(), set.clone()); move |_| set(&|shown| shown.retain(|x| *x != s)) } }
				span { class: "flex-1 text-body text-ink", "{s}" }
			}
		}
		if !hidden.is_empty() {
			div { class: "my-0.5 h-px bg-border-subtle" }
		}
		for s in hidden {
			label { key: "{s}", class: "flex h-7 items-center gap-2 rounded-[var(--control-radius)] px-2 pl-7.5 hover:bg-hover",
				Checkbox { checked: false, on_checked_change: { let (s, set) = (s.clone(), set.clone()); move |_| set(&|shown| shown.push(s.clone())) } }
				span { class: "flex-1 text-body text-ink-soft", "{s}" }
			}
		}
	}
}

/// Clean: "Save as…". Changed and the viewer's: modified, Revert, Save. Changed and the team's: the team copy
/// is read-only, so it is kept as the viewer's own.
#[component]
pub(super) fn Keep(view: View, defined: View) -> Element {
	let ctx = use_context::<Ctx>();
	let mut naming = use_signal(|| false);
	let mine = ctx.library.read().as_ref().expect("views render once the library arrived").views.contains_key(&defined.name);
	let dirty = view != defined;
	let save = use_callback(move |name: String| {
		naming.set(false);
		let kept = View { name: name.clone(), ..view.clone() };
		spawn(async move {
			if ctx.keep(LibraryEdit::Keep(Kept::View(kept))).await {
				ctx.say("saved", super::toast::Tone::Success, format!("Saved \"{name}\" in your views"), None);
				ctx.open(tilers_dioxus::PanelId(format!("view:{name}")));
			}
		});
	});
	if naming() {
		return rsx! {
			div { class: "w-56 shrink-0", NameForm { placeholder: if mine || !dirty { "save as…" } else { "save as mine…" }, on_name: move |name| save.call(name), on_cancel: move |()| naming.set(false) } }
		};
	}
	rsx! {
		div { class: "flex shrink-0 items-center gap-1",
			if dirty {
				span { class: "flex items-center gap-1 px-1 text-caption text-ink-soft",
					title: if mine { "Changed since you last saved it" } else { "The team's view changes only through a proposal: keep yours to save" },
					if mine {
						span { class: "size-1.5 rounded-full bg-accent-warn" }
						"modified"
					} else {
						Icon { glyph: Glyph::Lock, class: "size-3" }
						"team · read-only"
					}
				}
				Btn { style: Style::Ghost, glyph: Glyph::Revert, title: "Back to the view as kept", onclick: { let name = defined.name.clone(); move |_| { spawn_forever(ctx.revert(name.clone())); } }, "Revert" }
				Btn { style: Style::Outline, title: if mine { "Keep these settings as a new view of yours" } else { "Keep these settings as a view of yours; the team's stays as it is" }, onclick: move |_| naming.set(true), if mine { "Save as…" } else { "Save as mine" } }
				if mine {
					Btn { style: Style::Primary, title: "Overwrite your view with these settings", onclick: { let name = defined.name.clone(); move |_| save.call(name.clone()) }, "Save" }
				}
			} else {
				Btn { style: Style::Ghost, glyph: Glyph::Save, title: "Keep a copy of this view under a new name", onclick: move |_| naming.set(true), "Save as…" }
			}
		}
	}
}
