//! An issue tab: its fields, each edited against the value it showed, and any clash on it, inline.

use std::str::FromStr as _;

use dioxus::prelude::*;
use ev_lib::uikit_dioxus::*;
use strum::IntoEnumIterator as _;
use tedi_core::{
	IssueLink,
	board::{Card, restage},
	conflict::{Conflict, Field, FieldEdit, list, sides},
	stage::Stage,
};

use super::{Ctx, view::CardView};
use crate::api;

#[component]
pub fn IssuePanel(link: IssueLink) -> Element {
	let ctx = use_context::<Ctx>();
	let Some(card) = ctx.card(&link) else {
		return rsx! { div { class: "p-3 text-sm", "{link} is not on the board: not stored here, or not loaded yet" } };
	};
	let conflict = ctx.conflicts.read().get(&link).cloned();
	let children: Vec<Card> = ctx
		.board
		.read()
		.as_ref()
		.expect("the card came from it")
		.cards
		.iter()
		.filter(|c| c.parent.as_ref() == Some(&link))
		.cloned()
		.collect();
	rsx! {
		div { class: "flex h-full flex-col gap-3 overflow-auto p-3 text-sm",
			if let Some(conflict) = conflict { Clashes { link: link.clone(), conflict } }
			if card.conflicted { Recorded { link: link.clone() } }
			div { class: "flex items-center gap-2",
				a { class: "text-ink-soft hover:underline", href: "{link}", target: "_blank", "{link.project()}#{link.number()}" }
				StagePicker { card: card.clone() }
			}
			Editable { link: link.clone(), field: Field::Title, current: card.title.clone(), multiline: false }
			Editable { link: link.clone(), field: Field::Labels, current: list(&card.labels), multiline: false }
			Editable { link: link.clone(), field: Field::Assignees, current: list(&card.assignees), multiline: false }
			if !card.blocked_by.is_empty() {
				div { class: "flex flex-wrap items-center gap-1",
					span { class: "text-ink-soft", "blocked by" }
					for b in card.blocked_by.clone() {
						button { class: "hover:underline", onclick: { let b = b.clone(); move |_| ctx.open_issue(b.clone()) }, "{b.shorthand(link.project())}" }
					}
				}
			}
			if !card.prs.is_empty() {
				div { class: "flex flex-wrap gap-1",
					for pr in card.prs.iter() { a { href: "{pr.url}", target: "_blank", Badge { "PR {pr.state}" } } }
				}
			}
			Editable { link: link.clone(), field: Field::Body, current: card.body.clone(), multiline: true }
			if !children.is_empty() {
				div { class: "flex flex-col gap-1",
					span { class: "text-ink-soft", "sub-issues" }
					for child in children {
						CardView { key: "{child.link}", card: child.clone(), shown: vec![tedi_core::board::Shown::Stage, tedi_core::board::Shown::Assignees] }
					}
				}
			}
		}
	}
}

/// A drop on another column, for when there is no board to drag on (a phone).
#[component]
fn StagePicker(card: Card) -> Element {
	let ctx = use_context::<Ctx>();
	let toaster = use_toaster();
	rsx! {
		Select {
			value: card.stage.to_string(),
			on_value_change: move |s: String| {
				let to = Stage::from_str(&s).expect("offered from `Stage`");
				match restage(&card.labels, to) {
					Ok((labels, state)) => {
						let mut edits = vec![FieldEdit { field: Field::Labels, seen: list(&card.labels), new: list(&labels) }];
						if state != card.state {
							edits.push(FieldEdit { field: Field::State, seen: card.state.to_checkbox_contents(), new: state.to_checkbox_contents() });
						}
						spawn(ctx.edit(card.link.clone(), edits));
					}
					Err(e) => _ = toaster.info(e.to_string()),
				}
			},
			SelectTrigger { size: Size::Sm, SelectValue {} }
			SelectContent {
				for stage in Stage::iter() {
					SelectItem { value: stage.to_string(), text_value: stage.to_string(), "{stage}" }
				}
			}
		}
	}
}

/// A field shown as it is; editing it remembers what it showed then, which the edit is made against.
#[component]
fn Editable(link: IssueLink, field: Field, current: String, multiline: bool) -> Element {
	let ctx = use_context::<Ctx>();
	let mut draft = use_signal(|| None::<(String, String)>);
	let Some((seen, text)) = draft() else {
		let shown = current.clone();
		return rsx! {
			div { class: "group flex items-start gap-2",
				span { class: "w-20 shrink-0 text-ink-soft", "{field}" }
				div { class: if multiline { "flex-1 whitespace-pre-wrap" } else { "flex-1" }, "{shown}" }
				Button { variant: ButtonVariant::Ghost, size: Size::Xs, r#type: "button", onclick: move |_| draft.set(Some((current.clone(), current.clone()))), "edit" }
			}
		};
	};
	let save = move |_| {
		let (seen, new) = draft.take().expect("saved from the draft");
		spawn(ctx.edit(link.clone(), vec![FieldEdit { field, seen, new }]));
	};
	let oninput = move |e: FormEvent| draft.set(Some((seen.clone(), e.value())));
	rsx! {
		div { class: "flex flex-col gap-1",
			span { class: "text-ink-soft", "{field}" }
			if multiline {
				Textarea { value: text, rows: 12, oninput }
			} else {
				Input { value: text, oninput }
			}
			div { class: "flex gap-1",
				Button { size: Size::Sm, r#type: "button", onclick: save, "Save" }
				Button { variant: ButtonVariant::Ghost, size: Size::Sm, r#type: "button", onclick: move |_| draft.set(None), "Cancel" }
			}
		}
	}
}

/// An edit that came back clashing: each field as it is now ("theirs"), as you set it ("yours"), and
/// what you saw. Nothing was written, so answering it is just another edit, made against theirs.
#[component]
fn Clashes(link: IssueLink, conflict: Conflict) -> Element {
	let ctx = use_context::<Ctx>();
	let mut merging = use_signal(|| None::<Vec<String>>);
	if !conflict.files.is_empty() {
		return rsx! { Recorded { link: link.clone() } }; // a pull clashed with the store: resolved through its file
	}
	let forget = {
		let link = link.clone();
		move |_| {
			let mut conflicts = ctx.conflicts;
			conflicts.write().remove(&link);
		}
	};
	let resend = {
		let (link, clashes) = (link.clone(), conflict.clashes.clone());
		move |values: Vec<String>| {
			let edits = clashes
				.iter()
				.zip(values)
				.map(|(c, new)| FieldEdit {
					field: c.field,
					seen: c.remote.clone(),
					new,
				})
				.collect();
			spawn(ctx.edit(link.clone(), edits));
		}
	};
	let yours: Vec<String> = conflict.clashes.iter().map(|c| c.local.clone()).collect();
	rsx! {
		div { class: "flex flex-col gap-2 rounded-md border border-accent-error p-2",
			span { class: "font-semibold", "Changed since you saw it" }
			for (i , c) in conflict.clashes.iter().enumerate() {
				div { key: "{c.field}", class: "grid grid-cols-[5rem_1fr] gap-1",
					span { class: "text-ink-soft", "{c.field}" }
					span {}
					span { class: "text-ink-soft", "theirs" }
					div { class: "whitespace-pre-wrap", "{c.remote}" }
					span { class: "text-ink-soft", "yours" }
					match merging() {
						Some(values) => rsx! {
							Textarea { value: values[i].clone(), rows: 4, oninput: move |e: FormEvent| merging.with_mut(|m| m.as_mut().expect("while merging")[i] = e.value()) }
						},
						None => rsx! { div { class: "whitespace-pre-wrap", "{c.local}" } },
					}
					if let Some(was) = &c.consensus {
						span { class: "text-ink-soft", "you saw" }
						div { class: "whitespace-pre-wrap text-ink-soft", "{was}" }
					}
				}
			}
			div { class: "flex flex-wrap gap-1",
				match merging() {
					Some(values) => rsx! {
						Button { size: Size::Sm, r#type: "button", onclick: { let resend = resend.clone(); move |_| resend(values.clone()) }, "Save merged" }
						Button { variant: ButtonVariant::Ghost, size: Size::Sm, r#type: "button", onclick: move |_| merging.set(None), "Back" }
					},
					None => rsx! {
						Button { variant: ButtonVariant::Outline, size: Size::Sm, r#type: "button", onclick: forget, "Keep theirs" }
						Button { variant: ButtonVariant::Outline, size: Size::Sm, r#type: "button", onclick: { let resend = resend.clone(); let yours = yours.clone(); move |_| resend(yours.clone()) }, "Overwrite" }
						Button { size: Size::Sm, r#type: "button", onclick: { let yours = yours.clone(); move |_| merging.set(Some(yours.clone())) }, "Edit merged" }
					},
				}
			}
		}
	}
}

/// A clash a pull recorded: the node as the store and GitHub hold it, between markers, in its file.
/// Writing a marker-free version there lands it for everyone.
#[component]
fn Recorded(link: IssueLink) -> Element {
	let ctx = use_context::<Ctx>();
	let toaster = ctx.toaster;
	let mut file = use_resource({
		let link = link.clone();
		move || api::pending_conflict(link.clone())
	});
	let mut editing = use_signal(|| None::<String>);
	let resolve = {
		let link = link.clone();
		move |resolution: String| {
			let link = link.clone();
			spawn(async move {
				match api::resolve(link.clone(), resolution).await {
					Ok(None) => {
						editing.set(None);
						let mut conflicts = ctx.conflicts;
						conflicts.write().remove(&link);
						toaster.positive("Resolved");
					}
					Ok(Some(_)) => _ = toaster.warn("Markers remain: it is still pending"),
					Err(e) => _ = toaster.error(e.to_string()),
				}
				file.restart();
			});
		}
	};
	let Some(loaded) = file() else {
		return rsx! { Spinner {} };
	};
	let content = match loaded {
		Ok(Some(content)) => content,
		Ok(None) => return rsx! {},
		Err(e) => return rsx! { Badge { variant: BadgeVariant::Error, "{e}" } },
	};
	let sides = sides(&content);
	rsx! {
		div { class: "flex flex-col gap-2 rounded-md border border-accent-error p-2",
			span { class: "font-semibold", "Changed here and on GitHub since they last agreed" }
			match editing() {
				Some(text) => rsx! {
					Textarea { class: "font-mono", value: text.clone(), rows: 16, oninput: move |e: FormEvent| editing.set(Some(e.value())) }
					div { class: "flex gap-1",
						Button { size: Size::Sm, r#type: "button", onclick: { let resolve = resolve.clone(); move |_| resolve(text.clone()) }, "Save merged" }
						Button { variant: ButtonVariant::Ghost, size: Size::Sm, r#type: "button", onclick: move |_| editing.set(None), "Back" }
					}
				},
				None => rsx! {
					pre { class: "max-h-64 overflow-auto whitespace-pre-wrap font-mono text-xs", "{content}" }
					div { class: "flex flex-wrap gap-1",
						if let Some((local, remote)) = sides {
							Button { variant: ButtonVariant::Outline, size: Size::Sm, r#type: "button", onclick: { let resolve = resolve.clone(); move |_| resolve(remote.clone()) }, "Keep theirs" }
							Button { variant: ButtonVariant::Outline, size: Size::Sm, r#type: "button", onclick: { let resolve = resolve.clone(); move |_| resolve(local.clone()) }, "Overwrite" }
						}
						Button { size: Size::Sm, r#type: "button", onclick: move |_| editing.set(Some(content.clone())), "Edit merged" }
					}
				},
			}
		}
	}
}
