//! An issue tab (Figma: IssuePanel): every row edits in place, against the value it showed; a clash shows
//! both values with three ways out. A pull request's tab only shows it.

use dioxus::prelude::*;
use ev_lib::uikit_dioxus::{Spinner, Textarea};
use tedi_core::{
	IssueLink,
	board::{Card, Shown},
	conflict::{Conflict, Field as EditedField, FieldEdit, list, sides},
};

use super::{
	Ctx,
	card::{Avatar, CardView, Field, ago},
	icons::{Glyph, Icon},
	kit::{Btn, IconBtn, MenuItem, Picker, Style},
};
use crate::api;

#[component]
pub fn IssuePanel(link: IssueLink) -> Element {
	let ctx = use_context::<Ctx>();
	let Some(card) = ctx.card(&link) else {
		return rsx! { div { class: "p-4 text-body text-ink-soft", "{link} is not on the board: not stored here, or not loaded yet" } };
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
	let body = card.body.split("\n# Blockers").next().expect("split yields at least one piece").trim_end().to_string(); // the blockers show as their own field
	let editable = card.pr.is_none();
	rsx! {
		div { class: "flex h-full flex-col gap-3 overflow-auto bg-card px-4 py-3",
			div { class: "flex h-6 shrink-0 items-center gap-1.5",
				Icon { glyph: if card.pr.is_some() { Glyph::Pr } else { Glyph::Issue }, class: "size-3.5 text-positive-ink" }
				a { class: "truncate text-mono-s text-ink-soft hover:underline", href: "{link}", target: "_blank", "{link.project()}#{link.number()}" }
				div { class: "flex-1" }
				IconBtn { glyph: Glyph::Link, title: "Copy its link", onclick: { let link = link.clone(); move |_| copy(ctx, link.to_string()) } }
			}
			if editable {
				Title { link: link.clone(), current: card.title.clone() }
			} else {
				h2 { class: "text-h2 text-ink", "{card.title}" }
			}
			if let Some(conflict) = conflict { Clashes { link: link.clone(), conflict } }
			if card.conflicted { Recorded { link: link.clone() } }
			div { class: "flex flex-wrap items-center gap-2", StatePicker { card: card.clone() } }
			div { class: "flex flex-col",
				if let Some(pr) = &card.pr {
					Row { label: "author", span { class: "flex items-center gap-1.5 text-body text-ink", Avatar { login: pr.author.clone() } "{pr.author}" } }
					Row { label: "closes",
						for issue in pr.closes.clone() {
							button { key: "{issue}", r#type: "button", class: "text-body text-accent-info-ink hover:underline", onclick: { let issue = issue.clone(); move |_| ctx.open_issue(issue.clone()) }, "{issue.shorthand(link.project())}" }
						}
					}
				} else {
					Row { label: "importance", Field { card: card.clone(), shown: Shown::Importance } }
					Row { label: "hours", Field { card: card.clone(), shown: Shown::Hours } }
				}
				Row { label: "labels",
					Field { card: card.clone(), shown: Shown::Type }
					Field { card: card.clone(), shown: Shown::Labels }
					if editable { AddLabel { card: card.clone() } }
				}
				Row { label: "assignees",
					if editable {
						Editable { link: link.clone(), field: EditedField::Assignees, current: list(&card.assignees), multiline: false,
							span { class: "flex flex-wrap items-center gap-2",
								if card.assignees.is_empty() { span { class: "text-body text-ink-faint", "nobody" } }
								for a in card.assignees.iter() { span { key: "{a}", class: "flex items-center gap-1.5 text-body text-ink", Avatar { login: a.clone() } "{a}" } }
							}
						}
					} else {
						for a in card.assignees.iter() { span { key: "{a}", class: "flex items-center gap-1.5 text-body text-ink", Avatar { login: a.clone() } "{a}" } }
					}
				}
				if !card.blocked_by.is_empty() {
					Row { label: "blockers",
						for b in card.blocked_by.clone() {
							button {
								key: "{b}",
								r#type: "button",
								class: "flex min-w-0 max-w-full items-center gap-1 text-body text-accent-error hover:underline",
								onclick: { let b = b.clone(); move |_| ctx.open_issue(b.clone()) },
								Icon { glyph: Glyph::Blocker, class: "size-3 shrink-0" }
								span { class: "truncate whitespace-nowrap",
									"{b.shorthand(link.project())}"
									if let Some(c) = ctx.card(&b) { " {c.title}" }
								}
							}
						}
					}
				}
				Row { label: "sprint", Field { card: card.clone(), shown: Shown::Sprint } }
				if !card.prs.is_empty() {
					Row { label: "linked PR",
						for pr in card.prs.iter() {
							a { key: "{pr.url}", class: "flex items-center gap-1 text-body text-accent-info-ink hover:underline", href: "{pr.url}", target: "_blank",
								Icon { glyph: Glyph::Pr, class: "size-3" }
								"#{pr.url.rsplit('/').next().expect(\"a url has a last segment\")} · {pr.state}"
							}
						}
					}
				}
			}
			if editable {
				Editable { link: link.clone(), field: EditedField::Body, current: card.body.clone(), multiline: true,
					if body.is_empty() { span { class: "text-body text-ink-faint", "No body" } } else { Markdown { text: body.clone() } }
				}
			} else {
				Markdown { text: body.clone() }
			}
			if let Some(at) = card.updated {
				span { class: "flex items-center gap-1.5 text-caption text-ink-faint", Icon { glyph: Glyph::Sync, class: "size-3" } "changed {ago(at)}" }
			}
			if !children.is_empty() {
				div { class: "flex flex-col gap-1.5",
					span { class: "text-overline text-ink-soft", "Sub-issues" }
					for child in children {
						CardView { key: "{child.link}", card: child.clone(), shown: ctx.workflows().into_keys().map(Shown::State).chain([Shown::Assignees]).collect::<Vec<_>>() }
					}
				}
			}
		}
	}
}

fn copy(ctx: Ctx, text: String) {
	ev_lib::uikit_dioxus::write_clipboard(&text.clone(), move |copied| match copied {
		true => ctx.say("copy", super::toast::Tone::Success, format!("Copied {text}"), None),
		false => ctx.say("copy", super::toast::Tone::Error, "the browser refused the clipboard", None),
	});
}

/// \`label · value\`, one property per row.
#[component]
fn Row(label: &'static str, children: Element) -> Element {
	rsx! {
		div { class: "flex min-h-[30px] items-center gap-2 border-b border-border-subtle py-1",
			span { class: "w-[88px] shrink-0 text-body-s text-ink-soft", "{label}" }
			div { class: "flex min-w-0 flex-1 flex-wrap items-center gap-1", {children} }
		}
	}
}

/// The title, edited in place against what it showed.
#[component]
fn Title(link: IssueLink, current: String) -> Element {
	let ctx = use_context::<Ctx>();
	let mut draft = use_signal(|| None::<(String, String)>);
	match draft() {
		None => rsx! {
			h2 {
				class: "cursor-text rounded-[var(--control-radius)] text-h2 text-ink",
				title: "Double-click to edit",
				ondoubleclick: { let current = current.clone(); move |_| draft.set(Some((current.clone(), current.clone()))) },
				"{current}"
			}
		},
		Some((seen, text)) => rsx! {
			input {
				class: "rounded-[var(--control-radius)] border border-ring bg-transparent px-1 text-h2 text-ink outline-none",
				value: text,
				autofocus: true,
				oninput: { let seen = seen.clone(); move |e: FormEvent| draft.set(Some((seen.clone(), e.value()))) },
				onblur: move |_| draft.set(None),
				onkeydown: move |e: KeyboardEvent| match e.key() {
					Key::Enter => {
						let (seen, new) = draft.take().expect("editing");
						if seen != new {
							spawn(ctx.edit(link.clone(), vec![FieldEdit { field: EditedField::Title, seen, new }]));
						}
					}
					Key::Escape => draft.set(None),
					_ => {}
				},
			}
		},
	}
}

/// \`+\`: a label typed in, added to what the card shows.
#[component]
fn AddLabel(card: Card) -> Element {
	let ctx = use_context::<Ctx>();
	let mut typing = use_signal(|| None::<String>);
	match typing() {
		None => rsx! { IconBtn { glyph: Glyph::Plus, title: "Add a label", onclick: move |_| typing.set(Some(String::new())) } },
		Some(text) => rsx! {
			input {
				class: "h-[18px] w-28 rounded-[var(--chip-radius)] border border-ring bg-transparent px-1.5 text-mono-s text-ink outline-none",
				value: text,
				autofocus: true,
				placeholder: "label",
				oninput: move |e: FormEvent| typing.set(Some(e.value())),
				onkeydown: move |e: KeyboardEvent| match e.key() {
					Key::Enter => {
						let label = typing.take().expect("typing").trim().to_string();
						if !label.is_empty() && !card.labels.contains(&label) {
							let mut labels = card.labels.clone();
							labels.push(label);
							spawn(ctx.edit(card.link.clone(), vec![FieldEdit { field: EditedField::Labels, seen: list(&card.labels), new: list(&labels) }]));
						}
					}
					Key::Escape => typing.set(None),
					_ => {}
				},
			}
		},
	}
}

/// One select per workflow the card sits in: \`kanban / triage ▾\`.
#[component]
fn StatePicker(card: Card) -> Element {
	let ctx = use_context::<Ctx>();
	rsx! {
		for (name , workflow) in ctx.workflows() {
			Picker { key: "{name}", glyph: if name == "pull_requests" { Glyph::Pr } else { Glyph::Board }, label: format!("{name} / {}", card.states[&name]),
				for leaf in workflow.leaves() {
					MenuItem {
						key: "{leaf}",
						selected: card.states[&name] == leaf,
						label: leaf.to_string(),
						onclick: {
							let (card, name, leaf) = (card.clone(), name.clone(), leaf.clone());
							move |_| {
								if card.states[&name] != leaf {
									spawn(ctx.move_card(card.clone(), name.clone(), leaf.clone()));
								}
							}
						},
					}
				}
			}
		}
	}
}

/// line: Enter) keeps the edit, Esc drops it.
#[component]
fn Editable(link: IssueLink, field: EditedField, current: String, multiline: bool, children: Element) -> Element {
	let ctx = use_context::<Ctx>();
	let mut draft = use_signal(|| None::<(String, String)>);
	let Some((seen, text)) = draft() else {
		let start = {
			let current = current.clone();
			move |_| draft.set(Some((current.clone(), current.clone())))
		};
		return rsx! {
			div { class: if multiline { "group/ed flex min-w-0 flex-col gap-1" } else { "group/ed flex min-w-0 flex-1 flex-col gap-1" },
				if multiline {
					div { class: "flex h-6 items-center",
						span { class: "flex-1 text-overline text-ink-soft", "{field}" }
						Btn { style: Style::Ghost, glyph: Glyph::Pencil, title: "or double-click it", onclick: start.clone(), "Edit" }
					}
					div { class: "cursor-text", title: "Double-click to edit", ondoubleclick: start, {children} }
				} else {
					div { class: "flex cursor-text items-center gap-1", title: "Double-click to edit", ondoubleclick: start, {children} }
				}
			}
		};
	};
	let mut save = move || {
		let (seen, new) = draft.take().expect("saved from the draft");
		if seen != new {
			spawn(ctx.edit(link.clone(), vec![FieldEdit { field, seen, new }]));
		}
	};
	let oninput = move |e: FormEvent| draft.set(Some((seen.clone(), e.value())));
	let onkeydown = {
		let mut save = save.clone();
		move |e: KeyboardEvent| match e.key() {
			Key::Enter if !multiline || e.modifiers().ctrl() || e.modifiers().meta() => {
				e.prevent_default();
				save();
			}
			Key::Escape => draft.set(None),
			_ => {}
		}
	};
	rsx! {
		div { class: "flex min-w-0 flex-1 flex-col gap-1.5",
			if multiline {
				span { class: "text-overline text-ink-soft", "{field}" }
				textarea { class: "min-h-48 rounded-[var(--control-radius)] border border-ring bg-transparent p-2 text-mono text-ink outline-none", value: text, autofocus: true, oninput, onkeydown }
			} else {
				input { class: "h-7 rounded-[var(--control-radius)] border border-ring bg-transparent px-2 text-body text-ink outline-none", value: text, autofocus: true, oninput, onkeydown }
			}
			div { class: "flex items-center gap-1",
				Btn { style: Style::Primary, onclick: move |_| save(), "Save" }
				Btn { style: Style::Ghost, onclick: move |_| draft.set(None), "Cancel" }
				span { class: "px-1 text-caption text-ink-faint", if multiline { "Ctrl+Enter keeps · Esc drops" } else { "Enter keeps · Esc drops" } }
			}
		}
	}
}

/// GitHub markdown, read. Raw HTML in it shows as text: a body is anyone's writing, never markup to run.
#[component]
fn Markdown(text: String) -> Element {
	use pulldown_cmark::{Event, Options, Parser, html};
	let events = Parser::new_ext(&text, Options::ENABLE_TABLES | Options::ENABLE_STRIKETHROUGH | Options::ENABLE_TASKLISTS).map(|e| match e {
		Event::Html(raw) | Event::InlineHtml(raw) => Event::Text(raw),
		e => e,
	});
	let mut out = String::new();
	html::push_html(&mut out, events);
	rsx! { div { class: "md text-body text-ink-mid", dangerous_inner_html: out } }
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
		div { class: "flex flex-col gap-2 rounded-[var(--radius)] border border-chip-warn-border bg-accent-warn/14 px-3 py-2",
			for (i , c) in conflict.clashes.iter().enumerate() {
				div { key: "{c.field}", class: "flex flex-col gap-1.5",
					span { class: "flex items-center gap-1.5 text-body-s font-medium text-accent-warn",
						Icon { glyph: Glyph::Alert, class: "size-3.5" }
						"{c.field} changed on GitHub while you edited"
					}
					div { class: "flex items-start gap-2",
						span { class: "w-[120px] shrink-0 text-caption text-ink-soft", "yours" }
						match merging() {
							Some(values) => rsx! {
								textarea { class: "min-h-16 flex-1 rounded-[var(--control-radius)] border border-ring bg-transparent p-1.5 text-body text-ink outline-none", value: values[i].clone(), oninput: move |e: FormEvent| merging.with_mut(|m| m.as_mut().expect("while merging")[i] = e.value()) }
							},
							None => rsx! { span { class: "text-body whitespace-pre-wrap text-ink", "{c.local}" } },
						}
					}
					div { class: "flex items-start gap-2",
						span { class: "w-[120px] shrink-0 text-caption text-ink-soft", "GitHub" }
						span { class: "text-body whitespace-pre-wrap text-ink", "{c.remote}" }
					}
					if let Some(was) = &c.consensus {
						div { class: "flex items-start gap-2",
							span { class: "w-[120px] shrink-0 text-caption text-ink-soft", "you saw" }
							span { class: "text-body whitespace-pre-wrap text-ink-soft", "{was}" }
						}
					}
				}
			}
			div { class: "flex flex-wrap items-center gap-1.5",
				match merging() {
					Some(values) => rsx! {
						Btn { style: Style::Primary, onclick: { let resend = resend.clone(); move |_| resend(values.clone()) }, "Save merged" }
						Btn { style: Style::Ghost, onclick: move |_| merging.set(None), "Back" }
					},
					None => rsx! {
						Btn { style: Style::Primary, onclick: { let resend = resend.clone(); let yours = yours.clone(); move |_| resend(yours.clone()) }, "Keep mine" }
						Btn { style: Style::Outline, onclick: forget, "Take GitHub" }
						Btn { style: Style::Ghost, onclick: { let yours = yours.clone(); move |_| merging.set(Some(yours.clone())) }, "Merge…" }
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
						ctx.say(format!("clash:{link}"), super::toast::Tone::Success, "Resolved", None);
					}
					Ok(Some(_)) => ctx.say(format!("clash:{link}"), super::toast::Tone::Warn, "Markers remain: it is still pending", None),
					Err(e) => ctx.say(format!("clash:{link}"), super::toast::Tone::Error, e.to_string(), None),
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
		Err(e) => return rsx! { span { class: "text-caption text-accent-error", "{e}" } },
	};
	let sides = sides(&content);
	rsx! {
		div { class: "flex flex-col gap-2 rounded-[var(--radius)] border border-chip-warn-border bg-accent-warn/14 px-3 py-2",
			span { class: "flex items-center gap-1.5 text-body-s font-medium text-accent-warn", Icon { glyph: Glyph::Alert, class: "size-3.5" } "changed here and on GitHub since they last agreed" }
			match editing() {
				Some(text) => rsx! {
					Textarea { class: "font-mono", value: text.clone(), rows: 16, oninput: move |e: FormEvent| editing.set(Some(e.value())) }
					div { class: "flex gap-1.5",
						Btn { style: Style::Primary, onclick: { let resolve = resolve.clone(); move |_| resolve(text.clone()) }, "Save merged" }
						Btn { style: Style::Ghost, onclick: move |_| editing.set(None), "Back" }
					}
				},
				None => rsx! {
					pre { class: "max-h-64 overflow-auto text-mono-s whitespace-pre-wrap text-ink-mid", "{content}" }
					div { class: "flex flex-wrap gap-1.5",
						if let Some((local, remote)) = sides {
							Btn { style: Style::Primary, onclick: { let resolve = resolve.clone(); move |_| resolve(local.clone()) }, "Keep mine" }
							Btn { style: Style::Outline, onclick: { let resolve = resolve.clone(); move |_| resolve(remote.clone()) }, "Take GitHub" }
						}
						Btn { style: Style::Ghost, onclick: move |_| editing.set(Some(content.clone())), "Merge…" }
					}
				},
			}
		}
	}
}
