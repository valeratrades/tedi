//! The view builder (Figma: 02 · View construction): a view is a name, a grouping, the fields a card shows and the
//! boxes they are in. The left builds it, the right shows the workspace's cards through the draft, and the diff
//! says what changed against where it came from.

use dioxus::prelude::*;
use ev_lib::uikit_dioxus::Checkbox;
use tedi_core::board::{GroupBy, Shown};
use tilers_dioxus::PanelId;

use super::{
	Ctx, Modal,
	icons::{Glyph, Icon},
	kit::{Btn, Style},
	toolbar::ColumnsPicker,
	view::{Behind, Empty, Results, count, outcome},
};
use crate::api::{self, Boxes, Construction, Item, Kept, LibraryEdit, Sign, View, ViewPart};

/// `from`: the view the draft starts as; none: a blank one.
#[component]
pub(super) fn Builder(from: Option<String>) -> Element {
	let ctx = use_context::<Ctx>();
	let upstream: Option<View> = from
		.as_ref()
		.map(|name| ctx.views().into_iter().find(|v| &v.name == name).expect("a builder opens on a kept view"));
	let mine = from
		.as_ref()
		.is_some_and(|name| ctx.library.peek().as_ref().expect("arrived before any panel").views.contains_key(name));
	let mut draft = use_signal(|| match from.as_ref().and_then(|name| ctx.view(name)) {
		Some(v) if mine => v,
		Some(v) => View {
			name: format!("{} (mine)", v.name),
			..v
		},
		None => View {
			name: String::new(),
			group_by: ctx.workflows().into_keys().next().map_or(GroupBy::Nothing, GroupBy::State),
			shown: vec![Shown::Repo, Shown::Assignees, Shown::Importance, Shown::Hours, Shown::Labels],
			boxes: Boxes::default(),
		},
	});
	let edit = use_callback(move |part: ViewPart| draft.write().set(part));
	let mut relaid = use_signal(|| 0);
	let onlay = use_callback(move |columns: String| {
		let laid = ctx.columns(&columns).expect("offered from the library or the team's");
		draft.write().boxes = laid.boxes;
		relaid += 1;
	});
	let d = draft.read().clone();
	let outcome = outcome(&ctx, &d.group_by);
	let groups: Vec<String> = match &outcome {
		Some(Ok(groups)) => groups.iter().map(|(g, _)| g.clone()).collect(),
		Some(Err(_)) | None => Vec::new(), // nothing to show a box of
	};
	let changes = upstream
		.as_ref()
		.map(|u| api::diff(&Construction::View(u.clone()), &Construction::View(View { name: u.name.clone(), ..d.clone() })));
	rsx! {
		div { class: "flex h-full min-h-0",
			div { class: "flex w-[400px] shrink-0 flex-col border-r border-border-subtle bg-card",
				div { class: "flex min-h-0 flex-1 flex-col gap-4 overflow-y-auto px-4 py-3",
					div { class: "flex items-center gap-2",
						Icon { glyph: Glyph::View, class: "size-4 text-ink-soft" }
						span { class: "text-h3 text-ink", "Build view" }
						match (&upstream, mine) {
							(Some(u), false) => rsx! { span { class: "flex items-center gap-1 text-caption text-ink-soft", Icon { glyph: Glyph::Fork, class: "size-3" } "fork of team/{u.name}" } },
							(Some(_), true) => rsx! { span { class: "flex items-center gap-1 text-caption text-primary-ink", Icon { glyph: Glyph::User, class: "size-3" } "mine" } },
							(None, _) => rsx! {},
						}
					}
					Section { title: "Name",
						input {
							class: "h-7 rounded-[var(--control-radius)] border border-border px-2 text-body text-ink outline-none focus:border-ring",
							value: d.name.clone(),
							placeholder: "what it shows, e.g. Triage — arb only",
							oninput: { let mut draft = draft; move |e: FormEvent| draft.write().name = e.value() },
						}
					}
					Section { title: "Group by", hint: "a board grouped by state is the kanban",
						div { class: "flex flex-wrap gap-1",
							for g in ctx.groupings() {
								button {
									key: "{g}",
									r#type: "button",
									class: if g == d.group_by { "h-6 rounded-[var(--control-radius)] border border-primary-border bg-selected px-2 text-mono-s text-primary-ink" } else { "h-6 rounded-[var(--control-radius)] border border-border-subtle px-2 text-mono-s text-ink-mid hover:bg-hover" },
									onclick: { let g = g.clone(); move |_| edit.call(ViewPart::GroupBy(g.clone())) },
									"{g}"
								}
							}
						}
					}
					Section { title: "Shown fields", hint: "drag to order", Fields { view: draft } }
					Section { title: "Columns",
						div { class: "flex", ColumnsPicker { groups, hidden: d.boxes.hidden.clone(), edit } }
					}
				}
				div { class: "flex shrink-0 items-center gap-2 border-t border-border-subtle px-4 py-3",
					Btn { style: Style::Ghost, md: true, onclick: move |_| close(ctx), "Discard" }
					div { class: "flex-1" }
					if upstream.is_some() && !mine {
						Btn { style: Style::Outline, md: true, glyph: Glyph::Publish, disabled: d.name.trim().is_empty(), onclick: { let d = d.clone(); move |_| keep(ctx, d.clone(), true) }, "Propose to team" }
					}
					Btn { style: Style::Primary, md: true, glyph: Glyph::Save, disabled: d.name.trim().is_empty(), onclick: { let d = d.clone(); move |_| keep(ctx, d.clone(), false) }, "Save to mine" }
				}
			}
			div { class: "flex min-w-0 flex-1 flex-col",
				div { class: "flex h-9 shrink-0 items-center gap-2 border-b border-border-subtle px-3",
					span { class: "size-1.5 rounded-full bg-positive-ink" }
					span { class: "text-body-s font-medium text-ink", "Live preview" }
					if let Some(Ok(groups)) = &outcome {
						span { class: "text-mono-s text-ink-soft", "{count(groups, &d.boxes.hidden)} cards  ·  {groups.len()} groups  ·  in this workspace" }
					}
					div { class: "flex-1" }
					if let (Some(u), Some(changes)) = (&upstream, &changes) {
						if !changes.is_empty() {
							span { class: "inline-flex h-6 max-w-[60%] items-center gap-1.5 truncate rounded-[var(--control-radius)] border border-border-subtle px-2 text-mono-s",
								span { class: "text-ink-soft", "vs team/{u.name}" }
								for (i , c) in changes.iter().enumerate() {
									span {
										key: "{i}",
										class: match c.sign { Sign::Added => "text-positive-ink", Sign::Removed => "text-accent-error", Sign::Changed => "text-accent-warn" },
										match c.sign { Sign::Added => "+", Sign::Removed => "-", Sign::Changed => "~" }
										"{c.text}"
									}
								}
							}
						}
					}
				}
				match (outcome, ctx.need()) {
					(Some(Ok(groups)), Some(need)) => rsx! { Behind { what: "the draft", need, Results { view: d, groups, relaid: relaid(), edit, onlay } } },
					(Some(Err(why)), _) => rsx! { Empty { glyph: Glyph::Alert, error: true, title: "The draft doesn't run", body: why } },
					(None, _) | (_, None) => rsx! {}, // none is open
				}
			}
		}
	}
}

//#[codestyle::skip(swappable-args)]
#[component]
fn Section(title: &'static str, #[props(default)] hint: &'static str, children: Element) -> Element {
	rsx! {
		div { class: "flex flex-col gap-1.5",
			span { class: "flex items-baseline gap-1.5",
				span { class: "text-overline text-ink-soft", "{title}" }
				if !hint.is_empty() { span { class: "text-caption text-ink-faint", "{hint}" } }
			}
			{children}
		}
	}
}

/// Every field a card can show, as checkboxes; the shown ones lead, in their order, dragged to reorder.
#[component]
fn Fields(view: Signal<View>) -> Element {
	let ctx = use_context::<Ctx>();
	let mut view = view;
	let shown = view.read().shown.clone();
	let ordered: Vec<(Shown, bool)> = shown
		.iter()
		.cloned()
		.map(|s| (s, true))
		.chain(ctx.fields().filter(|s| !shown.contains(s)).map(|s| (s, false)))
		.collect();
	let mut dragged = use_signal(|| None::<usize>);
	rsx! {
		div { class: "grid grid-cols-[repeat(auto-fill,minmax(120px,1fr))] gap-x-3 gap-y-1.5",
			for (i , (s , on)) in ordered.into_iter().enumerate() {
				label {
					key: "{s}",
					class: if on { "flex cursor-grab items-center gap-1.5 text-body text-ink" } else { "flex items-center gap-1.5 text-body text-ink-soft" },
					draggable: if on { "true" } else { "false" },
					ondragstart: move |_| dragged.set(Some(i)),
					ondragover: move |e| e.prevent_default(),
					ondrop: move |e: DragEvent| {
						e.prevent_default();
						let Some(from) = dragged.take() else { return };
						let mut v = view.write();
						if from < v.shown.len() && i < v.shown.len() {
							let moved = v.shown.remove(from);
							v.shown.insert(i, moved);
						}
					},
					Checkbox {
						class: "shrink-0",
						checked: on,
						on_checked_change: { let s = s.clone(); move |now: bool| if now { view.write().shown.push(s.clone()) } else { view.write().shown.retain(|x| *x != s) } },
					}
					span { class: "truncate", "{s}" }
				}
			}
		}
	}
}

fn close(ctx: Ctx) {
	let tile = ctx
		.api
		.peek()
		.expect("a builder shows on an entered dock")
		.tab_ids()
		.into_iter()
		.find(|t| t.0.starts_with("builder:"));
	if let Some(id) = tile {
		ctx.api.peek().expect("checked above").close(&id);
	}
}

/// Saves the draft into the viewer's views, and opens it; `propose` then offers it to the team.
fn keep(ctx: Ctx, draft: View, propose: bool) {
	let name = draft.name.trim().to_string();
	let view = View { name: name.clone(), ..draft };
	spawn(async move {
		if !ctx.keep(LibraryEdit::Keep(Kept::View(view))).await {
			return;
		}
		close(ctx);
		ctx.open(PanelId(format!("view:{name}")));
		if propose {
			let mut m = ctx.modal;
			m.set(Some(Modal::Propose(Item::View(name))));
		} else {
			ctx.say("saved", super::toast::Tone::Success, format!("Saved \"{name}\" in your views"), None);
		}
	});
}
