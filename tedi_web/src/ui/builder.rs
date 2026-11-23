//! The view builder (Figma: 02 · View construction): a view is name, query, layout, grouping, order and the
//! fields a card shows. The left builds it, the right runs the draft live over the board, and the diff says
//! what changed against where it came from.

use dioxus::prelude::*;
use ev_lib::uikit_dioxus::Checkbox;
use strum::IntoEnumIterator as _;
use tedi_core::board::{Facet, GroupBy, Layout, Query, Shown, Sort, SortKey, View};
use tilers_dioxus::PanelId;

use super::{
	Ctx, Modal,
	card::{Chip, Tone},
	facets::{FacetBar, InView},
	icons::{Glyph, Icon},
	kit::{Btn, Picker, Style},
	toolbar::{OrderMenu, Segmented},
	view::{Empty, Outcome, Results, outcome},
};
use crate::api::{self, Construction, Item, Kept, LibraryEdit, Sign};

/// \`from\`: the view the draft starts as; none: a blank one.
#[component]
pub(super) fn Builder(from: Option<String>) -> Element {
	let ctx = use_context::<Ctx>();
	let upstream: Option<View> = from
		.as_ref()
		.map(|name| ctx.views().into_iter().find(|v| &v.name == name).expect("a builder opens on a kept view"));
	let mine = from
		.as_ref()
		.is_some_and(|name| ctx.library.peek().as_ref().expect("arrived before any panel").filters.contains_key(name));
	let draft = use_signal(|| match &upstream {
		Some(v) if mine => v.clone(),
		Some(v) => View {
			name: format!("{} (mine)", v.name),
			..v.clone()
		},
		None => View {
			name: String::new(),
			query: "is:open".into(),
			layout: Layout::Board,
			group_by: ctx.workflows().into_keys().next().map_or(GroupBy::Nothing, GroupBy::State),
			sort: vec![Sort { by: SortKey::Rank, reversed: false }].into(),
			shown: vec![Shown::Repo, Shown::Assignees, Shown::Importance, Shown::Hours, Shown::Labels],
		},
	});
	use_context_provider(|| InView(draft));
	let mut good = use_signal(|| draft.peek().clone());
	use_effect(move || {
		let now = draft.read().clone();
		if now.query.parse::<Query>().is_ok() && *good.peek() != now {
			good.set(now);
		}
	});
	if ctx.board.read().is_none() {
		return rsx! { ev_lib::uikit_dioxus::Spinner {} };
	}
	let d = draft.read().clone();
	let error = d.query.parse::<Query>().err().map(|e| e.to_string());
	let ran = if error.is_none() { d.clone() } else { good.read().clone() };
	let outcome = outcome(&ctx, &ran);
	let changes = upstream
		.as_ref()
		.map(|u| api::diff(&Construction::View(u.clone()), &Construction::View(View { name: u.name.clone(), ..d.clone() })));
	let scope = ctx.scope().map(|(path, _)| path);
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
					Section { title: "Query", hint: "the whole of the narrowing",
						label { class: if error.is_some() { "flex h-7 items-center gap-2 rounded-[var(--control-radius)] border border-accent-error px-2" } else { "flex h-7 items-center gap-2 rounded-[var(--control-radius)] border border-input px-2 focus-within:border-ring" },
							title: error.clone().unwrap_or_default(),
							Icon { glyph: Glyph::Search, class: "size-4 text-ink-soft" }
							input {
								class: "min-w-0 flex-1 bg-transparent text-mono text-ink outline-none placeholder:text-ink-faint",
								value: d.query.clone(),
								placeholder: "repo:, state:, assignee:, t:, i:>=5, label: or words",
								spellcheck: "false",
								oninput: { let mut draft = draft; move |e: FormEvent| draft.write().query = e.value() },
							}
						}
						if let Ok(o) = &outcome {
							Clauses { query: o.query.clone() }
							FacetBar { query: o.query.clone(), wrap: true }
						}
					}
					Section { title: "Layout", Segmented { layout: d.layout, on_change: { let mut draft = draft; move |l| draft.write().layout = l } } }
					Section { title: "Group by", hint: "a board grouped by state is the kanban",
						div { class: "flex flex-wrap gap-1",
							for g in ctx.workflows().into_keys().map(GroupBy::State).chain([GroupBy::Repo, GroupBy::Assignee, GroupBy::Type, GroupBy::Importance, GroupBy::Nothing]) {
								button {
									key: "{g}",
									r#type: "button",
									class: if g == d.group_by { "h-6 rounded-[var(--control-radius)] border border-primary-border bg-selected px-2 text-mono-s text-primary-ink" } else { "h-6 rounded-[var(--control-radius)] border border-border-subtle px-2 text-mono-s text-ink-mid hover:bg-hover" },
									onclick: { let (g, mut draft) = (g.clone(), draft); move |_| draft.write().group_by = g.clone() },
									"{g}"
								}
							}
						}
					}
					Section { title: "Sort",
						div { class: "flex flex-wrap items-center gap-2",
							Picker { glyph: Glyph::Sort, label: d.sort.to_string(), OrderMenu { view: draft, workflows: ctx.workflows().into_keys().collect::<Vec<_>>() } }
						}
					}
					Section { title: "Shown fields", hint: "drag to order", Fields { view: draft } }
				}
				div { class: "flex shrink-0 items-center gap-2 border-t border-border-subtle px-4 py-3",
					Btn { style: Style::Ghost, md: true, onclick: move |_| close(ctx), "Discard" }
					div { class: "flex-1" }
					if upstream.is_some() && !mine {
						Btn { style: Style::Outline, md: true, glyph: Glyph::Publish, disabled: d.name.trim().is_empty(), onclick: { let d = d.clone(); move |_| keep(ctx, d.clone(), true) }, "Propose to team" }
					}
					Btn { style: Style::Primary, md: true, glyph: Glyph::Save, disabled: d.name.trim().is_empty() || error.is_some(), onclick: { let d = d.clone(); move |_| keep(ctx, d.clone(), false) }, "Save to mine" }
				}
			}
			div { class: "flex min-w-0 flex-1 flex-col",
				div { class: "flex h-9 shrink-0 items-center gap-2 border-b border-border-subtle px-3",
					span { class: "size-1.5 rounded-full bg-positive-ink" }
					span { class: "text-body-s font-medium text-ink", "Live preview" }
					if let Ok(o) = &outcome {
						span { class: "text-mono-s text-ink-soft",
							"{o.count} cards  ·  {o.groups.len()} groups"
							if let Some(s) = &scope { "  ·  in {s}" }
						}
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
				match outcome {
					Ok(Outcome { count, groups, loosen, .. }) => rsx! { Results { ran, count, groups, loosen } },
					Err(why) => rsx! { Empty { glyph: Glyph::Alert, error: true, title: "The draft doesn't run", body: why } },
				}
			}
		}
	}
}

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

/// The query's clauses as chips, each dropped by its ×: the query and its chips edit each other.
#[component]
fn Clauses(query: Query) -> Element {
	let in_view = use_context::<InView>();
	let clauses: Vec<(Facet, String)> = Facet::iter().flat_map(|f| query.picked(f).into_iter().map(move |v| (f, v))).collect();
	rsx! {
		div { class: "flex flex-wrap gap-1",
			for (facet , value) in clauses {
				{
					let excluded = value.starts_with('-');
					let tone = if excluded { Tone::Error } else { Tone::Warn };
					rsx! {
						Chip { key: "{facet}{value}", tone,
							"{facet}:{value}"
							button {
								r#type: "button",
								class: "opacity-70 hover:opacity-100",
								title: "drop it",
								onclick: { let value = value.clone(); move |_| in_view.narrow(|q| q.toggle(facet, value.trim_start_matches('-'), excluded).expect("a clause's own value parses")) },
								Icon { glyph: Glyph::X, class: "size-3" }
							}
						}
					}
				}
			}
		}
	}
}

/// Every field a card can show, as checkboxes; the shown ones lead, in their order, dragged to reorder.
#[component]
fn Fields(view: Signal<View>) -> Element {
	let ctx = use_context::<Ctx>();
	let mut view = view;
	let shown = view.read().shown.clone();
	let all: Vec<Shown> = [Shown::Repo, Shown::Number, Shown::Assignees]
		.into_iter()
		.chain(ctx.workflows().into_keys().map(Shown::State))
		.chain([
			Shown::Importance,
			Shown::Hours,
			Shown::Type,
			Shown::Labels,
			Shown::Blockers,
			Shown::Prs,
			Shown::Sprint,
			Shown::Updated,
		])
		.collect();
	let ordered: Vec<(Shown, bool)> = shown
		.iter()
		.cloned()
		.map(|s| (s, true))
		.chain(all.into_iter().filter(|s| !shown.contains(s)).map(|s| (s, false)))
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

/// Saves the draft into the viewer's views, and opens it; \`propose\` then offers it to the team.
fn keep(ctx: Ctx, draft: View, propose: bool) {
	let name = draft.name.trim().to_string();
	let view = View { name: name.clone(), ..draft };
	spawn(async move {
		if !ctx.keep(LibraryEdit::Keep(Kept::Filter(view))).await {
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
