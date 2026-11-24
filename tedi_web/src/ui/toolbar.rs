//! A view's toolbar (Figma: ViewToolbar). Row one builds the view: query, grouping, order, the fields
//! a card shows. Row two narrows it by facets, counts what it shows, and keeps it.

use dioxus::prelude::*;
use ev_lib::uikit_dioxus::{Checkbox, Popover, PopoverContent, PopoverTrigger};
use tedi_core::board::{GroupBy, Order, Query, Shown, Sort, SortKey, View};

use super::{
	Ctx,
	facets::FacetBar,
	icons::{Glyph, Icon},
	kit::{Btn, IconBtn, MenuItem, Picker, Style},
	library::NameForm,
};

/// What the toolbar shows and edits: the view on screen, against how it is kept.
#[derive(Clone, PartialEq, Props)]
pub(super) struct Toolbar {
	pub(super) view: Signal<View>,
	pub(super) defined: View,
	/// The query that ran: the view's own, or the last one that parsed while it doesn't.
	pub(super) query: Query,
	pub(super) error: Option<String>,
	pub(super) count: usize,
}

#[component]
pub(super) fn ViewToolbar(props: Toolbar) -> Element {
	let ctx = use_context::<Ctx>();
	let Toolbar {
		mut view,
		defined,
		query,
		error,
		count,
	} = props;
	let v = view.read().clone();
	let workflows: Vec<String> = ctx.workflows().into_keys().collect();
	let compact = ctx.band() == Some(tilers_dioxus::Band::Sm);
	let groupings: Vec<GroupBy> = workflows
		.iter()
		.cloned()
		.map(GroupBy::State)
		.chain([GroupBy::Repo, GroupBy::Assignee, GroupBy::Type, GroupBy::Importance, GroupBy::Nothing])
		.collect();
	if compact {
		let mut settings = use_signal(|| false);
		return rsx! {
			div { class: "flex shrink-0 flex-col gap-2 border-b border-border-subtle bg-card px-2 py-2",
				div { class: "flex items-center gap-1.5",
					label { class: if error.is_some() { "flex h-7 min-w-64 flex-1 items-center gap-2 rounded-[var(--control-radius)] border border-accent-error px-2" } else { "flex h-7 min-w-64 flex-1 items-center gap-2 rounded-[var(--control-radius)] border border-input px-2 focus-within:border-ring" },
						title: error.clone().unwrap_or_default(),
						Icon { glyph: Glyph::Search, class: "size-4 text-ink-soft" }
						input {
							class: "min-w-0 flex-1 bg-transparent text-mono text-ink outline-none placeholder:text-ink-faint",
							value: v.query.clone(),
							placeholder: "repo:, state:, assignee:, t:, i:>=5, label: or words in titles and bodies",
							spellcheck: "false",
							oninput: move |e: FormEvent| view.write().query = e.value(),
						}
					}
						Popover { open: settings(), on_open_change: move |v| settings.set(v),
						PopoverTrigger { class: if settings() { "inline-flex size-7 items-center justify-center rounded-[var(--control-radius)] bg-active text-ink" } else { "inline-flex size-7 items-center justify-center rounded-[var(--control-radius)] text-ink-soft" }, aria_label: "Grouping, order and fields", Icon { glyph: Glyph::Fields } }
						PopoverContent { align: "end", class: "flex w-72 flex-col gap-2 rounded-[var(--radius)] border border-border bg-popover p-2 text-ink",
							Picker { glyph: Glyph::Group, label: v.group_by.to_string(),
								for g in groupings {
									MenuItem { key: "{g}", selected: g == v.group_by, label: g.to_string(), onclick: { let g = g.clone(); move |_| view.write().group_by = g.clone() } }
								}
							}
							Picker { glyph: Glyph::Sort, label: v.sort.to_string(), OrderMenu { view, workflows: workflows.clone() } }
							Picker { glyph: Glyph::Fields, label: format!("{} fields", v.shown.len()), FieldsMenu { view, workflows } }
							div { class: "flex items-center gap-1",
								span { class: "flex-1 text-mono-s text-ink-soft", "{count} cards" }
								Keep { view, defined: defined.clone() }
							}
						}
					}
				}
				FacetBar { query: query.clone(), wrap: false, Refresh {} }
			}
		};
	}
	rsx! {
		div { class: "flex shrink-0 flex-col gap-2 border-b border-border-subtle bg-card px-3 py-2",
			div { class: "flex flex-wrap items-center gap-2",
				label { class: if error.is_some() { "flex h-7 min-w-64 flex-1 items-center gap-2 rounded-[var(--control-radius)] border border-accent-error px-2" } else { "flex h-7 min-w-64 flex-1 items-center gap-2 rounded-[var(--control-radius)] border border-input px-2 focus-within:border-ring" },
					title: error.clone().unwrap_or_default(),
					Icon { glyph: Glyph::Search, class: "size-4 text-ink-soft" }
					input {
						class: "min-w-0 flex-1 bg-transparent text-mono text-ink outline-none placeholder:text-ink-faint",
						value: v.query.clone(),
						placeholder: "repo:, state:, assignee:, t:, i:>=5, label: or words in titles and bodies",
						spellcheck: "false",
						oninput: move |e: FormEvent| view.write().query = e.value(),
					}
				}
				Picker { glyph: Glyph::Group, label: v.group_by.to_string(),
					for g in groupings {
						MenuItem { key: "{g}", selected: g == v.group_by, label: g.to_string(), onclick: { let g = g.clone(); move |_| view.write().group_by = g.clone() } }
					}
				}
				Picker { glyph: Glyph::Sort, label: v.sort.to_string(), OrderMenu { view, workflows: workflows.clone() } }
				Picker { glyph: Glyph::Fields, label: format!("{} fields", v.shown.len()), FieldsMenu { view, workflows } }
			}
			div { class: "flex items-center gap-1",
				FacetBar { query, wrap: true, Refresh {} }
				span { class: "shrink-0 px-1 text-mono-s text-ink-soft", "{count} cards" }
				tilers_dioxus::LockToggle {}
				Keep { view, defined }
			}
		}
	}
}

fn keys(workflows: &[String]) -> Vec<SortKey> {
	[SortKey::Rank, SortKey::Importance, SortKey::Hours, SortKey::Updated]
		.into_iter()
		.chain(workflows.iter().cloned().map(SortKey::State))
		.chain([SortKey::Number])
		.collect()
}

/// The first key orders, each \`then\` key orders what those before it tie on; a key picked again flips.
#[component]
pub(super) fn OrderMenu(view: Signal<View>, workflows: Vec<String>) -> Element {
	let order = view.read().sort.keys().to_vec();
	let mut set = move |at: usize, by: Option<SortKey>| {
		let mut keys = view.peek().sort.keys().to_vec();
		match by {
			None => keys.truncate(at),
			Some(by) => {
				let flipped = keys.get(at).is_some_and(|s| s.by == by).then(|| !keys[at].reversed);
				keys.truncate(at);
				keys.retain(|s| s.by != by);
				keys.push(Sort {
					by,
					reversed: flipped.unwrap_or(false),
				});
			}
		}
		if !keys.is_empty() {
			view.write().sort = Order::from(keys);
		}
	};
	let slots = order.len().min(2) + 1;
	rsx! {
		for at in 0..slots.min(3) {
			span { key: "head-{at}", class: "px-2 pt-1.5 pb-0.5 text-overline text-ink-soft", if at == 0 { "Sort by" } else { "then" } }
			if at > 0 {
				MenuItem { key: "none-{at}", selected: order.len() == at, label: "nothing", onclick: move |_| set(at, None) }
			}
			for by in keys(&workflows) {
				{
					let held = order.get(at).filter(|s| s.by == by);
					let meta = match held {
						Some(Sort { reversed: true, .. }) => "reversed",
						Some(_) => "↻ flips",
						None => "",
					};
					rsx! {
						MenuItem { key: "{at}-{by}", selected: held.is_some(), label: by.to_string(), meta, onclick: { let by = by.clone(); move |_| set(at, Some(by.clone())) } }
					}
				}
			}
		}
	}
}

/// Every field a card can show; the shown ones in their order, dragged to reorder.
#[component]
fn FieldsMenu(view: Signal<View>, workflows: Vec<String>) -> Element {
	let shown = view.read().shown.clone();
	let all: Vec<Shown> = [Shown::Repo, Shown::Number, Shown::Assignees]
		.into_iter()
		.chain(workflows.into_iter().map(Shown::State))
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
	let hidden: Vec<Shown> = all.into_iter().filter(|s| !shown.contains(s)).collect();
	let mut dragged = use_signal(|| None::<usize>);
	rsx! {
		span { class: "px-2 pt-1.5 pb-0.5 text-overline text-ink-soft", "Shown · drag to order" }
		for (i , s) in shown.iter().cloned().enumerate() {
			div {
				key: "{s}",
				draggable: "true",
				ondragstart: move |_| dragged.set(Some(i)),
				ondragover: move |e| e.prevent_default(),
				ondrop: move |e: DragEvent| {
					e.prevent_default();
					let Some(from) = dragged.take() else { return };
					let mut v = view.write();
					let moved = v.shown.remove(from);
					v.shown.insert(i, moved);
				},
				class: "flex h-7 cursor-grab items-center gap-2 rounded-[var(--control-radius)] px-2 hover:bg-hover",
				Icon { glyph: Glyph::Grip, class: "size-3.5 text-ink-faint" }
				Checkbox { checked: true, on_checked_change: { let s = s.clone(); move |_| view.write().shown.retain(|x| *x != s) } }
				span { class: "flex-1 text-body text-ink", "{s}" }
			}
		}
		if !hidden.is_empty() {
			div { class: "my-0.5 h-px bg-border-subtle" }
		}
		for s in hidden {
			label { key: "{s}", class: "flex h-7 items-center gap-2 rounded-[var(--control-radius)] px-2 pl-7.5 hover:bg-hover",
				Checkbox { checked: false, on_checked_change: { let s = s.clone(); move |_| view.write().shown.push(s.clone()) } }
				span { class: "flex-1 text-body text-ink-soft", "{s}" }
			}
		}
	}
}

/// `⟳`: every watched repo pulled from GitHub now; lit until the board reloads.
#[component]
fn Refresh() -> Element {
	let ctx = use_context::<Ctx>();
	let mut pulling = use_signal(|| false);
	use_effect(move || {
		ctx.board.read();
		pulling.set(false);
	});
	rsx! {
		IconBtn {
			glyph: Glyph::Sync,
			active: pulling(),
			title: if pulling() { "Pulling from GitHub…" } else { "Pull every watched repo from GitHub now" },
			onclick: move |_| {
				pulling.set(true);
				spawn(async move {
					if let Err(e) = crate::api::refresh().await {
						pulling.set(false);
						ctx.say("refresh", super::toast::Tone::Error, format!("refresh: {e}"), None);
					}
				});
			},
		}
	}
}

/// Clean: "Save as…". Changed and the viewer's: modified, Revert, Save. Changed and the team's: the team copy
/// is read-only, so it is kept as the viewer's own.
#[component]
fn Keep(view: Signal<View>, defined: View) -> Element {
	let ctx = use_context::<Ctx>();
	let mut naming = use_signal(|| false);
	let mine = ctx.library.read().as_ref().expect("views render once the library arrived").filters.contains_key(&defined.name);
	let dirty = *view.read() != defined;
	let mut save = move |name: String| {
		naming.set(false);
		let kept = View {
			name: name.clone(),
			..view.peek().clone()
		};
		spawn(async move {
			if ctx.keep(crate::api::LibraryEdit::Keep(crate::api::Kept::Filter(kept))).await {
				ctx.say("saved", super::toast::Tone::Success, format!("Saved \"{name}\" in your views"), None);
				ctx.open(tilers_dioxus::PanelId(format!("view:{name}")));
			}
		});
	};
	if naming() {
		return rsx! {
			div { class: "w-56 shrink-0", NameForm { placeholder: if mine || !dirty { "save as…" } else { "save as mine…" }, on_name: save, on_cancel: move |()| naming.set(false) } }
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
				Btn { style: Style::Ghost, glyph: Glyph::Revert, title: "Back to the view as kept", onclick: { let defined = defined.clone(); move |_| view.set(defined.clone()) }, "Revert" }
				Btn { style: Style::Outline, title: if mine { "Keep these settings as a new view of yours" } else { "Keep these settings as a view of yours; the team's stays as it is" }, onclick: move |_| naming.set(true), if mine { "Save as…" } else { "Save as mine" } }
				if mine {
					Btn { style: Style::Primary, title: "Overwrite your view with these settings", onclick: { let name = defined.name.clone(); move |_| save(name.clone()) }, "Save" }
				}
			} else {
				Btn { style: Style::Ghost, glyph: Glyph::Save, title: "Keep a copy of this view under a new name", onclick: move |_| naming.set(true), "Save as…" }
			}
		}
	}
}
