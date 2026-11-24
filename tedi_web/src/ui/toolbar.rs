//! A view's toolbar (Figma: ViewToolbar). Row one builds the view: query, grouping, order, the fields
//! a card shows. Row two narrows it by facets, counts what it shows, and keeps it.

use dioxus::prelude::*;
use ev_lib::uikit_dioxus::{Checkbox, Popover, PopoverContent, PopoverTrigger};
use tedi_core::board::{GroupBy, Query, Shown, Sort, SortKey, View};

use super::{
	Ctx,
	facets::FacetBar,
	icons::{Glyph, Icon},
	kit::{Btn, IconBtn, Menu, MenuItem, Picker, Style},
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
	/// None: what it shows isn't here yet.
	pub(super) count: Option<usize>,
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
							SortLabels { view }
							Picker { glyph: Glyph::Fields, label: format!("{} fields", v.shown.len()), FieldsMenu { view, workflows } }
							div { class: "flex items-center gap-1",
								span { class: "flex-1 text-mono-s text-ink-soft", if let Some(count) = count { "{count} cards" } }
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
				SortLabels { view }
				Picker { glyph: Glyph::Fields, label: format!("{} fields", v.shown.len()), FieldsMenu { view, workflows } }
			}
			div { class: "flex items-center gap-1",
				FacetBar { query, wrap: true, Refresh {} }
				span { class: "shrink-0 px-1 text-mono-s text-ink-soft", if let Some(count) = count { "{count} cards" } }
				tilers_dioxus::LockToggle {}
				Keep { view, defined }
			}
		}
	}
}

/// A view's whole order, first key first, each a label: a click reverses it, a drag (or an arrow key) moves it.
#[component]
pub(super) fn SortLabels(view: Signal<View>) -> Element {
	let ctx = use_context::<Ctx>();
	let order = view.read().sort.keys().to_vec();
	let offered: Vec<SortKey> = [SortKey::Rank, SortKey::Created, SortKey::Updated, SortKey::Importance, SortKey::Hours]
		.into_iter()
		.chain(ctx.workflows().into_keys().map(SortKey::State))
		.chain([SortKey::Number])
		.filter(|by| !order.iter().any(|s| s.by == *by))
		.collect();
	let mut dragged = use_signal(|| None::<usize>);
	let mut over = use_signal(|| None::<usize>);
	let last = order.len() - 1;
	rsx! {
		div {
			role: "group",
			aria_label: "Sort",
			class: "flex min-h-7 flex-wrap items-center gap-1 rounded-[var(--control-radius)] border border-border py-px pr-0.5 pl-2",
			ondragover: move |e| {
				if dragged.peek().is_some() {
					e.prevent_default();
				}
			},
			ondrop: move |e: DragEvent| {
				let (Some(from), Some(to)) = (dragged.take(), over.take()) else { return };
				e.prevent_default();
				reorder(view, |keys| {
					let moved = keys.remove(from);
					keys.insert(to, moved);
				});
			},
			Icon { glyph: Glyph::Sort, class: "h-6 w-4 shrink-0 text-ink-soft" }
			for (i , sort) in order.into_iter().enumerate() {
				{
					let (arrow, first) = first(&sort);
					let by = sort.by.to_string();
					let face = match (dragged(), over()) {
						(Some(from), _) if from == i => "border-chip-primary-border opacity-40",
						(Some(from), Some(to)) if to == i && from > i => "border-ring border-l-[3px]",
						(Some(_), Some(to)) if to == i => "border-ring border-r-[3px]",
						_ => "border-chip-primary-border hover:bg-hover",
					};
					rsx! {
						span {
							key: "{by}",
							class: "inline-flex h-[18px] cursor-grab items-center gap-1 rounded-[var(--chip-radius)] border pr-1 pl-1.5 text-mono-s whitespace-nowrap text-primary-ink {face}",
							draggable: "true",
							ondragstart: {
								let by = by.clone();
								move |e: DragEvent| {
									e.data_transfer().set_data("text/plain", &by).expect("a dragstart's transfer is writable"); // Firefox starts no drag without data
									dragged.set(Some(i));
								}
							},
							ondragover: move |_| {
								if *over.peek() != Some(i) {
									over.set(Some(i));
								}
							},
							ondragend: move |_| {
								dragged.set(None);
								over.set(None);
							},
							// no <button>: Firefox drags nothing by one
							span {
								role: "button",
								tabindex: "0",
								title: "{first} · click to reverse, drag to move",
								aria_label: "{by}: {first}",
								onclick: move |_| reorder(view, |keys| keys[i].reversed = !keys[i].reversed),
								onkeydown: move |e: KeyboardEvent| {
									match e.key() {
										Key::Enter => reorder(view, |keys| keys[i].reversed = !keys[i].reversed),
										Key::Character(c) if c == " " => reorder(view, |keys| keys[i].reversed = !keys[i].reversed),
										Key::ArrowLeft if i > 0 => reorder(view, |keys| keys.swap(i, i - 1)),
										Key::ArrowRight if i < last => reorder(view, |keys| keys.swap(i, i + 1)),
										_ => return,
									}
									e.prevent_default();
								},
								"{by} {arrow}"
							}
							if last > 0 {
								button {
									r#type: "button",
									class: "opacity-70 hover:opacity-100",
									title: "Drop {by}",
									aria_label: "Drop {by}",
									onclick: move |_| reorder(view, |keys| _ = keys.remove(i)),
									Icon { glyph: Glyph::X, class: "size-3" }
								}
							}
						}
					}
				}
			}
			if !offered.is_empty() {
				Menu { glyph: Glyph::Plus, title: "Add a sort key",
					for by in offered {
						MenuItem { key: "{by}", selected: false, label: by.to_string(), onclick: move |_| {
							let by = by.clone();
							reorder(view, |keys| {
								let at = keys.len() - usize::from(keys.last().is_some_and(|s| s.by == SortKey::Rank)); // rank leaves no ties: what follows it orders nothing
								keys.insert(at, Sort { by, reversed: false });
							});
						} }
					}
				}
			}
		}
	}
}

fn reorder(mut view: Signal<View>, f: impl FnOnce(&mut Vec<Sort>)) {
	let mut keys = view.peek().sort.keys().to_vec();
	f(&mut keys);
	view.write().sort = keys.into();
}

/// Which end comes first: as an arrow, down from the high or the late end, and in words.
fn first(sort: &Sort) -> (&'static str, &'static str) {
	let (falls, own, reversed) = match sort.by {
		SortKey::Rank | SortKey::Importance => (true, "highest first", "lowest first"),
		SortKey::Created => (true, "newest first", "oldest first"),
		SortKey::Updated => (true, "latest first", "longest untouched first"),
		SortKey::Hours | SortKey::Number => (false, "lowest first", "highest first"),
		SortKey::State(_) => (false, "in the workflow's order", "against the workflow's order"),
	};
	(if falls == sort.reversed { "↑" } else { "↓" }, if sort.reversed { reversed } else { own })
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
		let shown = view.peek().clone();
		let kept = crate::api::Kept::Filter {
			arranged_as: shown.name.clone(),
			view: View { name: name.clone(), ..shown },
		};
		spawn(async move {
			if ctx.keep(crate::api::LibraryEdit::Keep(kept)).await {
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
