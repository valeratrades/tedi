//! An issue tab, laid out as GitHub's: the conversation, its properties beside it. Every property is set in
//! place, against the value it showed; a clash shows both values with three ways out. A pull request's tab
//! only shows it, its reviewers aside.

use std::collections::BTreeSet;

use dioxus::{core::spawn_forever, prelude::*};
use ev_lib::uikit_dioxus::{Popover, PopoverContent, PopoverTrigger, Spinner, Textarea};
use tedi_core::{
	CloseState, IssueLink, PrState, Review, Sprint,
	board::{Card, Said, Shown, Target},
	chain::Point,
	conflict::{CommentEdit, Conflict, Field as EditedField, FieldEdit, list, sides},
	labels::{Color, Scheme},
};

use super::{
	Ctx,
	card::{Avatar, CardView, Field, KindIcon, ago},
	icons::{Glyph, Icon},
	kit::{Btn, IconBtn, MenuItem, Picker, Style},
	route::Go,
};
use crate::api;

#[component]
pub fn IssuePanel(link: IssueLink) -> Element {
	rsx! { super::view::Behind { what: "issue {link}", need: super::Need::Cards(vec![link.clone()]), Opened { link } } }
}

#[component]
fn Opened(link: IssueLink) -> Element {
	let ctx = use_context::<Ctx>();
	let Some(card) = ctx.card(&link) else {
		return rsx! { div { class: "p-4 text-body text-ink-soft", "{link} is not on the board: not stored here" } };
	};
	let conflict = ctx.conflicts.read().get(&link).cloned();
	rsx! {
		div { class: "@container h-full overflow-auto bg-card",
			div { class: "flex flex-col gap-3 px-4 py-3",
				div { class: "flex h-6 shrink-0 items-center gap-1.5",
					KindIcon { card: card.clone() }
					a { class: "truncate text-mono-s text-ink-soft hover:underline", href: "{link}", target: "_blank", "{link.project()}" }
					Number { link: link.clone() }
					div { class: "flex-1" }
					IconBtn { glyph: Glyph::Scope, title: "Scope to it", onclick: { let link = link.clone(); move |_| ctx.scope_to(Point::Issue(link.clone())) } }
					IconBtn { glyph: Glyph::Link, title: "Copy its link", onclick: { let link = link.clone(); move |_| copy(ctx, link.to_string()) } }
				}
				if card.pr.is_none() {
					Title { link: link.clone(), current: card.title.clone() }
				} else {
					h2 { class: "text-h2 text-ink", "{card.title}" }
				}
				Standing { card: card.clone() }
				if let Some(conflict) = conflict { Clashes { link: link.clone(), conflict } }
				if card.conflicted { Recorded { link: link.clone() } }
				div { class: "flex flex-col gap-4 @2xl:flex-row",
					div { class: "flex min-w-0 flex-1 flex-col gap-4",
						if card.pr.is_none() {
							Conversation { key: "{link}", card: card.clone() }
						} else {
							Markdown { text: card.body.clone() }
						}
					}
					div { class: "order-first flex shrink-0 flex-col @2xl:order-last @2xl:w-60", Sidebar { card } }
				}
			}
		}
	}
}

/// An address can open an issue before the board arrives: its title joins it then.
#[component]
pub(super) fn IssueTab(link: IssueLink) -> Element {
	let ctx = use_context::<Ctx>();
	let title = ctx.card(&link).map_or_else(|| format!("#{}", link.number()), |c| format!("#{} {}", link.number(), c.title));
	ctx.tab_label(Glyph::Issue, &title)
}

/// Open or closed, closing it, and whose sub-issue it is.
#[component]
fn Standing(card: Card) -> Element {
	let ctx = use_context::<Ctx>();
	let link = card.link.clone();
	let (glyph, face, text) = match (&card.pr, &card.state) {
		(Some(pr), _) => (
			Glyph::Pr,
			match pr.state {
				PrState::Draft => "bg-secondary text-ink-mid",
				PrState::Open => "bg-positive/14 text-positive-ink",
				PrState::Merged => "bg-chart-6/14 text-chart-6",
				PrState::Closed => "bg-accent-error/14 text-accent-error",
			},
			pr.state.to_string(),
		),
		(None, CloseState::Closed) => (Glyph::Check, "bg-chart-6/14 text-chart-6", "closed".to_string()),
		(None, s) if s.is_closed() => (Glyph::Blocker, "bg-secondary text-ink-mid", "closed · not planned".to_string()),
		(None, CloseState::InProgress(p)) => (Glyph::Issue, "bg-positive/14 text-positive-ink", format!("open · {}", p.to_string().trim_start_matches("p:"))),
		(None, _) => (Glyph::Issue, "bg-positive/14 text-positive-ink", "open".to_string()),
	};
	let parent = card.parent.as_ref().and_then(|p| ctx.card(p));
	rsx! {
		div { class: "flex flex-wrap items-center gap-2",
			span { class: "inline-flex h-6 items-center gap-1.5 rounded-full px-2.5 text-body-s font-medium capitalize {face}", Icon { glyph, class: "size-3.5" } "{text}" }
			if card.pr.is_none() { Closing { card: card.clone(), said: None } }
			StatePicker { card: card.clone() }
			if let Some(at) = card.updated {
				span { class: "text-caption text-ink-faint", title: "{at}", "changed {ago(at)}" }
			}
		}
		if let Some(parent) = parent {
			Go {
				to: ctx.at.read().with_issue(&parent.link),
				class: "flex min-w-0 items-center gap-1.5 text-left text-body-s text-ink-soft hover:underline",
				Icon { glyph: Glyph::Group, class: "size-3.5 shrink-0" }
				span { class: "truncate", "sub-issue of {parent.link.shorthand(link.project())} {parent.title}" }
			}
		}
	}
}

/// Closes it, as completed or as not planned; closed, reopens it. `said`: a comment posted first, then dropped.
#[component]
fn Closing(card: Card, said: Option<Signal<String>>) -> Element {
	let ctx = use_context::<Ctx>();
	let closed = card.state.is_closed();
	let set = {
		let card = card.clone();
		move |new: &'static str| {
			let (link, seen) = (card.link.clone(), card.state.to_checkbox_contents());
			// outlives this button, gone with the comment's text
			spawn_forever(async move {
				if let Some(mut said) = said {
					let body = said.peek().clone();
					if !ctx.comment(link.clone(), CommentEdit::Add { body }).await {
						return;
					}
					said.set(String::new());
				}
				ctx.edit(
					link,
					vec![FieldEdit {
						field: EditedField::State,
						seen,
						new: new.to_string(),
					}],
				);
			});
		}
	};
	let with = if said.is_some() { " with comment" } else { "" };
	let mut open = use_signal(|| false);
	if closed {
		return rsx! { Btn { style: Style::Outline, glyph: Glyph::Revert, onclick: move |_| set(" "), "Reopen{with}" } };
	}
	rsx! {
		span { class: "inline-flex items-center",
			Btn { style: Style::Outline, glyph: Glyph::Check, title: "as completed", onclick: { let set = set.clone(); move |_| set("x") }, "Close{with}" }
			Popover { open: open(), on_open_change: move |v| open.set(v),
				PopoverTrigger { class: "inline-flex h-6 w-5 items-center justify-center text-ink-soft hover:text-ink", aria_label: "close as…", Icon { glyph: Glyph::ChevronDown, class: "size-3" } }
				PopoverContent { align: "start", class: "flex w-48 flex-col gap-0.5 rounded-[var(--radius)] border border-border bg-popover p-1 text-ink",
					for (mark , label , glyph) in [("x", "Close as completed", Glyph::Check), ("-", "Close as not planned", Glyph::Blocker)] {
						MenuItem { key: "{mark}", selected: false, label, glyph, onclick: {
							let set = set.clone();
							move |_| {
								open.set(false);
								set(mark);
							}
						} }
					}
				}
			}
		}
	}
}

/// Every property beside the conversation; each is set from its own menu.
#[component]
fn Sidebar(card: Card) -> Element {
	let ctx = use_context::<Ctx>();
	let link = card.link.clone();
	let home = link.project();
	let board = ctx.board.read();
	let children: Vec<&Card> = board.cards.iter().filter(|c| c.parent.as_ref() == Some(&link)).collect();
	let editable = card.pr.is_none();
	let mut assignable = board.assignable.get(&home.to_string()).cloned().unwrap_or_default(); // a repo no pull read them of yet
	assignable.sort_by_key(|a| (ctx.me().as_ref() != Some(a), a.to_lowercase()));
	let faces = |also: &[String]| -> Vec<Choice> {
		assignable
			.iter()
			.chain(also.iter().filter(|a| !assignable.contains(a)))
			.map(|a| Choice {
				value: a.clone(),
				text: a.clone(),
				meta: if ctx.me().as_ref() == Some(a) { "you".into() } else { String::new() },
				group: None,
				mark: Mark::Face,
			})
			.collect()
	};
	let assign = {
		let link = link.clone();
		move |seen: Vec<String>, new: Vec<String>| {
			ctx.edit(
				link.clone(),
				vec![FieldEdit {
					field: EditedField::Assignees,
					seen: list(&seen),
					new: list(&new),
				}],
			);
		}
	};
	rsx! {
		if let Some(pr) = card.pr.clone() {
			Section { label: "author", Person { login: pr.author.clone() } }
			Section {
				label: "reviewers",
				action: rsx! {
					Chooser {
						title: "Request reviews",
						glyph: Glyph::Plus,
						choices: faces(&pr.requested).into_iter().filter(|c| c.value != pr.author).collect::<Vec<_>>(),
						picked: pr.requested.clone(),
						onpick: {
							let link = link.clone();
							move |(seen, new): (Vec<String>, Vec<String>)| {
								ctx.request_reviews(link.clone(), seen, new);
							}
						},
					}
				},
				if pr.requested.is_empty() && pr.reviews.is_empty() { None_ { "no one" } }
				for r in pr.requested.clone() {
					span { key: "asked {r}", class: "flex w-full items-center gap-1.5", Person { login: r } span { class: "text-caption text-accent-warn", "awaited" } }
				}
				for (login , verdict) in pr.reviews.iter().filter(|(l, _)| !pr.requested.contains(l)) {
					span { key: "{login}", class: "flex w-full items-center gap-1.5",
						Person { login: login.clone() }
						match verdict {
							Review::Approved => rsx! { span { class: "text-caption text-positive-ink", "approved" } },
							Review::ChangesRequested => rsx! { span { class: "text-caption text-accent-error", "changes requested" } },
							Review::Commented => rsx! { span { class: "text-caption text-ink-soft", "commented" } },
							Review::Dismissed => rsx! { span { class: "text-caption text-ink-faint", "dismissed" } },
						}
					}
				}
			}
		}
		if editable {
			Section {
				label: "assignees",
				action: rsx! {
					Chooser { title: "Assign", glyph: Glyph::Plus, choices: faces(&card.assignees), picked: card.assignees.clone(), onpick: { let assign = assign.clone(); move |(seen, new)| assign(seen, new) } }
				},
				for a in card.assignees.clone() { Person { key: "{a}", login: a } }
				if card.assignees.is_empty() {
					None_ {
						"no one"
						if let Some(me) = ctx.me().filter(|me| assignable.contains(me)) {
							" · "
							button { r#type: "button", class: "text-accent-info-ink hover:underline", onclick: move |_| assign(Vec::new(), vec![me.clone()]), "assign yourself" }
						}
					}
				}
			}
			Section { label: "labels", action: rsx! { Labels { card: card.clone() } },
				Field { card: card.clone(), shown: Shown::Type }
				Field { card: card.clone(), shown: Shown::Labels }
			}
			Section { label: "importance", Field { card: card.clone(), shown: Shown::Importance } }
			Section { label: "hours", Field { card: card.clone(), shown: Shown::Hours } }
			Milestones { card: card.clone() }
		} else {
			Section { label: "assignees",
				for a in card.assignees.clone() { Person { key: "{a}", login: a } }
				if card.assignees.is_empty() { None_ { "no one" } }
			}
			Section { label: "labels",
				Field { card: card.clone(), shown: Shown::Type }
				Field { card: card.clone(), shown: Shown::Labels }
			}
		}
		if let Some(pr) = card.pr.as_ref().filter(|pr| !pr.closes.is_empty()) {
			Section { label: "closes",
				for issue in pr.closes.clone() {
					Go { key: "{issue}", to: ctx.at.read().with_issue(&issue), class: "text-body text-accent-info-ink hover:underline", "{issue.shorthand(home)}" }
				}
			}
		}
		if !card.blocked_by.is_empty() {
			Section { label: "blocked by",
				for b in card.blocked_by.clone() {
					Go {
						key: "{b}",
						to: ctx.at.read().with_issue(&b),
						class: "flex min-w-0 max-w-full items-center gap-1 text-body text-accent-error hover:underline",
						Icon { glyph: Glyph::Blocker, class: "size-3 shrink-0" }
						span { class: "truncate whitespace-nowrap",
							"{b.shorthand(home)}"
							if let Some(c) = ctx.card(&b) { " {c.title}" }
						}
					}
				}
			}
		}
		if !card.prs.is_empty() {
			Section { label: "development",
				for pr in card.prs.iter() {
					a { key: "{pr.url}", class: "flex items-center gap-1 text-body text-accent-info-ink hover:underline", href: "{pr.url}", target: "_blank",
						Icon { glyph: Glyph::Pr, class: "size-3" }
						"#{pr.url.rsplit('/').next().expect(\"a url has a last segment\")} · {pr.state}"
					}
				}
			}
		}
		if !children.is_empty() {
			Section { label: "sub-issues", span { class: "text-body-s text-ink-mid", "{children.iter().filter(|c| !c.open()).count()} of {children.len()} closed" } }
		}
	}
}

/// What sits in a section holding nothing.
#[component]
fn None_(children: Element) -> Element {
	rsx! { span { class: "text-body text-ink-faint", {children} } }
}

#[component]
fn Person(login: String) -> Element {
	rsx! { span { class: "flex items-center gap-1.5 text-body text-ink", Avatar { login: login.clone() } "{login}" } }
}

/// One property: `label · value · its menu` on a line when narrow, stacked beside the conversation.
#[component]
fn Section(label: &'static str, action: Option<Element>, children: Element) -> Element {
	rsx! {
		div { class: "grid min-h-[30px] grid-cols-[88px_1fr_auto] items-center gap-x-2 border-b border-border-subtle py-1 @2xl:grid-cols-[1fr_auto] @2xl:gap-y-1 @2xl:py-2",
			span { class: "text-body-s text-ink-soft @2xl:font-medium", "{label}" }
			div { class: "flex min-w-0 flex-wrap items-center gap-x-2 gap-y-1 @2xl:col-span-2 @2xl:row-start-2", {children} }
			div { class: "flex min-h-6 items-center @2xl:col-start-2 @2xl:row-start-1", {action} }
		}
	}
}

/// What precedes a choice's text.
#[derive(Clone, PartialEq)]
enum Mark {
	Dot(Option<Color>),
	Face,
	Milestone,
}

#[derive(Clone, PartialEq)]
struct Choice {
	value: String,
	text: String,
	meta: String,
	/// A heading in the menu; at most one choice under it is picked.
	group: Option<String>,
	mark: Mark,
}

/// A searched menu of `choices`, picked and unpicked while it is open and sent once, when it closes: what was
/// picked as it opened, and what is now.
#[component]
fn Chooser(title: String, glyph: Glyph, choices: Vec<Choice>, picked: Vec<String>, onpick: EventHandler<(Vec<String>, Vec<String>)>) -> Element {
	let mut open = use_signal(|| false);
	let mut seen = use_signal(Vec::<String>::new);
	let mut draft = use_signal(Vec::<String>::new);
	let mut needle = use_signal(String::new);
	let mut turn = move |on: bool| {
		if on {
			seen.set(picked.clone());
			draft.set(picked.clone());
			needle.set(String::new());
		} else if list(&seen.peek()) != list(&draft.peek()) {
			onpick.call((seen.peek().clone(), draft.peek().clone()));
		}
		open.set(on);
	};
	let toggle = {
		let choices = choices.clone();
		move |choice: &Choice| {
			let rivals: Vec<String> = choices.iter().filter(|c| c.group.is_some() && c.group == choice.group).map(|c| c.value.clone()).collect();
			draft.with_mut(|d| match d.iter().position(|v| *v == choice.value) {
				Some(at) => _ = d.remove(at),
				None => {
					d.retain(|v| !rivals.contains(v));
					d.push(choice.value.clone());
				}
			});
		}
	};
	let hits: Vec<Choice> = match open() {
		true => {
			let needle = needle.read().to_lowercase();
			choices.into_iter().filter(|c| c.text.to_lowercase().contains(&needle)).collect()
		}
		false => Vec::new(),
	};
	let first = hits.first().cloned();
	rsx! {
		Popover { open: open(), on_open_change: move |v| turn(v),
			PopoverTrigger { class: "inline-flex size-6 items-center justify-center rounded-[var(--control-radius)] text-ink-soft hover:bg-hover hover:text-ink", aria_label: title.clone(), Icon { glyph } }
			PopoverContent { align: "end", class: "flex w-[260px] flex-col gap-0.5 rounded-[var(--radius)] border border-border bg-popover p-1 text-ink",
				div { class: "flex h-7 shrink-0 items-center gap-1.5 rounded-[var(--control-radius)] border border-border-subtle px-2",
					Icon { glyph: Glyph::Search, class: "size-3.5 text-ink-faint" }
					input {
						class: "min-w-0 flex-1 bg-transparent text-body text-ink outline-none placeholder:text-ink-faint",
						placeholder: "{title}…",
						autofocus: true,
						onmounted: move |e| async move {
							_ = e.data().set_focus(true).await; // a menu closed again already has nothing to focus
						},
						value: needle(),
						oninput: move |e| needle.set(e.value()),
						onkeydown: {
							let mut toggle = toggle.clone();
							move |e: KeyboardEvent| {
								if e.key() == Key::Enter
									&& let Some(first) = &first
								{
									toggle(first);
								}
							}
						},
					}
				}
				div { class: "flex max-h-72 flex-col gap-0.5 overflow-y-auto",
					if hits.is_empty() {
						span { class: "px-2 py-1 text-caption text-ink-soft", "Nothing to pick" }
					}
					for (i , choice) in hits.iter().enumerate() {
						if let Some(group) = choice.group.as_ref().filter(|_| i == 0 || hits[i - 1].group != choice.group) {
							span { class: "shrink-0 px-2 pt-1.5 pb-0.5 text-overline text-ink-faint", "{group}" }
						}
						button {
							r#type: "button",
							class: "flex h-7 shrink-0 items-center gap-2 rounded-[var(--control-radius)] px-2 text-left hover:bg-hover",
							"aria-pressed": draft.read().contains(&choice.value),
							onclick: {
								let (mut toggle, choice) = (toggle.clone(), choice.clone());
								move |_| toggle(&choice)
							},
							span { class: "flex w-3.5 shrink-0 justify-center", if draft.read().contains(&choice.value) { Icon { glyph: Glyph::Check, class: "size-3.5 text-primary-ink" } } }
							match &choice.mark {
								Mark::Dot(Some(Color([r, g, b]))) => rsx! { span { class: "size-3 shrink-0 rounded-full", style: "background: rgb({r} {g} {b})" } },
								Mark::Dot(None) => rsx! { span { class: "size-3 shrink-0 rounded-full border border-border-strong" } },
								Mark::Face => rsx! { Avatar { login: choice.text.clone() } },
								Mark::Milestone => rsx! { Icon { glyph: Glyph::Sprint, class: "size-3.5 shrink-0 text-ink-soft" } },
							}
							span { class: "flex-1 truncate text-body text-ink", "{choice.text}" }
							if !choice.meta.is_empty() { span { class: "shrink-0 text-mono-s text-ink-soft", "{choice.meta}" } }
						}
					}
				}
			}
		}
	}
}

/// The repo's labels, in their GitHub colours; the scheme's axes under headings, one value each.
#[component]
fn Labels(card: Card) -> Element {
	let ctx = use_context::<Ctx>();
	let board = ctx.board.read();
	let colors = board.colors.get(&card.link.project().to_string());
	let names: BTreeSet<&String> = colors.into_iter().flat_map(|c| c.keys()).chain(&card.labels).collect();
	let mut choices: Vec<Choice> = names
		.into_iter()
		.map(|name| {
			let scheme = Scheme::from(std::slice::from_ref(name));
			let group = [("type", scheme.kind.is_some()), ("importance", scheme.importance.is_some()), ("hours", scheme.hours.is_some())]
				.into_iter()
				.find(|(_, on)| *on)
				.map(|(group, _)| group.to_string());
			Choice {
				value: name.clone(),
				text: name.clone(),
				meta: String::new(),
				group,
				mark: Mark::Dot(colors.and_then(|c| c.get(name)).copied()),
			}
		})
		.collect();
	choices.sort_by_key(|c| {
		let scheme = Scheme::from(std::slice::from_ref(&c.value));
		(c.group.clone(), scheme.importance.map(u16::from).or(scheme.hours), c.text.to_lowercase())
	});
	rsx! {
		Chooser {
			title: "Label",
			glyph: Glyph::Plus,
			choices,
			picked: card.labels.clone(),
			onpick: {
				let link = card.link.clone();
				move |(seen, new): (Vec<String>, Vec<String>)| {
					ctx.edit(link.clone(), vec![FieldEdit { field: EditedField::Labels, seen: list(&seen), new: list(&new) }]);
				}
			},
		}
	}
}

/// The milestones hosting it, of any repo; `+` offers those in the workspace's scope, one a repo.
#[component]
fn Milestones(card: Card) -> Element {
	let ctx = use_context::<Ctx>();
	let home = card.link.project();
	let board = ctx.board.read();
	let within = ctx.within().unwrap_or_default(); // nothing of its scope is on the board: the panel shows without one
	let hosts = |t: &Target| t.hosts(&card.link);
	let mut offered: Vec<&Target> = board
		.targets
		.iter()
		.filter(|t| !matches!(t.sprint(), Some(Sprint::Planning(_))) && (within.offers(&t.link) || hosts(t))) // what is planned is its own file's to say
		.collect();
	offered.sort_by_key(|t| (t.link.repo_info() != home, t.link.repo_info().to_string(), t.due_on.is_none(), t.due_on, t.title.to_lowercase()));
	let hosting: Vec<&Target> = offered.iter().copied().filter(|t| hosts(t)).collect();
	let due = |at: jiff::Timestamp| at.strftime("%b %-d").to_string();
	let choices: Vec<Choice> = offered
		.into_iter()
		.map(|t| Choice {
			value: t.link.as_str().to_string(),
			text: t.title.clone(),
			meta: t.due_on.map(due).unwrap_or_default(), // no deadline set
			group: Some(t.link.repo_info().to_string()),
			mark: Mark::Milestone,
		})
		.collect();
	let set = {
		let link = card.link.clone();
		move |seen: Vec<String>, new: Vec<String>| {
			ctx.set_milestones(link.clone(), seen, new);
		}
	};
	let picked: Vec<String> = hosting.iter().map(|t| t.link.as_str().to_string()).collect();
	rsx! {
		Section {
			label: "milestones",
			action: rsx! { Chooser { title: "Add to a milestone", glyph: Glyph::Plus, choices, picked, onpick: { let set = set.clone(); move |(seen, new)| set(seen, new) } } },
			if hosting.is_empty() { None_ { "none" } }
			for t in hosting {
				{
					let known: Vec<&Card> = board.cards.iter().filter(|c| t.hosts(&c.link)).collect();
					let closed = known.iter().filter(|c| !c.open()).count();
					let share = (closed * 100).div_euclid(known.len()); // it holds this card at least
					let url = t.link.as_str().to_string();
					rsx! {
						div { key: "{url}", class: "group/ms flex w-full min-w-0 flex-col gap-1 py-0.5",
							div { class: "flex min-w-0 items-center gap-1.5",
								Icon { glyph: Glyph::Sprint, class: "size-3.5 shrink-0 text-ink-soft" }
								a { class: "truncate text-body text-ink hover:underline", href: "{url}", target: "_blank", "{t.title}" }
								if t.link.repo_info() != home { span { class: "shrink-0 truncate text-mono-s text-ink-faint", "{t.link.repo_info().repo()}" } }
								div { class: "flex-1" }
								button {
									r#type: "button",
									class: "shrink-0 text-ink-faint hover:text-ink",
									title: "take it out of {t.title}",
									aria_label: "take it out of {t.title}",
									onclick: { let (set, url) = (set.clone(), url.clone()); move |_| set(vec![url.clone()], Vec::new()) },
									Icon { glyph: Glyph::X, class: "size-3" }
								}
							}
							div { class: "flex items-center gap-2",
								div { class: "h-1 flex-1 overflow-hidden rounded-full bg-secondary", div { class: "h-full bg-positive-ink", style: "width: {share}%" } }
								span { class: "shrink-0 text-caption text-ink-soft",
									"{closed}/{known.len()}"
									if let Some(at) = t.due_on { " · due {due(at)}" }
								}
							}
						}
					}
				}
			}
		}
	}
}

/// Its body, its sub-issues, what was said on it, and a comment to add.
#[component]
fn Conversation(card: Card) -> Element {
	let ctx = use_context::<Ctx>();
	let link = card.link.clone();
	let thread = use_resource({
		let link = link.clone();
		move || {
			let link = link.clone();
			_ = ctx.board.read(); // reread on every change announced
			async move { api::thread(link).await }
		}
	});
	let children: Vec<Card> = ctx.board.read().cards.iter().filter(|c| c.parent.as_ref() == Some(&link)).cloned().collect();
	let body = card.body.split("\n# Blockers").next().expect("split yields at least one piece").trim_end().to_string(); // the blockers show as their own field
	let tick = {
		let (link, held) = (link.clone(), card.body.clone());
		move |at: usize| {
			let mut new = held.clone();
			let box_ = &held[at..at + 3];
			new.replace_range(at..at + 3, if box_ == "[ ]" { "[x]" } else { "[ ]" });
			ctx.edit(
				link.clone(),
				vec![FieldEdit {
					field: EditedField::Body,
					seen: held.clone(),
					new,
				}],
			);
		}
	};
	let read = thread.read();
	let author = match &*read {
		Some(Ok(t)) => t.author.clone(),
		_ => None,
	};
	rsx! {
		Editable { link: link.clone(), author, current: card.body.clone(),
			if body.is_empty() { None_ { "No description" } } else { Markdown { text: body.clone(), ontick: tick } }
		}
		if !children.is_empty() {
			div { class: "flex flex-col gap-1.5",
				span { class: "text-overline text-ink-soft", "Sub-issues" }
				for child in children {
					CardView { key: "{child.link}", card: child.clone(), shown: ctx.workflows().into_keys().map(Shown::State).chain([Shown::Assignees]).collect::<Vec<_>>() }
				}
			}
		}
		match &*read {
			None => rsx! { Spinner {} },
			Some(Err(e)) => rsx! { span { class: "text-caption text-accent-error", "{e}" } },
			Some(Ok(thread)) => rsx! {
				for (i , said) in thread.comments.iter().enumerate() {
					Comment { key: "{said.id:?} {i}", link: link.clone(), said: said.clone() }
				}
			},
		}
		Composer { card: card.clone() }
	}
}

/// A comment; its author edits and deletes it.
#[component]
fn Comment(link: IssueLink, said: Said) -> Element {
	let ctx = use_context::<Ctx>();
	let mut draft = use_signal(|| None::<(String, String)>);
	let own = said.id.filter(|_| said.by.is_some() && said.by == ctx.me());
	rsx! {
		div { class: "rounded-[var(--radius)] border border-border-subtle",
			div { class: "flex h-8 items-center gap-1.5 border-b border-border-subtle px-3",
				if let Some(by) = said.by.clone() { Person { login: by } }
				if let Some(at) = said.at { span { class: "text-caption text-ink-faint", title: "{at}", "{ago(at)}" } }
				div { class: "flex-1" }
				if let Some(id) = own {
					IconBtn { glyph: Glyph::Pencil, title: "Edit", onclick: { let body = said.body.clone(); move |_| draft.set(Some((body.clone(), body.clone()))) } }
					IconBtn { glyph: Glyph::X, title: "Delete", onclick: {
						let (link, seen) = (link.clone(), said.body.clone());
						move |_| {
							let (link, seen) = (link.clone(), seen.clone());
							ctx.say(format!("delete:{id}"), super::toast::Tone::Warn, "Delete the comment?", Some(("Delete", Callback::new(move |()| {
								let (link, seen) = (link.clone(), seen.clone());
								spawn(async move { _ = ctx.comment(link, CommentEdit::Delete { id, seen }).await });
							}))));
						}
					} }
				}
			}
			div { class: "flex flex-col gap-1.5 px-3 py-2",
				match (draft(), own) {
					(Some((seen, text)), Some(id)) => {
						let save = {
							let link = link.clone();
							move || {
								let (seen, new) = draft.peek().clone().expect("saved from the draft");
								let link = link.clone();
								spawn(async move {
									if seen == new || ctx.comment(link, CommentEdit::Change { id, seen, new }).await {
										draft.set(None);
									}
								});
							}
						};
						rsx! {
							Draft { text, oninput: move |v| draft.set(Some((seen.clone(), v))), onsave: { let save = save.clone(); move |()| save() }, ondrop: move |()| draft.set(None) }
							div { class: "flex items-center gap-1",
								Btn { style: Style::Primary, onclick: move |_| save(), "Save" }
								Btn { style: Style::Ghost, onclick: move |_| draft.set(None), "Cancel" }
							}
						}
					}
					_ => rsx! { Markdown { text: said.body.clone() } },
				}
			}
		}
	}
}

//#[codestyle::skip(swappable-args)]
/// Markdown being written: Ctrl+Enter keeps it, Esc drops it.
#[component]
fn Draft(text: String, #[props(default)] placeholder: String, oninput: EventHandler<String>, onsave: EventHandler<()>, ondrop: EventHandler<()>) -> Element {
	rsx! {
		textarea {
			class: "min-h-24 w-full rounded-[var(--control-radius)] border border-border bg-transparent p-2 text-mono text-ink outline-none focus:border-ring placeholder:text-ink-faint",
			placeholder,
			value: text,
			oninput: move |e: FormEvent| oninput.call(e.value()),
			onkeydown: move |e: KeyboardEvent| match e.key() {
				Key::Enter if e.modifiers().ctrl() || e.modifiers().meta() => {
					e.prevent_default();
					onsave.call(());
				}
				Key::Escape => ondrop.call(()),
				_ => {}
			},
		}
	}
}

#[component]
fn Composer(card: Card) -> Element {
	let ctx = use_context::<Ctx>();
	let mut text = use_signal(String::new);
	let mut sending = use_signal(|| false);
	let blank = text.read().trim().is_empty();
	let send = {
		let link = card.link.clone();
		move || {
			if sending() || text.peek().trim().is_empty() {
				return;
			}
			let link = link.clone();
			spawn(async move {
				sending.set(true);
				let body = text.peek().clone();
				if ctx.comment(link, CommentEdit::Add { body }).await {
					text.set(String::new());
				}
				sending.set(false);
			});
		}
	};
	rsx! {
		div { class: "flex flex-col gap-1.5",
			Draft { text: text(), placeholder: "Add a comment", oninput: move |v| text.set(v), onsave: { let send = send.clone(); move |()| send() }, ondrop: |()| {} }
			div { class: "flex items-center justify-end gap-1.5",
				span { class: "flex-1 text-caption text-ink-faint", "Markdown · Ctrl+Enter posts" }
				if !blank { Closing { card: card.clone(), said: Some(text) } }
				Btn { style: Style::Primary, disabled: blank || sending(), onclick: move |_| send(), "Comment" }
			}
		}
	}
}

/// `#n`, copying its link.
#[component]
pub(super) fn Number(link: IssueLink) -> Element {
	let ctx = use_context::<Ctx>();
	rsx! {
		button {
			r#type: "button",
			class: "shrink-0 cursor-pointer text-mono-s text-ink-faint hover:underline",
			title: "Copy its link",
			onclick: {
				let link = link.clone();
				move |e: MouseEvent| {
					e.stop_propagation();
					copy(ctx, link.to_string());
				}
			},
			"#{link.number()}"
		}
	}
}

fn copy(ctx: Ctx, text: String) {
	ev_lib::uikit_dioxus::write_clipboard(&text.clone(), move |copied| match copied {
		true => ctx.say("copy", super::toast::Tone::Success, format!("Copied {text}"), None),
		false => ctx.say("copy", super::toast::Tone::Error, "the browser refused the clipboard", None),
	});
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
							ctx.edit(link.clone(), vec![FieldEdit { field: EditedField::Title, seen, new }]);
						}
					}
					Key::Escape => draft.set(None),
					_ => {}
				},
			}
		},
	}
}

/// One select per workflow the card sits in: `kanban / triage ▾`.
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
									ctx.move_card(card.clone(), name.clone(), leaf.clone());
								}
							}
						},
					}
				}
			}
		}
	}
}

/// The body, as its author wrote it; edited in place against what it showed.
#[component]
fn Editable(link: IssueLink, author: Option<String>, current: String, children: Element) -> Element {
	let ctx = use_context::<Ctx>();
	let mut draft = use_signal(|| None::<(String, String)>);
	let start = move |_| draft.set(Some((current.clone(), current.clone())));
	let save = move || {
		let (seen, new) = draft.take().expect("saved from the draft");
		if seen != new {
			ctx.edit(
				link.clone(),
				vec![FieldEdit {
					field: EditedField::Body,
					seen,
					new,
				}],
			);
		}
	};
	rsx! {
		div { class: "rounded-[var(--radius)] border border-border-subtle",
			div { class: "flex h-8 items-center gap-1.5 border-b border-border-subtle px-3",
				if let Some(author) = author { Person { login: author } }
				div { class: "flex-1" }
				if draft.read().is_none() {
					Btn { style: Style::Ghost, glyph: Glyph::Pencil, title: "or double-click it", onclick: start.clone(), "Edit" }
				}
			}
			div { class: "flex flex-col gap-1.5 px-3 py-2",
				match draft() {
					None => rsx! { div { class: "cursor-text", title: "Double-click to edit", ondoubleclick: start, {children} } },
					Some((seen, text)) => rsx! {
						Draft { text, oninput: move |v| draft.set(Some((seen.clone(), v))), onsave: { let mut save = save.clone(); move |()| save() }, ondrop: move |()| draft.set(None) }
						div { class: "flex items-center gap-1",
							Btn { style: Style::Primary, onclick: { let mut save = save.clone(); move |_| save() }, "Save" }
							Btn { style: Style::Ghost, onclick: move |_| draft.set(None), "Cancel" }
							span { class: "px-1 text-caption text-ink-faint", "Ctrl+Enter keeps · Esc drops" }
						}
					},
				}
			}
		}
	}
}

/// GitHub markdown, read. Raw HTML in it shows as text: a body is anyone's writing, never markup to run.
/// `ontick`: a task's box is clicked, at this byte of `text`.
#[component]
pub(super) fn Markdown(text: String, ontick: Option<EventHandler<usize>>) -> Element {
	use pulldown_cmark::{Event, Options, Parser, html};
	let events = Parser::new_ext(&text, Options::ENABLE_TABLES | Options::ENABLE_STRIKETHROUGH | Options::ENABLE_TASKLISTS)
		.into_offset_iter()
		.map(|(e, at)| match e {
			Event::Html(raw) | Event::InlineHtml(raw) => Event::Text(raw),
			Event::TaskListMarker(done) if ontick.is_some() => {
				let checked = if done { " checked" } else { "" };
				Event::InlineHtml(format!("<input type=\"checkbox\" data-at=\"{}\"{checked}>", at.start).into())
			}
			e => e,
		});
	let mut out = String::new();
	html::push_html(&mut out, events);
	let onclick = move |e: MouseEvent| {
		use dioxus::web::WebEventExt as _;
		use wasm_bindgen::JsCast as _;
		let Some(box_) = e.as_web_event().target().and_then(|t| t.dyn_into::<web_sys::Element>().ok()) else {
			return;
		};
		if let (Some(ontick), Some(at)) = (ontick, box_.get_attribute("data-at")) {
			e.prevent_default(); // it shows ticked once the edit landed
			ontick.call(at.parse().expect("written above as an offset"));
		}
	};
	rsx! { div { class: "md text-body text-ink-mid", dangerous_inner_html: out, onclick } }
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
			ctx.edit(link.clone(), edits);
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
