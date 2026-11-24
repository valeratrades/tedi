//! A card, read first by its title: \`repo #n\` and who above it, the fields its view shows below
//! (Figma: Card, Chip, Stepper, Avatar).

use dioxus::prelude::*;
use ev_lib::uikit_dioxus::*;
use strum::IntoEnumIterator as _;
use tedi_core::{
	PrState,
	board::{Card, Facet, Lift, Shown},
	conflict::{Field as Edited, FieldEdit, list},
	labels::{Color, HOURS, Scheme, Type},
};

use super::{
	Ctx,
	facets::Pick,
	icons::{Glyph, Icon},
	route::Go,
};

#[component]
pub(super) fn CardView(card: Card, shown: Vec<Shown>, #[props(default)] lifted: Option<Lift>) -> Element {
	let ctx = use_context::<Ctx>();
	let link = card.link.clone();
	let open = ctx.panels.read().iter().any(|p| p.id.0 == format!("issue:{link}"));
	let (edge, why) = match (lifted, card.conflicted, open) {
		(Some(Lift::Blocks), ..) => ("border-accent-error", "blocks a card here"),
		(Some(Lift::Bottleneck), ..) => ("border-chart-6", "in the tree of a Bottlenecks"),
		(None, true, _) => ("border-chip-warn-border", ""),
		(None, false, true) => ("border-primary-ink", ""),
		(None, false, false) => ("border-border-subtle hover:border-border-strong", ""),
	};
	let has = |s: &Shown| shown.contains(s);
	let meta: Vec<Shown> = shown
		.iter()
		.filter(|s| !matches!(s, Shown::Repo | Shown::Number | Shown::Assignees | Shown::Blockers | Shown::Prs) && shows(&card, s))
		.cloned()
		.collect();
	rsx! {
		div { class: "flex flex-col gap-1.5 rounded-[var(--radius)] border bg-card px-3 py-2 {edge}", title: "{why}",
			div { class: "flex h-4 items-center gap-1.5",
				KindIcon { card: card.clone() }
				if has(&Shown::Repo) {
					Pick { facet: Facet::Repo, value: card.link.project().to_string(),
						span { class: "text-mono-s text-ink-soft", "{card.link.project().repo()}" }
					}
				}
				span { class: "text-mono-s text-ink-faint", "#{card.link.number()}" }
				div { class: "flex-1" }
				if has(&Shown::Assignees) {
					for a in card.assignees.iter() {
						Pick { key: "{a}", facet: Facet::Assignee, value: a.clone(), Avatar { login: a.clone() } }
					}
				}
			}
			Go {
				to: ctx.at.read().with_issue(&link),
				class: "text-left text-body font-medium text-ink hover:underline",
				"{card.title}"
			}
			if !meta.is_empty() {
				div { class: "flex flex-wrap items-center gap-1",
					for s in meta {
						Field { key: "{s}", card: card.clone(), shown: s }
					}
				}
			}
			if has(&Shown::Blockers) {
				Blockers { card: card.clone() }
			}
			if has(&Shown::Prs) {
				Field { card: card.clone(), shown: Shown::Prs }
			}
			if card.conflicted {
				div { class: "flex items-center gap-1.5 rounded-[var(--chip-radius)] bg-accent-warn/14 px-1.5 py-1",
					Icon { glyph: Glyph::Alert, class: "size-3 shrink-0 text-accent-warn" }
					span { class: "flex-1 text-caption text-accent-warn", "a field clashed with GitHub" }
					Go { to: ctx.at.read().with_issue(&link), class: "text-body-s font-medium text-accent-warn hover:underline", "Resolve" }
				}
			}
		}
	}
}

/// Whether \`card\` has anything to show for \`shown\`, so an empty meta row takes no room.
fn shows(card: &Card, shown: &Shown) -> bool {
	match shown {
		Shown::Type => card.kind.is_some(), // unset, it's set from the issue panel: a card stays quiet about it
		Shown::Labels => card.labels.iter().any(|l| !scheme_label(l)),
		Shown::Updated => card.updated.is_some(),
		Shown::State(w) => card.states.contains_key(w),
		Shown::Sprint | Shown::Importance | Shown::Hours => true,
		Shown::Repo | Shown::Number | Shown::Assignees | Shown::Blockers | Shown::Prs => true,
	}
}

/// \`t:\`, \`i:\` and \`h:\`: shown as their own fields, never among the labels.
fn scheme_label(l: &String) -> bool {
	let scheme = Scheme::from(std::slice::from_ref(l));
	scheme.kind.is_some() || scheme.importance.is_some() || scheme.hours.is_some()
}

/// An issue's open/closed circle, a pull request's branch in its state's colour.
#[component]
pub(super) fn KindIcon(card: Card) -> Element {
	let (glyph, ink) = match &card.pr {
		None if card.open() => (Glyph::Issue, "text-positive-ink"),
		None => (Glyph::Check, "text-chart-6"),
		Some(pr) => (
			Glyph::Pr,
			match pr.state {
				PrState::Draft => "text-ink-soft",
				PrState::Open => "text-positive-ink",
				PrState::Merged => "text-chart-6",
				PrState::Closed => "text-accent-error",
			},
		),
	};
	rsx! { Icon { glyph, class: "size-3 shrink-0 {ink}" } }
}

/// "⊘ blocked by devops#38": only blockers still open block.
#[component]
fn Blockers(card: Card) -> Element {
	let ctx = use_context::<Ctx>();
	let board = ctx.board.read();
	let cards = &board.as_ref().expect("cards render from the board").cards;
	let open: Vec<_> = card.blocked_by.iter().filter(|b| cards.iter().any(|c| &c.link == *b && c.open())).cloned().collect();
	if open.is_empty() {
		return rsx! {};
	}
	rsx! {
		div { class: "flex items-center gap-1 text-caption text-accent-error",
			Icon { glyph: Glyph::Blocker, class: "size-3 shrink-0" }
			span { "blocked by" }
			for (i , b) in open.into_iter().enumerate() {
				Go {
					key: "{b}",
					to: ctx.at.read().with_issue(&b),
					class: "hover:underline",
					if i > 0 { ", " }
					"{b.shorthand(card.link.project())}"
				}
			}
		}
	}
}

/// One field of a card, as its view's \`shown\` names it: on a card's meta row, or in a table cell.
#[component]
pub(super) fn Field(card: Card, shown: Shown) -> Element {
	let ctx = use_context::<Ctx>();
	match shown {
		Shown::Repo => rsx! { Pick { facet: Facet::Repo, value: card.link.project().to_string(), span { class: "text-mono-s text-ink-soft", "{card.link.project()}" } } },
		Shown::Number => rsx! { span { class: "text-mono-s text-ink-faint", "#{card.link.number()}" } },
		Shown::Assignees => rsx! {
			span { class: "inline-flex items-center gap-1",
				for a in card.assignees.iter() {
					Pick { key: "{a}", facet: Facet::Assignee, value: a.clone(), Avatar { login: a.clone() } }
				}
			}
		},
		Shown::Sprint => {
			let sprints: Vec<String> = ctx.sprints.read().iter().flatten().filter(|s| s.links.contains(&card.link)).map(|s| s.title.clone()).collect();
			rsx! {
				for s in sprints {
					span { key: "{s}", class: "inline-flex items-center gap-1 text-caption text-ink-soft", Icon { glyph: Glyph::Sprint, class: "size-3" } "{s}" }
				}
			}
		}
		Shown::Updated => rsx! { if let Some(at) = card.updated { span { class: "text-caption text-ink-soft", title: "{at}", "{ago(at)}" } } },
		Shown::State(workflow) => rsx! {
			if let Some(at) = card.states.get(&workflow) { // a view kept naming a workflow since dropped
				Pick { facet: Facet::State, value: format!("{workflow}/{at}"), Chip { tone: Tone::Neutral, "{at}" } }
			}
		},
		Shown::Type => rsx! { Stepper { card: card.clone(), axis: Axis::Type } },
		Shown::Importance => rsx! { Stepper { card: card.clone(), axis: Axis::Importance } },
		Shown::Hours => rsx! { Stepper { card: card.clone(), axis: Axis::Hours } },
		Shown::Labels => {
			let labels: Vec<(String, Tone)> = card
				.labels
				.iter()
				.filter(|l| !scheme_label(l))
				.map(|l| (l.clone(), Tone::of(label_color(&ctx, &card, l))))
				.collect();
			rsx! {
				for (l , tone) in labels {
					Pick { key: "{l}", facet: Facet::Label, value: l.clone(), Chip { tone, "{l}" } }
				}
			}
		}
		Shown::Blockers => rsx! { Blockers { card: card.clone() } },
		Shown::Prs => match &card.pr {
			Some(pr) => rsx! {
				for issue in pr.closes.iter() {
					Go {
						key: "{issue}",
						to: ctx.at.read().with_issue(issue),
						Chip { tone: Tone::Info, Icon { glyph: Glyph::Issue, class: "size-3" } "closes {issue.shorthand(card.link.project())}" }
					}
				}
			},
			None => rsx! {
				for pr in card.prs.iter() {
					a { key: "{pr.url}", href: "{pr.url}", target: "_blank",
						Chip { tone: Tone::Info, Icon { glyph: Glyph::Pr, class: "size-3" } "#{pr.url.rsplit('/').next().expect(\"a url has a last segment\")} · {pr.state}" }
					}
				}
			},
		},
	}
}

/// What a chip's hue means (the kit's chip roles, then two chart hues without significance). u2 is flat:
/// only the edge and the text take the hue, the fill stays the plane.
#[derive(Clone, Copy, Debug, Default, PartialEq)]
pub(super) enum Tone {
	#[default]
	Neutral,
	Primary,
	Positive,
	Info,
	Warn,
	Error,
	Pink,
	Purple,
}
impl Tone {
	fn classes(self) -> &'static str {
		match self {
			Tone::Neutral => "border-chip-neutral-border text-ink-mid",
			Tone::Primary => "border-chip-primary-border text-primary-ink",
			Tone::Positive => "border-chip-positive-border text-positive-ink",
			Tone::Info => "border-chip-info-border text-accent-info-ink",
			Tone::Warn => "border-chip-warn-border text-accent-warn",
			Tone::Error => "border-chip-error-border text-accent-error",
			Tone::Pink => "border-chart-4 text-chart-4",
			Tone::Purple => "border-chart-6 text-chart-6",
		}
	}

	/// The tone nearest a label's GitHub colour: its hue picks it, a washed-out one is neutral.
	fn of(color: Option<Color>) -> Tone {
		let Some(Color([r, g, b])) = color else { return Tone::Neutral };
		let [r, g, b] = [r, g, b].map(|c| f32::from(c) / 255.);
		let (max, min) = (r.max(g).max(b), r.min(g).min(b));
		let chroma = max - min;
		if chroma < 0.15 {
			return Tone::Neutral;
		}
		let hue = 60.
			* match max {
				m if m == r => ((g - b) / chroma).rem_euclid(6.),
				m if m == g => (b - r) / chroma + 2.,
				_ => (r - g) / chroma + 4.,
			};
		match hue {
			h if !(15. ..345.).contains(&h) => Tone::Error,
			h if h < 70. => Tone::Warn,
			h if h < 165. => Tone::Positive,
			h if h < 195. => Tone::Primary,
			h if h < 255. => Tone::Info,
			h if h < 295. => Tone::Purple,
			_ => Tone::Pink,
		}
	}
}
impl From<Type> for Tone {
	fn from(kind: Type) -> Tone {
		match kind {
			Type::Bug => Tone::Error,
			Type::Feature => Tone::Positive,
			Type::Enhancement => Tone::Info,
			Type::Question => Tone::Pink,
			Type::Chore => Tone::Neutral,
			Type::Epic => Tone::Purple,
			Type::Architectural => Tone::Primary,
		}
	}
}

#[component]
pub(super) fn Chip(tone: Tone, children: Element) -> Element {
	rsx! {
		span { class: "inline-flex h-[18px] items-center gap-1 rounded-[var(--chip-radius)] border px-1.5 text-mono-s whitespace-nowrap {tone.classes()}", {children} }
	}
}

fn label_color(ctx: &Ctx, card: &Card, label: &str) -> Option<Color> {
	let board = ctx.board.read();
	let colors = &board.as_ref().expect("cards render from the board").colors;
	colors.get(&card.link.project().to_string())?.get(label).copied()
}

/// Initials on a chart hue the login always gets.
#[component]
pub(super) fn Avatar(login: String) -> Element {
	let words: Vec<&str> = login.split(|c: char| !c.is_ascii_alphabetic()).filter(|w| !w.is_empty()).collect();
	let initials: String = match words.as_slice() {
		[] => "?".into(),
		[one] => one.chars().take(2).collect(),
		[first, .., last] => [first, last].iter().filter_map(|w| w.chars().next()).collect(),
	};
	let hue = ["bg-chart-1", "bg-chart-2", "bg-chart-3", "bg-chart-4", "bg-chart-5", "bg-chart-6"][login.bytes().map(usize::from).sum::<usize>() % 6];
	rsx! {
		span { class: "inline-flex size-4 shrink-0 items-center justify-center rounded-full text-[8px] leading-none text-on-accent-info uppercase {hue}", title: "@{login}", "{initials}" }
	}
}

#[derive(Clone, Copy, PartialEq)]
enum Axis {
	Type,
	Importance,
	Hours,
}

/// \`t:bug\`, \`i 6 − +\`, \`h ? − +\`: the value opens every value its axis takes; a pick or a step is a labels
/// edit, made against the labels the card shows. Unset reads \`?\` in neutral ink, so the gap shows.
#[component]
fn Stepper(card: Card, axis: Axis) -> Element {
	let ctx = use_context::<Ctx>();
	let mut open = use_signal(|| false);
	let (facet, held, values, counts_as) = match axis {
		Axis::Type => (Facet::Type, card.kind.map(|k| k.to_string()), Type::iter().map(|t| t.to_string()).collect::<Vec<_>>(), None),
		Axis::Importance => (
			Facet::Importance,
			card.importance.map(|v| format!("i:{v}")),
			(0..=9).map(|v| format!("i:{v}")).collect(),
			Some("i:3".to_string()),
		), // \`labels.nix\`: "\`i:3\` is the default"
		Axis::Hours => (Facet::Hours, card.hours.map(|v| format!("h:{v}")), HOURS.iter().map(|v| format!("h:{v}")).collect(), None),
	};
	let editable = card.pr.is_none(); // a pull request is read-only here
	let bare = held.as_deref().map(|l| l.split_once(':').expect("scheme labels carry a prefix").1.to_string());
	let tone = match (axis, card.kind) {
		(Axis::Type, Some(kind)) => Tone::from(kind),
		(Axis::Importance, _) if held.is_some() => Tone::Warn,
		(Axis::Hours, _) if held.is_some() => Tone::Info,
		_ => Tone::Neutral,
	};
	let ink = if held.is_some() { "" } else { "text-ink-soft" };
	let text = match axis {
		Axis::Type => held.clone().unwrap_or_else(|| "t:?".to_string()),
		Axis::Importance => format!("i {}", bare.as_deref().unwrap_or("?")),
		Axis::Hours => format!("h {}", bare.as_deref().unwrap_or("?")),
	};
	let set = {
		let card = card.clone();
		move |to: Option<String>| {
			let mut labels: Vec<String> = card
				.labels
				.iter()
				.filter(|l| {
					let scheme = Scheme::from(std::slice::from_ref(*l));
					match axis {
						Axis::Type => scheme.kind.is_none(),
						Axis::Importance => scheme.importance.is_none(),
						Axis::Hours => scheme.hours.is_none(),
					}
				})
				.cloned()
				.collect();
			labels.extend(to);
			ctx.edit(
				card.link.clone(),
				vec![FieldEdit {
					field: Edited::Labels,
					seen: list(&card.labels),
					new: list(&labels),
				}],
			);
		}
	};
	let next = |up: bool| {
		let at = held.as_ref().or(counts_as.as_ref()).map(|v| values.iter().position(|s| s == v).expect("the scheme's own values"));
		let i = match (at, up) {
			(None, true) => Some(0),
			(None, false) => None,
			(Some(i), true) => Some(i + 1),
			(Some(i), false) => i.checked_sub(1),
		};
		i.and_then(|i| values.get(i).cloned())
	};
	let steps = [("−", "lower", "lowest", next(false)), ("+", "raise", "highest", next(true))];
	if !editable {
		return match (axis, bare) {
			(Axis::Type, None) => rsx! {},
			(_, bare) => rsx! { Pick { facet, value: bare.unwrap_or_else(|| "none".to_string()), Chip { tone, span { class: ink, "{text}" } } } },
		};
	}
	let tone = if held.is_none() && axis != Axis::Type { Tone::Neutral } else { tone };
	rsx! {
		span { class: "inline-flex h-[18px] items-center gap-0.5 rounded-[var(--chip-radius)] border pr-0.5 pl-1.5 text-mono-s whitespace-nowrap {tone.classes()}",
			Popover { open: open(), on_open_change: move |v| open.set(v),
				PopoverTrigger { class: "cursor-pointer {ink}", aria_label: format!("set {facet}"), "{text}" }
				PopoverContent { align: "start", class: "flex max-h-80 w-max flex-col overflow-y-auto p-1 text-body font-normal text-ink",
					for value in values.iter().cloned().map(Some).chain(std::iter::once(None)) {
						button {
							key: "{value:?}",
							r#type: "button",
							class: "flex items-center gap-2 rounded px-1.5 py-1 text-left hover:bg-hover",
							onclick: {
								let (set, value) = (set.clone(), value.clone());
								move |_| {
									open.set(false);
									set(value.clone());
								}
							},
							span { class: "w-3 text-center", if value == held { Icon { glyph: Glyph::Check, class: "size-3" } } }
							span { class: "flex-1 text-mono", {value.clone().unwrap_or_else(|| "none".to_string())} }
						}
					}
				}
			}
			if axis != Axis::Type {
				for (sign , verb , end , to) in steps {
					button {
						key: "{verb}",
						r#type: "button",
						class: "rounded px-0.5 enabled:hover:bg-hover disabled:opacity-30 {ink}",
						disabled: to.is_none(),
						title: if to.is_some() { "{verb}" } else { "already the {end}" },
						onclick: { let set = set.clone(); move |_| set(to.clone()) },
						"{sign}"
					}
				}
			}
		}
	}
}

/// \`at\` as how long before now: \`2m ago\`, \`3d ago\`.
pub(super) fn ago(at: jiff::Timestamp) -> String {
	let secs = jiff::Timestamp::now().duration_since(at).as_secs().max(0);
	let (unit, per) = [("mo", 2_592_000), ("d", 86_400), ("h", 3_600), ("m", 60)]
		.into_iter()
		.find(|(_, per)| secs >= *per)
		.unwrap_or(("s", 1)); // under a minute counts in seconds
	format!("{}{unit} ago", secs.div_euclid(per))
}
