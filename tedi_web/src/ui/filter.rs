//! What every tile of the open workspace runs, in one bar above them (Figma: FilterBar, ScopeLine): the scope's
//! terms, the search and the order, the facets rewriting the search.

use dioxus::prelude::*;
use ev_lib::uikit_dioxus::{Popover, PopoverContent, PopoverTrigger};
use tedi_core::{
	board::{Order, Query, Scope, Slice, Sort, SortKey, Terms},
	chain::Point,
};
use tilers_dioxus::{Band, PanelId};

use super::{
	Ctx, Editing, Modal,
	facets::FacetBar,
	icons::{Glyph, Icon},
	kit::{IconBtn, Kbd, Menu, MenuItem},
	library::NameForm,
	scope::{Offered, glyph_at},
	toast,
};
use crate::api::{Item, Lens, WorkspacePart};

#[component]
pub(super) fn FilterBar() -> Element {
	let ctx = use_context::<Ctx>();
	let mut sorting = use_signal(|| false);
	let Some(lens) = ctx.lens.read().clone() else {
		return rsx! {};
	};
	let compact = ctx.band() == Some(Band::Sm);
	let of = match &*ctx.viewing.read() {
		Some(shared) => shared.link.to_string(),
		None => ctx.workspace.read().clone().expect("a lens is an open workspace's"),
	};
	rsx! {
		div { class: if compact { "flex shrink-0 flex-col gap-2 border-b border-border-subtle bg-card p-2" } else { "flex shrink-0 flex-col gap-1.5 border-b border-border-subtle bg-card px-3 py-2" },
			ScopeLine { scope: lens.scope.clone() }
			div { class: if compact { "flex items-center gap-2" } else { "flex flex-wrap items-start gap-2" },
				Search { key: "{of}", search: lens.search.clone() }
				if compact {
					Popover { open: sorting(), on_open_change: move |v| sorting.set(v),
						PopoverTrigger { class: "inline-flex size-7 shrink-0 items-center justify-center rounded-[var(--control-radius)] text-ink-soft hover:bg-hover", aria_label: "Order", Icon { glyph: Glyph::Sort } }
						PopoverContent { align: "end", class: "w-72 rounded-[var(--radius)] border border-border bg-popover p-2 text-ink", SortLabels { order: lens.sort.clone() } }
					}
				} else {
					SortLabels { order: lens.sort.clone() }
				}
			}
			FacetBar { lens, wrap: !compact, Refresh {} }
		}
	}
}

/// `[Start from ▾] repo:service-arb/* × +`: plain terms, whatever scope they were copied from; or
/// `[Start from ▾] ev-main / service-arb / site ×`: a point of the chain.
#[component]
fn ScopeLine(scope: Slice) -> Element {
	let mut open = use_signal(|| false);
	let here = match &scope {
		Slice::Point(point) => rsx! { Crumbs { point: point.clone() } },
		Slice::Terms(terms) => rsx! { TermsHere { terms: terms.clone() } },
	};
	rsx! {
		div { class: "flex min-h-7 flex-wrap items-center gap-2", role: "group", aria_label: "Scope",
			Popover { open: open(), on_open_change: move |v| open.set(v),
				PopoverTrigger { class: if open() { "flex h-7 items-center gap-1.5 rounded-[var(--control-radius)] border border-ring px-2" } else { "flex h-7 items-center gap-1.5 rounded-[var(--control-radius)] border border-border px-2 hover:bg-hover" },
					Icon { glyph: Glyph::Scope, class: "size-3.5 text-chart-3" }
					span { class: "text-body font-medium text-ink", "Start from" }
					Icon { glyph: Glyph::ChevronDown, class: "size-3.5 text-ink-soft" }
				}
				PopoverContent { align: "start", class: "flex w-[300px] flex-col gap-0.5 rounded-[var(--radius)] border border-border bg-popover p-1 text-ink",
					StartFrom { scope, onpick: move |()| open.set(false) }
				}
			}
			{here}
		}
	}
}

/// What holds `point`, from the top, down to it.
#[component]
fn Crumbs(point: Point) -> Element {
	let ctx = use_context::<Ctx>();
	let crumbs = ctx.crumbs(&point);
	let last = crumbs.len() - 1;
	rsx! {
		nav { class: "flex min-w-0 flex-wrap items-center gap-1.5", aria_label: "Scope",
			for (i , at) in crumbs.into_iter().enumerate() {
				Fragment { key: "{at}",
					if i == last {
						span { class: "max-w-64 truncate text-body font-medium text-ink", aria_current: "true", {ctx.called(&at)} }
					} else {
						button {
							r#type: "button",
							class: "max-w-40 truncate text-body text-ink-soft hover:text-ink hover:underline",
							onclick: { let at = at.clone(); move |_| ctx.scope_to(at.clone()) },
							{ctx.called(&at)}
						}
						span { class: "text-body text-ink-faint", "/" }
					}
				}
			}
			IconBtn { glyph: Glyph::X, title: "Leave this scope", onclick: move |_| ctx.start_from(None) }
		}
	}
}

#[component]
fn TermsHere(terms: Terms) -> Element {
	let ctx = use_context::<Ctx>();
	let mut adding = use_signal(|| false);
	let scope = terms.to_string();
	let terms: Vec<String> = scope.split_whitespace().map(str::to_string).collect();
	let narrow = move |terms: String| match terms.parse::<Terms>() {
		Ok(terms) => ctx.edit_lens(WorkspacePart::Scope(Slice::Terms(terms))),
		Err(e) => ctx.say("scope", toast::Tone::Error, e.to_string(), None),
	};
	rsx! {
			for (i , term) in terms.iter().cloned().enumerate() {
				span { key: "{i}-{term}", class: "inline-flex h-5 max-w-full items-center gap-1 rounded-[var(--control-radius)] bg-secondary pr-1 pl-1.5 text-mono-s text-ink-mid",
					span { class: "truncate", "{term}" }
					button {
						r#type: "button",
						class: "text-ink-soft hover:text-ink",
						title: "Drop {term}",
						aria_label: "Drop {term}",
						onclick: {
							let terms = terms.clone();
							move |_| {
								let mut rest = terms.clone();
								rest.remove(i);
								narrow(rest.join(" "));
							}
						},
						Icon { glyph: Glyph::X, class: "size-3" }
					}
				}
			}
			if adding() {
				div { class: "w-56",
					NameForm {
						placeholder: "repo:owner/*, is:issue, t:bug",
						on_name: move |term: String| {
							adding.set(false);
							narrow(format!("{scope} {term}"));
						},
						on_cancel: move |()| adding.set(false),
					}
				}
			} else {
				IconBtn { glyph: Glyph::Plus, title: "Add a term to the scope", onclick: move |_| adding.set(true) }
			}
	}
}

/// What the point it is at holds, orgs at the top; then the named scopes, starred first: one picked is copied onto
/// the line. Each with its open cards.
#[component]
fn StartFrom(scope: Slice, onpick: EventHandler<()>) -> Element {
	let ctx = use_context::<Ctx>();
	let mut needle = use_signal(String::new);
	let library = ctx.library.read();
	let library = library.as_ref().expect("a workspace is open once the library arrived");
	let mut scopes = ctx.scopes();
	scopes.sort_by_key(|s| !library.favorites.contains(&Item::Scope(s.path.clone()))); // stable: the tree's order within each
	let count = |open: Option<usize>| open.map(|n| n.to_string()).unwrap_or_default(); // before the board arrives
	let at = if let Slice::Point(point) = &scope { Some(point) } else { None };
	let found = |name: &str| name.to_lowercase().contains(&needle.read().to_lowercase());
	let below: Vec<Offered> = ctx.below(at).into_iter().filter(|o| found(&o.name)).collect();
	scopes.retain(|s| found(&s.path));
	rsx! {
		div { class: "flex h-7 items-center gap-1.5 rounded-[var(--control-radius)] border border-border-subtle px-2",
			Icon { glyph: Glyph::Search, class: "size-3.5 text-ink-faint" }
			input {
				class: "min-w-0 flex-1 bg-transparent text-body text-ink outline-none placeholder:text-ink-faint",
				placeholder: "scope…",
				value: needle(),
				oninput: move |e| needle.set(e.value()),
			}
		}
		div { class: "flex max-h-80 flex-col gap-0.5 overflow-y-auto",
			MenuItem { selected: false, glyph: Glyph::Scope, label: "All repos", meta: count(ctx.open_in(&Slice::default())), onclick: move |_| { onpick.call(()); ctx.start_from(None); } }
			if !below.is_empty() {
				span { class: "truncate px-2 pt-1.5 pb-0.5 text-overline text-ink-soft", if let Some(at) = at { "in {ctx.called(at)}" } else { "orgs" } }
			}
			for o in below {
				MenuItem {
					key: "{o.point}",
					selected: false,
					glyph: glyph_at(&o.point),
					label: o.name,
					meta: count(o.open),
					onclick: move |_| {
						onpick.call(());
						ctx.scope_to(o.point.clone());
					},
				}
			}
			if !scopes.is_empty() {
				span { class: "px-2 pt-1.5 pb-0.5 text-overline text-ink-soft", "named scopes" }
			}
			for s in scopes {
				{
					let starred = library.favorites.contains(&Item::Scope(s.path.clone()));
					let pad = if starred { 0 } else { s.depth * 16 }; // out of the tree, ahead of it
					let mine = library.scopes.contains_key(&s.path);
					rsx! {
						div { key: "{s.path}", class: "flex flex-col", style: "padding-left: {pad}px",
							MenuItem {
								selected: false,
								glyph: if starred { Glyph::Star } else { Glyph::Scope },
								label: if mine { format!("{} · mine", s.name()) } else { s.name().to_string() },
								meta: count(ctx.open_in(&Slice::Terms(s.scope.filter()))),
								onclick: {
									let path = s.path.clone();
									move |_| {
										onpick.call(());
										ctx.start_from(Some(path.clone()));
									}
								},
							}
						}
					}
				}
			}
		}
		div { class: "my-0.5 h-px bg-border-subtle" }
		if let Slice::Terms(terms) = scope.clone() && terms != Terms::default() {
			MenuItem {
				selected: false,
				glyph: Glyph::Save,
				label: "Save these terms as a scope…",
				onclick: move |_| {
					onpick.call(());
					let mut m = ctx.modal;
					m.set(Some(Modal::Scope(Editing::New(Scope::from(terms.clone())))));
				},
			}
		}
		button { r#type: "button", class: "flex h-7 items-center gap-2 rounded-[var(--control-radius)] px-2 hover:bg-hover",
			onclick: move |_| { onpick.call(()); ctx.open(PanelId("library:scopes".into())); },
			Icon { glyph: Glyph::Pencil, class: "size-4 text-ink-soft" }
			span { class: "flex-1 text-left text-body text-ink", "Edit scopes…" }
			Kbd { "⌘⇧P" }
		}
	}
}

/// The search as typed; while it doesn't parse, the tiles stay on the last that did.
#[component]
fn Search(search: Query) -> Element {
	let ctx = use_context::<Ctx>();
	let mut typed = use_signal(|| search.to_string());
	use_effect(use_reactive!(|search| {
		if typed.peek().parse::<Query>().ok().as_ref() != Some(&search) {
			typed.set(search.to_string()); // rewritten by a facet or a chip
		}
	}));
	let error = typed.read().parse::<Query>().err().map(|e| e.to_string());
	rsx! {
		label {
			class: if error.is_some() { "flex h-7 min-w-64 flex-1 items-center gap-2 rounded-[var(--control-radius)] border border-accent-error px-2" } else { "flex h-7 min-w-64 flex-1 items-center gap-2 rounded-[var(--control-radius)] border border-input px-2 focus-within:border-ring" },
			title: error.unwrap_or_default(),
			Icon { glyph: Glyph::Search, class: "size-4 text-ink-soft" }
			input {
				class: "min-w-0 flex-1 bg-transparent text-mono text-ink outline-none placeholder:text-ink-faint",
				aria_label: "Search",
				value: typed(),
				placeholder: "repo:, state:, assignee:, t:, i:>=5, label: or words in titles and bodies",
				spellcheck: "false",
				oninput: move |e: FormEvent| {
					typed.set(e.value());
					if let Ok(search) = e.value().parse::<Query>() {
						ctx.edit_lens(WorkspacePart::Search(search));
					}
				},
			}
		}
	}
}

/// The whole order, first key first, each a label: a click reverses it, a drag (or an arrow key) moves it.
#[component]
fn SortLabels(order: Order) -> Element {
	let ctx = use_context::<Ctx>();
	let order = order.keys().to_vec();
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
				reorder(ctx, |keys| {
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
								onclick: move |_| reorder(ctx, |keys| keys[i].reversed = !keys[i].reversed),
								onkeydown: move |e: KeyboardEvent| {
									match e.key() {
										Key::Enter => reorder(ctx, |keys| keys[i].reversed = !keys[i].reversed),
										Key::Character(c) if c == " " => reorder(ctx, |keys| keys[i].reversed = !keys[i].reversed),
										Key::ArrowLeft if i > 0 => reorder(ctx, |keys| keys.swap(i, i - 1)),
										Key::ArrowRight if i < last => reorder(ctx, |keys| keys.swap(i, i + 1)),
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
									onclick: move |_| reorder(ctx, |keys| _ = keys.remove(i)),
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
							reorder(ctx, |keys| {
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

fn reorder(ctx: Ctx, f: impl FnOnce(&mut Vec<Sort>)) {
	let Some(Lens { sort, .. }) = ctx.lens_now() else {
		return ctx.none_open();
	};
	let mut keys = sort.keys().to_vec();
	f(&mut keys);
	ctx.edit_lens(WorkspacePart::Sort(keys.into()));
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
