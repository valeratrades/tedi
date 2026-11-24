//! The scope editor (Figma: ScopeEditor): name, the scope it nests in, the repos it covers — explicit or
//! patterns, with their live count — and the terms every card holds. Saved, it is a starting point: no workspace
//! it was applied to follows it.

use dioxus::prelude::*;
use tedi_core::board::{Query, Scope, Terms};

use super::{
	Ctx, Editing,
	icons::{Glyph, Icon},
	kit::{Btn, IconBtn, MenuItem, Picker, Style},
};
use crate::api::{Kept, LibraryEdit, Shelf};

/// Open cards `terms` hold; `None` before they arrive.
pub(super) fn open_in(ctx: &Ctx, terms: &Terms) -> Option<usize> {
	let board = ctx.board.read();
	let mut q: Query = "is:open".parse().expect("a constant, valid query");
	q.narrow(terms);
	if ctx.awaits(q.covers_prs()) {
		return None;
	}
	Some(q.run(&board.as_ref()?.cards, ctx.me().as_deref()).expect("clauses run").len())
}

#[component]
pub(super) fn ScopeEditor(editing: Editing) -> Element {
	let ctx = use_context::<Ctx>();
	let (initial_path, initial) = {
		let library = ctx.library.read();
		let library = library.as_ref().expect("the editor opens from a loaded screen");
		match &editing {
			Editing::New(scope) => (String::new(), scope.clone()),
			Editing::Own(path) => (path.clone(), library.scopes[path].clone()),
			Editing::Team(path) => {
				let at = ctx.scopes().into_iter().find(|s| s.path == *path).expect("offered from the team's");
				(path.clone(), Scope::from(at.scope.filter())) // flat: what it and every one nested in it cover
			}
		}
	};
	let team = matches!(editing, Editing::Team(_));
	let (initial_parent, initial_name) = match initial_path.rsplit_once(" / ") {
		Some((parent, name)) => (Some(parent.to_string()), name.to_string()),
		None => (None, initial_path.clone()),
	};
	let mut name = use_signal(|| if team { format!("my {initial_name}") } else { initial_name.clone() });
	let mut parent = use_signal(|| initial_parent.clone());
	let mut repos = use_signal(|| initial.repos.clone());
	let mut typed = use_signal(String::new);
	let mut terms = use_signal(|| initial.terms.clone());
	let mut term = use_signal(String::new);
	let mut bad = use_signal(|| None::<String>);
	let nested = initial.children.clone();
	let close = move || {
		let mut m = ctx.modal;
		m.set(None);
	};
	let board = ctx.board.read();
	let mut known: Vec<String> = board.as_ref().map_or_else(Vec::new, |b| b.cards.iter().map(|c| c.link.project().to_string()).collect()); // before the board arrives nothing is offered
	known.extend(known.clone().iter().map(|r| format!("{}/*", r.split('/').next().expect("owner/repo"))));
	drop(board);
	known.sort();
	known.dedup();
	let needle = typed.read().trim().to_lowercase();
	let offered: Vec<(String, usize)> = known
		.into_iter()
		.filter(|r| !repos.read().contains(r) && r.to_lowercase().contains(&needle))
		.take(12)
		.map(|r| {
			let scope = Scope {
				repos: vec![r.clone()],
				..Default::default()
			};
			let n = open_in(&ctx, &scope.filter()).unwrap_or(0); // offered only from the board's cards, so it has arrived
			(r, n)
		})
		.collect();
	let scope = Scope {
		repos: repos.read().clone(),
		terms: terms.read().clone(),
		children: nested,
	};
	let covers = (!scope.is_empty()).then(|| open_in(&ctx, &scope.filter())).flatten();
	let inside = format!("{initial_path} / ");
	let parents: Vec<String> = ctx.scopes().into_iter().map(|s| s.path).filter(|p| *p != initial_path && !p.starts_with(&inside)).collect();
	let path = match parent() {
		Some(p) => format!("{p} / {}", name.read().trim()),
		None => name.read().trim().to_string(),
	};
	let save = {
		let (editing, path, scope) = (editing.clone(), path.clone(), scope.clone());
		move |_| {
			let (editing, to, scope) = (editing.clone(), path.clone(), scope.clone());
			spawn(async move {
				if let Editing::Own(from) = &editing
					&& *from != to && !ctx
					.keep(LibraryEdit::Rename {
						shelf: Shelf::Scopes,
						from: from.clone(),
						to: to.clone(),
					})
					.await
				{
					return;
				}
				if ctx.keep(LibraryEdit::Keep(Kept::Scope { name: to, scope })).await {
					close();
				}
			});
		}
	};
	let mut add = move |r: String| {
		if !r.is_empty() && !repos.read().contains(&r) {
			repos.write().push(r);
		}
		typed.set(String::new());
	};
	rsx! {
		div { class: "fixed inset-0 z-[100] flex items-center justify-center bg-scrim p-4", onclick: move |_| close(),
			div {
				class: "flex w-[560px] max-w-full flex-col gap-4 rounded-[10px] border border-border bg-popover px-5 py-4 text-ink",
				onclick: |e: MouseEvent| e.stop_propagation(),
				onkeydown: move |e: KeyboardEvent| if e.key() == Key::Escape { close() },
				div { class: "flex h-6 items-center gap-2",
					Icon { glyph: Glyph::Scope, class: "size-4 text-chart-3" }
					span { class: "text-h3 text-ink", match &editing { Editing::New(_) => "New scope", _ => "Edit scope" } }
					if team {
						span { class: "flex items-center gap-1 text-caption text-ink-soft", Icon { glyph: Glyph::Users, class: "size-3" } "team · read-only" }
					} else {
						span { class: "flex items-center gap-1 text-caption text-primary-ink", Icon { glyph: Glyph::User, class: "size-3" } "mine" }
					}
					div { class: "flex-1" }
					IconBtn { glyph: Glyph::X, title: "Close", onclick: move |_| close() }
				}
				div { class: "flex items-end gap-3",
					label { class: "flex min-w-0 flex-1 flex-col gap-1.5",
						span { class: "text-body-s font-medium text-ink-mid", "Name" }
						input {
							class: "h-7 rounded-[var(--control-radius)] border border-border px-2 text-body text-ink outline-none focus:border-ring",
							value: name(),
							placeholder: "e.g. backend",
							autofocus: true,
							oninput: move |e: FormEvent| name.set(e.value()),
						}
					}
					div { class: "flex flex-col gap-1.5",
						span { class: "text-body-s font-medium text-ink-mid", "Inside" }
						Picker { glyph: Glyph::Scope, label: parent().unwrap_or_else(|| "nothing".into()),
							MenuItem { selected: parent().is_none(), label: "nothing", onclick: move |_| parent.set(None) }
							for p in parents {
								MenuItem { key: "{p}", selected: parent().as_ref() == Some(&p), label: p.clone(), onclick: { let p = p.clone(); move |_| parent.set(Some(p.clone())) } }
							}
						}
					}
				}
				div { class: "flex flex-col gap-1.5",
					span { class: "text-body-s font-medium text-ink-mid", "Repos" }
					div { class: "flex flex-col gap-2 rounded-[var(--control-radius)] border border-border p-2",
						if !repos.read().is_empty() {
							div { class: "flex flex-wrap gap-1",
								for r in repos.read().clone() {
									span { key: "{r}", class: "inline-flex h-5 items-center gap-1 rounded-[var(--control-radius)] border border-primary-border bg-selected pr-1 pl-1.5 text-mono-s text-ink",
										"{r}"
										button { r#type: "button", class: "text-ink-soft hover:text-ink", title: "Drop {r}", onclick: { let r = r.clone(); move |_| repos.write().retain(|x| *x != r) }, Icon { glyph: Glyph::X, class: "size-3" } }
									}
								}
							}
						}
						form {
							class: "flex h-7 items-center gap-1.5 rounded-[var(--control-radius)] border border-border-subtle px-2 focus-within:border-ring",
							onsubmit: move |e: FormEvent| {
								e.prevent_default();
								let r = typed.read().trim().to_string();
								add(r);
							},
							Icon { glyph: Glyph::Search, class: "size-3.5 text-ink-soft" }
							input {
								class: "min-w-0 flex-1 bg-transparent text-mono text-ink outline-none placeholder:font-sans placeholder:text-body-s placeholder:text-ink-faint",
								value: typed(),
								placeholder: "filter, or type a pattern (owner/*, owner/site*) and Enter",
								oninput: move |e: FormEvent| typed.set(e.value()),
							}
						}
						div { class: "flex max-h-36 flex-wrap gap-1 overflow-y-auto",
							for (r , n) in offered {
								button {
									key: "{r}",
									r#type: "button",
									class: "inline-flex h-5 items-center gap-1 rounded-[var(--control-radius)] border border-border-subtle pr-1.5 pl-1 text-mono-s text-ink-mid hover:bg-hover",
									onclick: { let r = r.clone(); move |_| add(r.clone()) },
									Icon { glyph: Glyph::Plus, class: "size-3" }
									"{r}"
									span { class: "text-ink-faint", "{n}" }
								}
							}
						}
					}
					span { class: "text-caption text-ink-soft", "patterns stay live as repos are added" }
				}
				div { class: "flex flex-col gap-1.5",
					span { class: "text-body-s font-medium text-ink-mid", "Terms" }
					div { class: "flex flex-col gap-2 rounded-[var(--control-radius)] border border-border p-2",
						if *terms.read() != Terms::default() {
							div { class: "flex flex-wrap gap-1",
								for (i , t) in terms.read().to_string().split_whitespace().map(str::to_string).enumerate() {
									span { key: "{i}-{t}", class: "inline-flex h-5 items-center gap-1 rounded-[var(--control-radius)] border border-primary-border bg-selected pr-1 pl-1.5 text-mono-s text-ink",
										"{t}"
										button {
											r#type: "button",
											class: "text-ink-soft hover:text-ink",
											title: "Drop {t}",
											onclick: move |_| {
												let shown = terms.peek().to_string();
												let rest: Vec<&str> = shown.split_whitespace().enumerate().filter(|(at, _)| *at != i).map(|(_, t)| t).collect();
												terms.set(rest.join(" ").parse().expect("each was a clause"));
											},
											Icon { glyph: Glyph::X, class: "size-3" }
										}
									}
								}
							}
						}
						form {
							class: if bad.read().is_some() { "flex h-7 items-center gap-1.5 rounded-[var(--control-radius)] border border-accent-error px-2" } else { "flex h-7 items-center gap-1.5 rounded-[var(--control-radius)] border border-border-subtle px-2 focus-within:border-ring" },
							onsubmit: move |e: FormEvent| {
								e.prevent_default();
								match format!("{} {}", terms.peek(), term.peek()).parse::<Terms>() {
									Ok(all) => {
										terms.set(all);
										term.set(String::new());
										bad.set(None);
									}
									Err(why) => bad.set(Some(why.to_string())),
								}
							},
							Icon { glyph: Glyph::Search, class: "size-3.5 text-ink-soft" }
							input {
								class: "min-w-0 flex-1 bg-transparent text-mono text-ink outline-none placeholder:font-sans placeholder:text-body-s placeholder:text-ink-faint",
								value: term(),
								placeholder: "a term (is:issue, t:bug) and Enter",
								oninput: move |e: FormEvent| {
									term.set(e.value());
									bad.set(None);
								},
							}
						}
					}
					span { class: if bad.read().is_some() { "text-caption text-accent-error" } else { "text-caption text-ink-soft" },
						match (bad(), covers) {
							(Some(why), _) => why,
							(None, Some(n)) => format!("every card holds them too, no free text  ·  {n} open cards"),
							(None, None) => "narrows by nothing yet".to_string(),
						}
					}
				}
				div { class: "flex items-center gap-2",
					if let Editing::Own(n) = &editing {
						Btn { style: Style::Danger, md: true, onclick: { let n = n.clone(); move |_| { let name = n.clone(); spawn(async move { if ctx.keep(LibraryEdit::Drop { shelf: Shelf::Scopes, name }).await { close(); } }); } }, "Delete" }
					}
					div { class: "flex-1" }
					Btn { style: Style::Ghost, md: true, onclick: move |_| close(), "Cancel" }
					Btn {
						style: Style::Primary,
						md: true,
						disabled: name.read().trim().is_empty() || scope.is_empty(),
						onclick: save,
						if team { "Fork to my library" } else { "Save scope" }
					}
				}
			}
		}
	}
}
