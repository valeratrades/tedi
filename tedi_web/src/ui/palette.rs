//! ⌘K (Figma: CommandPalette): one index over every view, scope, workspace and column layout, the open
//! issues, and actions. \`v: s: w: c: #\` narrow it to one kind. ⏎ opens in place, ⇧⏎ in a new tile, ⌘⏎ as
//! its library page.

use dioxus::prelude::*;
use ev_lib::uikit_dioxus::use_command_palette_shortcut;
use tilers_dioxus::PanelId;

use super::{
	Ctx, Editing, Modal,
	icons::{Glyph, Icon},
	kit::Kbd,
	shell::glyph_of,
};
use crate::api::LibraryEdit;

#[derive(Clone, PartialEq)]
enum Go {
	Panel(PanelId),
	Scope(Option<String>),
	Workspace(String),
	NewScope,
}

#[derive(Clone, PartialEq)]
struct Hit {
	kind: &'static str,
	glyph: Glyph,
	label: String,
	meta: String,
	go: Go,
	/// Its library page, for ⌘⏎.
	page: Option<PanelId>,
}

const KINDS: [(&str, &str); 5] = [("v:", "views"), ("s:", "scopes"), ("w:", "workspaces"), ("c:", "column layouts"), ("#", "issues")];

#[component]
pub(super) fn Palette() -> Element {
	let ctx = use_context::<Ctx>();
	let mut open = ctx.palette;
	use_command_palette_shortcut(move || open.toggle(), true);
	if !open() {
		return rsx! {};
	}
	rsx! { Open {} }
}

#[component]
fn Open() -> Element {
	let ctx = use_context::<Ctx>();
	let mut open = ctx.palette;
	let mut typed = use_signal(String::new);
	let mut at = use_signal(|| 0usize);
	let hits = hits(&ctx);
	let text = typed.read().trim().to_lowercase();
	let (only, needle) = match KINDS.iter().find(|(p, _)| text.starts_with(p)) {
		Some((p, kind)) => (Some(*kind), text[p.len()..].trim().to_string()),
		None => (None, text.clone()),
	};
	let shown: Vec<Hit> = hits
		.into_iter()
		.filter(|h| only.is_none_or(|k| k == h.kind) && h.label.to_lowercase().contains(&needle))
		.take(60)
		.collect();
	let chosen = (*at.read()).min(shown.len().saturating_sub(1));
	let mut close = move || {
		open.set(false);
		let mut into = ctx.into;
		into.set(None);
	};
	let go = {
		let shown = shown.clone();
		move |i: usize, beside: bool, page: bool| {
			let Some(hit) = shown.get(i).cloned() else { return };
			close();
			match (hit.go, page.then_some(hit.page).flatten()) {
				(_, Some(page)) => ctx.open(page),
				(Go::Panel(id), None) => {
					let mut into = ctx.into;
					match into.take() {
						Some(group) => {
							ctx.host(&id);
							(*ctx.api.peek()).expect("the palette opens over an entered dock").add_tab(group, id.clone());
							ctx.open(id);
						}
						None if beside => ctx.open_beside(id),
						None => ctx.open(id),
					}
				}
				(Go::Scope(path), None) => {
					spawn(async move { _ = ctx.keep(LibraryEdit::Scope(path)).await });
				}
				(Go::Workspace(name), None) => ctx.switch(&name),
				(Go::NewScope, None) => {
					let mut m = ctx.modal;
					m.set(Some(Modal::Scope(Editing::New)));
				}
			}
		}
	};
	let mut sections: Vec<(&'static str, Vec<(usize, Hit)>)> = Vec::new();
	for (i, h) in shown.iter().cloned().enumerate() {
		match sections.iter_mut().find(|(k, _)| *k == h.kind) {
			Some((_, list)) => list.push((i, h)),
			None => sections.push((h.kind, vec![(i, h)])),
		}
	}
	let count = shown.len();
	rsx! {
		div { class: "fixed inset-0 z-[110] flex items-start justify-center bg-scrim px-4 pt-[12vh]", onclick: move |_| close(),
			div { class: "flex w-[600px] max-w-full flex-col overflow-hidden rounded-[10px] border border-border bg-popover text-ink", onclick: |e: MouseEvent| e.stop_propagation(),
				div { class: "flex h-12 items-center gap-2 border-b border-border-subtle px-4",
					Icon { glyph: Glyph::Command, class: "size-4 text-ink-soft" }
					input {
						class: "min-w-0 flex-1 bg-transparent text-body-l text-ink outline-none placeholder:text-ink-faint",
						placeholder: "views, scopes, workspaces, issues, actions…  (v: s: w: c: #)",
						autofocus: true,
						value: typed(),
						oninput: move |e: FormEvent| {
							typed.set(e.value());
							at.set(0);
						},
						onkeydown: {
							let mut go = go.clone();
							move |e: KeyboardEvent| match e.key() {
								Key::Escape => close(),
								Key::ArrowDown => at.set((chosen + 1).min(count.saturating_sub(1))),
								Key::ArrowUp => at.set(chosen.saturating_sub(1)),
								Key::Enter => go(chosen, e.modifiers().shift(), e.modifiers().meta() || e.modifiers().ctrl()),
								_ => {}
							}
						},
					}
					Kbd { "esc" }
				}
				div { class: "flex max-h-[50vh] flex-col gap-0.5 overflow-y-auto p-1.5",
					if count == 0 {
						span { class: "px-2 py-3 text-body text-ink-soft", "Nothing named so" }
					}
					for (kind , list) in sections {
						span { key: "{kind}", class: "px-2 pt-2 pb-1 text-overline text-ink-faint", "{kind}" }
						for (i , h) in list {
							button {
								key: "{kind}-{i}",
								r#type: "button",
								class: if i == chosen { "flex h-7 shrink-0 items-center gap-2 rounded-[var(--control-radius)] bg-hover px-2 text-left" } else { "flex h-7 shrink-0 items-center gap-2 rounded-[var(--control-radius)] px-2 text-left" },
								onmouseenter: move |_| at.set(i),
								onclick: { let mut go = go.clone(); move |e: MouseEvent| go(i, e.modifiers().shift(), e.modifiers().meta() || e.modifiers().ctrl()) },
								Icon { glyph: h.glyph, class: "size-3.5 text-ink-soft" }
								span { class: "flex-1 truncate text-body text-ink", "{h.label}" }
								span { class: "text-mono-s text-ink-soft", if i == chosen { "⏎ open" } else { "{h.meta}" } }
							}
						}
					}
				}
				div { class: "flex h-8 items-center gap-3 border-t border-border-subtle bg-secondary px-4 text-caption text-ink-soft",
					span { "↑↓ move" }
					span { "⏎ open" }
					span { "⇧⏎ open in new tile" }
					span { "⌘⏎ open in library" }
				}
			}
		}
	}
}

fn hits(ctx: &Ctx) -> Vec<Hit> {
	let mut out = Vec::new();
	let library = ctx.library.read();
	let setup = ctx.setup.read();
	let (Some(library), Some(setup)) = (library.as_ref(), setup.as_ref()) else {
		return out;
	};
	for v in ctx.views() {
		let team = !library.filters.contains_key(&v.name);
		out.push(Hit {
			kind: "views",
			glyph: glyph_of(&v),
			meta: if team { "team".into() } else { "mine".into() },
			page: Some(PanelId(format!("library:views:{}", v.name))),
			go: Go::Panel(PanelId(format!("view:{}", v.name))),
			label: v.name,
		});
	}
	fn walk(prefix: &str, level: &std::collections::BTreeMap<String, tedi_core::board::Scope>, out: &mut Vec<Hit>) {
		for (name, scope) in level {
			let path = if prefix.is_empty() { name.clone() } else { format!("{prefix} / {name}") };
			out.push(Hit {
				kind: "scopes",
				glyph: Glyph::Scope,
				label: format!("Scope → {path}"),
				meta: String::new(),
				go: Go::Scope(Some(path.clone())),
				page: Some(PanelId(format!("library:scopes:{path}"))),
			});
			walk(&path, &scope.children, out);
		}
	}
	out.push(Hit {
		kind: "scopes",
		glyph: Glyph::Scope,
		label: "Scope → All repos".into(),
		meta: String::new(),
		go: Go::Scope(None),
		page: None,
	});
	walk("", &library.scopes(&setup.scopes).expect("the server serves no library whose scope lost its parent"), &mut out);
	out.push(Hit {
		kind: "scopes",
		glyph: Glyph::Plus,
		label: "New scope…".into(),
		meta: String::new(),
		go: Go::NewScope,
		page: None,
	});
	for (n, name) in library.workspaces.keys().enumerate() {
		out.push(Hit {
			kind: "workspaces",
			glyph: Glyph::Workspace,
			label: format!("Switch workspace: {name}"),
			meta: if n < 9 { format!("⌘⇧{}", n + 1) } else { String::new() },
			go: Go::Workspace(name.clone()),
			page: Some(PanelId(format!("library:workspaces:{name}"))),
		});
	}
	for name in setup.columns.keys().chain(library.columns.keys()) {
		out.push(Hit {
			kind: "column layouts",
			glyph: Glyph::Columns,
			label: name.clone(),
			meta: String::new(),
			go: Go::Panel(PanelId(format!("library:columns:{name}"))),
			page: None,
		});
	}
	for s in setup.sprints.iter() {
		out.push(Hit {
			kind: "sprints",
			glyph: Glyph::Sprint,
			label: s.title.clone(),
			meta: format!("{}", s.links.len()),
			go: Go::Panel(PanelId(format!("sprint:{}", s.title))),
			page: None,
		});
	}
	out.push(Hit {
		kind: "actions",
		glyph: Glyph::View,
		label: "Build a view".into(),
		meta: String::new(),
		go: Go::Panel(PanelId("builder:".into())),
		page: None,
	});
	if let Some(board) = ctx.board.read().as_ref() {
		for c in board.cards.iter().filter(|c| c.open()) {
			out.push(Hit {
				kind: "issues",
				glyph: if c.pr.is_some() { Glyph::Pr } else { Glyph::Issue },
				label: format!("#{} {}", c.link.number(), c.title),
				meta: c.link.project().repo().to_string(),
				go: Go::Panel(PanelId(format!("issue:{}", c.link))),
				page: None,
			});
		}
	}
	out
}
