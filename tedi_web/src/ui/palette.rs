//! ⌘K (Figma: CommandPalette): one index over every view, scope and workspace, the open
//! issues. `v: s: w: #` narrow it to one kind. ⏎ opens in place, ⇧⏎ in a new tile, ⌘⏎ as
//! its library page, ⌥⏎ scopes to it.

use dioxus::prelude::*;
use ev_lib::uikit_dioxus::use_command_palette_shortcut;
use tedi_core::chain::Point;
use tilers_dioxus::PanelId;

use super::{
	Ctx, Editing, Modal,
	icons::{Glyph, Icon},
	kit::Kbd,
	scope::glyph_at,
	sprint_panel,
};

const KINDS: [(&str, &str); 4] = [("v:", "views"), ("s:", "scopes"), ("w:", "workspaces"), ("#", "issues")];

#[derive(Clone, PartialEq)]
enum Go {
	Panel(PanelId),
	/// Copied into the open workspace.
	Scope(Option<String>),
	Point(Point),
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
	/// What ⌥⏎ scopes to.
	scope: Option<Point>,
}

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
		move |i: usize, beside: bool, page: bool, scoped: bool| {
			let Some(hit) = shown.get(i).cloned() else { return };
			close();
			if let (true, Some(point)) = (scoped, hit.scope) {
				return ctx.scope_to(point);
			}
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
				(Go::Scope(path), None) => ctx.start_from(path),
				(Go::Point(point), None) => ctx.scope_to(point),
				(Go::Workspace(name), None) => ctx.go(ctx.at_workspace(&name)),
				(Go::NewScope, None) => {
					let mut m = ctx.modal;
					m.set(Some(Modal::Scope(Editing::New(Default::default()))));
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
						placeholder: "views, scopes, workspaces, issues…  (v: s: w: #)",
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
								Key::Enter => go(chosen, e.modifiers().shift(), e.modifiers().meta() || e.modifiers().ctrl(), e.modifiers().alt()),
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
								onclick: { let mut go = go.clone(); move |e: MouseEvent| go(i, e.modifiers().shift(), e.modifiers().meta() || e.modifiers().ctrl(), e.modifiers().alt()) },
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
					span { "⌥⏎ scope to it" }
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
		let team = !library.views.contains_key(&v.name);
		out.push(Hit {
			kind: "views",
			glyph: Glyph::View,
			meta: if team { "team".into() } else { "mine".into() },
			page: Some(PanelId(format!("library:views:{}", v.name))),
			go: Go::Panel(PanelId(format!("view:{}", v.name))),
			label: v.name,
			scope: None,
		});
	}
	out.push(Hit {
		kind: "scopes",
		glyph: Glyph::Scope,
		label: "Start from → All repos".into(),
		meta: String::new(),
		go: Go::Scope(None),
		page: None,
		scope: None,
	});
	for s in ctx.scopes() {
		out.push(Hit {
			kind: "scopes",
			glyph: Glyph::Scope,
			label: format!("Start from → {}", s.path),
			meta: String::new(),
			page: Some(PanelId(format!("library:scopes:{}", s.path))),
			go: Go::Scope(Some(s.path)),
			scope: None,
		});
	}
	for place in ctx.places() {
		out.push(Hit {
			kind: "scopes",
			glyph: glyph_at(&place.point),
			label: ctx.path(&place.point),
			meta: place.open.map(|n| n.to_string()).unwrap_or_default(), // before the board arrives
			page: None,
			scope: Some(place.point.clone()),
			go: Go::Point(place.point),
		});
	}
	out.push(Hit {
		kind: "scopes",
		glyph: Glyph::Plus,
		label: "New scope…".into(),
		meta: String::new(),
		go: Go::NewScope,
		page: None,
		scope: None,
	});
	for (n, name) in setup.workspaces.keys().chain(library.workspaces.keys()).enumerate() {
		out.push(Hit {
			kind: "workspaces",
			glyph: Glyph::Workspace,
			label: format!("Switch workspace: {name}"),
			meta: if n < setup.workspaces.len() { "team".into() } else { String::new() },
			go: Go::Workspace(name.clone()),
			page: Some(PanelId(format!("library:workspaces:{name}"))),
			scope: None,
		});
	}
	for s in ctx.sprints() {
		out.push(Hit {
			kind: "sprints",
			glyph: Glyph::Sprint,
			label: s.title.clone(),
			meta: format!("{} {}", s.link.repo_info().repo(), s.nodes.len()),
			go: Go::Panel(sprint_panel(&s.link)),
			page: None,
			scope: None,
		});
	}
	for c in ctx.board.read().cards.iter().filter(|c| c.open()) {
		out.push(Hit {
			kind: "issues",
			glyph: if c.pr.is_some() { Glyph::Pr } else { Glyph::Issue },
			label: format!("#{} {}", c.link.number(), c.title),
			meta: c.link.project().repo().to_string(),
			go: Go::Panel(PanelId(format!("issue:{}", c.link))),
			page: None,
			scope: Some(Point::Issue(c.link.clone())),
		});
	}
	out
}
