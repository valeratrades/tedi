//! The open workspace and its menu (Figma: WorkspaceMenu, 04 · Workspace switching), and a shared screen
//! shown read-only under a banner until it is forked (Flow 2).

use dioxus::prelude::*;
use ev_lib::uikit_dioxus::{Popover, PopoverContent, PopoverTrigger};
use tilers_dioxus::{Band, PanelId};

use super::{
	Ctx, Modal,
	card::{Chip, Tone, ago},
	icons::{Glyph, Icon},
	kit::{Btn, MenuItem as Row, Style},
	library::NameForm,
	toast::Tone as Said,
};
use crate::api::{self, Arrangement, Construction, Item, Kept, LibraryEdit, Origin, Owner, ShareLink, Shared, Shelf, band_name};

/// Where `link` opens this board: its origin and the path it is served under.
pub(super) fn share_url(link: &ShareLink) -> String {
	let location = web_sys::window().expect("a browser window").location();
	let (origin, path) = (location.origin().expect("a page has an origin"), location.pathname().expect("a page has a path"));
	let root = path.find("/l/").map_or(path.trim_end_matches('/'), |at| &path[..at]);
	format!("{origin}{root}{link}")
}

pub(super) fn copy_link(ctx: Ctx, link: &ShareLink) {
	let url = share_url(link);
	ev_lib::uikit_dioxus::write_clipboard(&url.clone(), move |copied| match copied {
		true => ctx.say("share", Said::Success, format!("Link copied · {url}"), None),
		false => ctx.say("share", Said::Error, "the browser refused the clipboard", None),
	});
}

/// The workspace on screen: the viewer's own, or the team's.
fn current(ctx: &Ctx) -> Option<(String, Owner, Arrangement)> {
	let library = ctx.library.read();
	let setup = ctx.setup.read();
	let name = library.as_ref()?.workspace.clone()?;
	if let Some(tiles) = library.as_ref()?.workspaces.get(&name) {
		return Some((name, Owner::Viewer(String::new()), tiles.clone()));
	}
	let tiles = setup.as_ref()?.workspaces.get(&name)?.clone();
	Some((name, Owner::Team, tiles))
}

/// The screen, every band it was arranged for, the live one as it stands now.
fn screen(ctx: &Ctx) -> Arrangement {
	let mut tiles = ctx.library.read().as_ref().expect("the menu opens from a loaded screen").on_screen.tiles.clone();
	if let Some(api) = *ctx.api.peek() {
		tiles.insert(api.band(), api.save());
	}
	tiles
}

/// ⌘S: the live band into the open workspace.
fn save(ctx: Ctx) {
	let Some(api) = *ctx.api.peek() else { return };
	match current(&ctx) {
		Some((name, Owner::Viewer(_), _)) => {
			let tiles = Arrangement::from([(api.band(), api.save())]);
			spawn(async move {
				if ctx.keep(LibraryEdit::Keep(Kept::Workspace { name: name.clone(), tiles })).await {
					ctx.say("workspace", Said::Success, format!("Saved {name}"), None);
				}
			});
		}
		Some((name, Owner::Team, _)) => ctx.say("workspace", Said::Info, format!("{name} is the team's: save it as a copy"), None),
		None => ctx.say("workspace", Said::Info, "no workspace is open: keep this screen as a new one", None),
	}
}

/// ⌘⇧C: a pinned link to the open workspace, every band it arranged, in the screen's scope.
pub(super) fn share_screen(ctx: Ctx) {
	let Some((name, _, tiles)) = current(&ctx) else {
		return ctx.say("share", Said::Info, "keep this screen as a workspace to share it", None);
	};
	let scope = ctx.library.read().as_ref().and_then(|l| l.scope.clone());
	spawn(async move {
		match api::share(Item::Workspace(name), tiles.keys().copied().collect(), scope).await {
			Ok(link) => copy_link(ctx, &link),
			Err(e) => ctx.say("share", Said::Error, e.to_string(), None),
		}
	});
}

/// The screen kept as `name` and opened; a copy of the team's remembers it as its upstream.
fn keep_as(ctx: Ctx, name: String, from_team: Option<String>) {
	let tiles = screen(&ctx);
	spawn(async move {
		if let Some(team) = from_team
			&& !ctx
				.keep(LibraryEdit::Fork {
					from: Origin::Team(Item::Workspace(team)),
					name: name.clone(),
				})
				.await
		{
			return;
		}
		if ctx.keep(LibraryEdit::Keep(Kept::Workspace { name: name.clone(), tiles })).await {
			ctx.keep(LibraryEdit::Workspace(Some(name))).await;
		}
	});
}

/// ⌘S, ⌘⇧C, ⌘⇧1–9.
enum Shortcut {
	Save,
	Share,
	Recent(usize),
}

#[derive(Clone, Copy, PartialEq)]
enum Naming {
	Copy,
	Rename,
	New,
}

/// The top bar's ▾: save, copy, rename, share or publish the open workspace; the recent ones; a new one; the Library.
#[component]
pub(super) fn WorkspaceMenu() -> Element {
	let ctx = use_context::<Ctx>();
	let mut open = use_signal(|| false);
	let mut naming = use_signal(|| None::<Naming>);
	let (sender, keys) = use_hook(|| {
		let (sender, keys) = futures::channel::mpsc::channel::<Shortcut>(8);
		(std::rc::Rc::new(std::cell::RefCell::new(sender)), std::rc::Rc::new(std::cell::RefCell::new(Some(keys))))
	});
	let listener = use_hook(move || {
		use wasm_bindgen::{JsCast as _, closure::Closure};
		let sender = sender.clone();
		let listener = Closure::<dyn FnMut(web_sys::KeyboardEvent)>::new(move |e: web_sys::KeyboardEvent| {
			if !(e.meta_key() || e.ctrl_key()) {
				return;
			}
			let typing = e
				.target()
				.and_then(|t| t.dyn_into::<web_sys::Element>().ok())
				.is_some_and(|t| matches!(t.tag_name().as_str(), "INPUT" | "TEXTAREA"));
			let code = e.code();
			let shortcut = match (e.shift_key(), code.as_str()) {
				(false, "KeyS") => Shortcut::Save,
				(true, "KeyC") if !typing => Shortcut::Share,
				(true, digit) => match digit.strip_prefix("Digit").and_then(|d| d.parse::<usize>().ok()).filter(|d| (1..=9).contains(d)) {
					Some(at) => Shortcut::Recent(at),
					None => return,
				},
				_ => return,
			};
			e.prevent_default();
			_ = sender.borrow_mut().try_send(shortcut); // full only with 8 shortcuts still unhandled: this one is dropped
		});
		web_sys::window()
			.expect("a browser window")
			.add_event_listener_with_callback("keydown", listener.as_ref().unchecked_ref())
			.expect("a window takes listeners");
		std::rc::Rc::new(listener)
	});
	use_drop(move || {
		use wasm_bindgen::JsCast as _;
		web_sys::window()
			.expect("a browser window")
			.remove_event_listener_with_callback("keydown", listener.as_ref().as_ref().unchecked_ref())
			.expect("a window drops listeners");
	});
	use_future(move || {
		let keys = keys.borrow_mut().take();
		async move {
			use futures::StreamExt as _;
			let Some(mut keys) = keys else { return }; // taken by this future's first run
			while let Some(key) = keys.next().await {
				match key {
					Shortcut::Save => save(ctx),
					Shortcut::Share => share_screen(ctx),
					Shortcut::Recent(at) => {
						let recent = ctx.library.read().as_ref().map(|l| l.recent.clone()).unwrap_or_default(); // the library not yet arrived
						if let Some(name) = recent.get(at - 1) {
							ctx.switch(name);
						}
					}
				}
			}
		}
	});
	let library = ctx.library.read().clone();
	let Some(library) = library else {
		return rsx! {};
	};
	let team = ctx.setup.read().as_ref().map(|s| s.workspaces.clone()).unwrap_or_default(); // setup arrives with the library
	let at = current(&ctx);
	let own = matches!(at, Some((_, Owner::Viewer(_), _)));
	let forked = at.as_ref().is_some_and(|(n, ..)| library.forks.iter().any(|f| f.item == Item::Workspace(n.clone())));
	let mut close = move || {
		open.set(false);
		naming.set(None);
	};
	let named = move |name: String| {
		let kind = naming();
		close();
		match (kind, current(&ctx)) {
			(Some(Naming::Rename), Some((from, ..))) => {
				spawn(async move {
					_ = ctx
						.keep(LibraryEdit::Rename {
							shelf: Shelf::Workspaces,
							from,
							to: name,
						})
						.await
				});
			}
			(Some(Naming::Copy), Some((from, Owner::Team, _))) => keep_as(ctx, name, Some(from)),
			_ => keep_as(ctx, name, None),
		}
	};
	rsx! {
		Popover { open: open(), on_open_change: move |v: bool| { open.set(v); if !v { naming.set(None); } },
			PopoverTrigger { class: "flex size-6 items-center justify-center rounded-[var(--control-radius)] text-ink-soft hover:bg-hover hover:text-ink", aria_label: "Workspace menu",
				Icon { glyph: Glyph::ChevronDown }
			}
			PopoverContent { align: "end", class: "flex w-72 flex-col gap-0.5 rounded-[var(--radius)] border border-border bg-popover p-1 text-ink",
				if let Some((name, owner, tiles)) = at.clone() {
					div { class: "flex flex-col gap-1 px-2 py-1.5",
						div { class: "flex items-center gap-1.5",
							Icon { glyph: Glyph::Workspace, class: "size-3.5 text-primary-ink" }
							span { class: "truncate text-body font-semibold", "{name}" }
							span { class: "text-caption text-primary-ink",
								match (&owner, forked) {
									(Owner::Team, _) => "team",
									(_, true) => "fork",
									_ => "mine",
								}
							}
						}
						div { class: "flex items-center gap-1 text-caption text-ink-soft",
							"arranged for"
							for band in [Band::Sm, Band::Md, Band::Xl] {
								Chip { key: "{band:?}", tone: if tiles.contains_key(&band) { Tone::Primary } else { Tone::Neutral }, "{band_name(band)}" }
							}
							if let (true, Some(at)) = (own, library.autosaved) {
								span { class: "ml-auto", "autosaved {ago(at)}" }
							}
						}
					}
					div { class: "my-0.5 h-px bg-border-subtle" }
				}
				match naming() {
					Some(kind) => rsx! {
						div { class: "px-1 py-1",
							NameForm {
								initial: if kind == Naming::Rename { at.as_ref().map(|(n, ..)| n.clone()).unwrap_or_default() } else { String::new() }, // renaming starts from the name
								placeholder: match kind { Naming::Copy => "the copy's name", Naming::Rename => "new name", Naming::New => "the new workspace's name" },
								on_name: named,
								on_cancel: move |()| naming.set(None),
							}
						}
					},
					None => rsx! {
						if at.is_some() {
							Row { selected: false, glyph: Glyph::Save, label: "Save", meta: "⌘S", onclick: move |_| { close(); save(ctx); } }
							Row { selected: false, glyph: Glyph::Fork, label: "Save as copy…", onclick: move |_| naming.set(Some(Naming::Copy)) }
							if own {
								Row { selected: false, glyph: Glyph::Pencil, label: "Rename", onclick: move |_| naming.set(Some(Naming::Rename)) }
							}
							Row { selected: false, glyph: Glyph::Link, label: "Copy share link", meta: "⌘⇧C", onclick: move |_| { close(); share_screen(ctx); } }
							if own {
								Row {
									selected: false,
									glyph: Glyph::Publish,
									label: "Publish to team…",
									onclick: {
										let name = at.as_ref().map(|(n, ..)| n.clone()).expect("own");
										move |_| { close(); let mut m = ctx.modal; m.set(Some(Modal::Propose(Item::Workspace(name.clone())))); }
									},
								}
							}
						}
						if !library.recent.is_empty() {
							span { class: "px-2 pt-2 pb-1 text-overline text-ink-soft", "Recent" }
							for (i , name) in library.recent.iter().enumerate() {
								Row {
									key: "{name}",
									selected: at.as_ref().is_some_and(|(n, ..)| n == name),
									glyph: Glyph::Workspace,
									label: if team.contains_key(name) { format!("{name} · team") } else { name.clone() },
									meta: format!("⌘⇧{}", i + 1),
									onclick: { let name = name.clone(); move |_| { close(); ctx.switch(&name); } },
								}
							}
						}
						div { class: "my-0.5 h-px bg-border-subtle" }
						Row { selected: false, glyph: Glyph::Plus, label: "New workspace from screen", onclick: move |_| naming.set(Some(Naming::New)) }
						Row { selected: false, glyph: Glyph::Search, label: "Browse all workspaces…", meta: "Library", onclick: move |_| { close(); ctx.open(PanelId("library:workspaces".into())); } }
					},
				}
			}
		}
	}
}

/// `shared`, a workspace, onto the dock in place of the viewer's, nothing of it kept.
pub(super) fn view_shared(ctx: Ctx, shared: Shared) {
	let Construction::Workspace(tiles) = &shared.construction else {
		panic!("only a workspace takes the dock: {:?}", shared.link)
	};
	let api = (*ctx.api.peek()).expect("a band is entered before a link can be shown");
	let band = api.band();
	let layout = tiles.get(&band).cloned();
	let (mut viewing, mut arranged) = (ctx.viewing, ctx.arranged);
	arranged.set(None); // what the dock shows now isn't the viewer's to keep
	viewing.set(Some(shared));
	match layout {
		Some(layout) => _ = ctx.show(api, &layout),
		None => ctx.say("share", Said::Info, format!("the link carries no {} arrangement", band_name(band)), None),
	}
}

/// Opens a link this page was loaded at; over a shared workspace: whose it is, back to the viewer's own, or fork it.
#[component]
pub(super) fn Banner() -> Element {
	let ctx = use_context::<Ctx>();
	let mut landed = use_signal(|| false);
	use_effect(move || {
		let ready = ctx.setup.read().is_some() && ctx.library.read().is_some() && ctx.api.read().is_some();
		if !ready || *landed.peek() {
			return;
		}
		landed.set(true);
		let location = web_sys::window().expect("a browser window").location();
		let (path, search) = (location.pathname().expect("a page has a path"), location.search().expect("a page has a query"));
		let Some(at) = path.find("/l/") else { return };
		let link = format!("{}{search}", &path[at..]);
		let root = if at == 0 { "/" } else { &path[..at] };
		// the board's own address again: a reload shows the viewer's screen, the link stays in Shared with me
		web_sys::window()
			.expect("a browser window")
			.history()
			.expect("a window has a history")
			.replace_state_with_url(&wasm_bindgen::JsValue::NULL, "", Some(root))
			.expect("a same-origin path replaces the address");
		spawn(async move {
			let link: ShareLink = match link.parse() {
				Ok(link) => link,
				Err(e) => return ctx.say("share", Said::Error, e, None),
			};
			match api::shared(link.clone()).await {
				Ok(shared) if matches!(shared.construction, Construction::Workspace(_)) => view_shared(ctx, shared),
				Ok(_) => ctx.open(PanelId(format!("library:link:{link}"))),
				Err(e) => ctx.say("share", Said::Error, e.to_string(), None),
			}
		});
	});
	let viewing = ctx.viewing.read().clone();
	let Some(shared) = viewing else {
		return rsx! {};
	};
	let back = move || {
		let mut viewing = ctx.viewing;
		viewing.set(None);
		if let Some(api) = *ctx.api.peek() {
			ctx.arrange(api);
		}
	};
	let name = shared.link.item.name().to_string();
	rsx! {
		div { class: "flex h-9 shrink-0 items-center gap-2 border-b border-chip-info-border bg-selected px-3 text-body",
			Icon { glyph: Glyph::Eye, class: "size-3.5 text-accent-info-ink" }
			span { class: "min-w-0 flex-1 truncate", "@{shared.link.owner}'s workspace “{name}” · read-only" }
			Btn { style: Style::Ghost, onclick: move |_| back(), "Back to mine" }
			Btn {
				style: Style::Primary,
				glyph: Glyph::Fork,
				onclick: {
					let (link, name) = (shared.link.clone(), name.clone());
					move |_| {
						let (from, name) = (Origin::Link(link.clone()), name.clone());
						spawn(async move {
							if ctx.keep(LibraryEdit::Fork { from, name: name.clone() }).await && ctx.keep(LibraryEdit::Workspace(Some(name.clone()))).await {
								let mut viewing = ctx.viewing;
								viewing.set(None);
								ctx.switch(&name);
							}
						});
					}
				},
				"Fork"
			}
		}
	}
}
