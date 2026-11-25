//! The open workspace's name and its menu (Figma: WorkspaceSelect, WorkspaceMenu, 04 · Workspace switching), and a
//! shared one shown read-only under a banner until it is forked (Flow 2).

use dioxus::prelude::*;
use ev_lib::uikit_dioxus::{Popover, PopoverContent, PopoverTrigger};
use tilers_dioxus::{Band, PanelId};

use super::{
	Ctx, Modal,
	card::{Chip, Tone, ago},
	icons::{Glyph, Icon},
	kit::{Btn, MenuItem as Row, Style},
	library::NameForm,
	route,
	toast::Tone as Said,
};
use crate::api::{self, Construction, Item, Kept, LibraryEdit, Origin, Owner, ShareLink, Shared, Shelf, Workspace, band_name};

/// Where `link` opens this board: its origin and the path it is served under.
pub(super) fn share_url(link: &ShareLink) -> String {
	let origin = web_sys::window().expect("a browser window").location().origin().expect("a page has an origin");
	format!("{origin}{}{link}", route::base())
}

pub(super) fn copy_link(ctx: Ctx, link: &ShareLink) {
	let url = share_url(link);
	ev_lib::uikit_dioxus::write_clipboard(&url.clone(), move |copied| match copied {
		true => ctx.say("share", Said::Success, format!("Link copied · {url}"), None),
		false => ctx.say("share", Said::Error, "the browser refused the clipboard", None),
	});
}

/// The open workspace, as last edited, and whose it is.
fn current(ctx: &Ctx) -> Option<(String, Owner, Workspace)> {
	if ctx.viewing.read().is_some() {
		return None; // another viewer's is on the dock
	}
	let name = ctx.open_workspace()?;
	let own = ctx.library.read().as_ref()?.workspaces.contains_key(&name);
	let workspace = ctx.workspace_named(&name)?;
	Some((name, if own { Owner::Viewer(String::new()) } else { Owner::Team }, workspace))
}

/// The workspaces last selected that are still there.
fn recent(ctx: &Ctx) -> Vec<String> {
	let library = ctx.library.read();
	let names = library.iter().flat_map(|l| l.recent.iter().cloned());
	names.filter(|n| ctx.id_of(&Item::Workspace(n.clone())).is_some()).collect()
}

/// ⌘S: what is yet to settle, saved now.
fn save(ctx: Ctx) {
	match current(&ctx) {
		Some((name, Owner::Viewer(_), _)) =>
			_ = spawn(async move {
				ctx.flush().await;
				ctx.say("workspace", Said::Success, format!("Saved {name}"), None);
			}),
		Some((name, Owner::Team, _)) => ctx.say("workspace", Said::Info, format!("{name} is the team's: save it as a copy"), None),
		None => ctx.none_open(),
	}
}

/// ⌘⇧C: a pinned link to the open workspace, every band it arranged and its lens.
pub(super) fn share_screen(ctx: Ctx) {
	let Some((name, owner, workspace)) = current(&ctx) else {
		return ctx.none_open();
	};
	let drafted = ctx.library.read().as_ref().is_some_and(|l| l.drafts.workspaces.contains_key(&name));
	spawn(async move {
		ctx.flush().await;
		match api::share(Item::Workspace(name.clone()), workspace.tiles.keys().copied().collect()).await {
			Ok(link) => copy_link(ctx, &link),
			Err(e) => return ctx.say("share", Said::Error, e.to_string(), None),
		}
		if owner == Owner::Team && drafted {
			ctx.say("workspace", Said::Info, format!("{name} is linked as the team has it: save yours as a copy to share that"), None);
		}
	});
}

/// The open workspace, as last edited, kept as `name` and opened; a copy of the team's remembers it as its upstream.
fn keep_as(ctx: Ctx, name: String, from: (String, Owner, Workspace)) {
	let (upstream, owner, workspace) = from;
	spawn(async move {
		if owner == Owner::Team
			&& !ctx
				.keep(LibraryEdit::Fork {
					from: Origin::Team(Item::Workspace(upstream)),
					name: name.clone(),
				})
				.await
		{
			return;
		}
		if ctx.keep(LibraryEdit::Keep(Kept::Workspace { name: name.clone(), workspace })).await {
			ctx.go(ctx.at_workspace(&name));
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
}

/// The open workspace, by name: ▾ saves, copies, renames, shares or publishes it; offers the recent ones, a new
/// one, the Library.
#[component]
pub(super) fn WorkspaceSelect() -> Element {
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
					Shortcut::Recent(at) =>
						if let Some(name) = recent(&ctx).get(at - 1) {
							ctx.go(ctx.at_workspace(name));
						},
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
	let recent = recent(&ctx);
	let own = matches!(at, Some((_, Owner::Viewer(_), _)));
	let drafted = at
		.as_ref()
		.is_some_and(|(n, ..)| !own && ctx.setup.read().as_ref().and_then(|s| s.workspaces.get(n).cloned()) != ctx.workspace_named(n));
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
			(Some(Naming::Copy), Some(from)) => keep_as(ctx, name, from),
			_ => {} // gone while it was being named
		}
	};
	rsx! {
		Popover { open: open(), on_open_change: move |v: bool| { open.set(v); if !v { naming.set(None); } },
			PopoverTrigger { class: if open() { "flex h-7 min-w-0 items-center gap-1.5 rounded-[var(--control-radius)] border border-ring px-2" } else { "flex h-7 min-w-0 items-center gap-1.5 rounded-[var(--control-radius)] border border-border px-2 hover:bg-hover" }, aria_label: "Workspace menu",
				Icon { glyph: Glyph::Workspace, class: "size-3.5 shrink-0 text-chart-6" }
				match &at {
					Some((name, ..)) => rsx! { span { class: "truncate text-body font-medium text-ink", "{name}" } },
					None => rsx! { span { class: "text-body font-medium text-ink-soft", "Workspace" } },
				}
				Icon { glyph: Glyph::ChevronDown, class: "size-3.5 shrink-0 text-ink-soft" }
			}
			PopoverContent { align: "start", class: "flex w-72 flex-col gap-0.5 rounded-[var(--radius)] border border-border bg-popover p-1 text-ink",
				if let Some((name, owner, Workspace { tiles, .. })) = at.clone() {
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
							if let (true, Some(at)) = (own, library.autosaved.get(&name)) {
								span { class: "ml-auto", "autosaved {ago(*at)}" }
							}
							if drafted {
								span { class: "ml-auto", "modified" }
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
								placeholder: match kind { Naming::Copy => "the copy's name", Naming::Rename => "new name" },
								on_name: named,
								on_cancel: move |()| naming.set(None),
							}
						}
					},
					None => rsx! {
						if at.is_some() {
							if own {
								Row { selected: false, glyph: Glyph::Save, label: "Save", meta: "⌘S", onclick: move |_| { close(); save(ctx); } }
							}
							if drafted {
								Row {
									selected: false,
									glyph: Glyph::Revert,
									label: "Revert",
									onclick: {
										let name = at.as_ref().map(|(n, ..)| n.clone()).expect("drafted");
										move |_| {
											close();
											let name = name.clone();
											spawn(async move {
												if ctx.keep(LibraryEdit::Revert(Item::Workspace(name.clone()))).await {
													ctx.switch(&name).await;
												}
											});
										}
									},
								}
							}
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
						if !recent.is_empty() {
							span { class: "px-2 pt-2 pb-1 text-overline text-ink-soft", "Recent" }
							for (i , name) in recent.iter().enumerate() {
								Row {
									key: "{name}",
									selected: at.as_ref().is_some_and(|(n, ..)| n == name),
									glyph: Glyph::Workspace,
									label: if team.contains_key(name) { format!("{name} · team") } else { name.clone() },
									meta: format!("⌘⇧{}", i + 1),
									onclick: { let name = name.clone(); move |_| { close(); ctx.go(ctx.at_workspace(&name)); } },
								}
							}
						}
						div { class: "my-0.5 h-px bg-border-subtle" }
						Row { selected: false, glyph: Glyph::Plus, label: "New workspace", onclick: move |_| { close(); spawn(ctx.new_workspace()); } }
						Row { selected: false, glyph: Glyph::Search, label: "Browse all workspaces…", meta: "Library", onclick: move |_| { close(); ctx.open(PanelId("library:workspaces".into())); } }
					},
				}
			}
		}
	}
}

/// `shared`, a workspace, onto the dock in place of the viewer's, nothing of it kept.
pub(super) fn view_shared(ctx: Ctx, shared: Shared) {
	let Construction::Workspace(Workspace { tiles, .. }) = &shared.construction else {
		panic!("only a workspace takes the dock: {:?}", shared.link)
	};
	let api = (*ctx.api.peek()).expect("a band is entered before a link can be shown");
	let band = api.band();
	let tiles = tiles.clone();
	super::notice(api, ctx.seen, |made| ctx.keep_tiles(made)); // before the dock is another's
	let (mut viewing, mut seen) = (ctx.viewing, ctx.seen);
	viewing.set(Some(shared));
	if !tiles.contains_key(&band) {
		ctx.say("share", Said::Info, format!("the link carries no {} arrangement", band_name(band)), None);
	}
	ctx.lay_out(api, Some(tiles));
	seen.set(None); // what the dock shows now isn't the viewer's to keep
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
		let link = route::bar();
		if !link.starts_with("/l/") {
			return;
		}
		// the board's own address again: a reload shows the viewer's screen, the link stays in Shared with me
		route::history(route::Step::Replace, &route::Address::default());
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
							if ctx.keep(LibraryEdit::Fork { from, name: name.clone() }).await {
								ctx.go(ctx.at_workspace(&name));
							}
						});
					}
				},
				"Fork"
			}
		}
	}
}
