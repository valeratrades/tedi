//! The client: a sidebar of workspaces, views, scopes and sprints over a tiled workspace, every tile a view or
//! an issue. Cards arrive whole and are queried here; edits leave as server fns. What is on screen, and what
//! the viewer keeps, is their library on the server.

mod card;
mod facets;
mod icons;
mod issue;
mod kit;
mod library;
mod scope;
mod shell;
mod toast;
mod toolbar;
mod view;

use std::collections::{BTreeMap, HashMap};

use dioxus::prelude::*;
use ev_lib::uikit_dioxus::*;
use futures::StreamExt as _;
use icons::{Glyph, Icon};
use tedi_core::{
	IssueLink,
	board::{Board, Card, Scope, View, scope_at},
	conflict::{Conflict, FieldEdit},
	workflow::{State, Workflow},
};
use tilers_dioxus::{Band, DockPanel, Group, GroupId, MinSize, PackedApi, PackedArea, PanelId, Tile, model::serial};

use crate::api::{self, Dock, Edited, Item, Library, LibraryEdit, Setup};

const HELP: [(&str, &str); 6] = [
	("⌘K", "jump to a view, scope, sprint or issue"),
	("⇧⏎", "in the palette: open in a new tile"),
	("⇧-click", "a facet value or a card's chip: rule it out"),
	("⌥-click", "an importance or hours bound: the exact value"),
	("drag", "a sidebar row onto the dock: open it there"),
	("drag", "a card onto a column: move it there"),
];

#[component]
pub fn App() -> Element {
	rsx! {
		Fonts {}
		Shell {}
	}
}

/// Shared by every tile.
#[derive(Clone, Copy)]
struct Ctx {
	setup: Signal<Option<Setup>>,
	library: Signal<Option<Library>>,
	board: Signal<Option<Board>>,
	/// Edits that came back clashing, shown on their issue until answered.
	conflicts: Signal<HashMap<IssueLink, Conflict>>,
	panels: Signal<Vec<DockPanel>>,
	api: Signal<Option<PackedApi>>,
	/// The band the dock was last arranged for from the library: until then, what it shows isn't the viewer's to keep.
	arranged: Signal<Option<Band>>,
	/// Why setup or the library didn't arrive: nothing renders without both.
	failure: Signal<Option<String>>,
	/// The dialog over the screen.
	modal: Signal<Option<Modal>>,
	/// The command palette, open.
	palette: Signal<bool>,
	/// The tile the palette's pick lands in as a tab, when its + opened it.
	into: Signal<Option<GroupId>>,
	/// On a phone, the shelves' sheet, open.
	sheet: Signal<bool>,
	/// The outcomes on screen, oldest first.
	notes: Signal<Vec<toast::Note>>,
}
impl Ctx {
	/// An outcome, replacing any on screen under `key`.
	pub(super) fn say(&self, key: impl Into<String>, tone: toast::Tone, text: impl Into<String>, action: Option<(&'static str, Callback<()>)>) {
		let mut notes = self.notes;
		let key = key.into();
		let id = notes.peek().iter().map(|n| n.id).max().map_or(0, |m| m + 1);
		let mut notes = notes.write();
		notes.retain(|n| n.key != key);
		notes.push(toast::Note {
			id,
			key,
			tone,
			text: text.into(),
			action,
		});
	}

	pub(super) fn unsay(&self, id: u64) {
		let mut notes = self.notes;
		notes.write().retain(|n| n.id != id);
	}

	fn card(&self, link: &IssueLink) -> Option<Card> {
		self.board.read().as_ref()?.cards.iter().find(|c| &c.link == link).cloned()
	}

	/// What cards sit in, by name.
	fn workflows(&self) -> BTreeMap<String, Workflow> {
		self.setup.read().as_ref().expect("tiles render once setup arrived").workflows.clone()
	}

	fn me(&self) -> Option<String> {
		self.setup.read().as_ref()?.me.clone()
	}

	/// The team views, then the viewer's own.
	fn views(&self) -> Vec<View> {
		let setup = self.setup.read();
		let library = self.library.read();
		let team = setup.as_ref().map(|s| s.views.clone()).into_iter().flatten();
		team.chain(library.as_ref().map(|l| l.filters.values().cloned().collect::<Vec<_>>()).into_iter().flatten())
			.collect()
	}

	/// The scope everything on screen is narrowed to: its path, and its filter while it is still configured.
	fn scope(&self) -> Option<(String, Option<String>)> {
		let path = self.library.read().as_ref()?.scope.clone()?;
		let at = scope_at(&self.scopes(), &path);
		Some((path, at))
	}

	/// The team's, then the viewer's own.
	fn scopes(&self) -> BTreeMap<String, Scope> {
		let library = self.library.read();
		let setup = self.setup.read();
		library
			.as_ref()
			.expect("read once the library arrived")
			.scopes(&setup.as_ref().expect("arrives with the library").scopes)
	}

	fn favorite(&self, what: &Item) -> bool {
		self.library.read().as_ref().is_some_and(|l| l.favorites.contains(what))
	}

	fn open_issue(&self, link: IssueLink) {
		self.open(PanelId(format!("issue:{link}")));
	}

	/// Shows `id`, hosting it first if it isn't: as a tab of the largest tile holding its kind (issues one,
	/// views and sprints the other), else beside the widest tile, which gives up the room.
	fn open(&self, id: PanelId) {
		let mut api = (*self.api.peek()).expect("a band is entered before anything can be opened");
		self.host(&id);
		let mut grid = serial::load(&api.save()).expect("a layout loads back from its own save");
		if let Some(cell) = grid.cells.iter_mut().find(|c| c.group.tabs.contains(&id)) {
			cell.group.active = cell.group.tabs.iter().position(|t| *t == id).expect("found in it");
			api.load(&serial::save(&grid)).expect("its own save, another tab shown");
			return;
		}
		let issue = |p: &PanelId| p.0.starts_with("issue:");
		if let Some(cell) = grid.cells.iter().filter(|c| c.group.tabs.iter().any(|t| issue(t) == issue(&id))).max_by_key(|c| c.w * c.h) {
			api.add_tab(cell.group.id, id.clone());
			return self.open(id);
		}
		let group = Group::new(api.mint_group_id(), id.clone());
		let (cols, rows) = (api.cols(), api.rows());
		match grid.cells.iter().enumerate().max_by_key(|(_, c)| c.w) {
			None => api.place(group, cols, rows),
			Some((i, widest)) => {
				let w = if issue(&id) { cols * 3 / 8 } else { widest.w / 2 };
				if widest.w > w {
					api.resize(i, widest.w - w, widest.h);
				}
				api.place(group, w, widest.h);
			}
		}
	}

	/// The dock's tabs `from` renamed `to`: a view kept under another name.
	fn retitle(&self, from: &PanelId, to: PanelId) {
		let mut api = (*self.api.peek()).expect("a band is entered before anything can be renamed");
		let mut grid = serial::load(&api.save()).expect("a layout loads back from its own save");
		let Some(tab) = grid.cells.iter_mut().flat_map(|c| c.group.tabs.iter_mut()).find(|t| *t == from) else {
			return; // not on screen
		};
		*tab = to.clone();
		self.host(&to);
		api.load(&serial::save(&grid)).expect("its own save, one tab renamed");
	}

	/// A sidebar item started a drag: wherever it drops on the dock, `id` lands.
	fn carry(&self, id: PanelId) {
		let mut api = (*self.api.peek()).expect("a band is entered before the sidebar can be dragged from");
		self.host(&id);
		let (w, h) = size(&id);
		api.carry(id, w, h);
	}

	fn host(&self, id: &PanelId) {
		let mut panels = self.panels;
		if !panels.peek().iter().any(|p| &p.id == id) {
			let panel = self.panel(id.clone());
			panels.write().push(panel);
		}
	}

	/// The panel `id` names, its content looked up afresh: a layout holds ids only.
	fn panel(&self, id: PanelId) -> DockPanel {
		let setup = self.setup.peek();
		let setup = setup.as_ref().expect("panels are hosted once setup arrived");
		let (glyph, title, content) = match id.0.split_once(':') {
			Some(("view", name)) => (
				self.views().iter().find(|v| v.name == name).map_or(Glyph::View, shell::glyph_of),
				name.to_string(),
				rsx! { view::ViewPanel { name: name.to_string() } },
			),
			Some(("sprint", title)) => match setup.sprints.iter().find(|s| s.title == title) {
				Some(sprint) => (Glyph::Sprint, title.to_string(), rsx! { view::SprintPanel { sprint: sprint.clone() } }),
				None => (Glyph::Sprint, title.to_string(), rsx! { "The sprint {title} is closed" }),
			},
			Some(("issue", link)) => match IssueLink::parse(link) {
				Some(link) => {
					let title = self.card(&link).map_or_else(|| format!("#{}", link.number()), |c| format!("#{} {}", link.number(), c.title));
					(Glyph::Issue, title, rsx! { issue::IssuePanel { link } })
				}
				None => (Glyph::Alert, id.0.clone(), rsx! { "Not an issue: {link}" }),
			},
			_ => (Glyph::Alert, id.0.clone(), rsx! { "Unknown panel {id.0}" }),
		};
		DockPanel {
			label: Some(self.tab_label(glyph, &title, &id)),
			id,
			title,
			content,
			min: MinSize::Rem { w: 16.0, h: 8.0 },
		}
	}

	/// A tab as the design draws it (Figma: Tab): what it holds, its title, and × to close it.
	pub(super) fn tab_label(&self, glyph: Glyph, title: &str, id: &PanelId) -> Element {
		let (api, id, title) = (self.api, id.clone(), title.to_string());
		rsx! {
			span { class: "group/tab flex min-w-0 items-center gap-1.5",
				Icon { glyph, class: "size-3.5 shrink-0 text-ink-soft" }
				span { class: "truncate text-body", "{title}" }
				button {
					r#type: "button",
					class: "flex size-4 shrink-0 items-center justify-center rounded text-ink-faint opacity-0 group-hover/tab:opacity-100 hover:bg-hover hover:text-ink [.tl-active_&]:opacity-100",
					aria_label: "Close {title}",
					onclick: move |_| (*api.peek()).expect("a tab shows on an entered dock").close(&id),
					Icon { glyph: Glyph::X, class: "size-3" }
				}
			}
		}
	}

	/// \`edits\` to \`link\`, as seen on its card: a landed one can be undone from its note, a clash lands on the
	/// issue's tab, inline.
	async fn edit(self, link: IssueLink, edits: Vec<FieldEdit>) {
		let fields = edits.iter().map(|e| e.field.to_string()).collect::<Vec<_>>().join(", ");
		let back: Vec<FieldEdit> = edits
			.iter()
			.map(|e| FieldEdit {
				field: e.field,
				seen: e.new.clone(),
				new: e.seen.clone(),
			})
			.collect();
		let key = format!("edit:{link}");
		match api::edit(link.clone(), edits).await {
			Ok(Edited::Landed) => {
				let mut conflicts = self.conflicts;
				conflicts.write().remove(&link);
				let undo = Callback::new({
					let link = link.clone();
					move |()| {
						spawn(self.edit(link.clone(), back.clone()));
					}
				});
				self.say(key, toast::Tone::Info, format!("Edited {fields} on #{}", link.number()), Some(("Undo", undo)));
			}
			Ok(Edited::Conflict(conflict)) => self.clashed(link, conflict),
			Ok(Edited::Rejected(why)) => self.refused(key, link, format!("Edit refused: {why}")),
			Err(e) => self.say(key, toast::Tone::Error, e.to_string(), None),
		}
	}

	/// \`card\` to \`to\` of \`workflow\`, from where it shows sitting: a landed move can be undone, a hook's
	/// refusal says why, a clash lands on the issue's tab.
	async fn move_card(self, card: Card, workflow: String, to: State) {
		let seen = card.states[&workflow].clone();
		let key = format!("move:{}", card.link);
		match api::move_card(card.link.clone(), workflow.clone(), to.clone(), seen.clone()).await {
			Ok(Edited::Landed) => {
				let mut conflicts = self.conflicts;
				conflicts.write().remove(&card.link);
				let mut moved = card.clone();
				moved.states.insert(workflow.clone(), to.clone());
				let undo = Callback::new(move |()| {
					spawn(self.move_card(moved.clone(), workflow.clone(), seen.clone()));
				});
				self.say(key, toast::Tone::Info, format!("Moved #{} → {to}", card.link.number()), Some(("Undo", undo)));
			}
			Ok(Edited::Conflict(conflict)) => self.clashed(card.link, conflict),
			Ok(Edited::Rejected(why)) => self.refused(key, card.link, format!("Move refused: {why}")),
			Err(e) => self.say(key, toast::Tone::Error, e.to_string(), None),
		}
	}

	fn clashed(self, link: IssueLink, conflict: Conflict) {
		let fields = conflict.clashes.iter().map(|c| c.field.to_string()).collect::<Vec<_>>().join(", ");
		let mut conflicts = self.conflicts;
		conflicts.write().insert(link.clone(), conflict);
		let resolve = Callback::new({
			let link = link.clone();
			move |()| self.open_issue(link.clone())
		});
		self.say(
			format!("clash:{link}"),
			toast::Tone::Warn,
			format!("{fields} on #{} changed on GitHub", link.number()),
			Some(("Resolve", resolve)),
		);
		self.open_issue(link);
	}

	fn refused(self, key: String, link: IssueLink, why: String) {
		let open = Callback::new(move |()| self.open_issue(link.clone()));
		self.say(key, toast::Tone::Error, why, Some(("Open", open)));
	}

	/// Whether it landed; why not is toasted.
	async fn keep(self, edit: LibraryEdit) -> bool {
		match api::edit_library(edit).await {
			Ok(library) => {
				let mut l = self.library;
				l.set(Some(library));
				true
			}
			Err(e) => {
				self.say("library", toast::Tone::Error, e.to_string(), None);
				false
			}
		}
	}

	/// The band's tiles as the library has them, else the first view.
	fn arrange(self, api: PackedApi) {
		let (true, Some(library)) = (self.setup.peek().is_some(), self.library.peek().clone()) else {
			return; // `Live` arranges once both arrived
		};
		let band = api.band();
		let shown = match library.on_screen.tiles.get(&band) {
			Some(layout) => self.show(api, layout),
			None => {
				if let Some(view) = self.views().into_iter().next() {
					self.open(PanelId(format!("view:{}", view.name)));
				}
				true
			}
		};
		if shown {
			let mut arranged = self.arranged;
			arranged.set(Some(band));
		}
	}

	/// `layout` on the dock, its panels hosted first, cut to the rows this window shows: a row is a fixed height,
	/// so a layout kept on a taller window would scroll here.
	fn show(self, mut api: PackedApi, layout: &str) -> bool {
		match serial::load(layout) {
			Ok(mut grid) => {
				for id in grid.cells.iter().flat_map(|c| c.group.tabs.iter()) {
					self.host(id);
				}
				let rows = api.rows();
				for cell in grid.cells.iter_mut().filter(|c| c.y < rows && c.y + c.h > rows) {
					cell.h = rows - cell.y;
				}
				api.load(&serial::save(&grid)).expect("parsed above, heights cut");
				true
			}
			Err(e) => {
				self.say(
					"layout",
					toast::Tone::Error,
					format!("the kept layout doesn't load ({e:?}); nothing kept is overwritten until another is shown"),
					None,
				);
				false
			}
		}
	}

	/// Workspace `name` onto the dock, if it has this band's.
	fn switch(self, name: &str) {
		let api = (*self.api.peek()).expect("a band is entered before a workspace can be switched to");
		let band = api.band();
		let library = self.library.peek();
		let workspace = library.as_ref().and_then(|l| l.workspaces.get(name)).expect("offered from the library");
		match workspace.get(&band) {
			Some(layout) => {
				let layout = layout.clone();
				drop(library);
				if self.show(api, &layout) {
					let mut arranged = self.arranged;
					arranged.set(Some(band));
				}
			}
			None => self.say(
				"workspace",
				toast::Tone::Warn,
				format!("{name} keeps no {band} arrangement: arrange this screen, then save it as {name}"),
				None,
			),
		}
	}
}

#[derive(Clone, Debug, PartialEq)]
enum Modal {
	Scope(Editing),
	/// The stored issues the board couldn't read, and why.
	Unreadable,
	/// The keys, the dock's and the board's.
	Help,
}

#[derive(Clone, Debug, PartialEq)]
enum Editing {
	New,
	/// One of the viewer's, by name.
	Own(String),
	/// The team's, by path: saved, it becomes the viewer's own.
	Team(String),
}

/// What a tile wants when it opens.
fn size(id: &PanelId) -> (u32, u32) {
	match id.0.split_once(':').map(|(kind, _)| kind) {
		Some("sprint") => (24, 18),
		Some("issue") => (40, 14),
		_ => (40, 18),
	}
}

/// Sends what `api`'s dock shows as `dock`'s, once it settles, from the moment it was arranged for its band.
fn use_autosave(ctx: Ctx, api: Signal<Option<PackedApi>>, arranged: Signal<Option<Band>>, dock: Dock) {
	let mut pending = use_signal(|| None::<dioxus::core::Task>);
	let mut sent = use_signal(|| None::<(Band, String)>);
	use_effect(move || {
		let Some(api) = *api.read() else { return };
		let (band, layout) = (api.band(), api.save());
		if *arranged.read() != Some(band) || sent.peek().as_ref() == Some(&(band, layout.clone())) {
			return;
		}
		if let Some(task) = pending.take() {
			task.cancel();
		}
		let dock = dock.clone();
		pending.set(Some(spawn(async move {
			gloo_timers::future::sleep(std::time::Duration::from_millis(800)).await;
			sent.set(Some((band, layout.clone())));
			ctx.keep(LibraryEdit::Arranged { dock, band, layout }).await;
		})));
	});
}

#[component]
fn Shell() -> Element {
	let ctx = use_context_provider(|| Ctx {
		setup: Signal::new(None),
		library: Signal::new(None),
		board: Signal::new(None),
		conflicts: Signal::new(HashMap::new()),
		panels: Signal::new(Vec::new()),
		api: Signal::new(None),
		arranged: Signal::new(None),
		failure: Signal::new(None),
		modal: Signal::new(None),
		palette: Signal::new(false),
		into: Signal::new(None),
		sheet: Signal::new(false),
		notes: Signal::new(Vec::new()),
	});
	use_autosave(ctx, ctx.api, ctx.arranged, Dock::Tiles);
	rsx! {
		Live {}
		div { class: "flex h-svh overflow-hidden bg-background text-ink",
			shell::Nav {}
			div { class: "flex min-w-0 flex-1 flex-col",
				shell::TopBar {}
				div { class: "relative min-h-0 flex-1",
					if let Some(why) = ctx.failure.read().clone() {
						div { class: "absolute inset-0 z-10 flex items-start justify-center bg-background p-8",
							view::Empty { glyph: icons::Glyph::Alert, error: true, title: "The board didn't load", body: why }
						}
					}
					div { class: "absolute inset-0",
						PackedArea {
							panels: ctx.panels,
							on_band: Some(Callback::new(move |api: PackedApi| {
								let (mut handle, mut arranged) = (ctx.api, ctx.arranged);
								handle.set(Some(api));
								arranged.set(None);
								ctx.arrange(api);
							})),
							config: None,
							actions: Some(Callback::new(|tile: Tile| rsx! { TileActions { tile } })),
						}
					}
				}
			}
		}
		Modals {}
		Palette {}
		toast::Toasts {}
	}
}

/// Setup and the library, then the board, reloaded on every change the server announces.
#[component]
fn Live() -> Element {
	let ctx = use_context::<Ctx>();
	use_future(move || async move {
		let mut failure = ctx.failure;
		match futures::join!(api::setup(), api::library()) {
			(Ok(setup), Ok(library)) => {
				let (mut s, mut l) = (ctx.setup, ctx.library);
				s.set(Some(setup));
				l.set(Some(library));
				if let Some(api) = *ctx.api.peek() {
					ctx.arrange(api);
				}
			}
			(Err(e), _) => return failure.set(Some(format!("setup: {e}"))),
			(_, Err(e)) => return failure.set(Some(format!("your library: {e}"))),
		}
		loop {
			match api::changes().await {
				Ok(mut ticks) => {
					reload(ctx).await;
					while let Some(tick) = ticks.next().await {
						match tick {
							Ok(()) => reload(ctx).await,
							Err(e) => ctx.say("live", toast::Tone::Error, format!("live updates: {e}"), None),
						}
					}
				}
				Err(e) => ctx.say("live", toast::Tone::Error, format!("live updates: {e}"), None),
			}
			gloo_timers::future::sleep(std::time::Duration::from_secs(3)).await; // the server restarted or the network dropped: reconnect
		}
	});
	rsx! {}
}

async fn reload(ctx: Ctx) {
	match api::board().await {
		Ok(board) => {
			let mut b = ctx.board;
			b.set(Some(board));
		}
		Err(e) => ctx.say("board", toast::Tone::Error, format!("board: {e}"), None),
	}
}

/// A tile's header, right of its tabs (Figma: TileBar): + a panel into it, split the shown one off, maximize, close.
#[component]
fn TileActions(tile: Tile) -> Element {
	let ctx = use_context::<Ctx>();
	let Tile { group, active, maximized, .. } = tile;
	let tabs = {
		let api = (*ctx.api.read()).expect("a tile shows on an entered dock");
		let grid = serial::load(&api.save()).expect("a layout loads back from its own save");
		grid.cells.into_iter().find(|c| c.group.id == group).map(|c| c.group.tabs).expect("the tile is the dock's")
	};
	let api = move || (*ctx.api.peek()).expect("a tile shows on an entered dock");
	rsx! {
		kit::IconBtn { glyph: Glyph::Plus, title: "Add a panel here", onclick: move |_| { let (mut into, mut p) = (ctx.into, ctx.palette); into.set(Some(group)); p.set(true); } }
		if tabs.len() > 1 {
			kit::IconBtn {
				glyph: Glyph::Split,
				title: "Split it off beside",
				onclick: {
					let active = active.clone();
					move |_| {
						let mut a = api();
						a.close(&active);
						let (w, h) = size(&active);
						let g = a.mint_group_id();
						a.place(Group::new(g, active.clone()), w, h);
					}
				},
			}
		}
		kit::IconBtn { glyph: Glyph::Maximize, title: if maximized { "Restore" } else { "Maximize" }, active: maximized, onclick: move |_| api().maximize(group) }
		kit::IconBtn { glyph: Glyph::X, title: "Close the tile", onclick: move |_| { let mut a = api(); for t in tabs.iter() { a.close(t); } } }
	}
}

/// Over everything the dock draws, its grips included.
#[component]
fn Modals() -> Element {
	let ctx = use_context::<Ctx>();
	let close = move |v: bool| {
		if !v {
			let mut m = ctx.modal;
			m.set(None);
		}
	};
	let modal = ctx.modal.read().clone();
	rsx! {
		div { class: "relative z-[100]",
			match modal {
				None => rsx! {},
				Some(Modal::Scope(editing)) => rsx! { scope::ScopeEditor { key: "{editing:?}", editing } },
				Some(Modal::Help) => rsx! {
					Dialog { open: true, on_open_change: close,
						DialogContent { class: "flex flex-col gap-3 sm:max-w-md",
							DialogHeader { DialogTitle { "Keys" } }
							for (keys , what) in HELP {
								div { key: "{keys}", class: "flex items-center gap-3 text-body",
									span { class: "flex-1 text-ink-mid", "{what}" }
									kit::Kbd { "{keys}" }
								}
							}
						}
					}
				},
				Some(Modal::Unreadable) => {
					let board = ctx.board.read();
					let files = board.as_ref().map(|b| b.unreadable.clone()).expect("offered from the board's");
					rsx! {
						Dialog { open: true, on_open_change: close,
							DialogContent { class: "max-h-[80vh] overflow-y-auto sm:max-w-4xl",
								DialogHeader { DialogTitle { "Stored issues that don't parse" } }
								for (path , why) in files {
									div { key: "{path.display()}", class: "flex flex-col gap-1 border-t pt-2",
										code { class: "break-all text-xs", "{path.display()}" }
										pre { class: "min-w-0 overflow-x-auto whitespace-pre text-xs text-ink-soft", "{plain(&why)}" }
									}
								}
							}
						}
					}
				}
			}
		}
	}
}

/// `text` without its terminal colors.
fn plain(text: &str) -> String {
	let mut out = String::with_capacity(text.len());
	let mut chars = text.chars();
	while let Some(c) = chars.next() {
		if c == '\x1b' {
			chars.by_ref().find(|c| c.is_ascii_alphabetic());
		} else {
			out.push(c);
		}
	}
	out
}

/// ⌘K: every view, scope, sprint and open issue by name.
#[component]
fn Palette() -> Element {
	let ctx = use_context::<Ctx>();
	let mut open = ctx.palette;
	use_command_palette_shortcut(move || open.toggle(), true);
	let item = |id: PanelId, label: String| CommandPaletteItem {
		id: id.0.clone(),
		label,
		icon: None,
		shortcut: None,
		keywords: Vec::new(),
		disabled: false,
		on_select: EventHandler::new(move |_| {
			let mut into = ctx.into;
			if let Some(group) = into.take() {
				ctx.host(&id);
				(*ctx.api.peek()).expect("the palette opens over an entered dock").add_tab(group, id.clone());
			}
			ctx.open(id.clone());
		}),
	};
	let mut groups = Vec::new();
	if let Some(setup) = ctx.setup.read().as_ref() {
		let mut views: Vec<CommandPaletteItem> = ctx.views().into_iter().map(|v| item(PanelId(format!("view:{}", v.name)), v.name)).collect();
		views.extend(setup.sprints.iter().map(|s| item(PanelId(format!("sprint:{}", s.title)), format!("Sprint: {}", s.title))));
		groups.push(CommandPaletteGroup {
			id: "views".into(),
			heading: Some("Views".into()),
			items: views,
		});
	}
	if let Some(board) = ctx.board.read().as_ref() {
		groups.push(CommandPaletteGroup {
			id: "issues".into(),
			heading: Some("Open issues".into()),
			items: board
				.cards
				.iter()
				.filter(|c| c.open())
				.map(|c| item(PanelId(format!("issue:{}", c.link)), format!("{} #{} {}", c.link.project(), c.link.number(), c.title)))
				.collect(),
		});
	}
	rsx! {
		CommandPalette { open: open(), on_open_change: move |v| open.set(v), groups }
	}
}
