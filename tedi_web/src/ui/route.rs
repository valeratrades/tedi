//! The address bar. What is on screen reads as an address, and an address moves the screen:
//! `/view/<id>[/<column layout's id>]`, `/sprint/<owner>/<repo>/<n>`, `/library/<at>`, `/builder/<view>` is the panel last brought up;
//! `?issue=owner/repo/n` the floating issues, the shown one last; `ws=<id>` the workspace; `scope=` the scope, a named one's
//! path or a point's GitHub URL.

use std::{fmt, str::FromStr};

use dioxus::{core::spawn_forever, prelude::*};
use futures::StreamExt as _;
use tedi_core::{IssueLink, board::Slice};
use tilers_dioxus::{PanelId, model::serial};
use wasm_bindgen::{JsCast as _, JsValue, closure::Closure};

use super::{Ctx, floating_issues, toast::Tone};
use crate::api::{Item, LibraryEdit};

#[derive(Clone, Debug, Default, PartialEq)]
pub(super) struct Address {
	main: Option<Main>,
	issues: Vec<IssueLink>,
	/// By id.
	workspace: Option<String>,
	scope: Option<Slice>,
}
impl Address {
	pub(super) fn href(&self) -> String {
		format!("{}{self}", base())
	}

	/// `link` shown, above what is here.
	pub(super) fn with_issue(&self, link: &IssueLink) -> Self {
		let mut to = self.clone();
		to.issues.retain(|l| l != link);
		to.issues.push(link.clone());
		to
	}

	pub(super) fn with_scope(&self, scope: Option<Slice>) -> Self {
		Self { scope, ..self.clone() }
	}
}

#[derive(Clone, Debug, PartialEq)]
enum Main {
	/// Both by id.
	View {
		id: String,
		columns: Option<String>,
	},
	Panel(PanelId),
}

/// The path the board is served under, without a trailing slash.
pub(super) fn base() -> String {
	dioxus::cli_config::base_path().map_or_else(String::new, |b| format!("/{}", b.trim_matches('/')))
}

/// The address bar's path and query, under `base`.
pub(super) fn bar() -> String {
	let location = web_sys::window().expect("a browser window").location();
	let (path, search) = (location.pathname().expect("a page has a path"), location.search().expect("a page has a query"));
	format!("{}{search}", path.strip_prefix(&base()).expect("served under its base path"))
}

impl Ctx {
	/// What `id` names: the viewer's, or the team's.
	fn named(&self, id: &str) -> Option<Item> {
		let (library, setup) = (self.library.read(), self.setup.read());
		library.as_ref()?.ids.get(id).or_else(|| setup.as_ref()?.ids.get(id)).cloned()
	}

	fn id_of(&self, item: &Item) -> Option<String> {
		let (library, setup) = (self.library.read(), self.setup.read());
		let held = library.as_ref()?.ids.iter().chain(setup.as_ref()?.ids.iter()).find(|(_, held)| *held == item);
		held.map(|(id, _)| id.clone())
	}

	/// The column layout `view` was last laid out as, under the grouping it shows.
	pub(super) fn laid(&self, view: &str) -> Option<String> {
		let by = self.grouping(view)?;
		self.library.read().as_ref()?.on_screen.laid.get(view)?.get(&by).cloned()
	}

	pub(super) fn grouping(&self, view: &str) -> Option<tedi_core::board::GroupBy> {
		let library = self.library.read();
		match library.as_ref()?.on_screen.views.get(view) {
			Some(shown) => Some(shown.group_by.clone()),
			None => self.views().into_iter().find(|v| v.name == view).map(|v| v.group_by), // never edited on screen: as kept
		}
	}

	/// The view last brought up, while it is what the address names.
	pub(super) fn current_view(&self) -> Option<String> {
		match &self.at.read().main {
			Some(Main::View { id, .. }) => match self.named(id)? {
				Item::View(name) => Some(name),
				other => panic!("only views are addressed as one: {other:?}"),
			},
			_ => None,
		}
	}

	pub(super) fn shows(&self, panel: &PanelId) -> bool {
		self.main_of(panel).is_some_and(|m| self.at.read().main == Some(m))
	}

	/// None: a view kept nowhere the address could name it (one carried by a shared workspace).
	fn main_of(&self, panel: &PanelId) -> Option<Main> {
		Some(match panel.0.strip_prefix("view:") {
			Some(view) => Main::View {
				id: self.id_of(&Item::View(view.to_string()))?,
				columns: self.laid(view).and_then(|name| self.id_of(&Item::Columns(name))),
			},
			None => Main::Panel(panel.clone()),
		})
	}

	/// `panel` brought up, beside what is here.
	pub(super) fn at_panel(&self, panel: &PanelId) -> Address {
		Address {
			main: self.main_of(panel),
			..self.at.read().clone()
		}
	}

	//#[codestyle::skip(swappable-args)]
	pub(super) fn at_columns(&self, view: &str, columns: &str) -> Address {
		Address {
			main: self.id_of(&Item::View(view.to_string())).map(|id| Main::View {
				id,
				columns: self.id_of(&Item::Columns(columns.to_string())),
			}),
			..self.at.read().clone()
		}
	}

	/// The workspace as it is kept.
	pub(super) fn at_workspace(&self, name: &str) -> Address {
		Address {
			main: None,
			issues: Vec::new(),
			workspace: Some(self.id_of(&Item::Workspace(name.to_string())).expect("every workspace offered has an id")),
			scope: self.at.read().scope.clone(),
		}
	}

	/// What is on screen; none while it is not the viewer's, or not there yet.
	fn here(&self) -> Option<Address> {
		let api = (*self.api.read())?;
		if *self.arranged.read() != Some(api.band()) || self.viewing.read().is_some() {
			return None;
		}
		let (workspace, scope) = {
			let library = self.library.read();
			let library = library.as_ref()?;
			(library.workspace.clone(), library.scope.clone())
		};
		let grid = serial::load(&api.save()).expect("a layout loads back from its own save");
		let is_issue = |p: &PanelId| p.0.starts_with("issue:");
		let docked = grid.cells.iter().map(|c| (c.w * c.h, c.group.active_panel()));
		let floating = grid.floats.iter().map(|f| (0, f.group.active_panel()));
		let up: Vec<(u32, &PanelId)> = docked.chain(floating).filter(|(_, p)| !is_issue(p)).collect();
		let main = {
			let (mut main, mut was_up) = (self.main, self.up);
			let raised = up.iter().rev().find(|(_, p)| !was_up.peek().contains(p)).filter(|_| !was_up.peek().is_empty());
			let held = main.peek().clone().filter(|m| up.iter().any(|(_, p)| *p == m));
			let now = raised
				.map(|(_, p)| (*p).clone())
				.or(held)
				.or_else(|| up.iter().max_by_key(|(area, _)| *area).map(|(_, p)| (*p).clone()));
			was_up.set(up.iter().map(|(_, p)| (*p).clone()).collect());
			if *main.peek() != now {
				main.set(now.clone());
			}
			now
		};
		let mut issues = Vec::new();
		for float in &grid.floats {
			let shown = float.group.active_panel();
			let mut tabs: Vec<&PanelId> = float.group.tabs.iter().filter(|p| is_issue(p)).collect();
			tabs.sort_by_key(|p| *p == shown); // stable: the shown one last
			issues.extend(tabs.into_iter().filter_map(|p| IssueLink::parse(&p.0["issue:".len()..])));
		}
		Some(Address {
			main: main.and_then(|m| self.main_of(&m)),
			issues,
			workspace: workspace.and_then(|name| self.id_of(&Item::Workspace(name))),
			scope,
		})
	}

	/// The screen moved to `to`, as a step Back undoes.
	pub(super) fn go(self, to: Address) {
		if *self.at.peek() != to {
			let mut at = self.at;
			at.set(to.clone());
			history(Step::Push, &to);
		}
		spawn_forever(self.reach(to));
	}

	/// What differs from `to` changed; a workspace and a column layout it leaves out stand.
	pub(super) async fn reach(self, to: Address) {
		let (mut moving, mut settling) = (self.moving, self.settling);
		moving += 1;
		let library = || self.library.peek().clone().expect("the screen moves once the library arrived");
		if let Some(id) = &to.workspace {
			match self.named(id) {
				Some(Item::Workspace(name)) =>
					if to.main.is_none() || library().workspace.as_ref() != Some(&name) {
						self.switch(&name).await;
					},
				_ => self.lost("workspace", id),
			}
		}
		if library().scope != to.scope {
			self.keep(LibraryEdit::Scope(to.scope.clone())).await;
		}
		match &to.main {
			Some(Main::View { id, columns }) => match self.named(id) {
				Some(Item::View(view)) => {
					self.open(PanelId(format!("view:{view}")));
					match columns.as_ref().map(|c| (c, self.named(c))) {
						None => {}
						Some((_, Some(Item::Columns(name)))) =>
							if self.laid(&view).as_ref() != Some(&name) {
								self.lay(view, name).await;
							},
						Some((c, _)) => self.lost("column layout", c),
					}
				}
				_ => self.lost("view", id),
			},
			Some(Main::Panel(panel)) => self.open(panel.clone()),
			None => {}
		}
		let mut api = (*self.api.peek()).expect("the screen moves once a band is entered");
		for id in floating_issues(&serial::load(&api.save()).expect("a layout loads back from its own save")) {
			if !to.issues.iter().any(|l| id.0 == format!("issue:{l}")) {
				api.close(&id);
			}
		}
		for link in to.issues {
			self.open_issue(link);
		}
		moving -= 1;
		settling.set(true);
	}

	//#[codestyle::skip(swappable-args)]
	fn lost(&self, what: &str, id: &str) {
		self.say("address", Tone::Error, format!("no {what} {id} here: another viewer's opens by a share link"), None);
	}

	//#[codestyle::skip(swappable-args)]
	/// `view` laid out as the column layout `columns`, whatever was dragged since it last was.
	pub(super) async fn lay(self, view: String, columns: String) {
		if self.keep(LibraryEdit::Lay { view: view.clone(), columns }).await {
			let mut relaid = self.relaid;
			*relaid.write().entry(view).or_default() += 1;
		}
	}
}

pub(super) enum Step {
	Push,
	Replace,
}

pub(super) fn history(step: Step, to: &Address) {
	let history = web_sys::window().expect("a browser window").history().expect("a window has a history");
	let href = to.href();
	match step {
		Step::Push => history.push_state_with_url(&JsValue::NULL, "", Some(&href)),
		Step::Replace => history.replace_state_with_url(&JsValue::NULL, "", Some(&href)),
	}
	.expect("a same-origin path replaces the address");
}

/// The address the page was loaded at; a share link's is the banner's to open.
pub(super) fn landed() -> Result<Address, String> {
	let bar = bar();
	match bar.starts_with("/l/") {
		true => Ok(Address::default()),
		false => bar.parse(),
	}
}

/// The bar kept to the screen, and the screen moved by Back and Forward.
pub(super) fn use_address(ctx: Ctx) {
	use_effect(move || {
		let Some(now) = ctx.here() else { return };
		if *ctx.moving.read() > 0 {
			return;
		}
		let (mut at, mut settling) = (ctx.at, ctx.settling);
		let step = if *settling.read() { Step::Replace } else { Step::Push };
		if *settling.peek() {
			settling.set(false);
		}
		if *at.peek() != now {
			history(step, &now);
			at.set(now);
		}
	});
	use_future(move || async move {
		//UNBOUNDED one unit per popstate event
		let (tell, mut told) = futures::channel::mpsc::unbounded::<()>();
		let listener = Closure::<dyn FnMut(web_sys::Event)>::new(move |_| tell.unbounded_send(()).expect("the shell listens for good"));
		web_sys::window()
			.expect("a browser window")
			.add_event_listener_with_callback("popstate", listener.as_ref().unchecked_ref())
			.expect("a window takes listeners");
		listener.forget(); // the shell never unmounts
		while told.next().await.is_some() {
			match landed() {
				Ok(to) if ctx.library.peek().is_some() && ctx.api.peek().is_some() => {
					let mut at = ctx.at;
					at.set(to.clone());
					ctx.reach(to).await;
				}
				Ok(to) => {
					let mut at = ctx.at;
					at.set(to); // reached once the screen is arranged
				}
				Err(e) => ctx.say("address", Tone::Error, e, None),
			}
		}
	});
}

//#[codestyle::skip(swappable-args)]
/// A link to `to`: a plain click moves this screen, any other is the browser's.
#[component]
pub(super) fn Go(to: Address, #[props(default)] class: String, #[props(default)] title: String, children: Element) -> Element {
	let ctx = use_context::<Ctx>();
	rsx! {
		a {
			class,
			title,
			href: to.href(),
			draggable: "false",
			onclick: move |e: MouseEvent| {
				if e.modifiers().is_empty() {
					e.prevent_default();
					ctx.go(to.clone());
				}
			},
			{children}
		}
	}
}

impl fmt::Display for Address {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		let enc = |s: &str| urlencoding::encode(s).into_owned();
		match &self.main {
			None => f.write_str("/")?,
			Some(Main::View { id, columns: None }) => write!(f, "/view/{id}")?,
			Some(Main::View { id, columns: Some(columns) }) => write!(f, "/view/{id}/{columns}")?,
			Some(Main::Panel(panel)) => {
				let (kind, at) = panel.0.split_once(':').expect("a panel's id is <kind>:<what>");
				write!(f, "/{kind}")?;
				if kind == "sprint" {
					write!(f, "/{at}")?; // a milestone's path
				} else if !at.is_empty() {
					write!(f, "/{}", enc(at))?;
				}
			}
		}
		let mut query: Vec<String> = self
			.issues
			.iter()
			.map(|link| match link {
				IssueLink::Owned(_) => format!("issue={}/{}", link.project(), link.number()),
				IssueLink::Virtual(_) => format!("issue={}", enc(&link.to_string())),
			})
			.collect();
		query.extend(self.workspace.iter().map(|id| format!("ws={id}")));
		query.extend(self.scope.iter().map(|s| format!("scope={}", enc(&s.to_string()))));
		match query.is_empty() {
			true => Ok(()),
			false => write!(f, "?{}", query.join("&")),
		}
	}
}

impl FromStr for Address {
	type Err = String;

	fn from_str(s: &str) -> Result<Self, String> {
		let bad = || format!("{s} is no address on this board");
		let dec = |s: &str| urlencoding::decode(s).map(|d| d.into_owned()).map_err(|_| bad());
		let (path, query) = s.split_once('?').unwrap_or((s, ""));
		let main = match path.trim_matches('/').split('/').collect::<Vec<_>>().as_slice() {
			[""] => None,
			["view", id] => Some(Main::View { id: id.to_string(), columns: None }),
			["view", id, columns] => Some(Main::View {
				id: id.to_string(),
				columns: Some(columns.to_string()),
			}),
			[kind @ ("library" | "builder")] => Some(Main::Panel(PanelId(format!("{kind}:")))),
			["sprint", owner, repo, n] => Some(Main::Panel(PanelId(format!("sprint:{owner}/{repo}/{n}")))),
			[kind @ ("library" | "builder"), at] => Some(Main::Panel(PanelId(format!("{kind}:{}", dec(at)?)))),
			_ => return Err(bad()),
		};
		let mut to = Address { main, ..Default::default() };
		for pair in query.split('&').filter(|p| !p.is_empty()) {
			match pair.split_once('=').ok_or_else(bad)? {
				("issue", at) => {
					let at = dec(at)?;
					let link = match at.split('/').collect::<Vec<_>>().as_slice() {
						[owner, repo, n] if n.parse::<u64>().is_ok() => IssueLink::parse(&format!("https://github.com/{owner}/{repo}/issues/{n}")),
						_ => IssueLink::parse(&at),
					};
					to.issues.push(link.ok_or_else(bad)?);
				}
				("ws", id) => to.workspace = Some(id.to_string()),
				("scope", slice) => to.scope = Some(dec(slice)?.parse().map_err(|_| bad())?),
				_ => return Err(bad()),
			}
		}
		Ok(to)
	}
}
