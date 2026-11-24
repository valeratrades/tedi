//! The one client↔server seam: what crosses it, and the server fns.

use std::collections::{BTreeMap, BTreeSet};

#[cfg(feature = "server")]
use dioxus_fullstack::HeaderMap;
use dioxus_fullstack::{ServerEvents, ServerFnError, get, post, server};
use serde::{Deserialize, Serialize};
use tedi_core::{
	IssueLink,
	board::{Board, GroupBy, Scope, View},
	conflict::{Conflict, FieldEdit},
	workflow::{State, Workflow},
};
use tilers_core::Band;

/// A dock's layout per band, as tilers saves it: a phone's is not a desktop's.
pub type Arrangement = BTreeMap<Band, String>;

/// Each group's column color.
pub type Tints = BTreeMap<String, Tint>;

/// What the sidebar and every view start from.
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct Setup {
	/// Who edits. Today the instance's own GitHub user, for every request; per request once editors sign in.
	pub me: Option<String>,
	/// What a card sits in, by name: their buckets are a state grouping's columns.
	pub workflows: BTreeMap<String, Workflow>,
	pub scopes: BTreeMap<String, Scope>,
	pub views: Vec<View>,
	pub sprints: Vec<Sprint>,
	/// The team's columns, offered beside each viewer's own.
	pub columns: BTreeMap<String, Columns>,
	/// The team's workspaces, offered beside each viewer's own.
	pub workspaces: BTreeMap<String, Arrangement>,
}

/// An open sprint milestone: its task view's issues, in order.
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct Sprint {
	pub title: String,
	pub links: Vec<IssueLink>,
}

/// One viewer's board as they left it, and the filters and arrangements they keep, each kept by
/// copy: switching to one copies it onto the screen, saving copies the screen's band out.
#[derive(Clone, Debug, Default, Deserialize, PartialEq, Serialize)]
pub struct Library {
	/// What everything on screen is narrowed to, by path; none: the whole store.
	#[serde(default)]
	pub scope: Option<String>,
	/// The workspace last switched to, by name; none: the screen is no kept workspace's.
	#[serde(default)]
	pub workspace: Option<String>,
	pub on_screen: OnScreen,
	pub filters: BTreeMap<String, View>,
	pub workspaces: BTreeMap<String, Arrangement>,
	pub columns: BTreeMap<String, Columns>,
	/// Beside the team's, never named as one of theirs.
	#[serde(default)]
	pub scopes: BTreeMap<String, Scope>,
	pub favorites: BTreeSet<Item>,
	/// What each kept copy was copied from.
	#[serde(default)]
	pub forks: Vec<Forked>,
	/// Workspaces switched to, the latest first, at most 9.
	#[serde(default)]
	pub recent: Vec<String>,
	/// When arranging the screen last wrote the current workspace.
	#[serde(default)]
	pub autosaved: Option<jiff::Timestamp>,
	/// Links opened, the latest first: shared with this viewer.
	#[serde(default)]
	pub received: Vec<ShareLink>,
}
impl Library {
	/// The team's scopes with the viewer's own in them, each at its path (`EV / core`); a parent gone from the
	/// team's config leaves its own scope nowhere to sit, which is the error.
	pub fn scopes(&self, team: &BTreeMap<String, Scope>) -> Result<BTreeMap<String, Scope>, String> {
		let mut tree = team.clone();
		let mut own: Vec<(&String, &Scope)> = self.scopes.iter().collect();
		own.sort_by_key(|(path, _)| path.matches(" / ").count()); // parents before what they hold
		for (path, scope) in own {
			let mut names: Vec<&str> = path.split(" / ").collect();
			let leaf = names.pop().expect("split yields one");
			let mut level = &mut tree;
			for name in names {
				level = &mut level.get_mut(name).ok_or_else(|| format!("your scope {path} sits in {name}, which is no longer there"))?.children;
			}
			if level.insert(leaf.to_string(), scope.clone()).is_some() {
				return Err(format!("your scope {path} has the path of one the team's config now holds"));
			}
		}
		Ok(tree)
	}
}

#[derive(Clone, Debug, Default, Deserialize, PartialEq, Serialize)]
pub struct OnScreen {
	pub tiles: Arrangement,
	/// Each view as last edited: it reopens so.
	#[serde(default)]
	pub views: BTreeMap<String, View>,
	/// Each view's columns, per grouping.
	pub columns: BTreeMap<String, BTreeMap<GroupBy, Arrangement>>,
	#[serde(default)]
	pub tints: BTreeMap<String, BTreeMap<GroupBy, Tints>>,
	/// The views whose columns are locked.
	#[serde(default)]
	pub locked: BTreeSet<String>,
}

/// A view's columns, kept for the grouping whose keys they are.
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct Columns {
	pub by: GroupBy,
	#[serde(default)]
	pub layout: Arrangement,
	#[serde(default)]
	pub tints: Tints,
}

/// Notion's palette.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize, strum::Display, strum::EnumIter)]
#[serde(rename_all = "snake_case")]
#[strum(serialize_all = "snake_case")]
pub enum Tint {
	Gray,
	Brown,
	Orange,
	Yellow,
	Green,
	Blue,
	Purple,
	Pink,
	Red,
}

/// A library object by kind and name, owner aside: team and viewer names never coincide.
#[derive(Clone, Debug, Deserialize, Eq, Hash, Ord, PartialEq, PartialOrd, Serialize)]
pub enum Item {
	View(String),
	/// Its path: names ` / `-joined.
	Scope(String),
	Workspace(String),
	Columns(String),
}
impl Item {
	pub fn new(shelf: Shelf, name: String) -> Self {
		match shelf {
			Shelf::Filters => Item::View(name),
			Shelf::Workspaces => Item::Workspace(name),
			Shelf::Columns => Item::Columns(name),
			Shelf::Scopes => Item::Scope(name),
		}
	}

	pub fn shelf(&self) -> Shelf {
		match self {
			Item::View(_) => Shelf::Filters,
			Item::Scope(_) => Shelf::Scopes,
			Item::Workspace(_) => Shelf::Workspaces,
			Item::Columns(_) => Shelf::Columns,
		}
	}

	pub fn name(&self) -> &str {
		match self {
			Item::View(n) | Item::Scope(n) | Item::Workspace(n) | Item::Columns(n) => n,
		}
	}
}

/// A kind of library object; as a link's segment `view`, `scope`, `ws`, `columns`.
#[derive(Clone, Copy, Debug, Deserialize, Eq, Hash, Ord, PartialEq, PartialOrd, Serialize, strum::Display, strum::EnumIter, strum::EnumString)]
pub enum Shelf {
	#[strum(serialize = "view")]
	Filters,
	#[strum(serialize = "ws")]
	Workspaces,
	#[strum(serialize = "columns")]
	Columns,
	#[strum(serialize = "scope")]
	Scopes,
}

/// Whose a library object is.
#[derive(Clone, Debug, Deserialize, Eq, Hash, Ord, PartialEq, PartialOrd, Serialize)]
pub enum Owner {
	/// The config's: read-only on the board.
	Team,
	Viewer(String),
}
impl std::fmt::Display for Owner {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Owner::Team => f.write_str("team"),
			Owner::Viewer(v) => f.write_str(v),
		}
	}
}

/// What a library object holds.
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub enum Construction {
	View(View),
	Scope(Scope),
	Workspace(Arrangement),
	Columns(Columns),
}

/// Every object these hold, a scope at each of its paths.
pub fn constructions(views: &[View], scopes: &BTreeMap<String, Scope>, columns: &BTreeMap<String, Columns>, workspaces: &BTreeMap<String, Arrangement>) -> BTreeMap<Item, Construction> {
	fn walk(at: &str, scopes: &BTreeMap<String, Scope>, out: &mut BTreeMap<Item, Construction>) {
		for (name, scope) in scopes {
			let path = if at.is_empty() { name.clone() } else { format!("{at} / {name}") };
			out.insert(Item::Scope(path.clone()), Construction::Scope(scope.clone()));
			walk(&path, &scope.children, out);
		}
	}
	let mut out: BTreeMap<Item, Construction> = views.iter().map(|v| (Item::View(v.name.clone()), Construction::View(v.clone()))).collect();
	walk("", scopes, &mut out);
	out.extend(columns.iter().map(|(n, c)| (Item::Columns(n.clone()), Construction::Columns(c.clone()))));
	out.extend(workspaces.iter().map(|(n, w)| (Item::Workspace(n.clone()), Construction::Workspace(w.clone()))));
	out
}

/// A kept copy's origin, so it can show how far the two moved apart.
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct Upstream {
	pub owner: Owner,
	pub name: String,
	/// None: the team's config has no history to number it by.
	pub version: Option<u32>,
	/// Saves that changed the copy since it was forked or pulled.
	pub ahead: u32,
}

#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct Forked {
	pub item: Item,
	pub upstream: Upstream,
}

/// `/l/{view|scope|ws|columns}/{owner}/{name}@{version}?bands=md,xl&scope=…`: one version of a library
/// object, read-only to whoever opens it until they fork it; the scope is the sharer's, which the receiver may drop.
#[derive(Clone, Debug, Deserialize, Eq, Hash, PartialEq, Serialize)]
#[serde(try_from = "String", into = "String")]
pub struct ShareLink {
	pub item: Item,
	pub owner: Owner,
	pub version: u32,
	/// A workspace's: the bands it carries; none for anything else.
	pub bands: Vec<Band>,
	pub scope: Option<String>,
}
impl std::fmt::Display for ShareLink {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		let enc = |s: &str| urlencoding::encode(s).into_owned();
		write!(f, "/l/{}/{}/{}@{}", self.item.shelf(), enc(&self.owner.to_string()), enc(self.item.name()), self.version)?;
		let mut query = Vec::new();
		if !self.bands.is_empty() {
			query.push(format!("bands={}", self.bands.iter().map(|b| band_name(*b)).collect::<Vec<_>>().join(",")));
		}
		if let Some(scope) = &self.scope {
			query.push(format!("scope={}", enc(scope)));
		}
		if !query.is_empty() {
			write!(f, "?{}", query.join("&"))?;
		}
		Ok(())
	}
}
impl std::str::FromStr for ShareLink {
	type Err = String;

	fn from_str(s: &str) -> Result<Self, String> {
		let bad = || format!("{s} is not a share link (/l/{{kind}}/{{owner}}/{{name}}@{{version}})");
		let dec = |s: &str| urlencoding::decode(s).map(|d| d.into_owned()).map_err(|_| bad());
		let (path, query) = s.split_once('?').unwrap_or((s, ""));
		let rest = path.strip_prefix("/l/").ok_or_else(bad)?;
		let mut parts = rest.splitn(3, '/');
		let (Some(kind), Some(owner), Some(named)) = (parts.next(), parts.next(), parts.next()) else {
			return Err(bad());
		};
		let shelf: Shelf = kind.parse().map_err(|_| bad())?;
		let (name, version) = named.rsplit_once('@').ok_or_else(bad)?;
		let owner = match dec(owner)?.as_str() {
			"team" => Owner::Team,
			viewer => Owner::Viewer(viewer.to_string()),
		};
		let (mut bands, mut scope) = (Vec::new(), None);
		for pair in query.split('&').filter(|p| !p.is_empty()) {
			match pair.split_once('=').ok_or_else(bad)? {
				("bands", list) => bands = list.split(',').map(|b| band_named(b).ok_or_else(bad)).collect::<Result<_, _>>()?,
				("scope", path) => scope = Some(dec(path)?),
				_ => return Err(bad()),
			}
		}
		Ok(ShareLink {
			item: Item::new(shelf, dec(name)?),
			owner,
			version: version.parse().map_err(|_| bad())?,
			bands,
			scope,
		})
	}
}
impl TryFrom<String> for ShareLink {
	type Error = String;

	fn try_from(s: String) -> Result<Self, String> {
		s.parse()
	}
}
impl From<ShareLink> for String {
	fn from(link: ShareLink) -> String {
		link.to_string()
	}
}

pub fn band_name(band: Band) -> &'static str {
	match band {
		Band::Sm => "sm",
		Band::Md => "md",
		Band::Xl => "xl",
	}
}

/// One version of a shared object; a workspace's own views come with it, each at the version it was shared at.
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct Shared {
	pub link: ShareLink,
	pub construction: Construction,
	pub carried: BTreeMap<String, (u32, View)>,
}

/// Where a fork copies from.
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub enum Origin {
	/// As the team's config holds it now.
	Team(Item),
	Link(ShareLink),
}

/// Where a proposal lands in the team's config.
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub enum Onto {
	/// The team's object of this name, replaced.
	Update(String),
	/// A new team object of this name.
	New(String),
}

/// What the server knows of each library object the requesting viewer sees: the team's and their own.
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct Catalog {
	pub entries: Vec<Entry>,
}

#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct Entry {
	pub owner: Owner,
	pub item: Item,
	pub usage: Usage,
	/// The team's: each version, oldest first; empty without history.
	pub history: Vec<Version>,
	/// The team's: which of `history` the board runs; none: none of them (unpublished).
	pub version: Option<u32>,
	/// Every viewer's copies of it.
	pub forks: Vec<Fork>,
	/// A fork's: versions its upstream gained since; none: the upstream has no history.
	pub behind: Option<u32>,
}

/// Counted over every viewer's library.
#[derive(Clone, Debug, Default, Deserialize, PartialEq, Serialize)]
pub struct Usage {
	/// Viewers whose library refers to it: on screen, in a workspace, as the scope, a favorite or a fork.
	pub viewers: u32,
	pub forks: u32,
	pub favorites: u32,
}

#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct Version {
	pub v: u32,
	pub author: String,
	pub at: jiff::Timestamp,
	/// From the version before; all of it for the first.
	pub changes: Vec<Change>,
}

#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct Fork {
	pub viewer: String,
	pub name: String,
	pub ahead: u32,
	pub behind: Option<u32>,
}

#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct Change {
	pub sign: Sign,
	pub text: String,
}

#[derive(Clone, Copy, Debug, Deserialize, PartialEq, Serialize)]
pub enum Sign {
	Added,
	Removed,
	Changed,
}

/// What turns `from` into `to`, line by line: query clauses, fields, repos, tints, bands.
pub fn diff(from: &Construction, to: &Construction) -> Vec<Change> {
	let mut out = Vec::new();
	let mut push = |sign, text: String| out.push(Change { sign, text });
	fn sets<'a>(from: impl IntoIterator<Item = &'a str>, to: impl IntoIterator<Item = &'a str>) -> (Vec<&'a str>, Vec<&'a str>) {
		let (from, to): (Vec<&str>, Vec<&str>) = (from.into_iter().collect(), to.into_iter().collect());
		(
			to.iter().filter(|t| !from.contains(t)).copied().collect(),
			from.iter().filter(|f| !to.contains(f)).copied().collect(),
		)
	}
	match (from, to) {
		(Construction::View(a), Construction::View(b)) => {
			let (added, removed) = sets(a.query.split_whitespace(), b.query.split_whitespace());
			added.into_iter().for_each(|c| push(Sign::Added, c.to_string()));
			removed.into_iter().for_each(|c| push(Sign::Removed, c.to_string()));
			if a.group_by != b.group_by {
				push(Sign::Changed, format!("group_by {} → {}", a.group_by, b.group_by));
			}
			if a.sort != b.sort {
				push(Sign::Changed, format!("sort {} → {}", a.sort, b.sort));
			}
			let (fa, fb): (Vec<String>, Vec<String>) = (a.shown.iter().cloned().map(String::from).collect(), b.shown.iter().cloned().map(String::from).collect());
			let (added, removed) = sets(fa.iter().map(String::as_str), fb.iter().map(String::as_str));
			added.into_iter().for_each(|f| push(Sign::Added, format!("field: {f}")));
			removed.into_iter().for_each(|f| push(Sign::Removed, format!("field: {f}")));
		}
		(Construction::Scope(a), Construction::Scope(b)) => {
			fn walk(at: &str, a: Option<&Scope>, b: Option<&Scope>, push: &mut dyn FnMut(Sign, String)) {
				let empty = Scope {
					repos: Vec::new(),
					children: BTreeMap::new(),
				};
				let (a, b) = (a.unwrap_or(&empty), b.unwrap_or(&empty));
				for repo in b.repos.iter().filter(|r| !a.repos.contains(r)) {
					push(Sign::Added, format!("{at}repo:{repo}"));
				}
				for repo in a.repos.iter().filter(|r| !b.repos.contains(r)) {
					push(Sign::Removed, format!("{at}repo:{repo}"));
				}
				for name in a.children.keys().chain(b.children.keys()).collect::<BTreeSet<_>>() {
					let (ca, cb) = (a.children.get(name), b.children.get(name));
					match (ca, cb) {
						(None, Some(_)) => push(Sign::Added, format!("{at}{name}")),
						(Some(_), None) => push(Sign::Removed, format!("{at}{name}")),
						_ => {}
					}
					walk(&format!("{at}{name} / "), ca, cb, push);
				}
			}
			walk("", Some(a), Some(b), &mut push);
		}
		(Construction::Workspace(a), Construction::Workspace(b)) =>
			for band in a.keys().chain(b.keys()).collect::<BTreeSet<_>>() {
				match (a.get(band), b.get(band)) {
					(None, Some(_)) => push(Sign::Added, format!("{} arrangement", band_name(*band))),
					(Some(_), None) => push(Sign::Removed, format!("{} arrangement", band_name(*band))),
					(Some(x), Some(y)) if x != y => push(Sign::Changed, format!("{} arrangement", band_name(*band))),
					_ => {}
				}
			},
		(Construction::Columns(a), Construction::Columns(b)) => {
			if a.by != b.by {
				push(Sign::Changed, format!("by {} → {}", a.by, b.by));
			}
			for group in a.tints.keys().chain(b.tints.keys()).collect::<BTreeSet<_>>() {
				match (a.tints.get(group), b.tints.get(group)) {
					(None, Some(t)) => push(Sign::Added, format!("{group} {t}")),
					(Some(t), None) => push(Sign::Removed, format!("{group} {t}")),
					(Some(x), Some(y)) if x != y => push(Sign::Changed, format!("{group} {x} → {y}")),
					_ => {}
				}
			}
			for band in a.layout.keys().chain(b.layout.keys()).collect::<BTreeSet<_>>() {
				if a.layout.get(band) != b.layout.get(band) {
					push(Sign::Changed, format!("{} arrangement", band_name(*band)));
				}
			}
		}
		(from, to) => panic!("a diff compares one kind: {from:?} against {to:?}"),
	}
	out
}

#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub enum Dock {
	Tiles,
	Columns { view: String, by: GroupBy },
}

#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub enum Kept {
	/// Replaces the view whole, with the columns and tints on screen as `arranged_as`'s.
	Filter { view: View, arranged_as: String },
	/// Replaces `name`'s bands `tiles` holds, keeping its others.
	Workspace { name: String, tiles: Arrangement },
	/// Replaces `name`'s `band` and its tints.
	Columns { name: String, by: GroupBy, band: Band, layout: String, tints: Tints },
	/// Replaces `name` whole.
	Scope { name: String, scope: Scope },
}

/// One change to a library, each touching only what it names, so two tabs of one viewer don't undo each other.
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub enum LibraryEdit {
	/// What a dock on screen now shows.
	Arranged {
		dock: Dock,
		band: Band,
		layout: String,
	},
	/// What a view on screen now shows.
	Viewed(View),
	Tinted {
		view: String,
		by: GroupBy,
		tints: Tints,
	},
	Locked {
		view: String,
		locked: bool,
	},
	Keep(Kept),
	Rename {
		shelf: Shelf,
		from: String,
		to: String,
	},
	Drop {
		shelf: Shelf,
		name: String,
	},
	Favorite {
		what: Item,
		on: bool,
	},
	Scope(Option<String>),
	/// The workspace the screen now shows: the one arranging it writes to.
	Workspace(Option<String>),
	/// A copy kept as `name`, remembering where it came from; a workspace's own views come along unless
	/// one of that name is already there.
	Fork {
		from: Origin,
		name: String,
	},
	/// A fork made its upstream's latest again; refused while it is ahead.
	Pull(Item),
}

#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub enum Edited {
	Landed,
	/// Nothing landed: a field moved on since it was seen, or GitHub and the store clashed on the pull.
	Conflict(Conflict),
	/// Nothing landed: a hook refused the move, or the card left the state it was moved from.
	Rejected(String),
}

#[server]
pub async fn setup() -> Result<Setup, ServerFnError> {
	crate::server::setup().await.map_err(failed)
}

#[get("/api/board")]
pub async fn board() -> Result<Board, ServerFnError> {
	Ok((*crate::server::board().await.map_err(failed)?).clone())
}

/// Pulls every watched repo from GitHub; the board reloads on the tick its end fires.
#[server]
pub async fn refresh() -> Result<(), ServerFnError> {
	crate::server::refresh();
	Ok(())
}

/// Fires whenever the store may have changed: an edit, a webhook's pull, the periodic pull, a refresh.
#[server]
pub async fn changes() -> Result<ServerEvents<()>, ServerFnError> {
	Ok(crate::server::changes())
}

#[server]
pub async fn edit(link: IssueLink, edits: Vec<FieldEdit>) -> Result<Edited, ServerFnError> {
	crate::server::edit(link, edits).await.map_err(failed)
}

/// `link`'s card to `to` of `workflow`, moved from `seen`: each bucket it enters runs its hooks.
#[server]
pub async fn move_card(link: IssueLink, workflow: String, to: State, seen: State) -> Result<Edited, ServerFnError> {
	crate::server::move_card(link, workflow, to, seen).await.map_err(failed)
}

/// The clash file waiting on `link`'s line, markers and all.
#[server]
pub async fn pending_conflict(link: IssueLink) -> Result<Option<String>, ServerFnError> {
	crate::server::pending_conflict(&link).await.map_err(failed)
}

/// `resolution` written over the pending clash file; landed once it holds no markers. What is still pending comes back.
#[server]
pub async fn resolve(link: IssueLink, resolution: String) -> Result<Option<String>, ServerFnError> {
	crate::server::resolve(&link, resolution).await.map_err(failed)
}

/// The requesting viewer's.
#[get("/api/library", headers: HeaderMap)]
pub async fn library() -> Result<Library, ServerFnError> {
	crate::server::library(&headers).await.map_err(failed)
}

/// The requesting viewer's, after `edit`.
#[post("/api/library/edit", headers: HeaderMap)]
pub async fn edit_library(edit: LibraryEdit) -> Result<Library, ServerFnError> {
	crate::server::edit_library(&headers, edit).await.map_err(failed)
}

#[get("/api/catalog", headers: HeaderMap)]
pub async fn catalog() -> Result<Catalog, ServerFnError> {
	crate::server::catalog(&headers).await.map_err(failed)
}

/// `item` pinned as it stands now; a workspace's only with `bands`.
#[post("/api/share", headers: HeaderMap)]
pub async fn share(item: Item, bands: Vec<Band>, scope: Option<String>) -> Result<ShareLink, ServerFnError> {
	crate::server::share(&headers, item, bands, scope).await.map_err(failed)
}

/// What `link` pins; it joins the requesting viewer's `received`.
#[post("/api/shared", headers: HeaderMap)]
pub async fn shared(link: ShareLink) -> Result<Shared, ServerFnError> {
	crate::server::shared(&headers, link).await.map_err(failed)
}

/// The viewer's `item` as a pull request onto the team's config; its url.
#[post("/api/propose", headers: HeaderMap)]
pub async fn propose(item: Item, onto: Onto, message: String) -> Result<String, ServerFnError> {
	crate::server::propose(&headers, item, onto, message).await.map_err(failed)
}

fn band_named(name: &str) -> Option<Band> {
	Some(match name {
		"sm" => Band::Sm,
		"md" => Band::Md,
		"xl" => Band::Xl,
		_ => return None,
	})
}
impl std::fmt::Display for Sign {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		f.write_str(match self {
			Sign::Added => "+",
			Sign::Removed => "-",
			Sign::Changed => "~",
		})
	}
}

#[cfg(feature = "server")]
fn failed(e: color_eyre::Report) -> ServerFnError {
	ServerFnError::new(format!("{e:#}"))
}
