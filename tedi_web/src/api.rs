//! The one client↔server seam: what crosses it, and the server fns.

use std::collections::{BTreeMap, BTreeSet};

use dioxus_fullstack::{ByteStream, ServerEvents, ServerFnError, get, post};
#[cfg(feature = "server")]
use dioxus_fullstack::{HeaderMap, HeaderValue, http::header::CONTENT_TYPE};
use serde::{Deserialize, Serialize};
use tedi_core::{
	IssueLink, MilestoneLink,
	board::{Answer, GroupBy, Listing, Order, Query, Scope, Shown, Slice, Sort, SortKey, Thread},
	chain::{Orgs, Point},
	conflict::{CommentEdit, Edited, FieldEdit},
	workflow::{State, Trees},
};
use tilers_core::Band;

/// A dock's layout per band, as tilers saves it: a phone's is not a desktop's.
pub type Arrangement = BTreeMap<Band, String>;

/// Each group's column color.
pub type Tints = BTreeMap<String, Tint>;

/// What the sidebar and every view start from.
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct Setup {
	/// The viewer, on GitHub: their keys' login, or whoever runs an ungated instance. `None`: no key says.
	pub me: Option<String>,
	/// Each viewer brings their own keys and scopes; an ungated instance is its operator's alone.
	pub accounts: bool,
	/// What a card sits in, by name: their buckets are a state grouping's columns.
	pub workflows: Trees,
	pub orgs: Orgs,
	pub scopes: BTreeMap<String, Scope>,
	/// The team's: one a layout, under its name, then the rest.
	pub views: Vec<View>,
	/// The team's workspaces, offered beside each viewer's own.
	pub workspaces: BTreeMap<String, Workspace>,
	/// The team's, as an address names them.
	pub ids: BTreeMap<String, Item>,
}

/// A layout: what cards are boxed by, a grouping's groups, or where they sit on the chain below the workspace's scope (`chain`).
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
#[serde(try_from = "String", into = "String")]
pub enum Boxing {
	Groups(GroupBy),
	Chain,
}
impl std::fmt::Display for Boxing {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Boxing::Groups(by) => by.fmt(f),
			Boxing::Chain => f.write_str("chain"),
		}
	}
}
impl TryFrom<String> for Boxing {
	type Error = <GroupBy as std::str::FromStr>::Err;

	fn try_from(s: String) -> Result<Self, Self::Error> {
		match s.as_str() {
			"chain" => Ok(Boxing::Chain),
			by => Ok(Boxing::Groups(by.parse()?)),
		}
	}
}
impl From<Boxing> for String {
	fn from(boxing: Boxing) -> String {
		boxing.to_string()
	}
}

/// How the groups of a grouping, or the nodes of the chain, are boxed.
#[derive(Clone, Debug, Default, Deserialize, PartialEq, Serialize)]
pub struct Boxes {
	#[serde(default, skip_serializing_if = "BTreeMap::is_empty")] // a band without one seeds a box per group
	pub arrangement: Arrangement,
	#[serde(default, skip_serializing_if = "BTreeMap::is_empty")] // untinted
	pub tints: Tints,
	/// Of the chain's nodes, how the leaf issues right in each are laid out.
	#[serde(default, skip_serializing_if = "BTreeMap::is_empty")] // a node without one seeds a tile per issue
	pub nodes: BTreeMap<Point, Arrangement>,
}

/// A layout as it is set up; which cards are in it, and in what order, is the lens's.
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct View {
	pub name: String,
	pub group_by: Boxing,
	/// What a card shows beside its title, in this order.
	pub shown: Vec<Shown>,
	#[serde(flatten)]
	pub boxes: Boxes,
}
impl View {
	pub fn set(&mut self, part: ViewPart) {
		match part {
			ViewPart::Shown(shown) => self.shown = shown,
			ViewPart::Arrangement { of: None, band, layout } => _ = self.boxes.arrangement.insert(band, layout),
			ViewPart::Arrangement { of: Some(node), band, layout } => _ = self.boxes.nodes.entry(node).or_default().insert(band, layout),
			ViewPart::Tints(tints) => self.boxes.tints = tints,
		}
	}
}

/// Which cards every tile of a workspace shows, and in what order.
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct Lens {
	pub scope: Slice,
	pub search: Query,
	pub sort: Order,
}
impl Default for Lens {
	fn default() -> Self {
		Self {
			scope: Slice::default(),
			search: Query::default(),
			sort: vec![Sort { by: SortKey::Rank, reversed: false }].into(), // the query's own order
		}
	}
}

#[derive(Clone, Debug, Default, Deserialize, PartialEq, Serialize)]
pub struct Workspace {
	pub tiles: Arrangement,
	pub lens: Lens,
}
impl Workspace {
	pub fn set(&mut self, part: WorkspacePart) {
		match part {
			WorkspacePart::Tiles { band, layout } => _ = self.tiles.insert(band, layout),
			WorkspacePart::Scope(scope) => self.lens.scope = scope,
			WorkspacePart::Search(search) => self.lens.search = search,
			WorkspacePart::Sort(sort) => self.lens.sort = sort,
		}
	}
}

/// What one viewer keeps, each by copy, and what they have on screen.
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct Library {
	/// The workspace last selected, by name: what a bare address opens. None: the last one selected is gone.
	pub workspace: Option<String>,
	pub views: BTreeMap<String, View>,
	pub workspaces: BTreeMap<String, Workspace>,
	/// Beside the team's, never named as one of theirs.
	pub scopes: BTreeMap<String, Scope>,
	pub drafts: Drafts,
	/// The views whose columns are locked.
	#[serde(default)]
	pub locked: BTreeSet<String>,
	pub favorites: BTreeSet<Item>,
	/// What is kept here, as an address names it: the same through a rename. Scopes are named by path.
	#[serde(default)]
	pub ids: BTreeMap<String, Item>,
	/// What each kept copy was copied from.
	#[serde(default)]
	pub forks: Vec<Forked>,
	/// Workspaces selected, the latest first, at most 9.
	#[serde(default)]
	pub recent: Vec<String>,
	/// When each workspace was last edited.
	#[serde(default)]
	pub autosaved: BTreeMap<String, jiff::Timestamp>,
	/// Links opened, the latest first: shared with this viewer.
	#[serde(default)]
	pub received: Vec<ShareLink>,
}
impl Library {
	/// `name` as last edited: the viewer's own, or the team's.
	pub fn view(&self, team: &[View], name: &str) -> Option<View> {
		let kept = self.views.get(name).or_else(|| team.iter().find(|v| v.name == name))?;
		Some(self.drafts.views.get(name).unwrap_or(kept).clone()) // undrafted: as kept
	}

	/// `name` as last edited: the viewer's own, or the team's.
	pub fn workspace(&self, team: &BTreeMap<String, Workspace>, name: &str) -> Option<Workspace> {
		let kept = self.workspaces.get(name).or_else(|| team.get(name))?;
		Some(self.drafts.workspaces.get(name).unwrap_or(kept).clone()) // undrafted: as kept
	}

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

/// A viewer's who never opened the board.
impl Default for Library {
	fn default() -> Self {
		let first = "ws 1".to_string();
		Self {
			workspace: Some(first.clone()),
			views: BTreeMap::new(),
			workspaces: BTreeMap::from([(first, Workspace::default())]),
			scopes: BTreeMap::new(),
			drafts: Drafts::default(),
			locked: BTreeSet::new(),
			favorites: BTreeSet::new(),
			ids: BTreeMap::new(),
			forks: Vec::new(),
			recent: Vec::new(),
			autosaved: BTreeMap::new(),
			received: Vec::new(),
		}
	}
}

/// What is kept elsewhere, as last edited, where it differs.
#[derive(Clone, Debug, Default, Deserialize, PartialEq, Serialize)]
pub struct Drafts {
	pub views: BTreeMap<String, View>,
	/// The team's: the viewer's own are written as they are edited.
	pub workspaces: BTreeMap<String, Workspace>,
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
}
impl Item {
	pub fn new(shelf: Shelf, name: String) -> Self {
		match shelf {
			Shelf::Views => Item::View(name),
			Shelf::Workspaces => Item::Workspace(name),
			Shelf::Scopes => Item::Scope(name),
		}
	}

	pub fn shelf(&self) -> Shelf {
		match self {
			Item::View(_) => Shelf::Views,
			Item::Scope(_) => Shelf::Scopes,
			Item::Workspace(_) => Shelf::Workspaces,
		}
	}

	pub fn name(&self) -> &str {
		match self {
			Item::View(n) | Item::Scope(n) | Item::Workspace(n) => n,
		}
	}
}

/// A kind of library object; as a link's segment `view`, `scope`, `ws`.
#[derive(Clone, Copy, Debug, Deserialize, Eq, Hash, Ord, PartialEq, PartialOrd, Serialize, strum::Display, strum::EnumIter, strum::EnumString)]
pub enum Shelf {
	#[strum(serialize = "view")]
	Views,
	#[strum(serialize = "ws")]
	Workspaces,
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
	Workspace(Workspace),
}

/// Every object these hold, a scope at each of its paths.
pub fn constructions(views: &[View], scopes: &BTreeMap<String, Scope>, workspaces: &BTreeMap<String, Workspace>) -> BTreeMap<Item, Construction> {
	fn walk(at: &str, scopes: &BTreeMap<String, Scope>, out: &mut BTreeMap<Item, Construction>) {
		for (name, scope) in scopes {
			let path = if at.is_empty() { name.clone() } else { format!("{at} / {name}") };
			out.insert(Item::Scope(path.clone()), Construction::Scope(scope.clone()));
			walk(&path, &scope.children, out);
		}
	}
	let mut out: BTreeMap<Item, Construction> = views.iter().map(|v| (Item::View(v.name.clone()), Construction::View(v.clone()))).collect();
	walk("", scopes, &mut out);
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

/// `/l/{view|scope|ws}/{owner}/{name}@{version}?bands=md,xl`: one version of a library object, read-only
/// to whoever opens it until they fork it.
#[derive(Clone, Debug, Deserialize, Eq, Hash, PartialEq, Serialize)]
#[serde(try_from = "String", into = "String")]
pub struct ShareLink {
	pub item: Item,
	pub owner: Owner,
	pub version: u32,
	/// A workspace's: the bands it carries; none for anything else.
	pub bands: Vec<Band>,
}
impl std::fmt::Display for ShareLink {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		let enc = |s: &str| urlencoding::encode(s).into_owned();
		write!(f, "/l/{}/{}/{}@{}", self.item.shelf(), enc(&self.owner.to_string()), enc(self.item.name()), self.version)?;
		if !self.bands.is_empty() {
			write!(f, "?bands={}", self.bands.iter().map(|b| band_name(*b)).collect::<Vec<_>>().join(","))?;
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
		let bands = match query {
			"" => Vec::new(),
			_ => {
				let list = query.strip_prefix("bands=").ok_or_else(bad)?;
				list.split(',').map(|b| band_named(b).ok_or_else(bad)).collect::<Result<_, _>>()?
			}
		};
		Ok(ShareLink {
			item: Item::new(shelf, dec(name)?),
			owner,
			version: version.parse().map_err(|_| bad())?,
			bands,
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
	/// Viewers whose library refers to it: in a workspace, as the one selected, a favorite or a fork.
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

/// What turns `from` into `to`, line by line: clauses, fields, repos, tints, bands.
pub fn diff(from: &Construction, to: &Construction) -> Vec<Change> {
	let mut out = Vec::new();
	let mut push = |sign, text: String| out.push(Change { sign, text });
	fn sets<T: PartialEq + ToString>(label: &str, from: impl IntoIterator<Item = T>, to: impl IntoIterator<Item = T>, push: &mut dyn FnMut(Sign, String)) {
		let (from, to): (Vec<T>, Vec<T>) = (from.into_iter().collect(), to.into_iter().collect());
		to.iter().filter(|t| !from.contains(t)).for_each(|t| push(Sign::Added, format!("{label}{}", t.to_string())));
		from.iter().filter(|f| !to.contains(f)).for_each(|f| push(Sign::Removed, format!("{label}{}", f.to_string())));
	}
	fn arrangements(of: &str, from: &Arrangement, to: &Arrangement, push: &mut dyn FnMut(Sign, String)) {
		for band in from.keys().chain(to.keys()).collect::<BTreeSet<_>>() {
			let sign = match (from.get(band), to.get(band)) {
				(None, Some(_)) => Sign::Added,
				(Some(_), None) => Sign::Removed,
				(a, b) if a != b => Sign::Changed,
				_ => continue,
			};
			push(sign, format!("{of}{} arrangement", band_name(*band)));
		}
	}
	fn boxes(from: &Boxes, to: &Boxes, push: &mut dyn FnMut(Sign, String)) {
		for group in from.tints.keys().chain(to.tints.keys()).collect::<BTreeSet<_>>() {
			match (from.tints.get(group), to.tints.get(group)) {
				(None, Some(t)) => push(Sign::Added, format!("{group} {t}")),
				(Some(t), None) => push(Sign::Removed, format!("{group} {t}")),
				(Some(x), Some(y)) if x != y => push(Sign::Changed, format!("{group} {x} → {y}")),
				_ => {}
			}
		}
		arrangements("", &from.arrangement, &to.arrangement, push);
		for node in from.nodes.keys().chain(to.nodes.keys()).collect::<BTreeSet<_>>() {
			let of = |boxes: &Boxes| boxes.nodes.get(node).cloned().unwrap_or_default(); // arranged on one side only
			arrangements(&format!("{node} "), &of(from), &of(to), push);
		}
	}
	fn clauses(query: &impl ToString) -> Vec<String> {
		query.to_string().split_whitespace().map(str::to_string).collect()
	}
	match (from, to) {
		(Construction::View(a), Construction::View(b)) => {
			if a.group_by != b.group_by {
				push(Sign::Changed, format!("group_by {} → {}", a.group_by, b.group_by));
			}
			sets("field: ", &a.shown, &b.shown, &mut push);
			boxes(&a.boxes, &b.boxes, &mut push);
		}
		(Construction::Scope(a), Construction::Scope(b)) => {
			fn walk(at: &str, a: Option<&Scope>, b: Option<&Scope>, push: &mut dyn FnMut(Sign, String)) {
				let empty = Scope::default();
				let (a, b) = (a.unwrap_or(&empty), b.unwrap_or(&empty)); // on one side only: all of it is added or removed
				sets(&format!("{at}repo:"), &a.repos, &b.repos, push);
				sets(at, clauses(&a.terms), clauses(&b.terms), push);
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
		(Construction::Workspace(a), Construction::Workspace(b)) => {
			sets("scope ", clauses(&a.lens.scope), clauses(&b.lens.scope), &mut push);
			sets("search ", clauses(&a.lens.search), clauses(&b.lens.search), &mut push);
			if a.lens.sort != b.lens.sort {
				push(Sign::Changed, format!("sort {} → {}", a.lens.sort, b.lens.sort));
			}
			arrangements("", &a.tiles, &b.tiles, &mut push);
		}
		(from, to) => panic!("a diff compares one kind: {from:?} against {to:?}"),
	}
	out
}

#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub enum WorkspacePart {
	Tiles { band: Band, layout: String },
	Scope(Slice),
	Search(Query),
	Sort(Order),
}

#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub enum ViewPart {
	Shown(Vec<Shown>),
	Arrangement {
		/// The chain's node whose leaf issues it lays out; none: it lays out the groups.
		of: Option<Point>,
		band: Band,
		layout: String,
	},
	Tints(Tints),
}

/// Each replaces `name` whole.
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub enum Kept {
	View(View),
	Workspace { name: String, workspace: Workspace },
	Scope { name: String, scope: Scope },
}

/// One change to a library, each touching only what it names, so two tabs of one viewer don't undo each other.
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub enum LibraryEdit {
	/// The viewer's own is written; the team's is drafted.
	Workspace {
		workspace: String,
		part: WorkspacePart,
	},
	/// Drafted, whosever it is.
	View {
		view: String,
		part: ViewPart,
	},
	/// Its draft is dropped.
	Revert(Item),
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
	/// The workspace last selected.
	Open(String),
	/// An empty one, under the lowest `ws N` nobody's is, and selected.
	NewWorkspace,
	/// A copy kept as `name`, remembering where it came from; a workspace's own views come along unless
	/// one of that name is already there.
	Fork {
		from: Origin,
		name: String,
	},
	/// A fork made its upstream's latest again; refused while it is ahead.
	Pull(Item),
}

/// What a viewer brings: keys to GitHub, and the scopes of it they want stored.
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct Account {
	/// Whose the keys are: they all answer one.
	pub login: Option<String>,
	pub keys: Vec<Key>,
	pub scopes: Vec<Stored>,
}

/// A GitHub token, by the owner it is used on; the token itself stays with the server.
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct Key {
	pub owner: String,
	pub outcome: Option<Outcome>,
}

/// `owner` or `owner/repo`, stored here as far as the key on that owner sees it.
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct Stored {
	pub scope: String,
	/// As of its last pull.
	pub repos: usize,
	pub outcome: Option<Outcome>,
}

/// How a pull ended. `None` where there is none: not pulled yet.
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct Outcome {
	pub at: jiff::Timestamp,
	pub error: Option<String>,
}

#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub enum AccountEdit {
	/// Verified against GitHub before it is kept, in the place of one held on that owner.
	Key {
		owner: String,
		token: String,
	},
	DropKey(String),
	Scope(String),
	DropScope(String),
}

#[get("/api/setup", headers: HeaderMap)]
pub async fn setup() -> Result<Setup, ServerFnError> {
	crate::server::setup(&headers).await.map_err(failed)
}

/// A view was ready at `percent` of its progress, or stood full for `late` percent of its wait: what it counts is off.
#[post("/api/off")]
pub async fn off(what: String, percent: u64, late: u64) -> Result<(), ServerFnError> {
	crate::server::off(&what, percent, late);
	Ok(())
}

/// The board's parts within the viewer's reach, a JSON line each, as they are read: those the queries `first` show before the others.
#[post("/api/board", headers: HeaderMap)]
pub async fn board(first: Vec<String>) -> Result<ByteStream, ServerFnError> {
	let lines = crate::server::board(&headers, first).await.map_err(failed)?;
	let context = FullstackContext::current().expect("in a server fn");
	context.add_response_header(CONTENT_TYPE, HeaderValue::from_static("text/plain; charset=utf-8"));
	Ok(lines.into())
}

/// What the query `asked` holds for the viewer: how an instance whose home this is reads it.
#[post("/api/query", headers: HeaderMap)]
pub async fn query(asked: String) -> Result<Answer, ServerFnError> {
	crate::server::query(&headers, asked).await.map_err(failed)
}

/// What the query holds for the viewer, as a task view lists it.
#[post("/api/view", headers: HeaderMap)]
pub async fn view(query: String, group: GroupBy) -> Result<Listing, ServerFnError> {
	crate::server::view(&headers, query, group).await.map_err(failed)
}

/// Pulls every watched repo and the viewer's own from GitHub; the board reloads on the tick its end fires.
#[post("/api/refresh", headers: HeaderMap)]
pub async fn refresh() -> Result<(), ServerFnError> {
	crate::server::refresh(&headers).map_err(failed)
}

/// Fires whenever the store may have changed: an edit, a webhook's pull, the periodic pull, a refresh.
#[get("/api/changes")]
pub async fn changes() -> Result<ServerEvents<()>, ServerFnError> {
	Ok(crate::server::changes())
}

#[post("/api/edit", headers: HeaderMap)]
pub async fn edit(link: IssueLink, edits: Vec<FieldEdit>) -> Result<Edited, ServerFnError> {
	crate::server::edit(&headers, link, edits).await.map_err(failed)
}

/// `link`'s card to `to` of `workflow`, moved from `seen` (`None`: wherever it sits): each bucket it enters runs its hooks.
#[post("/api/move", headers: HeaderMap)]
pub async fn move_card(link: IssueLink, workflow: String, to: State, seen: Option<State>) -> Result<Edited, ServerFnError> {
	crate::server::move_card(&headers, link, workflow, to, seen).await.map_err(failed)
}

/// Which milestone of `repo` (`owner/repo`) hosts `link`, from `seen` to `new`.
#[post("/api/milestone", headers: HeaderMap)]
pub async fn set_milestone(link: IssueLink, repo: String, seen: Option<MilestoneLink>, new: Option<MilestoneLink>) -> Result<Edited, ServerFnError> {
	crate::server::set_milestone(&headers, link, repo, seen, new).await.map_err(failed)
}

#[post("/api/thread", headers: HeaderMap)]
pub async fn thread(link: IssueLink) -> Result<Thread, ServerFnError> {
	crate::server::thread(&headers, link).await.map_err(failed)
}

#[post("/api/comment", headers: HeaderMap)]
pub async fn comment(link: IssueLink, edit: CommentEdit) -> Result<Edited, ServerFnError> {
	crate::server::comment(&headers, link, edit).await.map_err(failed)
}

/// `reviewer`'s review of the pull request `link` asked for, or the ask withdrawn.
#[post("/api/review", headers: HeaderMap)]
pub async fn request_review(link: IssueLink, reviewer: String, on: bool) -> Result<(), ServerFnError> {
	crate::server::request_review(&headers, link, reviewer, on).await.map_err(failed)
}

/// The clash file waiting on `link`'s line, markers and all.
#[post("/api/conflict", headers: HeaderMap)]
pub async fn pending_conflict(link: IssueLink) -> Result<Option<String>, ServerFnError> {
	crate::server::pending_conflict(&headers, link).await.map_err(failed)
}

/// `resolution` written over the pending clash file; landed once it holds no markers. What is still pending comes back.
#[post("/api/resolve", headers: HeaderMap)]
pub async fn resolve(link: IssueLink, resolution: String) -> Result<Option<String>, ServerFnError> {
	crate::server::resolve(&headers, link, resolution).await.map_err(failed)
}

/// The requesting viewer's.
#[get("/api/account", headers: HeaderMap)]
pub async fn account() -> Result<Account, ServerFnError> {
	crate::server::account(&headers).await.map_err(failed)
}

/// The requesting viewer's, after `edit`.
#[post("/api/account/edit", headers: HeaderMap)]
pub async fn edit_account(edit: AccountEdit) -> Result<Account, ServerFnError> {
	crate::server::edit_account(&headers, edit).await.map_err(failed)
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

/// The requesting viewer's, after each of `edits` that applies: what a page still held as it went, so nobody hears why one doesn't.
#[post("/api/library/left", headers: HeaderMap)]
pub async fn leave_library(edits: Vec<LibraryEdit>) -> Result<(), ServerFnError> {
	crate::server::leave_library(&headers, edits).await.map_err(failed)
}

#[get("/api/catalog", headers: HeaderMap)]
pub async fn catalog() -> Result<Catalog, ServerFnError> {
	crate::server::catalog(&headers).await.map_err(failed)
}

/// `item` pinned as it stands now; a workspace's only with `bands`.
#[post("/api/share", headers: HeaderMap)]
pub async fn share(item: Item, bands: Vec<Band>) -> Result<ShareLink, ServerFnError> {
	crate::server::share(&headers, item, bands).await.map_err(failed)
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
