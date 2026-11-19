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
	pub on_screen: OnScreen,
	pub filters: BTreeMap<String, View>,
	pub workspaces: BTreeMap<String, Arrangement>,
	pub columns: BTreeMap<String, Columns>,
	/// Beside the team's, never named as one of theirs.
	#[serde(default)]
	pub scopes: BTreeMap<String, Scope>,
	pub favorites: BTreeSet<Favorite>,
}
impl Library {
	/// The team's scopes, then the viewer's own.
	pub fn scopes(&self, team: &BTreeMap<String, Scope>) -> BTreeMap<String, Scope> {
		team.iter().chain(&self.scopes).map(|(n, s)| (n.clone(), s.clone())).collect()
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

#[derive(Clone, Debug, Deserialize, Eq, Ord, PartialEq, PartialOrd, Serialize)]
pub enum Favorite {
	View(String),
	/// Its path: names ` / `-joined.
	Scope(String),
	Workspace(String),
	Columns(String),
}

#[derive(Clone, Copy, Debug, Deserialize, PartialEq, Serialize)]
pub enum Shelf {
	Filters,
	Workspaces,
	Columns,
	Scopes,
}

#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub enum Dock {
	Tiles,
	Columns { view: String, by: GroupBy },
}

#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub enum Kept {
	Filter(View),
	/// Replaces `name`'s `band`, keeping its other bands.
	Workspace {
		name: String,
		band: Band,
		layout: String,
	},
	/// Replaces `name`'s `band` and its tints.
	Columns {
		name: String,
		by: GroupBy,
		band: Band,
		layout: String,
		tints: Tints,
	},
	/// Replaces `name` whole.
	Scope {
		name: String,
		scope: Scope,
	},
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
		what: Favorite,
		on: bool,
	},
	Scope(Option<String>),
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

/// Fires whenever the store may have changed: an edit, a webhook's pull, the periodic pull.
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

#[cfg(feature = "server")]
fn failed(e: color_eyre::Report) -> ServerFnError {
	ServerFnError::new(format!("{e:#}"))
}
