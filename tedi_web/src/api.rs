//! The one client↔server seam: what crosses it, and the server fns.

use std::collections::BTreeMap;

use dioxus_fullstack::{ServerEvents, ServerFnError, server};
use serde::{Deserialize, Serialize};
use tedi_core::{
	IssueLink,
	board::{Board, Scope, View},
	conflict::{Conflict, FieldEdit},
};

/// What the sidebar and every view start from.
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct Setup {
	/// Who edits. Today the instance's own GitHub user, for every request; per request once editors sign in.
	pub me: Option<String>,
	pub team: Vec<View>,
	pub personal: Vec<View>,
	pub scopes: BTreeMap<String, Scope>,
	pub sprints: Vec<Sprint>,
}

/// An open sprint milestone: its task view's issues, in order.
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct Sprint {
	pub title: String,
	pub links: Vec<IssueLink>,
}

#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub enum Edited {
	Landed,
	/// Nothing landed: a field moved on since it was seen, or GitHub and the store clashed on the pull.
	Conflict(Conflict),
}

#[server]
pub async fn setup() -> Result<Setup, ServerFnError> {
	crate::server::setup().await.map_err(failed)
}

#[server]
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

/// Saved among the editor's own views, replacing one of the same name.
#[server]
pub async fn save_view(view: View) -> Result<(), ServerFnError> {
	crate::server::save_view(view).map_err(failed)
}

#[server]
pub async fn delete_view(name: String) -> Result<(), ServerFnError> {
	crate::server::delete_view(&name).map_err(failed)
}

#[cfg(feature = "server")]
fn failed(e: color_eyre::Report) -> ServerFnError {
	ServerFnError::new(format!("{e:#}"))
}
