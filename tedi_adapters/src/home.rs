//! The home: the `tedi web` that holds the workflow states, asked over its own API, through whatever gate stands before it.

use std::path::PathBuf;

use serde::{Deserialize, Serialize, de::DeserializeOwned};
use tedi_core::{
	IssueLink,
	board::{Answer, GroupBy, Listing},
	conflict::Edited,
	workflow::State,
};

#[derive(Debug, thiserror::Error)]
pub enum HomeError {
	#[error("{0} can't be reached")]
	Unreachable(String, #[source] reqwest::Error),
	#[error("{0} doesn't know this session: run `tedi login`")]
	SignedOut(String),
	#[error("{0}")]
	Refused(String),
	#[error("`home` is not a url: {0}")]
	Url(String),
	#[error("reading the session at {}", .0.display())]
	Session(PathBuf, #[source] std::io::Error),
}
impl HomeError {
	/// Worth another try later, nothing having been decided: out of reach, or signed out.
	pub fn is_away(&self) -> bool {
		matches!(self, Self::Unreachable(..) | Self::SignedOut(_))
	}
}

/// What `tedi login` keeps of a sign-in.
#[derive(Deserialize, Serialize)]
pub(crate) struct Session {
	pub(crate) home: String,
	pub(crate) session: String,
}

pub struct Home {
	pub(crate) url: String,
	http: reqwest::Client,
	/// Where `tedi login` keeps the session.
	session: PathBuf,
}
impl Home {
	pub fn new(url: &str, session: PathBuf) -> Self {
		Self {
			url: url.trim_end_matches('/').to_string(),
			http: crate::client_builder().redirect(reqwest::redirect::Policy::none()).build().expect("default client config"),
			session,
		}
	}

	pub async fn query(&self, query: &str) -> Result<Answer, HomeError> {
		self.ask("query", &serde_json::json!({ "asked": query })).await
	}

	/// The view `named` there, or that query.
	pub async fn view(&self, named: &str, group: Option<&GroupBy>) -> Result<Listing, HomeError> {
		self.ask("view", &serde_json::json!({ "named": named, "group": group })).await
	}

	/// `link`'s card to `to` of `workflow`, made against it sitting at `seen` (`None`: wherever it sits).
	pub async fn move_card(&self, link: &IssueLink, workflow: &str, to: &State, seen: Option<&State>) -> Result<Edited, HomeError> {
		self.ask("move", &serde_json::json!({ "link": link, "workflow": workflow, "to": to, "seen": seen })).await
	}

	async fn ask<T: DeserializeOwned>(&self, what: &str, body: &serde_json::Value) -> Result<T, HomeError> {
		let away = |e| HomeError::Unreachable(self.url.clone(), e);
		let mut request = self.http.post(format!("{}/api/{what}", self.url)).json(body);
		if let Some(session) = self.session()? {
			request = request.bearer_auth(session);
		}
		let res = request.send().await.map_err(away)?;
		let status = res.status();
		if status == reqwest::StatusCode::UNAUTHORIZED || status.is_redirection() {
			return Err(HomeError::SignedOut(self.url.clone())); // a gate sends a browser off to sign in
		}
		if status.is_server_error() && res.headers().get(reqwest::header::CONTENT_TYPE).is_none_or(|t| !t.as_bytes().starts_with(b"application/json")) {
			return Err(away(res.error_for_status().expect_err("a 5xx")));
		}
		let body = res.text().await.map_err(away)?;
		match status.is_success() {
			true => serde_json::from_str(&body).map_err(|e| HomeError::Refused(format!("{}/api/{what} answered what this tedi can't read ({e}): {body}", self.url))),
			false => Err(HomeError::Refused(format!("{}/api/{what} ({status}): {body}", self.url))),
		}
	}

	fn session(&self) -> Result<Option<String>, HomeError> {
		match std::fs::read_to_string(&self.session) {
			Ok(kept) => {
				let kept: Session = serde_json::from_str(&kept).map_err(|e| HomeError::Session(self.session.clone(), e.into()))?;
				Ok((kept.home == self.url).then_some(kept.session)) // signed in somewhere else is signed out here
			}
			Err(e) if e.kind() == std::io::ErrorKind::NotFound => Ok(None), // an ungated home takes anyone
			Err(e) => Err(HomeError::Session(self.session.clone(), e)),
		}
	}
}
