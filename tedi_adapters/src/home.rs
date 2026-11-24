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

pub struct Home {
	url: String,
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

	/// A session for the gate before this home, got as a native app does (RFC 8252): the browser, sent where the gate says by
	/// `open`, comes back to a loopback port with a code, exchanged by whoever holds its verifier.
	pub async fn login(&self, open: impl FnOnce(&str)) -> Result<(), LoginError> {
		use base64::{Engine as _, engine::general_purpose::URL_SAFE_NO_PAD};
		use sha2::Digest as _;

		let refused = self.http.post(format!("{}/api/query", self.url)).bearer_auth("-").send().await?;
		if refused.status() != reqwest::StatusCode::UNAUTHORIZED {
			return Err(LoginError::Ungated(self.url.clone(), refused.status()));
		}
		let authorize = refused
			.headers()
			.get(reqwest::header::WWW_AUTHENTICATE)
			.and_then(|v| v.to_str().ok()?.strip_prefix("Bearer authorization_uri=\"")?.strip_suffix('"')) //IGNORED_ERROR: a gate saying anything else says no uri
			.ok_or_else(|| LoginError::NoUri(self.url.clone()))?;

		let listener = tokio::net::TcpListener::bind(("127.0.0.1", 0)).await?;
		let redirect = format!("http://{}/signed-in", listener.local_addr()?);
		let (state, verifier) = (random(), random());
		let challenge = URL_SAFE_NO_PAD.encode(sha2::Sha256::digest(&verifier));
		open(&format!(
			"{authorize}?redirect_uri={}&state={state}&code_challenge={challenge}&code_challenge_method=S256",
			urlencoding::encode(&redirect)
		));
		let code = returned(&listener, &state).await?;

		#[derive(Deserialize)]
		struct Granted {
			session: String,
		}
		let granted = self
			.http
			.post(format!("{authorize}/token"))
			.json(&serde_json::json!({ "code": code, "code_verifier": verifier }))
			.send()
			.await?;
		if !granted.status().is_success() {
			return Err(LoginError::Refused(granted.text().await?));
		}
		let kept = Session {
			home: self.url.clone(),
			session: granted.json::<Granted>().await?.session,
		};
		self.keep(&kept)?;
		Ok(())
	}

	fn keep(&self, session: &Session) -> std::io::Result<()> {
		use std::{io::Write as _, os::unix::fs::OpenOptionsExt as _};

		let fresh = self.session.with_extension("json.new");
		match std::fs::remove_file(&fresh) {
			Ok(()) => {} // a crash's: `mode` is only a new file's
			Err(e) if e.kind() == std::io::ErrorKind::NotFound => {}
			Err(e) => return Err(e),
		}
		let mut out = std::fs::OpenOptions::new().write(true).create_new(true).mode(0o600).open(&fresh)?;
		out.write_all(&serde_json::to_vec(session)?)?;
		std::fs::rename(&fresh, &self.session)
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
			false => {
				let said = serde_json::from_str::<serde_json::Value>(&body)
					.ok()
					.and_then(|v| v.pointer("/data/ServerError/message")?.as_str().map(str::to_owned)); //IGNORED_ERROR: what isn't a server fn's error is shown whole
				Err(HomeError::Refused(said.unwrap_or_else(|| format!("{}/api/{what} ({status}): {body}", self.url))))
			}
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

#[derive(Debug, thiserror::Error)]
pub enum LoginError {
	#[error("{0} takes anyone ({1}): there is nothing to sign in to")]
	Ungated(String, reqwest::StatusCode),
	#[error("the gate before {0} names no place to sign in at")]
	NoUri(String),
	#[error("the sign-in was refused: {0}")]
	Refused(String),
	#[error(transparent)]
	Request(#[from] reqwest::Error),
	#[error(transparent)]
	Io(#[from] std::io::Error),
}

/// What `tedi login` keeps of a sign-in.
#[derive(Deserialize, Serialize)]
struct Session {
	home: String,
	session: String,
}

/// 256 bits, as RFC 7636 writes a verifier.
fn random() -> String {
	use base64::{Engine as _, engine::general_purpose::URL_SAFE_NO_PAD};
	let mut bytes = [0; 32];
	rustls::crypto::ring::default_provider().secure_random.fill(&mut bytes).expect("the system has randomness");
	URL_SAFE_NO_PAD.encode(bytes)
}

/// The code the browser brings back; any other request to the port is turned away.
async fn returned(listener: &tokio::net::TcpListener, state: &str) -> Result<String, LoginError> {
	use tokio::io::{AsyncBufReadExt as _, AsyncWriteExt as _, BufReader};
	//LOOP a browser may ask for more than the page it was sent to
	loop {
		let (mut stream, _) = listener.accept().await?;
		let mut line = String::new();
		BufReader::new(&mut stream).read_line(&mut line).await?;
		let query = line.split(' ').nth(1).and_then(|path| path.strip_prefix("/signed-in?")).unwrap_or_default(); // any other page asks nothing
		let param = |name: &str| {
			query
				.split('&')
				.filter_map(|p| p.split_once('='))
				.find(|(k, _)| *k == name)
				.and_then(|(_, v)| urlencoding::decode(v).ok())
				.map(|v| v.into_owned())
		}; //IGNORED_ERROR: what doesn't decode wasn't sent by the gate
		let ours = param("state").as_deref() == Some(state);
		let (status, said, outcome) = match (ours, param("code"), param("error")) {
			(true, Some(code), _) => ("200 OK", "Signed in: this tab can be closed.", Some(Ok(code))),
			(true, None, Some(error)) => ("200 OK", "Not signed in: this tab can be closed.", Some(Err(LoginError::Refused(error)))),
			_ => ("404 Not Found", "", None),
		};
		let answer = format!(
			"HTTP/1.1 {status}\r\nContent-Type: text/plain; charset=utf-8\r\nContent-Length: {}\r\nConnection: close\r\n\r\n{said}",
			said.len()
		);
		stream.write_all(answer.as_bytes()).await?;
		stream.shutdown().await?;
		if let Some(outcome) = outcome {
			return outcome;
		}
	}
}
