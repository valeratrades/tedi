//! Accounts: the keys and scopes a viewer brings, what they reach with them, and whom they act as.

use std::{
	collections::{BTreeMap, BTreeSet},
	path::PathBuf,
};

use color_eyre::eyre::{Result, WrapErr as _, bail};
use dioxus::server::http::HeaderMap;
use jiff::Timestamp;
use serde::{Deserialize, Serialize};
use tedi_core::RepoInfo;
use tedi_task_operations::{
	github::{self, GithubError, client::Actor},
	reach::Reach,
};
use tokio::sync::Mutex;

use super::{Viewer, settings, viewer};
use crate::api::{self, AccountEdit, Key, Outcome, Stored};

/// A request's viewer.
pub(super) struct Caller {
	pub(super) reach: Reach,
	/// `None`: the instance itself, ungated.
	actor: Option<Actor>,
}
impl Caller {
	pub(super) fn of(headers: &HeaderMap) -> Result<Self> {
		let key = viewer(headers)?;
		match settings().viewer {
			Viewer::Operator => Ok(Self { reach: Reach::All, actor: None }),
			Viewer::Header(_) => {
				let account = read(&key)?;
				Ok(Self {
					reach: account.reach(),
					actor: Some(account.actor()?),
				})
			}
		}
	}

	/// `f`, done as them.
	pub(super) async fn act<F: Future>(&self, f: F) -> F::Output {
		match &self.actor {
			Some(actor) => github::client::acting(actor.clone(), f).await,
			None => f.await,
		}
	}
}

/// A viewer's, as kept: readable by this process' user alone.
#[derive(Default, Deserialize, Serialize)]
pub(super) struct Account {
	login: Option<String>,
	/// By lowercased owner.
	keys: BTreeMap<String, String>,
	/// By `owner` or `owner/repo`, lowercased.
	scopes: BTreeMap<String, Scoped>,
}
#[derive(Default, Deserialize, Serialize)]
struct Scoped {
	/// What the account's own key listed of it on its last pull.
	listed: BTreeSet<String>,
	outcome: Option<Outcome>,
}
impl Account {
	fn reach(&self) -> Reach {
		let listed = self.scopes.values().flat_map(|s| s.listed.iter().cloned());
		Reach::Of(settings().watch.iter().map(|w| w.to_lowercase()).chain(listed).collect())
	}

	fn actor(&self) -> Result<Actor> {
		Ok(Actor::new(self.login.clone(), &self.keys)?)
	}

	/// The scopes naming `repo`.
	fn naming(&self, repo: &str) -> Vec<String> {
		self.scopes.keys().filter(|s| names(s, repo)).cloned().collect()
	}

	fn shown(&self) -> api::Account {
		let outcome_on = |owner: &str| {
			let outcomes = self.scopes.iter().filter(|(scope, _)| owner_of(scope) == owner).filter_map(|(_, s)| s.outcome.clone());
			outcomes.max_by_key(|o| (o.error.is_some(), o.at))
		};
		api::Account {
			login: self.login.clone(),
			keys: self
				.keys
				.keys()
				.map(|owner| Key {
					owner: owner.clone(),
					outcome: outcome_on(owner),
				})
				.collect(),
			scopes: self
				.scopes
				.iter()
				.map(|(scope, s)| Stored {
					scope: scope.clone(),
					repos: s.listed.len(),
					outcome: s.outcome.clone(),
				})
				.collect(),
		}
	}
}

pub async fn account(headers: &HeaderMap) -> Result<api::Account> {
	Ok(read(&gated(headers)?)?.shown())
}

pub async fn edit_account(headers: &HeaderMap, edit: AccountEdit) -> Result<api::Account> {
	let key = gated(headers)?;
	let verified = match &edit {
		AccountEdit::Key { owner, token } => Some(verify(owner, token).await?),
		_ => None,
	};
	let _held = ACCOUNTS.lock().await;
	let mut account = read(&key)?;
	match edit {
		AccountEdit::Key { owner, token } => {
			let owner = owner.trim().to_lowercase();
			let login = verified.expect("verified above");
			if let Some(held) = account.login.as_ref().filter(|held| **held != login)
				&& account.keys.keys().any(|o| *o != owner)
			{
				bail!("that key is {login}'s, and this account's are {held}'s");
			}
			account.login = Some(login);
			account.keys.insert(owner, token.trim().to_string());
		}
		AccountEdit::DropKey(owner) => {
			if account.keys.remove(&owner).is_none() {
				bail!("no key on {owner} is held");
			}
			if account.keys.is_empty() {
				account.login = None;
			}
			for (_, scoped) in account.scopes.iter_mut().filter(|(scope, _)| owner_of(scope) == owner) {
				scoped.listed.clear(); // reached by that key alone
			}
		}
		AccountEdit::Scope(scope) => {
			let scope = scope.trim().trim_matches('/').to_lowercase();
			let parts: Vec<&str> = scope.split('/').collect();
			let plain = |s: &&str| !s.is_empty() && s.chars().all(|c| c.is_ascii_alphanumeric() || "._-".contains(c));
			if parts.len() > 2 || !parts.iter().all(plain) || parts[0].len() > 39 || parts.get(1).is_some_and(|r| r.len() > 100) {
				bail!("a scope is `owner` or `owner/repo`, not {scope:?}");
			}
			account.scopes.entry(scope).or_default();
		}
		AccountEdit::DropScope(scope) =>
			if account.scopes.remove(&scope).is_none() {
				bail!("{scope} is no scope of yours");
			},
	}
	write(&key, &account)?;
	super::announce(super::Pull::Account(key));
	Ok(account.shown())
}

/// Every account's scopes, pulled as their viewer; of one, its own naming `only` when that is given.
pub(super) async fn pull(key: &str, only: Option<&str>) -> Result<()> {
	let account = read(key)?;
	let actor = account.actor()?;
	let scopes: Vec<String> = match only {
		Some(repo) => account.naming(repo),
		None => account.scopes.keys().cloned().collect(),
	};
	let watched = Reach::Of(settings().watch.iter().map(|w| w.to_lowercase()).collect());
	for scope in scopes {
		let listed = match account.keys.contains_key(owner_of(&scope)) {
			true => github::client::acting(actor.clone(), list(&scope)).await,
			false => Err(GithubError::new_other(format!("no key on {}", owner_of(&scope)))),
		};
		let pulled = match &listed {
			Ok(repos) => {
				let targets: Vec<String> = repos
					.iter()
					.filter(|r| !watched.covers(**r))
					.map(|r| r.to_string())
					.filter(|r| only.is_none_or(|o| o.eq_ignore_ascii_case(r)))
					.collect();
				github::client::acting(actor.clone(), tedi_task_operations::pull::pull_repos(&targets, &settings().workflows, Timestamp::now())).await
			}
			Err(e) => Err(color_eyre::eyre::eyre!("{e}")),
		};
		let _held = ACCOUNTS.lock().await;
		let mut account = read(key)?;
		let Some(scoped) = account.scopes.get_mut(&scope) else { continue }; // dropped meanwhile
		match listed {
			Ok(repos) => scoped.listed = repos.iter().map(ToString::to_string).collect(),
			Err(e) if e.is_transient() => {} // GitHub didn't say: what it last listed stands
			Err(_) => scoped.listed.clear(),
		}
		if !account.keys.contains_key(owner_of(&scope)) {
			account.scopes.get_mut(&scope).expect("held above").listed.clear(); // dropped meanwhile
		}
		account.scopes.get_mut(&scope).expect("held above").outcome = Some(Outcome {
			at: Timestamp::now(),
			error: pulled.err().map(|e| format!("{e:#}")),
		});
		write(key, &account)?;
	}
	Ok(())
}

/// The accounts with a scope naming `repo`.
pub(super) fn naming(repo: &str) -> Result<Vec<String>> {
	let mut out = Vec::new();
	for key in keys()? {
		if !read(&key)?.naming(repo).is_empty() {
			out.push(key);
		}
	}
	Ok(out)
}

pub(super) fn keys() -> Result<Vec<String>> {
	let entries = match std::fs::read_dir(dir()) {
		Ok(entries) => entries,
		Err(e) if e.kind() == std::io::ErrorKind::NotFound => return Ok(Vec::new()), // nobody brought anything yet
		Err(e) => return Err(e.into()),
	};
	let mut out = Vec::new();
	for entry in entries {
		let path = entry?.path();
		if path.extension().is_some_and(|e| e == "json")
			&& let Some(key) = path.file_stem().and_then(|s| s.to_str())
		{
			out.push(key.to_string());
		}
	}
	Ok(out)
}

/// Held across each account's read-modify-write.
static ACCOUNTS: Mutex<()> = Mutex::const_new(());

/// The viewer's, where viewers are told apart.
fn gated(headers: &HeaderMap) -> Result<String> {
	match settings().viewer {
		Viewer::Header(_) => viewer(headers),
		Viewer::Operator => bail!("this instance is ungated: it acts as its operator, who reaches all of it"),
	}
}

//#[codestyle::skip(swappable-args)]
/// Whose `token` is, once it lists `owner`.
async fn verify(owner: &str, token: &str) -> Result<String> {
	let owner = owner.trim();
	let keyed = github::client::get()?.keyed(&BTreeMap::from([(owner.to_lowercase(), token.trim().to_string())]));
	let login = keyed.fetch_authenticated_user().await.wrap_err("GitHub takes no such token")?;
	keyed.list_owner_repos(owner).await.wrap_err_with(|| format!("{login}'s token doesn't list {owner}"))?;
	Ok(login)
}

async fn list(scope: &str) -> Result<Vec<RepoInfo>, GithubError> {
	let gh = github::client::get()?;
	match scope.split_once('/') {
		None => gh.list_owner_repos(scope).await,
		Some((owner, repo)) => {
			let repo = RepoInfo::new(owner, repo);
			match gh.repo_exists(repo).await? {
				true => Ok(vec![repo]),
				false => Err(GithubError::new_other(format!("no {repo} your key sees"))),
			}
		}
	}
}

fn owner_of(scope: &str) -> &str {
	scope.split_once('/').map_or(scope, |(owner, _)| owner)
}

//#[codestyle::skip(swappable-args)]
fn names(scope: &str, repo: &str) -> bool {
	repo.eq_ignore_ascii_case(scope) || owner_of(repo).eq_ignore_ascii_case(scope)
}

fn dir() -> PathBuf {
	tedi_task_operations::paths::data_dir("web/accounts")
}

fn read(key: &str) -> Result<Account> {
	let file = dir().join(format!("{key}.json"));
	match std::fs::read_to_string(&file) {
		Ok(json) => serde_json::from_str(&json).wrap_err_with(|| format!("{} doesn't read as an account", file.display())),
		Err(e) if e.kind() == std::io::ErrorKind::NotFound => Ok(Account::default()), // a viewer who never brought anything
		Err(e) => Err(e.into()),
	}
}

fn write(key: &str, account: &Account) -> Result<()> {
	use std::{io::Write as _, os::unix::fs::OpenOptionsExt as _};

	let file = dir().join(format!("{key}.json"));
	let fresh = file.with_extension("json.new");
	match std::fs::remove_file(&fresh) {
		Ok(()) => {} // a crash's: `mode` is only a new file's
		Err(e) if e.kind() == std::io::ErrorKind::NotFound => {}
		Err(e) => return Err(e.into()),
	}
	let mut out = std::fs::OpenOptions::new().write(true).create_new(true).mode(0o600).open(&fresh)?;
	out.write_all(serde_json::to_string_pretty(account)?.as_bytes())?;
	std::fs::rename(&fresh, &file)?; // whole or not at all
	Ok(())
}
