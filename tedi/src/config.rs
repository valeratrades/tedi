use std::collections::BTreeMap;

use v_utils::macros as v_macros;

pub const EXE_NAME: &str = env!("CARGO_PKG_NAME");

#[derive(Clone, Debug, v_macros::LiveSettings, v_macros::MyConfigPrimitives, v_macros::Settings)]
pub struct AppConfig {
	/// Exactly one of `github_token` and `github_app` is set: [`AppConfig::github`] reads them.
	github_token: Option<String>,
	/// As a GitHub App, whose installations each carry their own rate budget.
	#[settings(skip)]
	github_app: Option<GithubApp>,
	/// The GitHub App's webhook secret: `tedi web` takes deliveries only with it.
	pub github_webhook_secret: Option<String>,
	/// Max GitHub requests in flight at once (default: 32). GitHub's secondary rate limit trips around 100.
	pub github_max_in_flight: Option<usize>,
	pub timer: Option<Timer>,
	pub milestones: Option<Milestones>,
	pub manual_stats: Option<ManualStats>,
	/// Orgs (`owner`) and repos (`owner/repo`) `tedi pull` stores when none are named.
	pub watch: Option<Vec<String>>,
	/// Main org → the orgs it owns (`orgs.EV-Main = ["EV-Quant"]`); owning one doesn't pull it.
	#[settings(skip)]
	#[serde(default)] // a user may have none
	pub orgs: tedi_core::chain::Orgs,
	/// What `tedi view` and the web board read.
	#[settings(skip)]
	pub board: Option<Board>,
	#[settings(skip)]
	#[serde(default)]
	pub yes: bool,
}
// Two keys rather than one flattened enum: v_utils' Settings warns of every key a flattened field takes as unknown.
impl AppConfig {
	pub fn github(&self) -> color_eyre::Result<GithubAuth> {
		match (&self.github_token, &self.github_app) {
			(Some(token), None) => Ok(GithubAuth::Token(token.clone())),
			(None, Some(app)) => Ok(GithubAuth::App(app.clone())),
			(Some(_), Some(_)) => Err(color_eyre::eyre::eyre!("both `github_token` and `github_app` are set: tedi acts as one of them")),
			(None, None) => Err(color_eyre::eyre::eyre!("neither `github_token` nor `github_app` is set")),
		}
	}
}

/// Whom tedi acts as on GitHub.
pub enum GithubAuth {
	Token(String),
	App(GithubApp),
}
#[derive(Clone, Debug, v_macros::MyConfigPrimitives)]
pub struct GithubApp {
	#[private_value]
	pub id: u64,
	/// The PEM GitHub hands out.
	pub private_key: String,
}

#[derive(Clone, Debug, serde::Deserialize, serde::Serialize)]
pub struct Board {
	/// By name: `preset:<name>` (`kanban`, `pull_requests`) or a workflow file's path.
	#[serde(default)] // a board may place issues in none
	pub workflows: BTreeMap<String, String>,
	pub views: Vec<tedi_task_operations::board::View>,
	/// Named slices of the team's repos, nesting narrower ones (`[board.scopes.EV-Main.tooling] repos = ["EV-Main/tilers"]`).
	#[serde(default)] // a board may have none
	pub scopes: BTreeMap<String, tedi_task_operations::board::Scope>,
	/// Column colorings every viewer is offered (`[board.columns.notion] by = "state:kanban"`, `tints = { triage = "orange" }`).
	#[serde(default)] // a board may have none
	pub columns: BTreeMap<String, tedi_web::api::Columns>,
	/// Dock arrangements every viewer is offered, per band (`Xl = "<tilers layout>"`).
	#[serde(default)] // a board may have none
	pub workspaces: BTreeMap<String, tedi_web::api::Arrangement>,
	/// Where this board's team config lives in git: its history numbers each object's versions, and a proposal is a pull request on it.
	pub source: Option<tedi_web::Source>,
}
impl Board {
	/// Its workflows, loaded with their hooks; every workflow a view or a column coloring names is among them.
	pub fn workflows(&self) -> color_eyre::Result<tedi_task_operations::workflow::Workflows> {
		use color_eyre::eyre::{WrapErr as _, bail};
		use tedi_task_operations::board::{GroupBy, Shown, SortKey};

		let loaded: tedi_task_operations::workflow::Workflows = self
			.workflows
			.iter()
			.map(|(name, spec)| {
				Ok((
					name.clone(),
					tedi_task_operations::workflow::Loaded::load(spec).wrap_err_with(|| format!("the workflow {name} ({spec})"))?,
				))
			})
			.collect::<color_eyre::Result<_>>()?;
		for view in &self.views {
			let mut named: Vec<&String> = view.shown.iter().filter_map(|s| if let Shown::State(w) = s { Some(w) } else { None }).collect();
			named.extend(if let GroupBy::State(w) = &view.group_by { Some(w) } else { None });
			named.extend(view.sort.keys().iter().filter_map(|s| if let SortKey::State(w) = &s.by { Some(w) } else { None }));
			if let Some(w) = named.into_iter().find(|w| !loaded.contains_key(*w)) {
				bail!("the view {} names the workflow {w}, and `board.workflows` doesn't", view.name);
			}
		}
		for (name, columns) in &self.columns {
			if let GroupBy::State(w) = &columns.by
				&& !loaded.contains_key(w)
			{
				bail!("the columns {name} are by the workflow {w}, and `board.workflows` doesn't name it");
			}
		}
		Ok(loaded)
	}
}

#[derive(Clone, Debug, v_macros::MyConfigPrimitives, v_macros::SettingsNested)]
pub struct Milestones {
	/// Github repo URL for milestones (e.g., "https://github.com/owner/repo" or "owner/repo")
	pub url: String,
	/// Which milestone timeframe to source blockers from (default: "1d")
	pub blocker_timeframe: Option<String>,
}
impl Milestones {
	pub fn blocker_tf(&self) -> &str {
		self.blocker_timeframe.as_deref().unwrap_or("1d")
	}
}

#[derive(Clone, Debug, v_macros::MyConfigPrimitives, smart_default::SmartDefault)]
pub struct ManualStats {
	#[default = "%Y-%m-%d"]
	pub date_format: String,
}

#[derive(Clone, Debug, Default, v_macros::MyConfigPrimitives, v_macros::SettingsNested)]
pub struct Timer {
	pub hard_stop_coeff: f32,
}
