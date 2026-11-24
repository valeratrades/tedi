use std::{collections::BTreeMap, path::PathBuf};

use tedi_adapters::home::Home;
use tedi_task_operations::workflow::Workflows;
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
	/// What `tedi view` and the web board read. An instance holds its workflows with this, or asks the one that does: its `home`.
	#[settings(skip)]
	board: Option<Board>,
	/// The `tedi web` holding the workflow states, by the url it is served at.
	home: Option<String>,
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

	/// Who answers for workflow states.
	pub fn states(&self) -> color_eyre::Result<States> {
		match (&self.board, &self.home) {
			(Some(_), Some(_)) => Err(color_eyre::eyre::eyre!(
				"both `board` and `home` are set: an instance holds its workflows, or asks the one that does"
			)),
			(Some(board), None) => Ok(States::Here(board.clone())),
			(None, Some(home)) => Ok(States::Home(Home::new(home, session_file()))),
			(None, None) => Ok(States::Nowhere),
		}
	}
}

pub enum States {
	Here(Board),
	Home(Home),
	/// Nothing places issues in anything.
	Nowhere,
}
impl States {
	/// Those held here: with a home, no rule of them runs.
	pub fn workflows(&self) -> color_eyre::Result<Workflows> {
		match self {
			States::Here(board) => board.workflows(),
			States::Home(_) | States::Nowhere => Ok(Workflows::default()),
		}
	}
}

/// Where `tedi login` keeps the session with the home.
pub fn session_file() -> PathBuf {
	tedi_task_operations::paths::state_file("session.json")
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
	pub views: Vec<tedi_web::api::View>,
	/// Named slices of the team's repos, nesting narrower ones (`[board.scopes.EV-Main.tooling] repos = ["EV-Main/tilers"]`).
	#[serde(default)] // a board may have none
	pub scopes: BTreeMap<String, tedi_task_operations::board::Scope>,
	/// Column colorings every viewer is offered (`[board.columns.notion] by = "state:kanban"`, `tints = { triage = "orange" }`).
	#[serde(default)] // a board may have none
	pub columns: BTreeMap<String, tedi_web::api::Columns>,
	/// Workspaces every viewer is offered (`tiles = { Xl = "<tilers layout>" }`, `lens = { scope = "repo:EV-Main/*", search = "is:open", sort = "rank" }`).
	#[serde(default)] // a board may have none
	pub workspaces: BTreeMap<String, tedi_web::api::Workspace>,
	/// Where this board's team config lives in git: its history numbers each object's versions, and a proposal is a pull request on it.
	pub source: Option<tedi_web::Source>,
}
impl Board {
	/// Its workflows, loaded with their hooks; every workflow a view, a column layout or a workspace's lens names is among them.
	pub fn workflows(&self) -> color_eyre::Result<Workflows> {
		use color_eyre::eyre::bail;
		use tedi_task_operations::board::{Facet, GroupBy, Shown, SortKey};

		let loaded = Workflows::load(&self.workflows)?;
		for view in &self.views {
			let mut named: Vec<&String> = view.shown.iter().filter_map(|s| if let Shown::State(w) = s { Some(w) } else { None }).collect();
			named.extend(if let GroupBy::State(w) = &view.group_by { Some(w) } else { None });
			if let Some(w) = named.into_iter().find(|w| !loaded.trees.contains_key(*w)) {
				bail!("the view {} names the workflow {w}, and `board.workflows` doesn't", view.name);
			}
		}
		for (name, columns) in &self.columns {
			if let GroupBy::State(w) = &columns.by
				&& !loaded.trees.contains_key(w)
			{
				bail!("the columns {name} are by the workflow {w}, and `board.workflows` doesn't name it");
			}
		}
		for (name, workspace) in &self.workspaces {
			let lens = &workspace.lens;
			let mut named: Vec<String> = lens
				.sort
				.keys()
				.iter()
				.filter_map(|s| if let SortKey::State(w) = &s.by { Some(w.clone()) } else { None })
				.collect();
			named.extend([&*lens.scope, &lens.search].iter().flat_map(|q| q.picked(Facet::State)).map(|at| {
				let (workflow, _) = at.trim_start_matches('-').split_once('/').expect("a state: value is <workflow>/<bucket>");
				workflow.to_string()
			}));
			if let Some(w) = named.into_iter().find(|w| !loaded.trees.contains_key(w)) {
				bail!("the workspace {name}'s lens names the workflow {w}, and `board.workflows` doesn't");
			}
		}
		Ok(loaded)
	}
}

#[derive(Clone, Debug, v_macros::MyConfigPrimitives, v_macros::SettingsNested)]
pub struct Milestones {
	/// Github repo URL for milestones (e.g., `https://github.com/owner/repo` or `owner/repo`)
	pub url: String,
	/// Which milestone timeframe to source blockers from (default: "1d")
	pub blocker_timeframe: Option<String>,
	/// The bucket of the `home`'s that reads as picked up (`kanban/selected`): opening a planning sprint opens it beside
	pub selected: Option<String>,
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
