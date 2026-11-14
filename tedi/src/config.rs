use v_utils::macros as v_macros;

pub const EXE_NAME: &str = env!("CARGO_PKG_NAME");

#[derive(Clone, Debug, Default, v_macros::LiveSettings, v_macros::MyConfigPrimitives, v_macros::Settings)]
pub struct AppConfig {
	pub github_token: String,
	/// The GitHub App's webhook secret: `tedi web` takes deliveries only with it.
	pub github_webhook_secret: Option<String>,
	/// Max GitHub requests in flight at once (default: 32). GitHub's secondary rate limit trips around 100.
	pub github_max_in_flight: Option<usize>,
	pub timer: Option<Timer>,
	pub milestones: Option<Milestones>,
	pub manual_stats: Option<ManualStats>,
	/// Orgs (`owner`) and repos (`owner/repo`) `tedi pull` stores when none are named.
	pub watch: Option<Vec<String>>,
	/// What `tedi view` and the web board read.
	#[settings(skip)]
	pub board: Option<Board>,
	#[settings(skip)]
	#[serde(default)]
	pub yes: bool,
}

#[derive(Clone, Debug, serde::Deserialize, serde::Serialize)]
pub struct Board {
	/// A closed issue leaves Done / Won't do for Backlog this long after closing (`1w`, `10d`).
	pub archive_after: String,
	pub views: Vec<tedi_task_operations::board::View>,
	/// Named slices of the team's repos, nesting narrower ones (`[board.scopes.EV-Main.tooling] repos = ["EV-Main/tilers"]`).
	#[serde(default)] // a board may have none
	pub scopes: std::collections::BTreeMap<String, tedi_task_operations::board::Scope>,
}
impl Board {
	pub fn archive_after(&self) -> color_eyre::Result<jiff::SignedDuration> {
		let span: jiff::Span = self.archive_after.parse()?;
		Ok(span.to_duration(jiff::SpanRelativeTo::days_are_24_hours())?)
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
