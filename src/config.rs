use v_utils::macros as v_macros;

pub const EXE_NAME: &str = env!("CARGO_PKG_NAME");

#[derive(Clone, Debug, Default, v_macros::LiveSettings, v_macros::MyConfigPrimitives, v_macros::Settings)]
pub struct AppConfig {
	pub github_token: String,
	pub timer: Option<Timer>,
	pub milestones: Option<Milestones>,
	pub manual_stats: Option<ManualStats>,
}

#[derive(Clone, Debug, v_macros::MyConfigPrimitives, v_macros::SettingsNested)]
pub struct Milestones {
	/// Github repo URL for milestones (e.g., "https://github.com/owner/repo" or "owner/repo")
	pub url: String,
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
