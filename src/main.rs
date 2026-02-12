#![feature(try_blocks)]
#![allow(clippy::len_zero)]
#![allow(clippy::doc_lazy_continuation)]
const MANUAL_PATH_APPENDIX: &str = "manual_stats/";
pub mod config;
pub mod utils;
/// Mock behavior type for testing.
#[derive(Clone, Copy, Debug, Default, ValueEnum)]
pub enum MockType {
	/// Standard mock - uses mock Github client but normal editor flow.
	#[default]
	#[value(name = "")]
	Standard,
	/// Ghost edit - skip editor and pretend edit was made.
	GhostEdit,
}
#[tokio::main]
async fn main() {
	v_utils::clientside!(extract_log_to());

	let cli = Cli::parse();

	let settings = exit_on_error(config::LiveSettings::new(cli.settings_flags.clone(), Duration::from_secs(3)));

	// Commands that require GitHub client (when not offline)
	let needs_github = matches!(cli.command, Commands::Open(_) | Commands::Blocker(_)) && !cli.offline;

	let github_client: Option<tedi::github::BoxedGithubClient> = if cli.mock.is_some() {
		Some(std::sync::Arc::new(mock_github::MockGithubClient::new("mock_user")))
	} else if needs_github {
		let config = exit_on_error(settings.config());
		let client = tedi::github::RealGithubClient::new(config.github_token.clone());
		Some(std::sync::Arc::new(client))
	} else {
		// Commands that don't need GitHub - don't initialize client
		None
	};

	// Set global GitHub client and current user for sink operations
	if let Some(client) = github_client {
		if let Ok(user) = client.fetch_authenticated_user().await {
			tracing::info!("Authenticated as: {user}");
			tedi::current_user::set(user);
		}
		tedi::github::client::set(client);
	}

	// All the functions here can rely on config being correct.
	exit_on_error(match cli.command {
		Commands::Manual(manual_args) => manual_stats::update_or_open(&settings, manual_args).await,
		Commands::Milestones(milestones_command) => milestones::milestones_command(&settings, milestones_command).await,
		Commands::Init(args) => {
			shell_init::output(&settings, args);
			Ok(())
		}
		Commands::Blocker(args) => blocker_interactions::main(args, cli.offline).await,
		Commands::Clockify(args) => blocker_interactions::clockify::clockify_main(&settings, args).await,
		Commands::PerfEval(args) => perf_eval::main(&settings, args).await,
		Commands::WatchMonitors(args) => watch_monitors::main(&settings, args),
		Commands::Open(args) => open_interactions::open_command(&settings, args, cli.offline, cli.mock).await,
	});
}
mod blocker_interactions;
mod manual_stats;
mod milestones;
mod mock_github;
mod open_interactions;
mod perf_eval;
mod shell_init;
mod watch_monitors;
use std::time::Duration;

use clap::{Parser, Subcommand, ValueEnum};
use v_utils::utils::exit_on_error;

#[derive(Parser)]
#[command(author, version = concat!(env!("CARGO_PKG_VERSION"), " (", env!("GIT_HASH"), ")"), about, long_about = None)]
struct Cli {
	#[command(subcommand)]
	command: Commands,
	#[clap(flatten)]
	settings_flags: config::SettingsFlags,
	/// Use mock Github client. Optionally specify mock behavior.
	#[arg(long, global = true, hide = true, num_args = 0..=1, require_equals = true, default_missing_value = "")]
	mock: Option<MockType>,
	/// Skip all network operations - edit locally only, don't sync to Github.
	/// Automatically enabled for virtual projects (projects without Github remote).
	#[arg(long, global = true)]
	offline: bool,
	/// Log to a specific file (filename only, no path). Logs go to ~/.local/state/tedi/{filename}.log
	#[arg(long, global = true)]
	log_to: Option<String>,
}

#[derive(Subcommand)]
enum Commands {
	/// Record day's ev and other stats.
	///Following records ev of 420 for yesterday, then opens the file.
	///```rust
	///todo manual -d1 --ev 420 -o
	///```
	Manual(manual_stats::ManualArgs),
	/// Operations with milestones (1d, 1w, 1M, 1Q, 1y)
	Milestones(milestones::MilestonesArgs),
	/// Shell aliases and hooks. Usage: `todos init <shell> | source`
	Init(shell_init::ShellInitArgs),
	/// Blockers tree (use --integrated flag for issue files)
	Blocker(blocker_interactions::BlockerArgs),
	/// Clockify time tracking
	Clockify(blocker_interactions::clockify::ClockifyArgs),
	/// Performance evaluation with screenshots
	PerfEval(perf_eval::PerfEvalArgs),
	/// Watch monitors daemon - takes screenshots every 60s
	WatchMonitors(watch_monitors::WatchMonitorsArgs),
	/// Open a Github issue in $EDITOR
	Open(open_interactions::OpenArgs),
}

/// Extract --log-to value from args before full CLI parsing (needed for early logging init)
fn extract_log_to() -> Option<String> {
	let args: Vec<String> = std::env::args().collect();
	for (i, arg) in args.iter().enumerate() {
		if arg == "--log-to" {
			if let Some(value) = args.get(i + 1) {
				if value.contains('/') || value.contains('\\') {
					eprintln!("Error: --log-to accepts filename only, not a path");
					std::process::exit(1);
				}
				return Some(value.clone());
			}
		} else if let Some(value) = arg.strip_prefix("--log-to=") {
			if value.contains('/') || value.contains('\\') {
				eprintln!("Error: --log-to accepts filename only, not a path");
				std::process::exit(1);
			}
			return Some(value.to_string());
		}
	}
	None
}

// new work
