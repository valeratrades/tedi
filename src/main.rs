#![feature(try_blocks)]
#![allow(clippy::len_zero)]
#![allow(clippy::doc_lazy_continuation)]
const MANUAL_PATH_APPENDIX: &str = "manual_stats/";
mod blocker_interactions;
pub mod config;
mod manual_stats;
mod milestones;
mod mock_github;
mod open_interactions;
mod perf_eval;
mod shell_init;
pub mod utils;
mod watch_monitors;
use std::time::Duration;

use clap::{Parser, ValueEnum};
use v_utils::utils::exit_on_error;

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
#[derive(clap::Parser)]
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
#[derive(clap::Subcommand)]
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
	/// Monitor screenshots: watch daemon and annotation
	Monitors(watch_monitors::MonitorsArgs),
	/// Open a Github issue in $EDITOR
	Open(open_interactions::OpenArgs),
}
#[tokio::main]
async fn main() {
	v_utils::clientside!(extract_log_to());

	let cli = Cli::parse();

	let settings = exit_on_error(config::LiveSettings::new(cli.settings_flags.clone(), Duration::from_secs(3)));

	// Commands that may need GitHub client (always construct it - offline only skips network calls)
	let has_github_commands = matches!(cli.command, Commands::Open(_) | Commands::Blocker(_) | Commands::Milestones(_));

	let github_client: Option<tedi::github::BoxedGithubClient> = if cli.mock.is_some() {
		Some(std::sync::Arc::new(mock_github::MockGithubClient::new("mock_user")))
	} else if has_github_commands {
		let config = exit_on_error(settings.config());
		let client = tedi::github::RealGithubClient::new(config.github_token.clone());
		Some(std::sync::Arc::new(client))
	} else {
		None
	};

	// Set global GitHub client and current user for sink operations
	if let Some(client) = &github_client {
		if !cli.offline || cli.mock.is_some() {
			if let Ok(user) = client.fetch_authenticated_user().await {
				tracing::info!("Authenticated as: {user}");
				let cache_path = v_utils::xdg_cache_file!("authenticated_user.txt");
				let _ = std::fs::write(&cache_path, &user);
				tedi::current_user::set(user);
			}
		}
		tedi::github::client::set(client.clone());
	}
	// Load cached user if not fetched from network
	if tedi::current_user::get().is_none() {
		let cache_path = v_utils::xdg_cache_file!("authenticated_user.txt");
		if let Ok(user) = std::fs::read_to_string(&cache_path) {
			let user = user.trim().to_string();
			if !user.is_empty() {
				tracing::info!("Loaded cached user: {user}");
				tedi::current_user::set(user);
			}
		}
	}

	// All the functions here can rely on config being correct.
	exit_on_error(match cli.command {
		Commands::Manual(manual_args) => manual_stats::update_or_open(&settings, manual_args).await,
		Commands::Milestones(milestones_command) => milestones::milestones_command(&settings, milestones_command, cli.mock).await,
		Commands::Init(args) => {
			shell_init::output(&settings, args);
			Ok(())
		}
		Commands::Blocker(args) => blocker_interactions::main(args, cli.offline).await,
		Commands::Clockify(args) => blocker_interactions::clockify::clockify_main(&settings, args).await,
		Commands::PerfEval(args) => perf_eval::main(&settings, args).await,
		Commands::Monitors(args) => watch_monitors::main(&settings, args).await,
		Commands::Open(args) => open_interactions::open_command(&settings, args, cli.offline, cli.mock).await,
	});
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
