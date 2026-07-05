#![allow(clippy::len_zero)]
#![allow(clippy::doc_lazy_continuation)]
mod config;
mod shell_init;
mod sprints;
use std::{sync::Arc, time::Duration};

use clap::Parser;
use tedi_ops::MockType;
use v_utils::utils::exit_on_error;

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
	Manual(tedi_eval::manual_stats::ManualArgs),
	/// Operations with sprints (1d, 2w, 1Q, 1y, 3y, 7y)
	Sprints(sprints::SprintsArgs),
	/// Shell aliases and hooks. Usage: `todos init <shell> | source`
	Init(shell_init::ShellInitArgs),
	/// Clockify time tracking
	Clockify(tedi_adapters::clockify::ClockifyArgs),
	/// Performance evaluation with screenshots
	PerfEval(tedi_eval::perf_eval::PerfEvalArgs),
	/// Monitor screenshots: watch daemon and annotation
	Monitors(tedi_eval::watch_monitors::MonitorsArgs),
	/// Open a Github issue in $EDITOR
	Open(tedi_ops::open_interactions::OpenArgs),
}
#[tokio::main]
async fn main() {
	v_utils::clientside!(extract_log_to());

	let cli = Cli::parse();
	let settings = Arc::new(exit_on_error(config::LiveSettings::new(cli.settings_flags.clone(), Duration::from_secs(3))));

	// Commands that may need GitHub client (always construct it - offline only skips network calls)
	let has_github_commands = matches!(cli.command, Commands::Open(_) | Commands::Sprints(_));

	let github_client: Option<tedi_adapters::github::BoxedGithubClient> = if cli.mock.is_some() {
		Some(Arc::new(tedi_ops::mock_github::MockGithubClient::new("mock_user")))
	} else if has_github_commands {
		let config = exit_on_error(settings.config());
		let client = tedi_adapters::github::RealGithubClient::new(config.github_token.clone());
		Some(Arc::new(client))
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
				tedi_ops::current_user::set(user);
			}
		}
		tedi_adapters::github::client::set(client.clone());
	}
	// Load cached user if not fetched from network
	if tedi_ops::current_user::get().is_none() {
		let cache_path = v_utils::xdg_cache_file!("authenticated_user.txt");
		if let Ok(user) = std::fs::read_to_string(&cache_path) {
			let user = user.trim().to_string();
			if !user.is_empty() {
				tracing::info!("Loaded cached user: {user}");
				tedi_ops::current_user::set(user);
			}
		}
	}

	// All the functions here can rely on config being correct.
	exit_on_error(match cli.command {
		Commands::Manual(manual_args) => {
			let config = exit_on_error(settings.config());
			// Empty format string in config would silently produce empty dates, so treat it as unset.
			let date_format = config.manual_stats.as_ref().map(|m| m.date_format.as_str()).filter(|s| !s.is_empty()).unwrap_or("%Y-%m-%d");
			tedi_eval::manual_stats::update_or_open(date_format, manual_args).await
		}
		Commands::Sprints(sprints_command) => sprints::sprints_command(&*settings, sprints_command, cli.mock, cli.offline).await,
		Commands::Init(args) => {
			shell_init::output(&*settings, args);
			Ok(())
		}
		Commands::Clockify(args) => tedi_adapters::clockify::main(exit_on_error(settings.config()).yes, args).await,
		Commands::PerfEval(args) => tedi_eval::perf_eval::main(args).await,
		Commands::Monitors(args) => tedi_eval::watch_monitors::main(args).await,
		Commands::Open(args) => tedi_ops::open_interactions::open_command(args, cli.offline, cli.mock).await,
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
