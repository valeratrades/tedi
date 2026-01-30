#![allow(clippy::len_zero)]
pub mod config;
pub mod utils;
#[tokio::main]
async fn main() {
	{
		//TODO: deprecate color_eyre::install() after full miette migration
		color_eyre::config::HookBuilder::default().capture_span_trace_by_default(false).install().unwrap();
		miette::set_hook(Box::new(|_| Box::new(miette::MietteHandlerOpts::new().terminal_links(true).context_lines(3).build()))).expect("miette hook already set");
		if std::env::var("__IS_INTEGRATION_TEST").is_ok() {
			// SAFETY: This is called at program start before any other threads are spawned
			unsafe { std::env::set_var("LOG_DIRECTIVES", concat!("debug,", env!("CARGO_PKG_NAME"), "=debug")) };
			v_utils::utils::init_subscriber(v_utils::utils::LogDestination::default());
		} else if let Some(filename) = extract_log_to() {
			v_utils::utils::init_subscriber(v_utils::utils::LogDestination::xdg(env!("CARGO_PKG_NAME")).fname(filename).stderr_errors(true));
		} else {
			v_utils::utils::init_subscriber(v_utils::utils::LogDestination::xdg(env!("CARGO_PKG_NAME")).stderr_errors(true));
		}
	}

	let cli = Cli::parse();

	let settings = match config::LiveSettings::new(cli.settings_flags.clone(), Duration::from_secs(3)) {
		Ok(s) => s,
		Err(e) => {
			eprintln!("Error: {e}");
			std::process::exit(1);
		}
	};

	// Commands that require GitHub client (when not offline)
	let needs_github = matches!(cli.command, Commands::Open(_) | Commands::Blocker(_)) && !cli.offline;

	let github_client: Option<github::BoxedGithubClient> = if cli.mock {
		Some(std::sync::Arc::new(mock_github::MockGithubClient::new("mock_user")))
	} else if needs_github {
		match github::RealGithubClient::new(&settings) {
			Ok(client) => Some(std::sync::Arc::new(client)),
			Err(e) => {
				eprintln!("Error: {e}");
				std::process::exit(1);
			}
		}
	} else {
		// Commands that don't need GitHub - don't initialize client
		None
	};

	// Set global GitHub client for sink operations (if initialized)
	if let Some(client) = github_client {
		github::client::set(client);
	}

	// All the functions here can rely on config being correct.
	let success = match cli.command {
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
		Commands::Open(args) => open_interactions::open_command(&settings, args, cli.offline).await,
	};

	match success {
		Ok(_) => std::process::exit(0),
		Err(e) => {
			eprintln!("{e:?}");
			std::process::exit(1);
		}
	}
}
mod blocker_interactions;
mod github;
mod manual_stats;
mod milestones;
mod mock_github;
mod open_interactions;
mod perf_eval;
mod shell_init;
mod watch_monitors;
use std::time::Duration;

use clap::{Parser, Subcommand};

const MANUAL_PATH_APPENDIX: &str = "manual_stats/";

#[derive(Parser)]
#[command(author, version = concat!(env!("CARGO_PKG_VERSION"), " (", env!("GIT_HASH"), ")"), about, long_about = None)]
struct Cli {
	#[command(subcommand)]
	command: Commands,
	#[clap(flatten)]
	settings_flags: config::SettingsFlags,
	#[arg(long, global = true, hide = true)]
	mock: bool,
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
