#![feature(default_field_values)]
#![allow(clippy::len_zero)]
#![allow(clippy::doc_lazy_continuation)]
mod config;
mod shell_init;
mod sprints;
mod view;
mod web;
use std::{sync::Arc, time::Duration};

use clap::Parser;
use tedi_task_operations::{MockType, open_interactions::SyncMode};
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
	/// Open the editor on local state; reach Github only once it closes.
	#[arg(long, global = true, conflicts_with = "offline")]
	sync_on_exit: bool,
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
	Open(tedi_task_operations::open_interactions::OpenArgs),
	/// Open a query over the store as a task view in $EDITOR; edits to the issues it expands sync
	View(view::ViewArgs),
	/// Store every issue of orgs (`owner`) and repos (`owner/repo`); the config's `watch` when none are named
	Pull { targets: Vec<String> },
	/// Sign in to the config's `home`, through the gate before it, in the browser
	Login,
	/// Serve this store as the board: the web client, its API, a GitHub App webhook
	Web(web::WebArgs),
	/// Set a field of a stored issue, made against the value you saw it hold: if it moved on since, nothing is written
	Set {
		#[arg(value_parser = |s: &str| tedi_task_operations::IssueLink::parse(s).ok_or("not an issue link"))]
		link: tedi_task_operations::IssueLink,
		/// `title`, `labels` / `assignees` (`, `-separated), `state` (` `, `x`, `-`) or `body`
		field: tedi_task_operations::conflict_resolve::Field,
		new: String,
		/// What the field held when you decided on `new`
		#[arg(long)]
		seen: String,
	},
	/// Move a stored issue or pull request to a bucket of a workflow: each bucket it enters runs its hooks, whose edits sync
	Move {
		#[arg(value_parser = |s: &str| tedi_task_operations::IssueLink::parse(s).ok_or("not an issue link"))]
		link: tedi_task_operations::IssueLink,
		/// `<workflow>/<bucket>[/<bucket>…]`, a leaf
		#[arg(value_parser = placement)]
		to: (String, tedi_core::workflow::State),
		/// Where you saw it sit, `<workflow>/<bucket>…`: if it left since, nothing moves. Without it, from wherever it sits
		#[arg(long, value_parser = placement)]
		seen: Option<(String, tedi_core::workflow::State)>,
	},
}

fn main() {
	// `tedi web` runs syncs on threads it doesn't spawn (dioxus' server-fn pool), and a sync's futures run deeper than std's 2MiB
	if std::env::var_os("RUST_MIN_STACK").is_none() {
		// SAFETY: first thing in the process: no other thread exists to read the environment
		unsafe { std::env::set_var("RUST_MIN_STACK", (8 << 20).to_string()) };
	}
	tokio::runtime::Builder::new_multi_thread().enable_all().build().expect("a runtime builds").block_on(run());
}
/// `<workflow>/<bucket>[/<bucket>…]`
fn placement(s: &str) -> Result<(String, tedi_core::workflow::State), String> {
	let (workflow, path) = s.split_once('/').ok_or_else(|| format!("{s:?}: a workflow, then its bucket path (`kanban/selected`)"))?;
	Ok((workflow.to_string(), path.parse().map_err(|e: tedi_core::workflow::WorkflowError| e.to_string())?))
}

async fn run() {
	v_utils::clientside!(extract_log_to());

	let cli = Cli::parse();
	let mode = match (cli.offline, cli.sync_on_exit) {
		(true, _) => SyncMode::Offline,
		(false, true) => SyncMode::OnExit,
		(false, false) => SyncMode::Full,
	};
	let settings = Arc::new(exit_on_error(config::LiveSettings::new(cli.settings_flags.clone(), Duration::from_secs(3))));

	// Commands that may need GitHub client (always construct it - offline only skips network calls)
	let has_github_commands = matches!(
		cli.command,
		Commands::Open(_) | Commands::Sprints(_) | Commands::Pull { .. } | Commands::View(_) | Commands::Set { .. } | Commands::Move { .. } | Commands::Web(_)
	);

	let github_client: Option<tedi_adapters::github::BoxedGithubClient> = if cli.mock.is_some() {
		Some(Arc::new(tedi_task_operations::mock_github::MockGithubClient::new("mock_user")))
	} else if has_github_commands {
		let config = exit_on_error(settings.config());
		let client = match exit_on_error(config.github()) {
			config::GithubAuth::Token(token) => tedi_adapters::github::RealGithubClient::new(token),
			config::GithubAuth::App(app) => exit_on_error(color_eyre::eyre::WrapErr::wrap_err(
				tedi_adapters::github::RealGithubClient::app(app.id, &app.private_key),
				"`github_app.private_key`",
			)),
		};
		let retrying = tedi_adapters::github::RetryingGithubClient::new(Arc::new(client), config.github_max_in_flight.unwrap_or(32));
		Some(Arc::new(retrying) as tedi_adapters::github::BoxedGithubClient)
	} else {
		None
	};

	let user_cache = v_utils::xdg_cache_file!("authenticated_user.txt");
	let cached_user = std::fs::read_to_string(&user_cache).ok().map(|u| u.trim().to_string()).filter(|u| !u.is_empty()); // unreadable reads as never fetched

	// Set global GitHub client and current user for sink operations
	if let Some(client) = &github_client {
		let fetch_user = match mode {
			SyncMode::Full => true,
			SyncMode::OnExit => cached_user.is_none(),
			SyncMode::Offline => cli.mock.is_some(),
		};
		if fetch_user && let Ok(user) = client.fetch_authenticated_user().await {
			tracing::info!("Authenticated as: {user}");
			let _ = std::fs::write(&user_cache, &user);
			tedi_task_operations::current_user::set(user);
		}
		tedi_adapters::github::client::set(client.clone());
	}
	if tedi_task_operations::current_user::get().is_none()
		&& let Some(user) = cached_user
	{
		tracing::info!("Loaded cached user: {user}");
		tedi_task_operations::current_user::set(user);
	}

	// a mock run's clock can be set, as the GitHub mock's state is
	let now = match (&cli.mock, std::env::var(concat!("tedi", "_MOCK_NOW"))) {
		(Some(_), Ok(at)) => exit_on_error(at.parse::<jiff::Timestamp>().map_err(color_eyre::Report::from)),
		_ => jiff::Timestamp::now(),
	};
	let states = || -> color_eyre::Result<config::States> { settings.config()?.states() };
	let workflows = || states()?.workflows();

	// All the functions here can rely on config being correct.
	exit_on_error(match cli.command {
		Commands::Manual(manual_args) => {
			let config = exit_on_error(settings.config());
			// Empty format string in config would silently produce empty dates, so treat it as unset.
			let date_format = config.manual_stats.as_ref().map(|m| m.date_format.as_str()).filter(|s| !s.is_empty()).unwrap_or("%Y-%m-%d");
			tedi_eval::manual_stats::update_or_open(date_format, manual_args).await
		}
		Commands::Sprints(sprints_command) => sprints::sprints_command(&settings, sprints_command, mode).await,
		Commands::Init(args) => {
			shell_init::output(&settings, args);
			Ok(())
		}
		Commands::Clockify(args) => tedi_adapters::clockify::main(exit_on_error(settings.config()).yes, args).await,
		Commands::PerfEval(args) => tedi_eval::perf_eval::main(args).await,
		Commands::Monitors(args) => tedi_eval::watch_monitors::main(args).await,
		Commands::Open(args) => tedi_task_operations::open_interactions::open_command(args, mode, cli.mock).await,
		Commands::View(args) => view::view_command(&settings, args, mode).await,
		Commands::Web(args) => web::web_command(&settings, args, mode).await,
		Commands::Set { link, field, new, seen } => {
			let link = exit_on_error(tedi_task_operations::reach::Reach::All.issue(link).map_err(color_eyre::Report::from));
			tedi_task_operations::board::edit(&link, vec![tedi_task_operations::conflict_resolve::FieldEdit { field, seen, new }], mode).await
		}
		Commands::Move { link, to: (workflow, to), seen } => move_command(exit_on_error(states()), link, (workflow, to), seen, mode, now).await,
		Commands::Login => match exit_on_error(states()) {
			config::States::Home(home) => match home.login(browse).await {
				Ok(()) => {
					println!("Signed in");
					Ok(())
				}
				Err(e) => Err(e.into()),
			},
			_ => Err(color_eyre::eyre::eyre!("`tedi login` signs in to the config's `home`, and none is set")),
		},
		Commands::Pull { targets } => match (targets.is_empty(), exit_on_error(settings.config()).watch) {
			(false, _) => tedi_task_operations::pull::pull_repos(&targets, &exit_on_error(workflows()), now).await,
			(true, Some(watch)) => tedi_task_operations::pull::pull_repos(&watch, &exit_on_error(workflows()), now).await,
			(true, None) => Err(color_eyre::eyre::eyre!("name what to pull, or list it under `watch` in the config")),
		},
	});
}

fn browse(url: &str) {
	println!("Opening {url}");
	let opener = if cfg!(target_os = "macos") { "open" } else { "xdg-open" };
	if let Err(e) = std::process::Command::new(opener).arg(url).spawn() {
		eprintln!("warning: {opener}: {e}. Open it yourself");
	}
}

async fn move_command(
	states: config::States,
	link: tedi_task_operations::IssueLink,
	(workflow, to): (String, tedi_core::workflow::State),
	seen: Option<(String, tedi_core::workflow::State)>,
	mode: SyncMode,
	now: jiff::Timestamp,
) -> color_eyre::Result<()> {
	use color_eyre::eyre::{bail, eyre};
	use tedi_core::conflict::Edited;
	use tedi_task_operations::{reach::Reach, workflow::Mover};

	let seen = match seen {
		Some((w, _)) if w != workflow => bail!("--seen names {w}, the move {workflow}"),
		Some((_, seen)) => Some(seen),
		None => None,
	};
	match states {
		config::States::Home(home) => match home.move_card(&link, &workflow, &to, seen.as_ref()).await? {
			Edited::Landed => tedi_task_operations::sprints::pull_stored(&[link]).await,
			Edited::Conflict(conflict) => Err(conflict.into()),
			Edited::Rejected(why) => bail!(why),
		},
		states => {
			let me = tedi_task_operations::current_user::get().ok_or_else(|| eyre!("who moves it is unknown: run once online, so tedi knows your GitHub user"))?;
			let (link, workflows) = (Reach::All.issue(link)?, states.workflows()?);
			tedi_task_operations::workflow::move_card(&link, &workflow, &to, seen.as_ref(), &Mover::Person(me), &workflows, mode, now).await
		}
	}
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
