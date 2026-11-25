use clap::Args;
use color_eyre::eyre::{Result, bail, eyre};
use jiff::Timestamp;
use tedi_adapters::github::GithubMilestone;
use tedi_core::{RepoInfo, Sprint, SprintError};
use tedi_task_operations::{
	TaskView,
	clockify_tracking::{HaltArgs, ResumeArgs},
	open_interactions::SyncMode,
	picked::Picking,
	search::SearchArgs,
};
use tedi_utils::search::SearchKind;
use v_utils::{Timeframe, TimeframeDesignator};

use crate::config::{LiveSettings, States};

#[derive(clap::Subcommand)]
pub enum SprintsCommands {
	Get {
		sprint: SprintRef,
	},
	/// Edit a sprint (a milestone, or the local urgent list) in your $EDITOR; a planning one opens with its Bottlenecks beside it
	Edit {
		sprint: SprintRef,
	},
	/// Ensures all milestones up to date, if yes - writes "OK" to $XDG_DATA_HOME/todo/healthcheck.status
	/// Can get outdated easily, so printed output of the command is prepended with the filename
	Healthcheck,
	/// Change the active sprint's selected issue (circular with --up/--down, tree descent
	/// with --in/--out, history with --prev/--next, else search over the whole reachable tree)
	Select {
		pattern: Option<String>,
		#[command(flatten)]
		search: SearchArgs,
		/// Forward in the selection history
		#[arg(long, short)]
		next: bool,
		/// Back in the selection history
		#[arg(long, short)]
		prev: bool,
		#[arg(long, short)]
		/// Go to issue above currently selected in the main Sprint
		up: bool,
		#[arg(long, short)]
		/// Go to issue below currently selected in the main Sprint
		down: bool,
		/// Descend into the selected milestone (its top workable issue) or issue (its top open child)
		#[arg(long = "in", short = 'i')]
		enter: bool,
		/// Ascend one level in the selection path
		#[arg(long, short)]
		out: bool,
		/// Reset to the top item of the sprint
		#[arg(long, short)]
		top: bool,
	},
	/// Operate on the selected issue in the active sprint
	Selected {
		#[command(subcommand)]
		op: SelectedOp,
	},
	/// Build a task view of the open local issues matching a query, best first
	Search {
		query: String,
		#[command(flatten)]
		search: SearchArgs,
	},
}
#[derive(clap::Subcommand)]
pub enum SelectedOp {
	/// Open the selected issue file in $EDITOR
	Open,
	/// Print the sprint's strip of musts and their completion, then the selected issue's blockers
	List {
		/// Emit Pango markup (for eww labels) instead of ANSI color.
		#[arg(long)]
		markup: bool,
	},
	/// Compactly show the current blocker
	Current {
		/// Emit Pango markup (for eww labels), suffixed with a Clockify-tracking dot:
		/// blue active, white paused, red tracking-elsewhere.
		#[arg(long)]
		markup: bool,
	},
	/// Append a blocker to the selected issue
	Add {
		text: String,
		/// Nest as a child of the current deepest blocker
		#[arg(short = 'n', long)]
		nest: bool,
	},
	/// Pop the current blocker (repeat `-p` to also pop ancestors)
	Pop {
		#[arg(short = 'p', long = "parent", action = clap::ArgAction::Count)]
		parents: u8,
	},
	/// Replace the current blocker in-place
	Set { text: String },
	/// Start a Clockify timer on the selected issue
	Resume(ResumeArgs),
	/// Stop the Clockify timer
	Halt(HaltArgs),
}
#[derive(Args)]
pub struct SprintsArgs {
	#[command(subcommand)]
	command: SprintsCommands,
}
/// A planning sprint, Bottlenecks, or the local urgent list.
#[derive(Clone)]
pub enum SprintRef {
	Urgent,
	Milestone(Sprint),
}
impl std::str::FromStr for SprintRef {
	type Err = SprintError;

	fn from_str(s: &str) -> Result<Self, SprintError> {
		match s {
			"urgent" => Ok(Self::Urgent),
			_ => s.parse().map(Self::Milestone),
		}
	}
}

pub static HEALTHCHECK_REL_PATH: &str = "healthcheck.status";
pub static SPRINT_HEADER_REL_PATH: &str = "sprint_header.md";

pub async fn sprints_command(settings: &LiveSettings, args: SprintsArgs, mode: SyncMode) -> Result<()> {
	use tedi_task_operations::{selection::Selected, sprints as ops, tabs};

	let yes = || settings.config().map(|c| c.yes).unwrap_or(false);
	match args.command {
		SprintsCommands::Get { sprint } => {
			let mut view = match sprint {
				SprintRef::Urgent => match std::fs::read_to_string(tedi_task_operations::selection::urgent_path()) {
					Ok(raw) => TaskView::parse(&raw),
					Err(e) if e.kind() == std::io::ErrorKind::NotFound => bail!("No urgent sprint. Create one with `sprints edit urgent`."),
					Err(e) => return Err(e.into()),
				},
				SprintRef::Milestone(sprint) => {
					let milestone = ops::fetch_sprint(milestone_repo(settings)?, sprint, mode).await?;
					if let Sprint::Planning(_) = sprint {
						not_outdated(sprint, milestone.identity.due_on)?;
					}
					milestone.body.0
				}
			};
			view.resolve_bare_refs(None);
			view.remove_issue_links(&tedi_task_operations::selection::closed_links(&view)); // as an edit would open it
			// read-only print: not worth a round-trip per referenced issue
			println!("{}", ops::expand_and_refresh(view, false).await?.text);
			Ok(())
		}
		SprintsCommands::Edit { sprint: SprintRef::Urgent } => {
			let picking = picking(settings)?;
			let picked = match &picking {
				Some(picking) => Some((picking, milestone_repo(settings)?)),
				None => None,
			};
			Box::pin(tabs::edit_urgent(picked, mode)).await
		}
		SprintsCommands::Edit {
			sprint: SprintRef::Milestone(sprint),
		} => {
			let link = Box::pin(tabs::edit_sprint(
				milestone_repo(settings)?,
				sprint,
				picking(settings)?.as_ref(),
				mode,
				tedi_eval::same_day_buffer(),
			))
			.await?; // on the stack, it overflows the main thread's unoptimized
			if sprint == blocker_sprint(settings)? {
				Selected::focus(link)?;
			}
			// The edit is already saved above; the trailing healthcheck is informational. It hard-requires the
			// `2w` sprint, so never let its absence abort a completed edit and make it read as discarded.
			if mode != SyncMode::Offline
				&& let Err(e) = healthcheck(settings).await
			{
				tracing::warn!("post-edit healthcheck failed (your edit was already saved): {e}");
				eprintln!("warning: post-edit healthcheck failed (your edit was already saved): {e}");
			}
			Ok(())
		}
		SprintsCommands::Healthcheck => healthcheck(settings).await,
		SprintsCommands::Select {
			pattern,
			search,
			next,
			prev,
			up,
			down,
			enter,
			out,
			top,
		} => {
			// A bare sprint (`sprints select 1d`) focuses it; anything else is an issue pattern
			// within the currently-active sprint.
			if mode != SyncMode::Offline
				&& !next && !prev
				&& !up && !down
				&& !enter && !out
				&& !top && let Some(sprint) = pattern.as_deref().and_then(|p| p.parse::<Sprint>().ok())
			{
				Selected::focus(ops::fetch_sprint(milestone_repo(settings)?, sprint, mode).await?.link().clone())?;
				ops::selected_show(yes()).await
			} else {
				ensure_selection(settings, mode).await?;
				if mode != SyncMode::Offline
					&& !next && !prev
					&& !up && !down && !enter
					&& !out && !top && let Some(picking) = picking(settings)?
					&& let Err(e) = Box::pin(picking.refresh(milestone_repo(settings)?)).await
				{
					eprintln!("warning: selected.md is as last fetched: {e:#}");
				}
				ops::select(pattern, SearchKind::try_from(search)?, next, prev, up, down, enter, out, top, yes()).await
			}
		}
		SprintsCommands::Selected { op } => {
			ensure_selection(settings, mode).await?;
			match op {
				SelectedOp::Open => ops::selected_open(mode, yes()).await,
				SelectedOp::List { markup } => ops::selected_list(markup),
				SelectedOp::Current { markup } => ops::selected_current(markup),
				SelectedOp::Add { text, nest } => ops::selected_add(text, nest, mode, yes()).await,
				SelectedOp::Pop { parents } => ops::selected_pop(parents as usize, mode, yes()).await,
				SelectedOp::Set { text } => ops::selected_set(text, mode, yes()).await,
				SelectedOp::Resume(resume_args) => ops::selected_resume(resume_args, yes()).await,
				SelectedOp::Halt(halt_args) => ops::selected_halt(halt_args).await,
			}
		}
		SprintsCommands::Search { query, search } => ops::search(&query, SearchKind::try_from(search)?).await,
	}
}

/// Ensure an active sprint is resolvable for selection. Urgent (a local file) needs nothing; otherwise, when
/// cold and online, fetch the lowest sprint (`blocker_tf`). When online, also fetch any milestone in the
/// active view not stored yet. Only what is missing locally is fetched, so `--sync-on-exit` reaches it too.
async fn ensure_selection(settings: &LiveSettings, mode: SyncMode) -> Result<()> {
	use tedi_task_operations::{selection::Selected, sprints as ops};

	if mode == SyncMode::Offline {
		return Ok(());
	}
	let missing = Selected::uncached_active_milestones();
	if !missing.is_empty() {
		ops::refresh_milestones(&missing).await;
	}
	if !Selected::active_ready() {
		Selected::focus(ops::fetch_sprint(milestone_repo(settings)?, blocker_sprint(settings)?, mode).await?.link().clone())?;
	}
	Ok(())
}

fn picking(settings: &LiveSettings) -> Result<Option<Picking>> {
	let config = settings.config()?;
	let Some(selected) = config.milestones.as_ref().and_then(|m| m.selected.as_deref()) else {
		return Ok(None);
	};
	let States::Home(home) = config.states()? else {
		bail!("`milestones.selected` is the `home`'s bucket, and no `home` is set");
	};
	let (workflow, bucket) = crate::placement(selected).map_err(|e| eyre!("`milestones.selected`: {e}"))?;
	Ok(Some(Picking { home, workflow, bucket }))
}

/// The sprint blockers are sourced from.
fn blocker_sprint(settings: &LiveSettings) -> Result<Sprint> {
	let config = settings.config()?;
	Ok(config.milestones.as_ref().map(|m| m.blocker_tf()).unwrap_or("1d").parse()?)
}

async fn request_milestones(settings: &LiveSettings) -> Result<Vec<GithubMilestone>> {
	let client = tedi_adapters::github::client::get()?;
	Ok(client.list_milestones(milestone_repo(settings)?).await?)
}

/// Parse owner and repo from various Github URL formats
/// Supports: `owner/repo`, `https://github.com/owner/repo`, `git@github.com:owner/repo.git`, etc.
fn parse_github_repo(url: &str) -> Result<(String, String)> {
	let url = url.trim();

	// Handle "owner/repo" format directly
	if !url.contains(':') && !url.contains("//") {
		let parts: Vec<&str> = url.split('/').collect();
		if parts.len() == 2 {
			return Ok((parts[0].to_string(), parts[1].trim_end_matches(".git").to_string()));
		}
	}

	// Handle URL formats
	let sections: Vec<&str> = url.split('/').collect();
	if sections.len() >= 2 {
		let owner = sections[sections.len() - 2];
		let repo = sections[sections.len() - 1].trim_end_matches(".git");

		// For SSH format (git@github.com:owner/repo), owner might contain ':'
		let owner = if owner.contains(':') { owner.split(':').next_back().unwrap_or(owner) } else { owner };

		return Ok((owner.to_string(), repo.to_string()));
	}

	Err(eyre!("Could not parse Github repo from URL: {url}"))
}

#[derive(Clone, Debug, PartialEq, thiserror::Error)]
#[error("Error on `{sprint}` milestone: {source}")]
struct GetMilestoneError {
	sprint: Sprint,
	#[source]
	source: MilestoneError,
}

#[derive(Clone, Debug, PartialEq, thiserror::Error)]
enum MilestoneError {
	#[error("Milestone is missing due_on date")]
	MissingDueOn,

	#[error("Milestone is outdated (due_on: {due_on}). Try moving it to a later date.")]
	MilestoneOutdated { due_on: Timestamp },

	#[error("Milestone not found. Here are all the existing milestones:\n{existing_milestones:?}")]
	MilestoneNotFound { existing_milestones: Vec<String> },

	#[error("Missing description")]
	MissingDescription,
}

fn not_outdated(sprint: Sprint, due_on: Option<Timestamp>) -> Result<(), GetMilestoneError> {
	let source = match due_on {
		None => MilestoneError::MissingDueOn,
		Some(due_on) if due_on + tedi_eval::same_day_buffer() < Timestamp::now() => MilestoneError::MilestoneOutdated { due_on },
		Some(_) => return Ok(()),
	};
	Err(GetMilestoneError { sprint, source })
}

fn get_milestone(sprint: Sprint, retrieved_milestones: &[GithubMilestone]) -> Result<String, GetMilestoneError> {
	let error = |source| GetMilestoneError { sprint, source };
	let milestone = retrieved_milestones.iter().find(|m| Sprint::titled(&m.title) == Some(sprint)).ok_or_else(|| {
		error(MilestoneError::MilestoneNotFound {
			existing_milestones: retrieved_milestones.iter().map(|m| m.title.clone()).collect(),
		})
	})?;
	not_outdated(sprint, milestone.due_on)?;
	milestone.description.clone().ok_or_else(|| error(MilestoneError::MissingDescription))
}

static KEY_MILESTONES: [Timeframe; 6] = [
	Timeframe::from_naive(1, TimeframeDesignator::Days),
	Timeframe::from_naive(2, TimeframeDesignator::Weeks),
	Timeframe::from_naive(1, TimeframeDesignator::IfcQuarters),
	Timeframe::from_naive(1, TimeframeDesignator::Years),
	Timeframe::from_naive(3, TimeframeDesignator::Years),
	Timeframe::from_naive(7, TimeframeDesignator::Years),
];

async fn healthcheck(settings: &LiveSettings) -> Result<()> {
	use std::fs;

	let healthcheck_path = v_utils::xdg_data_file!(HEALTHCHECK_REL_PATH);

	let retrieved_milestones = request_milestones(settings).await?;
	let results = KEY_MILESTONES
		.iter()
		.map(|tf| get_milestone(Sprint::Planning(*tf), &retrieved_milestones))
		.collect::<Vec<Result<String, GetMilestoneError>>>();
	{
		let mut health = String::new();
		for result in &results {
			match result {
				Ok(_) => {}
				Err(e) => {
					if !health.is_empty() {
						health.push('\n');
					}
					health.push_str(&e.to_string());
				}
			}
		}

		if health.is_empty() {
			health = "OK".to_string();
		}
		println!("{}\n{health}", healthcheck_path.display());

		fs::write(healthcheck_path, health).unwrap();
	}

	{
		let sprint_ms =
			&<std::result::Result<std::string::String, GetMilestoneError> as Clone>::clone(&results[1]).map_err(|e| eyre!("Couldn't parse 2w milestone which MUST be defined: {e}"))?;
		let sprint_header = sprint_ms
			.lines()
			.next()
			.ok_or_else(|| eyre!("2w milestone does not have a description. MUST have a description."))?;
		if !sprint_header.starts_with("# ") {
			eprintln!("2w milestone description does not start with a header. It SHOULD start with '# '.");
		}
		fs::write(v_utils::xdg_data_file!(SPRINT_HEADER_REL_PATH), sprint_header).unwrap();
	}

	// blocker commands read the sprint from the store
	let blockers = blocker_sprint(settings)?;
	if retrieved_milestones.iter().any(|m| Sprint::titled(&m.title) == Some(blockers)) {
		let sprint = tedi_task_operations::sprints::fetch_sprint(milestone_repo(settings)?, blockers, SyncMode::Full).await?;
		tedi_task_operations::selection::Selected::focus(sprint.link().clone())?;
	}

	Ok(())
}

/// The milestones repository from config.
pub(crate) fn milestone_repo(settings: &LiveSettings) -> Result<RepoInfo> {
	let config = settings.config()?;
	let milestones = config
		.milestones
		.as_ref()
		.ok_or_else(|| eyre!("milestones config section is required. Add [milestones] section with url to your config"))?;
	let (owner, repo) = parse_github_repo(&milestones.url)?;
	Ok(RepoInfo::new(&owner, &repo))
}
