use clap::Args;
use jiff::Timestamp;
use tedi_adapters::github::GithubMilestone;
use tedi_core::RepoInfo;
use tedi_ops::{
	IssueLink, MilestoneLink, TaskView,
	clockify_tracking::{HaltArgs, ResumeArgs},
	sprints::{expand_and_refresh, materialize_new_tasks, sync_blocker_changes, sync_milestone_changes},
};
use v_utils::prelude::*;

use crate::config::LiveSettings;

#[derive(clap::Subcommand)]
pub enum SprintsCommands {
	Get {
		sprint: SprintRef,
	},
	/// Edit a sprint (milestone description, or the local urgent list) in your $EDITOR
	Edit {
		sprint: SprintRef,
		#[arg(long)]
		offline: bool,
	},
	/// Ensures all milestones up to date, if yes - writes "OK" to $XDG_DATA_HOME/todo/healthcheck.status
	/// Can get outdated easily, so printed output of the command is prepended with the filename
	Healthcheck,
	/// Change the active sprint's selected issue (circular with --next/--prev, tree descent
	/// with --down/--up, else fzf/pattern over the whole reachable tree)
	Select {
		pattern: Option<String>,
		#[arg(long)]
		next: bool,
		#[arg(long)]
		prev: bool,
		/// Descend into the selected milestone (its top workable issue) or issue (its top open child)
		#[arg(long)]
		down: bool,
		/// Ascend one level in the selection path
		#[arg(long)]
		up: bool,
	},
	/// Operate on the selected issue in the active sprint
	Selected {
		#[command(subcommand)]
		op: SelectedOp,
	},
	/// Build a task view of local issues matching a label or text
	Search {
		query: String,
	},
}
#[derive(clap::Subcommand)]
pub enum SelectedOp {
	/// Open the selected issue file in $EDITOR
	Open,
	/// List the selected issue's blockers
	List,
	/// Compactly show the current blocker
	Current,
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
/// A sprint designator: a milestone timeframe, or the local urgent list.
#[derive(Clone)]
pub enum SprintRef {
	Urgent,
	Tf(Timeframe),
}
impl std::str::FromStr for SprintRef {
	type Err = Report;

	fn from_str(s: &str) -> Result<Self> {
		match s {
			"urgent" => Ok(Self::Urgent),
			_ => s.parse().map(Self::Tf),
		}
	}
}

pub static HEALTHCHECK_REL_PATH: &str = "healthcheck.status";
pub static SPRINT_HEADER_REL_PATH: &str = "sprint_header.md";

pub async fn sprints_command(settings: &LiveSettings, args: SprintsArgs, mock: Option<tedi_ops::MockType>, offline: bool) -> Result<()> {
	use tedi_ops::sprints as ops;

	let yes = || settings.config().map(|c| c.yes).unwrap_or(false);
	match args.command {
		SprintsCommands::Get { sprint } => {
			let raw = match sprint {
				SprintRef::Urgent => match std::fs::read_to_string(tedi_ops::local::urgent_path()) {
					Ok(c) => c,
					Err(e) if e.kind() == std::io::ErrorKind::NotFound => bail!("No urgent sprint. Create one with `sprints edit urgent`."),
					Err(e) => return Err(e.into()),
				},
				SprintRef::Tf(tf) => {
					let retrieved_milestones = request_milestones(settings).await?;
					let raw = get_milestone(tf, &retrieved_milestones)?;
					refresh_sprint_milestones(&raw).await;
					raw
				}
			};
			let expanded = expand_and_refresh(&raw).await?;
			println!("{expanded}");
			Ok(())
		}
		SprintsCommands::Edit { sprint, offline } => match sprint {
			SprintRef::Urgent => edit_urgent(offline).await,
			SprintRef::Tf(tf) => edit_milestone(settings, tf, offline, mock.is_some()).await,
		},
		SprintsCommands::Healthcheck => healthcheck(settings).await,
		SprintsCommands::Select { pattern, next, prev, down, up } => {
			// A bare timeframe (`sprints select 1d`) focuses that sprint precision; anything else
			// is an issue pattern within the currently-active sprint. The alpha-designator guard
			// keeps numeric issue patterns (`select 2`) from parsing as a timeframe.
			if !offline
				&& !next && !prev
				&& !down && !up
				&& let Some(p) = pattern.as_deref()
				&& p.chars().any(|c| c.is_ascii_alphabetic())
				&& let Ok(tf) = p.parse::<Timeframe>()
			{
				focus_sprint(settings, tf, yes()).await
			} else {
				ensure_selection_cache(settings, offline).await?;
				ops::select(pattern, next, prev, down, up, yes()).await
			}
		}
		SprintsCommands::Selected { op } => {
			ensure_selection_cache(settings, offline).await?;
			match op {
				SelectedOp::Open => ops::selected_open(offline, yes()).await,
				SelectedOp::List => ops::selected_list(),
				SelectedOp::Current => ops::selected_current(),
				SelectedOp::Add { text, nest } => ops::selected_add(text, nest, offline, yes()).await,
				SelectedOp::Pop { parents } => ops::selected_pop(parents as usize, offline, yes()).await,
				SelectedOp::Set { text } => ops::selected_set(text, offline, yes()).await,
				SelectedOp::Resume(resume_args) => ops::selected_resume(resume_args, yes()).await,
				SelectedOp::Halt(halt_args) => ops::selected_halt(halt_args).await,
			}
		}
		SprintsCommands::Search { query } => ops::search(&query).await,
	}
}

/// Refresh the milestone-content cache for every milestone ref in `content` (best-effort:
/// select ops serve stale cache; freshness is bounded by the fetching commands).
async fn refresh_sprint_milestones(content: &str) {
	let links = TaskView::parse(content).milestone_links();
	if links.is_empty() {
		return;
	}
	if let Err(e) = tedi_ops::sprints::refresh_milestone_cache(&links).await {
		tracing::warn!("failed to refresh milestone cache: {e}");
		eprintln!("warning: failed to refresh milestone cache: {e}");
	}
}

/// Ensure an active sprint is resolvable for selection. Urgent (a local file) needs nothing;
/// otherwise, when cold and online, fetch the lowest normal sprint (`blocker_tf`) into the cache.
/// When online, also fetch any milestone in the active view missing from the milestone cache.
async fn ensure_selection_cache(settings: &LiveSettings, offline: bool) -> Result<()> {
	if !offline {
		let missing = tedi_ops::local::Selected::uncached_active_milestones();
		if !missing.is_empty()
			&& let Err(e) = tedi_ops::sprints::refresh_milestone_cache(&missing).await
		{
			tracing::warn!("failed to refresh milestone cache: {e}");
			eprintln!("warning: failed to refresh milestone cache: {e}");
		}
	}
	if offline || tedi_ops::local::Selected::active_ready() {
		return Ok(());
	}
	let config = settings.config()?;
	let blocker_tf = config.milestones.as_ref().map(|m| m.blocker_tf()).unwrap_or("1d").to_string();
	let milestones = request_milestones(settings).await?;
	let desc = milestones
		.iter()
		.find(|m| m.title == blocker_tf)
		.and_then(|m| m.description.clone())
		.ok_or_else(|| eyre!("No `{blocker_tf}` sprint on GitHub yet. Add issues to the '{blocker_tf}' milestone (or create an urgent list)."))?;
	tedi_ops::sprints::refresh_selection_cache(&blocker_tf, &desc);
	refresh_sprint_milestones(&desc).await;
	Ok(())
}

/// `sprints select <tf>` — focus a sprint precision: cache its content and show its selection
/// (persisted, or auto-set to the top open issue).
async fn focus_sprint(settings: &LiveSettings, tf: Timeframe, yes: bool) -> Result<()> {
	let milestones = request_milestones(settings).await?;
	let desc = milestones
		.iter()
		.find(|m| m.title == tf.to_string())
		.and_then(|m| m.description.clone())
		.ok_or_else(|| eyre!("Sprint '{tf}' not found on GitHub (or it has no description)."))?;
	tedi_ops::sprints::refresh_selection_cache(&tf.to_string(), &desc);
	refresh_sprint_milestones(&desc).await;
	tedi_ops::sprints::selected_show(yes).await
}
async fn request_milestones(settings: &LiveSettings) -> Result<Vec<GithubMilestone>> {
	let config = settings.config()?;
	let milestones_config = config
		.milestones
		.as_ref()
		.ok_or_else(|| eyre!("milestones config section is required. Add [milestones] section with url to your config"))?;
	let (owner, repo) = parse_github_repo(&milestones_config.url)?;
	let client = tedi_adapters::github::client::get()?;
	Ok(client.list_milestones(RepoInfo::new(&owner, &repo)).await?)
}

/// Parse owner and repo from various Github URL formats
/// Supports: "owner/repo", "https://github.com/owner/repo", "git@github.com:owner/repo.git", etc.
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

#[derive(Clone, Debug, thiserror::Error, PartialEq)]
#[error("Error on `{requested_tf}` milestone: {source}")]
struct GetMilestoneError {
	requested_tf: Timeframe,
	#[source]
	source: MilestoneError,
}

#[derive(Clone, Debug, thiserror::Error, PartialEq)]
enum MilestoneError {
	#[error("Milestone is missing due_on date")]
	MissingDueOn,

	#[error("Milestone is outdated (due_on: {due_on}). Try moving it to a later date.")]
	MilestoneOutdated { due_on: Timestamp },

	#[error("Requested milestone on minute-designated timeframe (`m`). You likely meant to request Monthly (`M`).")]
	MinuteMilestone,

	#[error("Milestone not found. Here are all the existing milestones:\n{existing_milestones:?}")]
	MilestoneNotFound { existing_milestones: Vec<String> },

	#[error("Missing description")]
	MissingDescription,
}

fn get_milestone(tf: Timeframe, retrieved_milestones: &[GithubMilestone]) -> Result<String, GetMilestoneError> {
	if tf.designator() == TimeframeDesignator::Minutes {
		return Err(GetMilestoneError {
			requested_tf: tf,
			source: MilestoneError::MinuteMilestone,
		});
	}

	match retrieved_milestones.iter().find(|m| m.title == tf.to_string()) {
		Some(milestone) => {
			let due_on = milestone.due_on.as_ref().ok_or(GetMilestoneError {
				requested_tf: tf,
				source: MilestoneError::MissingDueOn,
			})?;

			if *due_on + tedi_eval::same_day_buffer() < Timestamp::now() {
				return Err(GetMilestoneError {
					requested_tf: tf,
					source: MilestoneError::MilestoneOutdated { due_on: *due_on },
				});
			}

			match milestone.description.clone() {
				Some(description) => Ok(description),
				None => Err(GetMilestoneError {
					requested_tf: tf,
					source: MilestoneError::MissingDescription,
				}),
			}
		}
		None => {
			let milestone_titles = retrieved_milestones.iter().map(|m| m.title.clone()).collect::<Vec<String>>();
			Err(GetMilestoneError {
				requested_tf: tf,
				source: MilestoneError::MilestoneNotFound {
					existing_milestones: milestone_titles,
				},
			})
		}
	}
}

static KEY_MILESTONES: [Timeframe; 6] = [
	Timeframe::from_naive(1, TimeframeDesignator::Days),
	Timeframe::from_naive(2, TimeframeDesignator::Weeks),
	Timeframe::from_naive(1, TimeframeDesignator::Quarters),
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
		.map(|tf| get_milestone(*tf, &retrieved_milestones))
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

	// Cache the blocker timeframe milestone for blocker commands
	cache_blocker_milestone(settings, &retrieved_milestones).await;

	Ok(())
}

/// Cache the blocker timeframe milestone description for synchronous blocker commands,
/// plus the contents of any milestone it references.
async fn cache_blocker_milestone(settings: &LiveSettings, milestones: &[GithubMilestone]) {
	let config = match settings.config() {
		Ok(c) => c,
		Err(_) => return,
	};
	let blocker_tf = config.milestones.as_ref().map(|m| m.blocker_tf()).unwrap_or("1d");

	if let Some(ms) = milestones.iter().find(|m| m.title == blocker_tf)
		&& let Some(ref desc) = ms.description
	{
		tedi_ops::sprints::refresh_selection_cache(blocker_tf, desc);
		refresh_sprint_milestones(desc).await;
	}
}

/// Edit the local urgent list (creating it if absent). Same expand/sync/collapse cycle as
/// milestones, but the storage is `urgent_path()` — no GitHub involved.
async fn edit_urgent(offline: bool) -> Result<()> {
	use std::fs;

	let _lock = tedi_ops::local::try_lock_urgent()?.ok_or_else(|| eyre!("Another instance is already editing the urgent sprint. Only one editor at a time is allowed."))?;

	let path = tedi_ops::local::urgent_path();
	let original = match fs::read_to_string(&path) {
		Ok(c) => c,
		Err(e) if e.kind() == std::io::ErrorKind::NotFound => String::new(),
		Err(e) => return Err(e.into()),
	};
	let expanded = expand_and_refresh(&original).await?;

	let tmp_path = tempfile::tempdir()?.keep().join("milestone_urgent.md");
	fs::write(&tmp_path, &expanded)?;
	eprintln!("[milestone] tmp_path: {}", tmp_path.display());

	tedi_ops::utils::open_file(&tmp_path, None).await?;

	let edited_content = fs::read_to_string(&tmp_path)?;
	if edited_content == expanded {
		println!("No changes made to urgent sprint");
		return Ok(());
	}

	let mut edited_doc = TaskView::parse(&edited_content);
	// urgent is local-only: milestone refs live on GitHub and have no meaning here
	let milestone_links = edited_doc.milestone_links();
	if !milestone_links.is_empty() {
		tedi_ops::utils::persist_rejected_changes(&edited_content);
		eprintln!("Your changes were saved to /tmp/tedi/rejected-changes.md — you can recover them from there.");
		bail!(
			"urgent sprint cannot contain milestone refs: {}",
			milestone_links.iter().map(|l| l.as_str()).collect::<Vec<_>>().join(", ")
		);
	}

	if let Err(e) = sync_blocker_changes(&edited_content, offline).await {
		tedi_ops::utils::persist_rejected_changes(&edited_content);
		eprintln!("Your changes were saved to /tmp/tedi/rejected-changes.md — you can recover them from there.");
		return Err(e);
	}

	if let Err(e) = materialize_new_tasks(&mut edited_doc, None, offline).await {
		tedi_ops::utils::persist_rejected_changes(&edited_content);
		eprintln!("Your changes were saved to /tmp/tedi/rejected-changes.md — you can recover them from there.");
		return Err(e);
	}

	edited_doc.collapse_to_links();
	fs::create_dir_all(path.parent().expect("urgent_path is always nested under data dir"))?;
	fs::write(&path, edited_doc.serialize())?;
	println!("Updated urgent sprint");
	Ok(())
}

async fn edit_milestone(settings: &LiveSettings, tf: Timeframe, offline: bool, mock: bool) -> Result<()> {
	use std::fs;

	let lock_path = v_utils::xdg_cache_file!(format!("milestones_{tf}.lock"));
	let lock_file = fs::File::create(&lock_path)?;
	match lock_file.try_lock() {
		Ok(()) => {}
		Err(std::fs::TryLockError::WouldBlock) => bail!("Another instance is already editing milestone '{tf}'. Only one editor at a time is allowed."),
		Err(std::fs::TryLockError::Error(e)) => return Err(e.into()),
	}

	let (original_description, milestone_number, is_outdated) = if mock {
		// In mock mode, read milestone content from env-specified file
		let mock_milestone_path =
			std::env::var(concat!(env!("CARGO_PKG_NAME"), "_MOCK_MILESTONE")).map_err(|_| eyre!("mock mode requires {}_MOCK_MILESTONE env var", env!("CARGO_PKG_NAME")))?;
		let content = fs::read_to_string(&mock_milestone_path)?;
		(content, 0, false)
	} else {
		let retrieved_milestones = request_milestones(settings).await?;
		let milestone = retrieved_milestones.iter().find(|m| m.title == tf.to_string()).ok_or_else(|| {
			let existing = retrieved_milestones.iter().map(|m| m.title.clone()).collect::<Vec<_>>();
			eyre!("Milestone '{tf}' not found. Existing milestones: {existing:?}")
		})?;
		let desc = milestone.description.clone().unwrap_or_default();
		let num = milestone.number;
		let outdated = milestone.due_on.map(|d| d + tedi_eval::same_day_buffer() < Timestamp::now()).unwrap_or(true);
		(desc, num, outdated)
	};

	// Refresh milestone contents so their refs expand inline, then expand for editing
	if !mock {
		refresh_sprint_milestones(&original_description).await;
	}
	let expanded_description = expand_and_refresh(&original_description).await?;

	// Write to temp file
	// Use into_path() to prevent TempDir from auto-deleting on drop (survives panics/errors)
	let tmp_path = tempfile::tempdir()?.keep().join(format!("milestone_{tf}.md"));
	fs::write(&tmp_path, &expanded_description)?;
	eprintln!("[milestone] tmp_path: {}", tmp_path.display());

	// Open in editor
	tedi_ops::utils::open_file(&tmp_path, None).await?;

	// Read back
	let edited_content = fs::read_to_string(&tmp_path)?;

	// Check if changed (compare against expanded, not original — expansion itself is not a user edit)
	if edited_content == expanded_description {
		println!("No changes made to milestone '{tf}'");
		return Ok(());
	}

	// Sync blocker changes back to individual issue files
	if let Err(e) = sync_blocker_changes(&edited_content, offline).await {
		tedi_ops::utils::persist_rejected_changes(&edited_content);
		eprintln!("Your changes were saved to /tmp/tedi/rejected-changes.md — you can recover them from there.");
		return Err(e);
	}

	// The edited doc is this milestone's body: its top-level new tasks belong to it, so they
	// materialize as real upstream issues (never virtuals) hosted in the milestones repo. Absent
	// a milestones config (only reachable in mock), ambient is unset and tasks fall back to virtual.
	let ambient = milestone_repo(settings)
		.ok()
		.and_then(|(owner, repo)| MilestoneLink::parse(&format!("https://github.com/{owner}/{repo}/milestone/{milestone_number}")));

	let mut edited_doc = TaskView::parse(&edited_content);
	if let Err(e) = materialize_new_tasks(&mut edited_doc, ambient, offline).await {
		tedi_ops::utils::persist_rejected_changes(&edited_content);
		eprintln!("Your changes were saved to /tmp/tedi/rejected-changes.md — you can recover them from there.");
		return Err(e);
	}

	// Persist edits made inside inlined milestone blocks (incl. materialized tasks) to their own
	// nodes. Mock mode forces local-only (never reaches the network).
	if let Err(e) = sync_milestone_changes(&edited_doc, offline || mock).await {
		tedi_ops::utils::persist_rejected_changes(&edited_content);
		eprintln!("Your changes were saved to /tmp/tedi/rejected-changes.md — you can recover them from there.");
		return Err(e);
	}

	// Collapse expanded issues back to bare links for storage
	edited_doc.collapse_to_links();
	let new_description = edited_doc.serialize();

	// Sync milestone assignments on GitHub: assign new issues, unassign removed ones
	if !mock && !offline {
		let mut orig_doc = TaskView::parse(&original_description);
		orig_doc.resolve_bare_refs();
		let old_links = orig_doc.issue_links();
		let new_doc = TaskView::parse(&new_description);
		let new_links = new_doc.issue_links();
		sync_milestone_assignments(settings, milestone_number, &old_links, &new_links).await?;
	}

	if mock {
		// In mock mode, write result back to the milestone file
		let mock_milestone_path = std::env::var(concat!(env!("CARGO_PKG_NAME"), "_MOCK_MILESTONE")).unwrap();
		fs::write(&mock_milestone_path, &new_description)?;
		println!("Updated milestone '{tf}'");
		return Ok(());
	}

	// Update the blocker cache if this is the blocker timeframe milestone
	let config = settings.config()?;
	let blocker_tf = config.milestones.as_ref().map(|m| m.blocker_tf()).unwrap_or("1d");
	if tf.to_string() == blocker_tf {
		tedi_ops::sprints::refresh_selection_cache(blocker_tf, &new_description);
	}

	// If outdated, archive old contents and update date
	if is_outdated {
		let new_date = Timestamp::now() + tf.signed_duration();
		println!("Milestone was outdated, updating due date to {}", new_date.strftime("%Y-%m-%d"));

		// Archive title: "2025/12/17_1d"
		let archive_title = format!("{}_{tf}", Timestamp::now().strftime("%Y/%m/%d"));

		// Run both API calls in parallel: update current milestone and create archived one
		let (update_result, archive_result) = tokio::join!(
			update_milestone(settings, milestone_number, &new_description, Some(new_date)),
			create_closed_milestone(settings, &archive_title, &original_description)
		);

		update_result?;
		if let Err(e) = archive_result {
			tracing::warn!("Failed to archive old milestone contents: {e}");
		}
	} else {
		// Not outdated, just update description
		update_milestone(settings, milestone_number, &new_description, None).await?;
	}

	println!("Updated milestone '{tf}'");

	// Run healthcheck after update
	healthcheck(settings).await?;

	Ok(())
}

async fn update_milestone(settings: &LiveSettings, milestone_number: u64, description: &str, due_on: Option<Timestamp>) -> Result<()> {
	let (owner, repo) = milestone_repo(settings)?;
	let client = tedi_adapters::github::client::get()?;
	client.update_milestone(RepoInfo::new(&owner, &repo), milestone_number, description, due_on).await?;
	Ok(())
}

/// Creates a new milestone with the given title, description, and immediately closes it
async fn create_closed_milestone(settings: &LiveSettings, title: &str, description: &str) -> Result<()> {
	let (owner, repo) = milestone_repo(settings)?;
	let client = tedi_adapters::github::client::get()?;
	client.create_milestone(RepoInfo::new(&owner, &repo), title, description, true).await?;
	Ok(())
}

/// Resolve the (owner, repo) of the milestones repository from config.
fn milestone_repo(settings: &LiveSettings) -> Result<(String, String)> {
	let config = settings.config()?;
	let milestones_config = config.milestones.as_ref().ok_or_else(|| eyre!("milestones config section is required"))?;
	parse_github_repo(&milestones_config.url)
}

/// Sync milestone assignments on GitHub.
///
/// Compares issue links in old vs new description, then:
/// - Assigns newly added issues to the milestone
/// - Unassigns removed issues from the milestone
/// Only operates on issues in the same repo as the milestone.
async fn sync_milestone_assignments(settings: &LiveSettings, milestone_number: u64, old_links: &[IssueLink], new_links: &[IssueLink]) -> Result<()> {
	use std::collections::HashSet;

	let config = settings.config()?;
	let milestones_config = config.milestones.as_ref().ok_or_else(|| eyre!("milestones config section is required"))?;
	let (ms_owner, ms_repo) = parse_github_repo(&milestones_config.url)?;

	let old_numbers: HashSet<u64> = old_links.iter().filter(|l| l.owner() == ms_owner && l.repo() == ms_repo).map(|l| l.number()).collect();
	let new_numbers: HashSet<u64> = new_links.iter().filter(|l| l.owner() == ms_owner && l.repo() == ms_repo).map(|l| l.number()).collect();

	let to_assign: Vec<u64> = new_numbers.difference(&old_numbers).copied().collect();
	let to_unassign: Vec<u64> = old_numbers.difference(&new_numbers).copied().collect();

	if to_assign.is_empty() && to_unassign.is_empty() {
		return Ok(());
	}

	let client = tedi_adapters::github::client::get()?;
	let repo = tedi_core::RepoInfo::new(&ms_owner, &ms_repo);

	let mut futs = Vec::new();
	for num in &to_assign {
		println!("Assigning #{num} to milestone");
		futs.push(client.set_issue_milestone(repo, *num, Some(milestone_number)));
	}
	for num in &to_unassign {
		println!("Unassigning #{num} from milestone");
		futs.push(client.set_issue_milestone(repo, *num, None));
	}
	for fut in futs {
		fut.await?;
	}

	Ok(())
}
