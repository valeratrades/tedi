//! Integration with issue files and the `open` module.
//!
//! This module handles blockers embedded in issue files, using the `open/` module's
//! file management and sync mechanics.
//!
//! Urgent mode: when `--urgent` is specified, blockers are stored in an owner-level
//! urgent.md file at `issues/{owner}/urgent.md`. This file is a simple blocker list
//! without Github sync. Only one urgent file can exist at a time across all owners.

use std::path::{Path, PathBuf};

use color_eyre::eyre::{Result, bail, eyre};
use tedi::{
	Issue, IssueLink, LazyIssue, Marker, MilestoneBlockerCache, RepoInfo, VirtualIssue,
	local::{ExactMatchLevel, FsReader, Local, LocalIssueSource},
};

use super::{BlockerSequence, operations::BlockerSequenceExt, source::BlockerSource};

/// Issue-based blocker source for blockers embedded in issue files.
pub struct BlockerIssueSource {
	pub virtual_issue_buffer_path: PathBuf,
	pub repo_info: RepoInfo,
	pub issue: VirtualIssue,
}
impl BlockerIssueSource {
	pub fn build(issue_path: PathBuf) -> Result<Self> {
		let content = std::fs::read_to_string(&issue_path)?;
		let issue = VirtualIssue::parse(&content, issue_path.clone())?;

		let rel_path = issue_path
			.strip_prefix(Local::issues_dir())
			.map_err(|_| eyre!("Issue file is not in issues directory: {issue_path:?}"))?;
		let components: Vec<_> = rel_path.components().collect();
		assert!(components.len() >= 3, "Path too short to extract repo info: {issue_path:?}");
		let owner = components[0].as_os_str().to_str().expect("non-utf8 owner in issue path");
		let repo = components[1].as_os_str().to_str().expect("non-utf8 repo in issue path");
		let repo_info = RepoInfo::new(owner, repo);

		Ok(Self {
			virtual_issue_buffer_path: issue_path,
			repo_info,
			issue,
		})
	}

	/// Get the currently selected blocker issue source, derived from milestone cache.
	pub fn current() -> Option<Self> {
		let cache = MilestoneBlockerCache::load()?;
		let path = cache.current_path()?;
		Some(Self::build(path).expect("failed to build BlockerIssueSource from milestone-cached link"))
	}

	/// Get relative path for display
	pub fn display_relative(&self) -> String {
		self.virtual_issue_buffer_path
			.strip_prefix(Local::issues_dir())
			.map(|p| p.to_string_lossy().to_string())
			.unwrap_or_else(|_| self.virtual_issue_buffer_path.to_string_lossy().to_string())
	}
}

/// Standalone blocker source for simple blocker files (no Issue metadata).
/// Used for urgent.md and similar standalone blocker lists.
pub struct StandaloneSource {
	path: PathBuf,
}
impl StandaloneSource {
	pub fn new(path: PathBuf) -> Self {
		Self { path }
	}

	/// Get the urgent blocker source if it exists
	pub fn urgent() -> Option<Self> {
		let path = Local::issues_dir().join("urgent.md");
		path.exists().then(|| Self::new(path))
	}

	/// Get or create the urgent blocker source
	pub fn urgent_or_create() -> Result<Self> {
		let path = Local::issues_dir().join("urgent.md");
		if !path.exists() {
			if let Some(parent) = path.parent() {
				std::fs::create_dir_all(parent)?;
			}
			std::fs::write(&path, "")?;
		}
		Ok(Self::new(path))
	}

	/// Save blockers to the file
	pub fn save(&self, blockers: &BlockerSequence) -> Result<()> {
		if let Some(parent) = self.path.parent() {
			std::fs::create_dir_all(parent)?;
		}
		let s: String = blockers.to_events().into();
		std::fs::write(&self.path, s)?;
		Ok(())
	}

	/// Remove the file if blockers are empty
	pub fn cleanup_if_empty(&self) -> Result<()> {
		if !self.path.exists() {
			return Ok(());
		}
		let blockers = <Self as BlockerSource>::load(self)?;
		if blockers.is_empty() {
			std::fs::remove_file(&self.path)?;
			eprintln!("Removed empty file: {}", self.path.display());
		}
		Ok(())
	}

	/// Get the path
	pub fn path(&self) -> &Path {
		&self.path
	}
}

/// Main entry point for integrated blocker commands (works with issue files).
/// This is the default mode for blocker commands.
pub async fn main_integrated(command: super::io::Command, offline: bool) -> Result<()> {
	use super::{io::Command, source::BlockerSource};
	use crate::open_interactions::{Modifier, SyncOptions, modify_and_sync_issue};

	match command {
		Command::Move(sub) => {
			use super::io::MoveCommand;
			let description_before = get_current_blocker_description(false);
			let result = match sub {
				MoveCommand::Up => MilestoneBlockerCache::move_by(1),
				MoveCommand::Down => MilestoneBlockerCache::move_by(-1),
				MoveCommand::To { pattern } => MilestoneBlockerCache::set_by_pattern(&pattern),
			};
			match result {
				Ok(link) => {
					let path =
						MilestoneBlockerCache::resolve_link_to_path(&link).ok_or_else(|| eyre!("Could not find local file for {}/{}/#{}", link.owner(), link.repo(), link.number()))?;
					let source = BlockerIssueSource::build(path)?;
					println!("Moved to: {}", source.display_name());

					// Post-update: follow refs then update clockify
					post_update(description_before, false).await?;

					// Show current after ref-following (may have changed)
					if let Some(source) = BlockerIssueSource::current()
						&& let Ok(blockers) = source.load()
						&& let Some(current) = blockers.current_with_context(&[])
					{
						println!("Current: {current}");
					}
				}
				Err(msg) => {
					bail!("{msg}");
				}
			}
		}

		Command::Open {
			pattern,
			set_after,
			urgent: is_urgent,
		} => {
			// When urgent exists, always open it (--urgent flag is only for creating new urgent file)
			if StandaloneSource::urgent().is_some() || is_urgent {
				let urgent = StandaloneSource::urgent_or_create()?;

				let description_before = get_current_blocker_description(false);
				v_utils::io::file_open::open(urgent.path()).await?;

				// Cleanup if empty after editing
				urgent.cleanup_if_empty()?;

				// Post-update
				post_update(description_before, true).await?;
			} else {
				let issue_source = if let Some(pat) = pattern {
					BlockerIssueSource::build(resolve_issue_file(&pat)?)?
				} else {
					BlockerIssueSource::current().ok_or_else(|| eyre!("No issue set. Run `todo milestones edit` to set up milestone."))?
				};

				let description_before = get_current_blocker_description(false);

				// Use unified modify flow
				let local_source = LocalIssueSource::<FsReader>::build_from_path(&issue_source.virtual_issue_buffer_path).await?;
				let issue = Issue::load(local_source).await?;
				modify_and_sync_issue(issue, offline, Modifier::Editor { open_at_blocker: false }, SyncOptions::default()).await?;

				// If set_after flag is set, point milestone cache at this issue
				if set_after {
					MilestoneBlockerCache::set_by_path(&issue_source.virtual_issue_buffer_path)?;
					println!("Set blockers to: {}", issue_source.display_name());
				}

				// Post-update
				post_update(description_before, false).await?;
			}
		}

		Command::List => {
			// Check if there's an urgent file - only show urgent when it exists
			if let Some(urgent) = StandaloneSource::urgent() {
				let blockers = urgent.load()?;
				if !blockers.is_empty() {
					println!("=== URGENT ===");
					println!("{}", String::from(&blockers));
					return Ok(());
				}
			}

			let source = BlockerIssueSource::current().ok_or_else(|| eyre!("No blocker source. Run `todo milestones edit` to set up milestone."))?;
			let blockers = source.load()?;

			if blockers.is_empty() {
				let marker = Marker::BlockersSection(tedi::Header::new(1, "Blockers"));
				println!("No `{marker}` marker found in issue body.");
			} else {
				let output = String::from(&blockers);
				if output.is_empty() {
					println!("Blockers section is empty.");
				} else {
					println!("{output}");
				}
			}
		}

		Command::Current { fully_qualified } => {
			// Check urgent file first - urgent tasks take priority
			if let Some(urgent) = StandaloneSource::urgent() {
				let blockers = urgent.load()?;
				if let Some(current) = blockers.current_with_context(&[]) {
					let prefix = if fully_qualified { "URGENT: " } else { "" };
					let output = format!("{prefix}{current}");
					const MAX_LEN: usize = 70;
					match output.len() {
						0..=MAX_LEN => println!("{output}"),
						_ => println!("{}...", &output[..(MAX_LEN - 3)]),
					}
					return Ok(());
				}
			}

			let cache = MilestoneBlockerCache::load();
			let source = BlockerIssueSource::current().ok_or_else(|| eyre!("No blocker source. Run `todo milestones edit` to set up milestone."))?;
			let blockers = source.load()?;

			if !blockers.is_empty() {
				let hierarchy = if fully_qualified { source.hierarchy() } else { vec![] };

				if let Some(current) = blockers.current_with_context(&hierarchy) {
					let position_prefix = cache.map(|c| format!("{}| ", c.current_index + 1)).unwrap_or_default();
					let output = format!("{position_prefix}{current}");
					const MAX_LEN: usize = 70;
					match output.len() {
						0..=MAX_LEN => println!("{output}"),
						_ => println!("{}...", &output[..(MAX_LEN - 3)]),
					}
				}
			}
			// No blockers section or no current blocker - silently exit (for status line integration)
		}

		Command::Pop => {
			let description_before = get_current_blocker_description(false);

			// Check if there's an urgent file - pop from there first
			if let Some(urgent) = StandaloneSource::urgent() {
				let mut blockers = urgent.load()?;
				if !blockers.is_empty() {
					let popped = blockers.pop();
					urgent.save(&blockers)?;

					if let Some(text) = popped {
						println!("Popped (urgent): {text}");
					}

					urgent.cleanup_if_empty()?;

					// Post-update
					post_update(description_before, true).await?;

					// Show new current
					if let Some(current) = blockers.current_with_context(&[]) {
						println!("Current (urgent): {current}");
					} else {
						println!("Urgent blockers now empty.");
					}
					return Ok(());
				}
			}

			let issue_source = BlockerIssueSource::current().ok_or_else(|| eyre!("No blocker source. Run `todo milestones edit` to set up milestone."))?;

			// Check if blockers section exists before attempting pop
			let blockers = issue_source.load()?;
			if blockers.is_empty() {
				let marker = Marker::BlockersSection(tedi::Header::new(1, "Blockers"));
				bail!("No `{marker}` marker found in issue body.");
			}

			// Use unified modify workflow
			let local_source = LocalIssueSource::<FsReader>::build_from_path(&issue_source.virtual_issue_buffer_path).await?;
			let issue = Issue::load(local_source).await?;
			let result = modify_and_sync_issue(issue, offline, Modifier::BlockerPop, SyncOptions::default()).await?;

			// Output results
			if let Some(output) = result.output {
				println!("{output}");
			}

			// Post-update
			post_update(description_before, false).await?;

			// Show new current blocker (reload to get updated state)
			let blockers = issue_source.load()?;
			if let Some(new_current) = blockers.current_with_context(&[]) {
				println!("Current: {new_current}");
			} else {
				println!("Blockers section is now empty.");
			}
		}

		Command::Add {
			name,
			nested: nest,
			urgent: is_urgent,
		} => {
			let description_before = get_current_blocker_description(false);

			if is_urgent {
				let urgent = StandaloneSource::urgent_or_create()?;
				let mut blockers = urgent.load()?;
				if nest {
					blockers.add_child(&name);
				} else {
					blockers.add(&name);
				}
				urgent.save(&blockers)?;

				post_update(description_before.clone(), true).await?;

				println!("Added to urgent: {name}");
				if let Some(current) = blockers.current_with_context(&[]) {
					println!("Current (urgent): {current}");
				}
			} else {
				let issue_source = BlockerIssueSource::current().ok_or_else(|| eyre!("No blocker source. Run `todo milestones edit` to set up milestone."))?;

				let local_source = LocalIssueSource::<FsReader>::build_from_path(&issue_source.virtual_issue_buffer_path).await?;
				let issue = Issue::load(local_source).await?;
				let result = modify_and_sync_issue(issue, offline, Modifier::BlockerAdd { text: name.clone(), nest }, SyncOptions::default()).await?;

				if let Some(output) = result.output {
					println!("{output}");
				}

				post_update(description_before, false).await?;

				let blockers = issue_source.load()?;
				if let Some(new_current) = blockers.current_with_context(&[]) {
					println!("Current: {new_current}");
				}
			}
		}

		Command::Resume(mut resume_args) => {
			let description = get_current_blocker_description(false).ok_or_else(|| eyre!("No current blocker task found. Add one with 'todo blocker add <task>'"))?;

			// Enable tracking state
			super::clockify::set_tracking_enabled(true)?;

			if resume_args.project.is_none() {
				resume_args.project = current_project();
			}

			// Start tracking with current blocker description
			super::clockify::start_tracking_for_task(
				|fully_qualified| get_current_blocker_description(fully_qualified).unwrap_or(description.clone()),
				&resume_args,
				None,
			)
			.await?;

			println!("Tracking resumed.");
		}

		Command::CurrentProject => {
			let project = current_project().ok_or_else(|| eyre!("No blocker source. Run `todo milestones edit` to set up milestone."))?;
			println!("{project}");
		}

		Command::Halt(halt_args) => {
			// Disable tracking state
			super::clockify::set_tracking_enabled(false)?;

			super::clockify::stop_current_tracking(halt_args.workspace.as_deref()).await?;

			println!("Tracking halted.");
		}
	}

	Ok(())
}

impl super::source::BlockerSource for BlockerIssueSource {
	fn load(&self) -> Result<BlockerSequence> {
		let content = std::fs::read_to_string(&self.virtual_issue_buffer_path)?;
		let parsed = VirtualIssue::parse(&content, self.virtual_issue_buffer_path.clone())?;
		Ok(parsed.contents.blockers)
	}

	fn display_name(&self) -> String {
		self.display_relative()
	}

	fn hierarchy(&self) -> Vec<String> {
		self.virtual_issue_buffer_path.file_stem().map(|s| vec![s.to_string_lossy().into_owned()]).unwrap_or_default()
	}
}

impl super::source::BlockerSource for StandaloneSource {
	fn load(&self) -> Result<BlockerSequence> {
		match std::fs::read_to_string(&self.path) {
			Ok(content) => Ok(BlockerSequence::parse(&content)),
			Err(e) if e.kind() == std::io::ErrorKind::NotFound => Ok(BlockerSequence::default()),
			Err(e) => Err(e.into()),
		}
	}

	fn display_name(&self) -> String {
		self.path.file_name().map(|n| n.to_string_lossy().to_string()).unwrap_or_else(|| "standalone".to_string())
	}

	fn hierarchy(&self) -> Vec<String> {
		vec!["urgent".to_string()]
	}
}

/// Resolve an issue file from a pattern, using fzf if multiple matches.
fn resolve_issue_file(pattern: &str) -> Result<PathBuf> {
	Local::fzf_issue(pattern, ExactMatchLevel::default())
}

/// Get the current clockify project name (repo/title) from the current blocker issue.
fn current_project() -> Option<String> {
	let source = BlockerIssueSource::current()?;
	let repo = source.repo_info.repo();
	let title = &source.issue.contents.title;
	Some(format!("{repo}/{title}"))
}

/// Get the current blocker description, checking urgent first then falling back to issue.
fn get_current_blocker_description(fully_qualified: bool) -> Option<String> {
	// Check urgent file first
	if let Some(urgent) = StandaloneSource::urgent()
		&& let Ok(blockers) = urgent.load()
	{
		let hierarchy = if fully_qualified { urgent.hierarchy() } else { vec![] };
		if let Some(current) = blockers.current_with_context(&hierarchy) {
			return Some(current);
		}
	}

	// Fall back to current issue
	let source = BlockerIssueSource::current()?;
	let blockers = source.load().ok()?;
	let hierarchy = if fully_qualified { source.hierarchy() } else { vec![] };
	blockers.current_with_context(&hierarchy)
}

/// Post-update step after any blocker mutation.
///
/// 1. If the current blocker references an issue, follow that ref (set milestone cache to it).
///    Recurses until no more refs are found, with cycle detection.
/// 2. Otherwise, update clockify tracking.
///
/// `is_urgent`: if true, error out when a blocker references an issue (urgent can't reference).
async fn post_update(description_before: Option<String>, is_urgent: bool) -> Result<()> {
	follow_blocker_refs(is_urgent, Vec::new())?;
	update_clockify_tracking(description_before).await;
	Ok(())
}

/// Follow issue refs in the current blocker chain.
///
/// Walks the current blocker's ancestor path for issue refs. If one is found,
/// sets the milestone cache to that issue and recurses. The `visited` list
/// prevents infinite loops — if we'd revisit a link, we error with the cycle.
fn follow_blocker_refs(is_urgent: bool, mut visited: Vec<IssueLink>) -> Result<()> {
	let issue_ref = if is_urgent {
		let urgent = StandaloneSource::urgent();
		let Some(urgent) = urgent else { return Ok(()) };
		let blockers = urgent.load()?;
		blockers.current_issue_ref()
	} else {
		let Some(source) = BlockerIssueSource::current() else { return Ok(()) };
		let blockers = source.load()?;
		let mut r = blockers.current_issue_ref();
		// Resolve bare refs using the parent issue's repo as context
		if let Some(ref mut issue_ref) = r {
			let ctx = format!("{}/{}", source.repo_info.owner(), source.repo_info.repo());
			issue_ref.resolve_with_context(&ctx);
		}
		r
	};

	let Some(issue_ref) = issue_ref else { return Ok(()) };

	if is_urgent {
		bail!("Urgent blockers cannot reference issues (found: {issue_ref})");
	}

	let Some(link) = issue_ref.to_issue_link() else {
		// Unresolved ref (bare #N without context) — nothing to follow
		return Ok(());
	};

	// Cycle detection
	if visited.iter().any(|v| v.number() == link.number() && v.owner() == link.owner() && v.repo() == link.repo()) {
		let cycle: Vec<String> = visited.iter().map(|l| format!("{}/{}#{}", l.owner(), l.repo(), l.number())).collect();
		bail!("Blocker reference cycle detected: {} → {issue_ref}", cycle.join(" → "));
	}

	let Some(path) = MilestoneBlockerCache::resolve_link_to_path(&link) else {
		// Referenced issue not found locally — nothing to follow
		return Ok(());
	};

	// Add current link to visited before following
	if let Some(current_link) = MilestoneBlockerCache::load().and_then(|c| c.current_link()) {
		visited.push(current_link);
	}

	MilestoneBlockerCache::set_by_path(&path)?;
	println!("Followed blocker ref → {}", BlockerIssueSource::build(path)?.display_name());

	// Recurse — the new current issue's blocker might also be a ref
	follow_blocker_refs(false, visited)
}

/// Update clockify tracking after a blocker change (add/pop/edit).
/// Only restarts tracking if the current blocker description actually changed.
async fn update_clockify_tracking(description_before: Option<String>) {
	if !super::clockify::is_tracking_enabled() {
		return;
	}

	let description_after = get_current_blocker_description(false);
	if description_before == description_after {
		return;
	}

	// Stop current tracking
	if let Err(e) = super::clockify::stop_current_tracking(None).await {
		tracing::warn!("failed to stop clockify tracking: {e}");
	}

	// Restart with new current blocker
	let Some(description) = description_after else {
		return;
	};
	let resume_args = super::clockify::ResumeArgs {
		project: current_project(),
		..Default::default()
	};
	if let Err(e) = super::clockify::start_tracking_for_task(
		|fully_qualified| get_current_blocker_description(fully_qualified).unwrap_or(description.clone()),
		&resume_args,
		None,
	)
	.await
	{
		eprintln!("Warning: Failed to start tracking for task: {e}");
	}
}
