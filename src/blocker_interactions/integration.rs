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
	DisplayFormat, Issue, LazyIssue, Marker, RepoInfo, VirtualIssue,
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

	/// Get the currently selected blocker issue source
	pub fn current() -> Option<Self> {
		let cache_path = get_current_blocker_cache_path();
		let path = std::fs::read_to_string(&cache_path).ok().map(|s| PathBuf::from(s.trim())).filter(|p| p.exists())?;
		Some(Self::build(path).expect("failed to build BlockerIssueSource from cached path"))
	}

	/// Set this issue as the current blocker issue
	pub fn set_current(&self) -> Result<()> {
		let cache_path = get_current_blocker_cache_path();
		std::fs::write(&cache_path, self.virtual_issue_buffer_path.to_string_lossy().as_bytes())?;
		Ok(())
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
		std::fs::write(&self.path, blockers.serialize(DisplayFormat::Headers))?;
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
pub async fn main_integrated(command: super::io::Command, format: DisplayFormat, offline: bool) -> Result<()> {
	use super::{io::Command, source::BlockerSource};
	use crate::open_interactions::{Modifier, SyncOptions, modify_and_sync_issue};

	match command {
		Command::Set { pattern } => {
			// Check if there's an urgent file - can't switch away until it's empty
			if let Some(urgent) = StandaloneSource::urgent() {
				let blockers = urgent.load()?;
				if !blockers.is_empty() {
					eprintln!("Cannot switch project while urgent tasks exist. Complete urgent tasks first.");
					eprintln!("  {}", urgent.path().display());
					if let Some(current) = blockers.current_with_context(&[]) {
						eprintln!("  Current urgent: {current}");
					}
					return Ok(());
				}
			}

			let issue_path = resolve_issue_file(&pattern)?;
			let source = BlockerIssueSource::build(issue_path)?;
			source.set_current()?;
			println!("Set blockers to: {}", source.display_name());

			// Load and show current blocker
			let blockers = source.load()?;
			if blockers.is_empty() {
				let marker = Marker::BlockersSection(tedi::Header::new(1, "Blockers"));
				println!("No `{marker}` marker found in issue body.");
			} else if let Some(current) = blockers.current_with_context(&[]) {
				println!("Current: {current}");
			} else {
				println!("Blockers section is empty.");
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

				v_utils::io::file_open::open(urgent.path()).await?;

				// Cleanup if empty after editing
				urgent.cleanup_if_empty()?;

				// Update tracking if enabled
				update_tracking_after_change().await;
			} else {
				let issue_source = if let Some(pat) = pattern {
					BlockerIssueSource::build(resolve_issue_file(&pat)?)?
				} else {
					BlockerIssueSource::current().ok_or_else(|| eyre!("No issue set. Use `todo blocker set <pattern>` first."))?
				};

				// Use unified modify flow
				let local_source = LocalIssueSource::<FsReader>::build_from_path(&issue_source.virtual_issue_buffer_path).await?;
				let issue = Issue::load(local_source).await?;
				modify_and_sync_issue(issue, offline, Modifier::Editor { open_at_blocker: false }, SyncOptions::default()).await?;

				// If set_after flag is set, update the current blocker issue
				if set_after {
					issue_source.set_current()?;
					println!("Set blockers to: {}", issue_source.display_name());
				}

				// Update tracking after change
				update_tracking_after_change().await;
			}
		}

		Command::List => {
			// Check if there's an urgent file - only show urgent when it exists
			if let Some(urgent) = StandaloneSource::urgent() {
				let blockers = urgent.load()?;
				if !blockers.is_empty() {
					println!("=== URGENT ===");
					println!("{}", blockers.serialize(format));
					return Ok(());
				}
			}

			let source = BlockerIssueSource::current().ok_or_else(|| eyre!("No blocker file set. Use `todo blocker set <pattern>` first."))?;
			let blockers = source.load()?;

			if blockers.is_empty() {
				let marker = Marker::BlockersSection(tedi::Header::new(1, "Blockers"));
				println!("No `{marker}` marker found in issue body.");
			} else {
				let output = blockers.serialize(format);
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

			let source = BlockerIssueSource::current().ok_or_else(|| eyre!("No blocker file set. Use `todo blocker set <pattern>` first."))?;
			let blockers = source.load()?;

			if !blockers.is_empty() {
				let hierarchy = if fully_qualified { source.hierarchy() } else { vec![] };

				if let Some(current) = blockers.current_with_context(&hierarchy) {
					const MAX_LEN: usize = 70;
					match current.len() {
						0..=MAX_LEN => println!("{current}"),
						_ => println!("{}...", &current[..(MAX_LEN - 3)]),
					}
				}
			}
			// No blockers section or no current blocker - silently exit (for status line integration)
		}

		Command::Pop => {
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

					// Update tracking after pop
					update_tracking_after_change().await;

					// Show new current
					if let Some(current) = blockers.current_with_context(&[]) {
						println!("Current (urgent): {current}");
					} else {
						println!("Urgent blockers now empty.");
					}
					return Ok(());
				}
			}

			let issue_source = BlockerIssueSource::current().ok_or_else(|| eyre!("No blocker file set. Use `todo blocker set <pattern>` first."))?;

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

			// Update tracking after pop
			update_tracking_after_change().await;

			// Show new current blocker (reload to get updated state)
			let blockers = issue_source.load()?;
			if let Some(new_current) = blockers.current_with_context(&[]) {
				println!("Current: {new_current}");
			} else {
				println!("Blockers section is now empty.");
			}
		}

		Command::Add { name, urgent: is_urgent } => {
			if is_urgent {
				// Add to global urgent.md
				let urgent = StandaloneSource::urgent_or_create()?;
				let mut blockers = urgent.load()?;
				blockers.add(&name);
				urgent.save(&blockers)?;

				// Update tracking after add
				update_tracking_after_change().await;

				println!("Added to urgent: {name}");
				if let Some(current) = blockers.current_with_context(&[]) {
					println!("Current (urgent): {current}");
				}
			} else {
				let issue_source = BlockerIssueSource::current().ok_or_else(|| eyre!("No blocker file set. Use `todo blocker set <pattern>` first."))?;

				// Use unified modify workflow
				let local_source = LocalIssueSource::<FsReader>::build_from_path(&issue_source.virtual_issue_buffer_path).await?;
				let issue = Issue::load(local_source).await?;
				let result = modify_and_sync_issue(issue, offline, Modifier::BlockerAdd { text: name.clone() }, SyncOptions::default()).await?;

				// Output results
				if let Some(output) = result.output {
					println!("{output}");
				}

				// Update tracking after add
				update_tracking_after_change().await;

				// Show new current blocker (reload to get updated state)
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

			// Use repo/title as project if not explicitly provided (only when not in urgent mode)
			if resume_args.project.is_none() {
				if let Some(source) = BlockerIssueSource::current() {
					let repo = source.repo_info.repo();
					let title = &source.issue.contents.title;
					resume_args.project = Some(format!("{repo}/{title}"));
				}
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

		Command::Halt(halt_args) => {
			// Disable tracking state
			super::clockify::set_tracking_enabled(false)?;

			super::clockify::stop_current_tracking(halt_args.workspace.as_deref()).await?;

			println!("Tracking halted.");
		}
	}

	Ok(())
}
/// Cache file for current blocker selection
static CURRENT_BLOCKER_ISSUE_CACHE: &str = "current_blocker_issue.txt";

/// Get the path to the current blocker issue cache file
fn get_current_blocker_cache_path() -> PathBuf {
	v_utils::xdg_cache_file!(CURRENT_BLOCKER_ISSUE_CACHE)
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

/// Update clockify tracking after a blocker change (add/pop).
/// Stops current tracking and starts new one with updated description.
async fn update_tracking_after_change() {
	if !super::clockify::is_tracking_enabled() {
		return;
	}

	// Stop current tracking
	if let Err(e) = super::clockify::stop_current_tracking(None).await {
		tracing::warn!("failed to stop clockify tracking: {e}");
	}

	// Restart with new current blocker
	if let Err(e) = super::clockify::restart_tracking_for_project(get_current_blocker_description, None).await {
		tracing::warn!("failed to restart clockify tracking: {e}");
	}
}
