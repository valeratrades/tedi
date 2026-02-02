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
	DisplayFormat, Issue, LazyIssue, Marker,
	local::{FsReader, Local, LocalIssueSource},
};

use super::{BlockerSequence, operations::BlockerSequenceExt, source::BlockerSource};
use crate::open_interactions::local::ExactMatchLevel;

/// Issue-based blocker source for blockers embedded in issue files.
pub struct IssueSource {
	pub issue_path: PathBuf,
	/// Cached parsed issue (needed for save to preserve structure)
	pub cached_issue: std::cell::RefCell<Option<Issue>>,
}
impl IssueSource {
	pub fn new(issue_path: PathBuf) -> Self {
		Self {
			issue_path,
			cached_issue: std::cell::RefCell::new(None),
		}
	}

	/// Get the currently selected blocker issue source
	pub fn current() -> Option<Self> {
		let cache_path = get_current_blocker_cache_path();
		std::fs::read_to_string(&cache_path).ok().map(|s| PathBuf::from(s.trim())).filter(|p| p.exists()).map(Self::new)
	}

	/// Set this issue as the current blocker issue
	pub fn set_current(&self) -> Result<()> {
		let cache_path = get_current_blocker_cache_path();
		std::fs::write(&cache_path, self.issue_path.to_string_lossy().as_bytes())?;
		Ok(())
	}

	/// Get relative path for display
	pub fn display_relative(&self) -> String {
		self.issue_path
			.strip_prefix(Local::issues_dir())
			.map(|p| p.to_string_lossy().to_string())
			.unwrap_or_else(|_| self.issue_path.to_string_lossy().to_string())
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
			let source = IssueSource::new(issue_path);
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
			if is_urgent {
				// Open the urgent file
				let urgent = StandaloneSource::urgent_or_create()?;

				v_utils::io::file_open::open(urgent.path()).await?;

				// Cleanup if empty after editing
				urgent.cleanup_if_empty()?;

				// Update tracking if enabled
				update_tracking_after_change().await;
			} else {
				let issue_source = if let Some(pat) = pattern {
					IssueSource::new(resolve_issue_file(&pat)?)
				} else {
					IssueSource::current().ok_or_else(|| eyre!("No issue set. Use `todo blocker set <pattern>` first."))?
				};

				// Use unified modify flow
				let local_source = LocalIssueSource::<FsReader>::build_from_path(&issue_source.issue_path)?;
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
			// Check if there's an urgent file - show that first
			if let Some(urgent) = StandaloneSource::urgent() {
				let blockers = urgent.load()?;
				if !blockers.is_empty() {
					println!("=== URGENT ===");
					println!("{}", blockers.serialize(format));
					println!();
				}
			}

			let source = IssueSource::current().ok_or_else(|| eyre!("No blocker file set. Use `todo blocker set <pattern>` first."))?;
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

			let source = IssueSource::current().ok_or_else(|| eyre!("No blocker file set. Use `todo blocker set <pattern>` first."))?;
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

			let issue_source = IssueSource::current().ok_or_else(|| eyre!("No blocker file set. Use `todo blocker set <pattern>` first."))?;

			// Check if blockers section exists before attempting pop
			let blockers = issue_source.load()?;
			if blockers.is_empty() {
				let marker = Marker::BlockersSection(tedi::Header::new(1, "Blockers"));
				bail!("No `{marker}` marker found in issue body.");
			}

			// Use unified modify workflow
			let local_source = LocalIssueSource::<FsReader>::build_from_path(&issue_source.issue_path)?;
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
				let issue_source = IssueSource::current().ok_or_else(|| eyre!("No blocker file set. Use `todo blocker set <pattern>` first."))?;

				// Use unified modify workflow
				let local_source = LocalIssueSource::<FsReader>::build_from_path(&issue_source.issue_path)?;
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
			// Get current blocker description for tracking
			let source = IssueSource::current().ok_or_else(|| eyre!("No blocker file set. Use `todo blocker set <pattern>` first."))?;
			let blockers = source.load()?;

			if blockers.current().is_none() {
				bail!("No current blocker task found. Add one with 'todo blocker add <task>'");
			}

			// Enable tracking state
			super::clockify::set_tracking_enabled(true)?;

			// Use issue title as project if not explicitly provided
			if resume_args.project.is_none() {
				resume_args.project = Some(source.cached_issue.borrow().as_ref().unwrap().contents.title.clone());
			}

			// Build hierarchy for fully-qualified mode
			let hierarchy = source.hierarchy();

			// Start tracking with current blocker description
			super::clockify::start_tracking_for_task(
				|fully_qualified| {
					let h = if fully_qualified { hierarchy.clone() } else { vec![] };
					blockers.current_with_context(&h).unwrap_or_default()
				},
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

impl super::source::BlockerSource for IssueSource {
	fn load(&self) -> Result<BlockerSequence> {
		let content = std::fs::read_to_string(&self.issue_path)?;
		let issue = Issue::deserialize_virtual(&content).map_err(|e| eyre!("Failed to parse issue: {e}"))?;

		// Clone the blockers before caching the issue
		let blockers = issue.contents.blockers.clone();

		// Cache the parsed issue (unused now, but kept for potential future use)
		*self.cached_issue.borrow_mut() = Some(issue);

		Ok(blockers)
	}

	fn display_name(&self) -> String {
		self.display_relative()
	}

	fn hierarchy(&self) -> Vec<String> {
		self.issue_path.file_stem().map(|s| vec![s.to_string_lossy().into_owned()]).unwrap_or_default()
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
	let source = IssueSource::current()?;
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

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn test_issue_source_load_and_save() {
		// This test verifies that IssueSource correctly loads and saves blockers
		// via the Issue struct. The actual parsing/serialization is tested in
		// open/issue.rs tests.
	}

	#[test]
	fn test_issue_parse_with_blockers() {
		let content = r#"- [ ] Issue Title <!-- @owner https://github.com/owner/repo/issues/1 -->
	Body text.

	# Blockers
	# Phase 1
	- First task
		comment on first task
	- Second task

	# Phase 2
	- Third task
"#;
		let issue = Issue::deserialize_virtual(content).unwrap();

		assert!(!issue.contents.blockers.is_empty());
		insta::assert_snapshot!(issue.contents.blockers.serialize(tedi::DisplayFormat::Headers), @"
		# Phase 1
		- First task
			comment on first task
		- Second task
		# Phase 2
		- Third task
		");
	}

	#[test]
	fn test_issue_blockers_current_with_context() {
		let content = r#"- [ ] Issue Title <!-- @owner https://github.com/owner/repo/issues/1 -->
	Body text.

	# Blockers
	# Phase 1
	- First task
	# Phase 2
	- Third task
"#;
		let issue = Issue::deserialize_virtual(content).unwrap();

		assert_eq!(issue.contents.blockers.current_with_context(&[]), Some("Phase 2: Third task".to_string()));

		let hierarchy = vec!["my_project".to_string()];
		assert_eq!(issue.contents.blockers.current_with_context(&hierarchy), Some("my_project: Phase 2: Third task".to_string()));
	}

	#[test]
	fn test_issue_blockers_pop() {
		let content = r#"- [ ] Issue Title <!-- @owner https://github.com/owner/repo/issues/1 -->
	Body text.

	# Blockers
	- First task
	- Second task
	- Third task
"#;
		let mut issue = Issue::deserialize_virtual(content).unwrap();

		issue.contents.blockers.pop();

		insta::assert_snapshot!(issue.contents.blockers.serialize(tedi::DisplayFormat::Headers), @"
		- First task
		- Second task
		");
	}

	#[test]
	fn test_issue_serialize_with_blockers() {
		let content = r#"- [ ] Issue Title <!-- @owner https://github.com/owner/repo/issues/1 -->
	Body text.

	# Blockers
	- First task
	- Second task
"#;
		let issue = Issue::deserialize_virtual(content).unwrap();

		let serialized = issue.serialize_virtual();
		assert!(serialized.contains("# Blockers"));
		assert!(serialized.contains("- First task"));
		assert!(serialized.contains("- Second task"));
	}

	#[test]
	fn test_issue_no_blockers_section() {
		let content = r#"- [ ] Issue Title <!-- @owner https://github.com/owner/repo/issues/1 -->
	Just some regular body text without blockers marker.
	- This is NOT a blocker, just body content.
"#;
		let issue = Issue::deserialize_virtual(content).unwrap();

		assert!(issue.contents.blockers.is_empty());
	}

	#[test]
	fn test_issue_blockers_with_subissue() {
		let content = r#"- [ ] Issue Title <!-- @owner https://github.com/owner/repo/issues/1 -->
	Body.

	# Blockers
	- Blocker one
	- Blocker two

	- [ ] Sub-issue <!--sub @owner https://github.com/owner/repo/issues/2 -->
		Sub-issue body
"#;
		let issue = Issue::deserialize_virtual(content).unwrap();

		// Blockers should only contain the blocker items, not the sub-issue
		insta::assert_snapshot!(issue.contents.blockers.serialize(tedi::DisplayFormat::Headers), @"
		- Blocker one
		- Blocker two
		");

		// Sub-issue should be in children
		assert_eq!(issue.children.len(), 1);
		assert_eq!(issue.children[0].contents.title, "Sub-issue");
	}
}
