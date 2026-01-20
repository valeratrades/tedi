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
use tedi::{DisplayFormat, Issue, Marker};

use super::{BlockerSequence, operations::BlockerSequenceExt, source::BlockerSource};
use crate::open_interactions::local::{ExactMatchLevel, Local};

/// Cache file for current blocker selection
static CURRENT_BLOCKER_ISSUE_CACHE: &str = "current_blocker_issue.txt";

/// Get the path to the current blocker issue cache file
fn get_current_blocker_cache_path() -> PathBuf {
	v_utils::xdg_cache_file!(CURRENT_BLOCKER_ISSUE_CACHE)
}

/// Get the currently selected blocker issue file path
pub fn get_current_blocker_issue() -> Option<PathBuf> {
	let cache_path = get_current_blocker_cache_path();
	std::fs::read_to_string(&cache_path).ok().map(|s| PathBuf::from(s.trim())).filter(|p| p.exists())
}

/// Set the current blocker issue file path
pub fn set_current_blocker_issue(path: &Path) -> Result<()> {
	let cache_path = get_current_blocker_cache_path();
	std::fs::write(&cache_path, path.to_string_lossy().as_bytes())?;
	Ok(())
}

/// Issue-based blocker source for blockers embedded in issue files.
pub struct IssueSource {
	issue_path: PathBuf,
	/// Cached parsed issue (needed for save to preserve structure)
	cached_issue: std::cell::RefCell<Option<Issue>>,
}

impl IssueSource {
	pub fn new(issue_path: PathBuf) -> Self {
		Self {
			issue_path,
			cached_issue: std::cell::RefCell::new(None),
		}
	}

	/// Get relative path for display
	pub fn display_relative(&self) -> String {
		self.issue_path
			.strip_prefix(Local::issues_dir())
			.map(|p| p.to_string_lossy().to_string())
			.unwrap_or_else(|_| self.issue_path.to_string_lossy().to_string())
	}
}

impl super::source::BlockerSource for IssueSource {
	fn load(&self) -> Result<BlockerSequence> {
		let content = std::fs::read_to_string(&self.issue_path)?;
		let issue = Issue::parse_virtual(&content, &self.issue_path).map_err(|e| eyre!("Failed to parse issue: {e}"))?;

		// Clone the blockers before caching the issue
		let blockers = issue.contents.blockers.clone();

		// Cache the parsed issue (unused now, but kept for potential future use)
		*self.cached_issue.borrow_mut() = Some(issue);

		Ok(blockers)
	}

	fn display_name(&self) -> String {
		self.display_relative()
	}

	fn path_for_hierarchy(&self) -> Option<PathBuf> {
		Some(self.issue_path.clone())
	}
}

/// Resolve an issue file from a pattern, using fzf if multiple matches.
fn resolve_issue_file(pattern: &str) -> Result<PathBuf> {
	// Always pass all files to fzf, let it handle filtering (uses fuzzy match by default)
	let all_files = Local::search_issue_files("")?;
	if all_files.is_empty() {
		bail!("No issue files found. Use `todo open <url>` to fetch an issue first.");
	}
	match Local::choose_issue_with_fzf(&all_files, pattern, ExactMatchLevel::default())? {
		Some(path) => Ok(path),
		None => bail!("No issue selected"),
	}
}

/// Get the current issue source, or error if none set.
fn get_current_source() -> Result<IssueSource> {
	let issue_path = get_current_blocker_issue().ok_or_else(|| eyre!("No blocker file set. Use `todo blocker set <pattern>` first."))?;
	Ok(IssueSource::new(issue_path))
}

//=============================================================================
// Urgent file handling - owner-level urgent.md at issues/{owner}/urgent.md
//=============================================================================

/// Get the owner from the current blocker issue path.
fn get_current_owner() -> Option<String> {
	let issue_path = get_current_blocker_issue()?;
	let ancestry = Local::extract_ancestry(&issue_path).ok()?;
	Some(ancestry.owner().to_string())
}

/// Get the path to the owner-level urgent.md file.
fn get_urgent_path(owner: &str) -> PathBuf {
	Local::issues_dir().join(owner).join("urgent.md")
}

/// Check if an urgent.md file exists for any owner.
/// Returns the path and owner name if found.
fn find_existing_urgent() -> Option<(PathBuf, String)> {
	let issues_dir = Local::issues_dir();
	if !issues_dir.exists() {
		return None;
	}

	for entry in std::fs::read_dir(&issues_dir).ok()?.flatten() {
		if entry.file_type().ok()?.is_dir() {
			let urgent_path = entry.path().join("urgent.md");
			if urgent_path.exists() {
				let owner = entry.file_name().to_string_lossy().to_string();
				return Some((urgent_path, owner));
			}
		}
	}
	None
}

/// Load blockers from an urgent.md file (simple blocker list, no issue metadata).
/// Returns empty blockers if the file doesn't exist yet.
fn load_urgent_blockers(path: &Path) -> Result<BlockerSequence> {
	match std::fs::read_to_string(path) {
		Ok(content) => Ok(BlockerSequence::parse(&content)),
		Err(e) if e.kind() == std::io::ErrorKind::NotFound => Ok(BlockerSequence::default()),
		Err(e) => Err(e.into()),
	}
}

/// Save blockers to an urgent.md file.
fn save_urgent_blockers(path: &Path, blockers: &BlockerSequence) -> Result<()> {
	if let Some(parent) = path.parent() {
		std::fs::create_dir_all(parent)?;
	}
	std::fs::write(path, blockers.serialize(DisplayFormat::Headers))?;
	Ok(())
}

/// Check if urgent file is semantically empty and delete it if so.
fn cleanup_urgent_if_empty(path: &Path) -> Result<()> {
	if !path.exists() {
		return Ok(());
	}

	let content = std::fs::read_to_string(path)?;
	let blockers = BlockerSequence::parse(&content);

	if blockers.is_empty() {
		std::fs::remove_file(path)?;
		eprintln!("Removed empty urgent file: {}", path.display());
	}

	Ok(())
}

/// Get the current blocker description for tracking purposes.
/// Checks urgent file first, then falls back to current issue.
/// Returns (description, is_urgent) if found.
fn get_current_blocker_description(fully_qualified: bool) -> Option<(String, bool)> {
	// Check urgent file first
	if let Some((urgent_path, owner)) = find_existing_urgent() {
		let blockers = load_urgent_blockers(&urgent_path).ok()?;
		if let Some(current) = blockers.current_with_context(&[]) {
			let prefix = if fully_qualified { format!("URGENT({owner}): ") } else { "URGENT: ".to_string() };
			return Some((format!("{prefix}{current}"), true));
		}
	}

	// Fall back to current issue
	let source = get_current_source().ok()?;
	let blockers = source.load().ok()?;
	let hierarchy = if fully_qualified {
		source
			.path_for_hierarchy()
			.and_then(|p| p.file_stem().map(|s| s.to_string_lossy().to_string()))
			.map(|s| vec![s])
			.unwrap_or_default()
	} else {
		vec![]
	};
	blockers.current_with_context(&hierarchy).map(|desc| (desc, false))
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
	if let Err(e) = super::clockify::restart_tracking_for_project(|fully_qualified| get_current_blocker_description(fully_qualified).map(|(desc, _)| desc), None).await {
		tracing::warn!("failed to restart clockify tracking: {e}");
	}
}

/// Main entry point for integrated blocker commands (works with issue files).
/// This is the default mode for blocker commands.
pub async fn main_integrated(command: super::io::Command, format: DisplayFormat, offline: bool) -> Result<()> {
	use super::{io::Command, source::BlockerSource};
	use crate::open_interactions::{Modifier, SyncOptions, modify_and_sync_issue, modify_issue_offline};

	match command {
		Command::Set { pattern } => {
			// Check if there's an urgent file - can't switch away until it's empty
			if let Some((urgent_path, owner)) = find_existing_urgent() {
				let blockers = load_urgent_blockers(&urgent_path)?;
				if !blockers.is_empty() {
					eprintln!("Cannot switch project while urgent tasks exist for '{owner}'. Complete urgent tasks first.");
					eprintln!("  {}", urgent_path.display());
					if let Some(current) = blockers.current_with_context(&[]) {
						eprintln!("  Current urgent: {current}");
					}
					return Ok(());
				}
			}

			let issue_path = resolve_issue_file(&pattern)?;
			set_current_blocker_issue(&issue_path)?;

			let source = IssueSource::new(issue_path);
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

		Command::Open { pattern, set_after, urgent } => {
			if urgent {
				// Open the urgent file for the current owner
				let owner = get_current_owner().ok_or_else(|| eyre!("No blocker file set. Use `todo blocker set <pattern>` first to establish owner context."))?;
				let urgent_path = get_urgent_path(&owner);

				if !urgent_path.exists() {
					// Create empty urgent file
					if let Some(parent) = urgent_path.parent() {
						std::fs::create_dir_all(parent)?;
					}
					std::fs::write(&urgent_path, "")?;
				}

				v_utils::io::file_open::open(&urgent_path).await?;

				// Cleanup if empty after editing
				cleanup_urgent_if_empty(&urgent_path)?;

				// Update tracking if enabled
				update_tracking_after_change().await;
			} else {
				let issue_path = if let Some(pat) = pattern {
					resolve_issue_file(&pat)?
				} else {
					get_current_blocker_issue().ok_or_else(|| eyre!("No issue set. Use `todo blocker set <pattern>` first."))?
				};

				// Open the issue file with $EDITOR (offline, no Github sync)
				v_utils::io::file_open::open(&issue_path).await?;

				// If set_after flag is set, update the current blocker issue
				if set_after {
					set_current_blocker_issue(&issue_path)?;
					let source = IssueSource::new(issue_path);
					println!("Set blockers to: {}", source.display_name());
				}
			}
		}

		Command::List => {
			// Check if there's an urgent file - show that first
			if let Some((urgent_path, owner)) = find_existing_urgent() {
				let blockers = load_urgent_blockers(&urgent_path)?;
				if !blockers.is_empty() {
					println!("=== URGENT ({owner}) ===");
					println!("{}", blockers.serialize(format));
					println!();
				}
			}

			let source = get_current_source()?;
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
			if let Some((urgent_path, owner)) = find_existing_urgent() {
				let blockers = load_urgent_blockers(&urgent_path)?;
				if let Some(current) = blockers.current_with_context(&[]) {
					let prefix = if fully_qualified { format!("URGENT({owner}): ") } else { "URGENT: ".to_string() };
					let output = format!("{prefix}{current}");
					const MAX_LEN: usize = 70;
					match output.len() {
						0..=MAX_LEN => println!("{output}"),
						_ => println!("{}...", &output[..(MAX_LEN - 3)]),
					}
					return Ok(());
				}
			}

			let source = get_current_source()?;
			let blockers = source.load()?;

			if !blockers.is_empty() {
				let hierarchy = if fully_qualified {
					source
						.path_for_hierarchy()
						.and_then(|p| p.file_stem().map(|s| s.to_string_lossy().to_string()))
						.map(|s| vec![s])
						.unwrap_or_default()
				} else {
					vec![]
				};

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
			if let Some((urgent_path, owner)) = find_existing_urgent() {
				let mut blockers = load_urgent_blockers(&urgent_path)?;
				if !blockers.is_empty() {
					let popped = blockers.pop();
					save_urgent_blockers(&urgent_path, &blockers)?;

					if let Some(text) = popped {
						println!("Popped (urgent {owner}): {text}");
					}

					cleanup_urgent_if_empty(&urgent_path)?;

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

			let issue_path = get_current_blocker_issue().ok_or_else(|| eyre!("No blocker file set. Use `todo blocker set <pattern>` first."))?;

			// Check if blockers section exists before attempting pop
			let source = IssueSource::new(issue_path.clone());
			let blockers = source.load()?;
			if blockers.is_empty() {
				let marker = Marker::BlockersSection(tedi::Header::new(1, "Blockers"));
				bail!("No `{marker}` marker found in issue body.");
			}

			// Use unified modify workflow - offline version skips Github client requirement
			let result = if offline {
				modify_issue_offline(&issue_path, Modifier::BlockerPop).await?
			} else {
				modify_and_sync_issue(&issue_path, offline, Modifier::BlockerPop, SyncOptions::default()).await?
			};

			// Output results
			if let Some(output) = result.output {
				println!("{output}");
			}

			// Update tracking after pop
			update_tracking_after_change().await;

			// Show new current blocker
			let source = IssueSource::new(issue_path);
			let blockers = source.load()?;
			if let Some(new_current) = blockers.current_with_context(&[]) {
				println!("Current: {new_current}");
			} else {
				println!("Blockers section is now empty.");
			}
		}

		Command::Add { name, urgent } => {
			if urgent {
				// Add to owner-level urgent.md
				let owner = get_current_owner().ok_or_else(|| eyre!("No blocker file set. Use `todo blocker set <pattern>` first to establish owner context."))?;

				// Check if another urgent file exists for a different owner
				if let Some((existing_path, existing_owner)) = find_existing_urgent()
					&& existing_owner != owner
				{
					bail!(
						"Cannot create urgent file for '{owner}': another urgent file exists for '{existing_owner}'. Complete that first.\n  {}",
						existing_path.display()
					);
				}

				let urgent_path = get_urgent_path(&owner);
				let mut blockers = load_urgent_blockers(&urgent_path)?;
				blockers.add(&name);
				save_urgent_blockers(&urgent_path, &blockers)?;

				// Update tracking after add
				update_tracking_after_change().await;

				println!("Added to urgent ({owner}): {name}");
				if let Some(current) = blockers.current_with_context(&[]) {
					println!("Current (urgent): {current}");
				}
			} else {
				let issue_path = get_current_blocker_issue().ok_or_else(|| eyre!("No blocker file set. Use `todo blocker set <pattern>` first."))?;

				// Use unified modify workflow - offline version skips Github client requirement
				let result = if offline {
					modify_issue_offline(&issue_path, Modifier::BlockerAdd { text: name.clone() }).await?
				} else {
					modify_and_sync_issue(&issue_path, offline, Modifier::BlockerAdd { text: name.clone() }, SyncOptions::default()).await?
				};

				// Output results
				if let Some(output) = result.output {
					println!("{output}");
				}

				// Update tracking after add
				update_tracking_after_change().await;

				// Show new current blocker
				let source = IssueSource::new(issue_path);
				let blockers = source.load()?;
				if let Some(new_current) = blockers.current_with_context(&[]) {
					println!("Current: {new_current}");
				}
			}
		}

		Command::Resume(resume_args) => {
			// Get current blocker description for tracking
			let source = get_current_source()?;
			let blockers = source.load()?;

			if blockers.current().is_none() {
				bail!("No current blocker task found. Add one with 'todo blocker add <task>'");
			}

			// Enable tracking state
			super::clockify::set_tracking_enabled(true)?;

			// Build hierarchy for fully-qualified mode
			let issue_name = source.path_for_hierarchy().and_then(|p| p.file_stem().map(|s| s.to_string_lossy().to_string()));

			// Start tracking with current blocker description
			super::clockify::start_tracking_for_task(
				|fully_qualified| {
					let hierarchy = if fully_qualified { issue_name.clone().map(|s| vec![s]).unwrap_or_default() } else { vec![] };
					blockers.current_with_context(&hierarchy).unwrap_or_default()
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

#[cfg(test)]
mod tests {
	use std::path::Path;

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
		let issue = Issue::parse_virtual(content, Path::new("test.md")).unwrap();

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
		let issue = Issue::parse_virtual(content, Path::new("test.md")).unwrap();

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
		let mut issue = Issue::parse_virtual(content, Path::new("test.md")).unwrap();

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
		let issue = Issue::parse_virtual(content, Path::new("test.md")).unwrap();

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
		let issue = Issue::parse_virtual(content, Path::new("test.md")).unwrap();

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
		let issue = Issue::parse_virtual(content, Path::new("test.md")).unwrap();

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
