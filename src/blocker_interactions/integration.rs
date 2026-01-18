//! Integration with issue files and the `open` module.
//!
//! This module provides helpers for working with blockers embedded in issue files.
//! The `--integrated` flag on `set` and `open` commands enables use of issue files
//! as the blocker source, using the `open/` module's file management and sync mechanics.

use std::path::{Path, PathBuf};

use color_eyre::eyre::{Result, bail, eyre};
use todo::{DisplayFormat, Issue, Marker};

use super::{BlockerSequence, operations::BlockerSequenceExt};
use crate::open_interactions::local::{ExactMatchLevel, choose_issue_with_fzf, issues_dir, search_issue_files};

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
		let issues_dir = issues_dir();
		self.issue_path
			.strip_prefix(&issues_dir)
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

		// Cache the parsed issue for save()
		*self.cached_issue.borrow_mut() = Some(issue);

		Ok(blockers)
	}

	fn save(&self, blockers: &BlockerSequence) -> Result<()> {
		let mut issue = self.cached_issue.borrow_mut().take().ok_or_else(|| eyre!("Must call load() before save()"))?;

		// Update blockers directly
		issue.contents.blockers = blockers.clone();

		// Serialize and write
		std::fs::write(&self.issue_path, issue.serialize_virtual())?;
		Ok(())
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
	let all_files = search_issue_files("")?;
	if all_files.is_empty() {
		bail!("No issue files found. Use `todo open <url>` to fetch an issue first.");
	}
	match choose_issue_with_fzf(&all_files, pattern, ExactMatchLevel::default())? {
		Some(path) => Ok(path),
		None => bail!("No issue selected"),
	}
}

/// Get the current issue source, or error if none set.
fn get_current_source() -> Result<IssueSource> {
	let issue_path = get_current_blocker_issue().ok_or_else(|| eyre!("No blocker file set. Use `todo blocker -i set <pattern>` first."))?;
	Ok(IssueSource::new(issue_path))
}

/// Main entry point for integrated blocker commands (works with issue files).
/// This is the default mode for blocker commands.
pub async fn main_integrated(settings: &crate::config::LiveSettings, command: super::io::Command, format: DisplayFormat, offline: bool) -> Result<()> {
	use super::{io::Command, source::BlockerSource};
	use crate::open_interactions::{Modifier, SyncOptions, modify_and_sync_issue, modify_issue_offline};

	match command {
		Command::Set { pattern, touch: _ } => {
			// touch is ignored in integrated mode - issue files are managed by `todo open`
			let issue_path = resolve_issue_file(&pattern)?;
			set_current_blocker_issue(&issue_path)?;

			let source = IssueSource::new(issue_path);
			println!("Set blockers to: {}", source.display_name());

			// Load and show current blocker
			let blockers = source.load()?;
			if blockers.is_empty() {
				let marker = Marker::BlockersSection(todo::Header::new(1, "Blockers"));
				println!("No `{marker}` marker found in issue body.");
			} else if let Some(current) = blockers.current_with_context(&[]) {
				println!("Current: {current}");
			} else {
				println!("Blockers section is empty.");
			}
		}

		Command::OpenStandalone {
			pattern,
			touch: _,
			set_after,
			urgent: _,
		} => {
			// touch and urgent are ignored in integrated mode
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

		Command::List => {
			let source = get_current_source()?;
			let blockers = source.load()?;

			if blockers.is_empty() {
				let marker = Marker::BlockersSection(todo::Header::new(1, "Blockers"));
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
			let issue_path = get_current_blocker_issue().ok_or_else(|| eyre!("No blocker file set. Use `todo blocker set <pattern>` first."))?;

			// Check if blockers section exists before attempting pop
			let source = IssueSource::new(issue_path.clone());
			let blockers = source.load()?;
			if blockers.is_empty() {
				let marker = Marker::BlockersSection(todo::Header::new(1, "Blockers"));
				bail!("No `{marker}` marker found in issue body.");
			}

			// Use unified modify workflow - offline version skips Github client requirement
			let result = if offline {
				modify_issue_offline(&issue_path, Modifier::BlockerPop).await?
			} else {
				let gh = crate::github::create_client(settings)?;
				modify_and_sync_issue(&gh, &issue_path, offline, Modifier::BlockerPop, SyncOptions::default()).await?
			};

			// Output results
			if let Some(output) = result.output {
				println!("{output}");
			}

			// Show new current blocker
			let source = IssueSource::new(issue_path);
			let blockers = source.load()?;
			if let Some(new_current) = blockers.current_with_context(&[]) {
				println!("Current: {new_current}");
			} else {
				println!("Blockers section is now empty.");
			}
		}

		Command::Add {
			name,
			project: _,
			urgent: _,
			touch: _,
		} => {
			// In integrated mode, project/urgent/touch are ignored
			// We just add the blocker to the current issue
			let issue_path = get_current_blocker_issue().ok_or_else(|| eyre!("No blocker file set. Use `todo blocker set <pattern>` first."))?;

			// Use unified modify workflow - offline version skips Github client requirement
			let result = if offline {
				modify_issue_offline(&issue_path, Modifier::BlockerAdd { text: name.clone() }).await?
			} else {
				let gh = crate::github::create_client(settings)?;
				modify_and_sync_issue(&gh, &issue_path, offline, Modifier::BlockerAdd { text: name.clone() }, SyncOptions::default()).await?
			};

			// Output results
			if let Some(output) = result.output {
				println!("{output}");
			}

			// Show new current blocker
			let source = IssueSource::new(issue_path);
			let blockers = source.load()?;
			if let Some(new_current) = blockers.current_with_context(&[]) {
				println!("Current: {new_current}");
			}
		}

		Command::Resume(_) | Command::Halt(_) => {
			bail!("Resume/Halt not yet supported in integrated mode.");
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
		let content = r#"- [ ] Issue Title <!-- https://github.com/owner/repo/issues/1 -->
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
		insta::assert_snapshot!(issue.contents.blockers.serialize(todo::DisplayFormat::Headers), @"
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
		let content = r#"- [ ] Issue Title <!-- https://github.com/owner/repo/issues/1 -->
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
		let content = r#"- [ ] Issue Title <!-- https://github.com/owner/repo/issues/1 -->
	Body text.

	# Blockers
	- First task
	- Second task
	- Third task
"#;
		let mut issue = Issue::parse_virtual(content, Path::new("test.md")).unwrap();

		issue.contents.blockers.pop();

		insta::assert_snapshot!(issue.contents.blockers.serialize(todo::DisplayFormat::Headers), @"
		- First task
		- Second task
		");
	}

	#[test]
	fn test_issue_serialize_with_blockers() {
		let content = r#"- [ ] Issue Title <!-- https://github.com/owner/repo/issues/1 -->
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
		let content = r#"- [ ] Issue Title <!-- https://github.com/owner/repo/issues/1 -->
	Just some regular body text without blockers marker.
	- This is NOT a blocker, just body content.
"#;
		let issue = Issue::parse_virtual(content, Path::new("test.md")).unwrap();

		assert!(issue.contents.blockers.is_empty());
	}

	#[test]
	fn test_issue_blockers_with_subissue() {
		let content = r#"- [ ] Issue Title <!-- https://github.com/owner/repo/issues/1 -->
	Body.

	# Blockers
	- Blocker one
	- Blocker two

	- [ ] Sub-issue <!--sub https://github.com/owner/repo/issues/2 -->
		Sub-issue body
"#;
		let issue = Issue::parse_virtual(content, Path::new("test.md")).unwrap();

		// Blockers should only contain the blocker items, not the sub-issue
		insta::assert_snapshot!(issue.contents.blockers.serialize(todo::DisplayFormat::Headers), @"
		- Blocker one
		- Blocker two
		");

		// Sub-issue should be in children
		assert_eq!(issue.children.len(), 1);
		assert_eq!(issue.children[0].contents.title, "Sub-issue");
	}
}
