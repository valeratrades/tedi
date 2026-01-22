//! Local filesystem storage for issues.
//!
//! This module handles all local storage concerns.
//!
//! The local storage structure:
//! ```text
//! XDG_DATA_HOME/todo/issues/{owner}/{repo}/
//!   .meta.json                           # Project + per-issue metadata
//!   {number}_-_{title}.md                # Flat issue file
//!   {number}_-_{title}/                  # Directory format (when has children)
//!     __main__.md                        # The issue content
//!     {child_number}_-_{child_title}.md  # Child issues
//! ```
//!
//! All public access goes through methods on the `Local` type.

pub mod conflict;
pub mod consensus;

use std::path::{Path, PathBuf};

//==============================================================================
// Error Types
//==============================================================================

/// Error type for local issue loading operations.
#[derive(Debug, miette::Diagnostic, thiserror::Error, derive_more::From)]
pub enum LocalError {
	/// File not found at the expected path.
	#[error("issue file not found: {path}")]
	FileNotFound { path: PathBuf },

	/// Failed to read file contents.
	#[error("failed to read file: {path}")]
	ReadError {
		path: PathBuf,
		#[source]
		source: std::io::Error,
	},

	/// Failed to parse issue content.
	#[error("failed to parse issue file: {path}")]
	ParseError {
		path: PathBuf,
		#[source]
		source: Box<crate::ParseError>,
	},

	/// Git operation failed (for consensus reads).
	#[error("git operation failed: {message}")]
	GitError { message: String },

	/// Unresolved merge conflict blocks operation.
	#[error(transparent)]
	#[diagnostic(transparent)]
	ConflictBlocked(conflict::ConflictBlockedError),
}

/// Error type for consensus sink operations.
#[derive(Debug, miette::Diagnostic, derive_more::Display, thiserror::Error)]
pub enum ConsensusSinkError {
	#[display("failed to write issue files: {_0}")]
	#[diagnostic(code(tedi::consensus::write))]
	Write(color_eyre::Report),

	#[display("git add failed: {_0}")]
	#[diagnostic(code(tedi::consensus::git_add))]
	GitAdd(String),

	#[display("git status failed: {_0}")]
	#[diagnostic(code(tedi::consensus::git_status))]
	GitStatus(String),

	#[display("git commit failed: {_0}")]
	#[diagnostic(code(tedi::consensus::git_commit))]
	GitCommit(String),

	#[display("files rejected by .gitignore:\n{_0}")]
	#[diagnostic(code(tedi::consensus::gitignore), help("Check your .gitignore rules or remove the conflicting patterns"))]
	GitIgnoreRejection(String),

	#[display("invalid data directory path (not valid UTF-8)")]
	#[diagnostic(code(tedi::consensus::invalid_path))]
	InvalidDataDir,

	#[display("{_0}")]
	#[diagnostic(code(tedi::consensus::io))]
	Io(#[from] std::io::Error),
}

use regex::Regex;
use serde::{Deserialize, Serialize};
use v_utils::prelude::*;

use crate::{Ancestry, FetchedIssue, Issue, sink::Sink};

//==============================================================================
// Local - The interface for local issue storage
//==============================================================================

/// Source variant for local issue loading.
///
/// Determines where to read issue content from.
#[derive(Clone, Debug)]
pub enum LocalSource {
	/// Read from git's committed state (HEAD).
	/// This is the last agreed-upon synced state.
	Consensus,
	/// Read from the current filesystem state.
	/// "Submitted" because it represents what the user has effectively submitted
	/// to the local storage - accounts for offline edits or changes made outside
	/// this tool.
	Submitted,
}

/// Marker type for sinking to filesystem (submitted state).
pub struct Submitted;

/// Marker type for sinking to git (consensus state).
pub struct Consensus;

/// Source for loading issues from local storage.
#[derive(Clone, Debug, derive_more::Deref)]
pub struct LocalPath {
	#[deref]
	pub path: PathBuf,
	pub source: LocalSource,
}

impl LocalPath {
	/// Create a new local path for reading from filesystem (submitted state).
	pub fn submitted(path: PathBuf) -> Self {
		Self {
			path,
			source: LocalSource::Submitted,
		}
	}

	/// Create a new local path for reading from git (consensus state).
	pub fn consensus(path: PathBuf) -> Self {
		Self {
			path,
			source: LocalSource::Consensus,
		}
	}

	/// Create a child path with the same source variant.
	pub fn child(&self, child_path: PathBuf) -> Self {
		Self {
			path: child_path,
			source: self.source.clone(),
		}
	}
}

impl<P: Into<PathBuf>> From<P> for LocalPath {
	fn from(path: P) -> Self {
		Self::submitted(path.into())
	}
}

/// Type for local filesystem operations.
///
/// All custom logic for local issue storage is consolidated as methods on this type.
pub enum Local {}
impl Local {
	/// The filename used for the main issue file when it has a directory for sub-issues.
	pub const MAIN_ISSUE_FILENAME: &'static str = "__main__";

	/// Get the virtual edit path for an issue.
	///
	/// Returns a path in `/tmp/{CARGO_PKG_NAME}/{ancestry}/{fname}.md` where:
	/// - ancestry is `owner/repo/lineage[0]/lineage[1]/...` (lineage elements joined by `/`)
	/// - fname is the proper issue filename based on number and title
	///
	/// This path is used when opening an editor for the user to edit the issue.
	pub fn virtual_edit_path(issue: &crate::Issue) -> PathBuf {
		let ancestry = &issue.identity.ancestry;
		let mut path = PathBuf::from("/tmp").join(env!("CARGO_PKG_NAME"));

		// Add owner/repo
		path = path.join(ancestry.owner()).join(ancestry.repo());

		// Add lineage components (parent issue numbers)
		for &parent_num in ancestry.lineage() {
			path = path.join(parent_num.to_string());
		}

		// Add the issue filename
		let fname = Self::format_issue_filename(issue.number(), &issue.contents.title, false);
		path.join(fname)
	}

	/// Returns the base directory for issue storage: XDG_DATA_HOME/todo/issues/
	pub fn issues_dir() -> PathBuf {
		v_utils::xdg_data_dir!("issues")
	}

	/// Read file content from the specified source.
	///
	/// - `Submitted`: reads from filesystem
	/// - `Consensus`: reads from git HEAD
	///
	/// Returns `None` if file doesn't exist (for Submitted) or isn't tracked (for Consensus).
	pub fn read_content(local_path: &LocalPath) -> Option<String> {
		match local_path.source {
			LocalSource::Submitted => std::fs::read_to_string(&local_path.path).ok(),
			LocalSource::Consensus => Self::read_git_content(&local_path.path),
		}
	}

	/// Read file content from git HEAD.
	fn read_git_content(file_path: &Path) -> Option<String> {
		use std::process::Command;

		let data_dir = Self::issues_dir();
		let data_dir_str = data_dir.to_str()?;
		let rel_path = file_path.strip_prefix(&data_dir).ok()?;
		let rel_path_str = rel_path.to_str()?;

		// Check if git is initialized
		let git_check = Command::new("git").args(["-C", data_dir_str, "rev-parse", "--git-dir"]).output().ok()?;
		if !git_check.status.success() {
			return None;
		}

		// Check if file is tracked
		let ls_output = Command::new("git").args(["-C", data_dir_str, "ls-files", rel_path_str]).output().ok()?;
		if !ls_output.status.success() || ls_output.stdout.is_empty() {
			return None;
		}

		// Read from HEAD
		let output = Command::new("git").args(["-C", data_dir_str, "show", &format!("HEAD:./{rel_path_str}")]).output().ok()?;

		if !output.status.success() {
			return None;
		}

		String::from_utf8(output.stdout).ok()
	}

	/// List directory entries from the specified source.
	///
	/// - `Submitted`: lists from filesystem
	/// - `Consensus`: lists from git HEAD
	fn list_dir_entries(local_path: &LocalPath) -> Vec<String> {
		match local_path.source {
			LocalSource::Submitted => Self::list_fs_entries(&local_path.path),
			LocalSource::Consensus => Self::list_git_entries(&local_path.path),
		}
	}

	/// List directory entries from filesystem.
	fn list_fs_entries(dir: &Path) -> Vec<String> {
		let Ok(entries) = std::fs::read_dir(dir) else {
			return Vec::new();
		};

		entries.flatten().filter_map(|e| e.file_name().to_str().map(|s| s.to_string())).collect()
	}

	/// List directory entries from git HEAD.
	fn list_git_entries(dir: &Path) -> Vec<String> {
		use std::process::Command;

		let data_dir = Self::issues_dir();
		let Some(data_dir_str) = data_dir.to_str() else {
			return Vec::new();
		};
		let Some(rel_dir) = dir.strip_prefix(&data_dir).ok() else {
			return Vec::new();
		};
		let Some(rel_dir_str) = rel_dir.to_str() else {
			return Vec::new();
		};

		let output = Command::new("git")
			.args(["-C", data_dir_str, "ls-tree", "--name-only", "HEAD", &format!("{rel_dir_str}/")])
			.output();

		let Ok(output) = output else {
			return Vec::new();
		};
		if !output.status.success() {
			return Vec::new();
		}

		let prefix = format!("{rel_dir_str}/");
		std::str::from_utf8(&output.stdout)
			.unwrap_or("")
			.lines()
			.filter_map(|line| line.strip_prefix(&prefix))
			.map(|s| s.to_string())
			.collect()
	}

	/// Check if a path is a directory in the specified source.
	fn is_dir(local_path: &LocalPath) -> bool {
		match local_path.source {
			LocalSource::Submitted => local_path.path.is_dir(),
			LocalSource::Consensus => Self::is_git_dir(&local_path.path),
		}
	}

	/// Check if a path is a directory in git.
	fn is_git_dir(dir: &Path) -> bool {
		use std::process::Command;

		let data_dir = Self::issues_dir();
		let Some(data_dir_str) = data_dir.to_str() else {
			return false;
		};
		let Some(rel_dir) = dir.strip_prefix(&data_dir).ok() else {
			return false;
		};
		let Some(rel_dir_str) = rel_dir.to_str() else {
			return false;
		};

		let check = Command::new("git").args(["-C", data_dir_str, "ls-tree", "HEAD", &format!("{rel_dir_str}/")]).output();
		check.map(|o| o.status.success() && !o.stdout.is_empty()).unwrap_or(false)
	}

	/// Check if a path exists in the specified source.
	fn path_exists(local_path: &LocalPath) -> bool {
		match local_path.source {
			LocalSource::Submitted => local_path.path.exists(),
			LocalSource::Consensus => Self::read_git_content(&local_path.path).is_some() || Self::is_git_dir(&local_path.path),
		}
	}

	/// Parse a single issue node from filesystem content.
	///
	/// This parses one issue file (without loading children from separate files).
	/// Children field will be empty - they're loaded separately via LazyIssue.
	fn parse_single_node(content: &str, ancestry: Ancestry, file_path: &Path) -> Result<Issue, LocalError> {
		let mut issue = Issue::parse_virtual_with_ancestry(content, file_path, ancestry).map_err(|e| LocalError::ParseError {
			path: file_path.to_path_buf(),
			source: Box::new(e),
		})?;
		// Clear any inline children (filesystem format stores them in separate files)
		issue.children.clear();
		Ok(issue)
	}

	/// Get the project directory path (where .meta.json lives).
	/// Structure: issues/{owner}/{repo}/
	pub fn project_dir(owner: &str, repo: &str) -> PathBuf {
		Self::issues_dir().join(owner).join(repo)
	}

	/// Sanitize a title for use in filenames.
	/// Converts spaces to underscores and removes special characters.
	pub fn sanitize_title(title: &str) -> String {
		title
			.chars()
			.map(|c| {
				if c.is_alphanumeric() || c == '-' || c == '_' {
					c
				} else if c == ' ' {
					'_'
				} else {
					'\0'
				}
			})
			.filter(|&c| c != '\0')
			.collect::<String>()
			.trim_matches('_')
			.to_string()
	}

	/// Format an issue filename from number and title.
	/// Format: {number}_-_{sanitized_title}.md or just {sanitized_title}.md if no number
	/// Adds .bak suffix for closed issues.
	fn format_issue_filename(issue_number: Option<u64>, title: &str, closed: bool) -> String {
		let sanitized = Self::sanitize_title(title);
		let base = match issue_number {
			Some(num) if sanitized.is_empty() => format!("{num}.md"),
			Some(num) => format!("{num}_-_{sanitized}.md"),
			None if sanitized.is_empty() => "untitled.md".to_string(),
			None => format!("{sanitized}.md"),
		};
		if closed { format!("{base}.bak") } else { base }
	}

	/// Format a FetchedIssue into a directory name: `{number}_-_{sanitized_title}`
	fn format_issue_dir_name(issue: &FetchedIssue) -> String {
		format!("{}_-_{}", issue.number(), Self::sanitize_title(&issue.title))
	}

	/// Get the directory name for an issue (used when it has sub-issues).
	/// Format: {number}_-_{sanitized_title}
	pub fn issue_dir_name(issue_number: Option<u64>, title: &str) -> String {
		let sanitized = Self::sanitize_title(title);
		match issue_number {
			Some(num) if sanitized.is_empty() => format!("{num}"),
			Some(num) => format!("{num}_-_{sanitized}"),
			None if sanitized.is_empty() => "untitled".to_string(),
			None => sanitized,
		}
	}

	/// Get the path for an issue file from an Issue.
	/// Uses the issue's ancestry to resolve the path.
	///
	/// # Errors
	/// Returns an error if ancestor directories can't be resolved from the ancestry.
	pub fn issue_file_path(issue: &Issue) -> Result<PathBuf> {
		let ancestry = &issue.identity.ancestry;
		let ancestor_dir_names = Self::build_ancestor_dir_names(ancestry)?;
		let closed = issue.contents.state.is_closed();
		Ok(Self::issue_file_path_from_dir_names(
			ancestry.owner(),
			ancestry.repo(),
			issue.number(),
			&issue.contents.title,
			closed,
			&ancestor_dir_names,
		))
	}

	/// Get the path for an issue file using ancestor directory names directly.
	#[deprecated]
	pub fn issue_file_path_from_dir_names(owner: &str, repo: &str, issue_number: Option<u64>, title: &str, closed: bool, ancestor_dir_names: &[String]) -> PathBuf {
		let mut path = Self::project_dir(owner, repo);

		for dir_name in ancestor_dir_names {
			path = path.join(dir_name);
		}

		let filename = Self::format_issue_filename(issue_number, title, closed);
		path.join(filename)
	}

	/// Get the path to the issue directory (where sub-issues are stored).
	pub fn issue_dir_path(issue_number: Option<u64>, title: &str, ancestry: Ancestry) -> PathBuf {
		let ancestor_dir_names: Vec<_> = ancestors.iter().map(|a| Self::format_issue_dir_name(a)).collect();
		Self::issue_dir_path_from_dir_names(owner, repo, issue_number, title, &ancestor_dir_names)
	}

	/// Get the path to the issue directory using ancestor directory names directly.
	pub fn issue_dir_path_from_dir_names(owner: &str, repo: &str, issue_number: Option<u64>, title: &str, ancestor_dir_names: &[String]) -> PathBuf {
		let mut path = Self::project_dir(owner, repo);

		for dir_name in ancestor_dir_names {
			path = path.join(dir_name);
		}

		path.join(Self::issue_dir_name(issue_number, title))
	}

	/// Get the path for the main issue file when stored inside a directory.
	/// Format: {dir}/__main__.md[.bak]
	pub fn main_file_path(issue_dir: &Path, closed: bool) -> PathBuf {
		let filename = if closed {
			format!("{}.md.bak", Self::MAIN_ISSUE_FILENAME)
		} else {
			format!("{}.md", Self::MAIN_ISSUE_FILENAME)
		};
		issue_dir.join(filename)
	}

	/// Find the actual file path for an issue, checking both flat and directory formats.
	pub fn find_issue_file(owner: &str, repo: &str, issue_number: Option<u64>, title: &str, ancestor_dir_names: &[String]) -> Option<PathBuf> {
		// Try flat format first (both open and closed)
		let flat_path = Self::issue_file_path_from_dir_names(owner, repo, issue_number, title, false, ancestor_dir_names);
		if flat_path.exists() {
			return Some(flat_path);
		}

		let flat_closed_path = Self::issue_file_path_from_dir_names(owner, repo, issue_number, title, true, ancestor_dir_names);
		if flat_closed_path.exists() {
			return Some(flat_closed_path);
		}

		// Try directory format
		let issue_dir = Self::issue_dir_path_from_dir_names(owner, repo, issue_number, title, ancestor_dir_names);
		if issue_dir.is_dir() {
			let main_path = Self::main_file_path(&issue_dir, false);
			if main_path.exists() {
				return Some(main_path);
			}

			let main_closed_path = Self::main_file_path(&issue_dir, true);
			if main_closed_path.exists() {
				return Some(main_closed_path);
			}
		}

		None
	}

	/// Search for issue files matching a pattern.
	//TODO: @v: check if we can deprecate this in favor of always using choose_issue_with_fzf
	pub fn search_issue_files(pattern: &str) -> Result<Vec<PathBuf>> {
		let issues_base = Self::issues_dir();
		if !issues_base.exists() {
			return Ok(vec![]);
		}

		let mut matches = Vec::new();
		let pattern_lower = pattern.to_lowercase();

		fn walk_dir(dir: &Path, pattern: &str, matches: &mut Vec<PathBuf>) -> std::io::Result<()> {
			for entry in std::fs::read_dir(dir)? {
				let entry = entry?;
				let path = entry.path();

				if path.is_dir() {
					walk_dir(&path, pattern, matches)?;
				} else if path.is_file()
					&& let Some(name) = path.file_name().and_then(|n| n.to_str())
					&& (name.ends_with(".md") || name.ends_with(".md.bak"))
				{
					let name_lower = name.to_lowercase();
					let path_str = path.to_string_lossy().to_lowercase();
					if pattern.is_empty() || name_lower.contains(pattern) || path_str.contains(pattern) {
						matches.push(path);
					}
				}
			}
			Ok(())
		}

		walk_dir(&issues_base, &pattern_lower, &mut matches)?;

		matches.sort_by(|a, b| {
			let a_time = std::fs::metadata(a).and_then(|m| m.modified()).ok();
			let b_time = std::fs::metadata(b).and_then(|m| m.modified()).ok();
			b_time.cmp(&a_time)
		});

		Ok(matches)
	}

	/// Build ancestor directory names by traversing the filesystem for an ancestry.
	/// Finds issues by number (either flat file or directory) and returns the directory names.
	pub fn build_ancestor_dir_names(ancestry: &Ancestry) -> Result<Vec<String>> {
		let mut path = Self::project_dir(ancestry.owner(), ancestry.repo());

		if !path.exists() {
			bail!("Project directory does not exist: {}", path.display());
		}

		let mut result = Vec::with_capacity(ancestry.lineage().len());

		for &issue_number in ancestry.lineage() {
			let dir_name = Self::find_issue_dir_name_by_number(&path, issue_number)
				.ok_or_else(|| eyre!("Parent issue #{issue_number} not found locally in {}. Fetch the parent issue first.", path.display()))?;

			path = path.join(&dir_name);
			result.push(dir_name);
		}

		Ok(result)
	}

	#[allow(deprecated)]
	/// Build a chain of FetchedIssue by traversing the filesystem for an ancestry.
	#[deprecated(note = "Use build_ancestor_dir_names instead")]
	pub fn build_ancestry_path(ancestry: &Ancestry) -> Result<Vec<FetchedIssue>> {
		let mut path = Self::project_dir(ancestry.owner(), ancestry.repo());

		if !path.exists() {
			bail!("Project directory does not exist: {}", path.display());
		}

		let mut result = Vec::with_capacity(ancestry.lineage().len());

		for &issue_number in ancestry.lineage() {
			let dir = Self::find_issue_dir_by_number(&path, issue_number)
				.ok_or_else(|| eyre!("Parent issue #{issue_number} not found locally in {}. Fetch the parent issue first.", path.display()))?;

			let dir_name = dir.file_name().and_then(|n| n.to_str()).unwrap_or("");
			let title = Self::extract_title_from_dir_name(dir_name, issue_number);

			let fetched = FetchedIssue::from_parts(ancestry.owner(), ancestry.repo(), issue_number, &title).ok_or_else(|| eyre!("Failed to construct FetchedIssue for #{issue_number}"))?;
			result.push(fetched);

			path = dir;
		}

		Ok(result)
	}

	/// Find the issue file for an ancestry.
	/// Navigates the filesystem using issue numbers in the lineage.
	pub fn find_issue_file_by_ancestry(ancestry: &Ancestry) -> Option<PathBuf> {
		let mut path = Self::project_dir(ancestry.owner(), ancestry.repo());

		if !path.exists() {
			return None;
		}

		let lineage = ancestry.lineage();
		if lineage.is_empty() {
			return None;
		}

		// Navigate to parent directories
		for &issue_number in &lineage[..lineage.len() - 1] {
			let dir = Self::find_issue_dir_by_number(&path, issue_number)?;
			path = dir;
		}

		// Find the target issue (last in lineage)
		let target_number = *lineage.last().unwrap();

		// Check for directory format first (issue with children)
		if let Some(dir) = Self::find_issue_dir_by_number(&path, target_number) {
			let main_file = Self::main_file_path(&dir, false);
			if main_file.exists() {
				return Some(main_file);
			}
			let main_bak = Self::main_file_path(&dir, true);
			if main_bak.exists() {
				return Some(main_bak);
			}
		}

		// Check for flat file format
		let entries = std::fs::read_dir(&path).ok()?;
		let prefix_with_sep = format!("{target_number}_-_");
		let exact_match = format!("{target_number}.md");
		let exact_match_bak = format!("{target_number}.md.bak");

		for entry in entries.flatten() {
			let entry_path = entry.path();
			if !entry_path.is_file() {
				continue;
			}

			let Some(name) = entry_path.file_name().and_then(|n| n.to_str()) else {
				continue;
			};

			if name.starts_with(&prefix_with_sep) && (name.ends_with(".md") || name.ends_with(".md.bak")) {
				return Some(entry_path);
			}
			if name == exact_match || name == exact_match_bak {
				return Some(entry_path);
			}
		}

		None
	}

	/// Find an issue directory by its number prefix.
	fn find_issue_dir_by_number(parent: &Path, issue_number: u64) -> Option<PathBuf> {
		let entries = std::fs::read_dir(parent).ok()?;

		let prefix_with_sep = format!("{issue_number}_-_");
		let exact_match = format!("{issue_number}");

		for entry in entries.flatten() {
			let path = entry.path();
			if !path.is_dir() {
				continue;
			}

			let Some(name) = path.file_name().and_then(|n| n.to_str()) else {
				continue;
			};

			if name.starts_with(&prefix_with_sep) || name == exact_match {
				return Some(path);
			}
		}

		None
	}

	/// Find an issue by number (flat file or directory) and return the directory name it would use.
	/// For flat file `99_-_title.md` returns `99_-_title`.
	/// For directory `99_-_title/` returns `99_-_title`.
	fn find_issue_dir_name_by_number(parent: &Path, issue_number: u64) -> Option<String> {
		let entries = std::fs::read_dir(parent).ok()?;

		let prefix_with_sep = format!("{issue_number}_-_");
		let exact_match_dir = format!("{issue_number}");

		for entry in entries.flatten() {
			let path = entry.path();
			let Some(name) = path.file_name().and_then(|n| n.to_str()) else {
				continue;
			};

			if path.is_dir() {
				if name.starts_with(&prefix_with_sep) || name == exact_match_dir {
					return Some(name.to_string());
				}
			} else if path.is_file() {
				// Flat file: strip .md or .md.bak extension to get dir name
				// Use if-let instead of ? to avoid early return on non-.md files
				if let Some(base) = name.strip_suffix(".md.bak").or_else(|| name.strip_suffix(".md")) {
					if base.starts_with(&prefix_with_sep) || base == exact_match_dir {
						return Some(base.to_string());
					}
				}
			}
		}

		None
	}

	/// Extract title from directory name.
	fn extract_title_from_dir_name(dir_name: &str, issue_number: u64) -> String {
		let prefix = format!("{issue_number}_-_");
		if let Some(title) = dir_name.strip_prefix(&prefix) {
			title.replace('_', " ")
		} else {
			String::new()
		}
	}

	/// Extract full ancestry (owner/repo/lineage) from an issue file path.
	///
	/// Path structure: `issues/{owner}/{repo}/{number}_-_{title}/.../file.md`
	/// The lineage contains PARENT issue numbers from the path components between repo and the target file.
	///
	/// For directory format (`__main__.md`), the immediately containing directory is the issue itself,
	/// not a parent, so it's excluded from lineage.
	pub fn extract_ancestry(path: &Path) -> Result<Ancestry> {
		let issues_base = Self::issues_dir();

		let rel_path = path.strip_prefix(&issues_base).map_err(|_| eyre!("Issue file is not in issues directory: {path:?}"))?;

		let components: Vec<_> = rel_path.components().collect();
		if components.len() < 2 {
			bail!("Path too short to extract owner/repo: {path:?}");
		}

		let owner = components[0].as_os_str().to_str().ok_or_else(|| eyre!("Could not extract owner from path: {path:?}"))?;
		let repo = components[1].as_os_str().to_str().ok_or_else(|| eyre!("Could not extract repo from path: {path:?}"))?;

		// Check if this is a directory format file (__main__.md or __main__.md.bak)
		let filename = components.last().and_then(|c| c.as_os_str().to_str()).unwrap_or("");
		let is_dir_format = filename.starts_with(Self::MAIN_ISSUE_FILENAME);

		// Everything between repo and the final component is a potential parent issue directory
		// Format: {number}_-_{title} or just {number}
		// For directory format, exclude the last directory (that's the issue itself, not a parent)
		let mut lineage = Vec::new();
		let end_offset = if is_dir_format { 2 } else { 1 }; // Skip filename + issue dir for dir format
		for component in &components[2..components.len().saturating_sub(end_offset)] {
			let name = component.as_os_str().to_str().ok_or_else(|| eyre!("Invalid path component: {component:?}"))?;

			// Skip if this looks like a file (has .md extension)
			if name.ends_with(".md") || name.ends_with(".md.bak") {
				continue;
			}

			// Extract issue number from directory name
			let issue_number = if let Some(sep_pos) = name.find("_-_") {
				name[..sep_pos].parse::<u64>().map_err(|_| eyre!("Invalid issue directory format: {name}"))?
			} else {
				name.parse::<u64>().map_err(|_| eyre!("Invalid issue directory format: {name}"))?
			};
			lineage.push(issue_number);
		}

		Ok(Ancestry::with_lineage(owner, repo, &lineage))
	}

	/// Extract owner/repo from an issue file path.
	#[deprecated(note = "Use extract_ancestry instead for full lineage information")]
	pub fn extract_owner_repo(path: &Path) -> Result<(String, String)> {
		let ancestry = Self::extract_ancestry(path)?;
		Ok((ancestry.owner().to_string(), ancestry.repo().to_string()))
	}

	/// Choose an issue file using fzf.
	pub fn choose_issue_with_fzf(files: &[PathBuf], initial_query: &str, exact: ExactMatchLevel) -> Result<Option<PathBuf>> {
		use std::{
			io::Write,
			process::{Command, Stdio},
		};

		let issues_base = Self::issues_dir();

		let file_list: Vec<String> = files
			.iter()
			.filter_map(|p| p.strip_prefix(&issues_base).ok().map(|rel| rel.to_string_lossy().to_string()))
			.collect();

		let (filtered_list, fzf_query): (Vec<&String>, String) = match exact {
			ExactMatchLevel::Fuzzy | ExactMatchLevel::ExactTerms => (file_list.iter().collect(), initial_query.to_string()),
			ExactMatchLevel::RegexSubstring =>
				if initial_query.is_empty() {
					(file_list.iter().collect(), String::new())
				} else {
					let re = Regex::new(initial_query).map_err(|e| eyre!("Invalid regex pattern: {e}"))?;
					let filtered: Vec<&String> = file_list.iter().filter(|f| re.is_match(f)).collect();
					(filtered, String::new())
				},
			ExactMatchLevel::RegexLine =>
				if initial_query.is_empty() {
					(file_list.iter().collect(), String::new())
				} else {
					let pattern = {
						let has_start = initial_query.starts_with('^');
						let has_end = initial_query.ends_with('$');
						match (has_start, has_end) {
							(true, true) => initial_query.to_string(),
							(true, false) => format!("{initial_query}$"),
							(false, true) => format!("^{initial_query}"),
							(false, false) => format!("^{initial_query}$"),
						}
					};
					let re = Regex::new(&pattern).map_err(|e| eyre!("Invalid regex pattern: {e}"))?;
					let filtered: Vec<&String> = file_list.iter().filter(|f| re.is_match(f)).collect();
					(filtered, String::new())
				},
		};

		let mut cmd = Command::new("fzf");

		cmd.arg("--query").arg(&fzf_query);

		if matches!(exact, ExactMatchLevel::ExactTerms) {
			cmd.arg("--exact");
		}

		cmd.arg("--select-1")
			.arg("--preview")
			.arg("cat {}")
			.arg("--preview-window")
			.arg("right:50%:wrap")
			.current_dir(&issues_base)
			.stdin(Stdio::piped())
			.stdout(Stdio::piped());

		let mut child = cmd.spawn()?;

		if let Some(stdin) = child.stdin.as_mut() {
			for file in &filtered_list {
				writeln!(stdin, "{file}")?;
			}
		}

		let output = child.wait_with_output()?;

		if output.status.success() {
			let selected = String::from_utf8_lossy(&output.stdout).trim().to_string();
			if !selected.is_empty() {
				return Ok(Some(issues_base.join(selected)));
			}
		}

		Ok(None)
	}

	/// Parse issue identity from a file path.
	pub fn parse_path_identity(issue_file_path: &Path) -> Result<PathIdentity> {
		let _ancestry = Self::extract_ancestry(issue_file_path)?;

		let filename = issue_file_path.file_name().and_then(|n| n.to_str()).ok_or_else(|| eyre!("Invalid issue file path"))?;

		let filename_no_bak = filename.strip_suffix(".bak").unwrap_or(filename);

		let (name_to_parse, parent_dir) = if filename_no_bak.starts_with(Self::MAIN_ISSUE_FILENAME) {
			let parent_dir = issue_file_path.parent();
			let name = parent_dir
				.and_then(|p| p.file_name())
				.and_then(|n| n.to_str())
				.ok_or_else(|| eyre!("Could not extract parent directory for __main__ file"))?;
			(name, parent_dir)
		} else {
			let name = filename_no_bak.strip_suffix(".md").unwrap_or(filename_no_bak);
			(name, issue_file_path.parent())
		};

		let (issue_number, title) = if let Some(sep_pos) = name_to_parse.find("_-_") {
			let number: u64 = name_to_parse[..sep_pos].parse()?;
			let title = name_to_parse[sep_pos + 3..].replace('_', " ");
			(number, title)
		} else if let Ok(number) = name_to_parse.parse::<u64>() {
			(number, String::new())
		} else {
			let title = name_to_parse.replace('_', " ");
			(0, title)
		};

		let parent_issue = if let Some(parent) = parent_dir {
			let check_dir = if filename_no_bak.starts_with(Self::MAIN_ISSUE_FILENAME) {
				parent.parent()
			} else {
				Some(parent)
			};

			if let Some(dir) = check_dir {
				let issues_base = Self::issues_dir();
				if let Ok(rel) = dir.strip_prefix(&issues_base) {
					let components: Vec<_> = rel.components().collect();
					if components.len() > 2 {
						if let Some(parent_dir_name) = components.last().and_then(|c| c.as_os_str().to_str()) {
							if let Some(sep_pos) = parent_dir_name.find("_-_") {
								parent_dir_name[..sep_pos].parse::<u64>().ok()
							} else {
								parent_dir_name.parse::<u64>().ok()
							}
						} else {
							None
						}
					} else {
						None
					}
				} else {
					None
				}
			} else {
				None
			}
		} else {
			None
		};

		Ok(PathIdentity { issue_number, title, parent_issue })
	}

	/// Check if a project is virtual (has no Github remote)
	pub fn is_virtual_project(repo_info: crate::RepoInfo) -> bool {
		Self::load_project_meta(repo_info.owner(), repo_info.repo()).virtual_project //TODO: update all occurences of `owner, repo` to just `repo_info` everywhere
	}

	/// Ensure a virtual project exists (creates if needed).
	pub fn ensure_virtual_project(owner: &str, repo: &str) -> Result<ProjectMeta> {
		let meta_path = Self::project_meta_path(owner, repo);
		if meta_path.exists() {
			let project_meta = Self::load_project_meta(owner, repo);
			if !project_meta.virtual_project {
				bail!("Project {owner}/{repo} exists but is not a virtual project");
			}
			Ok(project_meta)
		} else {
			let project_meta = ProjectMeta {
				virtual_project: true,
				next_virtual_issue_number: 1,
				issues: std::collections::BTreeMap::new(),
			};
			Self::save_project_meta(owner, repo, &project_meta)?;
			Ok(project_meta)
		}
	}

	/// Allocate the next issue number for a virtual project.
	pub fn allocate_virtual_issue_number(owner: &str, repo: &str) -> Result<u64> {
		let mut project_meta = Self::load_project_meta(owner, repo);
		if !project_meta.virtual_project {
			bail!("Cannot allocate virtual issue number for non-virtual project {owner}/{repo}");
		}

		if project_meta.next_virtual_issue_number == 0 {
			project_meta.next_virtual_issue_number = 1;
		}

		let issue_number = project_meta.next_virtual_issue_number;
		project_meta.next_virtual_issue_number += 1;
		Self::save_project_meta(owner, repo, &project_meta)?;

		Ok(issue_number)
	}

	/// Get the metadata file path for a project
	fn project_meta_path(owner: &str, repo: &str) -> PathBuf {
		Self::project_dir(owner, repo).join(".meta.json")
	}

	/// Load project metadata, creating empty if not exists
	pub fn load_project_meta(owner: &str, repo: &str) -> ProjectMeta {
		Self::load_project_meta_from_source(owner, repo, LocalSource::Submitted)
	}

	/// Load project metadata from a specific source (filesystem or git).
	fn load_project_meta_from_source(owner: &str, repo: &str, source: LocalSource) -> ProjectMeta {
		let meta_path = Self::project_meta_path(owner, repo);
		let content = match source {
			LocalSource::Submitted => std::fs::read_to_string(&meta_path).ok(),
			LocalSource::Consensus => Self::read_git_content(&meta_path),
		};

		match content {
			Some(c) => match serde_json::from_str(&c) {
				Ok(meta) => meta,
				Err(e) => panic!("corrupted project metadata at {}: {e}", meta_path.display()),
			},
			None => ProjectMeta::default(),
		}
	}

	/// Save project metadata
	fn save_project_meta(owner: &str, repo: &str, meta: &ProjectMeta) -> Result<()> {
		let meta_path = Self::project_meta_path(owner, repo);
		if let Some(parent) = meta_path.parent() {
			std::fs::create_dir_all(parent)?;
		}
		let content = serde_json::to_string_pretty(meta)?;
		std::fs::write(&meta_path, content)?;
		Ok(())
	}

	/// Load metadata for a specific issue from a specific source.
	fn load_issue_meta_from_source(owner: &str, repo: &str, issue_number: u64, source: LocalSource) -> Option<IssueMeta> {
		let project_meta = Self::load_project_meta_from_source(owner, repo, source);
		project_meta.issues.get(&issue_number).cloned()
	}

	/// Save metadata for a specific issue to the project's .meta.json.
	fn save_issue_meta(owner: &str, repo: &str, issue_number: u64, meta: &IssueMeta) -> Result<()> {
		let mut project_meta = Self::load_project_meta(owner, repo);
		project_meta.issues.insert(issue_number, meta.clone());
		Self::save_project_meta(owner, repo, &project_meta)
	}

	/// Remove metadata for a specific issue from the project's .meta.json.
	fn remove_issue_meta(owner: &str, repo: &str, issue_number: u64) -> Result<()> {
		let mut project_meta = Self::load_project_meta(owner, repo);
		if project_meta.issues.remove(&issue_number).is_some() {
			Self::save_project_meta(owner, repo, &project_meta)?;
		}
		Ok(())
	}
}

//NB: the reason we expose methods through Local, - is to shortcut the possibility of having them appear without clear reference to what part of logic owns them.
// Note that conventionally, same effect is achieved by deciding to not export methods out of a module, but use them as `mod::method`, which would translate to almost tht exactly the same eg `local::issue_dir()`
// Difference is: when using AI, I can't control how the methods are exported. This tag solves it.

//==============================================================================
// Types
//==============================================================================

/// Exact match level for fzf queries.
#[derive(Clone, Copy, Debug, Default)]
pub enum ExactMatchLevel {
	#[default]
	Fuzzy,
	ExactTerms,
	RegexSubstring,
	RegexLine,
}

impl TryFrom<u8> for ExactMatchLevel {
	type Error = &'static str;

	fn try_from(count: u8) -> Result<Self, Self::Error> {
		match count {
			0 => Ok(Self::Fuzzy),
			1 => Ok(Self::ExactTerms),
			2 => Ok(Self::RegexSubstring),
			3 => Ok(Self::RegexLine),
			_ => Err("--exact / -e can be specified at most 3 times"),
		}
	}
}

/// Issue identity extracted from a file path.
#[derive(Clone, Debug)]
pub struct PathIdentity {
	pub issue_number: u64,
	pub title: String,
	pub parent_issue: Option<u64>,
}

/// Project-level metadata file.
#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct ProjectMeta {
	#[serde(default)]
	pub virtual_project: bool,
	#[serde(default)]
	pub next_virtual_issue_number: u64,
	#[serde(default, skip_serializing_if = "std::collections::BTreeMap::is_empty")]
	pub issues: std::collections::BTreeMap<u64, IssueMeta>,
}

/// Per-issue metadata stored in .meta.json.
#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct IssueMeta {
	/// Timestamps for individual field changes.
	#[serde(default)]
	pub timestamps: crate::IssueTimestamps,
}

//==============================================================================
// LazyIssue Implementation
//==============================================================================

impl crate::LazyIssue<Local> for Issue {
	type Error = LocalError;
	type Source = LocalPath;

	async fn ancestry(source: &Self::Source) -> Result<crate::Ancestry, Self::Error> {
		Local::extract_ancestry(&source.path).map_err(|_| LocalError::FileNotFound { path: source.path.clone() })
	}

	async fn identity(&mut self, source: Self::Source) -> Result<crate::IssueIdentity, Self::Error> {
		if self.identity.is_linked() {
			return Ok(self.identity.clone());
		}

		let ancestry = Local::extract_ancestry(&source.path).map_err(|_| LocalError::FileNotFound { path: source.path.clone() })?;

		if self.contents.title.is_empty() {
			let content = Local::read_content(&source).ok_or_else(|| LocalError::FileNotFound { path: source.path.clone() })?;
			let parsed = Local::parse_single_node(&content, ancestry, &source.path)?;
			self.identity = parsed.identity;
			self.contents = parsed.contents;
		}

		if let Some(issue_number) = self.identity.number()
			&& let Some(meta) = Local::load_issue_meta_from_source(ancestry.owner(), ancestry.repo(), issue_number, source.source.clone())
			&& let Some(linked) = self.identity.remote.as_linked_mut()
		{
			linked.timestamps = meta.timestamps;
		}

		Ok(self.identity.clone())
	}

	async fn contents(&mut self, source: Self::Source) -> Result<crate::IssueContents, Self::Error> {
		if !self.contents.title.is_empty() {
			return Ok(self.contents.clone());
		}

		let ancestry = Local::extract_ancestry(&source.path).map_err(|_| LocalError::FileNotFound { path: source.path.clone() })?;

		let content = Local::read_content(&source).ok_or_else(|| LocalError::FileNotFound { path: source.path.clone() })?;
		let parsed = Local::parse_single_node(&content, ancestry, &source.path)?;
		self.identity = parsed.identity;
		self.contents = parsed.contents;

		Ok(self.contents.clone())
	}

	async fn children(&mut self, source: Self::Source) -> Result<Vec<Issue>, Self::Error> {
		if !self.children.is_empty() {
			return Ok(self.children.clone());
		}

		let is_dir_format = source
			.path
			.file_name()
			.and_then(|n| n.to_str())
			.map(|n| n.starts_with(Local::MAIN_ISSUE_FILENAME))
			.unwrap_or(false);

		if !is_dir_format {
			return Ok(Vec::new());
		}

		let Some(dir) = source.path.parent() else {
			return Ok(Vec::new());
		};

		let dir_source = LocalPath {
			path: dir.to_path_buf(),
			source: source.source.clone(),
		};
		let entries = Local::list_dir_entries(&dir_source);

		let mut children = Vec::new();

		for name in entries {
			if name.starts_with(Local::MAIN_ISSUE_FILENAME) {
				continue;
			}

			let entry_path = dir.join(&name);
			let entry_source = source.child(entry_path.clone());

			// Check if it's a file or directory
			let is_file = name.ends_with(".md") || name.ends_with(".md.bak");
			let is_directory = Local::is_dir(&entry_source);

			let child_source = if is_file {
				entry_source
			} else if is_directory {
				let main_path = Local::main_file_path(&entry_path, false);
				let main_closed_path = Local::main_file_path(&entry_path, true);

				let main_source = source.child(main_path);
				let main_closed_source = source.child(main_closed_path);

				if Local::path_exists(&main_source) {
					main_source
				} else if Local::path_exists(&main_closed_source) {
					main_closed_source
				} else {
					continue;
				}
			} else {
				continue;
			};

			let child = <Issue as crate::LazyIssue<Local>>::load(child_source).await?;
			children.push(child);
		}

		children.sort_by(|a, b| {
			let a_num = a.number().unwrap_or(0);
			let b_num = b.number().unwrap_or(0);
			a_num.cmp(&b_num)
		});

		self.children = children.clone();
		Ok(children)
	}

	async fn load(source: Self::Source) -> Result<Issue, Self::Error> {
		// Check for unresolved conflicts (only for Submitted, and only for root loads)
		if matches!(source.source, LocalSource::Submitted) {
			let ancestry = Local::extract_ancestry(&source.path).map_err(|_| LocalError::FileNotFound { path: source.path.clone() })?;
			conflict::check_conflict(ancestry.owner())?;
		}

		let ancestry = <Self as crate::LazyIssue<Local>>::ancestry(&source).await?;
		let mut issue = Issue::empty_local(ancestry);
		<Self as crate::LazyIssue<Local>>::identity(&mut issue, source.clone()).await?;
		<Self as crate::LazyIssue<Local>>::contents(&mut issue, source.clone()).await?;
		Box::pin(<Self as crate::LazyIssue<Local>>::children(&mut issue, source)).await?;
		Ok(issue)
	}
}

//==============================================================================
// Sink Implementation
//==============================================================================

//TODO: @claude: create proper error type for Submitted sink (see ConsensusSinkError for reference)
impl Sink<Submitted> for Issue {
	type Error = color_eyre::Report;

	async fn sink(&mut self, old: Option<&Issue>) -> Result<bool, Self::Error> {
		let owner = self.identity.owner();
		let repo = self.identity.repo();

		sink_issue_node(self, old, owner, repo, &[])
	}
}

impl Sink<Consensus> for Issue {
	type Error = ConsensusSinkError;

	async fn sink(&mut self, old: Option<&Issue>) -> Result<bool, Self::Error> {
		use std::process::Command;

		let owner = self.identity.owner();
		let repo = self.identity.repo();

		// Write files to filesystem (same as Submitted)
		let any_written = sink_issue_node(self, old, owner, repo, &[]).map_err(ConsensusSinkError::Write)?;

		if !any_written {
			return Ok(false);
		}

		// Stage and commit to git
		let data_dir = Local::issues_dir();
		let data_dir_str = data_dir.to_str().ok_or(ConsensusSinkError::InvalidDataDir)?;

		// Stage all changes
		let add_output = Command::new("git").args(["-C", data_dir_str, "add", "-A"]).output()?;
		if !add_output.status.success() {
			return Err(ConsensusSinkError::GitAdd(String::from_utf8_lossy(&add_output.stderr).into_owned()));
		}

		// Check if any files were ignored (would indicate .gitignore rejection)
		let status_output = Command::new("git").args(["-C", data_dir_str, "status", "--porcelain"]).output()?;
		if !status_output.status.success() {
			return Err(ConsensusSinkError::GitStatus(String::from_utf8_lossy(&status_output.stderr).into_owned()));
		}

		// Check for ignored files that we tried to add
		let project_dir = Local::project_dir(owner, repo);
		if let Ok(rel) = project_dir.strip_prefix(&data_dir) {
			let check_ignored = Command::new("git").args(["-C", data_dir_str, "check-ignore", "--no-index", "-v"]).arg(rel.join("**")).output()?;
			// check-ignore returns 0 if files ARE ignored, 1 if none are ignored
			if check_ignored.status.success() && !check_ignored.stdout.is_empty() {
				return Err(ConsensusSinkError::GitIgnoreRejection(String::from_utf8_lossy(&check_ignored.stdout).into_owned()));
			}
		}

		// Check if there are staged changes to commit
		let diff_output = Command::new("git").args(["-C", data_dir_str, "diff", "--cached", "--quiet"]).status()?;
		if diff_output.success() {
			// No staged changes - nothing to commit
			return Ok(false);
		}

		// Commit with a descriptive message
		let issue_number = self.number().map(|n| format!("#{n}")).unwrap_or_else(|| "new".to_string());
		let commit_msg = format!("sync: {owner}/{repo}{issue_number}");
		let commit_output = Command::new("git").args(["-C", data_dir_str, "commit", "-m", &commit_msg]).output()?;
		if !commit_output.status.success() {
			return Err(ConsensusSinkError::GitCommit(String::from_utf8_lossy(&commit_output.stderr).into_owned()));
		}

		Ok(true)
	}
}

/// Sink an issue node to the local filesystem.
/// Uses issue's identity.ancestry for path construction.
#[allow(deprecated)]
fn sink_issue_node(issue: &Issue, old: Option<&Issue>, owner: &str, repo: &str, _ancestors: &[FetchedIssue]) -> Result<bool> {
	// Duplicate issues self-eliminate: remove local files instead of writing
	if let crate::CloseState::Duplicate(dup_of) = issue.contents.state {
		tracing::info!(issue = ?issue.number(), duplicate_of = dup_of, "Removing duplicate issue from local storage");
		return remove_duplicate_issue(issue, owner, repo);
	}

	let issue_number = issue.number();
	let title = &issue.contents.title;
	let closed = issue.contents.state.is_closed();
	eprintln!("[sink_issue_node] issue #{issue_number:?} '{title}', closed: {closed}, state: {:?}", issue.contents.state);
	let has_children = !issue.children.is_empty();
	let old_has_children = old.map(|o| !o.children.is_empty()).unwrap_or(false);

	// Build ancestor directory names from issue's ancestry lineage
	let ancestor_dir_names = Local::build_ancestor_dir_names(&issue.identity.ancestry).unwrap_or_default();

	// If this issue has a parent (lineage non-empty), ensure the parent is in directory format.
	// The parent may currently be a flat file that needs to be converted to __main__.md.
	if let Some(parent_dir_name) = ancestor_dir_names.last() {
		let grandparent_dir_names = &ancestor_dir_names[..ancestor_dir_names.len() - 1];
		let mut parent_dir = Local::project_dir(owner, repo);
		for dir_name in grandparent_dir_names {
			parent_dir = parent_dir.join(dir_name);
		}
		parent_dir = parent_dir.join(parent_dir_name);

		if !parent_dir.is_dir() {
			// Parent exists as flat file, need to convert to directory
			let flat_open = parent_dir.with_extension("md");
			let flat_closed = PathBuf::from(format!("{}.md.bak", parent_dir.display()));

			std::fs::create_dir_all(&parent_dir)?;

			// Move whichever flat file exists to __main__.md (preserving closed state)
			if flat_closed.exists() {
				let main_path = Local::main_file_path(&parent_dir, true);
				std::fs::rename(&flat_closed, &main_path)?;
			} else if flat_open.exists() {
				let main_path = Local::main_file_path(&parent_dir, false);
				std::fs::rename(&flat_open, &main_path)?;
			}
		}
	}

	let format_changed = has_children != old_has_children;

	let issue_file_path = if has_children {
		let issue_dir = Local::issue_dir_path_from_dir_names(owner, repo, issue_number, title, &ancestor_dir_names);
		std::fs::create_dir_all(&issue_dir)?;

		if format_changed {
			let old_flat_path = Local::issue_file_path_from_dir_names(owner, repo, issue_number, title, false, &ancestor_dir_names);
			if old_flat_path.exists() {
				std::fs::remove_file(&old_flat_path)?;
			}
			let old_flat_closed = Local::issue_file_path_from_dir_names(owner, repo, issue_number, title, true, &ancestor_dir_names);
			if old_flat_closed.exists() {
				std::fs::remove_file(&old_flat_closed)?;
			}
		}

		let old_main_path = Local::main_file_path(&issue_dir, !closed);
		if old_main_path.exists() {
			std::fs::remove_file(&old_main_path)?;
		}

		Local::main_file_path(&issue_dir, closed)
	} else {
		// Clean up old file with opposite closed state
		let old_path = Local::issue_file_path_from_dir_names(owner, repo, issue_number, title, !closed, &ancestor_dir_names);
		if old_path.exists() {
			std::fs::remove_file(&old_path)?;
		}

		// If issue now has a number, also clean up old pending file (without number)
		if issue_number.is_some() {
			let pending_path = Local::issue_file_path_from_dir_names(owner, repo, None, title, false, &ancestor_dir_names);
			if pending_path.exists() {
				std::fs::remove_file(&pending_path)?;
			}
			let pending_closed = Local::issue_file_path_from_dir_names(owner, repo, None, title, true, &ancestor_dir_names);
			if pending_closed.exists() {
				std::fs::remove_file(&pending_closed)?;
			}
		}

		Local::issue_file_path_from_dir_names(owner, repo, issue_number, title, closed, &ancestor_dir_names)
	};

	let content = issue.serialize_filesystem();
	let node_changed = if format_changed {
		true
	} else if let Some(old_issue) = old {
		content != old_issue.serialize_filesystem()
	} else {
		true
	};

	let mut any_written = false;

	if node_changed {
		if let Some(parent) = issue_file_path.parent() {
			std::fs::create_dir_all(parent)?;
		}

		std::fs::write(&issue_file_path, &content)?;
		any_written = true;
	}

	if let Some(issue_num) = issue_number
		&& let Some(timestamps) = issue.identity.timestamps()
	{
		let meta = IssueMeta { timestamps: timestamps.clone() };
		Local::save_issue_meta(owner, repo, issue_num, &meta)?;
	}

	let old_children_map: std::collections::HashMap<u64, &Issue> = old.map(|o| o.children.iter().filter_map(|c| c.number().map(|n| (n, c))).collect()).unwrap_or_default();

	let new_child_numbers: std::collections::HashSet<u64> = issue.children.iter().filter_map(|c| c.number()).collect();

	for child in &issue.children {
		let old_child = child.number().and_then(|n| old_children_map.get(&n).copied());
		// Children have their own ancestry in their identity, so pass empty ancestors
		any_written |= sink_issue_node(child, old_child, owner, repo, &[])?;
	}

	for (&old_num, &old_child) in &old_children_map {
		if !new_child_numbers.contains(&old_num) {
			fn remove_issue_files(issue: &Issue, owner: &str, repo: &str) -> Result<()> {
				let issue_number = issue.number();
				let title = &issue.contents.title;
				let ancestor_dir_names = Local::build_ancestor_dir_names(&issue.identity.ancestry).unwrap_or_default();

				let flat_open = Local::issue_file_path_from_dir_names(owner, repo, issue_number, title, false, &ancestor_dir_names);
				let flat_closed = Local::issue_file_path_from_dir_names(owner, repo, issue_number, title, true, &ancestor_dir_names);
				let _ = std::fs::remove_file(&flat_open);
				let _ = std::fs::remove_file(&flat_closed);

				let issue_dir = Local::issue_dir_path_from_dir_names(owner, repo, issue_number, title, &ancestor_dir_names);
				if issue_dir.is_dir() {
					std::fs::remove_dir_all(&issue_dir)?;
				}

				Ok(())
			}
			remove_issue_files(old_child, owner, repo)?;
			Local::remove_issue_meta(owner, repo, old_num)?;
		}
	}

	Ok(any_written)
}

/// Remove a duplicate issue from local storage.
/// Called when an issue is marked as a duplicate - it should be removed rather than written.
fn remove_duplicate_issue(issue: &Issue, owner: &str, repo: &str) -> Result<bool> {
	let issue_number = issue.number();
	let title = &issue.contents.title;

	let ancestor_dir_names = Local::build_ancestor_dir_names(&issue.identity.ancestry).unwrap_or_default();

	// Remove flat file variants
	let flat_open = Local::issue_file_path_from_dir_names(owner, repo, issue_number, title, false, &ancestor_dir_names);
	let flat_closed = Local::issue_file_path_from_dir_names(owner, repo, issue_number, title, true, &ancestor_dir_names);
	let _ = std::fs::remove_file(&flat_open);
	let _ = std::fs::remove_file(&flat_closed);

	// Remove directory variant (if it has children)
	let issue_dir = Local::issue_dir_path_from_dir_names(owner, repo, issue_number, title, &ancestor_dir_names);
	if issue_dir.is_dir() {
		std::fs::remove_dir_all(&issue_dir)?;
	}

	// Remove metadata
	if let Some(num) = issue_number {
		Local::remove_issue_meta(owner, repo, num)?;
	}

	println!("Removed duplicate issue #{issue_number:?}");
	Ok(true)
}
