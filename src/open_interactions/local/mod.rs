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

use std::path::{Path, PathBuf};

//==============================================================================
// Error Types
//==============================================================================

/// Error type for local issue operations.
#[derive(Debug, thiserror::Error)]
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
		source: Box<tedi::ParseError>,
	},

	/// Invalid path structure - cannot extract owner/repo.
	#[error("invalid issue path structure: {path}")]
	InvalidPath { path: PathBuf },

	/// Git operation failed (for consensus reads).
	#[error("git operation failed: {message}")]
	GitError { message: String },
}

use regex::Regex;
use serde::{Deserialize, Serialize};
use tedi::{Ancestry, FetchedIssue, Issue};
use v_utils::prelude::*;

use crate::open_interactions::sink::Sink;

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

/// Source for loading issues from local storage.
#[derive(Clone, Debug)]
pub struct LocalPath {
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

/// Type for local filesystem operations.
///
/// All custom logic for local issue storage is consolidated as methods on this type.
pub enum Local {}
impl Local {
	/// The filename used for the main issue file when it has a directory for sub-issues.
	pub const MAIN_ISSUE_FILENAME: &'static str = "__main__";

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

	/// Load a full issue tree from a local path.
	///
	/// This is the primary entry point for loading issues from the filesystem.
	/// It recursively loads identity, contents, and children.
	pub async fn load_issue(source: LocalPath) -> Result<Issue, LocalError> {
		let (owner, repo) = Self::extract_owner_repo(&source.path).map_err(|_| LocalError::InvalidPath { path: source.path.clone() })?;
		let ancestry = tedi::Ancestry::root(&owner, &repo);
		let mut issue = Issue::empty_local(ancestry);

		<Issue as tedi::LazyIssue<Local>>::identity(&mut issue, source.clone()).await?;
		<Issue as tedi::LazyIssue<Local>>::contents(&mut issue, source.clone()).await?;
		Box::pin(<Issue as tedi::LazyIssue<Local>>::children(&mut issue, source)).await?;

		Ok(issue)
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

	/// Get the path for an issue file.
	/// Structure: issues/{owner}/{repo}/{number}_-_{title}.md[.bak]
	/// For nested: issues/{owner}/{repo}/{ancestor_dirs}/.../{number}_-_{title}.md[.bak]
	pub fn issue_file_path(owner: &str, repo: &str, issue_number: Option<u64>, title: &str, closed: bool, ancestors: &[FetchedIssue]) -> PathBuf {
		let mut path = Self::project_dir(owner, repo);

		for ancestor in ancestors {
			path = path.join(Self::format_issue_dir_name(ancestor));
		}

		let filename = Self::format_issue_filename(issue_number, title, closed);
		path.join(filename)
	}

	/// Get the path to the issue directory (where sub-issues are stored).
	pub fn issue_dir_path(owner: &str, repo: &str, issue_number: Option<u64>, title: &str, ancestors: &[FetchedIssue]) -> PathBuf {
		let mut path = Self::project_dir(owner, repo);

		for ancestor in ancestors {
			path = path.join(Self::format_issue_dir_name(ancestor));
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
	pub fn find_issue_file(owner: &str, repo: &str, issue_number: Option<u64>, title: &str, ancestors: &[FetchedIssue]) -> Option<PathBuf> {
		// Try flat format first (both open and closed)
		let flat_path = Self::issue_file_path(owner, repo, issue_number, title, false, ancestors);
		if flat_path.exists() {
			return Some(flat_path);
		}

		let flat_closed_path = Self::issue_file_path(owner, repo, issue_number, title, true, ancestors);
		if flat_closed_path.exists() {
			return Some(flat_closed_path);
		}

		// Try directory format
		let issue_dir = Self::issue_dir_path(owner, repo, issue_number, title, ancestors);
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

	/// Build a chain of FetchedIssue by traversing the filesystem for an ancestry.
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

	/// Extract title from directory name.
	fn extract_title_from_dir_name(dir_name: &str, issue_number: u64) -> String {
		let prefix = format!("{issue_number}_-_");
		if let Some(title) = dir_name.strip_prefix(&prefix) {
			title.replace('_', " ")
		} else {
			String::new()
		}
	}

	/// Extract owner/repo from an issue file path.
	pub fn extract_owner_repo(path: &Path) -> Result<(String, String)> {
		let issues_base = Self::issues_dir();

		let rel_path = path.strip_prefix(&issues_base).map_err(|_| eyre!("Issue file is not in issues directory: {path:?}"))?;

		let mut components = rel_path.components();
		let owner = components
			.next()
			.and_then(|c| c.as_os_str().to_str())
			.ok_or_else(|| eyre!("Could not extract owner from path: {path:?}"))?
			.to_string();
		let repo = components
			.next()
			.and_then(|c| c.as_os_str().to_str())
			.ok_or_else(|| eyre!("Could not extract repo from path: {path:?}"))?
			.to_string();

		Ok((owner, repo))
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
		let (_owner, _repo) = Self::extract_owner_repo(issue_file_path)?;

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
	pub fn is_virtual_project(owner: &str, repo: &str) -> bool {
		Self::load_project_meta(owner, repo).virtual_project
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
				issues: std::collections::HashMap::new(),
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
	fn load_project_meta(owner: &str, repo: &str) -> ProjectMeta {
		let meta_path = Self::project_meta_path(owner, repo);
		match std::fs::read_to_string(&meta_path) {
			Ok(content) => match serde_json::from_str(&content) {
				Ok(meta) => meta,
				Err(e) => panic!("corrupted project metadata at {}: {e}", meta_path.display()),
			},
			Err(e) if e.kind() == std::io::ErrorKind::NotFound => ProjectMeta::default(),
			Err(e) => panic!("failed to read project metadata at {}: {e}", meta_path.display()),
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

	/// Load metadata for a specific issue from the project's .meta.json.
	fn load_issue_meta(owner: &str, repo: &str, issue_number: u64) -> Option<IssueMeta> {
		let project_meta = Self::load_project_meta(owner, repo);
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
	#[serde(default, skip_serializing_if = "std::collections::HashMap::is_empty")]
	pub issues: std::collections::HashMap<u64, IssueMeta>,
}

/// Per-issue metadata stored in .meta.json.
#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct IssueMeta {
	/// Timestamps for individual field changes.
	#[serde(default)]
	pub timestamps: tedi::IssueTimestamps,
}

//==============================================================================
// LazyIssue Implementation
//==============================================================================

impl tedi::LazyIssue<Local> for Issue {
	type Error = LocalError;
	type Source = LocalPath;

	async fn identity(&mut self, source: Self::Source) -> Result<tedi::IssueIdentity, Self::Error> {
		if self.identity.is_linked() {
			return Ok(self.identity.clone());
		}

		let (owner, repo) = Local::extract_owner_repo(&source.path).map_err(|_| LocalError::InvalidPath { path: source.path.clone() })?;
		let ancestry = tedi::Ancestry::root(&owner, &repo);

		if self.contents.title.is_empty() {
			let content = Local::read_content(&source).ok_or_else(|| LocalError::FileNotFound { path: source.path.clone() })?;
			let parsed = Local::parse_single_node(&content, ancestry, &source.path)?;
			self.identity = parsed.identity;
			self.contents = parsed.contents;
		}

		if let Some(issue_number) = self.identity.number()
			&& let Some(meta) = Local::load_issue_meta(&owner, &repo, issue_number)
			&& let Some(linked) = self.identity.remote.as_linked_mut()
		{
			linked.timestamps = meta.timestamps;
		}

		Ok(self.identity.clone())
	}

	async fn contents(&mut self, source: Self::Source) -> Result<tedi::IssueContents, Self::Error> {
		if !self.contents.title.is_empty() {
			return Ok(self.contents.clone());
		}

		let (owner, repo) = Local::extract_owner_repo(&source.path).map_err(|_| LocalError::InvalidPath { path: source.path.clone() })?;
		let ancestry = tedi::Ancestry::root(&owner, &repo);

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

			let child = Local::load_issue(child_source).await?;
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
}

//==============================================================================
// Sink Implementation
//==============================================================================

impl Sink<Local> for Issue {
	async fn sink(&mut self, old: Option<&Issue>) -> color_eyre::Result<bool> {
		let owner = self.identity.owner();
		let repo = self.identity.repo();

		sink_issue_node(self, old, owner, repo, &[])
	}
}

fn sink_issue_node(issue: &Issue, old: Option<&Issue>, owner: &str, repo: &str, ancestors: &[FetchedIssue]) -> Result<bool> {
	let issue_number = issue.number();
	let title = &issue.contents.title;
	let closed = issue.contents.state.is_closed();
	let has_children = !issue.children.is_empty();
	let old_has_children = old.map(|o| !o.children.is_empty()).unwrap_or(false);

	let format_changed = has_children != old_has_children;

	let issue_file_path = if has_children {
		let issue_dir = Local::issue_dir_path(owner, repo, issue_number, title, ancestors);
		std::fs::create_dir_all(&issue_dir)?;

		if format_changed {
			let old_flat_path = Local::issue_file_path(owner, repo, issue_number, title, false, ancestors);
			if old_flat_path.exists() {
				std::fs::remove_file(&old_flat_path)?;
			}
			let old_flat_closed = Local::issue_file_path(owner, repo, issue_number, title, true, ancestors);
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
		let old_path = Local::issue_file_path(owner, repo, issue_number, title, !closed, ancestors);
		if old_path.exists() {
			std::fs::remove_file(&old_path)?;
		}

		Local::issue_file_path(owner, repo, issue_number, title, closed, ancestors)
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

	let mut child_ancestors = ancestors.to_vec();
	if let Some(fetched) = FetchedIssue::from_parts(owner, repo, issue_number.unwrap_or(0), title) {
		child_ancestors.push(fetched);
	}

	let old_children_map: std::collections::HashMap<u64, &Issue> = old.map(|o| o.children.iter().filter_map(|c| c.number().map(|n| (n, c))).collect()).unwrap_or_default();

	let new_child_numbers: std::collections::HashSet<u64> = issue.children.iter().filter_map(|c| c.number()).collect();

	for child in &issue.children {
		let old_child = child.number().and_then(|n| old_children_map.get(&n).copied());
		any_written |= sink_issue_node(child, old_child, owner, repo, &child_ancestors)?;
	}

	for (&old_num, &old_child) in &old_children_map {
		if !new_child_numbers.contains(&old_num) {
			fn remove_issue_files(issue: &Issue, owner: &str, repo: &str, ancestors: &[FetchedIssue]) -> Result<()> {
				let issue_number = issue.number();
				let title = &issue.contents.title;

				let flat_open = Local::issue_file_path(owner, repo, issue_number, title, false, ancestors);
				let flat_closed = Local::issue_file_path(owner, repo, issue_number, title, true, ancestors);
				let _ = std::fs::remove_file(&flat_open);
				let _ = std::fs::remove_file(&flat_closed);

				let issue_dir = Local::issue_dir_path(owner, repo, issue_number, title, ancestors);
				if issue_dir.is_dir() {
					std::fs::remove_dir_all(&issue_dir)?;
				}

				Ok(())
			}
			remove_issue_files(old_child, owner, repo, &child_ancestors)?;
			Local::remove_issue_meta(owner, repo, old_num)?;
		}
	}

	Ok(any_written)
}
