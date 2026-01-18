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

use jiff::Timestamp;
use regex::Regex;
use serde::{Deserialize, Serialize};
use todo::{Ancestry, FetchedIssue, Issue};
use v_utils::prelude::*;

use crate::open_interactions::sink::Sink;

//==============================================================================
// Local - The interface for local issue storage
//==============================================================================

/// Marker type for local filesystem operations.
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
				} else if path.is_file() {
					if let Some(name) = path.file_name().and_then(|n| n.to_str()) {
						if name.ends_with(".md") || name.ends_with(".md.bak") {
							let name_lower = name.to_lowercase();
							let path_str = path.to_string_lossy().to_lowercase();
							if pattern.is_empty() || name_lower.contains(pattern) || path_str.contains(pattern) {
								matches.push(path);
							}
						}
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
			Ok(content) => serde_json::from_str(&content).unwrap_or_default(),
			Err(_) => ProjectMeta::default(),
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
	#[serde(default, skip_serializing_if = "Option::is_none")]
	pub ts: Option<Timestamp>,
}

//==============================================================================
// LazyIssue Implementation
//==============================================================================

impl todo::LazyIssue<Local> for Issue {
	type Source = PathBuf;

	async fn identity(&mut self, source: Self::Source) -> todo::IssueIdentity {
		if self.identity.is_linked() {
			return self.identity.clone();
		}

		let (owner, repo) = Local::extract_owner_repo(&source).expect("valid issue path");

		if self.contents.title.is_empty() {
			let content = std::fs::read_to_string(&source).expect("file exists");
			let parsed = Issue::deserialize_filesystem(&content).expect("valid issue file");
			self.identity = parsed.identity;
			self.contents = parsed.contents;
		}

		if let Some(issue_number) = self.identity.number() {
			if let Some(meta) = Local::load_issue_meta(&owner, &repo, issue_number) {
				if let Some(linked) = &mut self.identity.linked {
					linked.ts = meta.ts;
				}
			}
		}

		self.identity.clone()
	}

	async fn contents(&mut self, source: Self::Source) -> todo::IssueContents {
		if !self.contents.title.is_empty() {
			return self.contents.clone();
		}

		let content = std::fs::read_to_string(&source).expect("file exists");
		let parsed = Issue::deserialize_filesystem(&content).expect("valid issue file");
		self.identity = parsed.identity;
		self.contents = parsed.contents;

		self.contents.clone()
	}

	async fn children(&mut self, source: Self::Source) -> Vec<Issue> {
		if !self.children.is_empty() {
			return self.children.clone();
		}

		let (owner, repo) = Local::extract_owner_repo(&source).expect("valid issue path");

		let is_dir_format = source.file_name().and_then(|n| n.to_str()).map(|n| n.starts_with(Local::MAIN_ISSUE_FILENAME)).unwrap_or(false);

		if !is_dir_format {
			return Vec::new();
		}

		let Some(dir) = source.parent() else {
			return Vec::new();
		};

		let Ok(entries) = std::fs::read_dir(dir) else {
			return Vec::new();
		};

		let mut children = Vec::new();

		for entry in entries.flatten() {
			let path = entry.path();
			let Some(name) = path.file_name().and_then(|n| n.to_str()) else {
				continue;
			};

			if name.starts_with(Local::MAIN_ISSUE_FILENAME) {
				continue;
			}

			let child_path = if path.is_file() && (name.ends_with(".md") || name.ends_with(".md.bak")) {
				path
			} else if path.is_dir() {
				let main_path = Local::main_file_path(&path, false);
				let main_closed_path = Local::main_file_path(&path, true);

				if main_path.exists() {
					main_path
				} else if main_closed_path.exists() {
					main_closed_path
				} else {
					continue;
				}
			} else {
				continue;
			};

			let mut child = Issue::empty_local(todo::Ancestry::root(&owner, &repo));
			<Issue as todo::LazyIssue<Local>>::identity(&mut child, child_path.clone()).await;
			<Issue as todo::LazyIssue<Local>>::contents(&mut child, child_path.clone()).await;
			Box::pin(<Issue as todo::LazyIssue<Local>>::children(&mut child, child_path)).await;

			children.push(child);
		}

		children.sort_by(|a, b| {
			let a_num = a.number().unwrap_or(0);
			let b_num = b.number().unwrap_or(0);
			a_num.cmp(&b_num)
		});

		self.children = children.clone();
		children
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
		&& let Some(ts) = issue.ts()
	{
		let meta = IssueMeta { ts: Some(ts) };
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
