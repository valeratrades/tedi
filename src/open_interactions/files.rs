//! File path handling for issue storage.

use std::path::{Path, PathBuf};

use todo::{Ancestry, FetchedIssue, Issue};
use v_utils::prelude::*;

// Extension type removed - all files are markdown (.md)

/// The filename used for the main issue file when it has a directory for sub-issues.
/// When an issue has sub-issues, instead of `123_-_title.md` being a file, it becomes
/// `123_-_title/__main__.md` with sub-issues stored alongside.
pub const MAIN_ISSUE_FILENAME: &str = "__main__";

/// Returns the base directory for issue storage: XDG_DATA_HOME/todo/issues/
pub fn issues_dir() -> PathBuf {
	v_utils::xdg_data_dir!("issues")
}

/// Sanitize a title for use in filenames.
/// Converts spaces to underscores and removes special characters.
pub fn sanitize_title_for_filename(title: &str) -> String {
	title
		.chars()
		.map(|c| {
			if c.is_alphanumeric() || c == '-' || c == '_' {
				c
			} else if c == ' ' {
				'_'
			} else {
				// Skip special characters
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
pub fn format_issue_filename(issue_number: Option<u64>, title: &str, closed: bool) -> String {
	let sanitized = sanitize_title_for_filename(title);
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
	format!("{}_-_{}", issue.number(), sanitize_title_for_filename(&issue.title))
}

/// Get the path for an issue file in XDG_DATA.
/// Structure: issues/{owner}/{repo}/{number}_-_{title}.md[.bak] (or just {title}.md for pending/virtual)
/// For nested sub-issues: issues/{owner}/{repo}/{ancestor1_dir}/{ancestor2_dir}/.../{number}_-_{title}.md[.bak]
///
/// `ancestors` is the chain of parent issues from root to immediate parent (not including the issue itself).
pub fn get_issue_file_path(owner: &str, repo: &str, issue_number: Option<u64>, title: &str, closed: bool, ancestors: &[FetchedIssue]) -> PathBuf {
	let mut path = issues_dir().join(owner).join(repo);

	// Build nested directory structure for all ancestors
	for ancestor in ancestors {
		path = path.join(format_issue_dir_name(ancestor));
	}

	let filename = format_issue_filename(issue_number, title, closed);
	path.join(filename)
}

/// Get the project directory path (where meta.json lives).
/// Structure: issues/{owner}/{repo}/
pub fn get_project_dir(owner: &str, repo: &str) -> PathBuf {
	issues_dir().join(owner).join(repo)
}

/// Get the .meta.json path for a project.
/// Structure: issues/{owner}/{repo}/.meta.json
pub fn get_meta_json_path(owner: &str, repo: &str) -> PathBuf {
	get_project_dir(owner, repo).join(".meta.json")
}

/// Build a chain of FetchedIssue by traversing the filesystem for an ancestry.
///
/// Goes through each issue number in the lineage and finds the corresponding
/// directory on disk (format: `{number}_-_*`), extracting titles from directory names.
///
/// Returns an error if any parent directory in the lineage doesn't exist locally.
pub fn build_ancestry_path(ancestry: &Ancestry) -> Result<Vec<FetchedIssue>> {
	let mut path = get_project_dir(ancestry.owner(), ancestry.repo());

	if !path.exists() {
		bail!("Project directory does not exist: {}", path.display());
	}

	let mut result = Vec::with_capacity(ancestry.lineage().len());

	for &issue_number in ancestry.lineage() {
		let dir =
			find_issue_dir_by_number(&path, issue_number).ok_or_else(|| eyre!("Parent issue #{issue_number} not found locally in {}. Fetch the parent issue first.", path.display()))?;

		// Extract title from directory name
		let dir_name = dir.file_name().and_then(|n| n.to_str()).unwrap_or("");
		let title = extract_title_from_dir_name(dir_name, issue_number);

		let fetched = FetchedIssue::from_parts(ancestry.owner(), ancestry.repo(), issue_number, &title).ok_or_else(|| eyre!("Failed to construct FetchedIssue for #{issue_number}"))?;
		result.push(fetched);

		path = dir;
	}

	Ok(result)
}

/// Find an issue directory by its number prefix.
///
/// Looks for a directory matching `{number}_-_*` or just `{number}` in the given path.
/// Returns the full path to the directory if found.
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

		// Match either `{number}_-_*` or exactly `{number}`
		if name.starts_with(&prefix_with_sep) || name == exact_match {
			return Some(path);
		}
	}

	None
}

/// Extract title from directory name.
/// Format: `{number}_-_{title}` returns `{title}` (with underscores as spaces),
/// just `{number}` returns empty string.
fn extract_title_from_dir_name(dir_name: &str, issue_number: u64) -> String {
	let prefix = format!("{issue_number}_-_");
	if let Some(title) = dir_name.strip_prefix(&prefix) {
		title.replace('_', " ")
	} else {
		String::new()
	}
}

/// Get the directory name for an issue (used when it has sub-issues).
/// Format: {number}_-_{sanitized_title}
pub fn get_issue_dir_name(issue_number: Option<u64>, title: &str) -> String {
	let sanitized = sanitize_title_for_filename(title);
	match issue_number {
		Some(num) if sanitized.is_empty() => format!("{num}"),
		Some(num) => format!("{num}_-_{sanitized}"),
		None if sanitized.is_empty() => "untitled".to_string(),
		None => sanitized,
	}
}

/// Get the path to the issue directory (where sub-issues would be stored).
/// This is the same as the file path but without the extension.
pub fn get_issue_dir_path(owner: &str, repo: &str, issue_number: Option<u64>, title: &str, ancestors: &[FetchedIssue]) -> PathBuf {
	let mut path = issues_dir().join(owner).join(repo);

	// Build nested directory structure for all ancestors
	for ancestor in ancestors {
		path = path.join(format_issue_dir_name(ancestor));
	}

	path.join(get_issue_dir_name(issue_number, title))
}

/// Get the path for the main issue file when stored inside a directory.
/// Format: {dir}/__main__.md[.bak]
pub fn get_main_file_path(issue_dir: &Path, closed: bool) -> PathBuf {
	let filename = if closed {
		format!("{MAIN_ISSUE_FILENAME}.md.bak")
	} else {
		format!("{MAIN_ISSUE_FILENAME}.md")
	};
	issue_dir.join(filename)
}

/// Find the actual file path for an issue, checking both flat and directory formats.
/// This handles the case where we need to find an existing issue file regardless of format.
///
/// Checks in order:
/// 1. Flat format: {parent}/{number}_-_{title}.md[.bak]
/// 2. Directory format: {parent}/{number}_-_{title}/__main__.md[.bak]
///
/// Returns None if no file is found in either format.
pub fn find_issue_file(owner: &str, repo: &str, issue_number: Option<u64>, title: &str, ancestors: &[FetchedIssue]) -> Option<PathBuf> {
	// Try flat format first (both open and closed)
	let flat_path = get_issue_file_path(owner, repo, issue_number, title, false, ancestors);
	if flat_path.exists() {
		return Some(flat_path);
	}

	let flat_closed_path = get_issue_file_path(owner, repo, issue_number, title, true, ancestors);
	if flat_closed_path.exists() {
		return Some(flat_closed_path);
	}

	// Try directory format
	let issue_dir = get_issue_dir_path(owner, repo, issue_number, title, ancestors);
	if issue_dir.is_dir() {
		// Check for __main__ file (both open and closed)
		let main_path = get_main_file_path(&issue_dir, false);
		if main_path.exists() {
			return Some(main_path);
		}

		let main_closed_path = get_main_file_path(&issue_dir, true);
		if main_closed_path.exists() {
			return Some(main_closed_path);
		}
	}

	None
}

/// Search for issue files matching a pattern.
/// Returns paths relative to the issues directory.
pub fn search_issue_files(pattern: &str) -> Result<Vec<PathBuf>> {
	let issues_base = issues_dir();
	if !issues_base.exists() {
		return Ok(vec![]);
	}

	let mut matches = Vec::new();
	let pattern_lower = pattern.to_lowercase();

	// Walk the issues directory
	fn walk_dir(dir: &Path, pattern: &str, matches: &mut Vec<PathBuf>) -> std::io::Result<()> {
		for entry in std::fs::read_dir(dir)? {
			let entry = entry?;
			let path = entry.path();

			if path.is_dir() {
				walk_dir(&path, pattern, matches)?;
			} else if path.is_file() {
				// Check if file matches the pattern
				if let Some(name) = path.file_name().and_then(|n| n.to_str()) {
					// Only consider .md files (including .bak versions)
					if name.ends_with(".md") || name.ends_with(".md.bak") {
						// Match against filename or full path
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

	// Sort by modification time (most recent first)
	matches.sort_by(|a, b| {
		let a_time = std::fs::metadata(a).and_then(|m| m.modified()).ok();
		let b_time = std::fs::metadata(b).and_then(|m| m.modified()).ok();
		b_time.cmp(&a_time)
	});

	Ok(matches)
}

/// Exact match level for fzf queries.
#[derive(Clone, Copy, Debug, Default)]
pub enum ExactMatchLevel {
	/// Default fuzzy matching
	#[default]
	Fuzzy,
	/// Exact substring match with space-separated terms (fzf --exact)
	ExactTerms,
	/// Regex pattern matching (substring)
	RegexSubstring,
	/// Regex pattern matching (full line)
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

/// Choose an issue file using fzf.
///
/// Takes all files and lets fzf handle the filtering with the specified exact match level.
/// Uses --select-1 to auto-select when only one match.
pub fn choose_issue_with_fzf(files: &[PathBuf], initial_query: &str, exact: ExactMatchLevel) -> Result<Option<PathBuf>> {
	use std::{
		io::Write,
		process::{Command, Stdio},
	};

	use regex::Regex;

	let issues_base = issues_dir();

	// Prepare file list with relative paths for display
	let file_list: Vec<String> = files
		.iter()
		.filter_map(|p| p.strip_prefix(&issues_base).ok().map(|rel| rel.to_string_lossy().to_string()))
		.collect();

	// For regex modes, pre-filter the file list
	let (filtered_list, fzf_query): (Vec<&String>, String) = match exact {
		ExactMatchLevel::Fuzzy | ExactMatchLevel::ExactTerms => {
			// No pre-filtering, pass query to fzf
			(file_list.iter().collect(), initial_query.to_string())
		}
		ExactMatchLevel::RegexSubstring => {
			// Pre-filter with regex (substring match)
			if initial_query.is_empty() {
				(file_list.iter().collect(), String::new())
			} else {
				let re = Regex::new(initial_query).map_err(|e| eyre!("Invalid regex pattern: {e}"))?;
				let filtered: Vec<&String> = file_list.iter().filter(|f| re.is_match(f)).collect();
				(filtered, String::new()) // Clear query since we already filtered
			}
		}
		ExactMatchLevel::RegexLine => {
			// Pre-filter with regex (full line match, auto-anchor if needed)
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
				(filtered, String::new()) // Clear query since we already filtered
			}
		}
	};

	// Build fzf command
	let mut cmd = Command::new("fzf");

	cmd.arg("--query").arg(&fzf_query);

	// Add --exact flag for ExactTerms mode
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

	// Write file list to fzf stdin
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

/// Extract owner/repo from an issue file path.
/// Path format: .../{owner}/{repo}/{issue_file} or .../{owner}/{repo}/{parent_dir}/{sub_issue_file}
pub fn extract_owner_repo_from_path(issue_file_path: &Path) -> Result<(String, String)> {
	let issues_base = issues_dir();

	// Get relative path from issues base
	let rel_path = issue_file_path
		.strip_prefix(&issues_base)
		.map_err(|_| eyre!("Issue file is not in issues directory: {issue_file_path:?}"))?;

	// Extract first two components as owner/repo
	let mut components = rel_path.components();
	let owner = components
		.next()
		.and_then(|c| c.as_os_str().to_str())
		.ok_or_else(|| eyre!("Could not extract owner from path: {issue_file_path:?}"))?
		.to_string();
	let repo = components
		.next()
		.and_then(|c| c.as_os_str().to_str())
		.ok_or_else(|| eyre!("Could not extract repo from path: {issue_file_path:?}"))?
		.to_string();

	Ok((owner, repo))
}

/// Load an issue tree from the filesystem.
///
/// This reads the issue at the given path using `deserialize_filesystem` (which loads
/// just the node, no children), then recursively scans the directory for child files
/// and loads them into the `children` field.
///
/// Also loads per-issue metadata (like `ts`) from the project's `.meta.json`.
///
/// For flat files (e.g., `123_-_title.md`), there are no children.
/// For directory format (e.g., `123_-_title/__main__.md`), children are loaded from
/// sibling files in the same directory.
pub fn load_issue_tree(issue_file_path: &Path) -> Result<Issue> {
	let (owner, repo) = extract_owner_repo_from_path(issue_file_path)?;
	let content = std::fs::read_to_string(issue_file_path)?;
	let mut issue = Issue::deserialize_filesystem(&content)?;

	// Load ts from .meta.json if the issue is linked (has a number)
	if let Some(issue_number) = issue.number() {
		if let Some(meta) = super::meta::load_issue_meta(&owner, &repo, issue_number) {
			if let Some(linked) = &mut issue.identity.linked {
				linked.ts = meta.ts;
			}
		}
	}

	// Determine if this is a directory format (has __main__ in path)
	let is_dir_format = issue_file_path.file_name().and_then(|n| n.to_str()).map(|n| n.starts_with(MAIN_ISSUE_FILENAME)).unwrap_or(false);

	if is_dir_format {
		// Load children from sibling files in the directory
		if let Some(dir) = issue_file_path.parent() {
			load_children_from_dir(&mut issue, dir, &owner, &repo)?;
		}
	}

	Ok(issue)
}

/// Recursively load children from a directory into the issue.
fn load_children_from_dir(issue: &mut Issue, dir: &Path, owner: &str, repo: &str) -> Result<()> {
	let Ok(entries) = std::fs::read_dir(dir) else {
		return Ok(());
	};

	for entry in entries.flatten() {
		let path = entry.path();
		let Some(name) = path.file_name().and_then(|n| n.to_str()) else {
			continue;
		};

		// Skip __main__ files (that's the parent issue itself)
		if name.starts_with(MAIN_ISSUE_FILENAME) {
			continue;
		}

		// Check if this is an issue file or directory
		if path.is_file() && (name.ends_with(".md") || name.ends_with(".md.bak")) {
			// Flat child file
			let child = load_issue_tree_inner(&path, owner, repo)?;
			issue.children.push(child);
		} else if path.is_dir() {
			// Directory child - look for __main__ file
			let main_path = get_main_file_path(&path, false);
			let main_closed_path = get_main_file_path(&path, true);

			let child_path = if main_path.exists() {
				main_path
			} else if main_closed_path.exists() {
				main_closed_path
			} else {
				continue;
			};

			let child = load_issue_tree_inner(&child_path, owner, repo)?;
			issue.children.push(child);
		}
	}

	// Sort children by issue number for consistent ordering
	issue.children.sort_by(|a, b| {
		let a_num = a.number().unwrap_or(0);
		let b_num = b.number().unwrap_or(0);
		a_num.cmp(&b_num)
	});

	Ok(())
}

/// Internal: load issue tree when owner/repo are already known.
fn load_issue_tree_inner(issue_file_path: &Path, owner: &str, repo: &str) -> Result<Issue> {
	let content = std::fs::read_to_string(issue_file_path)?;
	let mut issue = Issue::deserialize_filesystem(&content)?;

	// Load ts from .meta.json if the issue is linked (has a number)
	if let Some(issue_number) = issue.number() {
		if let Some(meta) = super::meta::load_issue_meta(owner, repo, issue_number) {
			if let Some(linked) = &mut issue.identity.linked {
				linked.ts = meta.ts;
			}
		}
	}

	// Determine if this is a directory format (has __main__ in path)
	let is_dir_format = issue_file_path.file_name().and_then(|n| n.to_str()).map(|n| n.starts_with(MAIN_ISSUE_FILENAME)).unwrap_or(false);

	if is_dir_format {
		// Load children from sibling files in the directory
		if let Some(dir) = issue_file_path.parent() {
			load_children_from_dir(&mut issue, dir, owner, repo)?;
		}
	}

	Ok(issue)
}

//==============================================================================
// Filesystem Sink Implementation
//==============================================================================

use super::sink::{Local, Sink};

impl Sink<Local> for Issue {
	/// Save an issue tree to the filesystem.
	///
	/// Compares against `old` state to only write files that actually changed.
	/// Each node is written to its own file using `serialize_filesystem`.
	/// If the issue has children, it uses directory format with `__main__.md`.
	/// Children are written as siblings in the directory.
	///
	/// Location is derived from the issue's ancestry (owner/repo/lineage).
	async fn sink(&mut self, old: Option<&Issue>) -> color_eyre::Result<bool> {
		let owner = self.identity.owner();
		let repo = self.identity.repo();

		sink_issue_node(self, old, owner, repo, &[])
	}
}

/// Recursively write an issue node and its children to the filesystem.
/// Only writes files that differ from `old` state.
/// Returns Ok(true) if any files were written.
fn sink_issue_node(issue: &Issue, old: Option<&Issue>, owner: &str, repo: &str, ancestors: &[FetchedIssue]) -> Result<bool> {
	let issue_number = issue.number();
	let title = &issue.contents.title;
	let closed = issue.contents.state.is_closed();
	let has_children = !issue.children.is_empty();
	let old_has_children = old.map(|o| !o.children.is_empty()).unwrap_or(false);

	// Determine if format changed (flat <-> directory)
	let format_changed = has_children != old_has_children;

	// Determine target path based on current state
	let issue_file_path = if has_children {
		// Directory format
		let issue_dir = get_issue_dir_path(owner, repo, issue_number, title, ancestors);
		std::fs::create_dir_all(&issue_dir)?;

		// Clean up old flat file if format changed to directory
		if format_changed {
			let old_flat_path = get_issue_file_path(owner, repo, issue_number, title, false, ancestors);
			if old_flat_path.exists() {
				std::fs::remove_file(&old_flat_path)?;
			}
			let old_flat_closed = get_issue_file_path(owner, repo, issue_number, title, true, ancestors);
			if old_flat_closed.exists() {
				std::fs::remove_file(&old_flat_closed)?;
			}
		}

		// Clean up the old main file with opposite close state
		let old_main_path = get_main_file_path(&issue_dir, !closed);
		if old_main_path.exists() {
			std::fs::remove_file(&old_main_path)?;
		}

		get_main_file_path(&issue_dir, closed)
	} else {
		// Flat format - clean up the file with opposite close state
		let old_path = get_issue_file_path(owner, repo, issue_number, title, !closed, ancestors);
		if old_path.exists() {
			std::fs::remove_file(&old_path)?;
		}

		get_issue_file_path(owner, repo, issue_number, title, closed, ancestors)
	};

	// Check if this node's content changed (excluding children)
	let content = issue.serialize_filesystem();
	let node_changed = if format_changed {
		true // Format change always requires write
	} else if let Some(old_issue) = old {
		// Compare serialized content (this compares the node without children)
		content != old_issue.serialize_filesystem()
	} else {
		true // No old state means new file
	};

	let mut any_written = false;

	if node_changed {
		// Create parent directories
		if let Some(parent) = issue_file_path.parent() {
			std::fs::create_dir_all(parent)?;
		}

		// Write this node
		std::fs::write(&issue_file_path, &content)?;
		any_written = true;
	}

	// Save ts metadata to .meta.json if issue has a number and ts
	if let Some(issue_num) = issue_number
		&& let Some(ts) = issue.ts()
	{
		let meta = super::meta::IssueMeta { ts: Some(ts) };
		super::meta::save_issue_meta(owner, repo, issue_num, &meta)?;
	}

	// Build ancestors for children
	let mut child_ancestors = ancestors.to_vec();
	if let Some(fetched) = FetchedIssue::from_parts(owner, repo, issue_number.unwrap_or(0), title) {
		child_ancestors.push(fetched);
	}

	// Build map of old children by issue number for matching
	let old_children_map: std::collections::HashMap<u64, &Issue> = old.map(|o| o.children.iter().filter_map(|c| c.number().map(|n| (n, c))).collect()).unwrap_or_default();

	// Build set of new children numbers for deletion check
	let new_child_numbers: std::collections::HashSet<u64> = issue.children.iter().filter_map(|c| c.number()).collect();

	// Recursively sink children, matching with old children by issue number
	for child in &issue.children {
		let old_child = child.number().and_then(|n| old_children_map.get(&n).copied());
		any_written |= sink_issue_node(child, old_child, owner, repo, &child_ancestors)?;
	}

	// Remove files for deleted children (in old but not in new)
	for (&old_num, &old_child) in &old_children_map {
		if !new_child_numbers.contains(&old_num) {
			/// Remove issue files from filesystem (both flat and directory formats).
			fn remove_issue_files(issue: &Issue, owner: &str, repo: &str, ancestors: &[FetchedIssue]) -> Result<()> {
				let issue_number = issue.number();
				let title = &issue.contents.title;

				// Try to remove flat format files (both open and closed)
				let flat_open = get_issue_file_path(owner, repo, issue_number, title, false, ancestors);
				let flat_closed = get_issue_file_path(owner, repo, issue_number, title, true, ancestors);
				let _ = std::fs::remove_file(&flat_open);
				let _ = std::fs::remove_file(&flat_closed);

				// Try to remove directory format
				let issue_dir = get_issue_dir_path(owner, repo, issue_number, title, ancestors);
				if issue_dir.is_dir() {
					std::fs::remove_dir_all(&issue_dir)?;
				}

				Ok(())
			}
			remove_issue_files(old_child, owner, repo, &child_ancestors)?;
		}
	}

	Ok(any_written)
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn test_main_issue_filename_constant() {
		assert_eq!(MAIN_ISSUE_FILENAME, "__main__");
	}

	#[test]
	fn test_get_issue_dir_name() {
		// With number and title
		assert_eq!(get_issue_dir_name(Some(123), "My Issue"), "123_-_My_Issue");

		// With number only
		assert_eq!(get_issue_dir_name(Some(123), ""), "123");

		// Without number
		assert_eq!(get_issue_dir_name(None, "My Issue"), "My_Issue");

		// Without number and title
		assert_eq!(get_issue_dir_name(None, ""), "untitled");
	}

	#[test]
	fn test_get_main_file_path() {
		let dir = PathBuf::from("/tmp/issues/123_-_title");

		// Open issue
		assert_eq!(get_main_file_path(&dir, false), PathBuf::from("/tmp/issues/123_-_title/__main__.md"));

		// Closed issue
		assert_eq!(get_main_file_path(&dir, true), PathBuf::from("/tmp/issues/123_-_title/__main__.md.bak"));
	}

	#[test]
	fn test_exact_match_level_try_from() {
		assert!(matches!(ExactMatchLevel::try_from(0), Ok(ExactMatchLevel::Fuzzy)));
		assert!(matches!(ExactMatchLevel::try_from(1), Ok(ExactMatchLevel::ExactTerms)));
		assert!(matches!(ExactMatchLevel::try_from(2), Ok(ExactMatchLevel::RegexSubstring)));
		assert!(matches!(ExactMatchLevel::try_from(3), Ok(ExactMatchLevel::RegexLine)));
		assert!(ExactMatchLevel::try_from(4).is_err());
		assert!(ExactMatchLevel::try_from(255).is_err());
	}
}
