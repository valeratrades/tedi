//! Git operations for issue sync.
//!
//! The consensus state (last synced state) is stored in git.
//! This module provides functions to read committed content.

use std::{path::Path, process::Command};

use todo::Issue;
use v_utils::prelude::*;

use super::local::issues_dir;

/// Result of checking if a file is tracked in git.
pub enum GitTrackingStatus {
	/// File is tracked and we have its committed content
	Tracked(String),
	/// File is not tracked in git (new file)
	Untracked,
	/// Git is not initialized in the issues directory
	NoGit,
}

/// Check if a file is tracked in git and read its committed content.
pub fn read_committed_content(file_path: &Path) -> GitTrackingStatus {
	let data_dir = issues_dir();
	let Some(data_dir_str) = data_dir.to_str() else {
		return GitTrackingStatus::NoGit;
	};

	// Get the relative path from issues_dir
	let Some(rel_path) = file_path.strip_prefix(&data_dir).ok() else {
		return GitTrackingStatus::NoGit;
	};
	let Some(rel_path_str) = rel_path.to_str() else {
		return GitTrackingStatus::NoGit;
	};

	// Check if git is initialized
	let Ok(git_check) = Command::new("git").args(["-C", data_dir_str, "rev-parse", "--git-dir"]).output() else {
		return GitTrackingStatus::NoGit;
	};
	if !git_check.status.success() {
		return GitTrackingStatus::NoGit;
	}

	// Check if the file is tracked in git
	let Ok(ls_output) = Command::new("git").args(["-C", data_dir_str, "ls-files", rel_path_str]).output() else {
		return GitTrackingStatus::NoGit;
	};
	if !ls_output.status.success() || ls_output.stdout.is_empty() {
		// File is not tracked by git - this is valid for new files
		return GitTrackingStatus::Untracked;
	}

	// File IS tracked - we MUST be able to read it. If we can't, that's a bug.
	// Note: HEAD:./path is needed because git show HEAD:path expects repo-root-relative paths,
	// but we're running from a subdirectory (issues_dir). The ./ prefix makes it cwd-relative.
	let output = Command::new("git")
		.args(["-C", data_dir_str, "show", &format!("HEAD:./{rel_path_str}")])
		.output()
		.expect("git show command failed to execute");

	if !output.status.success() {
		panic!(
			"BUG: File is tracked in git but cannot read committed content.\n\
			 File: {}\n\
			 Git error: {}",
			file_path.display(),
			String::from_utf8_lossy(&output.stderr)
		);
	}

	let content = String::from_utf8(output.stdout).expect("git show returned invalid UTF-8");
	GitTrackingStatus::Tracked(content)
}

/// Load the consensus Issue tree from git (last committed state).
///
/// This recursively loads child issues from git, similar to how `load_issue_tree`
/// loads children from the working directory. This ensures consistent comparison
/// between local (full tree from working dir) and consensus (full tree from git).
///
/// Returns:
/// - `Some(Issue)` if file is tracked and consensus loaded successfully
/// - `None` if file is not tracked (new file, no consensus yet)
///
/// Panics if file is tracked but consensus cannot be loaded (indicates a bug).
pub fn load_consensus_issue(file_path: &Path) -> Option<Issue> {
	match read_committed_content(file_path) {
		GitTrackingStatus::Tracked(content) => {
			// Parse as filesystem format (no children inline)
			let mut issue = Issue::deserialize_filesystem(&content).unwrap_or_else(|e| {
				panic!(
					"BUG: Failed to parse committed consensus issue.\n\
					 File: {}\n\
					 Parse error: {e}",
					file_path.display()
				)
			});

			// Determine if this is a directory format (has __main__ in path)
			let is_dir_format = file_path
				.file_name()
				.and_then(|n| n.to_str())
				.map(|n| n.starts_with(super::local::MAIN_ISSUE_FILENAME))
				.unwrap_or(false);

			if is_dir_format {
				// Load children from git
				if let Some(dir) = file_path.parent() {
					load_consensus_children(&mut issue, dir);
				}
			}

			Some(issue)
		}
		GitTrackingStatus::Untracked | GitTrackingStatus::NoGit => None,
	}
}

/// Recursively load child issues from git into the issue's children field.
fn load_consensus_children(issue: &mut Issue, dir: &Path) {
	let data_dir = issues_dir();
	let Some(data_dir_str) = data_dir.to_str() else {
		return;
	};
	let Some(rel_dir) = dir.strip_prefix(&data_dir).ok() else {
		return;
	};
	let Some(rel_dir_str) = rel_dir.to_str() else {
		return;
	};

	// List files in directory from git
	let output = Command::new("git")
		.args(["-C", data_dir_str, "ls-tree", "--name-only", "HEAD", &format!("{rel_dir_str}/")])
		.output();

	let Ok(output) = output else {
		return;
	};
	if !output.status.success() {
		return;
	}

	let entries: Vec<&str> = std::str::from_utf8(&output.stdout)
		.unwrap_or("")
		.lines()
		.filter_map(|line| line.strip_prefix(&format!("{rel_dir_str}/")))
		.collect();

	for entry in entries {
		// Skip __main__ files (that's the parent issue itself)
		if entry.starts_with(super::local::MAIN_ISSUE_FILENAME) {
			continue;
		}

		// Check if this is a directory or file
		let entry_path = dir.join(entry);
		let is_dir = {
			// Check git to see if this is a directory (has children)
			let check = Command::new("git").args(["-C", data_dir_str, "ls-tree", "HEAD", &format!("{rel_dir_str}/{entry}/")]).output();
			check.map(|o| o.status.success() && !o.stdout.is_empty()).unwrap_or(false)
		};

		if is_dir {
			// Directory child - look for __main__ file
			let main_path = entry_path.join(format!("{}.md", super::local::MAIN_ISSUE_FILENAME));
			let main_closed_path = entry_path.join(format!("{}.md.bak", super::local::MAIN_ISSUE_FILENAME));

			if let Some(child) = load_consensus_issue(&main_path).or_else(|| load_consensus_issue(&main_closed_path)) {
				issue.children.push(child);
			}
		} else if entry.ends_with(".md") || entry.ends_with(".md.bak") {
			// Flat child file
			if let Some(child) = load_consensus_issue(&entry_path) {
				issue.children.push(child);
			}
		}
	}

	// Sort children by issue number for consistent ordering
	issue.children.sort_by(|a, b| {
		let a_num = a.number().unwrap_or(0);
		let b_num = b.number().unwrap_or(0);
		a_num.cmp(&b_num)
	});
}

/// Check if git is initialized in the issues directory.
pub fn is_git_initialized() -> bool {
	let data_dir = issues_dir();
	let Some(data_dir_str) = data_dir.to_str() else {
		return false;
	};

	Command::new("git")
		.args(["-C", data_dir_str, "rev-parse", "--git-dir"])
		.output()
		.map(|o| o.status.success())
		.unwrap_or(false)
}

/// Stage and commit changes for an issue file.
pub fn commit_issue_changes(_file_path: &Path, owner: &str, repo: &str, issue_number: u64, message: Option<&str>) -> Result<()> {
	let data_dir = issues_dir();
	let data_dir_str = data_dir.to_str().ok_or_else(|| eyre!("Invalid data directory path"))?;

	// Stage changes
	let _ = Command::new("git").args(["-C", data_dir_str, "add", "-A"]).status()?;

	// Check if there are staged changes
	let diff_output = Command::new("git").args(["-C", data_dir_str, "diff", "--cached", "--quiet"]).status()?;
	if diff_output.success() {
		// No staged changes
		return Ok(());
	}

	// Commit with provided or default message
	let default_msg = format!("sync: {owner}/{repo}#{issue_number}");
	let commit_msg = message.unwrap_or(&default_msg);
	Command::new("git").args(["-C", data_dir_str, "commit", "-m", commit_msg]).output()?;

	Ok(())
}
