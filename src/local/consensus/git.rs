//! Git operations for consensus state management.
//!
//! Provides functions to commit issue changes to git.

use std::process::Command;

use v_utils::prelude::*;

use super::super::Local;

/// Check if git is initialized in the issues directory.
pub fn is_git_initialized() -> bool {
	let data_dir = Local::issues_dir();
	let Some(data_dir_str) = data_dir.to_str() else {
		return false;
	};

	Command::new("git")
		.args(["-C", data_dir_str, "rev-parse", "--git-dir"])
		.output()
		.expect("failed to execute git command")
		.status
		.success()
}

/// Stage and commit changes for an issue file.
pub fn commit_issue_changes(owner: &str, repo: &str, issue_number: u64) -> Result<()> {
	let data_dir = Local::issues_dir();
	let data_dir_str = data_dir.to_str().ok_or_else(|| eyre!("Invalid data directory path"))?;

	// Stage changes
	let add_status = Command::new("git").args(["-C", data_dir_str, "add", "-A"]).status()?;
	if !add_status.success() {
		bail!("git add -A failed");
	}

	// Check if there are staged changes
	let diff_output = Command::new("git").args(["-C", data_dir_str, "diff", "--cached", "--quiet"]).status()?;
	if diff_output.success() {
		// No staged changes
		return Ok(());
	}

	// Commit with provided or default message
	let commit_msg = format!("sync: {owner}/{repo}#{issue_number}");
	let commit_output = Command::new("git").args(["-C", data_dir_str, "commit", "-m", &commit_msg]).output()?;
	if !commit_output.status.success() {
		bail!("git commit failed: {}", String::from_utf8_lossy(&commit_output.stderr));
	}

	Ok(())
}
