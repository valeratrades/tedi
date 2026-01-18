//! Git operations for consensus state management.
//!
//! Provides functions to commit issue changes to git.

use std::{path::Path, process::Command};

use v_utils::prelude::*;

use crate::open_interactions::local::Local;

/// Check if git is initialized in the issues directory.
pub fn is_git_initialized() -> bool {
	let data_dir = Local::issues_dir();
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
	let data_dir = Local::issues_dir();
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
