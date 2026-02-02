//! Git operations for consensus state management.

use std::process::Command;

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
