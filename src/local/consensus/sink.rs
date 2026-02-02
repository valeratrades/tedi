//! Sink implementation for consensus (git) state.
//!
//! r[local.sink-only-mutation]
//!
//! The Consensus sink only handles git operations (stage + commit).
//! File writing is handled by `Sink<LocalFs>` - call that first.

use std::process::Command;

use super::super::Local;
use crate::{Issue, local::ConsensusSinkError, sink::Sink};

/// Marker type for sinking to git (consensus state).
pub struct Consensus;

/// r[local.sink-only-mutation]
impl Sink<Consensus> for Issue {
	type Error = ConsensusSinkError;

	/// Stage and commit changes to git.
	///
	/// **Important**: This only does git operations. Call `Sink<LocalFs>::sink()` first
	/// to write the actual files.
	async fn sink(&mut self, _old: Option<&Issue>) -> Result<bool, Self::Error> {
		let repo_info = self.repo_info();
		let owner = repo_info.owner();
		let repo = repo_info.repo();
		let git_id = self.git_id().expect("calling this before having had linked the issue means invalid implementation somewhere");

		let data_dir = Local::issues_dir();
		let data_dir_str = data_dir.to_str().ok_or(ConsensusSinkError::InvalidDataDir)?;

		// Stage changes
		let add_output = Command::new("git").args(["-C", data_dir_str, "add", "-A"]).output()?;
		if !add_output.status.success() {
			return Err(ConsensusSinkError::GitAdd(String::from_utf8_lossy(&add_output.stderr).into_owned()));
		}

		// Check for ignored files that we tried to add
		let project_dir = Local::project_dir(repo_info);
		if let Ok(rel) = project_dir.strip_prefix(&data_dir) {
			let check_ignored = Command::new("git").args(["-C", data_dir_str, "check-ignore", "--no-index", "-v"]).arg(rel.join("**")).output()?;
			// check-ignore returns 0 if files ARE ignored, 1 if none are ignored
			if check_ignored.status.success() && !check_ignored.stdout.is_empty() {
				return Err(ConsensusSinkError::GitIgnoreRejection(String::from_utf8_lossy(&check_ignored.stdout).into_owned()));
			}
		}

		// Check if there are staged changes
		let diff_output = Command::new("git").args(["-C", data_dir_str, "diff", "--cached", "--quiet"]).status()?;
		if diff_output.success() {
			// No staged changes
			return Ok(false);
		}

		// Commit
		let commit_msg = format!("sync: {owner}/{repo}#{git_id}");
		let commit_output = Command::new("git").args(["-C", data_dir_str, "commit", "-m", &commit_msg]).output()?;
		if !commit_output.status.success() {
			return Err(ConsensusSinkError::GitCommit(String::from_utf8_lossy(&commit_output.stderr).into_owned()));
		}

		Ok(true)
	}
}
