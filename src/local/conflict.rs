//! Conflict handling for issue sync.
//!
//! When local and remote diverge and cannot be auto-resolved, we create a real git merge conflict:
//! 1. Commit local state to current branch
//! 2. Create `remote-state` branch from parent commit
//! 3. Write remote state (as virtual format) to `{owner}/__conflict.md`
//! 4. Attempt git merge - if conflicts, user resolves with standard git tools
//!
//! Using `__conflict.md` in virtual format (all children inlined) ensures:
//! - Single file to resolve, not a tree of files
//! - Standard git merge tools work (mergetool, checkout --ours/--theirs)
//! - Easy to see the full issue structure in conflict markers

use std::{path::PathBuf, process::Command};

use miette::Diagnostic;
use thiserror::Error;
use v_utils::prelude::*;

use super::Local;
use crate::{HollowIssue, Issue, IssueIndex, RepoInfo, VirtualIssue, local::consensus::is_git_initialized};

//==============================================================================
// Error Types
//==============================================================================

/// Error returned when there are unresolved conflicts blocking operations.
#[derive(Debug, Diagnostic, Error)]
#[error("Unresolved merge conflict")]
#[diagnostic(
	code(tedi::conflict::unresolved),
	help(
		"Resolve the conflict in:\n  {0}\n\n\
		 Options:\n\
		 1. Edit the file to resolve conflict markers (<<<<<<< ======= >>>>>>>)\n\
		 2. Use: git checkout --ours {0} (keep local)\n\
		 3. Use: git checkout --theirs {0} (keep remote)\n\
		 4. Use: git mergetool\n\n\
		 Then: git add {0} && git commit",
		conflict_file.display()
	)
)]
pub struct ConflictBlockedError {
	pub conflict_file: PathBuf,
}

/// Error from conflict operations.
#[derive(Debug, Error)]
pub enum ConflictError {
	#[error("git is not initialized in issues directory")]
	GitNotInitialized,

	#[error("git operation failed: {message}")]
	GitError { message: String },

	#[error("IO error: {0}")]
	Io(#[from] std::io::Error),
}

//==============================================================================
// Conflict File Path
//==============================================================================

/// Get the conflict file path for a given owner.
/// Format: `{issues_dir}/{owner}/__conflict.md`
pub fn conflict_file_path(owner: &str) -> PathBuf {
	Local::issues_dir().join(owner).join("__conflict.md")
}

//==============================================================================
// Conflict Detection
//==============================================================================

/// Check for unresolved conflict for a given owner.
pub fn check_conflict(issue_index: IssueIndex) -> Result<Option<PathBuf>> {
	let conflict_fpath = conflict_file_path(issue_index.owner());

	if !conflict_fpath.exists() {
		return Ok(None);
	}

	let content = std::fs::read_to_string(&conflict_fpath)?;

	if has_conflict_markers(&content) {
		//Err(ConflictBlockedError { conflict_file })
		Ok(Some(conflict_fpath))
	} else {
		// have the conflict file, but user has had resolved it, - sync then cleanup
		{
			let virtual_issue = VirtualIssue::parse_virtual(&content, conflict_fpath)?;
			let project_meta = Local::load_project_meta(issue_index.repo_info());
			let issue = Issue::from_combined(HollowIssue::default(), virtual_issue, issue_index.parent().unwrap(), project_meta.virtual_project);

			//Q: we should preserve the timestamps of original issues post resolution.
			//C [consequence]: so we must have actual IssueIndex of the issue being resolved, to get both sources
			//C: this should take IssueIndex; then
			//TODO: impl IssueIndex -> HollowIssue
		}

		conflict_resolution_cleanup(issue_index.owner())?;
		Ok(None)
	}
}

/// Remove the conflict file after successful resolution.
pub fn remove_conflict_file(owner: &str) -> Result<()> {
	let conflict_file = conflict_file_path(owner);
	if conflict_file.exists() {
		std::fs::remove_file(&conflict_file)?;
	}
	Ok(())
}
/// Outcome of initiating a conflict merge.
pub enum ConflictOutcome {
	/// Merge succeeded automatically (no conflicts).
	AutoMerged,
	/// Merge has conflicts that need user resolution.
	NeedsResolution,
	/// Both sides are identical, no merge needed.
	NoChanges,
}
/// Initiate a git merge conflict between local and remote issue states.
///
/// This creates a real git conflict by:
/// 1. Committing current local state
/// 2. Creating a `remote-state` branch with remote's state
/// 3. Merging that branch (which may produce conflicts)
///
/// Both local and remote are written to `{owner}/__conflict.md` in virtual format.
pub fn initiate_conflict_merge(repo_info: RepoInfo, issue_number: u64, local_issue: &Issue, remote_issue: &Issue) -> Result<ConflictOutcome, ConflictError> {
	if !is_git_initialized() {
		return Err(ConflictError::GitNotInitialized);
	}

	let owner = repo_info.owner();
	let repo = repo_info.repo();

	let data_dir = Local::issues_dir();
	let data_dir_str = data_dir.to_str().ok_or_else(|| ConflictError::GitError {
		message: "Invalid data directory path".into(),
	})?;

	// Ensure owner directory exists
	let owner_dir = data_dir.join(owner);
	std::fs::create_dir_all(&owner_dir)?;

	let conflict_file = conflict_file_path(owner);
	let conflict_file_rel = conflict_file.strip_prefix(&data_dir).unwrap_or(&conflict_file);
	let conflict_file_rel_str = conflict_file_rel.to_string_lossy();

	// Get current branch name
	let branch_output = Command::new("git").args(["-C", data_dir_str, "rev-parse", "--abbrev-ref", "HEAD"]).output()?;
	let current_branch = String::from_utf8_lossy(&branch_output.stdout).trim().to_string();

	// Write local state to conflict file (virtual format)
	let local_virtual = local_issue.serialize_virtual();
	std::fs::write(&conflict_file, &local_virtual)?;

	// Stage and commit local state
	let add_status = Command::new("git").args(["-C", data_dir_str, "add", "-A"]).status()?;
	if !add_status.success() {
		return Err(ConflictError::GitError {
			message: "git add -A failed".into(),
		});
	}

	let commit_msg = format!("__conflict: local state for {owner}/{repo}#{issue_number}");
	let commit_output = Command::new("git").args(["-C", data_dir_str, "commit", "-m", &commit_msg]).output()?;

	let local_committed = commit_output.status.success();
	if local_committed {
		tracing::debug!("[conflict] Committed local state");
	}

	// Get base commit (parent of our commit, or HEAD if nothing to commit)
	let base_commit = if local_committed {
		let parent_output = Command::new("git").args(["-C", data_dir_str, "rev-parse", "HEAD~1"]).output()?;
		if parent_output.status.success() {
			String::from_utf8_lossy(&parent_output.stdout).trim().to_string()
		} else {
			"HEAD".to_string()
		}
	} else {
		"HEAD".to_string()
	};

	// Delete remote-state branch if it exists
	let _ = Command::new("git").args(["-C", data_dir_str, "branch", "-D", "remote-state"]).output();

	// Create remote-state branch from base
	let branch_status = Command::new("git").args(["-C", data_dir_str, "branch", "remote-state", &base_commit]).status()?;

	if !branch_status.success() {
		return Err(ConflictError::GitError {
			message: "Failed to create remote-state branch".into(),
		});
	}

	// Checkout remote-state branch
	let checkout_status = Command::new("git").args(["-C", data_dir_str, "checkout", "remote-state"]).status()?;

	if !checkout_status.success() {
		cleanup_branch(data_dir_str, &current_branch);
		return Err(ConflictError::GitError {
			message: "Failed to checkout remote-state branch".into(),
		});
	}

	// Write remote state to conflict file (virtual format)
	let remote_virtual = remote_issue.serialize_virtual();
	std::fs::write(&conflict_file, &remote_virtual)?;

	// Stage and commit remote state
	let add_status = Command::new("git").args(["-C", data_dir_str, "add", "-A"]).status()?;
	if !add_status.success() {
		cleanup_branch(data_dir_str, &current_branch);
		return Err(ConflictError::GitError {
			message: "git add -A failed".into(),
		});
	}

	// Check if there are changes to commit
	let diff_status = Command::new("git").args(["-C", data_dir_str, "diff", "--cached", "--quiet"]).status()?;

	if diff_status.success() {
		// No changes - states are identical
		let _ = Command::new("git").args(["-C", data_dir_str, "checkout", &current_branch]).status();
		cleanup_branch(data_dir_str, &current_branch);
		return Ok(ConflictOutcome::NoChanges);
	}

	let remote_commit_msg = format!("__conflict: remote state for {owner}/{repo}#{issue_number}");
	let commit_status = Command::new("git").args(["-C", data_dir_str, "commit", "-m", &remote_commit_msg]).status()?;

	if !commit_status.success() {
		let _ = Command::new("git").args(["-C", data_dir_str, "checkout", &current_branch]).status();
		cleanup_branch(data_dir_str, &current_branch);
		return Err(ConflictError::GitError {
			message: "Failed to commit remote state".into(),
		});
	}

	// Switch back to original branch
	let _ = Command::new("git").args(["-C", data_dir_str, "checkout", &current_branch]).status()?;

	// Attempt merge
	tracing::debug!("[conflict] Attempting merge of remote-state into {current_branch}");
	let merge_output = Command::new("git")
		.args([
			"-C",
			data_dir_str,
			"merge",
			"remote-state",
			"-m",
			&format!("Merge remote state for {owner}/{repo}#{issue_number}"),
		])
		.output()?;

	if merge_output.status.success() {
		// Merge succeeded without conflicts
		cleanup_branch(data_dir_str, &current_branch);
		tracing::debug!("[conflict] Merge succeeded automatically");
		return Ok(ConflictOutcome::AutoMerged);
	}

	// Check if it's a conflict or other error
	let stdout = String::from_utf8_lossy(&merge_output.stdout);
	let stderr = String::from_utf8_lossy(&merge_output.stderr);

	//TODO: switch all actual git operations to [gitoxide](https://docs.rs/gitoxide/latest/gitoxide/), to not have to parse STDOUT
	if stdout.contains("CONFLICT") || stderr.contains("CONFLICT") || stdout.contains("Automatic merge failed") {
		tracing::debug!("[conflict] Merge produced conflicts in {conflict_file_rel_str}");
		// Don't cleanup branch - user needs it for resolution
		Ok(ConflictOutcome::NeedsResolution)
	} else {
		// Some other error
		let _ = Command::new("git").args(["-C", data_dir_str, "merge", "--abort"]).status();
		cleanup_branch(data_dir_str, &current_branch);
		Err(ConflictError::GitError {
			message: format!("Merge failed: {}\n{}", stdout.trim(), stderr.trim()),
		})
	}
}
/// Complete the conflict resolution process.
///
/// Call this after user has resolved conflicts and committed.
/// Cleans up the remote-state branch.
pub fn conflict_resolution_cleanup(owner: &str) -> Result<()> {
	let data_dir = Local::issues_dir();
	let data_dir_str = data_dir.to_str().ok_or_else(|| eyre!("Invalid data directory path"))?;

	// Ensure we're not in the middle of a merge
	if is_merge_in_progress() {
		bail!("Git merge is still in progress. Complete the merge first: git add <file> && git commit");
	}

	// Cleanup branch
	let _ = Command::new("git").args(["-C", data_dir_str, "branch", "-D", "remote-state"]).output();

	// Remove conflict file if it still exists (user may have already removed it)
	remove_conflict_file(owner)?;

	Ok(())
}

/// Check if file content contains git conflict markers.
fn has_conflict_markers(content: &str) -> bool {
	let has_ours = content.contains("<<<<<<<");
	let has_separator = content.contains("=======");
	let has_theirs = content.contains(">>>>>>>");
	has_ours && has_separator && has_theirs
}

/// Check if we're in the middle of a git merge.
fn is_merge_in_progress() -> bool {
	let data_dir = Local::issues_dir();
	let merge_head = data_dir.join(".git/MERGE_HEAD");
	merge_head.exists()
}

//==============================================================================
// Conflict Resolution
//==============================================================================

//==============================================================================
// Conflict Creation (Git Branch Merge)
//==============================================================================

/// Cleanup the remote-state branch after merge completes.
fn cleanup_branch(data_dir_str: &str, _current_branch: &str) {
	let _ = Command::new("git").args(["-C", data_dir_str, "branch", "-D", "remote-state"]).output();
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn test_has_conflict_markers() {
		// No markers
		assert!(!has_conflict_markers("# Normal issue\n\nSome body text."));

		// All three markers
		let content = r#"# Issue title

<<<<<<< HEAD
Local changes
=======
Remote changes
>>>>>>> remote-state
"#;
		assert!(has_conflict_markers(content));

		// Just separator (like markdown divider)
		assert!(!has_conflict_markers("# Issue\n\n=======\n\nSome divider"));

		// Two of three markers
		assert!(!has_conflict_markers("<<<<<<< HEAD\nSome text\n======="));
	}

	#[test]
	fn test_conflict_file_path() {
		let path = conflict_file_path("myowner");
		assert!(path.to_string_lossy().contains("myowner"));
		assert!(path.to_string_lossy().ends_with("__conflict.md"));
	}
}
