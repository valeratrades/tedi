//! Conflict detection: file paths, marker checks, and the pure "is a conflict pending?" query.
//!
//! Resolution (opening the editor, sinking the resolved state, driving the git merge) lives
//! in the operation layer (`tedi_task_operations::conflict_resolve`). This module only *detects*.
//!
//! When local and remote diverge, resolution creates a real git merge conflict written to
//! `{owner}/__conflict.md` in virtual format (all children inlined) so standard git merge tools
//! work on a single file.

use std::path::PathBuf;

use miette::Diagnostic;
use thiserror::Error;
use v_utils::{macros::wrap_err, prelude::*};

use super::Local;
use crate::IssueIndex;

//==============================================================================
// Error Types
//==============================================================================

/// Error returned when there are unresolved conflicts blocking operations.
#[wrap_err]
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
#[wrap_err]
#[derive(Debug, Error)]
pub enum ConflictError {
	#[leaf]
	#[error("git is not initialized in issues directory")]
	GitNotInitialized,

	#[leaf]
	#[error("git operation failed: {message}")]
	GitError { message: String },

	#[foreign]
	Io(std::io::Error),
}

//==============================================================================
// Conflict File Path
//==============================================================================

/// Get the conflict file path for a given owner.
/// Format: `{issues_dir}/{owner}/__conflict.md`
pub fn conflict_file_path(owner: &str) -> PathBuf {
	Local::issues_dir().join(owner).join("__conflict.md")
}

/// Milestone conflict file — distinct from the issue one so the two never collide.
/// Format: `{issues_dir}/{owner}/__milestone_conflict.md`
pub fn milestone_conflict_file_path(owner: &str) -> PathBuf {
	Local::issues_dir().join(owner).join("__milestone_conflict.md")
}

//==============================================================================
// Conflict Detection
//==============================================================================

/// Outcome of initiating a conflict merge.
pub enum ConflictOutcome {
	/// Merge succeeded automatically (no conflicts).
	AutoMerged,
	/// Merge has conflicts that need user resolution.
	NeedsResolution,
	/// Both sides are identical, no merge needed.
	NoChanges,
}

/// A pending (unresolved) conflict blocking `index`: the conflict file exists and still carries
/// git markers. Pure — no editor, no sink. Virtual issues never sync, so they can never conflict.
pub fn pending_conflict(index: IssueIndex) -> Result<Option<PathBuf>> {
	if index.repo_info().is_virtual() {
		return Ok(None);
	}
	let conflict_fpath = conflict_file_path(index.owner().expect("github project"));
	if !conflict_fpath.exists() {
		return Ok(None);
	}
	let content = std::fs::read_to_string(&conflict_fpath)?;
	Ok(has_conflict_markers(&content).then_some(conflict_fpath))
}

/// Check if file content contains git conflict markers.
pub fn has_conflict_markers(content: &str) -> bool {
	let has_ours = content.contains("<<<<<<<");
	let has_separator = content.contains("=======");
	let has_theirs = content.contains(">>>>>>>");
	has_ours && has_separator && has_theirs
}

/// Check if we're in the middle of a git merge.
pub fn is_merge_in_progress() -> bool {
	let data_dir = Local::issues_dir();
	let merge_head = data_dir.join(".git/MERGE_HEAD");
	merge_head.exists()
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
