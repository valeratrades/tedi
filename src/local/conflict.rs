//! Conflict detection primitives for local issue storage.
//!
//! Pure filesystem checks — no remote/sync logic.
//! Dynamic conflict resolution lives in `open_interactions::conflict`.

use std::path::PathBuf;

use miette::Diagnostic;
use thiserror::Error;

use super::Local;

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

/// Remove the conflict file after successful resolution.
pub fn remove_conflict_file(owner: &str) -> Result<(), std::io::Error> {
	let conflict_file = conflict_file_path(owner);
	if conflict_file.exists() {
		std::fs::remove_file(&conflict_file)?;
	}
	Ok(())
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
