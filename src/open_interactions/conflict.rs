//! Conflict detection and blocking for merge sync semantics.
//!
//! When local and remote diverge, git merge leaves conflict markers in the file.
//! We record which files have conflicts, and block ALL operations until resolved.
//! Resolution is detected automatically by checking for conflict markers in the file.

use std::path::PathBuf;

use miette::Diagnostic;
use thiserror::Error;
use v_utils::prelude::*;

/// Get the conflicts directory
fn conflicts_dir() -> PathBuf {
	v_utils::xdg_state_dir!("conflicts")
}

/// Error returned when there are unresolved conflicts blocking operations
#[derive(Debug, Diagnostic, Error)]
#[error("Unresolved conflict blocks all operations")]
#[diagnostic(
	code(tedi::conflict::unresolved),
	help("Resolve the conflict markers (<<<<<<< ======= >>>>>>>), then stage and commit.\n\n{}", file_path.display())
)]
pub struct ConflictBlockedError {
	pub file_path: PathBuf,
}

/// Record that a file has conflicts that need resolution.
pub fn mark_conflict(file_path: &std::path::Path) -> Result<()> {
	let conflicts_dir = conflicts_dir();
	std::fs::create_dir_all(&conflicts_dir)?;

	// Use a hash of the path as filename to avoid nested directories
	let hash = {
		use std::hash::{Hash, Hasher};
		let mut hasher = std::collections::hash_map::DefaultHasher::new();
		file_path.hash(&mut hasher);
		hasher.finish()
	};

	let marker_path = conflicts_dir.join(format!("{hash:x}.conflict"));
	std::fs::write(&marker_path, file_path.to_string_lossy().as_bytes())?;
	Ok(())
}

/// Check if file content contains git conflict markers.
fn has_conflict_markers(content: &str) -> bool {
	// Git conflict markers: <<<<<<< (ours), ======= (separator), >>>>>>> (theirs)
	// All three must be present for it to be a real conflict
	let has_ours = content.contains("<<<<<<<");
	let has_separator = content.contains("=======");
	let has_theirs = content.contains(">>>>>>>");
	has_ours && has_separator && has_theirs
}

/// Check for any unresolved conflicts. Call this before any operation.
/// Returns Ok(()) if no conflicts, or Err with the first unresolved conflict file.
/// Automatically clears resolved conflicts (files without markers).
pub fn check_any_conflicts() -> Result<(), ConflictBlockedError> {
	let conflicts_dir = conflicts_dir();
	if !conflicts_dir.exists() {
		return Ok(());
	}

	let entries = match std::fs::read_dir(&conflicts_dir) {
		Ok(e) => e,
		Err(_) => return Ok(()),
	};

	for entry in entries.flatten() {
		let marker_path = entry.path();
		if marker_path.extension().map(|e| e == "conflict").unwrap_or(false) {
			// Read the file path from the marker
			let file_path_str = match std::fs::read_to_string(&marker_path) {
				Ok(s) => s,
				Err(_) => {
					// Can't read marker, remove it
					let _ = std::fs::remove_file(&marker_path);
					continue;
				}
			};

			let file_path = PathBuf::from(&file_path_str);

			// Check if the file still has conflict markers
			let content = match std::fs::read_to_string(&file_path) {
				Ok(c) => c,
				Err(_) => {
					// File doesn't exist anymore, remove marker
					let _ = std::fs::remove_file(&marker_path);
					continue;
				}
			};

			if has_conflict_markers(&content) {
				// Still has conflicts - block
				return Err(ConflictBlockedError { file_path });
			} else {
				// Resolved! Remove the marker
				let _ = std::fs::remove_file(&marker_path);
			}
		}
	}

	Ok(())
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
}
