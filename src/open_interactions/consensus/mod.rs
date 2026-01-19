//! Consensus state management for issue sync.
//!
//! The consensus state (last synced state) is stored in git.
//! This module provides:
//! - Loading consensus issues via `LazyIssue<Local>` with `LocalSource::Consensus`
//! - Committing changes as the new consensus

mod git;

use std::path::Path;

pub use git::{commit_issue_changes, is_git_initialized};
use tedi::Issue;

use super::{
	local::{Local, LocalError, LocalPath},
	sink::IssueLoadExt,
};

/// Load the consensus Issue tree from git (last committed state).
///
/// Uses `LazyIssue<Local>` with `LocalSource::Consensus` to read from git HEAD.
///
/// Returns:
/// - `Ok(Some(Issue))` if file is tracked and consensus loaded successfully
/// - `Ok(None)` if file is not tracked (new file, no consensus yet)
/// - `Err(LocalError)` if file exists but failed to load
pub async fn load_consensus_issue(file_path: &Path) -> Result<Option<Issue>, LocalError> {
	let source = LocalPath::consensus(file_path.to_path_buf());

	// Check if the file exists in git
	if Local::read_content(&source).is_none() {
		return Ok(None);
	}

	Issue::load_local(source).await.map(Some)
}
