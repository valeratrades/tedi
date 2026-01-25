//! Consensus state management for issue sync.
//!
//! The consensus state (last synced state) is stored in git.
//! This module provides:
//! - Loading consensus issues via `LazyIssue<LocalConsensus>` with `GitReader`
//! - Committing changes as the new consensus

/// Load the consensus Issue tree from git (last committed state).
///
/// Uses `LazyIssue<LocalConsensus>` to read from git HEAD.
///
/// Returns:
/// - `Ok(Some(Issue))` if file is tracked and consensus loaded successfully
/// - `Ok(None)` if file is not tracked (new file, no consensus yet)
/// - `Err(LocalError)` if file exists but failed to load
pub async fn load_consensus_issue(index: IssueIndex) -> Result<Option<Issue>, LocalError> {
	let source = LocalIssueSource::<GitReader>::from(index);

	// Check if the file exists in git
	let mut local_path = source.local_path.clone();
	if local_path.find_file_path(&source.reader).is_err() {
		return Ok(None);
	}

	<Issue as LazyIssue<LocalConsensus>>::load(source).await.map(Some)
}
mod git;

pub use git::{commit_issue_changes, is_git_initialized};

use super::{GitReader, LocalConsensus, LocalError, LocalIssueSource};
use crate::{Issue, IssueIndex, LazyIssue};
