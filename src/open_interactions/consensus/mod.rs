//! Consensus state management for issue sync.
//!
//! The consensus state (last synced state) is stored in git.
//! This module provides:
//! - Loading consensus issues via `LazyIssue<Local>` with `LocalSource::Consensus`
//! - Committing changes as the new consensus

mod git;

use std::path::Path;

pub use git::{commit_issue_changes, is_git_initialized};
use todo::{Ancestry, Issue, LazyIssue};

use super::local::{Local, LocalPath};

/// Load the consensus Issue tree from git (last committed state).
///
/// Uses `LazyIssue<Local>` with `LocalSource::Consensus` to read from git HEAD.
///
/// Returns:
/// - `Some(Issue)` if file is tracked and consensus loaded successfully
/// - `None` if file is not tracked (new file, no consensus yet)
pub async fn load_consensus_issue(file_path: &Path) -> Option<Issue> {
	let source = LocalPath::consensus(file_path.to_path_buf());

	// Check if the file exists in git
	if Local::read_content(&source).is_none() {
		return None;
	}

	let (owner, repo) = Local::extract_owner_repo(file_path).ok()?;
	let ancestry = Ancestry::root(&owner, &repo);

	let mut issue = Issue::empty_local(ancestry);

	<Issue as LazyIssue<Local>>::identity(&mut issue, source.clone()).await;
	<Issue as LazyIssue<Local>>::contents(&mut issue, source.clone()).await;
	Box::pin(<Issue as LazyIssue<Local>>::children(&mut issue, source)).await;

	Some(issue)
}
