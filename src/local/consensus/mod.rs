//! Consensus state management for issue sync.
//!
//! The consensus state (last synced state) is stored in git.
//! This module provides:
//! - Loading consensus issues via `LazyIssue<LocalIssueSource<GitReader>>`
//! - Committing changes via `Sink<Consensus>`

/// Load the consensus Issue tree from git (last committed state).
///
/// Uses `Issue::load(LocalIssueSource<GitReader>)` to read from git HEAD.
///
/// Returns:
/// - `Ok(Some(Issue))` if file is tracked and consensus loaded successfully
/// - `Ok(None)` if file is not tracked (new file, no consensus yet)
/// - `Err(LocalError)` if file exists but failed to load
#[deprecated(note = "pretty sure that's duplication over Issue::load(LocalIssueSource::<GitReader>::build(index.into()))")]
pub async fn load_consensus_issue(index: IssueIndex) -> Result<Option<Issue>, LocalError> {
	let source = LocalIssueSource::<GitReader>::build(LocalPath::new(index))?;

	// Check if the file exists in git
	let search_result = source.local_path.clone().resolve_parent(source.reader).and_then(|r| r.search());
	tracing::debug!(?search_result, "load_consensus_issue search");
	if search_result.is_err() {
		return Ok(None);
	}
	Issue::load(source).await.map(Some)
}

mod git;
mod sink;

pub use git::is_git_initialized;
pub use sink::Consensus;

use super::{GitReader, LocalError, LocalIssueSource, LocalPath};
use crate::{Issue, IssueIndex, LazyIssue};
