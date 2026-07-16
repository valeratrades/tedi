//! Lazy-loading protocol for issues.
//!
//! A source (local fs, remote GitHub) implements `LazyIssue` to populate an
//! `Issue` on demand. Lives beside its sources, not in the pure model.

use tedi_core::{Issue, IssueChildren, IssueContents, IssueIdentity, IssueIndex};

/// Trait for lazily loading an issue from a source.
///
/// `S` is the source type directly (e.g., `LocalIssueSource<FsReader>`, `RemoteSource`).
/// Methods load data on demand; `&mut self` allows caching intermediate results.
#[allow(async_fn_in_trait)]
pub trait LazyIssue<S: Clone + std::fmt::Debug>: Sized {
	/// Error type for operations on this source.
	type Error: std::error::Error;

	/// Resolve parent_index from the source.
	/// Returns None for root-level issues (no parent).
	async fn parent_index(source: &S) -> Result<Option<IssueIndex>, Self::Error>;
	async fn identity(&mut self, source: S) -> Result<IssueIdentity, Self::Error>;
	async fn contents(&mut self, source: S) -> Result<IssueContents, Self::Error>;
	async fn children(&mut self, source: S) -> Result<IssueChildren<Issue>, Self::Error>;

	/// Load a full issue tree from the source.
	/// Default implementation calls parent_index, then populates identity, contents, and children.
	async fn load(source: S) -> Result<Issue, Self::Error>
	where
		Issue: LazyIssue<S, Error = Self::Error>, {
		let parent_index = <Issue as LazyIssue<S>>::parent_index(&source).await?.expect("load requires parent_index to be Some");
		let mut issue = Issue::empty_local(parent_index);
		<Issue as LazyIssue<S>>::identity(&mut issue, source.clone()).await?;
		<Issue as LazyIssue<S>>::contents(&mut issue, source.clone()).await?;
		Box::pin(<Issue as LazyIssue<S>>::children(&mut issue, source)).await?;
		Ok(issue)
	}
}
