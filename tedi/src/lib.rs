#![feature(default_field_values)]
#![feature(vec_peek_mut)]
#![feature(error_generic_member_access)]

pub use tedi_adapters::github;
pub mod issue;
pub mod local;
pub mod mocks;
pub mod remote;
pub mod sink;

pub mod current_user {
	use std::sync::RwLock;

	static CURRENT_USER: RwLock<Option<String>> = RwLock::new(None);

	/// Set the current authenticated user for ownership checks.
	/// Must be called before serializing issues.
	pub fn set(user: String) {
		*CURRENT_USER.write().unwrap() = Some(user);
	}

	/// Get the current authenticated user.
	/// Returns None if not set.
	pub fn get() -> Option<String> {
		CURRENT_USER.read().unwrap().clone()
	}

	/// Check if the given user is the current authenticated user.
	pub fn is(user: &str) -> bool {
		CURRENT_USER.read().unwrap().as_deref() == Some(user)
	}
}

// Re-export all public types from issue module at crate root for convenience
pub use issue::{
	BlockerItem, BlockerSequence, BlockerSetState, CloseState, Comment, CommentIdentity, Comments, Events, HollowIssue, Issue, IssueContents, IssueError, IssueIdentity, IssueIndex,
	IssueLink, IssueMarker, IssueRef, IssueSelector, IssueTimestamps, LazyIssue, LinkedIssueMeta, MAX_INDEX_DEPTH, MAX_LINEAGE_DEPTH, MAX_TITLE_LENGTH, Marker, MilestoneDoc,
	OwnedCodeBlockKind, OwnedEvent, OwnedTag, OwnedTagEnd, ParseError, RepoInfo, TitleInGitPathError, VirtualIssue, join_with_blockers, parse_blockers_from_embedded,
	serialize_blockers_view, split_blockers,
};
pub use tedi_md::Header;
