//! Pure todo primitives: the data model (issues, blockers, locators, markers)
//! plus parse/serialize over `tedi_md::Events`. No IO, no async, no reqwest.
#![feature(error_generic_member_access)]

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

pub mod blockers;
pub mod error;
pub mod issue;
pub mod locate;
pub mod marker;

pub use blockers::{BlockerItem, BlockerSetState, Blockers, split_blockers};
pub use error::{IssueError, ParseContext, ParseError, TitleInGitPathError};
pub use issue::{CloseState, Comment, CommentIdentity, Comments, HollowIssue, Issue, IssueContents, IssueIdentity, IssueTimestamps, LinkedIssueMeta, VirtualIssue};
pub use locate::{IssueChildren, IssueIndex, IssueIndexParseError, IssueLink, IssueRef, IssueSelector, MAX_INDEX_DEPTH, MAX_LINEAGE_DEPTH, MAX_TITLE_LENGTH, RepoInfo, parse_repo_context};
pub use marker::{IssueMarker, Marker};
pub use tedi_md::{Events, Header, OwnedCodeBlockKind, OwnedEvent, OwnedTag, OwnedTagEnd};
