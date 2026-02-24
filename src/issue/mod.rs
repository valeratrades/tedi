//! Issue file format standard.
//!
//! This module contains the canonical representation of issue files,
//! including parsing, serialization, and all pure types.
//!
//! The issue format is designed for local-first issue tracking with
//! optional Github synchronization.

mod blocker;
pub(crate) mod issue_ref;
pub use issue_ref::IssueRef;
pub mod milestone_embed;
pub use blocker::{BlockerItem, BlockerSequence, BlockerSetState, MilestoneBlockerCache, join_with_blockers, split_blockers};
pub use milestone_embed::{MilestoneDoc, parse_blockers_from_embedded, serialize_blockers_view};

mod events;
pub use events::{Events, OwnedCodeBlockKind, OwnedEvent, OwnedTag, OwnedTagEnd};

mod error;
pub use error::{IssueError, ParseError, TitleInGitPathError};
mod marker;
pub use marker::{IssueMarker, Marker};

mod types;
pub use types::{
	CloseState, Comment, CommentIdentity, Comments, HollowIssue, Issue, IssueContents, IssueIdentity, IssueIndex, IssueLink, IssueSelector, IssueTimestamps, LazyIssue, LinkedIssueMeta,
	MAX_INDEX_DEPTH, MAX_LINEAGE_DEPTH, MAX_TITLE_LENGTH, RepoInfo, VirtualIssue,
};

// Re-export Header from parent
pub use crate::Header;
