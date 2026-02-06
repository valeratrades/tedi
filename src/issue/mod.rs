//! Issue file format standard.
//!
//! This module contains the canonical representation of issue files,
//! including parsing, serialization, and all pure types.
//!
//! The issue format is designed for local-first issue tracking with
//! optional Github synchronization.

mod blocker;
pub use blocker::{BlockerItem, BlockerSequence, DisplayFormat, HeaderLevel, Line, classify_line, join_with_blockers, split_blockers};

mod events;
pub use events::{Events, OwnedCodeBlockKind, OwnedEvent, OwnedTag, OwnedTagEnd};

mod error;
pub use error::{IssueError, ParseError, TitleInGitPathError};
mod marker;
pub use marker::{IssueMarker, Marker};

mod types;
pub use types::{
	CloseState, Comment, CommentIdentity, HollowIssue, Issue, IssueContents, IssueIdentity, IssueIndex, IssueLink, IssueSelector, IssueTimestamps, LazyIssue, LinkedIssueMeta,
	MAX_INDEX_DEPTH, MAX_LINEAGE_DEPTH, MAX_TITLE_LENGTH, RepoInfo, VirtualIssue,
};

// Re-export Header from parent
pub use crate::Header;
