//! Issue file format standard.
//!
//! This module contains the canonical representation of issue files,
//! including parsing, serialization, and all pure types.
//!
//! The issue format is designed for local-first issue tracking with
//! optional Github synchronization.

pub mod milestone_embed;
pub use milestone_embed::{MilestoneDoc, parse_blockers_from_embedded, serialize_blockers_view};
pub use tedi_core::{
	BlockerItem, BlockerSetState, Blockers, IssueError, IssueIndex, IssueLink, IssueMarker, IssueRef, IssueSelector, MAX_INDEX_DEPTH, MAX_LINEAGE_DEPTH, MAX_TITLE_LENGTH, Marker,
	ParseError, RepoInfo, TitleInGitPathError, split_blockers,
};
pub use tedi_md::{Events, Header, OwnedCodeBlockKind, OwnedEvent, OwnedTag, OwnedTagEnd};

mod types;
pub use types::{CloseState, Comment, CommentIdentity, Comments, HollowIssue, Issue, IssueContents, IssueIdentity, IssueTimestamps, LazyIssue, LinkedIssueMeta, VirtualIssue};
