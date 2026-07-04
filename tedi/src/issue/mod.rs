//! Issue file format glue for the tedi binary.
//!
//! The pure Issue model now lives in `tedi_core`; this module re-exports it
//! for convenience and hosts the tedi-side pieces (lazy loading, milestone
//! embedding) that still depend on IO/app context.

mod lazy;
pub mod milestone_embed;
pub use lazy::LazyIssue;
pub use milestone_embed::{MilestoneDoc, parse_blockers_from_embedded, serialize_blockers_view};
pub use tedi_core::{
	BlockerItem, BlockerSetState, Blockers, CloseState, Comment, CommentIdentity, Comments, HollowIssue, Issue, IssueContents, IssueError, IssueIdentity, IssueIndex, IssueLink, IssueMarker,
	IssueRef, IssueSelector, IssueTimestamps, LinkedIssueMeta, MAX_INDEX_DEPTH, MAX_LINEAGE_DEPTH, MAX_TITLE_LENGTH, Marker, ParseError, RepoInfo, TitleInGitPathError, VirtualIssue,
	split_blockers,
};
pub use tedi_md::{Events, Header, OwnedCodeBlockKind, OwnedEvent, OwnedTag, OwnedTagEnd};
