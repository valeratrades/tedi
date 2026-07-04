#![feature(default_field_values)]
#![feature(vec_peek_mut)]
#![feature(error_generic_member_access)]

pub use tedi_adapters::github;
pub mod issue;
pub mod local;
pub mod mocks;
pub mod remote;
pub mod sink;

// Re-export all public types from issue module at crate root for convenience
pub use issue::{
	BlockerItem, BlockerSetState, Blockers, CloseState, Comment, CommentIdentity, Comments, Events, HollowIssue, Issue, IssueContents, IssueError, IssueIdentity, IssueIndex, IssueLink,
	IssueMarker, IssueRef, IssueSelector, IssueTimestamps, LazyIssue, LinkedIssueMeta, MAX_INDEX_DEPTH, MAX_LINEAGE_DEPTH, MAX_TITLE_LENGTH, Marker, MilestoneDoc, OwnedCodeBlockKind,
	OwnedEvent, OwnedTag, OwnedTagEnd, ParseError, RepoInfo, TitleInGitPathError, VirtualIssue, parse_blockers_from_embedded, serialize_blockers_view, split_blockers,
};
pub use tedi_core::current_user;
pub use tedi_md::Header;
