#![feature(default_field_values)]
//! Operations over the tedi primitives: local/remote sources and sinks,
//! sync/merge/touch/conflict flows, blocker stack operations, mocks.
//!
//! Depends on `tedi_core` (pure model) and `tedi_adapters` (transport).
#![feature(error_generic_member_access)]
#![allow(clippy::len_zero)]
#![allow(clippy::doc_lazy_continuation)]

use clap::ValueEnum;
pub use tedi_adapters::github;
pub mod clockify_tracking;
mod lazy;
pub mod local;
pub mod mock_github;
pub mod mocks;
pub mod open_interactions;
pub mod paths;
pub mod remote;
pub mod sink;
pub mod sprints;
pub mod utils;

pub use lazy::LazyIssue;
pub use tedi_core::current_user;
// Data model re-exported at the crate root so moved code keeps resolving `crate::X`.
pub use tedi_core::{
	BlockerItem, BlockerSetState, Blockers, CloseState, Comment, CommentIdentity, Comments, HollowIssue, Issue, IssueContents, IssueError, IssueIdentity, IssueIndex, IssueLink, IssueMarker,
	IssueRef, IssueSelector, IssueTimestamps, LinkedIssueMeta, MAX_INDEX_DEPTH, MAX_LINEAGE_DEPTH, MAX_TITLE_LENGTH, Marker, MilestoneLink, MilestoneRef, NodeLink, ParseError, RepoInfo,
	TaskItemId, TaskView, TitleInGitPathError, VirtualIssue, parse_blockers_from_embedded, split_blockers,
};
pub use tedi_md::{Events, Header, OwnedCodeBlockKind, OwnedEvent, OwnedTag, OwnedTagEnd};

/// Mock behavior type for testing.
#[derive(Clone, Copy, Debug, Default, ValueEnum)]
pub enum MockType {
	/// Standard mock - uses mock Github client but normal editor flow.
	#[default]
	#[value(name = "")]
	Standard,
	/// Ghost edit - skip editor and pretend edit was made.
	GhostEdit,
}
