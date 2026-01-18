//! Github issue editing functionality.
//!
//! This module provides functionality for fetching Github issues,
//! editing them locally, and syncing changes back to Github.

mod command;
mod conflict;
mod fetch;
pub(crate) mod files;
mod git;
mod github_sync;
pub(crate) mod meta;
mod sink;
mod sync;
mod touch;
mod tree;
pub(crate) mod util;

// Re-export the public API
pub use command::{OpenArgs, open_command};
// Re-export sync types for blocker integration
pub use sync::{MergeMode, Modifier, ModifyResult, Side, SyncOptions, modify_and_sync_issue, modify_issue_offline};
// Re-export Issue from the library crate
pub use todo::Issue;
