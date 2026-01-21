//! Github issue editing functionality.
//!
//! This module provides functionality for fetching Github issues,
//! editing them locally, and syncing changes back to Github.

mod command;
mod conflict;
pub(crate) mod consensus;
mod merge;
pub(crate) mod remote;
mod sync;
mod touch;
pub(crate) mod util;

// Re-export from library
// Re-export the public API
pub use command::{OpenArgs, open_command};
// Re-export sync types for blocker integration
pub use sync::{MergeMode, Modifier, ModifyResult, Side, SyncOptions, modify_and_sync_issue, modify_issue_offline};
// Re-export Issue from the library crate
pub use tedi::Issue;
pub use tedi::{local, sink};
