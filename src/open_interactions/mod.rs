//! Github issue editing functionality.
//!
//! This module provides functionality for fetching Github issues,
//! editing them locally, and syncing changes back to Github.

mod command;
mod merge;
mod sync;
mod touch;

// Re-export the public API
pub use command::{OpenArgs, open_command};
// Re-export sync types for blocker interaction
#[allow(unused_imports)]
pub use sync::{MergeMode, Modifier, ModifyResult, Side, SyncOptions, modify_and_sync_issue};
