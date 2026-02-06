//! Github issue editing functionality.
//!
//! This module provides functionality for fetching Github issues,
//! editing them locally, and syncing changes back to Github.

mod command;
mod merge;
pub(crate) mod remote;
mod sync;
mod touch;
pub(crate) mod util;

// Re-export the public API
pub use command::{OpenArgs, open_command};
pub use remote::{Remote, RemoteError};
// Re-export sync types for blocker integration
#[allow(unused_imports)]
pub use sync::{MergeMode, Modifier, ModifyResult, Side, SyncOptions, modify_and_sync_issue};
pub use tedi::local;
pub(crate) use tedi::local::{conflict, consensus};
