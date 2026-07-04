//! Blocker management module.
//!
//! This module provides functionality for managing "blockers" - a stack-based task tracking system
//! where you work on one thing at a time. The core philosophy is:
//! - Forces prioritization (high leverage)
//! - Solving top 1 thing can often unlock many smaller ones for free
//!
//! # Architecture
//!
//! - `io`: CLI definitions and entry point
//! - `integration`: Issue-based blocker implementation (uses modify_and_sync_issue)
//! - `operations`: Extended operations on Blockers (pop, add, etc.)
//! - `source`: BlockerSource trait for data access abstraction
//! - `clockify_tracking`: Blocker-aware time tracking state over the `tedi_adapters::clockify` protocol
//!
//! Urgent mode stores blockers in `issues/{owner}/urgent.md` - a simple blocker list
//! without Github sync. Only one urgent file can exist at a time.
//!
//! Core types (BlockerItem, Blockers) are defined in the
//! library crate and re-exported here for convenience.

pub mod clockify_tracking;
pub(super) mod integration;
mod io;
mod operations;
mod source;

// Re-export core types from library
// Re-export the CLI API

use color_eyre::eyre::Result;
pub use io::BlockerArgs;
// Re-export extended operations
pub use operations::BlockersExt;

pub use crate::Blockers;

/// Main entry point for blocker commands
pub async fn main(args: BlockerArgs, offline: bool, yes: bool) -> Result<()> {
	io::main(args, offline, yes).await
}
