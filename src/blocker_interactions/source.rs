//! Data source abstraction for blocker storage.
//!
//! This module defines the `BlockerSource` trait for reading blockers from issue files.
//! Writing is handled through `modify_and_sync_issue` to ensure proper Github sync.

use color_eyre::eyre::Result;

use super::BlockerSequence;

/// Trait for blocker data sources.
/// Used for reading blocker content and displaying source information.
/// Note: Writing is done through `modify_and_sync_issue`, not through this trait.
pub trait BlockerSource {
	/// Load the blocker sequence
	fn load(&self) -> Result<BlockerSequence>;

	/// Get a display name for this source (for user messages)
	fn display_name(&self) -> String;

	/// Get the hierarchy for context display (e.g., project name).
	/// Returns parsed hierarchy components ready for use.
	fn hierarchy(&self) -> Vec<String>;
}
