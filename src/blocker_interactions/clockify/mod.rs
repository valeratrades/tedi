//! Clockify time tracking integration for blockers.
//!
//! - `protocol`: Raw Clockify API interactions (start/stop entries, resolve IDs)
//! - `tracking`: Blocker-aware tracking state (halt/resume, automatic task switching)

mod protocol;
mod tracking;

// Re-export protocol types for direct clockify commands
pub use protocol::{ClockifyArgs, main as clockify_main};
// Re-export tracking for blocker integration
pub use tracking::{HaltArgs, ResumeArgs, is_tracking_enabled, set_tracking_enabled, start_tracking_for_task, stop_current_tracking};
