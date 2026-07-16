#![feature(default_field_values)]
//! What we *do* with tedi issues: sync/merge/touch/conflict-resolution flows,
//! sprints, selection, blocker stack operations, time tracking.
//!
//! Depends on `tedi_task_primitives` (what an issue is). The reverse never holds —
//! the directional boundary is compiler-enforced.
#![feature(error_generic_member_access)]
#![allow(clippy::len_zero)]
#![allow(clippy::doc_lazy_continuation)]

// Re-export the primitive surface at the crate root so moved operation code keeps
// resolving `crate::X` (model types, `local`, `remote`, `sink`, `paths`, …).
pub use tedi_task_primitives::*;

pub mod clockify_tracking;
pub mod conflict_resolve;
pub mod open_interactions;
pub mod selection;
pub mod sprints;
