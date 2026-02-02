//! Integration tests entry point, following https://matklad.github.io/2021/02/27/delete-cargo-integration-tests.html

mod common;
pub use common::*;

mod blocker_integrated;
mod file_naming;
mod issue_preservation;
mod reset_conflict;
mod sync;
mod touch;
mod touch_parent;
