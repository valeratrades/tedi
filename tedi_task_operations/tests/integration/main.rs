//! Integration tests entry point, following https://matklad.github.io/2021/02/27/delete-cargo-integration-tests.html

mod common;
pub use common::*;

mod assignees;
mod blocker_dependencies;
mod blocker_integrated;
mod closed_subtree;
mod conflict;
mod file_naming;
mod issue_preservation;
mod label_scheme;
mod linked_prs;
mod pull;
mod reset_conflict;
mod search;
mod sprints;
mod store_lock;
mod sync;
mod touch;
mod touch_parent;

#[ctor::ctor]
fn init() {
	v_utils::install_color_eyre!();
	miette::set_hook(Box::new(|_| Box::new(miette::MietteHandlerOpts::new().terminal_links(true).build()))).expect("miette hook already set");
}
