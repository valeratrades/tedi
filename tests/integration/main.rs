//! Integration tests entry point, following https://matklad.github.io/2021/02/27/delete-cargo-integration-tests.html

#[ctor::ctor]
fn init() {
	v_utils::install_color_eyre!();
	miette::set_hook(Box::new(|_| Box::new(miette::MietteHandlerOpts::new().terminal_links(true).build()))).expect("miette hook already set");
}

mod common;
pub use common::*;

mod blocker_integrated;
mod file_naming;
mod issue_preservation;
mod reset_conflict;
mod sync;
mod touch;
mod touch_parent;
