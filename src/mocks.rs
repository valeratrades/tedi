use std::{
	cell::{Cell, RefCell},
	path::PathBuf,
};

use jiff::Timestamp;
use tracing::instrument;

thread_local! {
	static MOCK_TIMESTAMP: Cell<Option<Timestamp>> = const { Cell::new(None) };
	static MOCK_ISSUES_DIR: RefCell<Option<PathBuf>> = const { RefCell::new(None) };
}

pub struct MockTimestamp;

impl MockTimestamp {
	#[instrument(name = "MockTimestamp::now")]
	pub fn now() -> Timestamp {
		let ts = MOCK_TIMESTAMP.with(|ts| ts.get());
		tracing::debug!(?ts, "returning mock timestamp");
		ts.unwrap_or_else(Timestamp::now)
	}
}

#[instrument]
pub fn set_timestamp(timestamp: Timestamp) {
	MOCK_TIMESTAMP.with(|ts| ts.set(Some(timestamp)));
}

/// Override for `Local::issues_dir()` used in tests.
///
/// When set, `Local::issues_dir()` returns this path instead of the XDG-based default.
/// This allows tests to isolate their filesystem state per-thread.
pub struct MockIssuesDir;

impl MockIssuesDir {
	/// Get the overridden issues directory, if set.
	pub fn get() -> Option<PathBuf> {
		MOCK_ISSUES_DIR.with(|dir| dir.borrow().clone())
	}
}

/// Set the issues directory override for the current thread.
///
/// Used by test infrastructure to isolate each test's filesystem state.
#[instrument]
pub fn set_issues_dir(path: PathBuf) {
	MOCK_ISSUES_DIR.with(|dir| *dir.borrow_mut() = Some(path));
}

/// Clear the issues directory override for the current thread.
#[instrument]
pub fn clear_issues_dir() {
	MOCK_ISSUES_DIR.with(|dir| *dir.borrow_mut() = None);
}

/// Override for `Local::virtual_edit_path()` base directory used in tests.
///
/// When set, `Local::virtual_edit_path()` uses this directory instead of `/tmp/{PKG_NAME}`.
/// This allows tests to isolate their virtual edit state per-test.
pub struct MockVirtualEditDir;

impl MockVirtualEditDir {
	/// Get the overridden virtual edit directory, if set.
	pub fn get() -> Option<PathBuf> {
		MOCK_VIRTUAL_EDIT_DIR.with(|dir| dir.borrow().clone())
	}
}

/// Set the virtual edit directory override for the current thread.
///
/// Used by test infrastructure to isolate each test's virtual edit state.
#[instrument]
pub fn set_virtual_edit_dir(path: PathBuf) {
	MOCK_VIRTUAL_EDIT_DIR.with(|dir| *dir.borrow_mut() = Some(path));
}

/// Clear the virtual edit directory override for the current thread.
#[instrument]
pub fn clear_virtual_edit_dir() {
	MOCK_VIRTUAL_EDIT_DIR.with(|dir| *dir.borrow_mut() = None);
}
