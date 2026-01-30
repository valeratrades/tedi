use std::{cell::Cell, path::PathBuf};

use jiff::Timestamp;
use tracing::instrument;

thread_local! {
	static MOCK_TIMESTAMP: Cell<Option<Timestamp>> = const { Cell::new(None) };
	static MOCK_ISSUES_DIR: Cell<Option<PathBuf>> = const { Cell::new(None) };
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
		MOCK_ISSUES_DIR.with(|dir| {
			dir.take().map(|d| {
				// Put it back since Cell::take removes it
				let result = d.clone();
				dir.set(Some(d));
				result
			})
		})
	}
}

/// Set the issues directory override for the current thread.
///
/// Used by test infrastructure to isolate each test's filesystem state.
#[instrument]
pub fn set_issues_dir(path: PathBuf) {
	MOCK_ISSUES_DIR.with(|dir| dir.set(Some(path)));
}

/// Clear the issues directory override for the current thread.
#[instrument]
pub fn clear_issues_dir() {
	MOCK_ISSUES_DIR.with(|dir| dir.set(None));
}
