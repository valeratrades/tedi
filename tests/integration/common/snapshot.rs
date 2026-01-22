//! Snapshot helpers for integration tests.

use v_fixtures::{Fixture, FixtureRenderer};

use super::TestContext;

/// Capture the issues directory as a snapshot string.
///
/// Returns a fixture-format string showing all files in the issues directory.
/// Automatically normalizes git hashes in conflict markers.
pub fn snapshot_issues_dir(ctx: &TestContext) -> String {
	snapshot_issues_dir_redacting(ctx, &[])
}

/// Redact specific lines from a snapshot output.
///
/// # Why this exists
///
/// Some timestamps in `.meta.json` are set via `Timestamp::now()` when the code
/// detects local changes (e.g., user closes an issue). These timestamps are
/// non-deterministic and change between test runs, causing snapshot failures.
///
/// We can't easily parse and redact them because:
/// - The JSON is embedded in a multi-file fixture format
/// - Timestamps from seeded test data vs `now()` are indistinguishable by format
/// - Only certain fields (like `state`) get `now()` timestamps in certain tests
///
/// So we take the ugly-but-explicit approach: callers specify which output lines
/// to redact. This makes it obvious in the test which values are non-deterministic.
///
/// # Arguments
/// * `lines_to_redact` - 1-indexed line numbers to replace with "[REDACTED]"
pub fn snapshot_issues_dir_redacting(ctx: &TestContext, lines_to_redact: &[usize]) -> String {
	let issues_dir = ctx.xdg.data_dir().join("issues");

	let Some(fixture) = Fixture::read_from_directory(&issues_dir) else {
		return String::from("(empty - issues directory does not exist)");
	};

	if fixture.files.is_empty() {
		return String::from("(empty - no files in issues directory)");
	}

	FixtureRenderer::new(&fixture)
		.normalize_git_hashes()
		.redact_lines(lines_to_redact)
		.redact_message("        [REDACTED - non-deterministic timestamp]")
		.render()
}
