//! Tests for --reset flag behavior with conflicts.
//!
//! Issue #46: --reset should not trigger conflicts when modifying issues after reset.
//! After `open --reset`, the consensus matches remote exactly, so any user edit
//! should be detected as LocalOnly (no merge needed).

use crate::common::{TestContext, parse_virtual};

/// After --reset on an issue with sub-issues, marking a sub-issue closed should succeed.
/// No conflict should occur because consensus == remote after reset.
#[test]
fn test_reset_with_subissue_edit() {
	let ctx = TestContext::build("");

	// Remote has parent with one open sub-issue
	let remote_vi = parse_virtual(
		"- [ ] Parent Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\n\
		 \x20 parent body\n\n\
		 \x20 - [ ] Sub Issue <!--sub @mock_user https://github.com/o/r/issues/2 -->\n\n\
		 \x20   sub body\n",
	);
	ctx.remote(&remote_vi, None);

	// User edits: mark sub-issue as closed
	let edited = parse_virtual(
		"- [ ] Parent Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\n\
		 \x20 parent body\n\n\
		 \x20 - [x] Sub Issue <!--sub @mock_user https://github.com/o/r/issues/2 -->\n\n\
		 \x20   sub body\n",
	);

	// --reset fetches remote and establishes consensus, then user edits
	let out = ctx.open_url(("o", "r").into(), 1).args(&["--reset"]).edit(&edited).run();

	assert!(
		out.status.success() && !out.stderr.contains("Conflict") && !out.stdout.contains("Merging"),
		"Should succeed without conflict or merge. stderr: {}, stdout: {}",
		out.stderr,
		out.stdout
	);
}

/// After --reset on a simple issue, editing the body should succeed without conflict.
#[test]
fn test_reset_with_body_edit() {
	let ctx = TestContext::build("");

	let remote_vi = parse_virtual("- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\n  original body\n");
	ctx.remote(&remote_vi, None);

	let edited = parse_virtual("- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\n  modified body\n");

	let out = ctx.open_url(("o", "r").into(), 1).args(&["--reset"]).edit(&edited).run();

	assert!(
		out.status.success() && !out.stderr.contains("Conflict"),
		"Should succeed without conflict. stderr: {}",
		out.stderr
	);
}

/// Issue #46 bug scenario: --reset should discard local modifications to sub-issue files.
///
/// Local has diverged from remote (added content + blockers to parent sub-issue).
/// After --reset, consensus should match remote exactly, so editing (marking child closed)
/// should succeed without conflict or merge.
#[tokio::test]
async fn test_reset_discards_local_subissue_modifications() {
	let ctx = TestContext::build("");

	// Remote: clean 3-level hierarchy
	let remote_vi = parse_virtual(
		"- [ ] Grandparent <!-- @mock_user https://github.com/o/r/issues/1 -->\n\n\
		 \x20 grandparent body\n\n\
		 \x20 - [ ] Parent <!--sub @mock_user https://github.com/o/r/issues/2 -->\n\n\
		 \x20   original parent body\n\n\
		 \x20   - [ ] Child <!--sub @mock_user https://github.com/o/r/issues/3 -->\n\n\
		 \x20     child body\n",
	);
	ctx.remote(&remote_vi, None);

	// Local: same hierarchy but parent sub-issue has local modifications
	let local_vi = parse_virtual(
		"- [ ] Grandparent <!-- @mock_user https://github.com/o/r/issues/1 -->\n\n\
		 \x20 grandparent body\n\n\
		 \x20 - [ ] Parent <!--sub @mock_user https://github.com/o/r/issues/2 -->\n\n\
		 \x20   original parent body\n\
		 \x20   ADDED LOCAL CONTENT\n\n\
		 \x20   # Blockers\n\
		 \x20   - local blocker\n\n\
		 \x20   - [ ] Child <!--sub @mock_user https://github.com/o/r/issues/3 -->\n\n\
		 \x20     child body\n",
	);
	ctx.local(&local_vi, None).await;

	// --reset should discard local modifications, then .edit marks child closed
	let edited = parse_virtual(
		"- [ ] Grandparent <!-- @mock_user https://github.com/o/r/issues/1 -->\n\n\
		 \x20 grandparent body\n\n\
		 \x20 - [ ] Parent <!--sub @mock_user https://github.com/o/r/issues/2 -->\n\n\
		 \x20   original parent body\n\n\
		 \x20   - [x] Child <!--sub @mock_user https://github.com/o/r/issues/3 -->\n\n\
		 \x20     child body\n",
	);

	let out = ctx.open_url(("o", "r").into(), 1).args(&["--reset"]).edit(&edited).run();

	assert!(
		out.status.success() && !out.stderr.contains("Conflict") && !out.stdout.contains("Merging"),
		"Should succeed without conflict or merge after reset. stderr: {}, stdout: {}",
		out.stderr,
		out.stdout
	);
}
