//! Tests for --reset flag behavior with conflicts.
//!
//! Issue #46: --reset should not trigger conflicts when modifying issues after reset.
//! After `open --reset`, the consensus matches remote exactly, so any user edit
//! should be detected as LocalOnly (no merge needed).

use crate::common::{TestContext, git::GitExt, parse_virtual};

/// After --reset on an issue with sub-issues, marking a sub-issue closed should succeed.
/// No conflict should occur because consensus == remote after reset.
#[test]
fn test_reset_with_subissue_edit() {
	let ctx = TestContext::build("");

	// Remote has parent with one open sub-issue
	let remote_vi = parse_virtual(
		"- [ ] Parent Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\
		 \tparent body\n\
		 \n\
		 \t- [ ] Sub Issue <!--sub @mock_user https://github.com/o/r/issues/2 -->\n\
		 \t\tsub body\n",
	);
	ctx.remote(&remote_vi, None);

	// User edits: mark sub-issue as closed
	let edited = parse_virtual(
		"- [ ] Parent Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\
		 \tparent body\n\
		 \n\
		 \t- [x] Sub Issue <!--sub @mock_user https://github.com/o/r/issues/2 -->\n\
		 \t\tsub body\n",
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

	let remote_vi = parse_virtual("- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\toriginal body\n");
	ctx.remote(&remote_vi, None);

	let edited = parse_virtual("- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\tmodified body\n");

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
		"- [ ] Grandparent <!-- @mock_user https://github.com/o/r/issues/1 -->\n\
		 \tgrandparent body\n\
		 \n\
		 \t- [ ] Parent <!--sub @mock_user https://github.com/o/r/issues/2 -->\n\
		 \t\toriginal parent body\n\
		 \n\
		 \t\t- [ ] Child <!--sub @mock_user https://github.com/o/r/issues/3 -->\n\
		 \t\t\tchild body\n",
	);
	ctx.remote(&remote_vi, None);

	// Local: same hierarchy but parent sub-issue has local modifications
	let local_vi = parse_virtual(
		"- [ ] Grandparent <!-- @mock_user https://github.com/o/r/issues/1 -->\n\
		 \tgrandparent body\n\
		 \n\
		 \t- [ ] Parent <!--sub @mock_user https://github.com/o/r/issues/2 -->\n\
		 \t\toriginal parent body\n\
		 \t\tADDED LOCAL CONTENT\n\
		 \n\
		 \t\t# Blockers\n\
		 \t\t- local blocker\n\
		 \n\
		 \t\t- [ ] Child <!--sub @mock_user https://github.com/o/r/issues/3 -->\n\
		 \t\t\tchild body\n",
	);
	ctx.local(&local_vi, None).await;

	// --reset should discard local modifications, then .edit marks child closed
	let edited = parse_virtual(
		"- [ ] Grandparent <!-- @mock_user https://github.com/o/r/issues/1 -->\n\
		 \tgrandparent body\n\
		 \n\
		 \t- [ ] Parent <!--sub @mock_user https://github.com/o/r/issues/2 -->\n\
		 \t\toriginal parent body\n\
		 \n\
		 \t\t- [x] Child <!--sub @mock_user https://github.com/o/r/issues/3 -->\n\
		 \t\t\tchild body\n",
	);

	let out = ctx.open_url(("o", "r").into(), 1).args(&["--reset"]).edit(&edited).run();

	assert!(
		out.status.success() && !out.stderr.contains("Conflict") && !out.stdout.contains("Merging"),
		"Should succeed without conflict or merge after reset. stderr: {}, stdout: {}",
		out.stderr,
		out.stdout
	);
}
