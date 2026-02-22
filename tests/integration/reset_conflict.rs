//! Tests for --reset flag behavior with conflicts.
//!
//! Issue #46: --reset should not trigger conflicts when modifying issues after reset.
//! After `open --reset`, the consensus matches remote exactly, so any user edit
//! should be detected as LocalOnly (no merge needed).

use v_fixtures::FixtureRenderer;

use crate::{
	FixtureIssuesExt as _,
	common::{TestContext, parse_virtual},
	render_fixture,
};

/// After --reset on an issue with sub-issues, marking a sub-issue closed should succeed.
/// No conflict should occur because consensus == remote after reset.
#[test]
fn test_reset_with_subissue_edit() {
	let ctx = TestContext::build("");

	// Remote has parent with one open sub-issue
	let remote_vi = parse_virtual(
		r#"- [ ] Parent Issue <!-- @mock_user https://github.com/o/r/issues/1 -->

  parent body

  - [ ] Sub Issue <!--sub @mock_user https://github.com/o/r/issues/2 -->

    sub body
"#,
	);
	ctx.remote(&remote_vi, None);

	// User edits: mark sub-issue as closed
	let edited = parse_virtual(
		r#"- [ ] Parent Issue <!-- @mock_user https://github.com/o/r/issues/1 -->

  parent body

  - [x] Sub Issue <!--sub @mock_user https://github.com/o/r/issues/2 -->

    sub body
"#,
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

	let remote_vi = parse_virtual(
		r#"- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->

  original body
"#,
	);
	ctx.remote(&remote_vi, None);

	let edited = parse_virtual(
		r#"- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->

  modified body
"#,
	);

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
		r#"- [ ] Grandparent <!-- @mock_user https://github.com/o/r/issues/1 -->

  grandparent body

  - [ ] Parent <!--sub @mock_user https://github.com/o/r/issues/2 -->

    original parent body

    - [ ] Child <!--sub @mock_user https://github.com/o/r/issues/3 -->

      child body
"#,
	);
	ctx.remote(&remote_vi, None);

	// Local: same hierarchy but parent sub-issue has local modifications
	let local_vi = parse_virtual(
		r#"- [ ] Grandparent <!-- @mock_user https://github.com/o/r/issues/1 -->

  grandparent body

  - [ ] Parent <!--sub @mock_user https://github.com/o/r/issues/2 -->

    original parent body
    ADDED LOCAL CONTENT

    # Blockers
    - local blocker

    - [ ] Child <!--sub @mock_user https://github.com/o/r/issues/3 -->

      child body
"#,
	);
	ctx.local(&local_vi, None).await;

	// --reset should discard local modifications, then .edit marks child closed
	let edited = parse_virtual(
		r#"- [ ] Grandparent <!-- @mock_user https://github.com/o/r/issues/1 -->

  grandparent body

  - [ ] Parent <!--sub @mock_user https://github.com/o/r/issues/2 -->

    original parent body

    - [x] Child <!--sub @mock_user https://github.com/o/r/issues/3 -->

      child body
"#,
	);

	let out = ctx.open_url(("o", "r").into(), 1).args(&["--reset"]).edit(&edited).run();

	insta::assert_snapshot!(render_fixture(FixtureRenderer::try_new(&ctx).unwrap().skip_meta(), &out), @"
	//- /o/r/1_-_Grandparent/2_-_Parent/3_-_Child.md.bak
	- [x] Child <!-- @mock_user https://github.com/o/r/issues/3 -->
	  
	   <!--omitted {{{always-->
	  
	  child body
	  
	//- /o/r/1_-_Grandparent/2_-_Parent/__main__.md
	- [ ] Parent <!-- @mock_user https://github.com/o/r/issues/2 -->
	  
	  original parent body
	//- /o/r/1_-_Grandparent/__main__.md
	- [ ] Grandparent <!-- @mock_user https://github.com/o/r/issues/1 -->
	  
	  grandparent body
	");

	assert!(
		out.status.success() && !out.stderr.contains("Conflict") && !out.stdout.contains("Merging"),
		"Should succeed without conflict or merge after reset. stderr: {}, stdout: {}",
		out.stderr,
		out.stdout
	);
}
