//! Integration tests for sync conflict resolution.
//!
//! Tests the consensus-based sync logic where:
//! - Git commit state = last synced truth (consensus)
//! - Only conflict if BOTH local and remote changed since last sync
//! - Single-side changes auto-resolve
//!
//! Tests work with `VirtualIssue` for setup, converting to `Issue` through
//! the test context methods. The mock Github layer translates to API format at the boundary.
//!
//! ## Timestamp Seeds
//!
//! Each test uses unique seed values to ensure variety. Higher seed = newer timestamp = wins in merge.
//! Seeds are chosen to be either:
//! - Close together (e.g., 100 vs 105): tests edge cases where timestamps are similar
//! - Far apart (e.g., 50 vs 200): guarantees one side dominates
//!
//! ## Snapshot Testing
//!
//! Tests use insta snapshots to capture the resulting directory state.
//! The `.meta.json` file contains actual timestamps from seed-based generation,
//! so snapshots verify both file content and timestamp values.

use insta::assert_snapshot;
use rstest::rstest;
use v_fixtures::FixtureRenderer;

use crate::common::{
	FixtureIssuesExt, Seed, TestContext,
	are_you_sure::{UnsafePathExt, read_issue_file, write_to_path},
	parse_virtual, render_fixture,
};

/// Fixture for tests where consensus, local, and remote all have different bodies.
/// The key difference between these tests is the seed values, which determine timestamps
/// and therefore which side "wins" the merge.
struct DivergedBodiesFixture {
	ctx: TestContext,
	local: tedi_task_operations::Issue,
}

impl DivergedBodiesFixture {
	async fn new(consensus_seed: i64, local_seed: i64, remote_seed: i64) -> Self {
		let ctx = TestContext::build_with_preexisting_state_unsafe("");

		let consensus_vi = parse_virtual(
			r#"- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->
  consensus body
"#,
		);
		let local_vi = parse_virtual(
			r#"- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->
  local body
"#,
		);
		let remote_vi = parse_virtual(
			r#"- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->
  remote changed body
"#,
		);

		ctx.consensus(&consensus_vi, Some(Seed::new(consensus_seed))).await;
		let local = ctx.local(&local_vi, Some(Seed::new(local_seed))).await;
		ctx.remote(&remote_vi, Some(Seed::new(remote_seed)));

		Self { ctx, local }
	}
}

/// Tests that different timestamp seeds lead to different merge winners.
/// - remote_wins: seeds cause remote timestamps to win
/// - local_wins: seeds cause local timestamps to win
#[rstest]
#[case::remote_wins(-50, 40, 45, "remote changed body")]
#[case::local_wins(-70, 60, 65, "local body")]
#[tokio::test]
async fn test_both_diverged_merge_winner(#[case] consensus_seed: i64, #[case] local_seed: i64, #[case] remote_seed: i64, #[case] expected_body: &str) {
	let f = DivergedBodiesFixture::new(consensus_seed, local_seed, remote_seed).await;

	let out = f.ctx.open_issue(&f.local).run();

	// Verify the expected side won the merge
	let rendered = render_fixture(FixtureRenderer::try_new(&f.ctx).unwrap(), &out);
	assert!(rendered.contains(expected_body), "Expected body '{expected_body}' not found in:\n{rendered}");
}

/// When local matches consensus (no uncommitted changes) and remote has changed,
/// we only pull remote changes if --pull is specified.
#[tokio::test]
async fn test_only_remote_changed_takes_remote_with_pull() {
	let ctx = TestContext::build_with_preexisting_state_unsafe("");

	let consensus_vi = parse_virtual(
		r#"- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->
  consensus body
"#,
	);
	let remote_vi = parse_virtual(
		r#"- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->
  remote changed body
"#,
	);

	// Local matches consensus (no uncommitted changes), remote changed
	// Seeds: consensus=-45, remote=90 (remote much newer, guarantees dominance)
	let consensus = ctx.consensus(&consensus_vi, Some(Seed::new(-45))).await;
	ctx.remote(&remote_vi, Some(Seed::new(90)));

	// Must use --pull to fetch remote changes when local is unchanged
	let out = ctx.open_issue(&consensus).args(&["--pull"]).run();

	assert!(
		out.status.success() && (out.stdout.contains("Syncing") || out.stdout.contains("pre-open sync")),
		"Should succeed with sync activity. stdout: {}, stderr: {}",
		out.stdout,
		out.stderr
	);
}

#[tokio::test]
async fn test_only_local_changed_pushes_local() {
	let ctx = TestContext::build_with_preexisting_state_unsafe("");

	let consensus_vi = parse_virtual(
		r#"- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->
  consensus body
"#,
	);
	let local_vi = parse_virtual(
		r#"- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->
  local changed body
"#,
	);

	// Local changed, remote still matches consensus
	// Seeds: consensus=-25, local=85, remote=-25 (local much newer than unchanged remote)
	ctx.consensus(&consensus_vi, Some(Seed::new(-100))).await;
	let local = ctx.local(&local_vi, Some(Seed::new(100))).await;
	ctx.remote(&consensus_vi, Some(Seed::new(-100)));

	let out = ctx.open_issue(&local).run();

	assert!(out.status.success(), "Should succeed when only local changed. stderr: {}", out.stderr);

	// Capture the resulting directory state
	insta::assert_snapshot!(render_fixture(FixtureRenderer::try_new(&ctx).unwrap().redact_timestamps(&[9]), &out), @r#"
	//- /o/r/.meta.json
	{
	  "next_virtual_issue_number": 0,
	  "issues": {
	    "1": {
	      "user": "mock_user",
	      "timestamps": {
	        "title": "2001-09-12T11:20:39Z",
	        [REDACTED - non-deterministic timestamp]
	        "labels": "2001-09-12T01:55:52Z",
	        "state": "2001-09-12T00:39:25Z",
	        "comments": []
	      }
	    }
	  }
	}
	//- /o/r/1_-_Test_Issue.md
	- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->
	  local changed body
	"#);
}

#[tokio::test]
async fn test_reset_with_local_source_skips_sync() {
	let ctx = TestContext::build_with_preexisting_state_unsafe("");

	let consensus_vi = parse_virtual(
		r#"- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->
  consensus body
"#,
	);
	let local_vi = parse_virtual(
		r#"- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->
  local body
"#,
	);
	let remote_vi = parse_virtual(
		r#"- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->
  remote changed body
"#,
	);

	// --reset uses local as source, so timestamps don't affect result
	// Seeds: consensus=-30, local=20, remote=25
	ctx.consensus(&consensus_vi, Some(Seed::new(-30))).await;
	let local = ctx.local(&local_vi, Some(Seed::new(20))).await;
	ctx.remote(&remote_vi, Some(Seed::new(25)));

	// Run with --reset flag
	let out = ctx.open_issue(&local).args(&["--reset"]).run();

	eprintln!("stdout: {}", out.stdout);
	eprintln!("stderr: {}", out.stderr);
	eprintln!("status: {:?}", out.status);

	// With --reset, should reset to local state without sync
	assert!(out.status.success(), "Should succeed with --reset. stderr: {}", out.stderr);

	// Local file should still have local changes (not overwritten by remote)
	let issue_path = ctx.resolve_issue_path(&local);
	let content = read_issue_file(&issue_path);
	assert!(content.contains("local body"), "Local changes should be preserved with --reset");
}

/// Opening via URL when no local file exists should create the file from remote.
#[tokio::test]
async fn test_url_open_creates_local_file_from_remote() {
	let ctx = TestContext::build_with_preexisting_state_unsafe("");

	let remote_vi = parse_virtual(
		r#"- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->
  remote body content
"#,
	);
	// Seed: 15 (arbitrary, no comparison needed)
	ctx.remote(&remote_vi, Some(Seed::new(15)));

	// No local file exists - URL open should create it
	let expected_path = ctx.flat_issue_path(("o", "r").into(), 1, "Test Issue");
	assert!(!expected_path.exists(), "Local file should not exist before open");

	let out = ctx.open_url(("o", "r").into(), 1).run();

	eprintln!("stdout: {}", out.stdout);
	eprintln!("stderr: {}", out.stderr);

	assert!(out.status.success(), "Should succeed creating from URL. stderr: {}", out.stderr);

	// File should now exist with remote content
	assert!(expected_path.exists(), "Local file should be created");
	let content = read_issue_file(&expected_path);
	assert!(content.contains("remote body content"), "Should have remote content. Got: {content}");
}

/// A Github body may hold structure that looks like ours: a task list (shaped exactly like a
/// child item once indented under the title) and a heading of its own inside a list item. None
/// of it may be dropped or promoted on the way in, and the second open has to read back what
/// the first one wrote.
#[tokio::test]
async fn test_body_shaped_like_ours_survives_reopen() {
	let ctx = TestContext::build_with_preexisting_state_unsafe("");

	let remote_vi = parse_virtual("- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n  placeholder\n");
	ctx.remote(&remote_vi, Some(Seed::new(15)));
	ctx.set_remote_body(
		("o", "r").into(),
		1,
		r#"intro

- [ ] first step
  # Blockers
  - a note the item carries

- [ ] last step

# Blockers
- the real blocker
"#,
	);

	let out = ctx.open_url(("o", "r").into(), 1).run();
	assert!(out.status.success(), "stderr: {}", out.stderr);

	let content = read_issue_file(&ctx.flat_issue_path(("o", "r").into(), 1, "Test Issue"));
	for expected in ["  - [ ] first step", "- a note the item carries", "- [ ] last step", "- the real blocker"] {
		assert!(content.contains(expected), "{expected:?} was dropped on the way in. Got: {content}");
	}

	let reopen = ctx.open_url(("o", "r").into(), 1).run();
	assert!(reopen.status.success(), "reopen must parse the file we just wrote. stderr: {}", reopen.stderr);
}

/// Duplicates aren't tracked: as sub-issues they're filtered out, but a direct link (URL, sprint,
/// milestone) still reaches one.
#[tokio::test]
async fn test_opening_a_duplicate_errors_instead_of_panicking() {
	let ctx = TestContext::build_with_preexisting_state_unsafe("");
	ctx.remote(&parse_virtual("- [2] dup <!-- @mock_user https://github.com/o/r/issues/1 -->\n"), Some(Seed::new(15)));

	let out = ctx.open_url(("o", "r").into(), 1).run();
	assert!(!out.status.success(), "stdout: {}", out.stdout);
	assert!(
		!out.stderr.contains("panicked") && out.stderr.contains("o/r#1") && out.stderr.contains("duplicate"),
		"stderr: {}",
		out.stderr
	);
}

/// A Github title is plain text, but the title line is markdown: nothing markdown would interpret
/// (emphasis, escapes, entities, inline html) may change it or cost us the marker that follows.
#[tokio::test]
async fn test_markdown_in_remote_title_roundtrips() {
	let ctx = TestContext::build_with_preexisting_state_unsafe("");

	let remote_vi = parse_virtual("- [ ] placeholder <!-- @mock_user https://github.com/o/r/issues/1 -->\n  body\n\n  - [ ] child <!-- @mock_user https://github.com/o/r/issues/2 -->\n");
	ctx.remote(&remote_vi, Some(Seed::new(15)));
	let title = r"go after the **entire** body, `code` too \*x\* &amp; <b>y</b> [z] _w_";
	ctx.set_remote_title(("o", "r").into(), 1, title);
	ctx.set_remote_title(("o", "r").into(), 2, title);

	for _ in 0..2 {
		let out = ctx.open_url(("o", "r").into(), 1).run();
		assert!(out.status.success(), "stderr: {}", out.stderr);
	}

	let issues_dir = ctx.flat_issue_path(("o", "r").into(), 1, "x").parent().unwrap().to_path_buf();
	let parent_dir = std::fs::read_dir(&issues_dir)
		.unwrap()
		.map(|e| e.unwrap().path())
		.find(|p| p.is_dir() && p.file_name().unwrap().to_string_lossy().starts_with("1_-_"))
		.unwrap();
	let main = read_issue_file(&parent_dir.join("__main__.md"));
	let child_file = std::fs::read_dir(&parent_dir)
		.unwrap()
		.map(|e| e.unwrap().path())
		.find(|p| p.file_name().unwrap().to_string_lossy().starts_with("2_-_"))
		.unwrap();
	let child = read_issue_file(&child_file);
	for (what, line) in [("parent", main.lines().next().unwrap()), ("child", child.lines().next().unwrap())] {
		assert!(line.contains(&format!("{title} <!--")), "{what}'s title must read back as Github holds it. Got: {line}");
	}
	let link_line = main.lines().find(|l| l.contains("issues/2")).unwrap();
	assert!(link_line.contains(&format!("[{title}](")), "child link must carry the title verbatim. Got: {link_line}");
}

/// The conflict file is per owner, so it can sit resolved while a different issue of that owner
/// is opened. It names its own issue in its title line — anyone else's content must not land in it.
#[tokio::test]
async fn test_resolved_conflict_file_only_resolves_the_issue_it_names() {
	let ctx = TestContext::build_with_preexisting_state_unsafe("");

	let conflicted = parse_virtual("- [ ] one <!-- @mock_user https://github.com/o/r/issues/1 -->\n  body one\n");
	let bystander = parse_virtual("- [ ] two <!-- @mock_user https://github.com/o/r/issues/2 -->\n  body two\n");
	ctx.consensus(&conflicted, Some(Seed::new(15))).await;
	ctx.remote(&conflicted, Some(Seed::new(15)));
	ctx.consensus(&bystander, Some(Seed::new(15))).await;
	ctx.remote(&bystander, Some(Seed::new(15)));

	let bystander_path = ctx.flat_issue_path(("o", "r").into(), 2, "two");
	let conflict = bystander_path.parent().unwrap().parent().unwrap().join("__conflict.md");
	std::fs::write(&conflict, "- [ ] one <!-- @mock_user https://github.com/o/r/issues/1 -->\n  resolved one\n").unwrap();

	let out = ctx.open_url(("o", "r").into(), 2).run();
	assert!(out.status.success(), "stderr: {}", out.stderr);

	assert!(bystander_path.exists(), "o/r#2 was rewritten under o/r#1's title");
	let content = read_issue_file(&bystander_path);
	assert!(content.contains("body two") && !content.contains("resolved one"), "o/r#2 took o/r#1's resolution. Got: {content}");
	assert!(conflict.exists(), "o/r#1's resolution was consumed by another issue");
}

/// pulldown-cmark splits a code block's text per line when the block sits in a list item (the
/// local file), but not in a standalone body (Github). Same text, different events — so the
/// second open saw an edit nobody made.
#[tokio::test]
async fn test_multiline_code_block_is_a_roundtrip_fixpoint() {
	let ctx = TestContext::build_with_preexisting_state_unsafe("");

	let remote_vi = parse_virtual("- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n  placeholder\n");
	ctx.remote(&remote_vi, Some(Seed::new(15)));
	ctx.set_remote_body(("o", "r").into(), 1, "intro\n\n```sh\nline one\nline two\n```\n");

	let out = ctx.open_url(("o", "r").into(), 1).run();
	assert!(out.status.success(), "stderr: {}", out.stderr);

	let reopen = ctx.open_url(("o", "r").into(), 1).run();
	assert!(reopen.status.success(), "local and remote must agree on a body neither side touched. stderr: {}", reopen.stderr);
}

/// Github's "convert to issue" on a task-list item takes the item's whole rendered text as the
/// title, heading lines included. Written as-is, the title spills past the title line and the
/// file we produced no longer parses.
#[tokio::test]
async fn test_multiline_remote_title_is_rejected_before_write() {
	let ctx = TestContext::build_with_preexisting_state_unsafe("");

	let remote_vi = parse_virtual("- [ ] spoof location <!-- @mock_user https://github.com/o/r/issues/1 -->\n  body\n");
	ctx.remote(&remote_vi, Some(Seed::new(15)));
	ctx.set_remote_title(("o", "r").into(), 1, "spoof location\nBlockers");

	let out = ctx.open_url(("o", "r").into(), 1).run();
	assert!(!out.status.success(), "a multi-line title must not be written. stdout: {}", out.stdout);
	assert!(
		out.stderr.contains("o/r#1") && out.stderr.contains("title"),
		"error must name the issue and its title. stderr: {}",
		out.stderr
	);
}

/// Two consecutive plain paragraphs in a body are the one shape where the local file's first
/// paragraph shares the title line, so the item-interior span reaching `Events` carries both a
/// paragraph-bridge `SoftBreak` and the paragraph boundary it stands for. Counting both grew a
/// blank line on every parse→render cycle, which no `--reset` could settle: local and remote
/// disagreed on a body neither side had touched, and with equal `description` timestamps the
/// disagreement was a permanent conflict.
#[tokio::test]
async fn test_two_paragraph_body_is_a_roundtrip_fixpoint() {
	let ctx = TestContext::build_with_preexisting_state_unsafe("");

	let remote_vi = parse_virtual("- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n  placeholder\n");
	ctx.remote(&remote_vi, Some(Seed::new(15)));
	ctx.set_remote_body(("o", "r").into(), 1, "para one\n\npara two\n");

	let out = ctx.open_url(("o", "r").into(), 1).run();
	assert!(out.status.success(), "stderr: {}", out.stderr);

	let path = ctx.flat_issue_path(("o", "r").into(), 1, "Test Issue");
	let first = read_issue_file(&path);
	assert_snapshot!(first, @"
	- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->
	  para one

	  para two
	");

	let reopen = ctx.open_url(("o", "r").into(), 1).run();
	assert!(reopen.status.success(), "stderr: {}", reopen.stderr);
	let conflict = path.parent().unwrap().parent().unwrap().join("__conflict.md");
	assert!(!conflict.exists(), "a body neither side edited must not conflict");
	assert_eq!(first, read_issue_file(&path), "reparsing our own render must reproduce it byte for byte");
}

/// When opening via URL with --reset, local state should be completely replaced with remote.
/// No merge conflicts, no prompts - just nuke and replace.
#[tokio::test]
async fn test_reset_with_remote_url_nukes_local_state() {
	let ctx = TestContext::build_with_preexisting_state_unsafe("");

	let local_vi = parse_virtual(
		r#"- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->

  local body that should be nuked
"#,
	);
	let remote_vi = parse_virtual(
		r#"- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->

  remote body wins
"#,
	);

	// --reset overrides everything, but remote is the source when opening via URL
	// Seeds: consensus=-40, remote=80 (remote much newer)
	let local = ctx.consensus(&local_vi, Some(Seed::new(-40))).await;
	ctx.remote(&remote_vi, Some(Seed::new(80)));

	// Open via URL with --reset should nuke local and use remote
	let out = ctx.open_url(("o", "r").into(), 1).args(&["--reset"]).run();

	eprintln!("stdout: {}", out.stdout);
	eprintln!("stderr: {}", out.stderr);

	assert!(out.status.success(), "Should succeed with --reset via URL. stderr: {}", out.stderr);

	// Local file should now have remote content
	let issue_path = ctx.resolve_issue_path(&local);
	let content = read_issue_file(&issue_path);
	assert!(content.contains("remote body wins"), "Local should be replaced with remote. Got: {content}");
	assert!(!content.contains("local body that should be nuked"), "Local content should be gone");
}

/// When opening via URL with --reset and there's divergence, should NOT trigger merge conflict.
#[tokio::test]
async fn test_reset_with_remote_url_skips_merge_on_divergence() {
	let ctx = TestContext::build_with_preexisting_state_unsafe("");

	let consensus_vi = parse_virtual(
		r#"- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->
  consensus body
"#,
	);
	let local_vi = parse_virtual(
		r#"- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->
  local diverged body
"#,
	);
	let remote_vi = parse_virtual(
		r#"- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->
  remote diverged body
"#,
	);

	// Both diverged, but --reset via URL should skip merge and use remote
	// Seeds: consensus=-60, local=30, remote=35
	ctx.consensus(&consensus_vi, Some(Seed::new(-60))).await;
	let local = ctx.local(&local_vi, Some(Seed::new(30))).await;
	ctx.remote(&remote_vi, Some(Seed::new(35)));

	// Open via URL with --reset should NOT trigger merge conflict
	let out = ctx.open_url(("o", "r").into(), 1).args(&["--reset"]).run();

	eprintln!("stdout: {}", out.stdout);
	eprintln!("stderr: {}", out.stderr);

	// Should succeed without merge conflict
	assert!(out.status.success(), "Should succeed without merge conflict. stderr: {}", out.stderr);
	assert!(!out.stderr.contains("Conflict"), "Should not mention conflict with --reset");
	assert!(!out.stdout.contains("Merging"), "Should not attempt merge with --reset");

	// Local should have remote content
	let issue_path = ctx.resolve_issue_path(&local);
	let content = read_issue_file(&issue_path);
	assert!(content.contains("remote diverged body"), "Should have remote content. Got: {content}");
}

/// Remote changes land in the file the editor opens, with local matching consensus — the case the
/// pre-open sync used to skip, leaving the editor on a body Github had already moved past.
#[tokio::test]
async fn test_remote_change_reaches_the_editor() {
	let ctx = TestContext::build_with_preexisting_state_unsafe("");

	let local_vi = parse_virtual(
		r#"- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->
  local body
"#,
	);
	let remote_vi = parse_virtual(
		r#"- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->
  remote body from github
"#,
	);

	// Local unchanged from consensus, remote changed
	// Seeds: consensus=-20, remote=70
	let local = ctx.consensus(&local_vi, Some(Seed::new(-20))).await;
	ctx.remote(&remote_vi, Some(Seed::new(70)));

	let out = ctx.open_issue(&local).run();

	assert_snapshot!(render_fixture(FixtureRenderer::try_new(&ctx).unwrap().skip_meta(), &out), @"
	//- /o/r/1_-_Test_Issue.md
	- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->
	  remote body from github
	");
}

/// A divergence is recorded as a conflict before the editor ever opens — the user never gets to
/// type onto a body that was already superseded.
#[tokio::test]
async fn test_divergence_conflicts_before_editor() {
	let ctx = TestContext::build_with_preexisting_state_unsafe("");

	let consensus_vi = parse_virtual(
		r#"- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->
  consensus body
"#,
	);
	let local_vi = parse_virtual(
		r#"- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->
  local diverged body
"#,
	);
	let remote_vi = parse_virtual(
		r#"- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->
  remote diverged body
"#,
	);

	// Both local and remote changed since consensus
	ctx.consensus(&consensus_vi, Some(Seed::new(-100))).await;
	let local = ctx.local(&local_vi, Some(Seed::new(100))).await;
	ctx.remote(&remote_vi, Some(Seed::new(100)));

	let out = ctx.open_issue(&local).run();

	// Ensure conflict is opened
	assert_snapshot!(render_fixture(FixtureRenderer::try_new(&ctx).unwrap(), &out), @r#"
	//- /o/__conflict.md
	<<<<<<< HEAD
	- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->
	  local diverged body
	||||||| [hash]
	=======
	- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->
	  remote diverged body
	>>>>>>> remote-state
	//- /o/r/.meta.json
	{
	  "next_virtual_issue_number": 0,
	  "issues": {
	    "1": {
	      "user": "mock_user",
	      "timestamps": {
	        "title": "2001-09-12T11:20:39Z",
	        "description": "2001-09-12T10:04:12Z",
	        "labels": "2001-09-12T01:55:52Z",
	        "state": "2001-09-12T00:39:25Z",
	        "comments": []
	      }
	    }
	  }
	}
	//- /o/r/1_-_Test_Issue.md
	- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->
	  local diverged body
	"#)
}

#[tokio::test]
async fn test_closing_issue_syncs_state_change() {
	let ctx = TestContext::build_with_preexisting_state_unsafe("");

	let open_vi = parse_virtual(
		r#"- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->
  body
"#,
	);
	// Local = consensus = remote initially
	// Seeds: consensus=5, remote=5 (same seed = same base time)
	let open_issue = ctx.consensus(&open_vi, Some(Seed::new(5))).await;
	ctx.remote(&open_vi, Some(Seed::new(5)));

	// Edit to close the issue
	let mut closed_issue = open_vi.clone();
	closed_issue.contents.state = tedi_task_operations::CloseState::Closed;

	let out = ctx.open_issue(&open_issue).edit(&closed_issue).run();

	// Line 11 contains `state` timestamp set via Timestamp::now() when detecting state change
	let result_str = render_fixture(FixtureRenderer::try_new(&ctx).unwrap().redact_timestamps(&[11]), &out);

	insta::assert_snapshot!(result_str, @r#"
	//- /o/r/.meta.json
	{
	  "next_virtual_issue_number": 0,
	  "issues": {
	    "1": {
	      "user": "mock_user",
	      "timestamps": {
	        "title": "2001-09-11T09:15:20Z",
	        "description": "2001-09-11T04:30:34Z",
	        "labels": "2001-09-11T06:38:10Z",
	        [REDACTED - non-deterministic timestamp]
	        "comments": []
	      }
	    }
	  }
	}
	//- /o/r/1_-_Test_Issue.md.bak
	- [x] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->
	  body
	"#);
}

/// Sub-issues closed as duplicates should NOT appear in the pulled remote state.
/// Github marks these with state_reason="duplicate" - they should be filtered out entirely.
#[tokio::test]
async fn test_duplicate_sub_issues_filtered_from_remote() {
	let ctx = TestContext::build_with_preexisting_state_unsafe("");

	// Create parent with children for remote - normal closed and duplicate sub-issues
	let parent_vi = parse_virtual(
		r#"- [ ] Parent Issue <!-- @mock_user https://github.com/o/r/issues/1 -->

  parent body

  - [x] Normal Closed Sub <!--sub @mock_user https://github.com/o/r/issues/2 -->

    sub body

  - [2] Duplicate Sub <!--sub @mock_user https://github.com/o/r/issues/3 -->

    duplicate body
"#,
	);

	// Seed: -10 (arbitrary)
	ctx.remote(&parent_vi, Some(Seed::new(-10)));

	// Open via URL to fetch from remote
	let out = ctx.open_url(("o", "r").into(), 1).run();

	eprintln!("stdout: {}", out.stdout);
	eprintln!("stderr: {}", out.stderr);

	assert!(out.status.success(), "Should succeed. stderr: {}", out.stderr);

	insta::assert_snapshot!(render_fixture(FixtureRenderer::try_new(&ctx).unwrap().skip_meta(), &out), @"
	//- /o/r/1_-_Parent_Issue/2_-_Normal_Closed_Sub.md.bak
	- [x] Normal Closed Sub <!-- @mock_user https://github.com/o/r/issues/2 -->
	  sub body
	//- /o/r/1_-_Parent_Issue/__main__.md
	- [ ] Parent Issue <!-- @mock_user https://github.com/o/r/issues/1 -->
	  parent body

	  - [x] [Normal Closed Sub](./2_-_Normal_Closed_Sub.md.bak) <!-- @mock_user https://github.com/o/r/issues/2 -->
	");
}

/// Opening an issue twice when local matches remote should succeed (no-op).
/// This tests the case where you:
/// 1. Open an issue from URL (fetches remote)
/// 2. Open again without making changes
///
/// The second open should succeed, not fail with "Failed to commit remote state".
#[tokio::test]
async fn test_open_unchanged_succeeds() {
	let ctx = TestContext::build_with_preexisting_state_unsafe("");

	let vi = parse_virtual(
		r#"- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->

  issue body
"#,
	);
	// Seed: 10 (arbitrary)
	let issue = ctx.remote(&vi, Some(Seed::new(10)));

	// First open via URL
	let out = ctx.open_url(("o", "r").into(), 1).run();
	assert!(out.status.success(), "First open should succeed. stderr: {}", out.stderr);

	// Second open - should also succeed (no-op since nothing changed)
	let out = ctx.open_issue(&issue).run();
	assert!(out.status.success(), "Second open (unchanged) should succeed. stderr: {}", out.stderr);
}

/// Opening an issue by number when remote state matches local should succeed.
/// Reproduces: https://github.com/valeratrades/todo/issues/83
/// The issue happens when:
/// 1. `todo open --reset <url>` fetches and stores remote state
/// 2. `todo open <number>` is called (by number, not path)
/// 3. Remote state hasn't changed, but the merge machinery still runs
/// 4. Git commit fails because there's nothing to commit
#[tokio::test]
async fn test_open_by_number_unchanged_succeeds() {
	let ctx = TestContext::build_with_preexisting_state_unsafe("");

	let vi = parse_virtual(
		r#"- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->

  issue body
"#,
	);
	ctx.remote(&vi, None);

	// First open via URL with --reset
	let out = ctx.open_url(("o", "r").into(), 1).args(&["--reset"]).run();
	eprintln!("First open stdout: {}", out.stdout);
	eprintln!("First open stderr: {}", out.stderr);
	assert!(out.status.success(), "First open should succeed. stderr: {}", out.stderr);

	// Second open by number (simulating the failing case)
	// This uses the mock, so remote state is the same
	let out = ctx.open_url(("o", "r").into(), 1).run();
	eprintln!("Second open stdout: {}", out.stdout);
	eprintln!("Second open stderr: {}", out.stderr);
	assert!(out.status.success(), "Second open (unchanged) should succeed. stderr: {}", out.stderr);
}

/// --reset should only apply to the first sync (before editor).
/// After the user makes changes, normal sync should happen.
/// Reproduces the issue where changes made after --reset don't sync.
#[tokio::test]
async fn test_reset_syncs_changes_after_editor() {
	let ctx = TestContext::build_with_preexisting_state_unsafe("");

	let remote_vi = parse_virtual(
		r#"- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->

  remote body
"#,
	);
	ctx.remote(&remote_vi, None);

	// emulate user closing the issue after
	let mut modified_issue = remote_vi.clone();
	modified_issue.contents.state = tedi_task_operations::CloseState::Closed;
	let out = ctx.open_url(("o", "r").into(), 1).args(&["--reset"]).edit(&modified_issue).run();

	// want to see the issue closed here
	insta::assert_snapshot!(render_fixture(FixtureRenderer::try_new(&ctx).unwrap().redact_timestamps(&[11]), &out), @r#"
	//- /o/r/.meta.json
	{
	  "next_virtual_issue_number": 0,
	  "issues": {
	    "1": {
	      "user": "mock_user",
	      "timestamps": {
	        "title": null,
	        "description": null,
	        "labels": null,
	        [REDACTED - non-deterministic timestamp]
	        "comments": []
	      }
	    }
	  }
	}
	//- /o/r/1_-_Test_Issue.md.bak
	- [x] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->
	  remote body
	"#);
}

/// `!c` shorthand should expand to `<!-- new comment -->` and trigger comment creation.
/// When the user types `!c` on its own line, it should:
/// 1. Be expanded to `<!-- new comment -->` in the file
/// 2. Result in a new comment being created on Github
#[tokio::test]
async fn test_comment_shorthand_creates_comment() {
	let ctx = TestContext::build_with_preexisting_state_unsafe("");

	// Start with an issue that has no comments
	let vi = parse_virtual(
		r#"- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->
  issue body
"#,
	);
	let issue = ctx.consensus(&vi, None).await;
	ctx.remote(&vi, None);

	// Simulate user adding `!c` followed by comment content
	// After expansion, the file should have `<!-- new comment -->` marker
	let edited_content = r#"- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->
  issue body

  !c
  My new comment content
"#;

	// Write the edited content (simulating what user typed in editor)
	let issue_path = ctx.resolve_issue_path(&issue);
	write_to_path(&issue_path, edited_content);

	// Run open to trigger sync (which should expand !c and create the comment)
	let out = ctx.open_issue(&issue).run();

	//eprintln!("stdout: {}", out.stdout);
	//eprintln!("stderr: {}", out.stderr);

	// Capture the resulting directory state
	insta::assert_snapshot!(render_fixture(FixtureRenderer::try_new(&ctx).unwrap().skip_meta(), &out), @"
	//- /o/__conflict.md
	<<<<<<< HEAD
	- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->
	  issue body

	  <!--omitted {{{always-->
	  <!-- new comment -->
	  My new comment content

	  <!--,}}}-->
	||||||| [hash]
	=======
	- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->
	  issue body
	>>>>>>> remote-state
	//- /o/r/1_-_Test_Issue.md
	- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->
	  issue body

	  !c
	  My new comment content
	");
}

/// When local and remote have different sub-issues, force merge should preserve both.
/// This tests the scenario where:
/// - Local has sub-issue A that remote doesn't have
/// - Remote has sub-issue B that local doesn't have
/// - Local has an extra line in the description
/// After merge with --force (either side), consensus should contain both sub-issues.
///
/// Flag semantics:
/// - `--force` alone: prefer local on conflicts
/// - `--pull --force`: prefer remote on conflicts
#[rstest]
#[case::prefer_local(&["--force"], true)]
#[case::prefer_remote(&["--pull", "--force"], false)]
#[tokio::test]
async fn test_force_merge_preserves_both_sub_issues(#[case] args: &[&str], #[case] expect_local_description: bool) {
	let ctx = TestContext::build_with_preexisting_state_unsafe("");

	// Local: parent with local-only sub-issue and modified description
	let local_vi = parse_virtual(
		r#"- [ ] Parent Issue <!-- @mock_user https://github.com/o/r/issues/1 -->
  parent body
  extra line from local

  - [ ] Local Sub <!--sub @mock_user https://github.com/o/r/issues/2 -->
    local sub body
"#,
	);

	// Remote: parent with remote-only sub-issue (no extra description line)
	let remote_vi = parse_virtual(
		r#"- [ ] Parent Issue <!-- @mock_user https://github.com/o/r/issues/1 -->
  parent body

  - [ ] Remote Sub <!--sub @mock_user https://github.com/o/r/issues/3 -->
    remote sub body
"#,
	);

	// Consensus: original state (no sub-issues, original description)
	let consensus_vi = parse_virtual(
		r#"- [ ] Parent Issue <!-- @mock_user https://github.com/o/r/issues/1 -->
  parent body
"#,
	);

	ctx.consensus(&consensus_vi, Some(Seed::new(-100))).await;
	let local = ctx.local(&local_vi, Some(Seed::new(100))).await;
	ctx.remote(&remote_vi, Some(Seed::new(100)));

	let out = ctx.open_issue(&local).args(args).run();

	// Snapshot the result - different expectations based on which side wins conflicts
	//#[codestyle::skip]
	if expect_local_description {
		// --force: local wins conflicts, so "extra local line" should be present
		insta::assert_snapshot!(render_fixture(FixtureRenderer::try_new(&ctx).unwrap().skip_meta(), &out), @"
		//- /o/r/1_-_Parent_Issue/2_-_Local_Sub.md
		- [ ] Local Sub <!-- @mock_user https://github.com/o/r/issues/2 -->
		  local sub body
		//- /o/r/1_-_Parent_Issue/3_-_Remote_Sub.md
		- [ ] Remote Sub <!-- @mock_user https://github.com/o/r/issues/3 -->
		  remote sub body
		//- /o/r/1_-_Parent_Issue/__main__.md
		- [ ] Parent Issue <!-- @mock_user https://github.com/o/r/issues/1 -->
		  parent body
		  extra line from local

		  - [ ] [Local Sub](./2_-_Local_Sub.md) <!-- @mock_user https://github.com/o/r/issues/2 -->
		  - [ ] [Remote Sub](./3_-_Remote_Sub.md) <!-- @mock_user https://github.com/o/r/issues/3 -->
		");
	} else {
		// --pull --force: remote wins conflicts, so "extra local line" should NOT be present
		insta::assert_snapshot!(render_fixture(FixtureRenderer::try_new(&ctx).unwrap().skip_meta(), &out), @"
		//- /o/r/1_-_Parent_Issue/2_-_Local_Sub.md
		- [ ] Local Sub <!-- @mock_user https://github.com/o/r/issues/2 -->
		  local sub body
		//- /o/r/1_-_Parent_Issue/3_-_Remote_Sub.md
		- [ ] Remote Sub <!-- @mock_user https://github.com/o/r/issues/3 -->
		  remote sub body
		//- /o/r/1_-_Parent_Issue/__main__.md
		- [ ] Parent Issue <!-- @mock_user https://github.com/o/r/issues/1 -->
		  parent body

		  - [ ] [Local Sub](./2_-_Local_Sub.md) <!-- @mock_user https://github.com/o/r/issues/2 -->
		  - [ ] [Remote Sub](./3_-_Remote_Sub.md) <!-- @mock_user https://github.com/o/r/issues/3 -->
		");
	}
}

/// `!u` on the last line of the virtual file means "undo" - treat as if no changes were made.
/// User makes edits but then changes their mind, appends `!u` to abort sync.
/// The filesystem state should remain identical to the initial state.
#[tokio::test]
async fn test_undo_shorthand_aborts_sync() {
	let ctx = TestContext::build_with_preexisting_state_unsafe("");

	let vi = parse_virtual(
		r#"- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->
  issue body
"#,
	);
	let issue = ctx.consensus(&vi, None).await;
	ctx.remote(&vi, None);

	let (vpath, paused) = ctx.open_issue(&issue).args(&["--offline"]).break_to_edit();

	// Simulate user making edits but then deciding to abort
	let content = std::fs::read_to_string(&vpath).unwrap();
	std::fs::write(&vpath, format!("{content}  some random edits\n  another line of changes\n!u\n")).unwrap();

	let out = paused.resume();
	assert!(out.status.success(), "Should succeed. stderr: {}", out.stderr);

	// Issue file should remain unchanged from initial state
	assert_snapshot!(render_fixture(FixtureRenderer::try_new(&ctx).unwrap().skip_meta(), &out), @"
	//- /o/r/1_-_Test_Issue.md
	- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->
	  issue body
	");
}

/// Verify that .meta.json is written with timestamps when sinking to Consensus.
/// This is critical for the merge algorithm to work - timestamps determine which side wins.
#[tokio::test]
async fn test_consensus_sink_writes_meta_json_with_timestamps() {
	let ctx = TestContext::build_with_preexisting_state_unsafe("");

	// Set up a remote issue with a comment (will have timestamps from mock)
	let remote_vi = parse_virtual(
		r#"- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->
  remote body

  ---
  <!-- comment 1001 @commenter -->
  A test comment
"#,
	);
	ctx.remote(&remote_vi, None);

	// Fetch the issue via URL - this should sink to Consensus and write .meta.json
	let out = ctx.open_url(("o", "r").into(), 1).run();

	eprintln!("stdout: {}", out.stdout);
	eprintln!("stderr: {}", out.stderr);

	assert!(out.status.success(), "Fetch should succeed. stderr: {}", out.stderr);

	// Capture the resulting directory state (includes .meta.json with timestamps)
	insta::assert_snapshot!(render_fixture(FixtureRenderer::try_new(&ctx).unwrap().redact_timestamps(&[9]), &out), @r#"
	//- /o/r/.meta.json
	{
	  "next_virtual_issue_number": 0,
	  "issues": {
	    "1": {
	      "user": "mock_user",
	      "timestamps": {
	        "title": null,
	        [REDACTED - non-deterministic timestamp]
	        "labels": null,
	        "state": null,
	        "comments": []
	      }
	    }
	  }
	}
	//- /o/r/1_-_Test_Issue.md
	- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->
	  remote body
	  \---<!-- comment 1001 @commenter -->

	  A test comment
	"#);
}

/// Adding labels to an issue should sync them to remote.
/// Labels are specified as `[label1, label2] Title` in the file format.
#[tokio::test]
async fn test_adding_labels_syncs_to_remote() {
	let ctx = TestContext::build_with_preexisting_state_unsafe("");

	let vi = parse_virtual(
		r#"- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->
  body
"#,
	);
	let issue = ctx.consensus(&vi, Some(Seed::new(5))).await;
	ctx.remote(&vi, Some(Seed::new(5)));

	// Edit to add labels
	let mut labeled_vi = vi.clone();
	labeled_vi.contents.labels = vec!["bug".to_string(), "urgent".to_string()];

	let out = ctx.open_issue(&issue).edit(&labeled_vi).run();

	assert!(out.status.success(), "Should succeed. stderr: {}", out.stderr);
	assert!(out.stdout.contains("Updating issue #1 labels"), "Should push labels to remote. stdout: {}", out.stdout);

	// Verify labels appear in the resulting file
	let issue_path = ctx.resolve_issue_path(&issue);
	let content = read_issue_file(&issue_path);
	assert!(content.contains("(bug, urgent)"), "Labels should be in file. Got: {content}");
}

/// A title edited locally has to reach Github. Left local, it re-wins every merge on its own
/// timestamp and never settles.
#[tokio::test]
async fn test_local_title_edit_pushes_to_remote() {
	let ctx = TestContext::build_with_preexisting_state_unsafe("");

	let vi = parse_virtual("- [ ] old title <!-- @mock_user https://github.com/o/r/issues/1 -->\n  body\n");
	let issue = ctx.consensus(&vi, Some(Seed::new(5))).await;
	ctx.remote(&vi, Some(Seed::new(5)));

	let mut renamed = vi.clone();
	renamed.contents.title = "new title".to_string();
	let out = ctx.open_issue(&issue).edit(&renamed).run();

	assert!(out.status.success(), "stderr: {}", out.stderr);
	assert!(out.stdout.contains("Updating issue #1 title"), "the rename never reached Github. stdout: {}", out.stdout);
}
