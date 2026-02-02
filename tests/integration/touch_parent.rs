//! Integration tests for --parent flag with --touch mode.
//!
//! Tests the creation of missing parent components (repos) when using touch mode.

use v_fixtures::FixtureRenderer;

use crate::{
	common::{FixtureIssuesExt, TestContext},
	render_fixture,
};

/// Test that --parent=virtual creates a virtual project for a non-existent repo.
#[test]
fn test_parent_virtual_creates_virtual_project() {
	// Empty context - no existing repos
	let ctx = TestContext::build("");

	// Touch with --parent=virtual should create a virtual project
	let out = ctx.open_touch("newowner/newrepo/my-issue").args(&["--parent=virtual"]).ghost_edit().run();

	// Verify: virtual project created with the issue
	// Note: virtual projects are offline-only, so ghost_edit doesn't trigger sync - issue stays pending
	insta::assert_snapshot!(render_fixture(FixtureRenderer::try_new(&ctx).unwrap(), &out), @r#"
	//- /newowner/newrepo/.meta.json
	{
	  "virtual_project": true,
	  "next_virtual_issue_number": 1
	}
	//- /newowner/newrepo/my-issue.md
	- [ ] my-issue <!-- virtual: -->
	"#);

	assert!(out.status.success(), "Expected success, got stderr: {}", out.stderr);
}

/// Test that --parent (without value, defaults to default) errors when repo doesn't exist on GitHub.
#[test]
fn test_parent_default_errors_for_nonexistent_github_repo() {
	let ctx = TestContext::build("");

	// Touch with --parent for a repo that doesn't exist on GitHub
	let out = ctx.open_touch("nonexistent/repo/issue").args(&["--parent"]).run();

	// Should fail because the repo doesn't exist on GitHub
	assert!(!out.status.success(), "Expected failure for non-existent GitHub repo");
	assert!(
		out.stderr.contains("doesn't exist") || out.stderr.contains("not accessible"),
		"Expected error about repo not existing, got: {}",
		out.stderr
	);
}
