//! The config names how tedi reaches GitHub: `github_token`, or a `github_app` — exactly one.

use crate::common::TestContext;

#[test]
fn a_token_beside_an_app_is_refused() {
	let ctx = TestContext::build();
	ctx.xdg.write_config("config.toml", "[github_app]\nid = 1\nprivate_key = \"unread\"\n"); // the harness passes `github_token` through the env

	let out = ctx.run(&["--offline", "pull", "o"]);

	assert!(!out.status.success(), "stdout: {}", out.stdout);
	assert!(out.stderr.contains("both `github_token` and `github_app`"), "stderr: {}", out.stderr);
}
