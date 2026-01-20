//! Snapshot helpers for integration tests.

use v_fixtures::render_fixture;

use super::TestContext;

/// Capture the issues directory as a snapshot string.
///
/// Returns a fixture-format string showing all files in the issues directory.
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
	let snapshot = snapshot_issues_dir_inner(ctx);
	if lines_to_redact.is_empty() {
		return snapshot;
	}
	snapshot
		.lines()
		.enumerate()
		.map(|(i, line)| {
			let line_num = i + 1; // 1-indexed
			if lines_to_redact.contains(&line_num) {
				"        [REDACTED - non-deterministic timestamp]".to_string()
			} else {
				line.to_string()
			}
		})
		.collect::<Vec<_>>()
		.join("\n")
}

/// Inner implementation that does the actual directory snapshot.
fn snapshot_issues_dir_inner(ctx: &TestContext) -> String {
	// Regex to match git commit hashes in diff3 conflict markers (e.g., "||||||| a0f7d74")
	let hash_regex = regex::Regex::new(r"\|\|\|\|\|\|\| [0-9a-f]{7,40}").unwrap();

	let issues_dir = ctx.xdg.data_dir().join("issues");
	if !issues_dir.exists() {
		return String::from("(empty - issues directory does not exist)");
	}

	// Use a TempFixture-like approach: walk the directory and collect files
	let mut files = Vec::new();
	for entry in walkdir::WalkDir::new(&issues_dir)
		.into_iter()
		.filter_entry(|e| !e.path().to_string_lossy().contains(".git"))
		.filter_map(Result::ok)
	{
		let path = entry.path();
		if path.is_file() {
			let relative_path = path.strip_prefix(&issues_dir).expect("path should be under issues_dir");
			let relative_str = format!("/{}", relative_path.to_string_lossy());
			if let Ok(text) = std::fs::read_to_string(path) {
				// Normalize git hashes in conflict markers
				let normalized_text = hash_regex.replace_all(&text, "||||||| [hash]").to_string();
				files.push(v_fixtures::FixtureFile {
					path: relative_str,
					text: normalized_text,
				});
			}
		}
	}

	if files.is_empty() {
		return String::from("(empty - no files in issues directory)");
	}

	// Sort by path for deterministic output
	files.sort_by(|a, b| a.path.cmp(&b.path));
	render_fixture(&v_fixtures::Fixture { files })
}
