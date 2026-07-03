//! Snapshot helpers for integration tests.

use color_eyre::eyre::{Result, bail, eyre};
use v_fixtures::{Fixture, FixtureRenderer};

use super::TestContext;

/// Extension trait for creating and customizing issue fixture renderers.
pub trait FixtureIssuesExt<'a> {
	/// Create a renderer for the issues directory.
	/// Errors if the directory doesn't exist or is empty.
	/// Automatically normalizes git hashes.
	fn try_new(ctx: &'a TestContext) -> Result<FixtureRenderer<'a>>;

	/// Exclude .meta.json files from output.
	fn skip_meta(self) -> Self;

	/// Redact specific lines (1-indexed) with a timestamp-specific message.
	///
	/// Use this for timestamps set via `Timestamp::now()` which are non-deterministic.
	fn redact_timestamps(self, lines: &[usize]) -> Self;
}

impl<'a> FixtureIssuesExt<'a> for FixtureRenderer<'a> {
	fn try_new(ctx: &'a TestContext) -> Result<FixtureRenderer<'a>> {
		let issues_dir = ctx.xdg.data_dir().join("issues");

		let fixture = Fixture::read_from_directory(&issues_dir).ok_or_else(|| eyre!("issues directory doesn't exist: {}", issues_dir.display()))?;

		if fixture.files.is_empty() {
			bail!("issues directory is empty: {}", issues_dir.display());
		}

		// Leak the fixture to get 'a lifetime - this is fine for tests
		let fixture = Box::leak(Box::new(fixture));

		Ok(FixtureRenderer::new(fixture).normalize_git_hashes())
	}

	fn skip_meta(self) -> Self {
		self.regex(r"!\.meta\.json$")
	}

	fn redact_timestamps(self, lines: &[usize]) -> Self {
		self.redact_lines(lines).redact_message("        [REDACTED - non-deterministic timestamp]")
	}
}
