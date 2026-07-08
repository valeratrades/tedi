use std::{path::Path, process::Command};

use color_eyre::eyre::{Result, bail};
pub use tokio::sync::oneshot;
use tracing::{debug, instrument};
pub use v_utils::io::file_open::{Client as OpenClient, OpenMode, Position};

/// Environment variable name for mock pipe (integration tests)
const ENV_MOCK_PIPE: &str = "tedi_MOCK_PIPE";
const REJECTED_CHANGES_PATH: &str = "/tmp/tedi/rejected-changes.md";
/// Open a file in editor.
///
/// Behavior depends on environment:
/// - If `{PKG_NAME}_MOCK_PIPE` env var is set: waits for any data on the named pipe, then returns.
///   This allows integration tests to control when the "editor" closes.
/// - Otherwise: opens with $EDITOR normally.
///
/// If `position` is provided, the editor will open at the specified line and column (if supported).
#[instrument(level = "debug")]
pub async fn open_file<P: AsRef<Path> + std::fmt::Debug>(path: P, position: Option<Position>) -> Result<()> {
	// Check for integration test pipe-based mock mode
	if let Ok(pipe_path) = std::env::var(ENV_MOCK_PIPE) {
		if let Some(pos) = &position {
			// parsed by integration tests to snapshot cursor placement
			eprintln!("[mock] position: {}:{}", pos.line, pos.col.unwrap_or(1));
		}
		// Wait for signal on the pipe (any data or EOF when writer closes)
		eprintln!("[mock] Waiting for signal on pipe: {pipe_path}");
		let mut buf = [0u8; 1];
		// Use blocking read in a spawn_blocking to not block the async runtime
		tokio::task::spawn_blocking(move || {
			use std::io::Read;
			if let Ok(mut pipe) = std::fs::File::open(&pipe_path) {
				let _ = pipe.read(&mut buf);
			}
		})
		.await?;
		eprintln!("[mock] Signal received, continuing...");
		return Ok(());
	}

	let mut client = OpenClient::default().mode(OpenMode::Normal);
	if let Some(pos) = position {
		debug!("Opening file at position: {pos:?}");
		client = client.at(pos);
	}
	client.open(path).await?;
	Ok(())
}
/// Run fd (find alternative) with the given arguments.
/// Panics if fd is not installed.
pub fn fd(args: &[&str], dir: &Path) -> Result<String> {
	let output = Command::new("fd").args(args).current_dir(dir).output();

	match output {
		Ok(out) if out.status.success() => Ok(String::from_utf8(out.stdout)?),
		Ok(out) => bail!("fd failed: {}", String::from_utf8_lossy(&out.stderr)),
		Err(e) if e.kind() == std::io::ErrorKind::NotFound => {
			panic!("fd is not installed. Install it: https://github.com/sharkdp/fd")
		}
		Err(e) => bail!("Failed to run fd: {e}"),
	}
}
/// Run rg (ripgrep) with the given arguments.
/// Panics if rg is not installed.
pub fn rg(args: &[&str], dir: &Path) -> Result<String> {
	let output = Command::new("rg").args(args).current_dir(dir).output();

	match output {
		Ok(out) if out.status.success() => Ok(String::from_utf8(out.stdout)?),
		Ok(out) if out.status.code() == Some(1) => Ok(String::new()), // No matches
		Ok(out) => bail!("rg failed: {}", String::from_utf8_lossy(&out.stderr)),
		Err(e) if e.kind() == std::io::ErrorKind::NotFound => {
			panic!("rg (ripgrep) is not installed. Install it: https://github.com/BurntSushi/ripgrep")
		}
		Err(e) => bail!("Failed to run rg: {e}"),
	}
}
/// Persist rejected user-edited content to a known path for recovery.
/// Call this before propagating a parse/validation error on user-edited content.
pub fn persist_rejected_changes(content: &str) {
	let path = Path::new(REJECTED_CHANGES_PATH);
	if let Some(parent) = path.parent() {
		if let Err(e) = std::fs::create_dir_all(parent) {
			tracing::warn!("failed to create rejected-changes dir: {e}");
			return;
		}
	}
	if let Err(e) = std::fs::write(path, content) {
		tracing::warn!("failed to persist rejected changes: {e}");
	}
}
