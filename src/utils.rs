use std::{path::Path, process::Command, str::FromStr};

use color_eyre::eyre::{Report, Result, bail};
#[cfg(not(test))]
use jiff::Timestamp as TimestampImpl;
use jiff::{SignedDuration, civil};
#[cfg(test)]
use tedi::mocks::MockTimestamp as TimestampImpl;
pub use tokio::sync::oneshot;
use tracing::{debug, instrument};
pub use v_utils::io::file_open::{Client as OpenClient, OpenMode, Position};

use crate::config::LiveSettings;

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
pub fn format_date(days_back: usize, settings: &LiveSettings) -> String {
	let date = TimestampImpl::now() - SignedDuration::from_hours(days_back as i64 * 24);
	let offset = same_day_buffer();

	let config = settings.config().expect("failed to load config");
	let format_str = config.manual_stats.as_ref().map(|m| m.date_format.as_str()).unwrap_or("%Y-%m-%d");
	let format_str = if format_str.is_empty() { "%Y-%m-%d" } else { format_str };
	(date - offset).strftime(format_str).to_string()
}
/// Ends of each day-section as offset to wake-time
#[derive(Clone, Copy, Debug, Default, derive_new::new)]
pub struct DaySectionBorders {
	pub morning_end: f32,
	pub day_end: f32,
	pub evening_end: f32,
}
/// Diff of sleep time from 00:00 utc
pub fn same_day_buffer() -> SignedDuration {
	let waketime = std::env::var("WAKETIME").unwrap();
	let waketime = civil::Time::strptime("%H:%M", waketime.as_str()).unwrap();

	let borders = DaySectionBorders::from_str(&std::env::var("DAY_SECTION_BORDERS").unwrap()).unwrap();
	let sleep_offset_mins = (borders.evening_end * 60.0) as i64;

	// Calculate in total minutes from midnight, then wrap at 24h
	let waketime_mins = waketime.hour() as i64 * 60 + waketime.minute() as i64;
	let bedtime_mins = waketime_mins + sleep_offset_mins;
	let new_day_mins = (bedtime_mins + 6 * 60) % (24 * 60); // wrap at 24h
	SignedDuration::from_mins(new_day_mins)
}
/// Environment variable name for mock pipe (integration tests)
const ENV_MOCK_PIPE: &str = concat!(env!("CARGO_PKG_NAME"), "_MOCK_PIPE");

impl std::str::FromStr for DaySectionBorders {
	type Err = Report;

	fn from_str(borders_str: &str) -> Result<Self> {
		let mut vec_offsets = Vec::with_capacity(3);
		for s in borders_str.split(":") {
			vec_offsets.push(s.parse::<f32>()?);
		}
		if vec_offsets.len() == 3 {
			Ok(Self {
				morning_end: vec_offsets[0],
				day_end: vec_offsets[1],
				evening_end: vec_offsets[2],
			})
		} else {
			bail!("invalid dimensions");
		}
	}
}

#[cfg(test)]
mod tests {
	use std::time::Duration;

	use jiff::civil::date;

	use super::*;

	const ENV_GITHUB_TOKEN: &str = concat!(env!("CARGO_PKG_NAME"), "__GITHUB_TOKEN");

	fn init_test(t: Option<(i16, i8, i8, i8, i8, i8)>) -> LiveSettings {
		// SAFETY: This is only used in tests and doesn't cause race conditions in single-threaded test execution
		unsafe {
			std::env::set_var("WAKETIME", "05:00");
			std::env::set_var("DAY_SECTION_BORDERS", "2.5:10:16");
			std::env::set_var(ENV_GITHUB_TOKEN, "test_token");
		}

		if let Some(t) = t {
			let mock_now = date(t.0, t.1, t.2).at(t.3, t.4, t.5, 0).to_zoned(jiff::tz::TimeZone::UTC).unwrap().timestamp();
			tedi::mocks::set_timestamp(mock_now);
		}

		let flags = crate::config::SettingsFlags::default();
		LiveSettings::new(flags, Duration::from_secs(1)).unwrap()
	}

	#[test]
	fn test_same_day_buffer() {
		let _ = init_test(Some((2024, 5, 29, 12, 0, 0)));
		let offset = same_day_buffer();

		assert_eq!(offset, SignedDuration::from_hours(3));
	}

	#[test]
	fn test_format_date() {
		let settings = init_test(Some((2024, 5, 29, 12, 0, 0)));

		let formatted_date = format_date(1, &settings);
		assert_eq!(formatted_date, "2024-05-28");
	}

	#[test]
	fn test_correct_day() {
		let settings = init_test(Some((2024, 5, 29, 2, 59, 0)));
		let formatted_date = format_date(0, &settings);
		assert_eq!(formatted_date, "2024-05-28");

		let settings = init_test(Some((2024, 5, 29, 3, 1, 0)));
		let formatted_date = format_date(0, &settings);

		assert_eq!(formatted_date, "2024-05-29");
	}
}
