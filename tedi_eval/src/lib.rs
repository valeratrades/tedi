use std::str::FromStr;

use color_eyre::eyre::{Report, Result, bail};
#[cfg(not(test))]
use jiff::Timestamp as TimestampImpl;
use jiff::{SignedDuration, civil};
#[cfg(test)]
use mock_time::MockTimestamp as TimestampImpl;

pub mod manual_stats;
pub mod perf_eval;
pub mod watch_monitors;

pub fn format_date(days_back: usize, format_str: &str) -> String {
	let date = TimestampImpl::now() - SignedDuration::from_hours(days_back as i64 * 24);
	let offset = same_day_buffer();
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
mod mock_time {
	use std::cell::Cell;

	use jiff::Timestamp;

	thread_local! {
		static MOCK_TIMESTAMP: Cell<Option<Timestamp>> = const { Cell::new(None) };
	}

	pub struct MockTimestamp;

	impl MockTimestamp {
		pub fn now() -> Timestamp {
			MOCK_TIMESTAMP.with(|ts| ts.get()).unwrap_or_else(Timestamp::now)
		}
	}

	pub fn set(timestamp: Timestamp) {
		MOCK_TIMESTAMP.with(|ts| ts.set(Some(timestamp)));
	}
}

#[cfg(test)]
mod tests {
	use jiff::civil::date;

	use super::*;

	fn init_test(t: (i16, i8, i8, i8, i8, i8)) {
		// SAFETY: This is only used in tests and doesn't cause race conditions in single-threaded test execution
		unsafe {
			std::env::set_var("WAKETIME", "05:00");
			std::env::set_var("DAY_SECTION_BORDERS", "2.5:10:16");
		}
		let mock_now = date(t.0, t.1, t.2).at(t.3, t.4, t.5, 0).to_zoned(jiff::tz::TimeZone::UTC).unwrap().timestamp();
		mock_time::set(mock_now);
	}

	#[test]
	fn test_same_day_buffer() {
		init_test((2024, 5, 29, 12, 0, 0));
		assert_eq!(same_day_buffer(), SignedDuration::from_hours(3));
	}

	#[test]
	fn test_format_date() {
		init_test((2024, 5, 29, 12, 0, 0));
		assert_eq!(format_date(1, "%Y-%m-%d"), "2024-05-28");
	}

	#[test]
	fn test_correct_day() {
		init_test((2024, 5, 29, 2, 59, 0));
		assert_eq!(format_date(0, "%Y-%m-%d"), "2024-05-28");

		init_test((2024, 5, 29, 3, 1, 0));
		assert_eq!(format_date(0, "%Y-%m-%d"), "2024-05-29");
	}
}
