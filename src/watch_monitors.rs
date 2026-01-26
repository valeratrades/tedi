use std::{fs::File, io::BufWriter, thread, time::Duration};

use clap::Args;
use color_eyre::eyre::{Context, Result};
use jiff::{Timestamp, ToSpan, Zoned, civil};
use libwayshot::WayshotConnection;

use crate::config::LiveSettings;

pub fn main(_settings: &LiveSettings, _args: WatchMonitorsArgs) -> Result<()> {
	let cache_dir = v_utils::xdg_cache_dir!("watch_monitors");

	tracing::info!("Starting monitor watch daemon. Taking screenshots every 60 seconds.");

	//LOOP: it's a daemon
	loop {
		let now = Zoned::now();
		let date_dir = cache_dir.join(now.strftime("%Y-%m-%d").to_string());

		// Create the date directory if it doesn't exist
		std::fs::create_dir_all(&date_dir).wrap_err(format!("Failed to create directory: {}", date_dir.display()))?;

		// Create Wayshot connection
		let wayshot = match WayshotConnection::new() {
			Ok(w) => w,
			Err(e) => {
				tracing::error!("Failed to connect to Wayland compositor: {e:?}");
				thread::sleep(Duration::from_secs(60));
				continue;
			}
		};

		// Get list of outputs
		let outputs = wayshot.get_all_outputs();

		if outputs.is_empty() {
			tracing::warn!("No outputs found");
			thread::sleep(Duration::from_secs(60));
			continue;
		}

		let timestamp = now.strftime("%H-%M-%S").to_string();

		// Capture all outputs
		for (i, output) in outputs.iter().enumerate() {
			let filename = format!("{timestamp}-s{i}.png");
			let screenshot_path = date_dir.join(filename);

			match wayshot.screenshot_single_output(output, false) {
				Ok(image_buffer) =>
					if let Err(e) = save_screenshot_png(&image_buffer, &screenshot_path) {
						tracing::error!("Failed to save screenshot to {}: {e:?}", screenshot_path.display());
					} else {
						tracing::debug!("Screenshot saved to: {}", screenshot_path.display());
					},
				Err(e) => {
					tracing::error!("Failed to capture screenshot from output {i}: {e:?}");
				}
			}
		}

		// Cleanup old screenshots (run once per loop iteration)
		if let Err(e) = cleanup_old_screenshots(&cache_dir) {
			tracing::error!("Failed to cleanup old screenshots: {e:?}");
		}

		// Sleep for 60 seconds
		thread::sleep(Duration::from_secs(60));
	}
}
#[derive(Args, Debug)]
pub struct WatchMonitorsArgs {}

fn save_screenshot_png(image_buffer: &image::DynamicImage, path: &std::path::Path) -> Result<()> {
	let rgba = image_buffer.to_rgba8();
	let file = File::create(path).wrap_err(format!("Failed to create file: {}", path.display()))?;
	let writer = BufWriter::new(file);

	let mut encoder = png::Encoder::new(writer, rgba.width(), rgba.height());
	encoder.set_color(png::ColorType::Rgba);
	encoder.set_depth(png::BitDepth::Eight);

	let mut writer = encoder.write_header().wrap_err("Failed to write PNG header")?;
	writer.write_image_data(rgba.as_raw()).wrap_err("Failed to write PNG data")?;

	Ok(())
}

fn cleanup_old_screenshots(cache_dir: &std::path::Path) -> Result<()> {
	let threshold = Timestamp::now() - 1.day();

	for entry in std::fs::read_dir(cache_dir)? {
		let entry = entry?;
		let path = entry.path();

		if path.is_dir() {
			// Try to parse directory name as date (YYYY-MM-DD format)
			if let Some(dir_name) = path.file_name().and_then(|n| n.to_str())
				&& let Ok(dir_date) = civil::Date::strptime("%Y-%m-%d", dir_name)
			{
				let dir_timestamp = dir_date.at(0, 0, 0, 0).to_zoned(jiff::tz::TimeZone::UTC)?.timestamp();

				if dir_timestamp < threshold {
					tracing::info!("Removing old screenshot directory: {}", path.display());
					std::fs::remove_dir_all(&path)?;
				}
			}
		}
	}

	Ok(())
}
