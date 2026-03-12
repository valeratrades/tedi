use std::{fs::File, io::BufWriter, path::PathBuf, thread, time::Duration};

use ask_llm::{ImageContent, Message, Model, Role};
use clap::{Args, Subcommand};
use color_eyre::eyre::{Context, Result, bail};
use jiff::{Timestamp, ToSpan, Zoned, civil};
use libwayshot::WayshotConnection;
use v_utils::prelude::*;

use crate::config::LiveSettings;

#[derive(Debug, Subcommand)]
pub enum MonitorsCommands {
	/// Daemon that takes screenshots every 15 minutes, storing up to 24h of history.
	Watch,
	/// Take a fresh screenshot, then annotate all stored screenshots within the given timeframe.
	/// Output: `[HH:MM] <monitor_number> description [path]`
	Annotated {
		/// How far back to look (e.g. "1h", "6h", "24h"). Capped at stored history (24h).
		timeframe: Timeframe,
		/// Model name to pass to ask_llm (e.g. "Fast", "Medium", "Slow").
		#[arg(short, long, default_value = "Fast")]
		model: Model,
	},
}
#[derive(Args, Debug)]
pub struct MonitorsArgs {
	#[command(subcommand)]
	pub command: MonitorsCommands,
}

pub async fn main(_settings: &LiveSettings, args: MonitorsArgs) -> Result<()> {
	match args.command {
		MonitorsCommands::Watch => watch_daemon(),
		MonitorsCommands::Annotated { timeframe, model } => annotated(timeframe, model).await,
	}
}

fn cache_dir() -> PathBuf {
	v_utils::xdg_cache_dir!("watch_monitors")
}

// Watch daemon

fn watch_daemon() -> Result<()> {
	let cache_dir = cache_dir();

	tracing::info!("Starting monitor watch daemon. Taking screenshots every 15 minutes.");

	//LOOP: it's a daemon
	loop {
		let now = Zoned::now();
		let date_dir = cache_dir.join(now.strftime("%Y-%m-%d").to_string());

		std::fs::create_dir_all(&date_dir).wrap_err(format!("Failed to create directory: {}", date_dir.display()))?;

		let wayshot = match WayshotConnection::new() {
			Ok(w) => w,
			Err(e) => {
				tracing::error!("Failed to connect to Wayland compositor: {e:?}");
				thread::sleep(Duration::from_secs(900));
				continue;
			}
		};

		let outputs = wayshot.get_all_outputs();

		if outputs.is_empty() {
			tracing::warn!("No outputs found");
			thread::sleep(Duration::from_secs(900));
			continue;
		}

		let timestamp = now.strftime("%H-%M-%S").to_string();

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

		if let Err(e) = cleanup_old_screenshots(&cache_dir) {
			tracing::error!("Failed to cleanup old screenshots: {e:?}");
		}

		thread::sleep(Duration::from_secs(900));
	}
}

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

// Annotated command

/// Take a fresh screenshot, then collect all screenshots within the timeframe and annotate them via LLM.
async fn annotated(timeframe: Timeframe, model: Model) -> Result<()> {
	let cache_dir = cache_dir();

	// Take a fresh screenshot right now
	capture_screenshots_now(&cache_dir)?;

	// Collect all screenshots within the timeframe
	let cutoff = Timestamp::now() - timeframe.signed_duration();
	let screenshots = collect_screenshots(&cache_dir, cutoff)?;

	if screenshots.is_empty() {
		bail!("No screenshots found within the requested timeframe");
	}

	// Load all images for the LLM
	let mut images = Vec::new();
	for s in &screenshots {
		let png_bytes = std::fs::read(&s.path).wrap_err(format!("Failed to read screenshot: {}", s.path.display()))?;
		if png_bytes.is_empty() {
			tracing::warn!("Skipping empty screenshot: {}", s.path.display());
			continue;
		}
		let base64_data = base64::Engine::encode(&base64::engine::general_purpose::STANDARD, &png_bytes);
		images.push(ImageContent {
			base64_data,
			media_type: "image/png".to_string(),
		});
	}

	if images.is_empty() {
		bail!("All screenshot files were empty");
	}

	// Build the prompt
	let image_listing = screenshots
		.iter()
		.enumerate()
		.map(|(i, s)| format!("Image {}: [{}] monitor {} ({})", i + 1, s.time_str, s.monitor_index, s.path.display()))
		.collect::<Vec<_>>()
		.join("\n");

	let prompt = format!(
		r#"You are annotating workspace screenshots. Each image corresponds to a specific timestamp and monitor.

Here are the screenshots in chronological order:
{image_listing}

For EACH screenshot, provide a concise description of what is visible on the screen (e.g. "VSCode editing Rust file", "Firefox on GitHub PR", "terminal running tests", "Discord chat").

Format your response as one line per screenshot, EXACTLY matching this format:
<annotations>
[HH:MM] <monitor_number> description
</annotations>

Where HH:MM is the UTC time, monitor_number is the monitor index, and description is your brief annotation. One line per image, in the same order as the images above."#
	);

	let message = Message::new_with_text_and_images(Role::User, prompt, images);
	let mut conv = ask_llm::Conversation::new();
	conv.0.push(message);

	let response = ask_llm::conversation::<&str>(&conv, model, Some(4096), None).await?;

	let annotations_raw = response.extract_html_tag("annotations").inspect_err(|_| {
		eprintln!("Failed to extract <annotations> tag. Full response:\n{}\n", response.text);
	})?;

	// Parse annotations and attach file paths
	let annotation_lines: Vec<&str> = annotations_raw.lines().map(|l| l.trim()).filter(|l| !l.is_empty()).collect();

	if annotation_lines.len() != screenshots.len() {
		tracing::warn!("LLM returned {} annotations but we have {} screenshots — printing raw", annotation_lines.len(), screenshots.len());
		for line in &annotation_lines {
			println!("{line}");
		}
	} else {
		for (line, s) in annotation_lines.iter().zip(screenshots.iter()) {
			println!("{line} [{}]", s.path.display());
		}
	}

	tracing::info!("Cost: {:.4} cents", response.cost_cents);

	Ok(())
}

struct ScreenshotEntry {
	path: PathBuf,
	timestamp: Timestamp,
	time_str: String,
	monitor_index: usize,
}

/// Capture screenshots from all monitors right now and save them to the cache dir.
fn capture_screenshots_now(cache_dir: &std::path::Path) -> Result<()> {
	let now = Zoned::now();
	let date_dir = cache_dir.join(now.strftime("%Y-%m-%d").to_string());
	std::fs::create_dir_all(&date_dir).wrap_err(format!("Failed to create directory: {}", date_dir.display()))?;

	let wayshot = WayshotConnection::new().wrap_err("Failed to connect to Wayland compositor")?;
	let outputs = wayshot.get_all_outputs();

	if outputs.is_empty() {
		bail!("No monitor outputs found");
	}

	let timestamp = now.strftime("%H-%M-%S").to_string();

	for (i, output) in outputs.iter().enumerate() {
		let filename = format!("{timestamp}-s{i}.png");
		let screenshot_path = date_dir.join(filename);

		let image_buffer = wayshot
			.screenshot_single_output(output, false)
			.wrap_err(format!("Failed to capture screenshot from output {i}"))?;
		save_screenshot_png(&image_buffer, &screenshot_path)?;
		tracing::debug!("Fresh screenshot saved to: {}", screenshot_path.display());
	}

	Ok(())
}

/// Collect all screenshots from the cache that are newer than `cutoff`, sorted chronologically.
fn collect_screenshots(cache_dir: &std::path::Path, cutoff: Timestamp) -> Result<Vec<ScreenshotEntry>> {
	let mut entries = Vec::new();

	for dir_entry in std::fs::read_dir(cache_dir)? {
		let dir_entry = dir_entry?;
		let dir_path = dir_entry.path();

		if !dir_path.is_dir() {
			continue;
		}

		let dir_name = match dir_path.file_name().and_then(|n| n.to_str()) {
			Some(n) => n.to_string(),
			None => continue,
		};

		let date = match civil::Date::strptime("%Y-%m-%d", &dir_name) {
			Ok(d) => d,
			Err(_) => continue,
		};

		for file_entry in std::fs::read_dir(&dir_path)? {
			let file_entry = file_entry?;
			let file_path = file_entry.path();

			if file_path.extension().and_then(|s| s.to_str()) != Some("png") {
				continue;
			}

			let file_name = match file_path.file_stem().and_then(|s| s.to_str()) {
				Some(n) => n.to_string(),
				None => continue,
			};

			// Parse "HH-MM-SS-sN" format
			let (time_part, monitor_part) = match file_name.rsplit_once("-s") {
				Some((t, m)) => (t, m),
				None => continue,
			};

			let monitor_index: usize = match monitor_part.parse() {
				Ok(m) => m,
				Err(_) => continue,
			};

			let time = match civil::Time::strptime("%H-%M-%S", time_part) {
				Ok(t) => t,
				Err(_) => continue,
			};

			let zoned = date.at(time.hour(), time.minute(), time.second(), 0).to_zoned(jiff::tz::TimeZone::system())?;
			let timestamp = zoned.timestamp();

			if timestamp < cutoff {
				continue;
			}

			let time_str = format!("{:02}:{:02}", zoned.hour(), zoned.minute());

			entries.push(ScreenshotEntry {
				path: file_path,
				timestamp,
				time_str,
				monitor_index,
			});
		}
	}

	entries.sort_by_key(|e| e.timestamp);

	Ok(entries)
}
