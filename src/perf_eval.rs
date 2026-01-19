use std::process::Command;

use ask_llm::{ImageContent, Message, Model, Role};
use clap::Args;
use color_eyre::eyre::{Context, Result, bail};
use jiff::Zoned;

use crate::config::LiveSettings;

#[derive(Args, Debug)]
pub struct PerfEvalArgs {
	/// Github API token (can also be set via GITHUB_KEY env var)
	#[arg(long)]
	pub github_key: Option<String>,
}

pub async fn main(_settings: &LiveSettings, args: PerfEvalArgs) -> Result<()> {
	// Set GITHUB_KEY env var if provided via flag
	if let Some(ref github_key) = args.github_key {
		// SAFETY: Only called during initialization, before spawning threads for the LLM call
		unsafe {
			std::env::set_var("GITHUB_KEY", github_key);
		}
	}

	let cache_dir = v_utils::xdg_cache_dir!("perf_eval");

	let now = Zoned::now();
	let date_dir = cache_dir.join(now.strftime("%Y-%m-%d").to_string());

	// Check for recent screenshots from watch-monitors daemon
	if date_dir.exists() {
		// Find the most recent screenshot
		let mut most_recent: Option<(std::path::PathBuf, std::time::SystemTime)> = None;

		if let Ok(entries) = std::fs::read_dir(&date_dir) {
			for entry in entries.flatten() {
				let path = entry.path();
				if path.extension().and_then(|s| s.to_str()) == Some("png")
					&& let Ok(metadata) = std::fs::metadata(&path)
					&& let Ok(modified) = metadata.modified()
					&& (most_recent.is_none() || modified > most_recent.as_ref().unwrap().1)
				{
					most_recent = Some((path, modified));
				}
			}
		}

		if let Some((recent_path, modified_time)) = most_recent {
			if let Ok(elapsed) = modified_time.elapsed()
				&& elapsed.as_secs() > 61
			{
				bail!(
					"Most recent screenshot is {} seconds old (found at: {}).\n\
						The watch-monitors daemon should be running to provide fresh screenshots.\n\
						Start it with: {} watch-monitors\n\
						Or enable the systemd service: services.{}-watch-monitors.enable = true;",
					elapsed.as_secs(),
					recent_path.display(),
					env!("CARGO_PKG_NAME"),
					env!("CARGO_PKG_NAME"),
				);
			}
		} else {
			bail!(
				"No screenshots found in {}.\n\
				The watch-monitors daemon should be running to capture screenshots.\n\
				Start it with: {} watch-monitors\n\
				Or enable the systemd service: services.{}-watch-monitors.enable = true;",
				date_dir.display(),
				env!("CARGO_PKG_NAME"),
				env!("CARGO_PKG_NAME"),
			);
		}
	} else {
		bail!(
			"Screenshot directory does not exist: {}\n\
			The watch-monitors daemon should be running to capture screenshots.\n\
			Start it with: {} watch-monitors\n\
			Or enable the systemd service: services.{}-watch-monitors.enable = true;",
			date_dir.display(),
			env!("CARGO_PKG_NAME"),
			env!("CARGO_PKG_NAME"),
		);
	}

	// Load the most recent screenshot(s) instead of capturing new ones
	let mut screenshot_images = Vec::new();

	// Collect all PNG files with their metadata
	let mut entries_with_time: Vec<(std::path::PathBuf, std::time::SystemTime)> = Vec::new();
	for entry in std::fs::read_dir(&date_dir)?.filter_map(|e| e.ok()) {
		let path = entry.path();
		if path.extension().and_then(|s| s.to_str()) == Some("png")
			&& let Ok(metadata) = std::fs::metadata(&path)
			&& let Ok(modified) = metadata.modified()
		{
			entries_with_time.push((path, modified));
		}
	}

	if entries_with_time.is_empty() {
		bail!("No screenshots found in {}", date_dir.display());
	}

	// Sort by modification time (newest first)
	entries_with_time.sort_by(|a, b| b.1.cmp(&a.1));

	// Get the most recent timestamp
	let most_recent_time = entries_with_time[0].1;
	let five_minutes_ago = most_recent_time - std::time::Duration::from_secs(5 * 60);

	// Group screenshots by capture time (within 2 seconds tolerance)
	let mut capture_groups: Vec<Vec<std::path::PathBuf>> = Vec::new();
	let mut current_group: Vec<std::path::PathBuf> = Vec::new();
	let mut last_time: Option<std::time::SystemTime> = None;

	for (screenshot_path, modified_time) in &entries_with_time {
		// Skip screenshots older than 5 minutes
		if *modified_time < five_minutes_ago {
			continue;
		}

		if let Some(lt) = last_time {
			let time_diff = if *modified_time > lt {
				modified_time.duration_since(lt).unwrap_or_default()
			} else {
				lt.duration_since(*modified_time).unwrap_or_default()
			};

			// If more than 2 seconds apart, this is a new capture
			if time_diff.as_secs() > 2 && !current_group.is_empty() {
				capture_groups.push(current_group.clone());
				current_group.clear();
			}
		}

		current_group.push(screenshot_path.clone());
		last_time = Some(*modified_time);
	}

	// Add the last group if non-empty
	if !current_group.is_empty() {
		capture_groups.push(current_group);
	}

	// Take up to 5 most recent capture groups
	let captures_to_use = capture_groups.iter().take(5);

	// Load all screenshots from the selected captures
	for capture_group in captures_to_use {
		for screenshot_path in capture_group {
			let png_bytes = std::fs::read(screenshot_path).wrap_err(format!("Failed to read screenshot: {}", screenshot_path.display()))?;

			if png_bytes.is_empty() {
				tracing::warn!("Skipping empty screenshot file: {}", screenshot_path.display());
				continue;
			}

			let base64_data = base64::Engine::encode(&base64::engine::general_purpose::STANDARD, &png_bytes);

			screenshot_images.push(ImageContent {
				base64_data,
				media_type: "image/png".to_string(),
			});

			tracing::debug!("Using screenshot: {}", screenshot_path.display());
		}
	}

	if screenshot_images.is_empty() {
		bail!("Failed to load valid screenshots from {}", date_dir.display());
	}

	let num_captures = capture_groups.len().min(5);
	tracing::info!("Loaded {num_captures} screenshot capture(s) from the last 5 minutes");

	// Get current blocker
	let blocker_output = Command::new(std::env::current_exe()?)
		.args(["blocker", "current", "-f"])
		.output()
		.wrap_err("Failed to execute blocker current")?;

	let current_blocker = String::from_utf8_lossy(&blocker_output.stdout).trim().to_string();

	if current_blocker.is_empty() {
		bail!("No current blocker found. Set one with: {} blocker add <task>", env!("CARGO_PKG_NAME"));
	}

	// Get daily milestones
	let milestones_output = Command::new(std::env::current_exe()?)
		.args(["milestones", "get", "1d"])
		.output()
		.wrap_err("Failed to execute milestones get 1d")?;

	let daily_milestones = String::from_utf8_lossy(&milestones_output.stdout).trim().to_string();

	// Get static milestones
	let static_milestones_output = Command::new(std::env::current_exe()?)
		.args(["milestones", "get", "static"])
		.output()
		.wrap_err("Failed to execute milestones get static")?;

	let static_milestones = String::from_utf8_lossy(&static_milestones_output.stdout).trim().to_string();

	// Analyze all screenshots with LLM
	println!("\nAnalyzing screenshots...");
	let prompt = format!(
		r#"You are analyzing screenshots of a user's workspace taken over the last 5 minutes to assess how relevant their activity is to their stated goals.

IMPORTANT: You are receiving up to 5 screenshot captures (taken ~60 seconds apart), showing progression over time. Look at how the user's activity has evolved and what progress (if any) they are making toward their goals.

Task identified as current blocker. You're also partially judging relevance of it against daily objectives.
// eg if we are configuring nvim as it's blocking us from coding efficiently, that's already not directly related. But if it gets set to coding an unrelated task or say playing some game, - that's completely off.
{current_blocker}

Daily objectives. Main reference point for judging relevance:
// normally, the `current_blocker` will be relevant to one specific point outlined here, so interpret that one more as a contextual guide as to what should be happening this very moment.
{daily_milestones}

Static task axis (activities judged as always useful, even if I'm procrastinating on the current blocker (but, obviously, reduced relevance weight)):
{static_milestones}

Please analyze the screenshots chronologically (most recent first) and rate the overall relevance on a scale from 0 to 10, where:
- 0 = Completely unrelated or counterproductive
- 5 = Somewhat related
- 10 = Directly working at the goal; being productive

When scoring, consider:
1. Primary: Relevance to the current blocker and daily objectives (full weight)
2. Static: Relevance to the static task axis (1/3 weight)
3. If an activity is relevant to both primary goals and static activities, that should further increase the score
4. Progress over time: Are they making forward progress on goals, or staying stuck/distracted?

Provide a brief 1-2 sentence explanation that mentions the progression if applicable.

Format your response EXACTLY as follows:
<score>N</score>
<explanation>Your explanation here</explanation>

Replace N with an integer from 0 to 10."#
	);

	let message = Message::new_with_text_and_images(Role::User, prompt, screenshot_images);
	tracing::debug!(?message);

	let mut conv = ask_llm::Conversation::new();
	conv.0.push(message);

	match ask_llm::conversation::<&str>(&conv, Model::Medium, Some(4096), None).await {
		Ok(response) => {
			tracing::debug!("LLM response text: {}", response.text);

			// Parse score
			let score_raw = response.extract_html_tag("score").inspect_err(|_e| {
				eprintln!("Failed to extract <score> tag. Full response:\n{}\n", response.text);
			})?;

			let score_int: i32 = score_raw.trim().parse().wrap_err(format!("Failed to parse score as integer: '{score_raw}'"))?;

			if !(0..=10).contains(&score_int) {
				bail!("Score out of range: {score_int}");
			}

			let explanation = response.extract_html_tag("explanation").inspect_err(|_e| {
				eprintln!("Failed to extract <explanation> tag. Full response:\n{}\n", response.text);
			})?;

			println!("\nCurrent blocker: {current_blocker}");
			println!("Relevance score: {score_int}/10");
			println!("\nExplanation: {}", explanation.trim());

			tracing::info!("Cost: {:.4} cents", response.cost_cents);
		}
		Err(e) => {
			eprintln!("Error calling LLM: {e:?}");
			return Err(e);
		}
	}

	Ok(())
}
