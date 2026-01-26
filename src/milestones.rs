use clap::{Args, Subcommand};
use jiff::Timestamp;
use reqwest::Client;
use v_utils::prelude::*;

use crate::config::LiveSettings;

pub static HEALTHCHECK_REL_PATH: &str = "healthcheck.status";
pub static SPRINT_HEADER_REL_PATH: &str = "sprint_header.md";

#[derive(Args)]
pub struct MilestonesArgs {
	#[command(subcommand)]
	command: MilestonesCommands,
}

#[derive(Subcommand)]
pub enum MilestonesCommands {
	Get {
		tf: Timeframe,
	},
	/// Edit a milestone's description in your $EDITOR
	Edit {
		tf: Timeframe,
	},
	/// Ensures all milestones up to date, if yes - writes "OK" to $XDG_DATA_HOME/todo/healthcheck.status
	/// Can get outdated easily, so printed output of the command is prepended with the filename
	Healthcheck,
}

pub async fn milestones_command(settings: &LiveSettings, args: MilestonesArgs) -> Result<()> {
	match args.command {
		MilestonesCommands::Get { tf } => {
			let retrieved_milestones = request_milestones(settings).await?;
			let milestone = get_milestone(tf, &retrieved_milestones)?;
			println!("{milestone}");
			Ok(())
		}
		MilestonesCommands::Edit { tf } => edit_milestone(settings, tf).await,
		MilestonesCommands::Healthcheck => healthcheck(settings).await,
	}
}
#[derive(Debug, Deserialize)]
struct Milestone {
	number: u64,
	title: String,
	#[serde(rename = "state")]
	_state: String,
	due_on: Option<Timestamp>,
	description: Option<String>,
}

async fn request_milestones(settings: &LiveSettings) -> Result<Vec<Milestone>> {
	let config = settings.config()?;
	let milestones_config = config
		.milestones
		.as_ref()
		.ok_or_else(|| eyre!("milestones config section is required. Add [milestones] section with url to your config"))?;

	// Parse owner/repo from URL (supports "owner/repo", "https://github.com/owner/repo", etc.)
	let url_str = &milestones_config.url;
	let (owner, repo) = parse_github_repo(url_str)?;

	let api_url = format!("https://api.github.com/repos/{owner}/{repo}/milestones");

	let client = Client::new();
	let res = client
		.get(&api_url)
		.header("User-Agent", "Rust Github Client")
		.header("Authorization", format!("token {}", config.github_token))
		.send()
		.await?;
	info!(?res);

	let milestones = res.json::<Vec<Milestone>>().await?;
	Ok(milestones)
}

/// Parse owner and repo from various Github URL formats
/// Supports: "owner/repo", "https://github.com/owner/repo", "git@github.com:owner/repo.git", etc.
fn parse_github_repo(url: &str) -> Result<(String, String)> {
	let url = url.trim();

	// Handle "owner/repo" format directly
	if !url.contains(':') && !url.contains("//") {
		let parts: Vec<&str> = url.split('/').collect();
		if parts.len() == 2 {
			return Ok((parts[0].to_string(), parts[1].trim_end_matches(".git").to_string()));
		}
	}

	// Handle URL formats
	let sections: Vec<&str> = url.split('/').collect();
	if sections.len() >= 2 {
		let owner = sections[sections.len() - 2];
		let repo = sections[sections.len() - 1].trim_end_matches(".git");

		// For SSH format (git@github.com:owner/repo), owner might contain ':'
		let owner = if owner.contains(':') { owner.split(':').next_back().unwrap_or(owner) } else { owner };

		return Ok((owner.to_string(), repo.to_string()));
	}

	Err(eyre!("Could not parse Github repo from URL: {url}"))
}

#[derive(Clone, Debug, thiserror::Error, PartialEq)]
#[error("Error on `{requested_tf}` milestone: {source}")]
struct GetMilestoneError {
	requested_tf: Timeframe,
	#[source]
	source: MilestoneError,
}

#[derive(Clone, Debug, thiserror::Error, PartialEq)]
enum MilestoneError {
	#[error("Milestone is missing due_on date")]
	MissingDueOn,

	#[error("Milestone is outdated (due_on: {due_on}). Try moving it to a later date.")]
	MilestoneOutdated { due_on: Timestamp },

	#[error("Requested milestone on minute-designated timeframe (`m`). You likely meant to request Monthly (`M`).")]
	MinuteMilestone,

	#[error("Milestone not found. Here are all the existing milestones:\n{existing_milestones:?}")]
	MilestoneNotFound { existing_milestones: Vec<String> },

	#[error("Missing description")]
	MissingDescription,
}

fn get_milestone(tf: Timeframe, retrieved_milestones: &[Milestone]) -> Result<String, GetMilestoneError> {
	if tf.designator() == TimeframeDesignator::Minutes {
		return Err(GetMilestoneError {
			requested_tf: tf,
			source: MilestoneError::MinuteMilestone,
		});
	}

	match retrieved_milestones.iter().find(|m| m.title == tf.to_string()) {
		Some(milestone) => {
			let due_on = milestone.due_on.as_ref().ok_or(GetMilestoneError {
				requested_tf: tf,
				source: MilestoneError::MissingDueOn,
			})?;

			if *due_on < Timestamp::now() {
				return Err(GetMilestoneError {
					requested_tf: tf,
					source: MilestoneError::MilestoneOutdated { due_on: *due_on },
				});
			}

			match milestone.description.clone() {
				Some(description) => Ok(description),
				None => Err(GetMilestoneError {
					requested_tf: tf,
					source: MilestoneError::MissingDescription,
				}),
			}
		}
		None => {
			let milestone_titles = retrieved_milestones.iter().map(|m| m.title.clone()).collect::<Vec<String>>();
			Err(GetMilestoneError {
				requested_tf: tf,
				source: MilestoneError::MilestoneNotFound {
					existing_milestones: milestone_titles,
				},
			})
		}
	}
}

static KEY_MILESTONES: [Timeframe; 6] = [
	Timeframe::from_naive(1, TimeframeDesignator::Days),
	Timeframe::from_naive(2, TimeframeDesignator::Weeks),
	Timeframe::from_naive(1, TimeframeDesignator::Quarters),
	Timeframe::from_naive(1, TimeframeDesignator::Years),
	Timeframe::from_naive(3, TimeframeDesignator::Years),
	Timeframe::from_naive(7, TimeframeDesignator::Years),
];

async fn healthcheck(settings: &LiveSettings) -> Result<()> {
	use std::fs;

	let healthcheck_path = v_utils::xdg_data_file!(HEALTHCHECK_REL_PATH);

	let retrieved_milestones = request_milestones(settings).await?;
	let results = KEY_MILESTONES
		.iter()
		.map(|tf| get_milestone(*tf, &retrieved_milestones))
		.collect::<Vec<Result<String, GetMilestoneError>>>();
	{
		let mut health = String::new();
		for result in &results {
			match result {
				Ok(_) => {}
				Err(e) => {
					if !health.is_empty() {
						health.push('\n');
					}
					health.push_str(&e.to_string());
				}
			}
		}

		if health.is_empty() {
			health = "OK".to_string();
		}
		println!("{}\n{health}", healthcheck_path.display());

		fs::write(healthcheck_path, health).unwrap();
	}

	{
		let sprint_ms =
			&<std::result::Result<std::string::String, GetMilestoneError> as Clone>::clone(&results[1]).map_err(|e| eyre!("Couldn't parse 2w milestone which MUST be defined: {e}"))?;
		let sprint_header = sprint_ms
			.lines()
			.next()
			.ok_or_else(|| eyre!("2w milestone does not have a description. MUST have a description."))?;
		if !sprint_header.starts_with("# ") {
			eprintln!("2w milestone description does not start with a header. It SHOULD start with '# '.");
		}
		fs::write(v_utils::xdg_data_file!(SPRINT_HEADER_REL_PATH), sprint_header).unwrap();
	}

	Ok(())
}

async fn edit_milestone(settings: &LiveSettings, tf: Timeframe) -> Result<()> {
	use std::{fs, path::Path};

	let retrieved_milestones = request_milestones(settings).await?;

	// Find the milestone matching the timeframe
	let milestone = retrieved_milestones.iter().find(|m| m.title == tf.to_string()).ok_or_else(|| {
		let existing = retrieved_milestones.iter().map(|m| m.title.clone()).collect::<Vec<_>>();
		eyre!("Milestone '{tf}' not found. Existing milestones: {existing:?}")
	})?;

	let original_description = milestone.description.clone().unwrap_or_default();
	let milestone_number = milestone.number;

	// Check if milestone is outdated (due_on in the past)
	let is_outdated = milestone.due_on.map(|d| d < Timestamp::now()).unwrap_or(true); //IGNORED_ERROR: no due date = treat as outdated

	// Write to temp file
	let tmp_path = format!("/tmp/milestone_{tf}.md");
	fs::write(&tmp_path, &original_description)?;

	// Open in editor
	v_utils::io::file_open::open(Path::new(&tmp_path)).await?;

	// Read back
	let new_description = fs::read_to_string(&tmp_path)?;

	// Clean up temp file
	let _ = fs::remove_file(&tmp_path);

	// Check if changed
	if new_description == original_description {
		println!("No changes made to milestone '{tf}'");
		return Ok(());
	}

	// If outdated, archive old contents and update date
	if is_outdated {
		let new_date = Timestamp::now() + tf.signed_duration();
		println!("Milestone was outdated, updating due date to {}", new_date.strftime("%Y-%m-%d"));

		// Archive title: "2025/12/17_1d"
		let archive_title = format!("{}_{tf}", Timestamp::now().strftime("%Y/%m/%d"));

		// Run both API calls in parallel: update current milestone and create archived one
		let (update_result, archive_result) = tokio::join!(
			update_milestone(settings, milestone_number, &new_description, Some(new_date)),
			create_closed_milestone(settings, &archive_title, &original_description)
		);

		update_result?;
		if let Err(e) = archive_result {
			eprintln!("Warning: Failed to archive old milestone contents: {e}");
		}
	} else {
		// Not outdated, just update description
		update_milestone(settings, milestone_number, &new_description, None).await?;
	}

	println!("Updated milestone '{tf}'");

	// Run healthcheck after update
	healthcheck(settings).await?;

	Ok(())
}

async fn update_milestone(settings: &LiveSettings, milestone_number: u64, description: &str, due_on: Option<Timestamp>) -> Result<()> {
	let config = settings.config()?;
	let milestones_config = config.milestones.as_ref().ok_or_else(|| eyre!("milestones config section is required"))?;

	let (owner, repo) = parse_github_repo(&milestones_config.url)?;
	let api_url = format!("https://api.github.com/repos/{owner}/{repo}/milestones/{milestone_number}");

	let mut body = serde_json::json!({
		"description": description
	});

	if let Some(date) = due_on {
		body["due_on"] = serde_json::Value::String(date.to_string());
	}

	let client = Client::new();
	let res = client
		.patch(&api_url)
		.header("User-Agent", "Rust Github Client")
		.header("Authorization", format!("token {}", config.github_token))
		.header("Content-Type", "application/json")
		.json(&body)
		.send()
		.await?;

	if !res.status().is_success() {
		let status = res.status();
		let body = res.text().await.unwrap_or_default();
		bail!("Failed to update milestone: {status} - {body}");
	}

	Ok(())
}

/// Creates a new milestone with the given title, description, and immediately closes it
async fn create_closed_milestone(settings: &LiveSettings, title: &str, description: &str) -> Result<()> {
	let config = settings.config()?;
	let milestones_config = config.milestones.as_ref().ok_or_else(|| eyre!("milestones config section is required"))?;

	let (owner, repo) = parse_github_repo(&milestones_config.url)?;
	let api_url = format!("https://api.github.com/repos/{owner}/{repo}/milestones");

	let body = serde_json::json!({
		"title": title,
		"description": description,
		"state": "closed"
	});

	let client = Client::new();
	let res = client
		.post(&api_url)
		.header("User-Agent", "Rust Github Client")
		.header("Authorization", format!("token {}", config.github_token))
		.header("Content-Type", "application/json")
		.json(&body)
		.send()
		.await?;

	if !res.status().is_success() {
		let status = res.status();
		let body = res.text().await.unwrap_or_default();
		bail!("Failed to create closed milestone: {status} - {body}");
	}

	Ok(())
}
