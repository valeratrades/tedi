use clap::{Args, Subcommand};
use jiff::Timestamp;
use reqwest::Client;
use tedi::{
	Issue, IssueLink, LazyIssue, MilestoneBlockerCache, collapse_to_links, find_embedded_issues,
	local::{FsReader, Local, LocalIssueSource},
	parse_blockers_from_embedded, parse_embedded_title_line, parse_shorthand_ref, serialize_blockers_view,
};
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
		#[arg(long)]
		offline: bool,
	},
	/// Ensures all milestones up to date, if yes - writes "OK" to $XDG_DATA_HOME/todo/healthcheck.status
	/// Can get outdated easily, so printed output of the command is prepended with the filename
	Healthcheck,
}

pub async fn milestones_command(settings: &LiveSettings, args: MilestonesArgs, mock: Option<crate::MockType>) -> Result<()> {
	match args.command {
		MilestonesCommands::Get { tf } => {
			let retrieved_milestones = request_milestones(settings).await?;
			let milestone = get_milestone(tf, &retrieved_milestones)?;
			println!("{milestone}");
			Ok(())
		}
		MilestonesCommands::Edit { tf, offline } => edit_milestone(settings, tf, offline, mock.is_some()).await,
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

	// Cache the blocker timeframe milestone for blocker commands
	cache_blocker_milestone(settings, &retrieved_milestones);

	Ok(())
}

/// Cache the blocker timeframe milestone description for synchronous blocker commands.
fn cache_blocker_milestone(settings: &LiveSettings, milestones: &[Milestone]) {
	let config = match settings.config() {
		Ok(c) => c,
		Err(_) => return,
	};
	let blocker_tf = config.milestones.as_ref().map(|m| m.blocker_tf()).unwrap_or("1d");

	if let Some(ms) = milestones.iter().find(|m| m.title == blocker_tf)
		&& let Some(ref desc) = ms.description
		&& let Err(e) = MilestoneBlockerCache::update_from_description(desc)
	{
		eprintln!("Warning: failed to cache blocker milestone: {e}");
	}
}

async fn edit_milestone(settings: &LiveSettings, tf: Timeframe, offline: bool, mock: bool) -> Result<()> {
	use std::fs;

	let (original_description, milestone_number, is_outdated) = if mock {
		// In mock mode, read milestone content from env-specified file
		let mock_milestone_path =
			std::env::var(concat!(env!("CARGO_PKG_NAME"), "_MOCK_MILESTONE")).map_err(|_| eyre!("mock mode requires {}_MOCK_MILESTONE env var", env!("CARGO_PKG_NAME")))?;
		let content = fs::read_to_string(&mock_milestone_path)?;
		(content, 0, false)
	} else {
		let retrieved_milestones = request_milestones(settings).await?;
		let milestone = retrieved_milestones.iter().find(|m| m.title == tf.to_string()).ok_or_else(|| {
			let existing = retrieved_milestones.iter().map(|m| m.title.clone()).collect::<Vec<_>>();
			eyre!("Milestone '{tf}' not found. Existing milestones: {existing:?}")
		})?;
		let desc = milestone.description.clone().unwrap_or_default();
		let num = milestone.number;
		let outdated = milestone.due_on.map(|d| d < Timestamp::now()).unwrap_or(true);
		(desc, num, outdated)
	};

	// Expand shorthand refs and refresh embedded issues before editing
	let expanded_description = expand_and_refresh(&original_description).await?;

	// Write to temp file
	let tmp_dir = tempfile::tempdir()?;
	let tmp_path = tmp_dir.path().join(format!("milestone_{tf}.md"));
	fs::write(&tmp_path, &expanded_description)?;
	eprintln!("[milestone] tmp_path: {}", tmp_path.display());

	// Open in editor
	crate::utils::open_file(&tmp_path, None).await?;

	// Read back
	let edited_content = fs::read_to_string(&tmp_path)?;

	// Check if changed (compare against expanded, not original — expansion itself is not a user edit)
	if edited_content == expanded_description {
		println!("No changes made to milestone '{tf}'");
		return Ok(());
	}

	// Sync blocker changes back to individual issue files
	sync_blocker_changes(&edited_content, offline).await?;

	// Collapse expanded issues back to bare links for storage
	let new_description = collapse_to_links(&edited_content);

	if mock {
		// In mock mode, write result back to the milestone file
		let mock_milestone_path = std::env::var(concat!(env!("CARGO_PKG_NAME"), "_MOCK_MILESTONE")).unwrap();
		fs::write(&mock_milestone_path, &new_description)?;
		println!("Updated milestone '{tf}'");
		return Ok(());
	}

	// Update the blocker cache if this is the blocker timeframe milestone
	let config = settings.config()?;
	let blocker_tf = config.milestones.as_ref().map(|m| m.blocker_tf()).unwrap_or("1d");
	if tf.to_string() == blocker_tf
		&& let Err(e) = MilestoneBlockerCache::update_from_description(&new_description)
	{
		eprintln!("Warning: failed to update blocker cache: {e}");
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

//==============================================================================
// Embedded issue operations (async wrappers around lib functions)
//==============================================================================

/// Load a local issue by its IssueLink.
async fn load_local_issue(link: &IssueLink) -> Result<Issue> {
	let path = Local::find_by_number(link.repo_info(), link.number(), FsReader).ok_or_else(|| eyre!("issue #{} not found locally", link.number()))?;
	let local_source = LocalIssueSource::<FsReader>::build_from_path(&path).await?;
	Issue::load(local_source).await.map_err(Into::into)
}

/// Expand shorthand refs and refresh all embedded issue sections from local state.
///
/// Preserves the indentation prefix of the original line on the expanded view.
async fn expand_and_refresh(content: &str) -> Result<String> {
	fn is_indented(line: &str) -> bool {
		line.starts_with('\t') || line.starts_with(' ')
	}

	/// Extract the leading whitespace prefix from a line.
	fn indent_prefix(line: &str) -> &str {
		let trimmed = line.trim_start();
		&line[..line.len() - trimmed.len()]
	}

	/// Prepend `prefix` to each line of `text`.
	fn indent_block(text: &str, prefix: &str) -> String {
		if prefix.is_empty() {
			return text.to_string();
		}
		text.lines()
			.map(|line| if line.is_empty() { line.to_string() } else { format!("{prefix}{line}") })
			.collect::<Vec<_>>()
			.join("\n")
	}

	let lines: Vec<&str> = content.lines().collect();
	let mut result_lines: Vec<String> = Vec::new();
	let mut i = 0;

	while i < lines.len() {
		// Check for shorthand ref or bare URL: `owner/repo#123` or `https://...`
		if let Some(link) = parse_shorthand_ref(lines[i]) {
			let prefix = indent_prefix(lines[i]);
			let ref_line = i;
			i += 1;
			// Skip any indented content trailing the ref (leftover from previous expanded state)
			while i < lines.len() && (is_indented(lines[i]) || lines[i].trim().is_empty()) {
				if lines[i].trim().is_empty() && (i + 1 >= lines.len() || (!is_indented(lines[i + 1]) && !lines[i + 1].trim().is_empty())) {
					break;
				}
				i += 1;
			}
			match load_local_issue(&link).await {
				Ok(issue) => {
					result_lines.push(indent_block(&serialize_blockers_view(&issue), prefix));
				}
				Err(e) => {
					eprintln!("Warning: failed to expand {}: {e}", lines[ref_line].trim());
					result_lines.push(lines[ref_line].to_string());
				}
			}
			continue;
		}

		// Check for embedded issue title line — refresh from local
		if let Some(link) = parse_embedded_title_line(lines[i]) {
			let prefix = indent_prefix(lines[i]);
			let old_start = i;
			i += 1;
			// Skip the old embedded content
			while i < lines.len() && (is_indented(lines[i]) || lines[i].trim().is_empty()) {
				if lines[i].trim().is_empty() && (i + 1 >= lines.len() || (!is_indented(lines[i + 1]) && !lines[i + 1].trim().is_empty())) {
					break;
				}
				i += 1;
			}

			match load_local_issue(&link).await {
				Ok(issue) => {
					result_lines.push(indent_block(&serialize_blockers_view(&issue), prefix));
				}
				Err(e) => {
					eprintln!("Warning: failed to refresh {}/{}/#{}: {e}", link.owner(), link.repo(), link.number());
					for line in &lines[old_start..i] {
						result_lines.push(line.to_string());
					}
				}
			}
			continue;
		}

		// Regular line — keep as-is
		result_lines.push(lines[i].to_string());
		i += 1;
	}

	Ok(result_lines.join("\n"))
}

/// Parse blocker changes from edited milestone content and sync back to issue files.
async fn sync_blocker_changes(content: &str, offline: bool) -> Result<()> {
	use crate::open_interactions::{Modifier, SyncOptions, modify_and_sync_issue};

	let embedded = find_embedded_issues(content);

	let lines: Vec<&str> = content.lines().collect();

	for eref in &embedded {
		// Extract the embedded section text
		let section_lines: Vec<&str> = lines[eref.line_start..eref.line_end].to_vec();
		let section_text = section_lines.join("\n");

		// Parse blockers from the embedded section
		let edited_blockers = parse_blockers_from_embedded(&section_text);

		// Load the real issue from local fs
		let issue = match load_local_issue(&eref.link).await {
			Ok(issue) => issue,
			Err(e) => {
				eprintln!("Warning: failed to load {}/{}/#{} for sync: {e}", eref.link.owner(), eref.link.repo(), eref.link.number());
				continue;
			}
		};

		// Compare blockers — only sync if they actually differ
		if issue.contents.blockers == edited_blockers {
			continue;
		}

		println!("Syncing blocker changes for {}/{}#{}", eref.link.owner(), eref.link.repo(), eref.link.number());

		let modifier = Modifier::BlockerWrite { blockers: edited_blockers };
		let sync_opts = SyncOptions::default();
		modify_and_sync_issue(issue, offline, modifier, sync_opts).await?;
	}

	Ok(())
}
