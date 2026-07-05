//! Per-issue Clockify time tracking over the `tedi_adapters::clockify` protocol.
//!
//! The timer is keyed by the selected issue (`repo#number`). `resume`/`halt` start/stop
//! it; a small record file remembers which issue is being tracked so a selection change
//! can restart the timer on the new issue. There is no global blocker-tracking flag.

use std::{collections::HashMap, io::Write as IoWrite, path::PathBuf};

use clap::Parser;
use color_eyre::eyre::Result;
use serde::{Deserialize, Serialize};
use tedi_adapters::clockify as protocol;

#[derive(Clone, Debug, Default, Parser)]
pub struct ResumeArgs {
	/// Workspace ID or name (if omitted, use the user's active workspace)
	#[arg(short = 'w', long)]
	pub workspace: Option<String>,
	/// Project ID or name (if omitted, uses cached project default)
	#[arg(short = 'p', long)]
	pub project: Option<String>,
	/// Task ID or name (optional)
	#[arg(short = 't', long)]
	pub task: Option<String>,
	/// Comma-separated tag IDs or names (optional)
	#[arg(short = 'g', long)]
	pub tags: Option<String>,
	/// Mark entry as billable
	#[arg(short = 'b', long, default_value_t = false)]
	pub billable: bool,
}

#[derive(Clone, Debug, Parser)]
pub struct HaltArgs {
	/// Workspace ID or name (if omitted, use the user's active workspace)
	#[arg(short = 'w', long)]
	pub workspace: Option<String>,
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct WorkspaceSettings {
	pub fully_qualified: bool,
}

/// The issue key (`repo#number`) currently being tracked, if any.
pub fn tracked_issue() -> Option<String> {
	std::fs::read_to_string(tracked_issue_path()).ok().map(|s| s.trim().to_string()).filter(|s| !s.is_empty())
}

/// Record (or clear, with `None`) the issue key currently being tracked.
pub fn set_tracked_issue(key: Option<&str>) -> Result<()> {
	std::fs::write(tracked_issue_path(), key.unwrap_or(""))?;
	Ok(())
}

/// Get fully_qualified setting for a workspace, prompting the user if unset.
pub fn get_workspace_fully_qualified_setting(workspace: &str, yes: bool) -> Result<bool> {
	let cache = load_workspace_cache();
	if let Some(ws) = cache.workspaces.get(workspace) {
		return Ok(ws.fully_qualified);
	}
	println!("Workspace '{workspace}' fully-qualified mode setting not found.");
	let use_fully_qualified = if yes {
		println!("Use fully-qualified mode (legacy) for this workspace? [y/N]: y (--yes)");
		true
	} else {
		print!("Use fully-qualified mode (legacy) for this workspace? [y/N]: ");
		IoWrite::flush(&mut std::io::stdout())?;
		let mut input = String::new();
		std::io::stdin().read_line(&mut input)?;
		matches!(input.trim().to_lowercase().as_str(), "y" | "yes")
	};
	let mut cache = load_workspace_cache();
	cache.workspaces.insert(
		workspace.to_string(),
		WorkspaceSettings {
			fully_qualified: use_fully_qualified,
		},
	);
	save_workspace_cache(&cache)?;
	println!("Saved fully-qualified mode preference for workspace '{workspace}': {use_fully_qualified}");
	Ok(use_fully_qualified)
}

/// Stop the current time entry.
pub async fn stop_current_tracking(workspace: Option<&str>) -> Result<()> {
	protocol::stop_time_entry_with_defaults(workspace).await
}

/// Start tracking for a task. `get_description` returns the final description given the
/// workspace's fully_qualified setting.
pub async fn start_tracking_for_task<F>(get_description: F, resume_args: &ResumeArgs, workspace_override: Option<&str>, yes: bool) -> Result<()>
where
	F: FnOnce(bool) -> String, {
	let workspace = workspace_override.or(resume_args.workspace.as_deref());
	let fully_qualified = if let Some(ws) = workspace { get_workspace_fully_qualified_setting(ws, yes)? } else { false };
	let final_description = get_description(fully_qualified);
	protocol::start_time_entry_with_defaults(
		workspace,
		resume_args.project.as_deref(),
		final_description,
		resume_args.task.as_deref(),
		resume_args.tags.as_deref(),
		resume_args.billable,
		yes,
	)
	.await
}

static TRACKED_ISSUE_FILENAME: &str = "clockify_tracked_issue.txt";
static WORKSPACE_SETTINGS_FILENAME: &str = "workspace_settings.json";

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
struct WorkspaceCache {
	workspaces: HashMap<String, WorkspaceSettings>,
}

fn tracked_issue_path() -> PathBuf {
	crate::paths::state_file(TRACKED_ISSUE_FILENAME)
}
fn get_workspace_settings_path() -> PathBuf {
	crate::paths::cache_file(WORKSPACE_SETTINGS_FILENAME)
}
fn load_workspace_cache() -> WorkspaceCache {
	let cache_path = get_workspace_settings_path();
	match std::fs::read_to_string(&cache_path) {
		Ok(content) => serde_json::from_str(&content).unwrap_or_else(|e| panic!("corrupted workspace cache at {}: {e}", cache_path.display())),
		Err(e) if e.kind() == std::io::ErrorKind::NotFound => WorkspaceCache::default(),
		Err(e) => panic!("failed to read workspace cache at {}: {e}", cache_path.display()),
	}
}
fn save_workspace_cache(cache: &WorkspaceCache) -> Result<()> {
	std::fs::write(get_workspace_settings_path(), serde_json::to_string_pretty(cache)?)?;
	Ok(())
}
