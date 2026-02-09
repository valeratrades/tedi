//! Blocker-aware time tracking state management.
//!
//! Handles halt/resume commands and automatic task switching when blockers change.

use std::{collections::HashMap, io::Write as IoWrite};

use clap::Parser;
use color_eyre::eyre::Result;
use serde::{Deserialize, Serialize};

use super::protocol;

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct WorkspaceSettings {
	pub fully_qualified: bool,
}
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
/// Check if blocker tracking is enabled
pub fn is_tracking_enabled() -> bool {
	let state_path = get_blocker_state_path();
	match std::fs::read_to_string(&state_path) {
		Ok(content) => content.trim() == "true",
		Err(e) if e.kind() == std::io::ErrorKind::NotFound => {
			// File doesn't exist, create it with "false" and return false
			if let Err(e) = std::fs::write(&state_path, "false") {
				tracing::warn!("failed to initialize tracking state file: {e}");
			}
			false
		}
		Err(e) => {
			tracing::warn!("failed to read tracking state file: {e}");
			false
		}
	}
}
/// Set blocker tracking state (enabled/disabled)
pub fn set_tracking_enabled(enabled: bool) -> Result<()> {
	let state_path = get_blocker_state_path();
	std::fs::write(&state_path, if enabled { "true" } else { "false" })?;
	Ok(())
}
/// Get fully_qualified setting for a workspace, prompting user if not set
pub fn get_workspace_fully_qualified_setting(workspace: &str) -> Result<bool> {
	let cache = load_workspace_cache();

	if let Some(settings) = cache.workspaces.get(workspace) {
		Ok(settings.fully_qualified)
	} else {
		// Ask user for preference
		println!("Workspace '{workspace}' fully-qualified mode setting not found.");
		print!("Use fully-qualified mode (legacy) for this workspace? [y/N]: ");
		IoWrite::flush(&mut std::io::stdout())?;

		let mut input = String::new();
		std::io::stdin().read_line(&mut input)?;
		let use_fully_qualified = input.trim().to_lowercase() == "y" || input.trim().to_lowercase() == "yes";

		// Save the preference
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
}
/// Stop current time tracking
pub async fn stop_current_tracking(workspace: Option<&str>) -> Result<()> {
	protocol::stop_time_entry_with_defaults(workspace).await
}
/// Start tracking for a task with the given description
///
/// `get_description` is a callback that returns the final description to use,
/// given the fully_qualified setting for the workspace.
pub async fn start_tracking_for_task<F>(get_description: F, resume_args: &ResumeArgs, workspace_override: Option<&str>) -> Result<()>
where
	F: FnOnce(bool) -> String, {
	let workspace = workspace_override.or(resume_args.workspace.as_deref());

	// Determine fully_qualified mode from workspace settings (legacy mode for clockify)
	let fully_qualified = if let Some(ws) = workspace {
		get_workspace_fully_qualified_setting(ws)?
	} else {
		// If no workspace specified, use default (false)
		false
	};

	let final_description = get_description(fully_qualified);

	protocol::start_time_entry_with_defaults(
		workspace,
		resume_args.project.as_deref(),
		final_description,
		resume_args.task.as_deref(),
		resume_args.tags.as_deref(),
		resume_args.billable,
	)
	.await
}
static BLOCKER_STATE_FILENAME: &str = "blocker_state.txt";
static WORKSPACE_SETTINGS_FILENAME: &str = "workspace_settings.json";
#[derive(Clone, Debug, Default, Deserialize, Serialize)]
struct WorkspaceCache {
	workspaces: HashMap<String, WorkspaceSettings>,
}
fn get_blocker_state_path() -> std::path::PathBuf {
	v_utils::xdg_state_file!(BLOCKER_STATE_FILENAME)
}
fn get_workspace_settings_path() -> std::path::PathBuf {
	v_utils::xdg_cache_file!(WORKSPACE_SETTINGS_FILENAME)
}
fn load_workspace_cache() -> WorkspaceCache {
	let cache_path = get_workspace_settings_path();
	match std::fs::read_to_string(&cache_path) {
		Ok(content) => match serde_json::from_str(&content) {
			Ok(cache) => cache,
			Err(e) => panic!("corrupted workspace cache at {}: {e}", cache_path.display()),
		},
		Err(e) if e.kind() == std::io::ErrorKind::NotFound => WorkspaceCache::default(),
		Err(e) => panic!("failed to read workspace cache at {}: {e}", cache_path.display()),
	}
}
fn save_workspace_cache(cache: &WorkspaceCache) -> Result<()> {
	let cache_path = get_workspace_settings_path();
	let content = serde_json::to_string_pretty(cache)?;
	std::fs::write(&cache_path, content)?;
	Ok(())
}
