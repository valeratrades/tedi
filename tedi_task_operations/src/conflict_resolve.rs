//! Conflict resolution: the operation-layer counterpart to `conflict_detect`.
//!
//! Opens the editor on an unresolved conflict, sinks the resolved state to local + remote, and
//! drives the git branch-merge that *creates* a conflict when local and remote diverge.
//!
//! 1. Commit local state to current branch
//! 2. Create `remote-state` branch from parent commit
//! 3. Write remote state (virtual format) to `{owner}/__conflict.md`
//! 4. Attempt git merge — if conflicts, user resolves with standard git tools

use std::{path::Path, process::Command};

use v_utils::prelude::*;

use crate::{
	Issue, IssueIndex, LazyIssue as _, RepoInfo, VirtualIssue,
	local::{
		FsReader, GitReader, Local, LocalFs, LocalIssueSource, LocalPath,
		conflict_detect::{ConflictBlockedError, ConflictError, ConflictOutcome, conflict_file_path, has_conflict_markers, is_merge_in_progress, milestone_conflict_file_path},
		consensus::is_git_initialized,
	},
	remote::Remote,
	sink::Sink,
};

/// Load a local source at `index`, first resolving any pending conflict via the editor.
/// Use at user-facing load sites; ancestor/internal loads use the pure `LocalIssueSource::build`.
pub async fn build_resolving(index: IssueIndex) -> Result<LocalIssueSource<FsReader>> {
	resolve_pending_conflict(index).await?;
	Ok(LocalIssueSource::<FsReader>::build(LocalPath::new(index)).await?)
}

/// `build_resolving` for a filesystem path (extracts the index first).
pub async fn build_resolving_from_path(path: &Path) -> Result<LocalIssueSource<FsReader>> {
	let index = Local::extract_index_from_path(path)?;
	build_resolving(index).await
}

/// If a pending conflict blocks `index`, open it for resolution and re-check.
/// Errors with `ConflictBlockedError` if markers still remain after the editor exits.
pub async fn resolve_pending_conflict(index: IssueIndex) -> Result<()> {
	if let Some(conflict_file) = check_for_existing_conflict(index).await? {
		eprintln!("Unresolved merge conflict in: {}", conflict_file.display());
		eprintln!("Opening for resolution...");
		let modified = v_utils::io::file_open::open(&conflict_file).await?;
		if !modified {
			return Err(ConflictBlockedError::new(conflict_file).into());
		}
		// Re-check after user exits editor
		if let Some(conflict_file) = check_for_existing_conflict(index).await? {
			return Err(ConflictBlockedError::new(conflict_file).into());
		}
	}
	Ok(())
}

/// Check for conflict file. If user fixed it, sinks.
///
/// Returns `Some(path)` if conflict markers are still present, `None` if resolved (or no conflict).
/// When the file exists but markers are gone, syncs the resolved content to local + remote sinks.
pub async fn check_for_existing_conflict(issue_index: IssueIndex) -> Result<Option<std::path::PathBuf>> {
	// virtual issues never sync, so they can never conflict
	if issue_index.repo_info().is_virtual() {
		return Ok(None);
	}
	let conflict_fpath = conflict_file_path(issue_index.owner().expect("github project"));

	if !conflict_fpath.exists() {
		return Ok(None);
	}

	let content = std::fs::read_to_string(&conflict_fpath)?;

	if has_conflict_markers(&content) {
		Ok(Some(conflict_fpath))
	} else {
		// have the conflict file, but user has had resolved it, - sync then cleanup
		{
			let mut new_issue = {
				let mut new_issue_but_old_local_timestamps = {
					let virtual_issue = VirtualIssue::parse(&content, conflict_fpath)?;
					let hollow = Local::read_hollow_from_project_meta(issue_index)?;
					Issue::from_combined(hollow, virtual_issue, issue_index.parent().unwrap(), issue_index.repo_info().is_virtual())?
				};

				let last_consensus_issue = Issue::load(LocalIssueSource::<GitReader>::build(LocalPath::from(issue_index))?).await?;

				new_issue_but_old_local_timestamps.post_update(&last_consensus_issue);
				new_issue_but_old_local_timestamps
			};

			<Issue as Sink<LocalFs>>::sink(&mut new_issue, None).await?;
			<Issue as Sink<Remote>>::sink(&mut new_issue, None).await?;
		}
		conflict_resolution_cleanup(conflict_file_path(issue_index.owner().expect("github project")))?;
		Ok(None)
	}
}

/// Milestone counterpart to `check_for_existing_conflict`: if the resolution file's markers
/// are gone, rebuild the milestone from it (identity from meta, timestamps bumped against
/// consensus) and sink to local + remote, then clean up.
pub async fn check_for_existing_milestone_conflict(link: &crate::MilestoneLink) -> Result<Option<std::path::PathBuf>> {
	let owner = link.repo_info().owner().expect("github project").to_string();
	let conflict_fpath = milestone_conflict_file_path(&owner);
	if !conflict_fpath.exists() {
		return Ok(None);
	}
	let content = std::fs::read_to_string(&conflict_fpath)?;
	if has_conflict_markers(&content) {
		return Ok(Some(conflict_fpath));
	}

	let mut milestone = {
		let body = crate::MilestoneBody::parse(&content);
		let meta = Local::load_milestone_project_meta(link.repo_info(), &FsReader)
			.milestones
			.remove(&link.number())
			.unwrap_or_default();
		let mut m = crate::Milestone {
			identity: crate::MilestoneIdentity {
				link: link.clone(),
				state: meta.state,
				due_on: meta.due_on,
				title: meta.title,
				timestamps: meta.timestamps,
			},
			body,
		};
		if let Some(consensus) = Local::load_milestone(link, &GitReader)? {
			m.post_update(&consensus);
		}
		m
	};

	<crate::Milestone as Sink<LocalFs>>::sink(&mut milestone, None).await?;
	<crate::Milestone as Sink<Remote>>::sink(&mut milestone, None).await?;
	conflict_resolution_cleanup(conflict_fpath)?;
	Ok(None)
}

//==============================================================================
// Conflict Creation (Git Branch Merge)
//==============================================================================

/// Initiate a git merge conflict between local and remote node states.
///
/// This creates a real git conflict by:
/// 1. Committing current local state
/// 2. Creating a `remote-state` branch with remote's state
/// 3. Merging that branch (which may produce conflicts)
///
/// Both `local_serialized` and `remote_serialized` are written to `conflict_file`.
/// Node-agnostic: the caller serializes (issue or milestone) and picks the conflict path.
pub fn initiate_conflict_merge(
	repo_info: RepoInfo,
	node_number: u64,
	local_serialized: &str,
	remote_serialized: &str,
	conflict_file: std::path::PathBuf,
) -> Result<ConflictOutcome, ConflictError> {
	let issue_number = node_number;
	if !is_git_initialized() {
		return Err(ConflictError::new_git_not_initialized());
	}

	let owner = repo_info.owner().expect("github project");
	let repo = repo_info.repo();

	let data_dir = Local::issues_dir();
	let data_dir_str = data_dir.to_str().ok_or_else(|| ConflictError::new_git_error("Invalid data directory path".into()))?;

	// Ensure the conflict file's parent directory exists
	if let Some(parent) = conflict_file.parent() {
		std::fs::create_dir_all(parent)?;
	}

	let conflict_file_rel = conflict_file.strip_prefix(&data_dir).unwrap_or(&conflict_file);
	let conflict_file_rel_str = conflict_file_rel.to_string_lossy();

	// Get current branch name
	let branch_output = Command::new("git").args(["-C", data_dir_str, "rev-parse", "--abbrev-ref", "HEAD"]).output()?;
	let current_branch = String::from_utf8_lossy(&branch_output.stdout).trim().to_string();

	// Write local state to conflict file (node + child links)
	std::fs::write(&conflict_file, local_serialized)?;

	// Stage and commit local state
	let add_status = Command::new("git").args(["-C", data_dir_str, "add", "-A"]).status()?;
	if !add_status.success() {
		return Err(ConflictError::new_git_error("git add -A failed".into()));
	}

	let commit_msg = format!("__conflict: local state for {owner}/{repo}#{issue_number}");
	let commit_output = Command::new("git").args(["-C", data_dir_str, "commit", "-m", &commit_msg]).output()?;

	let local_committed = commit_output.status.success();
	if local_committed {
		tracing::debug!("[conflict] Committed local state");
	}

	// Get base commit (parent of our commit, or HEAD if nothing to commit)
	let base_commit = if local_committed {
		let parent_output = Command::new("git").args(["-C", data_dir_str, "rev-parse", "HEAD~1"]).output()?;
		if parent_output.status.success() {
			String::from_utf8_lossy(&parent_output.stdout).trim().to_string()
		} else {
			"HEAD".to_string()
		}
	} else {
		"HEAD".to_string()
	};

	// Delete remote-state branch if it exists
	let _ = Command::new("git").args(["-C", data_dir_str, "branch", "-D", "remote-state"]).output();

	// Create remote-state branch from base
	let branch_status = Command::new("git").args(["-C", data_dir_str, "branch", "remote-state", &base_commit]).status()?;

	if !branch_status.success() {
		return Err(ConflictError::new_git_error("Failed to create remote-state branch".into()));
	}

	// Checkout remote-state branch
	let checkout_status = Command::new("git").args(["-C", data_dir_str, "checkout", "remote-state"]).status()?;

	if !checkout_status.success() {
		cleanup_branch(data_dir_str, &current_branch);
		return Err(ConflictError::new_git_error("Failed to checkout remote-state branch".into()));
	}

	// Write remote state to conflict file (node + child links)
	std::fs::write(&conflict_file, remote_serialized)?;

	// Stage and commit remote state
	let add_status = Command::new("git").args(["-C", data_dir_str, "add", "-A"]).status()?;
	if !add_status.success() {
		cleanup_branch(data_dir_str, &current_branch);
		return Err(ConflictError::new_git_error("git add -A failed".into()));
	}

	// Check if there are changes to commit
	let diff_status = Command::new("git").args(["-C", data_dir_str, "diff", "--cached", "--quiet"]).status()?;

	if diff_status.success() {
		// No changes - states are identical
		let _ = Command::new("git").args(["-C", data_dir_str, "checkout", &current_branch]).status();
		cleanup_branch(data_dir_str, &current_branch);
		return Ok(ConflictOutcome::NoChanges);
	}

	let remote_commit_msg = format!("__conflict: remote state for {owner}/{repo}#{issue_number}");
	let commit_status = Command::new("git").args(["-C", data_dir_str, "commit", "-m", &remote_commit_msg]).status()?;

	if !commit_status.success() {
		let _ = Command::new("git").args(["-C", data_dir_str, "checkout", &current_branch]).status();
		cleanup_branch(data_dir_str, &current_branch);
		return Err(ConflictError::new_git_error("Failed to commit remote state".into()));
	}

	// Switch back to original branch
	let _ = Command::new("git").args(["-C", data_dir_str, "checkout", &current_branch]).status()?;

	// Attempt merge
	tracing::debug!("[conflict] Attempting merge of remote-state into {current_branch}");
	let merge_output = Command::new("git")
		.args([
			"-C",
			data_dir_str,
			"merge",
			"remote-state",
			"-m",
			&format!("Merge remote state for {owner}/{repo}#{issue_number}"),
		])
		.output()?;

	if merge_output.status.success() {
		// Merge succeeded without conflicts
		cleanup_branch(data_dir_str, &current_branch);
		tracing::debug!("[conflict] Merge succeeded automatically");
		return Ok(ConflictOutcome::AutoMerged);
	}

	// Check if it's a conflict or other error
	let stdout = String::from_utf8_lossy(&merge_output.stdout);
	let stderr = String::from_utf8_lossy(&merge_output.stderr);

	//TODO: switch all actual git operations to [gitoxide](https://docs.rs/gitoxide/latest/gitoxide/), to not have to parse STDOUT
	if stdout.contains("CONFLICT") || stderr.contains("CONFLICT") || stdout.contains("Automatic merge failed") {
		tracing::debug!("[conflict] Merge produced conflicts in {conflict_file_rel_str}");
		// Don't cleanup branch - user needs it for resolution
		Ok(ConflictOutcome::NeedsResolution)
	} else {
		// Some other error
		let _ = Command::new("git").args(["-C", data_dir_str, "merge", "--abort"]).status();
		cleanup_branch(data_dir_str, &current_branch);
		Err(ConflictError::new_git_error(format!("Merge failed: {}\n{}", stdout.trim(), stderr.trim())))
	}
}

/// Complete the conflict resolution process.
///
/// Call this after user has resolved conflicts and committed.
/// Cleans up the remote-state branch.
pub fn conflict_resolution_cleanup(conflict_file: std::path::PathBuf) -> Result<()> {
	let data_dir = Local::issues_dir();
	let data_dir_str = data_dir.to_str().ok_or_else(|| eyre!("Invalid data directory path"))?;

	// Ensure we're not in the middle of a merge
	if is_merge_in_progress() {
		bail!("Git merge is still in progress. Complete the merge first: git add <file> && git commit");
	}

	// Cleanup branch
	let _ = Command::new("git").args(["-C", data_dir_str, "branch", "-D", "remote-state"]).output();

	// Remove the conflict file after successful resolution.
	if conflict_file.exists() {
		std::fs::remove_file(&conflict_file)?;
	}

	Ok(())
}

/// Cleanup the remote-state branch after merge completes.
fn cleanup_branch(data_dir_str: &str, _current_branch: &str) {
	let _ = Command::new("git").args(["-C", data_dir_str, "branch", "-D", "remote-state"]).output();
}
