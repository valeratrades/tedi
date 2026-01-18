//! Sync local issue changes back to Github.
//!
//! ## Unified Sync Workflow
//!
//! All sync operations go through `sync_issue`, which:
//! 1. Takes local state, remote state, consensus (last committed), and a `MergeMode`
//! 2. Produces a merged state according to the mode
//! 3. Returns the merged issue for the caller to commit
//!
//! ## MergeMode semantics
//!
//! - `Normal`: Auto-resolve where only one side changed since consensus.
//!   If both changed the same thing → create git merge conflict.
//! - `Force(side)`: On conflicts, take the preferred side. Non-conflicting
//!   parts still merge normally.
//! - `Reset(side)`: Take preferred side entirely. Content only in the
//!   non-preferred side is deleted.
//!
//! ## Consensus-based comparison
//!
//! The git-committed state serves as "consensus" (last synced truth).
//! For each field/node:
//! - If only local changed since consensus → use local
//! - If only remote changed since consensus → use remote
//! - If both changed → apply MergeMode rules

/// Which side to prefer in merge operations.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Side {
	/// Prefer local file state
	Local,
	/// Prefer remote Github state
	Remote,
}

/// How to merge local and remote states.
#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub enum MergeMode {
	/// Auto-resolve conflicts where possible, create merge conflict otherwise.
	/// - Only local changed → use local
	/// - Only remote changed → use remote
	/// - Both changed → git merge conflict
	#[default]
	Normal,
	/// Force preferred side on conflicts, but keep non-conflicting parts from both.
	Force { prefer: Side },
	/// Reset to preferred side entirely. Content not in preferred side is deleted.
	Reset { prefer: Side },
}

/// Options for controlling sync behavior.
///
/// The `merge_mode` is consumed after first use (for pre-editor sync),
/// so post-editor sync runs with `MergeMode::Normal`.
#[derive(Debug, Default)]
pub struct SyncOptions {
	/// Merge mode for conflict resolution. Consumed after first use. For reasons, refer to https://github.com/valeratrades/todo/issues/83#issuecomment-3746995182
	merge_mode: std::cell::Cell<Option<MergeMode>>,
	/// Fetch and sync from remote before opening editor
	pub pull: bool,
}

impl SyncOptions {
	/// Create new sync options with the given merge mode and pull flag.
	pub fn new(merge_mode: Option<MergeMode>, pull: bool) -> Self {
		Self {
			merge_mode: std::cell::Cell::new(merge_mode),
			pull,
		}
	}

	/// Take the merge mode, returning Normal if already taken or not set.
	/// This ensures non-Normal modes are only used once.
	pub fn take_merge_mode(&self) -> MergeMode {
		self.merge_mode.take().unwrap_or_default()
	}

	/// Peek at the merge mode without consuming it.
	pub fn peek_merge_mode(&self) -> MergeMode {
		self.merge_mode.get().unwrap_or_default()
	}
}

use std::path::Path;

use todo::{CloseState, FetchedIssue, Issue};
use v_utils::prelude::*;

use super::{
	conflict::mark_conflict,
	consensus::{commit_issue_changes, is_git_initialized, load_consensus_issue},
	fetch::fetch_and_store_issue,
	local::Local,
	remote::load_full_issue_tree,
	sink::IssueSinkExt,
	tree::resolve_tree,
};
use crate::github::BoxedGithubClient;

//=============================================================================
// Sync implementation
//=============================================================================

/// Apply the merge mode to produce a merged issue.
///
/// Returns (merged_issue, local_needs_update, remote_needs_update).
/// - local_needs_update: true if local file should be rewritten with merged result
/// - remote_needs_update: true if changes should be pushed to Github
#[allow(clippy::too_many_arguments)]
async fn apply_merge_mode(
	local: &Issue,
	consensus: Option<&Issue>,
	remote: &Issue,
	mode: MergeMode,
	issue_file_path: &Path,
	owner: &str,
	repo: &str,
	issue_number: u64,
) -> Result<(Issue, bool, bool)> {
	// First, do the standard tree resolution to detect conflicts
	let resolution = resolve_tree(local, consensus, remote);

	match mode {
		MergeMode::Normal => {
			if resolution.has_conflicts {
				// Normal mode with conflicts: trigger git merge
				tracing::debug!("[sync] Unresolvable conflicts detected at paths: {:?}", resolution.conflict_paths);
				handle_divergence(issue_file_path, owner, repo, issue_number, remote).await?;
				// handle_divergence bails on conflict, so if we reach here it means merge succeeded
				// Re-read the merged file
				let merged_content = std::fs::read_to_string(issue_file_path)?;
				let merged = Issue::parse_virtual(&merged_content, issue_file_path)?;
				Ok((merged, false, true)) // File already written by merge, push to remote
			} else {
				// No conflicts, use resolution results
				if resolution.local_needs_update {
					tracing::debug!("[sync] Applying auto-resolved remote changes to local");
				}
				Ok((resolution.resolved, resolution.local_needs_update, resolution.remote_needs_update))
			}
		}
		MergeMode::Force { prefer } => {
			if resolution.has_conflicts {
				// Force mode resolves conflicts by taking the preferred side's content,
				// but non-conflicting additions from both sides are preserved.
				// Start with resolution.resolved which has merged non-conflicting changes.
				let mut merged = resolution.resolved.clone();

				// Apply preferred side's content to conflicting nodes
				for path in &resolution.conflict_paths {
					match prefer {
						Side::Local => {
							// Apply local content to this node
							if let Some(local_node) = get_node_at_path(local, path)
								&& let Some(merged_node) = get_node_at_path_mut(&mut merged, path)
							{
								apply_node_content(merged_node, local_node);
							}
						}
						Side::Remote => {
							// Apply remote content to this node
							if let Some(remote_node) = get_node_at_path(remote, path)
								&& let Some(merged_node) = get_node_at_path_mut(&mut merged, path)
							{
								apply_node_content(merged_node, remote_node);
							}
						}
					}
				}

				match prefer {
					Side::Local => {
						tracing::debug!("[sync] Force mode: resolved conflicts with local, preserving non-conflicting changes");
						println!("Force: local wins on conflicts (non-conflicting changes merged)");
						Ok((merged, resolution.local_needs_update, true))
					}
					Side::Remote => {
						tracing::debug!("[sync] Force mode: resolved conflicts with remote, preserving non-conflicting changes");
						println!("Force: remote wins on conflicts (non-conflicting changes merged)");
						Ok((merged, true, resolution.remote_needs_update))
					}
				}
			} else {
				// No conflicts, use normal resolution
				Ok((resolution.resolved, resolution.local_needs_update, resolution.remote_needs_update))
			}
		}
		MergeMode::Reset { prefer } => {
			// Reset takes preferred side entirely, regardless of conflicts
			match prefer {
				Side::Local => {
					// Reset to local: keep local as-is, push everything to remote
					tracing::debug!("[sync] Reset mode: taking local version entirely");
					println!("Reset: taking local version (remote will be overwritten)");
					Ok((local.clone(), false, true))
				}
				Side::Remote => {
					// Reset to remote: take remote entirely, don't push
					tracing::debug!("[sync] Reset mode: taking remote version entirely");
					println!("Reset: taking remote version (local replaced)");
					Ok((remote.clone(), true, false))
				}
			}
		}
	}
}

/// Get an immutable reference to a node at the given path in the issue tree.
fn get_node_at_path<'a>(issue: &'a Issue, path: &[usize]) -> Option<&'a Issue> {
	if path.is_empty() {
		return Some(issue);
	}
	let mut current = issue;
	for &idx in path {
		current = current.children.get(idx)?;
	}
	Some(current)
}

/// Get a mutable reference to a node at the given path in the issue tree.
fn get_node_at_path_mut<'a>(issue: &'a mut Issue, path: &[usize]) -> Option<&'a mut Issue> {
	if path.is_empty() {
		return Some(issue);
	}
	let mut current = issue;
	for &idx in path {
		current = current.children.get_mut(idx)?;
	}
	Some(current)
}

/// Apply the content of one issue node to another (body, comments, state, labels).
/// Does NOT modify children - only the node's own content.
fn apply_node_content(target: &mut Issue, source: &Issue) {
	target.contents.state = source.contents.state.clone();
	target.contents.labels = source.contents.labels.clone();
	target.contents.blockers = source.contents.blockers.clone();

	// Copy comments (body is first comment)
	target.contents.comments = source.contents.comments.clone();

	// Update timestamp from source identity (if both are linked)
	if let (Some(target_linked), Some(source_linked)) = (target.identity.remote.as_linked_mut(), source.identity.remote.as_linked()) {
		target_linked.ts = source_linked.ts;
	}
}

/// Handle divergence: both local and remote changed since last sync.
/// Creates a local `remote-state` branch with remote changes and initiates a git merge.
async fn handle_divergence(issue_file_path: &Path, owner: &str, repo: &str, issue_number: u64, remote_issue: &Issue) -> Result<()> {
	use std::process::Command;

	let data_dir = Local::issues_dir();
	let data_dir_str = data_dir.to_str().ok_or_else(|| eyre!("Invalid data directory path"))?;

	// Check if git is initialized
	if !is_git_initialized() {
		bail!(
			"Conflict detected: both local and remote have changes since last sync.\n\
			 \n\
			 To enable conflict resolution, initialize git in your issues directory:\n\
			   cd {} && git init\n\
			 \n\
			 Then re-run the command.",
			data_dir.display()
		);
	}

	// Get current branch name
	let branch_output = Command::new("git").args(["-C", data_dir_str, "rev-parse", "--abbrev-ref", "HEAD"]).output()?;
	let current_branch = String::from_utf8_lossy(&branch_output.stdout).trim().to_string();

	// Commit local changes with special prefix (so we know it's a conflict-in-progress)
	let _ = Command::new("git").args(["-C", data_dir_str, "add", "-A"]).status()?;

	let commit_msg = format!("__conflicts: local changes for issue #{issue_number}");
	let commit_output = Command::new("git").args(["-C", data_dir_str, "commit", "-m", &commit_msg]).output()?;

	if commit_output.status.success() {
		println!("Committed local changes.");
	}

	// Create branch for remote state
	let branch_name = "remote-state".to_string();

	// Delete branch if it exists (from previous attempt)
	let _ = Command::new("git").args(["-C", data_dir_str, "branch", "-D", &branch_name]).output();

	// Create new branch from current HEAD (before our conflict commit)
	let parent_output = Command::new("git").args(["-C", data_dir_str, "rev-parse", "HEAD~1"]).output();
	let base_commit = if let Ok(output) = parent_output
		&& output.status.success()
	{
		String::from_utf8_lossy(&output.stdout).trim().to_string()
	} else {
		"HEAD".to_string()
	};

	// Create remote-state branch from the base commit
	let branch_status = Command::new("git").args(["-C", data_dir_str, "branch", &branch_name, &base_commit]).status()?;

	if !branch_status.success() {
		bail!("Failed to create branch for remote state");
	}

	// Checkout the remote-state branch
	let checkout_status = Command::new("git").args(["-C", data_dir_str, "checkout", &branch_name]).status()?;

	if !checkout_status.success() {
		bail!("Failed to checkout remote-state branch");
	}

	// Write remote state to filesystem (each node to its own file)
	// Clone because sink_local takes &mut (for Remote's needs), but we don't mutate here
	remote_issue.clone().sink_local(None).await?;

	// Commit remote state (if there are any changes)
	let _ = Command::new("git").args(["-C", data_dir_str, "add", "-A"]).status()?;

	// Check if there are changes to commit
	let diff_status = Command::new("git").args(["-C", data_dir_str, "diff", "--cached", "--quiet"]).status()?;

	if !diff_status.success() {
		// There are staged changes, commit them
		let remote_commit_msg = format!("Remote state for {owner}/{repo}#{issue_number}");
		let commit_status = Command::new("git").args(["-C", data_dir_str, "commit", "-m", &remote_commit_msg]).status()?;

		if !commit_status.success() {
			// Restore original branch
			let _ = Command::new("git").args(["-C", data_dir_str, "checkout", &current_branch]).status();
			let _ = Command::new("git").args(["-C", data_dir_str, "branch", "-D", &branch_name]).status();
			bail!("Failed to commit remote state");
		}
	} else {
		// No changes - remote state matches local, no merge needed
		// Just switch back and clean up
		let _ = Command::new("git").args(["-C", data_dir_str, "checkout", &current_branch]).status();
		let _ = Command::new("git").args(["-C", data_dir_str, "branch", "-D", &branch_name]).status();
		println!("Remote state matches local, no changes needed.");
		return Ok(());
	}

	// Switch back to original branch
	let _ = Command::new("git").args(["-C", data_dir_str, "checkout", &current_branch]).status()?;

	// Attempt to merge remote-state into current branch
	println!("Merging remote changes...");
	let merge_output = Command::new("git")
		.args(["-C", data_dir_str, "merge", &branch_name, "-m", &format!("Merge remote state for issue #{issue_number}")])
		.output()?;

	if merge_output.status.success() {
		// Merge succeeded without conflicts - clean up
		let _ = Command::new("git").args(["-C", data_dir_str, "branch", "-D", &branch_name]).status();
		println!("Remote changes merged successfully.");
		return Ok(());
	}

	// Merge failed - likely due to conflicts
	let merge_stderr = String::from_utf8_lossy(&merge_output.stderr);
	let merge_stdout = String::from_utf8_lossy(&merge_output.stdout);

	// Check if it's actually a conflict (vs other error)
	if merge_stdout.contains("CONFLICT") || merge_stderr.contains("CONFLICT") || merge_stdout.contains("Automatic merge failed") {
		// Mark this file as having conflicts - blocks all operations until resolved
		let _ = mark_conflict(issue_file_path);

		bail!(
			"Conflict detected: both local and remote have changes for issue #{issue_number}.\n\
			 \n\
			 Git merge has been initiated. Resolve the conflicts in:\n\
			   {}\n\
			 \n\
			 To resolve:\n\
			 1. Edit the file to resolve conflict markers (<<<<<<< ======= >>>>>>>)\n\
			 2. Run: git add {} && git commit\n\
			 3. Re-run this command to sync your changes\n\
			 \n\
			 To abort the merge: git merge --abort",
			issue_file_path.display(),
			issue_file_path.display()
		);
	}

	// Some other error during merge
	Err(eyre!("Failed to merge remote changes:\n{}\n{}", merge_stdout.trim(), merge_stderr.trim()))
}

/// Result of applying a modifier to an issue.
pub struct ModifyResult {
	/// The text to display after the operation (e.g., "Popped: task name")
	pub output: Option<String>,
	/// Whether the file was modified by the user.
	/// When false (and not in integration test mode), skip all sync operations.
	pub file_modified: bool,
}

/// A modifier that can be applied to an issue file.
#[derive(Debug)]
pub enum Modifier {
	/// Open the file in an editor and wait for user to close it.
	/// If `open_at_blocker` is true, opens at the position of the last blocker item.
	Editor { open_at_blocker: bool },
	/// Pop the last blocker from the stack
	BlockerPop,
	/// Add a blocker to the stack
	BlockerAdd { text: String },
}

impl Modifier {
	/// Apply this modifier to an issue. Returns output to display.
	#[tracing::instrument]
	async fn apply(&self, issue: &mut Issue, issue_file_path: &Path) -> Result<ModifyResult> {
		match self {
			Modifier::Editor { open_at_blocker } => {
				// Serialize current state
				let content = issue.serialize_virtual();
				std::fs::write(issue_file_path, &content)?;

				// Record file modification time before opening editor
				let mtime_before = std::fs::metadata(issue_file_path)?.modified()?;

				// Calculate position if opening at blocker
				let position = if *open_at_blocker {
					issue.find_last_blocker_position().map(|(line, col)| crate::utils::Position::new(line, Some(col)))
				} else {
					None
				};

				// Open in editor (blocks until editor closes)
				crate::utils::open_file(issue_file_path, position).await?;

				// Check if file was modified by comparing modification times
				let mtime_after = std::fs::metadata(issue_file_path)?.modified()?;
				let file_modified = mtime_after != mtime_before;

				// Read edited content and re-parse
				let content = std::fs::read_to_string(issue_file_path)?;
				*issue = Issue::parse_virtual(&content, issue_file_path)?;

				Ok(ModifyResult { output: None, file_modified })
			}
			Modifier::BlockerPop => {
				use crate::blocker_interactions::BlockerSequenceExt;

				let popped = issue.contents.blockers.pop();
				let output = popped.map(|text| format!("Popped: {text}"));

				Ok(ModifyResult { output, file_modified: true })
			}
			Modifier::BlockerAdd { text } => {
				use crate::blocker_interactions::BlockerSequenceExt;

				issue.contents.blockers.add(text);
				let output = None; // will repeat it when printing the current

				Ok(ModifyResult { output, file_modified: true })
			}
		}
	}
}

/// Inner sync logic shared by open_local_issue and sync_issue_file.
///
/// This is the unified sync entry point. All sync operations go through here.
/// Uses the Sink trait to push changes to GitHub.
async fn sync_issue_to_github_inner(gh: &BoxedGithubClient, issue_file_path: &Path, owner: &str, repo: &str, issue_number: u64, issue: &mut Issue, merge_mode: MergeMode) -> Result<()> {
	// Load consensus from git (last committed state).
	// Consensus is REQUIRED for sync - if we're here, the file should be tracked.
	// For new issues, --touch creates them on Github and commits as consensus.
	// For URL mode, fetch commits as consensus.
	let consensus = load_consensus_issue(issue_file_path).await.ok_or_else(|| {
		eyre!(
			"BUG: Consensus missing for tracked issue.\n\
			 File: {}\n\
			 This indicates a bug - the file should have been committed before sync.\n\
			 Please report this issue.",
			issue_file_path.display()
		)
	})?;

	//=========================================================================
	// SYNC: Merge local and remote state, then push differences
	//=========================================================================
	// Pending items (new local sub-issues) are preserved during merge and
	// created on Github in post-sync. No pre-sync phase needed.
	let (local_needs_update, changed) = if issue.is_linked() {
		// Normal flow: fetch remote and merge
		let remote_issue = load_full_issue_tree(gh, owner, repo, issue_number).await?;

		// Apply merge mode to get the merged result
		let (merged, local_needs_update, remote_needs_update) = apply_merge_mode(issue, Some(&consensus), &remote_issue, merge_mode, issue_file_path, owner, repo, issue_number).await?;

		// Update issue with merged result
		*issue = merged;

		// Write local file if it needs updating
		if local_needs_update {
			println!("Remote changed, local file updated.");
			issue.sink_local(None).await?;
		}

		if !local_needs_update && !remote_needs_update {
			tracing::debug!("[sync] No changes detected");
		} else if remote_needs_update {
			tracing::debug!("[sync] Will push local changes to remote");
		}

		// Push differences to GitHub using Sink trait
		// Compare merged state against REMOTE (not consensus) to know what to push
		let changed = issue.sink_remote(Some(&remote_issue)).await?;

		(local_needs_update, changed)
	} else {
		// Issue was just created - sink with no old state
		// (everything is "new" relative to GitHub)
		let changed = issue.sink_remote(None).await?;
		(false, changed)
	};

	// Determine if we need to refresh from Github
	let needs_refresh = changed || local_needs_update;

	if needs_refresh {
		// Save updated issue tree (with new URLs for created issues)
		issue.sink_local(None).await?;

		// Get the actual issue number - may have been set by sink() if issue was pending
		let actual_issue_number = issue.number().unwrap_or(issue_number);

		// Re-fetch and update local file to reflect the synced state
		println!("Refreshing local issue file from Github...");

		// Determine parent issue info if this is a sub-issue
		let meta = Local::parse_path_identity(issue_file_path)?;
		let ancestors: Option<Vec<FetchedIssue>> = meta.parent_issue.and_then(|parent_num| {
			let parent_meta = Local::parse_path_identity(issue_file_path.parent()?.join("..").as_path()).ok()?;
			let fetched = FetchedIssue::from_parts(owner, repo, parent_num, &parent_meta.title)?;
			Some(vec![fetched])
		});

		// Store the old path before re-fetching
		let old_path = issue_file_path.to_path_buf();

		// Re-fetch creates file with potentially new title/state
		let new_path = fetch_and_store_issue(gh, owner, repo, actual_issue_number, ancestors).await?;

		// If the path changed, delete the old file
		if old_path != new_path && old_path.exists() {
			println!("Issue renamed/moved, removing old file: {old_path:?}");
			std::fs::remove_file(&old_path)?;

			// Handle old sub-issues directory cleanup
			let old_sub_dir = old_path.with_extension("");
			let old_sub_dir = if old_sub_dir.extension().is_some() { old_sub_dir.with_extension("") } else { old_sub_dir };

			let new_parent = new_path.parent();
			if old_sub_dir.is_dir()
				&& new_parent != Some(old_sub_dir.as_path())
				&& let Err(e) = std::fs::remove_dir_all(&old_sub_dir)
			{
				eprintln!("Warning: could not remove old sub-issues directory: {e}");
			}
		}

		// Commit the synced changes to local git
		commit_issue_changes(issue_file_path, owner, repo, actual_issue_number, None)?;
	} else {
		println!("No changes made.");
	}

	Ok(())
}

/// Open a local issue file with the default editor modifier.
/// If `open_at_blocker` is true, opens the editor at the position of the last blocker item.
#[tracing::instrument(level = "debug", skip(gh, sync_opts), target = "todo::open_interactions::sync")]
pub async fn open_local_issue(gh: &BoxedGithubClient, issue_file_path: &Path, offline: bool, sync_opts: SyncOptions, open_at_blocker: bool) -> Result<()> {
	modify_and_sync_issue(gh, issue_file_path, offline, Modifier::Editor { open_at_blocker }, sync_opts).await?;
	Ok(())
}

/// Modify a local issue file using the given modifier, then sync changes back to Github.
#[tracing::instrument(level = "debug", skip(gh), target = "todo::open_interactions::sync")]
pub async fn modify_and_sync_issue(gh: &BoxedGithubClient, issue_file_path: &Path, offline: bool, modifier: Modifier, sync_opts: SyncOptions) -> Result<ModifyResult> {
	use super::conflict::check_any_conflicts;

	// Check for any unresolved conflicts first
	check_any_conflicts()?;

	// Extract owner and repo from path
	let (owner, repo) = Local::extract_owner_repo(issue_file_path)?;

	// Auto-enable offline mode for virtual projects
	let offline = offline || Local::is_virtual_project(&owner, &repo);

	// Load metadata from path
	let meta = Local::parse_path_identity(issue_file_path)?;

	// Load the issue tree from filesystem using LazyIssue
	let mut issue = Issue::empty_local(todo::Ancestry::root(&owner, &repo));
	let source = super::local::LocalPath::submitted(issue_file_path.to_path_buf());
	<Issue as todo::LazyIssue<Local>>::identity(&mut issue, source.clone()).await;
	<Issue as todo::LazyIssue<Local>>::contents(&mut issue, source.clone()).await;
	Box::pin(<Issue as todo::LazyIssue<Local>>::children(&mut issue, source)).await;

	// === CONSENSUS-FIRST SYNC ===
	// We always show the user the consensus state, never raw local or raw remote.
	// If local differs from consensus, or if --pull was requested, we must sync first.
	//
	// Exception: `--reset` or `--force` with local preference (opening by local path without --pull)
	// means the user wants local to win - no pre-editor sync needed, post-editor sync handles it.
	//
	// This ensures the user always sees a consistent, synced view of the issue.
	if !offline && meta.issue_number != 0 {
		// Check if this prefers local (--reset/--force without --pull/URL)
		// In that case, skip pre-editor sync - post-editor sync will handle pushing local to remote
		let prefers_local = matches!(sync_opts.peek_merge_mode(), MergeMode::Reset { prefer: Side::Local } | MergeMode::Force { prefer: Side::Local });

		if !prefers_local {
			// Load consensus from git (last committed state)
			let consensus = load_consensus_issue(issue_file_path).await;

			// Determine if we need to sync before showing the user
			let local_differs_from_consensus = consensus.as_ref().map(|c| *c != issue).unwrap_or(false);
			let needs_pre_editor_sync = sync_opts.pull || local_differs_from_consensus;

			if needs_pre_editor_sync {
				let reason = if sync_opts.pull {
					"Pulling latest from Github..."
				} else {
					"Local differs from consensus, syncing..."
				};
				println!("{reason}");

				// Fetch remote state
				let remote_issue = load_full_issue_tree(gh, &owner, &repo, meta.issue_number).await?;

				// Consensus is required - if missing and local differs, that's a bug
				let consensus = consensus.ok_or_else(|| {
					eyre!(
						"BUG: Consensus missing but local file exists.\n\
						 File: {}\n\
						 This indicates a bug - the file should have been committed.",
						issue_file_path.display()
					)
				})?;

				// Take merge mode (consumes it, so post-editor sync will use Normal)
				let merge_mode = sync_opts.take_merge_mode();

				// Apply merge mode through unified sync logic
				let (merged, local_needs_update, _remote_needs_update) =
					apply_merge_mode(&issue, Some(&consensus), &remote_issue, merge_mode, issue_file_path, &owner, &repo, meta.issue_number).await?;

				if local_needs_update {
					// Write merged result to filesystem, passing old local state for file cleanup
					let old_local = std::mem::replace(&mut issue, merged);
					issue.sink_local(Some(&old_local)).await?;
					commit_issue_changes(issue_file_path, &owner, &repo, meta.issue_number, None)?;
				} else if issue != merged {
					// Issue changed but doesn't need file update (keeping local)
					let old_local = std::mem::replace(&mut issue, merged);
					// Still need to write the merged state for user to see
					issue.sink_local(Some(&old_local)).await?;
					commit_issue_changes(issue_file_path, &owner, &repo, meta.issue_number, None)?;
				} else {
					println!("Already up to date.");
				}
			}
		}
	}

	// Apply the modifier (editor, blocker command, etc.)
	let result = modifier.apply(&mut issue, issue_file_path).await?;

	// If file was not modified by the user, skip all sync operations and exit early.
	// Skip this check in integration tests to ensure they always run the full sync path.
	if !result.file_modified && std::env::var("__IS_INTEGRATION_TEST").is_err() {
		v_utils::log!("Aborted (no changes made)");
		return Ok(result);
	}

	// Handle duplicate close type: remove from local storage entirely
	if let CloseState::Duplicate(dup_number) = issue.contents.state {
		// Validate that the referenced duplicate issue exists
		if !offline {
			let exists = gh.fetch_issue(&owner, &repo, dup_number).await.is_ok();
			if !exists {
				bail!(
					"Cannot mark issue as duplicate of #{dup_number}: issue #{dup_number} does not exist in {owner}/{repo}.\n\
					 \n\
					 Check that the issue number is correct. The duplicate issue must exist in the same repository."
				);
			}
		}

		println!("Issue marked as duplicate of #{dup_number}, removing local file...");

		// Close on Github (if not already closed and not offline)
		let consensus = load_consensus_issue(issue_file_path)
			.await
			.expect("BUG: consensus missing for tracked issue during duplicate handling");
		let consensus_closed = consensus.contents.state.is_closed();
		if !offline && !consensus_closed {
			gh.update_issue_state(&owner, &repo, meta.issue_number, "closed").await?;
		}

		// Remove local file
		std::fs::remove_file(issue_file_path)?;

		// Remove sub-issues directory if it exists
		let sub_dir = issue_file_path.with_extension("");
		let sub_dir = if sub_dir.extension().is_some() { sub_dir.with_extension("") } else { sub_dir };
		if sub_dir.is_dir() {
			std::fs::remove_dir_all(&sub_dir)?;
		}

		// Commit the removal to git
		commit_issue_changes(issue_file_path, &owner, &repo, meta.issue_number, None)?;

		println!("Duplicate issue removed from local storage.");
		return Ok(result);
	}

	// Save the issue tree to filesystem (writes each node to its own file)
	issue.sink_local(None).await?;

	// If offline mode, skip all network operations
	if offline {
		println!("Offline mode: changes saved locally only.");
		return Ok(result);
	}

	// Post-editor sync: take merge mode (will be Normal if already consumed by pre-editor sync)
	let merge_mode = sync_opts.take_merge_mode();

	// Use shared sync logic
	sync_issue_to_github_inner(gh, issue_file_path, &owner, &repo, meta.issue_number, &mut issue, merge_mode).await?;

	Ok(result)
}

/// Modify a local issue file offline (no Github sync).
/// Use this when you know you're in offline mode and don't want to require a Github client.
#[tracing::instrument(level = "debug", target = "todo::open_interactions::sync")]
pub async fn modify_issue_offline(issue_file_path: &Path, modifier: Modifier) -> Result<ModifyResult> {
	// Load the issue tree from filesystem using LazyIssue
	let (owner, repo) = Local::extract_owner_repo(issue_file_path)?;
	let mut issue = Issue::empty_local(todo::Ancestry::root(&owner, &repo));
	let source = super::local::LocalPath::submitted(issue_file_path.to_path_buf());
	<Issue as todo::LazyIssue<Local>>::identity(&mut issue, source.clone()).await;
	<Issue as todo::LazyIssue<Local>>::contents(&mut issue, source.clone()).await;
	Box::pin(<Issue as todo::LazyIssue<Local>>::children(&mut issue, source)).await;

	// Apply the modifier (blocker command)
	let result = modifier.apply(&mut issue, issue_file_path).await?;

	// Save the issue tree to filesystem (writes each node to its own file)
	issue.sink_local(None).await?;

	println!("Offline mode: changes saved locally only.");

	Ok(result)
}
