//! Sync local issue changes with Github.
//!
//! ## Sync Algorithm
//!
//! The sync uses a four-merge approach:
//! ```text
//! local.merge(consensus, false);  local.merge(remote, force);
//! remote.merge(consensus, false); remote.merge(local, force);
//!
//! if local == remote { resolved } else { conflict }
//! ```
//!
//! ## MergeMode Semantics
//!
//! - `Normal`: Use timestamp-based resolution. Conflict if timestamps equal.
//! - `Force { prefer }`: On conflicts, take preferred side.
//! - `Reset { prefer }`: Take preferred side entirely.
//!
//! ## MergeMode Consumption
//!
//! The merge mode is consumed after first use (pre-editor sync), so post-editor
//! sync always uses `Normal`. This prevents accidental data loss.

use std::path::Path;

use color_eyre::eyre::{Result, bail};
use tedi::{
	CloseState, Issue, IssueLink, LazyIssue,
	local::{Local, LocalPath, Submitted},
	sink::Sink,
};

use super::{
	conflict::{ConflictOutcome, complete_conflict_resolution, initiate_conflict_merge, read_resolved_conflict},
	consensus::{commit_issue_changes, load_consensus_issue},
	merge::Merge,
	remote::{Remote, RemoteSource},
};

/// Modify a local issue, then sync changes back to Github.
///
/// Caller is responsible for loading the issue (via `LazyIssue<Local>::load`).
#[tracing::instrument]
pub async fn modify_and_sync_issue(mut issue: Issue, offline: bool, modifier: Modifier, sync_opts: SyncOptions) -> Result<ModifyResult> {
	let issue_file_path = Local::issue_file_path(&issue)?;
	let repo_info = issue.identity.ancestry.repo_info();
	let (owner, repo) = (repo_info.owner(), repo_info.repo()); //HACK: feels redundant

	let offline = offline || Local::is_virtual_project(issue.identity.ancestry.repo_info());
	let meta = Local::parse_path_identity(&issue_file_path)?;

	eprintln!("[after load] issue lineage: {:?}", issue.identity.ancestry.lineage());
	for (i, c) in issue.children.iter().enumerate() {
		eprintln!("[after load] child[{i}] lineage: {:?}", c.identity.ancestry.lineage());
	}

	// Handle virtual issues - they don't sync
	if issue.identity.remote.is_virtual() {
		let result = modifier.apply(&mut issue, &issue_file_path).await?;
		if result.file_modified {
			<Issue as Sink<Submitted>>::sink(&mut issue, None).await?;
		}
		return Ok(result);
	}

	// Pre-editor sync (if needed and online)
	if !offline && meta.issue_number != 0 {
		let prefers_local = matches!(sync_opts.peek_merge_mode(), MergeMode::Reset { prefer: Side::Local } | MergeMode::Force { prefer: Side::Local });

		if !prefers_local {
			let consensus = load_consensus_issue(&issue_file_path).await?;
			let local_differs = consensus.as_ref().map(|c| *c != issue).unwrap_or(false);

			if sync_opts.pull || local_differs {
				println!("{}", if sync_opts.pull { "Pulling latest..." } else { "Syncing..." });

				// Load remote
				let url = format!("https://github.com/{owner}/{repo}/issues/{}", meta.issue_number);
				let link = IssueLink::parse(&url).expect("valid URL");
				let remote_source = RemoteSource::with_lineage(link, issue.identity.ancestry().lineage());
				let remote = <Issue as LazyIssue<Remote>>::load(remote_source).await?;

				let mode = sync_opts.take_merge_mode();
				let (resolved, changed) = core::sync_issue(issue, consensus, remote, mode, &owner, &repo, meta.issue_number).await?;
				issue = resolved;

				if changed {
					commit_issue_changes(&owner, &repo, meta.issue_number)?;
				}
			}
		}
	}

	// Apply modifier
	let result = modifier.apply(&mut issue, &issue_file_path).await?;

	// Early exit if no changes (unless in test mode)
	if !result.file_modified && std::env::var("__IS_INTEGRATION_TEST").is_err() {
		v_utils::log!("Aborted (no changes made)");
		return Ok(result);
	}

	// Save locally (Sink<Submitted> handles duplicate removal)
	eprintln!("[save locally] issue lineage: {:?}", issue.identity.ancestry.lineage());
	for (i, c) in issue.children.iter().enumerate() {
		eprintln!("[save locally] child[{i}] lineage: {:?}", c.identity.ancestry.lineage());
	}
	<Issue as Sink<Submitted>>::sink(&mut issue, None).await?;

	if offline {
		println!("Offline: saved locally.");
		return Ok(result);
	}

	// Post-editor sync
	let mode = sync_opts.take_merge_mode();

	if issue.is_linked() {
		// Load fresh remote state for sync
		let url = format!("https://github.com/{owner}/{repo}/issues/{}", meta.issue_number);
		let link = IssueLink::parse(&url).expect("valid URL");
		let remote_source = RemoteSource::with_lineage(link, issue.identity.ancestry().lineage());
		let remote = <Issue as LazyIssue<Remote>>::load(remote_source).await?;

		let consensus = load_consensus_issue(&issue_file_path).await?;
		let (resolved, changed) = core::sync_issue(issue, consensus, remote, mode, &owner, &repo, meta.issue_number).await?;
		issue = resolved;

		if changed {
			// Re-sink local in case issue numbers changed
			<Issue as Sink<Submitted>>::sink(&mut issue, None).await?;
			let actual_number = issue.number().unwrap_or(meta.issue_number);
			commit_issue_changes(&owner, &repo, actual_number)?;
		} else {
			println!("No changes.");
		}
	} else {
		// New issue - just sink to remote
		<Issue as Sink<Remote>>::sink(&mut issue, None).await?;
		<Issue as Sink<Submitted>>::sink(&mut issue, None).await?;
		let actual_number = issue.number().unwrap_or(meta.issue_number);
		commit_issue_changes(&owner, &repo, actual_number)?;
	}

	Ok(result)
}

mod core {
	use super::*;
	/// Sync an issue between local and remote using the four-merge algorithm.
	///
	/// ```text
	/// local.merge(consensus, false);  local.merge(remote, force);
	/// remote.merge(consensus, false); remote.merge(local, force);
	///
	/// if local == remote { resolved } else { conflict }
	/// ```
	///
	/// Returns `(resolved_issue, changed)` where `changed` indicates if any updates were made.
	pub(super) async fn sync_issue(local: Issue, consensus: Option<Issue>, remote: Issue, mode: MergeMode, owner: &str, repo: &str, issue_number: u64) -> Result<(Issue, bool)> {
		// Handle Reset mode - take one side entirely
		if let MergeMode::Reset { prefer } = mode {
			return match prefer {
				Side::Local => {
					let mut resolved = local;
					<Issue as Sink<Remote>>::sink(&mut resolved, Some(&remote)).await?;
					Ok((resolved, true))
				}
				Side::Remote => {
					let mut resolved = remote;
					<Issue as Sink<Submitted>>::sink(&mut resolved, None).await?;
					Ok((resolved, true))
				}
			};
		}

		let force = matches!(mode, MergeMode::Force { .. });

		// Apply four-merge algorithm
		let mut local_merged = local.clone();
		let mut remote_merged = remote.clone();

		eprintln!("[sync_issue] Before merge:");
		eprintln!("  local lineage: {:?}", local.identity.ancestry.lineage());
		eprintln!("  remote lineage: {:?}", remote.identity.ancestry.lineage());
		for (i, c) in local.children.iter().enumerate() {
			eprintln!("  local child[{i}] lineage: {:?}", c.identity.ancestry.lineage());
		}
		for (i, c) in remote.children.iter().enumerate() {
			eprintln!("  remote child[{i}] lineage: {:?}", c.identity.ancestry.lineage());
		}

		if let Some(ref consensus) = consensus {
			let _ = local_merged.merge(consensus.clone(), false);
		}
		local_merged.merge(remote.clone(), force)?;

		eprintln!("[sync_issue] After merge:");
		eprintln!("  local_merged lineage: {:?}", local_merged.identity.ancestry.lineage());
		for (i, c) in local_merged.children.iter().enumerate() {
			eprintln!("  local_merged child[{i}] lineage: {:?}", c.identity.ancestry.lineage());
		}

		if let Some(consensus) = consensus {
			let _ = remote_merged.merge(consensus, false);
		}
		remote_merged.merge(local, force)?;

		// Compare results
		if local_merged == remote_merged {
			// Auto-resolved - sink to both sides
			let mut resolved = local_merged;
			<Issue as Sink<Submitted>>::sink(&mut resolved, None).await?;
			<Issue as Sink<Remote>>::sink(&mut resolved, None).await?;
			Ok((resolved, true))
		} else {
			// Conflict - initiate git merge
			match initiate_conflict_merge(owner, repo, issue_number, &local_merged, &remote_merged)? {
				ConflictOutcome::AutoMerged => {
					let resolved = read_resolved_conflict(owner)?;
					complete_conflict_resolution(owner)?;
					let mut resolved = resolved;
					<Issue as Sink<Submitted>>::sink(&mut resolved, None).await?;
					<Issue as Sink<Remote>>::sink(&mut resolved, None).await?;
					Ok((resolved, true))
				}
				ConflictOutcome::NeedsResolution => {
					bail!(
						"Conflict detected for {owner}/{repo}#{issue_number}.\n\
						Resolve using standard git tools, then re-run."
					);
				}
				ConflictOutcome::NoChanges => {
					// Git says no changes - take local
					Ok((local_merged, false))
				}
			}
		}
	}
}

mod types {
	use super::*;
	/// Which side to prefer in merge operations.
	#[derive(Clone, Copy, Debug, Eq, PartialEq)]
	pub enum Side {
		Local,
		Remote,
	}

	/// How to merge local and remote states.
	#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
	pub enum MergeMode {
		/// Timestamp-based resolution. Conflict if can't auto-resolve.
		#[default]
		Normal,
		/// Force preferred side on conflicts, keep non-conflicting from both.
		Force { prefer: Side },
		/// Take preferred side entirely.
		Reset { prefer: Side },
	}

	/// Options for controlling sync behavior.
	///
	/// The `merge_mode` is consumed after first use (pre-editor sync),
	/// so post-editor sync runs with `MergeMode::Normal`.
	#[derive(Debug, Default)]
	pub struct SyncOptions {
		merge_mode: std::cell::Cell<Option<MergeMode>>,
		/// Fetch and sync from remote before opening editor.
		pub pull: bool,
	}

	impl SyncOptions {
		pub fn new(merge_mode: Option<MergeMode>, pull: bool) -> Self {
			Self {
				merge_mode: std::cell::Cell::new(merge_mode),
				pull,
			}
		}

		/// Take the merge mode, returning Normal if already taken or not set.
		pub fn take_merge_mode(&self) -> MergeMode {
			self.merge_mode.take().unwrap_or_default()
		}

		/// Peek at the merge mode without consuming it.
		pub fn peek_merge_mode(&self) -> MergeMode {
			self.merge_mode.get().unwrap_or_default()
		}
	}

	/// Result of applying a modifier to an issue.
	pub struct ModifyResult {
		pub output: Option<String>,
		pub file_modified: bool,
	}

	/// A modifier that can be applied to an issue file.
	#[derive(Debug)]
	pub enum Modifier {
		Editor { open_at_blocker: bool },
		BlockerPop,
		BlockerAdd { text: String },
	}

	impl Modifier {
		#[tracing::instrument]
		pub(super) async fn apply(&self, issue: &mut Issue, issue_file_path: &Path) -> Result<ModifyResult> {
			let old_issue = issue.clone();

			let result = match self {
				Modifier::Editor { open_at_blocker } => {
					let content = issue.serialize_virtual();
					std::fs::write(issue_file_path, &content)?;

					let mtime_before = std::fs::metadata(issue_file_path)?.modified()?;

					let position = if *open_at_blocker {
						issue.find_last_blocker_position().map(|(line, col)| crate::utils::Position::new(line, Some(col)))
					} else {
						None
					};

					crate::utils::open_file(issue_file_path, position).await?;

					let mtime_after = std::fs::metadata(issue_file_path)?.modified()?;
					let file_modified = mtime_after != mtime_before;

					eprintln!("[Modifier::Editor] reading from: {:?}", issue_file_path);
					let content = std::fs::read_to_string(issue_file_path)?;
					eprintln!("[Modifier::Editor] content read:\n{content}");
					issue.update_from_virtual(&content, issue_file_path)?;

					eprintln!("[after update_from_virtual] issue state: {:?}", issue.contents.state);
					for (i, c) in issue.children.iter().enumerate() {
						eprintln!("[after update_from_virtual] child[{i}] state: {:?}", c.contents.state);
					}

					ModifyResult { output: None, file_modified }
				}
				Modifier::BlockerPop => {
					use crate::blocker_interactions::BlockerSequenceExt;
					let popped = issue.contents.blockers.pop();
					ModifyResult {
						output: popped.map(|text| format!("Popped: {text}")),
						file_modified: true,
					}
				}
				Modifier::BlockerAdd { text } => {
					use crate::blocker_interactions::BlockerSequenceExt;
					issue.contents.blockers.add(text);
					ModifyResult { output: None, file_modified: true }
				}
			};

			if result.file_modified {
				issue.update_timestamps_from_diff(&old_issue);
			}

			Ok(result)
		}
	}
}
pub use types::*;
