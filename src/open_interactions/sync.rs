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

use color_eyre::eyre::{Result, bail};
use tedi::{
	Issue, IssueIndex, IssueLink, IssueSelector, LazyIssue, RepoInfo,
	local::{FsReader, Local, LocalIssueSource, Submitted},
	sink::Sink,
};
use tracing::instrument;
use v_utils::elog;

use super::{
	conflict::{ConflictOutcome, complete_conflict_resolution, initiate_conflict_merge, read_resolved_conflict},
	consensus::{commit_issue_changes, load_consensus_issue},
	merge::Merge,
	remote::{Remote, RemoteSource},
};

/// Modify a local issue, then sync changes back to Github.
///
/// Caller is responsible for loading the issue (via `LazyIssue<Local>::load`).
#[instrument]
pub async fn modify_and_sync_issue(mut issue: Issue, offline: bool, modifier: Modifier, sync_opts: SyncOptions) -> Result<ModifyResult> {
	let repo_info = issue.identity.repo_info();
	let issue_index = IssueIndex::from(&issue);

	// if linked, check if local diverges from consensus. If yes, - need to sync the two. And while at it, let's pull remote too.
	if !offline && issue.is_linked() {
		let consensus = load_consensus_issue(issue_index).await?;
		let local_differs = consensus.as_ref().map(|c| *c != issue).unwrap_or(false); //IGNORED_ERROR: if consensus doesn't exist, then local doesn't need to think about it

		if sync_opts.pull || local_differs {
			elog!("triggered pre-open sync");
			core::sync(&mut issue, consensus, sync_opts.take_merge_mode()).await?;
		}
	}

	// expose for modification (by user or procedural)
	let new_modified = {
		let result = modifier.apply(&mut issue).await?;
		if !result.file_modified {
			v_utils::log!("Aborted (no changes made)");
			return Ok(result);
		}
		result
	};

	match offline || Local::is_virtual_project(repo_info) {
		true => {
			<Issue as Sink<Submitted>>::sink(&mut issue, None).await?;
			println!("Offline: saved locally and exiting.");
			return Ok(new_modified);
		}
		false => {
			// Post-editor sync
			let mode = sync_opts.take_merge_mode();
			match issue.is_linked() {
				true => {
					let consensus = load_consensus_issue(issue_index).await?;
					core::sync(&mut issue, consensus, mode).await?;
				}
				false => {
					// New issue - check if parent needs syncing first
					let parent_index = issue.identity.parent_index;
					if let Some((i, _)) = parent_index.index().iter().enumerate().find(|(_, s)| matches!(s, IssueSelector::Title(_))) {
						// 1. Sink current issue to local so ancestor can find it
						<Issue as Sink<Submitted>>::sink(&mut issue, None).await?;

						// 2. Load ancestor up to first Title selector
						let ancestor_index = IssueIndex::with_index(repo_info, parent_index.index()[..=i].to_vec());
						let mut ancestor = <Issue as LazyIssue<Local>>::load(LocalIssueSource::<FsReader>::from(ancestor_index)).await?;
						let old_ancestor = ancestor.clone();

						// 3. Sink ancestor to Remote, then Local (with old state for cleanup)
						<Issue as Sink<Remote>>::sink(&mut ancestor, None).await?;
						<Issue as Sink<Submitted>>::sink(&mut ancestor, Some(&old_ancestor)).await?;

						// 4. Commit
						commit_issue_changes(&ancestor)?;
					} else {
						<Issue as Sink<Remote>>::sink(&mut issue, None).await?;
						<Issue as Sink<Submitted>>::sink(&mut issue, None).await?;
						commit_issue_changes(&issue)?;
					}
				}
			}

			Ok(new_modified)
		}
	}
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
	#[instrument]
	pub(super) async fn resolve_merge(local: Issue, consensus: Option<Issue>, remote: Issue, mode: MergeMode, repo_info: RepoInfo, issue_number: u64) -> Result<(Issue, bool)> {
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

		// In Force mode, we pass force=true only to the merge where `other` is the preferred side.
		// merge(other, true) means	 "take other's values on conflicts".
		// So: prefer Local → force on remote_merged.merge(local)
		//     prefer Remote → force on local_merged.merge(remote)
		let (force_local_wins, force_remote_wins) = match mode {
			MergeMode::Force { prefer: Side::Local } => (true, false),
			MergeMode::Force { prefer: Side::Remote } => (false, true),
			_ => (false, false),
		};

		// Apply four-merge algorithm
		let mut local_merged = local.clone();
		let mut remote_merged = remote.clone();

		if let Some(ref consensus) = consensus {
			local_merged.merge(consensus.clone(), false)?;
		}
		local_merged.merge(remote.clone(), force_remote_wins)?;

		if let Some(consensus) = consensus {
			remote_merged.merge(consensus, false)?;
		}
		remote_merged.merge(local, force_local_wins)?;

		// Compare results
		match local_merged == remote_merged {
			true => {
				// Auto-resolved - sink to both sides
				let mut resolved = local_merged;
				<Issue as Sink<Submitted>>::sink(&mut resolved, None).await?;
				<Issue as Sink<Remote>>::sink(&mut resolved, None).await?;
				Ok((resolved, true))
			}
			false => {
				// Conflict - initiate git merge
				match initiate_conflict_merge(repo_info, issue_number, &local_merged, &remote_merged)? {
					ConflictOutcome::AutoMerged => {
						let resolved = read_resolved_conflict(repo_info.owner())?;
						complete_conflict_resolution(repo_info.owner())?;
						let mut resolved = resolved;
						<Issue as Sink<Submitted>>::sink(&mut resolved, None).await?;
						<Issue as Sink<Remote>>::sink(&mut resolved, None).await?;
						Ok((resolved, true))
					}
					ConflictOutcome::NeedsResolution => {
						bail!(
							"Conflict detected for {}/{}#{issue_number}.\n\
							Resolve using standard git tools, then re-run.",
							repo_info.owner(),
							repo_info.repo()
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

	pub(super) async fn sync(current_issue: &mut Issue, consensus: Option<Issue>, mode: MergeMode) -> Result<()> {
		eprintln!("DEBUG: core::sync entered");
		println!("Syncing...");
		let issue_number = current_issue.git_id().expect(
			"can't be linked and not have number associated\nunless we die in a weird moment I guess. If this ever triggers, should fix it to set issue as pending (not linked) and sink",
		);
		let repo_info = current_issue.repo_info();

		let url = format!("https://github.com/{}/{}/issues/{issue_number}", repo_info.owner(), repo_info.repo());
		let link = IssueLink::parse(&url).expect("valid URL");
		let remote_source = RemoteSource::with_lineage(link, &current_issue.identity.git_lineage()?);
		eprintln!("DEBUG: about to load Remote");
		let remote = <Issue as LazyIssue<Remote>>::load(remote_source).await?;
		eprintln!("DEBUG: Remote loaded, calling resolve_merge");

		let (resolved, changed) = core::resolve_merge(current_issue.clone(), consensus, remote, mode, repo_info, issue_number).await?;
		eprintln!("DEBUG: resolve_merge done");
		*current_issue = resolved;

		match changed {
			true => {
				// Re-sink local in case issue numbers changed
				<Issue as Sink<Submitted>>::sink(current_issue, None).await?;
				commit_issue_changes(current_issue)?;
			}
			false => println!("No changes."),
		}
		Ok(())
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
	}

	/// Result of applying a modifier to an issue.
	pub struct ModifyResult {
		pub output: Option<String>,
		pub file_modified: bool,
	}

	/// A modifier that can be applied to an issue file.
	#[derive(Debug)]
	pub enum Modifier {
		Editor {
			open_at_blocker: bool,
		},
		BlockerPop,
		BlockerAdd {
			text: String,
		},
		/// Mock modifier that does nothing but reports file as modified. For testing.
		MockGhostEdit,
	}

	impl Modifier {
		#[tracing::instrument]
		pub(super) async fn apply(&self, issue: &mut Issue) -> Result<ModifyResult> {
			let old_issue = issue.clone();
			let vpath = Local::virtual_edit_path(issue);

			let result = match self {
				Modifier::Editor { open_at_blocker } => {
					let content = issue.serialize_virtual();
					std::fs::write(&vpath, &content)?;

					let mtime_before = std::fs::metadata(&vpath)?.modified()?;

					let position = if *open_at_blocker {
						issue.find_last_blocker_position().map(|(line, col)| crate::utils::Position::new(line, Some(col)))
					} else {
						None
					};

					crate::utils::open_file(&vpath, position).await?;

					let mtime_after = std::fs::metadata(&vpath)?.modified()?;
					let file_modified = mtime_after != mtime_before;

					let content = std::fs::read_to_string(&vpath)?;
					tracing::Span::current().record("vpath", tracing::field::debug(&vpath));
					tracing::Span::current().record("content", content.as_str());
					issue.update_from_virtual(&content)?;

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
				Modifier::MockGhostEdit => ModifyResult { output: None, file_modified: true },
			};

			if result.file_modified {
				issue.update_timestamps_from_diff(&old_issue);
			}

			Ok(result)
		}
	}
}
pub use types::*;
