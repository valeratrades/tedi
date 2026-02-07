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
	HollowIssue, Issue, IssueIndex, IssueLink, IssueSelector, LazyIssue, RepoInfo, VirtualIssue,
	local::{
		Consensus, FsReader, Local, LocalFs, LocalIssueSource, LocalPath,
		conflict::{ConflictOutcome, initiate_conflict_merge},
		consensus::load_consensus_issue,
	},
	remote::{Remote, RemoteSource},
	sink::Sink,
};
use tracing::instrument;
use v_utils::elog;

use super::merge::Merge;

/// Modify a local issue, then sync changes back to Github.
///
/// Caller is responsible for loading the issue (via `Issue::load(LocalIssueSource)`).
#[instrument(skip_all, fields(
	repo = ?issue.identity.repo_info(),
	issue_id = ?issue.git_id(),
	title = %issue.contents.title,
	offline,
	modifier = ?modifier,
))]
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
		// Record this issue as the last modified
		let cache_path = v_utils::xdg_cache_file!("last_modified_issue");
		std::fs::write(&cache_path, issue.full_index().to_string()).ok();
		result
	};

	match offline || Local::is_virtual_project(repo_info) {
		true => {
			<Issue as Sink<LocalFs>>::sink(&mut issue, None).await?;
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
						<Issue as Sink<LocalFs>>::sink(&mut issue, None).await?;

						// 2. Load ancestor up to first Title selector
						let ancestor_index = IssueIndex::with_index(repo_info, parent_index.index()[..=i].to_vec());
						let ancestor_source = LocalIssueSource::<FsReader>::build(LocalPath::new(ancestor_index)).await?;
						let mut ancestor = Issue::load(ancestor_source).await?;
						let old_ancestor = ancestor.clone();

						// 3. Sink ancestor to Remote, then Local (with old state for cleanup)
						<Issue as Sink<Remote>>::sink(&mut ancestor, None).await?;
						<Issue as Sink<LocalFs>>::sink(&mut ancestor, Some(&old_ancestor)).await?;

						// 4. Commit
						<Issue as Sink<Consensus>>::sink(&mut ancestor, None).await?;
					} else {
						<Issue as Sink<Remote>>::sink(&mut issue, None).await?;
						<Issue as Sink<LocalFs>>::sink(&mut issue, None).await?;
						<Issue as Sink<Consensus>>::sink(&mut issue, None).await?;
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
	#[instrument(skip_all, fields(?mode))]
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
					<Issue as Sink<LocalFs>>::sink(&mut resolved, None).await?;
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
			local_merged.merge(consensus, false)?;
		}
		local_merged.merge(&remote, force_remote_wins)?;

		if let Some(consensus) = consensus {
			remote_merged.merge(&consensus, false)?;
		}
		remote_merged.merge(&local, force_local_wins)?;

		// Compare results
		match local_merged == remote_merged {
			true => {
				// Auto-resolved - sink to both sides
				let mut resolved = local_merged;
				<Issue as Sink<LocalFs>>::sink(&mut resolved, None).await?;
				<Issue as Sink<Remote>>::sink(&mut resolved, None).await?;
				Ok((resolved, true))
			}
			false => {
				// Conflict - initiate git merge
				match initiate_conflict_merge(repo_info, issue_number, &local_merged, &remote_merged)? {
					ConflictOutcome::AutoMerged => {
						unreachable!(
							"AutoMerged means when we triggered a merge of local against remote (which we've already checked are divergent), it succeeded. Which would be an implementation error, - whole point of the call is to record the conflict before getting user to resolve it manually."
						);
					}
					ConflictOutcome::NeedsResolution => {
						//TODO!: switch to a preview Error return type. This branch is EXPECTED to be reached during real-world usage, and must have first-class formatting and a miette error nicely propagated to user.
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

	#[instrument(skip_all, fields(?mode, has_consensus = consensus.is_some()))]
	pub(super) async fn sync(current_issue: &mut Issue, consensus: Option<Issue>, mode: MergeMode) -> Result<()> {
		println!("Syncing...");
		let issue_number = current_issue.git_id().expect(
			"can't be linked and not have number associated\nunless we die in a weird moment I guess. If this ever triggers, should fix it to set issue as pending (not linked) and sink",
		);
		let repo_info = current_issue.repo_info();

		let url = format!("https://github.com/{}/{}/issues/{issue_number}", repo_info.owner(), repo_info.repo());
		let link = IssueLink::parse(&url).expect("valid URL");
		let remote_source = RemoteSource::build(link, Some(&current_issue.identity.git_lineage()?))?; //DEPENDS: git_lineage() will error if any parent is not synced. //Q: should I move the logic for traversing IssueIndex in search of pending parents right here?
		let remote = Issue::load(remote_source).await?;

		let (resolved, changed) = core::resolve_merge(current_issue.clone(), consensus, remote, mode, repo_info, issue_number).await?;
		*current_issue = resolved;

		match changed {
			true => {
				// Re-sink local in case issue numbers changed
				<Issue as Sink<LocalFs>>::sink(current_issue, None).await?;
				<Issue as Sink<Consensus>>::sink(current_issue, None).await?;
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
		#[tracing::instrument(skip_all)]
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

					// `!u` on the last line means "undo": treat as if no changes were made
					let (content, file_modified) = {
						let trimmed = content.trim_end();
						let undo = trimmed.strip_suffix("!u").or_else(|| trimmed.strip_suffix("!U"));
						match undo {
							Some(before) if before.is_empty() || before.ends_with('\n') => (before.trim_end_matches('\n').to_string(), false),
							_ => (content, file_modified),
						}
					};

					tracing::Span::current().record("vpath", tracing::field::debug(&vpath));
					tracing::Span::current().record("content", content.as_str());
					let parent_idx = issue.identity.parent_index;
					let is_virtual = issue.identity.is_virtual;
					let hollow: HollowIssue = old_issue.clone().into();
					let virtual_issue = VirtualIssue::parse(&content, vpath.clone())?;
					*issue = Issue::from_combined(hollow, virtual_issue, parent_idx, is_virtual)?;

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
				issue.post_update(&old_issue);
			}

			Ok(result)
		}
	}
}
pub use types::*;
