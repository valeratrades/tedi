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
use tracing::instrument;
pub use types::*;
use v_utils::elog;

use super::merge::Merge;
use crate::{
	Issue, IssueIndex, IssueLink, IssueSelector, LazyIssue, RepoInfo, VirtualIssue,
	local::{
		Consensus, FsReader, Local, LocalFs, LocalIssueSource, LocalPath,
		conflict::{ConflictOutcome, conflict_file_path, initiate_conflict_merge},
		consensus::load_consensus_issue,
	},
	remote::{Remote, RemoteSource},
	sink::Sink,
};

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
	// Virtual issues carry a fabricated link — never fetch it.
	if !offline && issue.is_linked() && !issue.identity.is_virtual {
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
		let cache_path = crate::paths::cache_file("last_modified_issue");
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
					if let Some((i, _)) = parent_index
						.index()
						.iter()
						.enumerate()
						.find(|(_, s)| matches!(s, IssueSelector::Title(_) | IssueSelector::Exact(_)))
					{
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
#[derive(Debug, miette::Diagnostic, thiserror::Error)]
#[error(transparent)]
#[diagnostic(help("Your changes were saved to /tmp/tedi/rejected-changes.md — you can recover them from there."))]
struct RejectedEdit(#[from] crate::ParseError);

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
				<Issue as Sink<Remote>>::sink(&mut resolved, Some(&remote)).await?;
				Ok((resolved, true))
			}
			false => {
				// Conflict - initiate git merge
				match initiate_conflict_merge(
					repo_info,
					issue_number,
					&local_merged.to_string(),
					&remote_merged.to_string(),
					conflict_file_path(repo_info.owner()),
				)? {
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

	/// Fold an edited parent buffer back into the in-memory issue.
	///
	/// The buffer carries this node's own contents plus its children as one-line links (shallow —
	/// state/title/labels only). The children's real subtrees stay as loaded; we apply link-level
	/// changes, drop links the user removed, and add newly-typed pending children.
	fn apply_edited_buffer(issue: &mut Issue, old: &Issue, edited: VirtualIssue) {
		use std::collections::HashMap;

		issue.contents = edited.contents;
		let child_parent_idx = IssueIndex::from(&*issue);

		let mut children = HashMap::new();
		for (selector, edited_child) in edited.children {
			let child = match old.children.get(&selector) {
				Some(existing) => {
					let mut child = existing.clone();
					child.contents.title = edited_child.contents.title;
					child.contents.labels = edited_child.contents.labels;
					child.contents.state = edited_child.contents.state;
					child
				}
				None => Issue {
					identity: if issue.identity.is_virtual {
						crate::IssueIdentity::virtual_issue(child_parent_idx)
					} else {
						crate::IssueIdentity::pending(child_parent_idx)
					},
					contents: edited_child.contents,
					children: HashMap::new(),
				},
			};
			children.insert(selector, child);
		}
		issue.children = children;
	}

	/// A modifier that can be applied to an issue file.
	#[derive(Debug)]
	pub enum Modifier {
		Editor {
			open_at_blocker: bool,
		},
		BlockerPop {
			/// Number of parent ancestors to pop in addition to the current leaf.
			parents: usize,
		},
		BlockerAdd {
			text: String,
			nest: bool,
		},
		/// Replace the current (deepest) blocker's text in-place, preserving its position in the tree.
		BlockerSet {
			text: String,
		},
		/// Replace the issue's entire blocker sequence.
		/// Used by milestone editing to sync blocker changes back to individual issues.
		BlockerWrite {
			blockers: crate::Blockers,
		},
		/// Mock modifier that does nothing but reports file as modified. For testing.
		MockGhostEdit,
	}

	impl Modifier {
		#[tracing::instrument(skip_all)]
		pub(super) async fn apply(&self, issue: &mut Issue) -> Result<ModifyResult> {
			let old_issue = issue.clone();

			let result = match self {
				Modifier::Editor { open_at_blocker } => {
					// Open the issue's real source file and edit it in place; children show as links.
					let title = issue.contents.title.clone();
					let closed = issue.contents.state.is_closed();
					let has_children = !issue.children.is_empty();
					let path = LocalPath::from(&*issue).resolve_parent(FsReader)?.deterministic(&title, closed, has_children).path();
					let pre_existed = path.exists();
					std::fs::create_dir_all(path.parent().expect("issue file path always has a parent"))?;
					std::fs::write(&path, issue.to_string())?;

					let mtime_before = std::fs::metadata(&path)?.modified()?;

					let position = open_at_blocker.then(|| {
						let (line, col) = issue.editor_position();
						crate::utils::Position::new(line, Some(col))
					});

					crate::utils::open_file(&path, position).await?;

					let mtime_after = std::fs::metadata(&path)?.modified()?;
					let file_modified = mtime_after != mtime_before;

					let content = std::fs::read_to_string(&path)?;

					// `!u` on its own last line means "undo": treat as if no changes were made
					let trimmed = content.trim_end();
					let undo = matches!(
						trimmed.strip_suffix("!u").or_else(|| trimmed.strip_suffix("!U")),
						Some(before) if before.is_empty() || before.ends_with('\n')
					);

					if undo || !file_modified {
						// No effective change. If the file existed before, restore it to mirror the issue
						// (an undo leaves raw editor text on disk). If we created it just to open the editor
						// for a brand-new issue, remove it — an aborted touch must not create anything.
						if pre_existed {
							std::fs::write(&path, issue.to_string())?;
						} else {
							std::fs::remove_file(&path)?;
							if let Some(parent) = path.parent() {
								let _ = std::fs::remove_dir(parent); // only succeeds if we left it empty
							}
						}
						ModifyResult { output: None, file_modified: false }
					} else {
						let edited = VirtualIssue::parse(&content, path.clone()).map_err(|e| {
							crate::utils::persist_rejected_changes(&content);
							RejectedEdit(e)
						})?;
						apply_edited_buffer(issue, &old_issue, edited);
						ModifyResult { output: None, file_modified: true }
					}
				}
				Modifier::BlockerPop { parents } => {
					let popped = issue
						.contents
						.blockers
						.pop(*parents)
						.ok_or_else(|| color_eyre::eyre::eyre!("Cannot pop {parents} parents — blocker chain is shorter"))?;
					ModifyResult {
						output: Some(format!("Popped: {popped}")),
						file_modified: true,
					}
				}
				Modifier::BlockerAdd { text, nest } => {
					if *nest {
						issue.contents.blockers.add_child(text);
					} else {
						issue.contents.blockers.add(text);
					}
					ModifyResult { output: None, file_modified: true }
				}
				Modifier::BlockerSet { text } => {
					let old = issue.contents.blockers.set(text);
					ModifyResult {
						output: old.map(|prev| format!("Replaced: {prev} -> {text}")),
						file_modified: true,
					}
				}
				Modifier::BlockerWrite { blockers } => {
					let file_modified = issue.contents.blockers != *blockers;
					issue.contents.blockers = blockers.clone();
					ModifyResult { output: None, file_modified }
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
