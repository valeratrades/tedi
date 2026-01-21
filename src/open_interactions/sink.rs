//! Sink trait for pushing issues to sources (GitHub, filesystem).
//!
//! This module provides a unified interface for syncing issues to different
//! destinations. The key insight is that we only want to touch things that
//! have actually changed since the last sync.
//!
//! ## Design Overview
//!
//! The `Sink<S>` trait uses marker types (`Local`, `Remote`) to select the
//! implementation. Location is derived from the issue's `identity.ancestry`.
//!
//! The trait is called on the issue we want to push.
//!
//! ## Ordering Constraints
//!
//! 1. **Pending issues are created** before syncing their content
//! 2. **Pending comments must be created sequentially** (one by one) to
//!    preserve creation order on GitHub
//! 3. **Children are processed recursively** after the parent is synced

use std::collections::{HashMap, HashSet};

use tedi::{Comment, CommentIdentity, Issue, IssueIdentity, IssueLink};

//==============================================================================
// Diff Results
//==============================================================================

/// Result of comparing an issue node with its old state.
#[derive(Clone, Debug, Default)]
pub struct IssueDiff {
	/// Issue body changed (first comment)
	pub body_changed: bool,
	/// Issue state (open/closed) changed
	pub state_changed: bool,
	/// Issue title changed
	pub title_changed: bool,
	/// Issue labels changed
	pub labels_changed: bool,
	/// Comments to create (pending comments that don't exist in old)
	pub comments_to_create: Vec<Comment>,
	/// Comments to update (existing comments with changed body)
	pub comments_to_update: Vec<(u64, Comment)>,
	/// Comment IDs to delete (exist in old but not in new)
	pub comments_to_delete: Vec<u64>,
	/// Sub-issues to create (pending sub-issues)
	pub children_to_create: Vec<Issue>,
	/// Sub-issue numbers to delete (exist in old but not in new)
	pub children_to_delete: Vec<u64>,
}

impl IssueDiff {
	/// Returns true if there are any changes to sync.
	#[cfg(test)]
	pub fn has_changes(&self) -> bool {
		self.body_changed
			|| self.state_changed
			|| self.title_changed
			|| self.labels_changed
			|| !self.comments_to_create.is_empty()
			|| !self.comments_to_update.is_empty()
			|| !self.comments_to_delete.is_empty()
			|| !self.children_to_create.is_empty()
			|| !self.children_to_delete.is_empty()
	}
}

//==============================================================================
// Diff Computation
//==============================================================================

/// Compute the diff between `new` (consensus we're pushing) and `old` (current state of target).
///
/// This identifies what needs to be synced:
/// - Changed content (body, state, title, labels)
/// - Comments to create/update/delete
/// - Children to create/delete
pub fn compute_node_diff(new: &Issue, old: Option<&Issue>) -> IssueDiff {
	let mut diff = IssueDiff::default();

	let Some(old) = old else {
		// No old state - everything is new (but issue itself is handled separately)
		// Collect pending comments and children
		for comment in new.contents.comments.iter().skip(1) {
			if comment.identity.is_pending() {
				diff.comments_to_create.push(comment.clone());
			}
		}
		for child in &new.children {
			if child.is_local() {
				diff.children_to_create.push(child.clone());
			}
		}
		return diff;
	};

	// Compare body (first comment)
	let new_body = new.contents.comments.first().map(|c| c.body.render()).unwrap_or_default();
	let old_body = old.contents.comments.first().map(|c| c.body.render()).unwrap_or_default();
	diff.body_changed = new_body != old_body;

	// Compare state
	diff.state_changed = new.contents.state != old.contents.state;

	// Compare title
	diff.title_changed = new.contents.title != old.contents.title;

	// Compare labels
	diff.labels_changed = new.contents.labels != old.contents.labels;

	// Compare comments (skip first which is body)
	let old_comments: HashMap<u64, &Comment> = old.contents.comments.iter().skip(1).filter_map(|c| c.identity.id().map(|id| (id, c))).collect();
	let new_comment_ids: HashSet<u64> = new.contents.comments.iter().skip(1).filter_map(|c| c.identity.id()).collect();

	for comment in new.contents.comments.iter().skip(1) {
		match &comment.identity {
			CommentIdentity::Pending | CommentIdentity::Body => {
				// New pending comment to create
				if !comment.body.is_empty() {
					diff.comments_to_create.push(comment.clone());
				}
			}
			CommentIdentity::Created { id, .. } => {
				if let Some(old_comment) = old_comments.get(id) {
					// Existing comment - check if body changed
					if comment.body.render() != old_comment.body.render() {
						diff.comments_to_update.push((*id, comment.clone()));
					}
				}
				// Note: if comment exists in new but not in old, it shouldn't happen
				// (would mean we have an ID for something that doesn't exist)
			}
		}
	}

	// Find comments to delete (in old but not in new)
	for id in old_comments.keys() {
		if !new_comment_ids.contains(id) {
			diff.comments_to_delete.push(*id);
		}
	}

	// Compare children
	let old_children: HashMap<u64, &Issue> = old.children.iter().filter_map(|c| c.number().map(|n| (n, c))).collect();
	let new_child_numbers: HashSet<u64> = new.children.iter().filter_map(|c| c.number()).collect();

	for child in &new.children {
		if child.is_local() {
			diff.children_to_create.push(child.clone());
		}
	}

	// Find children to delete (in old but not in new)
	for num in old_children.keys() {
		if !new_child_numbers.contains(num) {
			diff.children_to_delete.push(*num);
		}
	}

	diff
}

//==============================================================================
// Sink Trait
//==============================================================================

pub use super::remote::Remote;

/// Trait for sinking (pushing) issues to a destination.
///
/// Implemented for `Issue` with marker type parameters to select the destination.
/// Location is derived from the issue's `identity.ancestry` (owner/repo/lineage).
/// For remote operations, the GitHub client is accessed via `github::client::get()`.
#[allow(async_fn_in_trait)]
pub trait Sink<S> {
	/// Error type for this sink implementation.
	type Error: std::fmt::Debug + std::fmt::Display;

	/// Sink this issue to the destination, comparing against `old` state.
	///
	/// # Arguments
	/// * `old` - The current state at the target location (from last pull), or None if no previous state exists
	///
	/// # Returns
	/// * `Ok(true)` if any changes were made
	/// * `Ok(false)` if already in sync
	/// * `Err(_)` on failure
	async fn sink(&mut self, old: Option<&Issue>) -> Result<bool, Self::Error>;
}

//==============================================================================
// GitHub Sink Implementation
//==============================================================================

//TODO: @claude: create proper error type for Remote sink (see ConsensusSinkError for reference)
impl Sink<Remote> for Issue {
	type Error = color_eyre::Report;

	async fn sink(&mut self, old: Option<&Issue>) -> Result<bool, Self::Error> {
		// Virtual issues never sync to remote - they're local-only
		if self.identity.is_virtual() {
			return Ok(false);
		}

		let gh = crate::github::client::get();
		// Copy ancestry upfront to avoid borrow conflicts when updating identity
		let repo_info = self.identity.ancestry.repo_info();
		let mut changed = false;

		// If this is a pending (local) issue, create it first
		if self.is_local() {
			let title = &self.contents.title;
			let body = self.body();
			let closed = self.contents.state.is_closed();
			let ancestry = self.identity.ancestry;

			println!("Creating issue: {title}");
			let created = gh.create_issue(repo_info, title, &body).await?;
			println!("Created issue #{}: {}", created.number, created.html_url);

			// Close if needed
			if closed {
				gh.update_issue_state(repo_info, created.number, "closed").await?;
			}

			// Link to parent if this issue has one
			let lineage = ancestry.lineage();
			if let Some(&parent_number) = lineage.last() {
				gh.add_sub_issue(repo_info, parent_number, created.id).await?;
			}

			// Update identity - keep same ancestry, just add linking info
			let url = format!("https://github.com/{}/{}/issues/{}", repo_info.owner(), repo_info.repo(), created.number);
			let link = IssueLink::parse(&url).expect("just constructed valid URL");
			let user = gh.fetch_authenticated_user().await?;
			self.identity = IssueIdentity::linked(ancestry, user, link, tedi::IssueTimestamps::default());
			changed = true;
		}

		let issue_number = self.number().expect("issue must have number after creation");

		// Sync content against old (if we have old state)
		let diff = compute_node_diff(self, old);

		if diff.body_changed {
			let body = self.body();
			println!("Updating issue #{issue_number} body...");
			gh.update_issue_body(repo_info, issue_number, &body).await?;
			changed = true;
		}

		if diff.state_changed {
			let state = self.contents.state.to_github_state();
			println!("Updating issue #{issue_number} state to {state}...");
			gh.update_issue_state(repo_info, issue_number, state).await?;
			changed = true;
		}

		// Create pending comments sequentially (order matters)
		for comment in self.contents.comments.iter_mut().skip(1) {
			if comment.identity.is_pending() && !comment.body.is_empty() {
				let body_str = comment.body.render();
				println!("Creating new comment on issue #{issue_number}...");
				gh.create_comment(repo_info, issue_number, &body_str).await?;
				changed = true;
			}
		}

		// Update existing comments
		for (comment_id, comment) in &diff.comments_to_update {
			if let CommentIdentity::Created { user, .. } = &comment.identity
				&& !tedi::current_user::is(user)
			{
				continue;
			}
			let body_str = comment.body.render();
			println!("Updating comment {comment_id}...");
			gh.update_comment(repo_info, *comment_id, &body_str).await?;
			changed = true;
		}

		// Delete removed comments
		for comment_id in &diff.comments_to_delete {
			println!("Deleting comment {comment_id} from issue #{issue_number}...");
			gh.delete_comment(repo_info, *comment_id).await?;
			changed = true;
		}

		// Recursively sink children
		// Match children by position when we have old, otherwise sink with None
		for (i, child) in self.children.iter_mut().enumerate() {
			let old_child = old.and_then(|o| o.children.get(i));
			changed |= Box::pin(<Issue as Sink<Remote>>::sink(child, old_child)).await?;
		}

		Ok(changed)
	}
}

#[cfg(test)]
mod tests {
	use tedi::{Ancestry, BlockerSequence, CloseState, IssueContents, IssueLink};

	use super::*;

	fn make_issue(title: &str, number: Option<u64>) -> Issue {
		let ancestry = Ancestry::root("o", "r");
		let identity = match number {
			Some(n) => {
				let link = IssueLink::parse(&format!("https://github.com/o/r/issues/{n}")).unwrap();
				IssueIdentity::linked(ancestry, "testuser".to_string(), link, tedi::IssueTimestamps::default())
			}
			None => IssueIdentity::local(ancestry),
		};

		Issue {
			identity,
			contents: IssueContents {
				title: title.to_string(),
				labels: vec![],
				state: CloseState::Open,
				comments: vec![Comment {
					identity: CommentIdentity::Body,
					body: tedi::Events::parse("body"),
				}],
				blockers: BlockerSequence::default(),
			},
			children: vec![],
		}
	}

	#[test]
	fn test_compute_node_diff_no_changes() {
		let issue = make_issue("Root", Some(1));
		let diff = compute_node_diff(&issue, Some(&issue));

		assert!(!diff.has_changes());
	}

	#[test]
	fn test_compute_node_diff_body_changed() {
		let old = make_issue("Root", Some(1));
		let mut new = make_issue("Root", Some(1));
		new.contents.comments[0].body = tedi::Events::parse("new body");

		let diff = compute_node_diff(&new, Some(&old));

		assert!(diff.body_changed);
		assert!(diff.has_changes());
	}

	#[test]
	fn test_compute_node_diff_state_changed() {
		let old = make_issue("Root", Some(1));
		let mut new = make_issue("Root", Some(1));
		new.contents.state = CloseState::Closed;

		let diff = compute_node_diff(&new, Some(&old));

		assert!(diff.state_changed);
		assert!(diff.has_changes());
	}

	#[test]
	fn test_compute_node_diff_pending_comment() {
		let old = make_issue("Root", Some(1));
		let mut new = make_issue("Root", Some(1));
		new.contents.comments.push(Comment {
			identity: CommentIdentity::Pending,
			body: tedi::Events::parse("new comment"),
		});

		let diff = compute_node_diff(&new, Some(&old));

		assert_eq!(diff.comments_to_create.len(), 1);
		assert!(diff.has_changes());
	}

	#[test]
	fn test_compute_node_diff_comment_deleted() {
		let mut old = make_issue("Root", Some(1));
		old.contents.comments.push(Comment {
			identity: CommentIdentity::Created { user: "user".to_string(), id: 123 },
			body: tedi::Events::parse("old comment"),
		});
		let new = make_issue("Root", Some(1));

		let diff = compute_node_diff(&new, Some(&old));

		assert_eq!(diff.comments_to_delete, vec![123]);
		assert!(diff.has_changes());
	}

	#[test]
	fn test_compute_node_diff_pending_child() {
		let old = make_issue("Root", Some(1));
		let mut new = make_issue("Root", Some(1));
		new.children.push(make_issue("New Child", None)); // Pending

		let diff = compute_node_diff(&new, Some(&old));

		assert_eq!(diff.children_to_create.len(), 1);
	}
}
