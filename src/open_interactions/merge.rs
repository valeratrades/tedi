//! Merge trait for combining Issue states.
//!
//! The merge operation combines two Issue states, using timestamps to determine
//! which fields to keep when both have values.
//!
//! ## Merge semantics
//!
//! For each field:
//! - If `other` has a newer timestamp → take `other`'s value
//! - If `other` has `Some` timestamp and `self` has `None` → take `other`'s value
//! - If both have `None` timestamps → keep `self`
//! - If timestamps are equal → keep `self` (caller can reverse order to prefer other)
//!
//! When `force=true`, always take `other`'s value regardless of timestamps.
//!
//! ## Special cases
//!
//! - Virtual issues cannot be merged (error)
//! - Pending issues have timestamps initialized to `default()` before merge

use jiff::Timestamp;
use tedi::Issue;
use thiserror::Error;

/// Error from merge operations.
#[derive(Debug, Error)]
pub enum MergeError {
	/// Cannot merge virtual-only issues.
	#[error("cannot merge virtual-only issue: virtual issues are local-only and should not participate in sync")]
	VirtualIssue,
}

/// Extension trait for merging Issues.
pub trait Merge {
	/// Merge `other` into `self`, using timestamps to resolve conflicts.
	///
	/// When `force=true`, always takes `other`'s values regardless of timestamps.
	/// When `force=false`, uses timestamp comparison to decide.
	///
	/// Returns error if either issue is virtual-only.
	fn merge(&mut self, other: Issue, force: bool) -> Result<(), MergeError>;
}

impl Merge for Issue {
	fn merge(&mut self, other: Issue, force: bool) -> Result<(), MergeError> {
		// Virtual issues cannot participate in merge
		if self.identity.is_virtual() || other.identity.is_virtual() {
			return Err(MergeError::VirtualIssue);
		}

		// Get timestamps, initializing pending issues to default
		let self_ts = self.identity.timestamps().cloned().unwrap_or_default();
		let other_ts = other.identity.timestamps().cloned().unwrap_or_default();

		// Helper closure: returns true if other dominates self for this field
		let dominated_by = |self_field_ts: Option<Timestamp>, other_field_ts: Option<Timestamp>| -> bool {
			if force {
				return true;
			}
			match (self_field_ts, other_field_ts) {
				(_, None) => false,          // other has no timestamp, keep self
				(None, Some(_)) => true,     // self has no timestamp, take other
				(Some(s), Some(o)) => o > s, // compare timestamps, take if other is newer
			}
		};

		// Merge title
		if dominated_by(self_ts.title, other_ts.title) {
			self.contents.title = other.contents.title.clone();
		}

		// Merge labels
		if dominated_by(self_ts.labels, other_ts.labels) {
			self.contents.labels = other.contents.labels.clone();
		}

		// Merge description (body + blockers)
		if dominated_by(self_ts.description, other_ts.description) {
			// Body is the first comment
			if let Some(other_body) = other.contents.comments.first() {
				if let Some(self_body) = self.contents.comments.first_mut() {
					self_body.body = other_body.body.clone();
				} else {
					self.contents.comments.insert(0, other_body.clone());
				}
			}
			self.contents.blockers = other.contents.blockers.clone();
		}

		// Merge state
		if dominated_by(self_ts.state, other_ts.state) {
			self.contents.state = other.contents.state.clone();
		}

		// Merge comments (excluding body which is first comment)
		// Compare using max timestamp from each side's comment timestamps
		let self_comments_ts = self_ts.comments.iter().max().copied();
		let other_comments_ts = other_ts.comments.iter().max().copied();
		if dominated_by(self_comments_ts, other_comments_ts) {
			// Replace all non-body comments with other's
			let self_body = self.contents.comments.first().cloned();
			self.contents.comments = other.contents.comments.clone();
			// Restore self's body if we didn't take other's description
			if !dominated_by(self_ts.description, other_ts.description)
				&& let Some(body) = self_body
			{
				if self.contents.comments.is_empty() {
					self.contents.comments.push(body);
				} else {
					self.contents.comments[0] = body;
				}
			}
		}

		// Update timestamps on self to reflect the merge
		if let Some(linked) = self.identity.mut_linked_issue_meta() {
			if dominated_by(self_ts.title, other_ts.title) {
				linked.timestamps.title = other_ts.title;
			}
			if dominated_by(self_ts.labels, other_ts.labels) {
				linked.timestamps.labels = other_ts.labels;
			}
			if dominated_by(self_ts.description, other_ts.description) {
				linked.timestamps.description = other_ts.description;
			}
			if dominated_by(self_ts.state, other_ts.state) {
				linked.timestamps.state = other_ts.state;
			}
			if dominated_by(self_comments_ts, other_comments_ts) {
				linked.timestamps.comments = other_ts.comments.clone();
			}
		}

		// Merge children
		merge_children(&mut self.children, other.children, force)?;

		Ok(())
	}
}

/// Merge children lists, matching by URL.
fn merge_children(self_children: &mut Vec<Issue>, other_children: Vec<Issue>, force: bool) -> Result<(), MergeError> {
	use std::collections::HashMap;

	// Build map of other children by URL (or by number for pending)
	let mut other_by_url: HashMap<String, Issue> = HashMap::new();
	let mut other_pending: Vec<Issue> = Vec::new();

	for child in other_children {
		if let Some(url) = child.url_str() {
			other_by_url.insert(url.to_string(), child);
		} else if let Some(num) = child.git_id() {
			// Pending issues matched by number
			other_by_url.insert(format!("pending:{num}"), child);
		} else {
			other_pending.push(child);
		}
	}

	// Merge existing children
	for self_child in self_children.iter_mut() {
		let key = if let Some(url) = self_child.url_str() {
			url.to_string()
		} else if let Some(num) = self_child.git_id() {
			format!("pending:{num}")
		} else {
			continue; // Can't match without URL or number
		};

		if let Some(other_child) = other_by_url.remove(&key) {
			self_child.merge(other_child, force)?;
		}
	}

	// Add new children from other (ones that weren't in self)
	for (_, child) in other_by_url {
		self_children.push(child);
	}

	// Add pending children from other that couldn't be matched
	for child in other_pending {
		// Only add if not virtual
		if !child.identity.is_virtual() {
			self_children.push(child);
		}
	}

	// Sort children by issue number for consistent ordering
	self_children.sort_by(|a, b| {
		let a_num = a.git_id().unwrap_or(u64::MAX);
		let b_num = b.git_id().unwrap_or(u64::MAX);
		a_num.cmp(&b_num)
	});

	Ok(())
}

#[cfg(test)]
mod tests {
	use tedi::{IssueContents, IssueIdentity, IssueIndex, IssueLink, IssueTimestamps, RepoInfo};

	use super::*;

	fn test_repo() -> RepoInfo {
		RepoInfo::new("test", "repo")
	}

	fn make_linked_issue(title: &str, number: u64, timestamps: IssueTimestamps) -> Issue {
		let url = format!("https://github.com/test/repo/issues/{number}");
		let link = IssueLink::parse(&url).unwrap();
		let parent_index = IssueIndex::repo_only(test_repo());
		let identity = IssueIdentity::linked(Some(parent_index), "user".to_string(), link, timestamps);
		Issue {
			identity,
			contents: IssueContents {
				title: title.to_string(),
				labels: vec![],
				state: tedi::CloseState::Open,
				comments: vec![],
				blockers: Default::default(),
			},
			children: vec![],
		}
	}

	fn make_pending_issue(title: &str) -> Issue {
		let parent_index = IssueIndex::repo_only(test_repo());
		let identity = IssueIdentity::pending(parent_index);
		Issue {
			identity,
			contents: IssueContents {
				title: title.to_string(),
				labels: vec![],
				state: tedi::CloseState::Open,
				comments: vec![],
				blockers: Default::default(),
			},
			children: vec![],
		}
	}

	#[test]
	fn test_merge_virtual_error() {
		let parent_index = IssueIndex::repo_only(test_repo());
		// Create a virtual issue (local-only, never synced to Github)
		let mut virtual_issue = Issue {
			identity: IssueIdentity::virtual_issue(parent_index),
			contents: IssueContents::default(),
			children: vec![],
		};

		let ts = Timestamp::now();
		let timestamps = IssueTimestamps {
			title: Some(ts),
			description: Some(ts),
			labels: Some(ts),
			state: Some(ts),
			comments: vec![],
		};
		let linked = make_linked_issue("test", 1, timestamps);

		let result = virtual_issue.merge(linked, false);
		assert!(matches!(result, Err(MergeError::VirtualIssue)));
	}

	#[test]
	fn test_merge_newer_wins() {
		let old_ts = Timestamp::from_second(1000).unwrap();
		let new_ts = Timestamp::from_second(2000).unwrap();

		let old_timestamps = IssueTimestamps {
			title: Some(old_ts),
			description: None,
			labels: None,
			state: None,
			comments: vec![],
		};
		let new_timestamps = IssueTimestamps {
			title: Some(new_ts),
			description: None,
			labels: None,
			state: None,
			comments: vec![],
		};

		let mut self_issue = make_linked_issue("old title", 1, old_timestamps);
		let other_issue = make_linked_issue("new title", 1, new_timestamps);

		self_issue.merge(other_issue, false).unwrap();

		assert_eq!(self_issue.contents.title, "new title");
	}

	#[test]
	fn test_merge_older_keeps_self() {
		let old_ts = Timestamp::from_second(1000).unwrap();
		let new_ts = Timestamp::from_second(2000).unwrap();

		let new_timestamps = IssueTimestamps {
			title: Some(new_ts),
			description: None,
			labels: None,
			state: None,
			comments: vec![],
		};
		let old_timestamps = IssueTimestamps {
			title: Some(old_ts),
			description: None,
			labels: None,
			state: None,
			comments: vec![],
		};

		let mut self_issue = make_linked_issue("newer title", 1, new_timestamps);
		let other_issue = make_linked_issue("older title", 1, old_timestamps);

		self_issue.merge(other_issue, false).unwrap();

		assert_eq!(self_issue.contents.title, "newer title");
	}

	#[test]
	fn test_merge_none_takes_some() {
		let ts = Timestamp::from_second(1000).unwrap();

		let none_timestamps = IssueTimestamps::default();
		let some_timestamps = IssueTimestamps {
			title: Some(ts),
			description: None,
			labels: None,
			state: None,
			comments: vec![],
		};

		let mut self_issue = make_linked_issue("self title", 1, none_timestamps);
		let other_issue = make_linked_issue("other title", 1, some_timestamps);

		self_issue.merge(other_issue, false).unwrap();

		assert_eq!(self_issue.contents.title, "other title");
	}

	#[test]
	fn test_merge_force_always_takes_other() {
		let old_ts = Timestamp::from_second(1000).unwrap();
		let new_ts = Timestamp::from_second(2000).unwrap();

		// self has newer timestamp
		let new_timestamps = IssueTimestamps {
			title: Some(new_ts),
			description: None,
			labels: None,
			state: None,
			comments: vec![],
		};
		let old_timestamps = IssueTimestamps {
			title: Some(old_ts),
			description: None,
			labels: None,
			state: None,
			comments: vec![],
		};

		let mut self_issue = make_linked_issue("newer title", 1, new_timestamps);
		let other_issue = make_linked_issue("older title", 1, old_timestamps);

		// With force=true, other should win even though it's older
		self_issue.merge(other_issue, true).unwrap();

		assert_eq!(self_issue.contents.title, "older title");
	}

	#[test]
	fn test_merge_pending_uses_default_timestamps() {
		let ts = Timestamp::from_second(1000).unwrap();
		let timestamps = IssueTimestamps {
			title: Some(ts),
			description: None,
			labels: None,
			state: None,
			comments: vec![],
		};

		let mut pending = make_pending_issue("pending title");
		let linked = make_linked_issue("linked title", 1, timestamps);

		// Pending has no timestamps (None), so linked should win
		pending.merge(linked, false).unwrap();

		assert_eq!(pending.contents.title, "linked title");
	}

	#[test]
	fn test_merge_same_timestamp_keeps_self() {
		let ts = Timestamp::from_second(1000).unwrap();

		let timestamps = IssueTimestamps {
			title: Some(ts),
			description: None,
			labels: None,
			state: None,
			comments: vec![],
		};

		let mut self_issue = make_linked_issue("self title", 1, timestamps.clone());
		let other_issue = make_linked_issue("other title", 1, timestamps);

		self_issue.merge(other_issue, false).unwrap();

		// Same timestamp -> keep self
		assert_eq!(self_issue.contents.title, "self title");
	}
}
