//! The Milestone primitive: identity, body (prose + hosted issue links),
//! parse/serialize, and a pure field-level `merge`.
//!
//! A milestone is the "even simpler" sibling of `Issue`: it owns no files of its own,
//! has no comments/blockers/child tree — just free prose plus a flat list of hosted
//! issue references. Title/state/due_on live in identity (persisted to `.meta.json`),
//! the body is the durable file content (the old `CachedMilestone.content` shape).

use jiff::Timestamp;
use serde::{Deserialize, Serialize};

use crate::{CloseState, IssueLink, MilestoneLink, TaskView};

/// Timestamps tracking when individual milestone fields last changed. A milestone-shaped
/// sibling of `IssueTimestamps`, used for field-level sync conflict resolution.
#[derive(Clone, Debug, Default, Deserialize, PartialEq, Serialize)]
pub struct MilestoneTimestamps {
	pub title: Option<Timestamp>,
	pub state: Option<Timestamp>,
	pub due_on: Option<Timestamp>,
	/// Description covers both the prose and the hosted-issue set.
	pub description: Option<Timestamp>,
}
impl MilestoneTimestamps {
	pub fn now() -> Self {
		let now = Timestamp::now();
		Self {
			title: Some(now),
			state: Some(now),
			due_on: Some(now),
			description: Some(now),
		}
	}

	/// Bump the stamp of every field that differs between `old` and `new` to now.
	pub fn update_from_diff(&mut self, old: &Milestone, new: &Milestone) {
		let now = Timestamp::now();
		if old.identity.title != new.identity.title {
			self.title = Some(now);
		}
		if old.identity.state != new.identity.state {
			self.state = Some(now);
		}
		if old.identity.due_on != new.identity.due_on {
			self.due_on = Some(now);
		}
		if old.body != new.body {
			self.description = Some(now);
		}
	}
}

/// Identity of a milestone — everything but the free body. Persisted field-by-field to
/// the project `.meta.json`; the link is the stable identifier.
#[derive(Clone, Debug, PartialEq)]
pub struct MilestoneIdentity {
	pub link: MilestoneLink,
	pub state: CloseState,
	pub due_on: Option<Timestamp>,
	pub title: String,
	pub timestamps: MilestoneTimestamps,
}

/// A milestone's body: the lossless task-view document itself. Hosted issue links are
/// derived on demand, never a stored projection.
#[derive(Clone, Default)]
pub struct MilestoneBody(pub TaskView);
impl MilestoneBody {
	pub fn parse(content: &str) -> Self {
		let mut doc = TaskView::parse(content);
		doc.resolve_bare_refs();
		Self(doc)
	}

	/// Hosted issue links, first-seen order, deduplicated (derived, not stored).
	pub fn hosted(&self) -> Vec<IssueLink> {
		let mut out = Vec::new();
		for link in self.0.issue_links() {
			if !out.contains(&link) {
				out.push(link);
			}
		}
		out
	}
}
// Semantic equality is "renders identically" — what change-detection and the sink care about.
// Hand-impl avoids forcing deep PartialEq/Debug across every TaskView node type.
impl PartialEq for MilestoneBody {
	fn eq(&self, other: &Self) -> bool {
		self.0.serialize() == other.0.serialize()
	}
}
impl std::fmt::Debug for MilestoneBody {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		f.debug_tuple("MilestoneBody").field(&self.0.serialize()).finish()
	}
}

/// A milestone: identity + body. The single durable, syncable milestone primitive.
#[derive(Clone, Debug, PartialEq)]
pub struct Milestone {
	pub identity: MilestoneIdentity,
	pub body: MilestoneBody,
}
impl Milestone {
	pub fn link(&self) -> &MilestoneLink {
		&self.identity.link
	}

	pub fn number(&self) -> u64 {
		self.identity.link.number()
	}

	pub fn is_closed(&self) -> bool {
		self.identity.state.is_closed()
	}

	/// Recompute timestamps after a modification, diffing against the pre-edit state.
	pub fn post_update(&mut self, old: &Milestone) {
		let mut ts = self.identity.timestamps.clone();
		ts.update_from_diff(old, self);
		self.identity.timestamps = ts;
	}

	/// Merge `other` into `self`, using timestamps to resolve per-field conflicts.
	/// `force` always takes `other`. Hosted links are a set union (same spirit as
	/// children merge — merge only adds; removals are a sink concern).
	pub fn merge(&mut self, other: &Milestone, force: bool) {
		let s = self.identity.timestamps.clone();
		let o = other.identity.timestamps.clone();
		let dominated = |sf: Option<Timestamp>, of: Option<Timestamp>| -> bool {
			if force {
				return true;
			}
			match (sf, of) {
				(_, None) => false,
				(None, Some(_)) => true,
				(Some(a), Some(b)) => b > a,
			}
		};

		if dominated(s.title, o.title) {
			self.identity.title = other.identity.title.clone();
		}
		if dominated(s.state, o.state) {
			self.identity.state = other.identity.state.clone();
		}
		if dominated(s.due_on, o.due_on) {
			self.identity.due_on = other.identity.due_on;
		}
		// hosted is a set union regardless of which doc wins — no GitHub-assigned issue is dropped.
		let mut union = self.body.hosted();
		for link in other.body.hosted() {
			if !union.contains(&link) {
				union.push(link);
			}
		}
		if dominated(s.description, o.description) {
			self.body = other.body.clone();
		}
		let present = self.body.hosted();
		let missing: Vec<IssueLink> = union.into_iter().filter(|l| !present.contains(l)).collect();
		self.body.0.push_issue_links(&missing);

		// Winning value carries the winning (newer) timestamp — keeps stamps consistent with values.
		self.identity.timestamps = MilestoneTimestamps {
			title: s.title.max(o.title),
			state: s.state.max(o.state),
			due_on: s.due_on.max(o.due_on),
			description: s.description.max(o.description),
		};
	}
}

/// Canonical file form: the lossless task-view render — checkboxes, order, nested headings
/// and blank lines all survive.
impl std::fmt::Display for Milestone {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		f.write_str(&self.body.0.serialize())
	}
}

#[cfg(test)]
mod tests {
	use super::*;

	fn ml(n: u64) -> MilestoneLink {
		MilestoneLink::parse(&format!("https://github.com/o/r/milestone/{n}")).unwrap()
	}
	fn milestone(body: &str, title: &str, ts: MilestoneTimestamps) -> Milestone {
		Milestone {
			identity: MilestoneIdentity {
				link: ml(3),
				state: CloseState::Open,
				due_on: None,
				title: title.to_string(),
				timestamps: ts,
			},
			body: MilestoneBody::parse(body),
		}
	}

	#[test]
	fn parse_and_hosted() {
		let body = MilestoneBody::parse("# Goals\n\nship it\n\n- o/r#5\n- https://github.com/o/r/issues/6\n");
		let hosted: Vec<u64> = body.hosted().iter().map(|l| l.number()).collect();
		assert_eq!(hosted, [5, 6]);
		insta::assert_snapshot!(body.0.serialize(), @"
		# Goals

		ship it

		- o/r#5
		- https://github.com/o/r/issues/6
		");
	}

	#[test]
	fn display_roundtrip() {
		let m = milestone(
			"# Goals\n\nship it\n\n- https://github.com/o/r/issues/5\n- https://github.com/o/r/issues/6",
			"big",
			MilestoneTimestamps::now(),
		);
		let s1 = m.to_string();
		let reparsed = MilestoneBody::parse(&s1);
		let m2 = Milestone {
			identity: m.identity.clone(),
			body: reparsed,
		};
		assert_eq!(s1, m2.to_string(), "Display must be idempotent through parse");
		insta::assert_snapshot!(s1, @"
		# Goals

		ship it

		- https://github.com/o/r/issues/5
		- https://github.com/o/r/issues/6
		");
	}

	#[test]
	fn merge_newer_description_wins_hosted_unions() {
		let old = Timestamp::from_second(1000).unwrap();
		let new = Timestamp::from_second(2000).unwrap();
		let self_ts = MilestoneTimestamps {
			description: Some(old),
			..Default::default()
		};
		let other_ts = MilestoneTimestamps {
			description: Some(new),
			..Default::default()
		};
		let mut a = milestone("old desc\n\n- https://github.com/o/r/issues/5", "t", self_ts);
		let b = milestone("new desc\n\n- https://github.com/o/r/issues/6", "t", other_ts);
		a.merge(&b, false);
		assert!(a.body.0.serialize().contains("new desc"));
		let mut hosted: Vec<u64> = a.body.hosted().iter().map(|l| l.number()).collect();
		hosted.sort_unstable();
		assert_eq!(hosted, [5, 6], "hosted is a set union regardless of which description won");
	}

	#[test]
	fn merge_older_keeps_self() {
		let old = Timestamp::from_second(1000).unwrap();
		let new = Timestamp::from_second(2000).unwrap();
		let mut a = milestone(
			"newer",
			"self",
			MilestoneTimestamps {
				title: Some(new),
				description: Some(new),
				..Default::default()
			},
		);
		let b = milestone(
			"older",
			"other",
			MilestoneTimestamps {
				title: Some(old),
				description: Some(old),
				..Default::default()
			},
		);
		a.merge(&b, false);
		assert_eq!(a.identity.title, "self");
		assert_eq!(a.body.0.serialize().trim(), "newer");
	}

	#[test]
	fn merge_force_takes_other() {
		let new = Timestamp::from_second(2000).unwrap();
		let mut a = milestone(
			"self",
			"self",
			MilestoneTimestamps {
				title: Some(new),
				..Default::default()
			},
		);
		let b = milestone("other", "other", MilestoneTimestamps::default());
		a.merge(&b, true);
		assert_eq!(a.identity.title, "other");
	}
}
