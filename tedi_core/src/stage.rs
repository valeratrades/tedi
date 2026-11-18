//! Where an issue sits on the board. Derived: GitHub tells the closed stages and the review ones, and
//! only what it can't — Icebox, Selected, In progress without a PR — is stored, as an `s:` pin.

use jiff::{SignedDuration, Timestamp};

use crate::{CloseState, LinkedPr, PrState, labels::Pin};

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd, serde::Deserialize, strum::Display, strum::EnumIter, strum::EnumString, serde::Serialize)]
#[serde(rename_all = "snake_case")]
#[strum(serialize_all = "snake_case")]
pub enum Stage {
	Icebox,
	/// The intake: nobody has looked at it yet.
	Triage,
	Selected,
	InProgress,
	InReview,
	Done,
	WontDo,
	/// Closed longer than `archive_after` ago.
	Backlog,
}

/// `closed_at`: when the state last changed, `None` when the timeline holds no such event.
pub fn stage(pin: Option<Pin>, state: &CloseState, prs: &[LinkedPr], closed_at: Option<Timestamp>, archive_after: SignedDuration, now: Timestamp) -> Stage {
	if state.is_closed() {
		return closed(matches!(state, CloseState::Closed), closed_at, archive_after, now);
	}
	let pr = |s: PrState| prs.iter().any(|p| p.state == s);
	match pin {
		_ if pr(PrState::Open) => Stage::InReview,
		Some(Pin::Wip) => Stage::InProgress,
		_ if pr(PrState::Draft) => Stage::InProgress,
		Some(Pin::Selected) => Stage::Selected,
		Some(Pin::Icebox) => Stage::Icebox,
		None => Stage::Triage,
	}
}

/// A pull request's own: its state alone tells it.
pub fn pr_stage(state: PrState, closed_at: Option<Timestamp>, archive_after: SignedDuration, now: Timestamp) -> Stage {
	match state {
		PrState::Draft => Stage::InProgress,
		PrState::Open => Stage::InReview,
		PrState::Merged => closed(true, closed_at, archive_after, now),
		PrState::Closed => closed(false, closed_at, archive_after, now),
	}
}

/// `closed_at` `None`: no close event on the timeline, it predates what GitHub keeps.
fn closed(completed: bool, closed_at: Option<Timestamp>, archive_after: SignedDuration, now: Timestamp) -> Stage {
	match (closed_at.is_none_or(|at| now.duration_since(at) > archive_after), completed) {
		(true, _) => Stage::Backlog,
		(false, true) => Stage::Done,
		(false, false) => Stage::WontDo,
	}
}

#[cfg(test)]
mod tests {
	use super::*;

	const WEEK: SignedDuration = SignedDuration::from_hours(24 * 7);

	fn now() -> Timestamp {
		"2026-11-14T00:00:00Z".parse().unwrap()
	}

	fn pr(state: PrState) -> LinkedPr {
		LinkedPr {
			url: "https://github.com/o/r/pull/1".into(),
			state,
		}
	}

	#[track_caller]
	fn check(pin: Option<Pin>, state: CloseState, prs: &[LinkedPr], closed_days_ago: Option<i64>, expected: Stage) {
		let closed_at = closed_days_ago.map(|d| now() - SignedDuration::from_hours(24 * d));
		assert_eq!(stage(pin, &state, prs, closed_at, WEEK, now()), expected);
	}

	#[test]
	fn open_stages() {
		check(None, CloseState::Open, &[], None, Stage::Triage);
		check(Some(Pin::Icebox), CloseState::Open, &[], None, Stage::Icebox);
		check(Some(Pin::Selected), CloseState::Open, &[], None, Stage::Selected);
		check(Some(Pin::Wip), CloseState::Open, &[], None, Stage::InProgress);
		check(Some(Pin::Selected), CloseState::Open, &[pr(PrState::Draft)], None, Stage::InProgress);
		check(Some(Pin::Wip), CloseState::Open, &[pr(PrState::Draft), pr(PrState::Open)], None, Stage::InReview);
		check(None, CloseState::Open, &[pr(PrState::Closed), pr(PrState::Merged)], None, Stage::Triage);
	}

	#[test]
	fn closed_stages_age_into_backlog() {
		check(Some(Pin::Wip), CloseState::Closed, &[pr(PrState::Open)], Some(1), Stage::Done);
		check(None, CloseState::NotPlanned, &[], Some(1), Stage::WontDo);
		check(None, CloseState::Closed, &[], Some(8), Stage::Backlog);
		check(None, CloseState::Closed, &[], None, Stage::Backlog);
	}

	#[test]
	fn pr_stages() {
		let ago = |d: i64| Some(now() - SignedDuration::from_hours(24 * d));
		let check = |state, closed_at, expected| assert_eq!(pr_stage(state, closed_at, WEEK, now()), expected, "{state} closed {closed_at:?}");
		check(PrState::Draft, None, Stage::InProgress);
		check(PrState::Open, None, Stage::InReview);
		check(PrState::Merged, ago(1), Stage::Done);
		check(PrState::Closed, ago(1), Stage::WontDo);
		check(PrState::Merged, ago(8), Stage::Backlog);
	}
}
