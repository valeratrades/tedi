//! Edits shown before they land: the board on screen is the server's with the pending ones laid over it. An
//! issue's leave one at a time, so each is made against what the one before it left. One that can't land is
//! taken off, with what was queued on top of it.

use std::collections::{BTreeMap, BTreeSet};

use dioxus::{core::spawn_forever, prelude::*};
use tedi_core::{
	IssueLink, MilestoneLink,
	board::{Board, Card},
	conflict::FieldEdit,
	workflow::State,
};

use super::{Ctx, toast};
use crate::api::{self, Edited};

#[derive(Clone, Debug, PartialEq)]
pub(super) struct Pending {
	pub(super) id: u64,
	link: IssueLink,
	op: Op,
	stage: Stage,
}

#[derive(Clone, Copy, Debug, PartialEq)]
enum Stage {
	Queued,
	Sent,
	/// The server holds it; the board last read may not.
	Landed,
}

#[derive(Clone, Debug, PartialEq)]
enum Op {
	Edit(Vec<FieldEdit>),
	Move {
		workflow: String,
		seen: State,
		to: State,
	},
	/// The one of `repo`'s hosting it.
	Milestone {
		repo: String,
		seen: Option<MilestoneLink>,
		new: Option<MilestoneLink>,
	},
	/// `reviewer`'s review asked for, or the ask withdrawn.
	Review {
		reviewer: String,
		on: bool,
	},
}
impl Op {
	fn show(&self, link: &IssueLink, board: &mut Board) {
		let Some(card) = board.cards.iter_mut().find(|c| c.link == *link) else {
			return; // gone from the board since: nothing to show it on
		};
		match self {
			Op::Edit(edits) =>
				for e in edits {
					card.write(e.field, &e.new).expect("only what a single value sets is offered for editing");
				},
			Op::Move { workflow, to, .. } => _ = card.states.insert(workflow.clone(), to.clone()),
			Op::Milestone { seen, new, .. } =>
				for t in &mut board.targets {
					if Some(&t.link) == seen.as_ref() {
						t.hosted.retain(|h| h != link);
					}
					if Some(&t.link) == new.as_ref() && !t.hosted.contains(link) {
						t.hosted.push(link.clone());
					}
				},
			Op::Review { reviewer, on } => {
				let requested = &mut card.pr.as_mut().expect("reviews are asked of pull requests").requested;
				requested.retain(|r| r != reviewer);
				if *on {
					requested.push(reviewer.clone());
				}
			}
		}
	}

	fn back(&self) -> Op {
		match self {
			Op::Edit(edits) => Op::Edit(
				edits
					.iter()
					.map(|e| FieldEdit {
						field: e.field,
						seen: e.new.clone(),
						new: e.seen.clone(),
					})
					.collect(),
			),
			Op::Move { workflow, seen, to } => Op::Move {
				workflow: workflow.clone(),
				seen: to.clone(),
				to: seen.clone(),
			},
			Op::Milestone { repo, seen, new } => Op::Milestone {
				repo: repo.clone(),
				seen: new.clone(),
				new: seen.clone(),
			},
			Op::Review { reviewer, on } => Op::Review {
				reviewer: reviewer.clone(),
				on: !on,
			},
		}
	}

	/// Whether it was made against what `under` was to leave.
	fn rests_on(&self, under: &Op) -> bool {
		match (self, under) {
			(Op::Edit(a), Op::Edit(b)) => a.iter().any(|a| b.iter().any(|b| a.field == b.field)),
			(Op::Move { workflow: a, .. }, Op::Move { workflow: b, .. }) => a == b,
			(Op::Milestone { repo: a, .. }, Op::Milestone { repo: b, .. }) => a == b,
			(Op::Review { reviewer: a, .. }, Op::Review { reviewer: b, .. }) => a == b,
			_ => false,
		}
	}

	/// Its outcome's note: one per issue and what is set, the last told.
	fn key(&self, link: &IssueLink) -> String {
		match self {
			Op::Edit(_) => format!("edit:{link}"),
			Op::Move { .. } => format!("move:{link}"),
			Op::Milestone { repo, .. } => format!("milestone:{link}:{repo}"),
			Op::Review { reviewer, .. } => format!("review:{link}:{reviewer}"),
		}
	}

	/// What was done once landed, and to what.
	fn told(&self, link: &IssueLink, ctx: &Ctx) -> (&'static str, String) {
		let n = link.number();
		match self {
			Op::Edit(edits) => ("Edited", format!("{} on #{n}", edits.iter().map(|e| e.field.to_string()).collect::<Vec<_>>().join(", "))),
			Op::Move { to, .. } => ("Moved", format!("#{n} → {to}")),
			Op::Milestone { repo, new: Some(m), .. } => {
				let board = ctx.board.peek();
				let target = board.as_ref().and_then(|b| b.targets.iter().find(|t| t.link == *m));
				let title = target.map_or_else(|| format!("milestone {}", m.number()), |t| t.title.clone()); // closed since it was offered
				("Put", format!("#{n} in {repo}'s {title}"))
			}
			Op::Milestone { repo, new: None, .. } => ("Took", format!("#{n} out of {repo}'s milestone")),
			Op::Review { reviewer, on: true } => ("Asked for", format!("{reviewer}'s review of #{n}")),
			Op::Review { reviewer, on: false } => ("Withdrew the ask for", format!("{reviewer}'s review of #{n}")),
		}
	}
}

/// The server's board, `pending` laid over it.
pub(super) fn shown(stored: &Option<Board>, pending: &[Pending]) -> Option<Board> {
	let mut board = stored.clone()?;
	for p in pending {
		p.op.show(&p.link, &mut board);
	}
	Some(board)
}

/// What a board read from here on holds.
pub(super) fn landed(pending: &[Pending]) -> Vec<u64> {
	pending.iter().filter(|p| p.stage == Stage::Landed).map(|p| p.id).collect()
}

/// Whether closing the page would lose any.
pub(super) fn unsent(pending: &[Pending]) -> bool {
	pending.iter().any(|p| p.stage != Stage::Landed)
}

impl Ctx {
	/// `edits` to `link`, as seen on its card: a landed one can be undone from its note, a clash lands on the
	/// issue's tab, inline.
	pub(super) fn edit(self, link: IssueLink, edits: Vec<FieldEdit>) {
		self.submit(link, Op::Edit(edits));
	}

	/// `card` to `to` of `workflow`, from where it shows sitting: a landed move can be undone, a hook's
	/// refusal says why, a clash lands on the issue's tab.
	pub(super) fn move_card(self, card: Card, workflow: String, to: State) {
		let seen = card.states[&workflow].clone();
		self.submit(card.link, Op::Move { workflow, seen, to });
	}

	/// `link` from the milestones `seen` to `new` (their urls), one a repo.
	pub(super) fn set_milestones(self, link: IssueLink, seen: Vec<String>, new: Vec<String>) {
		let of = |urls: &[String]| -> BTreeMap<String, MilestoneLink> {
			urls.iter()
				.map(|u| MilestoneLink::parse(u).expect("offered by the board"))
				.map(|m| (m.repo_info().to_string(), m))
				.collect()
		};
		let (from, to) = (of(&seen), of(&new));
		for repo in from.keys().chain(to.keys()).collect::<BTreeSet<_>>() {
			let (seen, new) = (from.get(repo).cloned(), to.get(repo).cloned());
			if seen != new {
				self.submit(link.clone(), Op::Milestone { repo: repo.clone(), seen, new });
			}
		}
	}

	/// The reviews asked of `link`, from `seen` to `new`.
	pub(super) fn request_reviews(self, link: IssueLink, seen: Vec<String>, new: Vec<String>) {
		let asked = new.iter().filter(|r| !seen.contains(r)).map(|r| (r, true));
		let withdrawn = seen.iter().filter(|r| !new.contains(r)).map(|r| (r, false));
		for (reviewer, on) in asked.chain(withdrawn) {
			let reviewer = reviewer.clone();
			self.submit(link.clone(), Op::Review { reviewer, on });
		}
	}

	fn submit(self, link: IssueLink, op: Op) {
		let (mut pending, mut notes) = (self.pending, self.notes);
		let key = op.key(&link);
		notes.write().retain(|n| n.key != key); // its Undo was made against what this replaces
		let mut pending = pending.write();
		let sending = pending.iter().any(|p| p.link == link && p.stage != Stage::Landed);
		if let Op::Edit(edits) = &op
			&& let Some(at) = pending.iter().rposition(|p| p.link == link && p.op.rests_on(&op))
			&& let Pending {
				op: Op::Edit(queued),
				stage: Stage::Queued,
				..
			} = &mut pending[at]
			&& queued.len() == edits.len()
			&& queued.iter().zip(edits).all(|(q, e)| q.field == e.field && q.new == e.seen)
		{
			for (q, e) in queued.iter_mut().zip(edits) {
				q.new = e.new.clone();
			}
			if queued.iter().all(|q| q.seen == q.new) {
				pending.remove(at);
			}
			return; // what queued it has a sender on its way
		}
		let id = pending.iter().map(|p| p.id).max().map_or(0, |m| m + 1);
		pending.push(Pending {
			id,
			link: link.clone(),
			op,
			stage: Stage::Queued,
		});
		drop(pending);
		if !sending {
			spawn_forever(self.send(link)); // the card it was made on may leave the screen with it
		}
	}

	/// Everything queued on `link`, oldest first, until none is.
	async fn send(self, link: IssueLink) {
		while let Some((id, op)) = self.next(&link) {
			let sent = match &op {
				Op::Edit(edits) => api::edit(link.clone(), edits.clone()).await,
				Op::Move { workflow, seen, to } => api::move_card(link.clone(), workflow.clone(), to.clone(), seen.clone()).await,
				Op::Milestone { repo, seen, new } => api::set_milestone(link.clone(), repo.clone(), seen.clone(), new.clone()).await,
				Op::Review { reviewer, on } => api::request_review(link.clone(), reviewer.clone(), *on).await.map(|()| Edited::Landed),
			};
			let why = match sent {
				Ok(Edited::Landed) => {
					self.landed(id, &link, &op);
					continue;
				}
				Ok(Edited::Conflict(mut conflict)) => {
					for e in self
						.take_off(id, &link, &op)
						.iter()
						.filter_map(|o| if let Op::Edit(edits) = o { Some(edits) } else { None })
						.flatten()
					{
						for clash in conflict.clashes.iter_mut().filter(|c| c.field == e.field && c.node == link) {
							clash.local = e.new.clone(); // yours is the last one asked for
						}
					}
					self.clashed(link.clone(), conflict);
					continue;
				}
				Ok(Edited::Rejected(why)) => why,
				Err(e) => e.to_string(),
			};
			self.take_off(id, &link, &op);
			self.refused(op.key(&link), link.clone(), format!("{} didn't go through: {why}", op.told(&link, &self).1));
		}
	}

	fn next(self, link: &IssueLink) -> Option<(u64, Op)> {
		let mut pending = self.pending;
		let mut pending = pending.write();
		let next = pending.iter_mut().find(|p| p.link == *link && p.stage == Stage::Queued)?;
		next.stage = Stage::Sent;
		Some((next.id, next.op.clone()))
	}

	fn landed(self, id: u64, link: &IssueLink, op: &Op) {
		let (mut pending, mut conflicts) = (self.pending, self.conflicts);
		let followed = {
			let mut pending = pending.write();
			pending.iter_mut().find(|p| p.id == id).expect("only its sender takes it off").stage = Stage::Landed;
			pending.iter().any(|p| p.link == *link && p.stage == Stage::Queued && p.op.rests_on(op))
		};
		conflicts.write().remove(link);
		spawn_forever(super::reload(self));
		if !followed {
			let (back, to) = (op.back(), link.clone());
			let undo = Callback::new(move |()| self.submit(to.clone(), back.clone()));
			let (done, what) = op.told(link, &self);
			self.say(op.key(link), toast::Tone::Info, format!("{done} {what}"), Some(("Undo", undo)));
		}
	}

	/// `id` and everything queued on top of it, oldest first, off the board.
	fn take_off(self, id: u64, link: &IssueLink, op: &Op) -> Vec<Op> {
		let mut pending = self.pending;
		let mut pending = pending.write();
		let (off, kept): (Vec<Pending>, Vec<Pending>) = pending.drain(..).partition(|p| p.id == id || (p.link == *link && p.stage == Stage::Queued && p.op.rests_on(op)));
		*pending = kept;
		off.into_iter().map(|p| p.op).collect()
	}
}
