//! The hooks tedi ships, each named in a workflow file with its parameters beside it.

use jiff::SignedDuration;
use serde::Deserialize;
use tedi_core::{
	CloseState, PrState,
	conflict::Field,
	workflow::{State, Workflow},
};

use crate::{Buffer, Ctx, Hook, Moment, Mover, Role};

#[derive(Debug, Deserialize)]
#[serde(tag = "builtin", rename_all = "snake_case", deny_unknown_fields)]
pub(crate) enum Builtin {
	/// Entering, an unassigned card is assigned to the person moving it.
	AssignMover,
	Close {
		reason: Reason,
	},
	/// Entering, a closed card opens.
	Reopen,
	/// Entering by hand is refused: only rules move a card here.
	Reject {
		reason: String,
	},
	/// A card whose leading pull request (open, else draft, else merged, else closed) is in `state`;
	/// a pull request's own card leads with itself.
	OnPr {
		state: PrState,
		to: State,
		#[serde(default)]
		forward_only: bool,
		#[serde(default)]
		from: Vec<State>,
	},
	OnClosed {
		reason: Reason,
		to: State,
		#[serde(default)]
		forward_only: bool,
		#[serde(default)]
		from: Vec<State>,
	},
	OnReopened {
		to: State,
		#[serde(default)]
		forward_only: bool,
		#[serde(default)]
		from: Vec<State>,
	},
	/// Closed longer than `after` ago, or so long that GitHub kept no close event.
	ClosedFor {
		#[serde(deserialize_with = "span")]
		after: SignedDuration,
		to: State,
		#[serde(default)]
		forward_only: bool,
		#[serde(default)]
		from: Vec<State>,
	},
}
impl Builtin {
	fn to(&self) -> Option<To<'_>> {
		match self {
			Builtin::OnPr { to, forward_only, from, .. }
			| Builtin::OnClosed { to, forward_only, from, .. }
			| Builtin::OnReopened { to, forward_only, from }
			| Builtin::ClosedFor { to, forward_only, from, .. } => Some(To {
				to,
				forward_only: *forward_only,
				from,
			}),
			Builtin::AssignMover | Builtin::Close { .. } | Builtin::Reopen | Builtin::Reject { .. } => None,
		}
	}

	pub(crate) fn load(name: &str, params: &serde_json::Map<String, serde_json::Value>, role: Role, workflow: &Workflow) -> Result<Self, String> {
		let mut whole = params.clone();
		whole.insert("builtin".into(), name.into());
		let builtin: Builtin = serde_json::from_value(whole.into()).map_err(|e| e.to_string())?;
		match (builtin.to(), role) {
			(None, Role::OnEnter) => {}
			(Some(to), Role::Rule) => {
				workflow.leaf(to.to).map_err(|e| e.to_string())?;
				if let Some(from) = to.from.iter().find(|f| workflow.path(f.names()).is_none()) {
					return Err(format!("from: no bucket {from}"));
				}
			}
			(_, Role::Rule) => return Err(format!("{name} is no rule")),
			(_, Role::OnEnter) => return Err(format!("{name} is a rule, not an on_enter hook")),
		}
		Ok(builtin)
	}
}

/// A rule's: where it moves a card; never to a leaf before where it sits, when `forward_only`; only from
/// under `from`'s buckets, when it names any. Flattened by hand: serde can't deny unknown fields beside a flatten.
struct To<'a> {
	to: &'a State,
	forward_only: bool,
	from: &'a [State],
}
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq)]
#[serde(rename_all = "snake_case")]
pub(crate) enum Reason {
	Completed,
	NotPlanned,
}
impl Reason {
	fn state(self) -> CloseState {
		match self {
			Reason::Completed => CloseState::Closed,
			Reason::NotPlanned => CloseState::NotPlanned,
		}
	}
}

fn span<'de, D: serde::Deserializer<'de>>(d: D) -> Result<SignedDuration, D::Error> {
	let span: jiff::Span = String::deserialize(d)?.parse().map_err(serde::de::Error::custom)?;
	span.to_duration(jiff::SpanRelativeTo::days_are_24_hours()).map_err(serde::de::Error::custom)
}

impl Hook for Builtin {
	fn call(&self, ctx: &Ctx, out: &mut Buffer) -> Result<(), String> {
		let card = ctx.card;
		match (&ctx.at, self) {
			(Moment::Entering { mover: Mover::Person(login), .. }, Builtin::AssignMover) if card.assignees.is_empty() => out.edit(Field::Assignees, login.clone())?,
			(Moment::Entering { .. }, Builtin::AssignMover) => {}
			(Moment::Entering { .. }, Builtin::Close { reason }) => out.edit(Field::State, reason.state().to_checkbox_contents())?,
			(Moment::Entering { .. }, Builtin::Reopen) if !card.open() => out.edit(Field::State, CloseState::Open.to_checkbox_contents())?,
			(Moment::Entering { .. }, Builtin::Reopen) => {}
			(Moment::Entering { mover: Mover::Person(_), .. }, Builtin::Reject { reason }) => out.rejected = Some(reason.clone()),
			(Moment::Entering { mover: Mover::Rule, .. }, Builtin::Reject { .. }) => {}
			(Moment::Pulled { current }, rule) => {
				let holds = match rule {
					Builtin::OnPr { state, .. } => leading(card) == Some(*state),
					Builtin::OnClosed { reason, .. } => card.state == reason.state(),
					Builtin::OnReopened { .. } => card.open(),
					Builtin::ClosedFor { after, .. } => !card.open() && card.closed_at.is_none_or(|at| ctx.now.duration_since(at) > *after),
					_ => unreachable!("loaded as a rule"),
				};
				let to = rule.to().expect("loaded as a rule");
				let from = to.from.is_empty() || to.from.iter().any(|f| current.is_under(f.names()));
				let forward = !to.forward_only || ctx.workflow.order(to.to) > ctx.workflow.order(current);
				if holds && from && forward {
					out.move_to = Some(to.to.clone());
				}
			}
			(Moment::Entering { .. }, _) => unreachable!("loaded as an on_enter hook"),
		}
		Ok(())
	}
}

fn leading(card: &tedi_core::board::Card) -> Option<PrState> {
	if let Some(pr) = &card.pr {
		return Some(pr.state);
	}
	[PrState::Open, PrState::Draft, PrState::Merged, PrState::Closed]
		.into_iter()
		.find(|s| card.prs.iter().any(|p| p.state == *s))
}
