//! What makes a milestone a sprint: its title alone, whoever's repo it sits in.

use v_utils::{Timeframe, TimeframeDesignator};

/// A collection of musts, by what they are a must by. In the order they are shown in: the shortest period first.
#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub enum Sprint {
	/// Before the period rolls over.
	Planning(Timeframe),
	/// First.
	Bottlenecks,
}
impl Sprint {
	/// What the milestone titled `title` is a sprint of, if it is one.
	pub fn titled(title: &str) -> Option<Self> {
		title.parse::<Self>().ok().filter(|s| s.to_string() == title) //IGNORED_ERROR: any other milestone's title
	}
}
impl std::fmt::Display for Sprint {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::Planning(tf) => tf.fmt(f),
			Self::Bottlenecks => f.write_str("Bottlenecks"),
		}
	}
}
/// As a person names one: `1q` is `1Q`, `bottlenecks` is `Bottlenecks`.
impl std::str::FromStr for Sprint {
	type Err = SprintError;

	fn from_str(s: &str) -> Result<Self, SprintError> {
		if s.eq_ignore_ascii_case("bottlenecks") {
			return Ok(Self::Bottlenecks);
		}
		match s.parse::<Timeframe>() {
			Ok(Timeframe(0)) | Err(_) => Err(SprintError::Unknown(s.into())),
			Ok(tf) => match tf.designator() {
				TimeframeDesignator::Milliseconds | TimeframeDesignator::Seconds | TimeframeDesignator::Minutes => Err(SprintError::Minutes(s.into())),
				TimeframeDesignator::Hours
				| TimeframeDesignator::Days
				| TimeframeDesignator::Weeks
				| TimeframeDesignator::IfcMonths
				| TimeframeDesignator::IfcQuarters
				| TimeframeDesignator::Years => Ok(Self::Planning(tf)),
			},
		}
	}
}

#[derive(Debug, thiserror::Error)]
pub enum SprintError {
	#[error("{0:?} is no sprint: a period (`1d`, `2w`, `1Q`, …) or `Bottlenecks`")]
	Unknown(String),
	#[error("{0:?} is a minute-designated period (`m`); monthly is `M`")]
	Minutes(String),
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn titles() {
		let of = |t: &str| Sprint::titled(t).map(|s| s.to_string());
		let mut shown: Vec<Sprint> = ["Bottlenecks", "1Q", "2w", "7y", "1d"].iter().filter_map(|t| Sprint::titled(t)).collect();
		shown.sort();
		insta::assert_snapshot!(format!("{:?}\n{:?}", shown.iter().map(ToString::to_string).collect::<Vec<_>>(), ["bottlenecks", "1q", "2", "5m", "0d", "v1.0", "2026/01/02_1d", ""].map(of)), @r#"
		["1d", "2w", "1Q", "7y", "Bottlenecks"]
		[None, None, None, None, None, None, None, None]
		"#);
	}
}
