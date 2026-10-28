//! The `-e` flag every issue-searching command shares.

use color_eyre::eyre::{Report, eyre};
use tedi_utils::search::SearchKind;

#[derive(Clone, Copy, Debug, clap::Args)]
pub struct SearchArgs {
	/// Matching mode, by count. Default: fuzzy terms, ANDed; `a | b` ORs; `'x` substring,
	/// `'x'` whole word, `^x`/`x$` anchors, `!x` excludes by title, `!!x` by any field.
	/// -e: exact terms (`'x` flips one back to fuzzy)
	/// -ee: one regex over the displayed row (substring match)
	/// -eee: the same, anchored to the full row
	#[arg(short = 'e', long, action = clap::ArgAction::Count)]
	exact: u8,
}
impl TryFrom<SearchArgs> for SearchKind {
	type Error = Report;

	fn try_from(args: SearchArgs) -> Result<Self, Report> {
		SearchKind::try_from(args.exact).map_err(|e| eyre!(e))
	}
}
