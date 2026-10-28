//! skim with our query engine swapped in for its own.

use std::{borrow::Cow, fmt, rc::Rc, sync::Arc};

use skim::{
	CaseMatching, ItemPreview, MatchEngine, MatchEngineFactory, MatchRange, MatchResult, PreviewContext, PreviewPosition, Rank, RankCriteria, Skim, SkimItem,
	item::RankBuilder,
	matcher::Matcher,
	prelude::{SkimOptionsBuilder, unbounded},
	tui::Size,
};

use super::{
	SearchError, SearchKind,
	query::{Prepared, Query},
	with_matcher,
};

pub(super) async fn run(prepared: Vec<Prepared>, kind: SearchKind, query: &str) -> Result<Option<usize>, SearchError> {
	let options = SkimOptionsBuilder::default()
		.query(query.to_owned())
		.preview(String::new()) // skim only asks items for a preview when a global one is set
		.bind(vec!["ctrl-r:ignore".to_owned()]) // rotate-mode rebuilds skim's own engine over ours
		.build()
		.expect("static options");
	let (tx, rx) = unbounded();
	tx.send(prepared.into_iter().map(|p| Arc::new(Item { prepared: p, kind }) as Arc<dyn SkimItem>).collect())
		.expect("receiver is held right here");
	drop(tx);

	let mut skim = Skim::init(options, Some(rx)).map_err(picker_error)?;
	// input order is the final tiebreak; skim's length/begin criteria would reintroduce path-length bias
	let rank_builder = Arc::new(RankBuilder::new(vec![RankCriteria::Score, RankCriteria::Index]));
	skim.app_mut().matcher = Matcher::builder(Rc::new(Factory(kind))).rank_builder(rank_builder).build();
	skim.start();
	skim.init_tui().map_err(picker_error)?;
	skim.enter().await.map_err(picker_error)?;
	skim.run().await.map_err(picker_error)?;
	let output = skim.output();
	if output.is_abort {
		return Err(SearchError::Aborted);
	}
	Ok(output.selected_items.first().map(|m| item(m.item.as_ref()).prepared.index))
}

fn picker_error(e: impl Into<Box<dyn std::error::Error + Send + Sync>>) -> SearchError {
	SearchError::Picker(e.into())
}

struct Item {
	prepared: Prepared,
	kind: SearchKind,
}
impl SkimItem for Item {
	fn text(&self) -> Cow<'_, str> {
		Cow::Borrowed(&self.prepared.line)
	}

	fn preview(&self, context: PreviewContext) -> ItemPreview {
		const HIGHLIGHT: &str = "\x1b[1;33m";
		const RESET: &str = "\x1b[0m";
		let body = &self.prepared.body;
		// an invalid in-progress regex has no hits to show; the row list already shows it matches nothing
		let highlights = Query::parse(self.kind, context.query).map(|q| q.body_highlights(body)).unwrap_or_default();
		let mut out = String::with_capacity(body.len());
		let mut first_hit_line = None;
		let (mut line, mut ranges) = (0usize, highlights.iter().peekable());
		let mut open_until = None;
		for (i, c) in body.chars().enumerate() {
			if open_until == Some(i) {
				out.push_str(RESET);
				open_until = None;
			}
			while ranges.next_if(|r| r.end <= i).is_some() {}
			if open_until.is_none()
				&& let Some(r) = ranges.next_if(|r| r.start <= i)
			{
				out.push_str(HIGHLIGHT);
				open_until = Some(r.end);
				first_hit_line.get_or_insert(line);
			}
			if c == '\n' {
				line += 1;
			}
			out.push(c);
		}
		if open_until.is_some() {
			out.push_str(RESET);
		}
		let v_scroll = u16::try_from(first_hit_line.unwrap_or(0).saturating_sub(2)).unwrap_or(u16::MAX);
		// `Size::default()` is 100%, so every offset is spelled out
		ItemPreview::AnsiWithPos(
			out,
			PreviewPosition {
				v_scroll: Size::Fixed(v_scroll),
				v_offset: Size::Fixed(0),
				h_scroll: Size::Fixed(0),
				h_offset: Size::Fixed(0),
			},
		)
	}
}

fn item(item: &dyn SkimItem) -> &Item {
	item.as_any().downcast_ref::<Item>().expect("the picker is only fed `Item`s")
}

struct Factory(SearchKind);
impl MatchEngineFactory for Factory {
	fn create_engine_with_case(&self, query: &str, _: CaseMatching) -> Box<dyn MatchEngine> {
		// an invalid regex while typing is transient state: it matches nothing
		Box::new(Engine(Query::parse(self.0, query).ok()))
	}
}

struct Engine(Option<Query>);
impl MatchEngine for Engine {
	fn match_item(&self, candidate: &dyn SkimItem) -> Option<MatchResult> {
		let query = self.0.as_ref()?;
		let hit = with_matcher(|m| query.score(&item(candidate).prepared, m))?;
		Some(MatchResult {
			rank: Rank {
				score: hit.score,
				..Default::default()
			},
			matched_range: MatchRange::Chars(hit.line_indices),
		})
	}
}
impl fmt::Display for Engine {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		f.write_str("tedi_utils::search")
	}
}
