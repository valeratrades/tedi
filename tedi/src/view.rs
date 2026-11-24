//! `tedi view`: a query over the store, opened as a task view — one `#` section per group.

use color_eyre::eyre::{Result, eyre};
use tedi_task_operations::{
	board::{self, Board, GroupBy, Query},
	open_interactions::SyncMode,
};

use crate::config::LiveSettings;

#[derive(clap::Args)]
pub struct ViewArgs {
	/// A view named in the config's `board.views`, or a query: `state:kanban/triage,kanban/selected t:bug i:>=5 repo:owner/* text`.
	query: Vec<String>,
	/// Split into one section per value; a named view brings its own.
	#[arg(long)]
	group: Option<GroupBy>,
}

pub async fn view_command(settings: &LiveSettings, args: ViewArgs, mode: SyncMode) -> Result<()> {
	let config = settings.config()?;
	let board_config = config.board.ok_or_else(|| eyre!("`tedi view` reads the config's `board` (`workflows`, `views`)"))?;
	let workflows = board_config.workflows()?;
	let named = args.query.join(" ");
	let (query, group) = match board_config.views.iter().find(|v| v.name == named) {
		Some(view) => (view.query.clone(), args.group.unwrap_or_else(|| view.group_by.clone())),
		None => (named, args.group.unwrap_or(GroupBy::Nothing)),
	};

	let mut reading = board::load(&workflows)?;
	let mut board = Board::default();
	while let Some(part) = reading.next().await? {
		board.extend([part]);
	}
	for (path, e) in &board.unreadable {
		eprintln!("unreadable, left out: {}: {}", path.display(), e.lines().next().unwrap_or_default());
	}
	let me = tedi_task_operations::current_user::get();
	let mut hits = query.parse::<Query>()?.run(&board.cards, me.as_deref())?;
	let issues = hits.len();
	hits.retain(|&i| board.cards[i].pr.is_none());
	if hits.len() < issues {
		eprintln!("{} pull requests match, left out: a task view holds issues", issues - hits.len());
	}
	let mut content = String::new();
	let trees = workflows.iter().map(|(name, w)| (name.clone(), w.workflow.clone())).collect();
	for (key, members) in board::group(&board.cards, &hits, &group, &trees)?.into_iter().filter(|(_, m)| !m.is_empty()) {
		if group != GroupBy::Nothing {
			content.push_str(&format!("# {key}\n"));
		}
		for p in members {
			content.push_str(&format!("- {}\n", board.cards[p.card].link));
		}
	}
	if content.is_empty() {
		println!("Nothing matches `{query}`");
		return Ok(());
	}

	tedi_task_operations::tabs::edit_view(&content, mode).await
}
