//! `tedi view`: a query over the store, opened as a task view — one `#` section per group — through
//! the sprint flow's expand-and-fold-back. A view stores nothing: only the issues it expanded change.

use color_eyre::eyre::{Result, eyre};
use tedi_task_operations::{
	TaskView,
	board::{self, GroupBy, Query},
	open_interactions::SyncMode,
	sprints::{expand_and_refresh, materialize_new_tasks, sync_embedded_issue_changes},
};

use crate::config::LiveSettings;

#[derive(clap::Args)]
pub struct ViewArgs {
	/// A view named in the config's `board.views`, or a query: `stage:triage,selected t:bug i:>=5 repo:owner/* text`.
	query: Vec<String>,
	/// Split into one section per value; a named view brings its own.
	#[arg(long)]
	group: Option<GroupBy>,
}

pub async fn view_command(settings: &LiveSettings, args: ViewArgs, mode: SyncMode) -> Result<()> {
	let config = settings.config()?;
	let board_config = config.board.ok_or_else(|| eyre!("`tedi view` reads the config's `board` (`archive_after`, `views`)"))?;
	let named = args.query.join(" ");
	let (query, group) = match board_config.views.iter().find(|v| v.name == named) {
		Some(view) => (view.query.clone(), args.group.unwrap_or(view.group_by)),
		None => (named, args.group.unwrap_or(GroupBy::Nothing)),
	};

	let board = board::load(board_config.archive_after()?, jiff::Timestamp::now()).await?;
	for (path, e) in &board.unreadable {
		eprintln!("unreadable, left out: {}: {}", path.display(), e.lines().next().unwrap_or_default());
	}
	let me = tedi_task_operations::current_user::get();
	let hits = query.parse::<Query>()?.run(&board.cards, me.as_deref())?;
	let mut content = String::new();
	for (key, members) in board::group(&board.cards, &hits, group).into_iter().filter(|(_, m)| !m.is_empty()) {
		if group != GroupBy::Nothing {
			content.push_str(&format!("# {key}\n"));
		}
		for i in members {
			content.push_str(&format!("- {}\n", board.cards[i].link));
		}
	}
	if content.is_empty() {
		println!("Nothing matches `{query}`");
		return Ok(());
	}

	let expanded = expand_and_refresh(TaskView::parse(&content), mode == SyncMode::Full).await?;
	let tmp_path = tempfile::tempdir()?.keep().join("view.md");
	std::fs::write(&tmp_path, &expanded)?;
	eprintln!("[view] tmp_path: {}", tmp_path.display());
	tedi_task_operations::utils::open_file(&tmp_path, None).await?;
	let _syncing = v_utils::io::Spinner::start(); // until the edit is synced

	let raw = std::fs::read_to_string(&tmp_path)?;
	if raw == expanded {
		return sync_embedded_issue_changes(&raw, mode).await; // an issue a killed run left unsynced has nothing else to carry it
	}
	let fold_back = async {
		let (edited, moves) = crate::sprints::take_moves(&raw, mode)?;
		if !moves.is_empty() {
			return Err(eyre!("`!mv` repoints the sprints that host an issue; move it from one of those, or with `tedi open`"));
		}
		sync_embedded_issue_changes(&edited, mode).await?;
		materialize_new_tasks(&mut TaskView::parse(&edited), None, mode).await?;
		Ok(())
	};
	match fold_back.await {
		Ok(()) => Ok(()),
		Err(e) => crate::sprints::dump_rejected(&raw, e),
	}
}
