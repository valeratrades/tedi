//! `tedi view`: a query over the store, opened as a task view — one `#` section per group.

use color_eyre::eyre::{Result, bail};
use tedi_task_operations::{
	board::{self, Board, GroupBy},
	open_interactions::SyncMode,
	reach::Reach,
};

use crate::config::{LiveSettings, States};

#[derive(clap::Args)]
pub struct ViewArgs {
	/// A view named in the config's `board.views` (the home's, with one), or a query: `state:kanban/triage,kanban/selected t:bug i:>=5 repo:owner/* text`.
	query: Vec<String>,
	/// Split into one section per value; a named view brings its own.
	#[arg(long)]
	group: Option<GroupBy>,
}

pub async fn view_command(settings: &LiveSettings, args: ViewArgs, mode: SyncMode) -> Result<()> {
	let named = args.query.join(" ");
	let listing = match settings.config()?.states()? {
		States::Home(home) => home.view(&named, args.group.as_ref()).await?,
		States::Here(config) => {
			let workflows = config.workflows()?;
			let mut reading = board::load(Reach::All, &workflows.trees)?;
			let mut board = Board::default();
			while let Some(part) = reading.next().await? {
				board.extend([part]);
			}
			for (path, e) in &board.unreadable {
				eprintln!("unreadable, left out: {}: {}", path.display(), e.lines().next().unwrap_or_default());
			}
			let me = tedi_task_operations::current_user::get();
			board::listing(&board, &named, args.group, &config.views, &workflows.trees, me.as_deref())?
		}
		States::Nowhere => bail!("`tedi view` reads the config's `board` (`workflows`, `views`), or asks its `home`"),
	};
	if listing.pull_requests > 0 {
		eprintln!("{} pull requests match, left out: a task view holds issues", listing.pull_requests);
	}
	let mut content = String::new();
	for (key, links) in &listing.groups {
		if let Some(key) = key {
			content.push_str(&format!("# {key}\n"));
		}
		for link in links {
			content.push_str(&format!("- {link}\n"));
		}
	}
	if content.is_empty() {
		println!("Nothing matches `{}`", listing.query);
		return Ok(());
	}
	tedi_task_operations::tabs::edit_view(&content, mode).await
}
