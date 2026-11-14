//! `tedi web`: this store, served as the board.

use std::{net::SocketAddr, path::PathBuf};

use color_eyre::eyre::{Result, bail, eyre};
use tedi_task_operations::{RepoInfo, open_interactions::SyncMode};

use crate::config::LiveSettings;

#[derive(clap::Args)]
pub struct WebArgs {
	#[arg(long)]
	addr: SocketAddr,
	/// The client bundle (`dx bundle -p tedi_web` output). Without it only the API is served, as `dx serve` wants.
	#[arg(long)]
	public: Option<PathBuf>,
	/// Pull the config's `watch` this often (`10m`), catching what no webhook announced.
	#[arg(long)]
	pull_every: Option<jiff::Span>,
}

pub async fn web_command(settings: &LiveSettings, args: WebArgs, mode: SyncMode) -> Result<()> {
	let config = settings.config()?;
	let board = config.board.ok_or_else(|| eyre!("`tedi web` reads the config's `board` (`archive_after`, `views`, `scopes`)"))?;
	let pull_every = match args.pull_every {
		Some(every) => Some(every.to_duration(jiff::SpanRelativeTo::days_are_24_hours())?.unsigned_abs()),
		None => None,
	};
	let watch = match (config.watch, pull_every, &config.github_webhook_secret) {
		(Some(watch), ..) => watch,
		(None, Some(_), _) => bail!("--pull-every pulls the config's `watch`, and there is none"),
		(None, _, Some(_)) => bail!("a webhook delivery is pulled only for what the config's `watch` names, and there is none"),
		(None, None, None) => Vec::new(),
	};
	let milestones = match &config.milestones {
		Some(m) => {
			let (owner, repo) = crate::sprints::parse_github_repo(&m.url)?;
			Some(RepoInfo::new(&owner, &repo))
		}
		None => None,
	};
	tedi_web::serve(tedi_web::Settings {
		addr: args.addr,
		public: args.public,
		mode,
		archive_after: board.archive_after()?,
		views: board.views,
		scopes: board.scopes,
		milestones,
		watch,
		pull_every,
		webhook_secret: config.github_webhook_secret,
	})
	.await
}
