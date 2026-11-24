//! `tedi web`: this store, served as the board.

use std::{net::SocketAddr, path::PathBuf};

use color_eyre::eyre::{Result, bail, eyre};
use tedi_task_operations::open_interactions::SyncMode;

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
	/// The header a gate in front of this board names the signed-in user in (`X-Auth-User`): each keeps their own library.
	/// Without it the board is ungated, and the library is the instance's GitHub user's.
	#[arg(long)]
	viewer_header: Option<String>,
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
	tedi_web::serve(tedi_web::Settings {
		addr: args.addr,
		public: args.public,
		mode,
		workflows: board.workflows()?,
		views: board.views,
		scopes: board.scopes,
		columns: board.columns,
		workspaces: board.workspaces,
		source: board.source,
		watch,
		pull_every,
		webhook_secret: config.github_webhook_secret,
		viewer: match args.viewer_header {
			Some(header) => tedi_web::Viewer::Header(header),
			None => tedi_web::Viewer::Operator,
		},
	})
	.await
}
