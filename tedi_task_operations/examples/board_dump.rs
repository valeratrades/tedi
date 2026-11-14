//! `repo#n<TAB>stage<TAB>rank` for every card in the store under `$XDG_DATA_HOME`.

#[tokio::main]
async fn main() -> color_eyre::Result<()> {
	let board = tedi_task_operations::board::load(jiff::SignedDuration::from_hours(24 * 7), jiff::Timestamp::now()).await?;
	for (path, e) in &board.unreadable {
		eprintln!("unreadable {}: {}", path.display(), e.lines().next().unwrap_or_default());
	}
	for c in board.cards {
		let link = &c.link;
		println!("{}#{}\t{}\t{}", link.project(), link.number(), c.stage, c.rank.map_or_else(String::new, |r| r.to_string()));
	}
	Ok(())
}
