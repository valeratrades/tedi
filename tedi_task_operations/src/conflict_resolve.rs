//! Conflict resolution: the operation-layer counterpart to `conflict_detect`.
//!
//! A divergence is recorded as a file holding both sides between markers, one per issue tree (named
//! by the root the sync ran from) or milestone. Resolving it is removing the markers: the file is then
//! taken as the new state, sunk to GitHub, local and consensus. Until then it blocks only its own node.

use std::path::{Path, PathBuf};

use color_eyre::eyre::{Result, WrapErr};

use crate::{
	Issue, IssueIndex, IssueLink, LazyIssue as _, VirtualIssue,
	local::{
		Consensus, FsReader, GitReader, Local, LocalFs, LocalIssueSource, LocalPath,
		conflict_detect::{ConflictBlockedError, conflicts_on_line, has_conflict_markers, milestone_conflict_path},
		consensus::load_consensus_issue,
	},
	open_interactions::{apply_edited_buffer, fetch_remote},
	remote::{Remote, RemoteSource},
	sink::Sink,
};

/// A sync that can't settle: each field both sides changed since consensus, every clashing node recorded
/// to a file of its own for resolution. Blocks only the line of issues those nodes sit on.
#[derive(Debug, miette::Diagnostic, thiserror::Error)]
#[diagnostic(code(tedi::conflict))]
pub struct Conflict {
	pub link: IssueLink,
	pub clashes: Vec<Clash>,
	pub files: Vec<PathBuf>,
}
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Clash {
	pub node: IssueLink,
	pub field: Field,
	/// `None` when there is no consensus: never synced from here.
	pub consensus: Option<String>,
	pub local: String,
	pub remote: String,
}
#[derive(Clone, Copy, Debug, Eq, PartialEq, strum::Display)]
#[strum(serialize_all = "snake_case")]
pub enum Field {
	Title,
	Labels,
	Assignees,
	Body,
	State,
	Comments,
	SubIssues,
}
impl std::fmt::Display for Conflict {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		let home = self.link.project();
		writeln!(f, "{home}#{} changed on both sides since the last sync:", self.link.number())?;
		for c in &self.clashes {
			// the first line the sides differ on
			let at = c.local.lines().zip(c.remote.lines()).take_while(|(l, r)| l == r).count();
			let line = |v: &str| v.lines().nth(at).map_or_else(|| "nothing".to_string(), |l| format!("{:?}", l.trim()));
			write!(f, "  {} {}: {} here, {} on GitHub", c.node.shorthand(home), c.field, line(&c.local), line(&c.remote))?;
			match &c.consensus {
				Some(was) => writeln!(f, " (was {})", line(was))?,
				None => writeln!(f)?,
			}
		}
		writeln!(f, "Resolve the markers (<<<<<<< ||||||| ======= >>>>>>>) in:")?;
		for file in &self.files {
			writeln!(f, "  {}", file.display())?;
		}
		write!(f, "then re-run.")
	}
}

/// Load a local source at `index`, first resolving any pending conflict via the editor.
/// Use at user-facing load sites; ancestor/internal loads use the pure `LocalIssueSource::build`.
pub async fn build_resolving(index: IssueIndex) -> Result<LocalIssueSource<FsReader>> {
	resolve_pending_conflict(index).await?;
	Ok(LocalIssueSource::<FsReader>::build(LocalPath::new(index)).await?)
}

/// `build_resolving` for a filesystem path (extracts the index first).
pub async fn build_resolving_from_path(path: &Path) -> Result<LocalIssueSource<FsReader>> {
	let index = Local::extract_index_from_path(path)?;
	build_resolving(index).await
}

/// If a pending conflict blocks `index`, open it for resolution and re-check.
/// Errors with `ConflictBlockedError` if markers still remain after the editor exits.
pub async fn resolve_pending_conflict(index: IssueIndex) -> Result<()> {
	refresh_pending_conflict(index).await?;
	if let Some(conflict_file) = check_for_existing_conflict(index).await? {
		eprintln!("Unresolved merge conflict in: {}", conflict_file.display());
		eprintln!("Opening for resolution...");
		let modified = v_utils::io::file_open::open(&conflict_file).await?;
		if !modified {
			return Err(ConflictBlockedError::new(conflict_file).into());
		}
		// Re-check after user exits editor
		if let Some(conflict_file) = check_for_existing_conflict(index).await? {
			return Err(ConflictBlockedError::new(conflict_file).into());
		}
	}
	Ok(())
}

/// Write `path` holding both sides between markers, the remote's kept aside to tell whether GitHub
/// moved on before it is resolved.
pub fn record_conflict(path: &Path, local: &str, consensus: Option<&str>, remote: &str) -> Result<()> {
	std::fs::create_dir_all(path.parent().expect("conflicts live in a repo dir"))?;
	let side = |s: &str| match s.ends_with('\n') {
		true => s.to_string(),
		false => format!("{s}\n"),
	};
	std::fs::write(
		path,
		format!(
			"<<<<<<< local\n{}||||||| consensus\n{}=======\n{}>>>>>>> remote\n",
			side(local),
			consensus.map(side).unwrap_or_default(),
			side(remote)
		),
	)?;
	std::fs::write(remote_snapshot(path), remote)?;
	Ok(())
}

/// A reset took one side whole: nothing on its line is left to resolve.
pub fn discard_conflicts(index: IssueIndex) -> Result<()> {
	for (_, path) in conflicts_on_line(index)? {
		std::fs::remove_file(remote_snapshot(&path))?;
		std::fs::remove_file(path)?;
	}
	Ok(())
}

fn remote_snapshot(path: &Path) -> PathBuf {
	path.with_extension("remote")
}

/// `Some(path)` while markers remain on `index`'s line. A file whose markers are gone is the
/// resolution of its node: sunk to GitHub, local and consensus, and that conflict is over.
pub async fn check_for_existing_conflict(index: IssueIndex) -> Result<Option<PathBuf>> {
	for (node, path) in conflicts_on_line(index)? {
		let content = std::fs::read_to_string(&path)?;
		match has_conflict_markers(&content) {
			true => return Ok(Some(path)),
			false => land_resolution(node, &path, &content).await?,
		}
	}
	Ok(None)
}

async fn land_resolution(issue_index: IssueIndex, conflict_fpath: &Path, content: &str) -> Result<()> {
	let mut new_issue = {
		let mut new_issue_but_old_local_timestamps = {
			let virtual_issue = VirtualIssue::parse(content, conflict_fpath.to_path_buf())?;
			// the resolution holds children as links only; their subtrees come from storage
			let local = Issue::load(LocalIssueSource::<FsReader>::build(LocalPath::from(issue_index)).await?).await?;
			let mut folded = local.clone();
			apply_edited_buffer(&mut folded, &local, virtual_issue.clone()).wrap_err_with(|| format!("{} can't be taken as the resolution", conflict_fpath.display()))?;
			let hollow = Local::read_hollow_from_project_meta(issue_index)?;
			let mut resolved = Issue::from_combined(hollow, virtual_issue, issue_index.parent().unwrap(), issue_index.repo_info().is_virtual())?;
			resolved.children = folded.children;
			resolved
		};

		let last_consensus_issue = Issue::load(LocalIssueSource::<GitReader>::build(LocalPath::from(issue_index))?).await?;

		new_issue_but_old_local_timestamps.post_update(&last_consensus_issue);
		new_issue_but_old_local_timestamps
	};

	let remote = fetch_remote(&new_issue).await?;
	<Issue as Sink<Remote>>::sink(&mut new_issue, Some(&remote)).await?;
	<Issue as Sink<LocalFs>>::sink(&mut new_issue, None).await?; // after the remote sink: it records the links Github now holds
	<Issue as Sink<Consensus>>::sink(&mut new_issue, None).await?;
	std::fs::remove_file(remote_snapshot(conflict_fpath))?;
	std::fs::remove_file(conflict_fpath)?;
	Ok(())
}

/// A conflict still carrying markers is re-raised against a fresh pull if Github moved since it was
/// written, so the user never resolves against a stale remote. Unchanged remote keeps the file as is.
async fn refresh_pending_conflict(index: IssueIndex) -> Result<()> {
	for (node, path) in conflicts_on_line(index)? {
		if has_conflict_markers(&std::fs::read_to_string(&path)?) {
			refresh(node, &path).await?;
		}
	}
	Ok(())
}

async fn refresh(issue_index: IssueIndex, conflict_fpath: &Path) -> Result<()> {
	let recorded_remote = std::fs::read_to_string(remote_snapshot(conflict_fpath))?;
	let num_path = issue_index.git_num_path()?;
	let (&number, lineage) = num_path.split_last().expect("a github issue index ends in its number");
	let remote = Issue::load(RemoteSource::build(IssueLink::in_project(issue_index.repo_info(), number), Some(lineage))?).await?;
	let remote_side = remote.to_string();
	if remote_side == recorded_remote {
		return Ok(());
	}
	std::fs::remove_file(conflict_fpath)?; // the stale one would block reading the local side; a sync re-raises it if this fails midway
	let local = Issue::load(LocalIssueSource::<FsReader>::build(LocalPath::new(issue_index)).await?).await?;
	let consensus = load_consensus_issue(issue_index).await?;
	record_conflict(conflict_fpath, &local.to_string(), consensus.map(|c| c.to_string()).as_deref(), &remote_side)
}

/// Milestone counterpart to `check_for_existing_conflict`: if the resolution file's markers
/// are gone, rebuild the milestone from it (identity from meta, timestamps bumped against
/// consensus) and sink to local + remote, then clean up.
pub async fn check_for_existing_milestone_conflict(link: &crate::MilestoneLink) -> Result<Option<PathBuf>> {
	let conflict_fpath = milestone_conflict_path(link);
	if !conflict_fpath.exists() {
		return Ok(None);
	}
	let content = std::fs::read_to_string(&conflict_fpath)?;
	if has_conflict_markers(&content) {
		return Ok(Some(conflict_fpath));
	}

	let mut milestone = {
		let body = crate::MilestoneBody::parse(&content, link.repo_info());
		let meta = Local::load_milestone_project_meta(link.repo_info(), &FsReader)
			.milestones
			.remove(&link.number())
			.unwrap_or_default();
		let mut m = crate::Milestone {
			identity: crate::MilestoneIdentity {
				link: link.clone(),
				state: meta.state,
				due_on: meta.due_on,
				title: meta.title,
				timestamps: meta.timestamps,
			},
			body,
		};
		if let Some(consensus) = Local::load_milestone(link, &GitReader)? {
			m.post_update(&consensus);
		}
		m
	};

	<crate::Milestone as Sink<LocalFs>>::sink(&mut milestone, None).await?;
	<crate::Milestone as Sink<Remote>>::sink(&mut milestone, None).await?;
	<crate::Milestone as Sink<Consensus>>::sink(&mut milestone, None).await?;
	std::fs::remove_file(remote_snapshot(&conflict_fpath))?;
	std::fs::remove_file(&conflict_fpath)?;
	Ok(None)
}
