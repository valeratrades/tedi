//! Conflict detection: where a pending conflict lives, and the pure "is one pending?" query.
//!
//! Resolution (opening the editor, sinking the resolved state) lives in the operation layer
//! (`tedi_task_operations::conflict_resolve`). This module only *detects*.
//!
//! A conflict is pending per node — an issue tree synced from its root, or a milestone — in its own
//! file under `{issues_dir}/.conflicts/`, outside what consensus commits, so one clash never blocks
//! anything else.

use std::path::PathBuf;

use color_eyre::eyre::Result;
use miette::Diagnostic;
use thiserror::Error;
use v_utils::macros::wrap_err;

use super::Local;
use crate::{IssueIndex, IssueSelector, MilestoneLink, RepoInfo};

/// Error returned when an unresolved conflict blocks an operation.
#[wrap_err]
#[derive(Debug, Diagnostic, Error)]
#[error("Unresolved merge conflict")]
#[diagnostic(
	code(tedi::conflict::unresolved),
	help("Resolve the conflict markers (<<<<<<< ||||||| ======= >>>>>>>) in:\n  {0}\nthen re-run.", conflict_file.display())
)]
pub struct ConflictBlockedError {
	pub conflict_file: PathBuf,
}

/// Relative to the issues dir; consensus commits never take it.
pub const CONFLICTS_DIR: &str = ".conflicts";

fn repo_conflicts_dir(repo: RepoInfo) -> PathBuf {
	Local::issues_dir().join(CONFLICTS_DIR).join(repo.owner().expect("github project")).join(repo.repo())
}

/// Named by the node's lineage, `{root}_{…}_{n}.md`: a pending conflict blocks its whole line,
/// ancestors (whose sync carries it) and descendants alike.
pub fn conflict_path(index: IssueIndex) -> PathBuf {
	let lineage = index.git_num_path().expect("a node that synced is numbered all the way");
	repo_conflicts_dir(index.repo_info()).join(format!("{}.md", lineage.iter().map(u64::to_string).collect::<Vec<_>>().join("_")))
}

/// Every conflict file on `index`'s line, with the node it holds, ancestors first.
pub fn conflicts_on_line(index: IssueIndex) -> Result<Vec<(IssueIndex, PathBuf)>> {
	if index.repo_info().is_virtual() {
		return Ok(Vec::new());
	}
	let numbered: Vec<u64> = index
		.index()
		.iter()
		.map_while(|s| match s {
			IssueSelector::GitId(n) => Some(*n),
			_ => None,
		})
		.collect();
	let dir = repo_conflicts_dir(index.repo_info());
	let entries = match std::fs::read_dir(&dir) {
		Ok(entries) => entries,
		Err(e) if e.kind() == std::io::ErrorKind::NotFound => return Ok(Vec::new()),
		Err(e) => return Err(e.into()),
	};
	let mut found = Vec::new();
	for entry in entries {
		let name = entry?.file_name();
		let Some(stem) = name.to_str().and_then(|n| n.strip_suffix(".md")) else { continue }; // the kept-aside remote sides
		let lineage: Vec<u64> = stem.split('_').map(|n| n.parse().expect("conflict files are named by lineage")).collect();
		let shared = lineage.iter().zip(&numbered).take_while(|(a, b)| a == b).count();
		if shared == lineage.len().min(numbered.len()) && shared > 0 {
			let node = IssueIndex::with_index(index.repo_info(), lineage.iter().map(|n| IssueSelector::GitId(*n)).collect());
			found.push((node, dir.join(&name)));
		}
	}
	found.sort_by_key(|(node, _)| node.index().len());
	Ok(found)
}

/// Apart from issue conflicts: no GitHub owner is named `__milestones__`.
pub fn milestone_conflict_path(link: &MilestoneLink) -> PathBuf {
	Local::issues_dir()
		.join(CONFLICTS_DIR)
		.join("__milestones__")
		.join(link.repo_info().owner().expect("github project"))
		.join(link.repo_info().repo())
		.join(format!("{}.md", link.number()))
}

/// A pending (unresolved) conflict blocking `index`: one still carrying markers on its line.
/// Pure — no editor, no sink. Virtual issues never sync, so they can never conflict.
pub fn pending_conflict(index: IssueIndex) -> Result<Option<PathBuf>> {
	for (_, path) in conflicts_on_line(index)? {
		if has_conflict_markers(&std::fs::read_to_string(&path)?) {
			return Ok(Some(path));
		}
	}
	Ok(None)
}

/// Check if file content contains git conflict markers.
pub fn has_conflict_markers(content: &str) -> bool {
	let has_ours = content.contains("<<<<<<<");
	let has_separator = content.contains("=======");
	let has_theirs = content.contains(">>>>>>>");
	has_ours && has_separator && has_theirs
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn test_has_conflict_markers() {
		// No markers
		assert!(!has_conflict_markers("# Normal issue\n\nSome body text."));

		// All three markers
		let content = r#"# Issue title

<<<<<<< HEAD
Local changes
=======
Remote changes
>>>>>>> remote-state
"#;
		assert!(has_conflict_markers(content));

		// Just separator (like markdown divider)
		assert!(!has_conflict_markers("# Issue\n\n=======\n\nSome divider"));

		// Two of three markers
		assert!(!has_conflict_markers("<<<<<<< HEAD\nSome text\n======="));
	}
}
