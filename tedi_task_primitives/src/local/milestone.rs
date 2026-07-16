//! Local filesystem storage for milestones — the durable sibling of issue storage.
//!
//! Layout (inside the issues git repo, so consensus + conflict come for free):
//! ```text
//! {issues_dir}/__milestones__/{owner}/{repo}/
//!   .meta.json                    # MilestoneProjectMeta: num → identity fields
//!   {number}_-_{title}.md         # the milestone body (prose + hosted `- <url>` lines)
//! ```
//! A milestone owns no directory tree — hosted issues are references stored in their
//! own `issues/` files. Identity (title/state/due_on/timestamps) lives in `.meta.json`;
//! the `.md` file holds only the body (`Milestone::Display`).

use std::{collections::BTreeMap, path::PathBuf};

use jiff::Timestamp;
use serde::{Deserialize, Serialize};
use tedi_core::{CloseState, MilestoneBody, MilestoneIdentity, MilestoneLink, MilestoneTimestamps};
use v_utils::prelude::*;

use super::{Consensus, FsReader, Local, LocalFs, LocalReader};
use crate::{Milestone, RepoInfo, sink::Sink};

/// Sentinel subdir under `issues_dir()` that scopes milestone storage without colliding
/// with any GitHub owner (owners can't contain `_`).
// ponytail: nesting under issues_dir keeps milestones in the existing git repo; a
// top-level `milestones/` dir would sit outside it and lose consensus/conflict.
const MILESTONES_SUBDIR: &str = "__milestones__";

/// Per-milestone identity metadata stored in the project `.meta.json`.
#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct MilestoneMeta {
	pub title: String,
	pub state: CloseState,
	#[serde(default, skip_serializing_if = "Option::is_none")]
	pub due_on: Option<Timestamp>,
	#[serde(default)]
	pub timestamps: MilestoneTimestamps,
}

/// Project-level milestone metadata file (`{owner}/{repo}/.meta.json`).
#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct MilestoneProjectMeta {
	#[serde(default, skip_serializing_if = "BTreeMap::is_empty")]
	pub milestones: BTreeMap<u64, MilestoneMeta>,
}

impl Local {
	/// `{issues_dir}/__milestones__/` — root of milestone storage (inside the git repo).
	pub fn milestones_dir() -> PathBuf {
		Self::issues_dir().join(MILESTONES_SUBDIR)
	}

	/// `{milestones_dir}/{owner}/{repo}/`
	pub fn milestone_project_dir(repo: RepoInfo) -> PathBuf {
		Self::milestones_dir().join(repo.owner().expect("milestones are github-only")).join(repo.repo())
	}

	fn milestone_meta_path(repo: RepoInfo) -> PathBuf {
		Self::milestone_project_dir(repo).join(".meta.json")
	}

	/// Deterministic body-file path `{project}/{number}_-_{title}.md` for writes.
	pub fn milestone_file_path(repo: RepoInfo, number: u64, title: &str) -> PathBuf {
		Self::milestone_project_dir(repo).join(tedi_core::issue_file_name(Some(number), title, false))
	}

	/// Find an existing milestone body file by number via `reader` (fs or git HEAD).
	pub fn find_milestone_path<R: LocalReader>(repo: RepoInfo, number: u64, reader: &R) -> Option<PathBuf> {
		let dir = Self::milestone_project_dir(repo);
		let entries = reader.list_dir(&dir).ok()?;
		let prefix = format!("{number}_-_");
		let name = entries.into_iter().find(|n| n.starts_with(&prefix) && n.ends_with(".md"))?;
		Some(dir.join(name))
	}

	pub fn load_milestone_project_meta<R: LocalReader>(repo: RepoInfo, reader: &R) -> MilestoneProjectMeta {
		let path = Self::milestone_meta_path(repo);
		match reader.read_content(&path) {
			Ok(c) if c.trim().is_empty() => MilestoneProjectMeta::default(),
			Ok(c) => serde_json::from_str(&c).unwrap_or_else(|e| panic!("corrupted milestone metadata at {}: {e}", path.display())),
			Err(e) if e.is_not_found() => MilestoneProjectMeta::default(),
			Err(e) => panic!("failed to read milestone metadata at {}: {e}", path.display()),
		}
	}

	fn save_milestone_project_meta(repo: RepoInfo, meta: &MilestoneProjectMeta) -> std::io::Result<()> {
		let path = Self::milestone_meta_path(repo);
		if let Some(parent) = path.parent() {
			std::fs::create_dir_all(parent)?;
		}
		let content = serde_json::to_string_pretty(meta).expect("MilestoneProjectMeta serialization is infallible");
		let tmp = path.with_extension("json.tmp");
		std::fs::write(&tmp, &content)?;
		std::fs::rename(&tmp, &path)
	}

	fn save_milestone_meta(repo: RepoInfo, number: u64, meta: &MilestoneMeta) -> std::io::Result<()> {
		let mut project = Self::load_milestone_project_meta(repo, &FsReader);
		project.milestones.insert(number, meta.clone());
		Self::save_milestone_project_meta(repo, &project)
	}

	/// Load a milestone from a reader (filesystem or git HEAD). `Ok(None)` if absent.
	pub fn load_milestone<R: LocalReader>(link: &MilestoneLink, reader: &R) -> Result<Option<Milestone>> {
		let repo = link.repo_info();
		let number = link.number();
		let Some(path) = Self::find_milestone_path(repo, number, reader) else {
			return Ok(None);
		};
		let content = reader.read_content(&path).map_err(|e| eyre!("{e}"))?;
		let body = MilestoneBody::parse(&content);
		let meta = Self::load_milestone_project_meta(repo, reader).milestones.remove(&number).unwrap_or_default();
		Ok(Some(Milestone {
			identity: MilestoneIdentity {
				link: link.clone(),
				state: meta.state,
				due_on: meta.due_on,
				title: meta.title,
				timestamps: meta.timestamps,
			},
			body,
		}))
	}
}

/// Marker filesystem sink: write the milestone body file + identity metadata.
impl Sink<LocalFs> for Milestone {
	type Error = std::io::Error;

	async fn sink(&mut self, old: Option<&Milestone>) -> Result<bool, Self::Error> {
		let repo = self.identity.link.repo_info();
		let number = self.number();
		let target = Local::milestone_file_path(repo, number, &self.identity.title);

		// Remove a stale file if the title (and thus filename) changed.
		if let Some(old) = old
			&& old.identity.title != self.identity.title
			&& let Some(old_path) = Local::find_milestone_path(repo, number, &FsReader)
			&& old_path != target
		{
			let _ = std::fs::remove_file(old_path);
		}

		let content = self.to_string();
		let changed = match old {
			Some(o) => o.to_string() != content || o.identity != self.identity,
			None => true,
		};
		std::fs::create_dir_all(target.parent().expect("milestone file always has a parent"))?;
		std::fs::write(&target, format!("{content}\n"))?;

		Local::save_milestone_meta(
			repo,
			number,
			&MilestoneMeta {
				title: self.identity.title.clone(),
				state: self.identity.state.clone(),
				due_on: self.identity.due_on,
				timestamps: self.identity.timestamps.clone(),
			},
		)?;

		Ok(changed)
	}
}

/// Consensus (git) sink: stage + commit. File writing is `Sink<LocalFs>`'s job.
impl Sink<Consensus> for Milestone {
	type Error = crate::local::ConsensusSinkError;

	async fn sink(&mut self, _old: Option<&Milestone>) -> Result<bool, Self::Error> {
		use std::process::Command;

		let repo = self.identity.link.repo_info();
		let data_dir = Local::issues_dir();
		let data_dir_str = data_dir.to_str().ok_or_else(crate::local::ConsensusSinkError::new_invalid_data_dir)?;

		let add = Command::new("git").args(["-C", data_dir_str, "add", "-A"]).output()?;
		if !add.status.success() {
			return Err(crate::local::ConsensusSinkError::new_git_add(String::from_utf8_lossy(&add.stderr).into_owned()));
		}
		let diff = Command::new("git").args(["-C", data_dir_str, "diff", "--cached", "--quiet"]).status()?;
		if diff.success() {
			return Ok(false);
		}
		let msg = format!("sync milestone: {repo}#{}", self.number());
		let commit = Command::new("git").args(["-C", data_dir_str, "commit", "-m", &msg]).output()?;
		if !commit.status.success() {
			return Err(crate::local::ConsensusSinkError::new_git_commit(String::from_utf8_lossy(&commit.stderr).into_owned()));
		}
		Ok(true)
	}
}
