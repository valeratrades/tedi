//! Where each issue of a project sits in each workflow (`tedi_core::workflow`): tedi's own, kept beside
//! the project's issues and never on GitHub. An issue absent sits in the workflow's intake.
//!
//! r[local.sink-only-mutation]

use std::collections::BTreeMap;

use tedi_core::workflow::State;

use super::{Local, LocalFs};
use crate::{RepoInfo, sink::Sink};

const STATES_FILE: &str = ".states.json";

/// One project's, by workflow, then by issue (or pull request) number.
#[derive(Clone, Debug, PartialEq)]
pub struct States {
	pub repo: RepoInfo,
	pub held: BTreeMap<String, BTreeMap<u64, State>>,
}

impl Local {
	pub fn load_states(repo: RepoInfo) -> std::io::Result<States> {
		let path = Self::project_dir(repo).join(STATES_FILE);
		let held = match std::fs::read_to_string(&path) {
			Ok(c) => serde_json::from_str(&c).unwrap_or_else(|e| panic!("corrupted workflow states at {}: {e}", path.display())),
			Err(e) if e.kind() == std::io::ErrorKind::NotFound => BTreeMap::new(),
			Err(e) => return Err(e),
		};
		Ok(States { repo, held })
	}
}

/// r[local.sink-only-mutation]
impl Sink<LocalFs> for States {
	type Error = std::io::Error;

	async fn sink(&mut self, old: Option<&Self>) -> Result<bool, std::io::Error> {
		assert!(super::store_lock::store_locked(), "workflow states are written under `lock_store`");
		if old == Some(self) {
			return Ok(false);
		}
		self.held.retain(|_, issues| !issues.is_empty());
		let dir = Local::project_dir(self.repo);
		std::fs::create_dir_all(&dir)?;
		let path = dir.join(STATES_FILE);
		let tmp = path.with_extension("json.tmp");
		std::fs::write(&tmp, serde_json::to_string_pretty(&self.held).expect("states serialize"))?;
		std::fs::rename(&tmp, &path)?; // whole or not at all
		Ok(true)
	}
}
