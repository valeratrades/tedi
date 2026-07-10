//! Touch mode: create or open issues from paths.

use std::path::PathBuf;

use v_utils::{macros::wrap_err, prelude::Report, utils::exit_on_error};

use super::command::ProjectType;
use crate::{
	IssueIndex, IssueSelector, RepoInfo, github,
	local::{FsReader, Local, LocalError, LocalIssueSource, LocalPath, LocalPathError, LocalPathErrorKind},
};

#[wrap_err]
#[derive(Debug, thiserror::Error)]
pub enum TouchError {
	#[own]
	Path(LocalPathError),

	#[own]
	Local(LocalError),

	#[leaf]
	#[error("'{input}' matches no existing issue, and creating one requires at least owner/repo/title")]
	CreationPathTooShort { input: String },

	#[leaf]
	#[error("Repository '{owner}/{repo}' doesn't exist locally and is not accessible on GitHub\nCheck that the owner/repo is correct, or use --parent=virtual for local-only tracking.")]
	RepoNotAccessible { owner: String, repo: String },

	#[error(transparent)]
	Other(#[from] Report),
}

/// Parse a path for --touch mode.
///
/// The input is first matched as a regex over the full logical paths of all existing
/// issues (`owner/repo/.../issue`; dir issues by dir path, flat files sans extension).
/// A unique match opens that issue; multiple matches go through fzf.
///
/// With no match, the input is split by `/` literally as `owner/repo/…/title` and the
/// issue is created — under the local project if it exists, else on GitHub if the repo
/// is accessible, else as a virtual project when `parent` allows it.
pub async fn parse_touch_path(user_input: &str, parent: Option<ProjectType>, offline: bool) -> Result<LocalIssueSource<FsReader>, TouchError> {
	let pattern = strip_md_extension(user_input);

	// logical path → file relpath
	let candidates: Vec<(String, PathBuf)> = Local::issue_file_relpaths()?
		.into_iter()
		.filter_map(|rel| {
			if rel.components().count() < 3 {
				return None; // not under owner/repo (e.g. the urgent file)
			}
			let name = rel.file_name()?.to_str()?;
			let logical = if name.starts_with(Local::MAIN_ISSUE_FILENAME) {
				rel.parent()?.to_string_lossy().to_string()
			} else {
				let base = name.strip_suffix(".md.bak").or_else(|| name.strip_suffix(".md")).unwrap_or(name);
				rel.with_file_name(base).to_string_lossy().to_string()
			};
			Some((logical, rel))
		})
		.collect();

	let names: Vec<String> = candidates.iter().map(|(logical, _)| logical.clone()).collect();
	if let Some(matched) = Local::resolve_pattern(&names, pattern)? {
		let (_, rel) = candidates.iter().find(|(logical, _)| *logical == matched).expect("resolve_pattern returns one of its inputs");
		return Ok(LocalIssueSource::<FsReader>::build_from_path(&Local::issues_dir().join(rel)).await?);
	}

	// No existing issue matches — creation: interpret segments literally as owner/repo/…/title.
	let segments: Vec<&str> = pattern.split('/').collect();
	if segments.len() < 3 {
		return Err(TouchError::new_creation_path_too_short(user_input.to_string()));
	}
	let repo_info = RepoInfo::new(segments[0], segments[1]);
	let titles = &segments[2..];

	let repo_known = Local::project_dir(repo_info).exists() || {
		!offline && {
			let client = exit_on_error(github::client::get());
			exit_on_error(client.repo_exists(repo_info).await)
		}
	};
	if !repo_known {
		match parent {
			Some(ProjectType::Virtual) => {
				Local::ensure_virtual_project(repo_info).expect("failed to create virtual project");
			}
			Some(ProjectType::Default) | None => return Err(TouchError::new_repo_not_accessible(segments[0].to_string(), segments[1].to_string())),
		}
	}

	let selectors: Vec<IssueSelector> = titles.iter().map(|t| IssueSelector::title(t)).collect();
	let index = IssueIndex::with_index(repo_info, selectors);
	let resolved = LocalPath::new(index).resolve_parent(FsReader)?;

	// The leaf may still resolve by Title (substring of sanitized filename) — user intent
	// ≠ sanitized filename, e.g. `o/r/parent` addressing `99_-_parent_issue.md`.
	// HACK: clone before search() since search() consumes self
	match resolved.clone().search() {
		Ok(found) => Ok(LocalIssueSource::<FsReader>::build_from_path(&found.path()).await?),
		Err(e) => match e.kind {
			LocalPathErrorKind::NotFound | LocalPathErrorKind::ParentIsFlat => {
				let create_path = resolved.deterministic(titles.last().expect("segments.len() >= 3"), false, false).path();
				Ok(LocalIssueSource::<FsReader>::build_from_path(&create_path).await?)
			}
			LocalPathErrorKind::MissingParent | LocalPathErrorKind::NotUnique | LocalPathErrorKind::Reader => Err(e.into()),
		},
	}
}

fn strip_md_extension(s: &str) -> &str {
	s.strip_suffix(".md").unwrap_or(s)
}

#[cfg(test)]
mod tests {
	use super::*;

	#[tokio::test]
	async fn test_parse_touch_path_errors() {
		// isolate from the developer's real issues dir: the pattern is matched against all existing issues first
		let tmp = std::env::temp_dir().join("tedi_touch_unit_test_empty");
		std::fs::create_dir_all(&tmp).unwrap();
		crate::mocks::set_issues_dir(tmp);

		let err1 = parse_touch_path("owner/issue.md", None, true).await.unwrap_err();
		let err2 = parse_touch_path("issue.md", None, true).await.unwrap_err();
		eprintln!("{err1:?}\n\n&{err2:?}"); // can't `snapshot` assert it, - error contains home path atm, which depends on the env
	}
}
