//! Remote GitHub operations for milestones: load from the API, and the single
//! upstream write path (`Sink<Remote>`) that owns both the description push and the
//! issue-assignment diff.

use tedi_core::{CloseState, IssueLink, MilestoneBody, MilestoneIdentity, MilestoneLink, MilestoneTimestamps};

use super::{Remote, RemoteSinkError};
use crate::{Milestone, github, sink::Sink};

/// Fetch a milestone from GitHub: its description becomes the body, and issues
/// assigned on GitHub are folded into `hosted` (so offline resolution sees them).
pub async fn load_remote_milestone(link: &MilestoneLink) -> Result<Milestone, github::GithubError> {
	let gh = github::client::get()?;
	let repo = link.repo_info();
	let number = link.number();

	let milestone = gh.get_milestone(repo, number).await?;
	let assigned = gh.list_milestone_issues(repo, number).await?;

	let mut body = MilestoneBody::parse(milestone.description.as_deref().unwrap_or(""));
	let assigned_links: Vec<IssueLink> = assigned
		.iter()
		.map(|issue| IssueLink::in_project(repo, issue.number))
		.collect();
	body.0.push_issue_links(&assigned_links);

	// GitHub exposes no per-field milestone timestamps — stamp every field with `updated_at`.
	let ts = milestone.updated_at;
	Ok(Milestone {
		identity: MilestoneIdentity {
			link: link.clone(),
			state: if milestone.state == "closed" { CloseState::Closed } else { CloseState::Open },
			due_on: milestone.due_on,
			title: milestone.title,
			timestamps: MilestoneTimestamps {
				title: ts,
				state: ts,
				due_on: ts,
				description: ts,
			},
		},
		body,
	})
}

/// The single upstream write path: push the description (+ due date) and reconcile
/// issue assignments to match `hosted`.
impl Sink<Remote> for Milestone {
	type Error = RemoteSinkError;

	async fn sink(&mut self, old: Option<&Milestone>) -> Result<bool, Self::Error> {
		use std::collections::HashSet;

		let gh = github::client::get()?;
		let repo = self.identity.link.repo_info();
		let number = self.number();
		let mut changed = false;

		let new_desc = self.to_string();
		let desc_or_date_changed = match old {
			Some(o) => o.to_string() != new_desc || o.identity.due_on != self.identity.due_on,
			None => true,
		};
		if desc_or_date_changed {
			println!("Updating milestone {repo}#{number} description...");
			gh.update_milestone(repo, number, &new_desc, self.identity.due_on).await?;
			changed = true;
		}

		// Assignment diff: only issues in the milestone's own repo can carry its milestone field.
		let in_repo = |l: &&IssueLink| l.project() == repo;
		let new_nums: HashSet<u64> = self.body.hosted().iter().filter(in_repo).map(|l| l.number()).collect();
		let old_nums: HashSet<u64> = old.map(|o| o.body.hosted().iter().filter(in_repo).map(|l| l.number()).collect()).unwrap_or_default();

		for num in new_nums.difference(&old_nums) {
			println!("Assigning #{num} to milestone {number}");
			gh.set_issue_milestone(repo, *num, Some(number)).await?;
			changed = true;
		}
		for num in old_nums.difference(&new_nums) {
			println!("Unassigning #{num} from milestone {number}");
			gh.set_issue_milestone(repo, *num, None).await?;
			changed = true;
		}

		Ok(changed)
	}
}
