//! Git and issue setup extensions for TestContext.
//!
//! Provides simple methods for setting up test scenarios:
//!
//! ```ignore
//! let ctx = TestContext::new("");
//! ctx.init_git();
//!
//! // Set up local file (uncommitted)
//! ctx.local(&issue);
//!
//! // Set up consensus state (committed to git)
//! ctx.consensus(&issue);
//!
//! // Set up mock remote (Github API responses)
//! ctx.remote(&issue);
//!
//! // All methods are additive - can call multiple times:
//! ctx.remote(&issue1);
//! ctx.remote(&issue2); // Adds to mock, doesn't replace
//!
//! // Typical sync test: consensus committed, local uncommitted, remote different
//! ctx.consensus(&base);
//! ctx.local(&modified);
//! ctx.remote(&remote_version);
//! ```
//!
//! Owner/repo/number are extracted from the Issue's identity (IssueLink).
//! If no link exists, defaults are used: owner="owner", repo="repo", number=1.

use std::{cell::RefCell, collections::HashSet, path::PathBuf};

use todo::Issue;
use v_fixtures::fs_standards::git::Git;

use super::TestContext;

/// Default owner for test issues without a link
const DEFAULT_OWNER: &str = "owner";
/// Default repo for test issues without a link
const DEFAULT_REPO: &str = "repo";
/// Default issue number for test issues without a link
const DEFAULT_NUMBER: u64 = 1;

/// State tracking for additive operations
#[derive(Default)]
pub struct GitState {
	/// Track which (owner, repo, number) have been used for local files
	local_issues: HashSet<(String, String, u64)>,
	/// Track which (owner, repo, number) have been used for consensus commits
	consensus_issues: HashSet<(String, String, u64)>,
	/// Accumulated mock remote state
	remote_issues: Vec<MockIssue>,
	remote_sub_issues: Vec<SubIssueRelation>,
	remote_comments: Vec<MockComment>,
	/// Track which (owner, repo, number) have been added to remote
	remote_issue_ids: HashSet<(String, String, u64)>,
}

thread_local! {
	static GIT_STATE: RefCell<std::collections::HashMap<usize, GitState>> = RefCell::new(std::collections::HashMap::new());
}

fn get_ctx_id(ctx: &TestContext) -> usize {
	ctx as *const TestContext as usize
}

fn with_state<F, R>(ctx: &TestContext, f: F) -> R
where
	F: FnOnce(&mut GitState) -> R, {
	GIT_STATE.with(|state| {
		let mut map = state.borrow_mut();
		let id = get_ctx_id(ctx);
		let entry = map.entry(id).or_default();
		f(entry)
	})
}

/// Extension trait for git and issue setup operations.
pub trait GitExt {
	/// Initialize git in the issues directory.
	fn init_git(&self) -> Git;

	/// Write issue to local file (uncommitted). Additive - can call multiple times.
	/// Returns the path to the issue file.
	/// Panics if same (owner, repo, number) is submitted twice.
	fn local(&self, issue: &Issue) -> PathBuf;

	/// Write issue and commit to git as consensus state. Additive - can call multiple times.
	/// Returns the path to the issue file.
	/// Panics if same (owner, repo, number) is submitted twice.
	fn consensus(&self, issue: &Issue) -> PathBuf;

	/// Set up mock Github API to return this issue. Additive - can call multiple times.
	/// Handles sub-issues automatically.
	/// Panics if same (owner, repo, number) is submitted twice.
	fn remote(&self, issue: &Issue);

	/// Get the flat format path for an issue: `{number}_-_{title}.md`
	fn flat_issue_path(&self, owner: &str, repo: &str, number: u64, title: &str) -> PathBuf;

	/// Get the directory format path for an issue: `{number}_-_{title}/__main__.md`
	fn dir_issue_path(&self, owner: &str, repo: &str, number: u64, title: &str) -> PathBuf;

	/// Get the issue path after sync (flat if no children, directory if has children).
	fn issue_path_after_sync(&self, owner: &str, repo: &str, number: u64, title: &str, has_children: bool) -> PathBuf;

	/// Get the path where an issue would be stored (flat format), extracting coords from issue.
	fn issue_path(&self, issue: &Issue) -> PathBuf;
}

impl GitExt for TestContext {
	fn init_git(&self) -> Git {
		Git::init(self.xdg.data_dir().join("issues"))
	}

	fn local(&self, issue: &Issue) -> PathBuf {
		let (owner, repo, number) = extract_issue_coords(issue);
		let key = (owner.clone(), repo.clone(), number);

		with_state(self, |state| {
			if state.local_issues.contains(&key) {
				panic!("local() called twice for same issue: {owner}/{repo}#{number}");
			}
			state.local_issues.insert(key);
		});

		self.write_issue_tree(&owner, &repo, number, issue)
	}

	fn consensus(&self, issue: &Issue) -> PathBuf {
		let (owner, repo, number) = extract_issue_coords(issue);
		let key = (owner.clone(), repo.clone(), number);

		with_state(self, |state| {
			if state.consensus_issues.contains(&key) {
				panic!("consensus() called twice for same issue: {owner}/{repo}#{number}");
			}
			state.consensus_issues.insert(key);
		});

		let path = self.write_issue_tree(&owner, &repo, number, issue);

		let git = self.init_git();
		git.add_all();
		git.commit(&format!("consensus {owner}/{repo}#{number}"));

		path
	}

	fn remote(&self, issue: &Issue) {
		let (owner, repo, number) = extract_issue_coords(issue);
		let key = (owner.clone(), repo.clone(), number);

		with_state(self, |state| {
			if state.remote_issue_ids.contains(&key) {
				panic!("remote() called twice for same issue: {owner}/{repo}#{number}");
			}

			// Recursively add issue and all its children
			add_issue_recursive(state, &owner, &repo, number, None, issue);
		});

		// Rebuild and write mock state
		self.rebuild_mock_state();
	}

	fn issue_path(&self, issue: &Issue) -> PathBuf {
		let (owner, repo, number) = extract_issue_coords(issue);
		self.flat_issue_path(&owner, &repo, number, &issue.contents.title)
	}

	fn flat_issue_path(&self, owner: &str, repo: &str, number: u64, title: &str) -> PathBuf {
		let sanitized = title.replace(' ', "_");
		self.xdg.data_dir().join(format!("issues/{owner}/{repo}/{number}_-_{sanitized}.md"))
	}

	fn dir_issue_path(&self, owner: &str, repo: &str, number: u64, title: &str) -> PathBuf {
		let sanitized = title.replace(' ', "_");
		self.xdg.data_dir().join(format!("issues/{owner}/{repo}/{number}_-_{sanitized}/__main__.md"))
	}

	fn issue_path_after_sync(&self, owner: &str, repo: &str, number: u64, title: &str, has_children: bool) -> PathBuf {
		if has_children {
			self.dir_issue_path(owner, repo, number, title)
		} else {
			self.flat_issue_path(owner, repo, number, title)
		}
	}
}

impl TestContext {
	/// Write an issue tree to the filesystem, with each node in its own file.
	/// Returns the path to the root issue file.
	fn write_issue_tree(&self, owner: &str, repo: &str, number: u64, issue: &Issue) -> PathBuf {
		self.write_issue_tree_recursive(owner, repo, number, issue, &[])
	}

	fn write_issue_tree_recursive(&self, owner: &str, repo: &str, number: u64, issue: &Issue, ancestors: &[String]) -> PathBuf {
		let sanitized_title = issue.contents.title.replace(' ', "_");
		let has_children = !issue.children.is_empty();

		// Build base path: issues/{owner}/{repo}/{ancestors...}
		let mut base_path = format!("issues/{owner}/{repo}");
		for ancestor in ancestors {
			base_path = format!("{base_path}/{ancestor}");
		}

		if has_children {
			// Directory format: {base}/{number}_-_{title}/__main__.md
			let dir_name = format!("{number}_-_{sanitized_title}");
			let dir_path = format!("{base_path}/{dir_name}");
			let file_path = format!("{dir_path}/__main__.md");
			self.xdg.write_data(&file_path, &issue.serialize_filesystem());

			// Write each child recursively
			let mut child_ancestors = ancestors.to_vec();
			child_ancestors.push(dir_name);
			for child in &issue.children {
				let child_number = child.number().unwrap_or(0);
				self.write_issue_tree_recursive(owner, repo, child_number, child, &child_ancestors);
			}

			self.xdg.data_dir().join(&dir_path).join("__main__.md")
		} else {
			// Flat format: {base}/{number}_-_{title}.md
			let filename = format!("{number}_-_{sanitized_title}.md");
			let file_path = format!("{base_path}/{filename}");
			self.xdg.write_data(&file_path, &issue.serialize_filesystem());
			self.xdg.data_dir().join(&base_path).join(&filename)
		}
	}

	fn rebuild_mock_state(&self) {
		with_state(self, |state| {
			let issues: Vec<serde_json::Value> = state
				.remote_issues
				.iter()
				.map(|i| {
					let mut json = serde_json::json!({
						"owner": i.owner,
						"repo": i.repo,
						"number": i.number,
						"title": i.title,
						"body": i.body,
						"state": i.state,
						"owner_login": i.owner_login
					});
					if let Some(reason) = &i.state_reason {
						json["state_reason"] = serde_json::Value::String(reason.clone());
					}
					json
				})
				.collect();

			// Group sub-issue relations by (owner, repo, parent)
			let mut sub_issues_map: std::collections::HashMap<(String, String, u64), Vec<u64>> = std::collections::HashMap::new();
			for rel in &state.remote_sub_issues {
				sub_issues_map.entry((rel.owner.clone(), rel.repo.clone(), rel.parent)).or_default().push(rel.child);
			}

			let sub_issues: Vec<serde_json::Value> = sub_issues_map
				.into_iter()
				.map(|((owner, repo, parent), children)| {
					serde_json::json!({
						"owner": owner,
						"repo": repo,
						"parent": parent,
						"children": children
					})
				})
				.collect();

			let comments: Vec<serde_json::Value> = state
				.remote_comments
				.iter()
				.map(|c| {
					serde_json::json!({
						"owner": c.owner,
						"repo": c.repo,
						"issue_number": c.issue_number,
						"comment_id": c.comment_id,
						"body": c.body,
						"owner_login": c.owner_login
					})
				})
				.collect();

			let mut mock_state = serde_json::json!({ "issues": issues });
			if !sub_issues.is_empty() {
				mock_state["sub_issues"] = serde_json::Value::Array(sub_issues);
			}
			if !comments.is_empty() {
				mock_state["comments"] = serde_json::Value::Array(comments);
			}

			self.setup_mock_state(&mock_state);
		});
	}
}

/// Extract owner, repo, number from an Issue's identity, with defaults.
fn extract_issue_coords(issue: &Issue) -> (String, String, u64) {
	if let Some(link) = issue.identity.link() {
		(link.owner().to_string(), link.repo().to_string(), link.number())
	} else {
		(DEFAULT_OWNER.to_string(), DEFAULT_REPO.to_string(), DEFAULT_NUMBER)
	}
}

/// Extract child issue number from its identity, or use default.
fn extract_child_number(child: &Issue, default: u64) -> u64 {
	child.number().unwrap_or(default)
}

/// Recursively add an issue and all its children to the mock state.
fn add_issue_recursive(state: &mut GitState, owner: &str, repo: &str, number: u64, parent_number: Option<u64>, issue: &Issue) {
	let key = (owner.to_string(), repo.to_string(), number);

	if state.remote_issue_ids.contains(&key) {
		panic!("remote() would add duplicate issue: {owner}/{repo}#{number}");
	}
	state.remote_issue_ids.insert(key);

	// Add the issue itself
	let issue_owner_login = issue.user().expect("issue identity must have user - use @user format in test fixtures").to_string();
	state.remote_issues.push(MockIssue {
		owner: owner.to_string(),
		repo: repo.to_string(),
		number,
		title: issue.contents.title.clone(),
		body: issue.body(),
		state: issue.contents.state.to_github_state().to_string(),
		state_reason: issue.contents.state.to_github_state_reason().map(|s| s.to_string()),
		owner_login: issue_owner_login,
	});

	// Add sub-issue relation if this is a child
	if let Some(parent) = parent_number {
		state.remote_sub_issues.push(SubIssueRelation {
			owner: owner.to_string(),
			repo: repo.to_string(),
			parent,
			child: number,
		});
	}

	// Extract comments (skip first which is the body)
	for comment in issue.contents.comments.iter().skip(1) {
		if let Some(id) = comment.identity.id() {
			let comment_owner_login = comment.identity.user().expect("comment identity must have user - use @user format in test fixtures").to_string();
			state.remote_comments.push(MockComment {
				owner: owner.to_string(),
				repo: repo.to_string(),
				issue_number: number,
				comment_id: id,
				body: comment.body.render(),
				owner_login: comment_owner_login,
			});
		}
	}

	// Recursively add children
	for (i, child) in issue.children.iter().enumerate() {
		let child_number = extract_child_number(child, number * 100 + 1 + i as u64);
		add_issue_recursive(state, owner, repo, child_number, Some(number), child);
	}
}

struct MockIssue {
	owner: String,
	repo: String,
	number: u64,
	title: String,
	body: String,
	state: String,
	state_reason: Option<String>,
	owner_login: String,
}

struct MockComment {
	owner: String,
	repo: String,
	issue_number: u64,
	comment_id: u64,
	body: String,
	owner_login: String,
}

struct SubIssueRelation {
	owner: String,
	repo: String,
	parent: u64,
	child: u64,
}

#[cfg(test)]
mod tests {
	use std::path::Path;

	use super::*;

	fn parse(content: &str) -> Issue {
		Issue::parse_virtual(content, Path::new("test.md")).expect("failed to parse test issue")
	}

	#[test]
	fn test_remote_handles_2_level_nesting() {
		let ctx = TestContext::new("");

		// Create a 3-level hierarchy: grandparent -> parent -> child
		let issue = parse(
			"- [ ] Grandparent <!-- @mock_user https://github.com/o/r/issues/1 -->\n\
			 \tgrandparent body\n\
			 \n\
			 \t- [ ] Parent <!--sub @mock_user https://github.com/o/r/issues/2 -->\n\
			 \t\tparent body\n\
			 \n\
			 \t\t- [ ] Child <!--sub @mock_user https://github.com/o/r/issues/3 -->\n\
			 \t\t\tchild body\n",
		);

		ctx.remote(&issue);

		// Read the mock state that was written
		let mock_content = std::fs::read_to_string(&ctx.mock_state_path).unwrap();
		let mock_state: serde_json::Value = serde_json::from_str(&mock_content).unwrap();

		// Should have 3 issues
		let issues = mock_state["issues"].as_array().unwrap();
		assert_eq!(issues.len(), 3, "Should have 3 issues (grandparent, parent, child)");

		// Check that all issues are present
		let numbers: Vec<u64> = issues.iter().map(|i| i["number"].as_u64().unwrap()).collect();
		assert!(numbers.contains(&1), "Should have grandparent issue #1");
		assert!(numbers.contains(&2), "Should have parent issue #2");
		assert!(numbers.contains(&3), "Should have child issue #3");

		// Should have 2 sub-issue relations
		let sub_issues = mock_state["sub_issues"].as_array().unwrap();
		assert_eq!(sub_issues.len(), 2, "Should have 2 sub-issue relations");

		// Check relations: 1->2 and 2->3
		let relations: Vec<(u64, Vec<u64>)> = sub_issues
			.iter()
			.map(|r| {
				let parent = r["parent"].as_u64().unwrap();
				let children: Vec<u64> = r["children"].as_array().unwrap().iter().map(|c| c.as_u64().unwrap()).collect();
				(parent, children)
			})
			.collect();

		assert!(relations.iter().any(|(p, c)| *p == 1 && c.contains(&2)), "Should have relation: grandparent(1) -> parent(2)");
		assert!(relations.iter().any(|(p, c)| *p == 2 && c.contains(&3)), "Should have relation: parent(2) -> child(3)");
	}
}
