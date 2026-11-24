//! Mock Github client for testing purposes.
//!
//! This module provides a mock implementation of the GithubClient trait that stores
//! all data in memory and can be used for integration testing without hitting the real API.

use std::{
	collections::{BTreeMap, BTreeSet, HashMap},
	path::{Path, PathBuf},
	sync::{
		Mutex,
		atomic::{AtomicU64, Ordering},
	},
};

use async_trait::async_trait;
use tedi_core::labels::Color;
use tracing::instrument;

use crate::{
	github::{
		CreatedIssue, FetchedIssue, FileCommit, FileProposal, GithubClient, GithubComment, GithubError, GithubIssue, GithubLabel, GithubMilestone, GithubUser, GraphqlTimeline, IssuePage,
		PrPage, RepoInfo, SubIssue,
	},
	local::Local,
};

/// Mock state file (integration tests): read on start, rewritten after every mutation, so the next process sees it as GitHub would
const ENV_MOCK_STATE: &str = "tedi_MOCK_STATE";
/// When set to an HTTP status (e.g. `503`), `get_milestone`/`update_milestone` return that status as
/// an `Api` error — lets tests inject a GitHub outage and exercise the deferral path.
const ENV_MOCK_FAIL_GET_MILESTONE: &str = "tedi_MOCK_FAIL_GET_MILESTONE";
/// State in the `{app}_MOCK_STATE` shape, merged in on the first call after the file appears — a
/// remote edit made by someone else mid-run (e.g. while the editor is open).
const ENV_MOCK_REMOTE_EDIT: &str = "tedi_MOCK_REMOTE_EDIT";
/// Once this many issue pages are listed, every issue read fails as GitHub's spent budget does.
const ENV_MOCK_RATE_LIMITED_AFTER_PAGES: &str = "tedi_MOCK_RATE_LIMITED_AFTER_PAGES";
/// Every call is appended here as it happens, so a test can see what reached GitHub up to a point.
const ENV_MOCK_CALL_LOG: &str = "tedi_MOCK_CALL_LOG";
/// Mock Github client that stores all state in memory.
/// Thread-safe for use in async contexts.
pub struct MockGithubClient {
	/// The authenticated user's login
	user_login: String,

	/// Counter for generating unique issue IDs
	next_issue_id: AtomicU64,

	/// Counter for generating unique comment IDs
	next_comment_id: AtomicU64,

	/// All issues, keyed by (owner, repo) -> issue_number -> issue
	issues: Mutex<HashMap<RepoKey, HashMap<u64, MockIssueData>>>,

	/// All comments, keyed by (owner, repo) -> comment_id -> comment
	comments: Mutex<HashMap<RepoKey, BTreeMap<u64, MockCommentData>>>,

	/// Sub-issue relationships: parent_issue_number -> vec of child issue numbers
	sub_issues: Mutex<HashMap<RepoKey, HashMap<u64, Vec<u64>>>>,

	/// All milestones, keyed by (owner, repo) -> number -> milestone
	milestones: Mutex<HashMap<RepoKey, BTreeMap<u64, MockMilestoneData>>>,

	/// Labels defined per repo; a repo listed here exists even without issues
	labels: Mutex<HashMap<RepoKey, Vec<String>>>,

	/// `blocked by` links, across repos
	dependencies: Mutex<Vec<MockDependency>>,

	/// `blocked by` links removed on Github: their removal event stamps the blocked issue
	unlinked: Mutex<Vec<MockDependency>>,

	/// Pull requests referencing an issue
	prs: Mutex<HashMap<(RepoKey, u64), Vec<crate::LinkedPr>>>,

	pull_requests: Mutex<HashMap<RepoKey, BTreeMap<u64, tedi_core::PullRequest>>>,

	/// Where a transferred issue went: GitHub redirects its old address there
	transferred: Mutex<HashMap<(RepoKey, u64), (RepoKey, u64)>>,

	/// A file's commits on the default branch, oldest first.
	files: Mutex<HashMap<(RepoKey, String), Vec<FileCommit>>>,

	/// Pull requests opened by `propose_file`, with their numbers.
	proposals: Mutex<Vec<(RepoKey, u64, FileProposal)>>,

	/// When set, `get_milestone`/`update_milestone` fail with this status (fault injection).
	fail_status: Option<reqwest::StatusCode>,

	rate_limited_after_pages: Option<u64>,
	pages_listed: AtomicU64,

	/// Call log for debugging
	call_log: Mutex<Vec<String>>,

	state_file: Option<PathBuf>,
}
impl MockGithubClient {
	/// Create a new mock client with the given authenticated user login
	pub fn new(user_login: &str) -> Self {
		let client = Self {
			user_login: user_login.to_string(),
			next_issue_id: AtomicU64::new(1000),
			next_comment_id: AtomicU64::new(5000),
			issues: Mutex::new(HashMap::new()),
			comments: Mutex::new(HashMap::new()),
			sub_issues: Mutex::new(HashMap::new()),
			milestones: Mutex::new(HashMap::new()),
			labels: Mutex::new(HashMap::new()),
			dependencies: Mutex::new(Vec::new()),
			unlinked: Mutex::new(Vec::new()),
			prs: Mutex::new(HashMap::new()),
			pull_requests: Mutex::new(HashMap::new()),
			transferred: Mutex::new(HashMap::new()),
			files: Mutex::new(HashMap::new()),
			proposals: Mutex::new(Vec::new()),
			fail_status: std::env::var(ENV_MOCK_FAIL_GET_MILESTONE)
				.ok()
				.and_then(|v| v.parse::<u16>().ok())
				.and_then(|c| reqwest::StatusCode::from_u16(c).ok()),
			rate_limited_after_pages: std::env::var(ENV_MOCK_RATE_LIMITED_AFTER_PAGES).ok().map(|v| v.parse().expect("a page count")),
			pages_listed: AtomicU64::new(0),
			call_log: Mutex::new(Vec::new()),
			state_file: std::env::var_os(ENV_MOCK_STATE).map(PathBuf::from),
		};

		if let Some(state_file) = &client.state_file
			&& let Ok(content) = std::fs::read_to_string(state_file)
		{
			if let Err(e) = client.load_state_json(&content) {
				eprintln!("[mock] Failed to load state from {}: {e}", state_file.display());
			} else {
				eprintln!("[mock] Loaded state from {}", state_file.display());
			}
		}

		client
	}

	/// Load state from JSON content
	fn load_state_json(&self, content: &str) -> Result<(), String> {
		use serde_json::Value;

		let state: Value = serde_json::from_str(content).map_err(|e| e.to_string())?;

		// Load issues
		if let Some(issues) = state.get("issues").and_then(|v| v.as_array()) {
			for issue in issues {
				let owner = issue.get("owner").and_then(|v| v.as_str()).ok_or("missing owner")?;
				let repo = issue.get("repo").and_then(|v| v.as_str()).ok_or("missing repo")?;
				let number = issue.get("number").and_then(|v| v.as_u64()).ok_or("missing number")?;
				let title = issue.get("title").and_then(|v| v.as_str()).unwrap_or("");
				let body = issue.get("body").and_then(|v| v.as_str()).unwrap_or("");
				let state_str = issue.get("state").and_then(|v| v.as_str()).unwrap_or("open");
				let state_reason = issue.get("state_reason").and_then(|v| v.as_str()).map(|s| s.to_string());
				let owner_login = issue.get("owner_login").and_then(|v| v.as_str()).unwrap_or("mock_user");
				let milestone = issue.get("milestone").and_then(|v| v.as_u64());

				let labels: Vec<String> = issue
					.get("labels")
					.and_then(|v| v.as_array())
					.map(|arr| arr.iter().filter_map(|v| v.as_str().map(|s| s.to_string())).collect())
					.unwrap_or_default();
				let assignees: Vec<String> = issue
					.get("assignees")
					.and_then(|v| v.as_array())
					.map(|arr| arr.iter().filter_map(|v| v.as_str().map(|s| s.to_string())).collect())
					.unwrap_or_default();

				let key = RepoKey::new(owner, repo);
				let id = self.next_issue_id.fetch_add(1, Ordering::SeqCst);

				// Read timestamps from JSON - required if tests use seeded timestamps
				let parse_ts = |field: &str| -> Option<jiff::Timestamp> { issue.get(field).and_then(|v| v.as_str()).map(|s| s.parse().expect("valid timestamp in mock JSON")) };
				let title_timestamp = parse_ts("title_timestamp");
				let description_timestamp = parse_ts("description_timestamp");
				let labels_timestamp = parse_ts("labels_timestamp");
				let state_timestamp = parse_ts("state_timestamp");
				let assignees_timestamp = parse_ts("assignees_timestamp");

				let issue_data = MockIssueData {
					number,
					id,
					title: title.to_string(),
					body: body.to_string(),
					state: state_str.to_string(),
					state_reason: state_reason.clone(),
					labels,
					owner_login: owner_login.to_string(),
					title_timestamp,
					description_timestamp,
					labels_timestamp,
					state_timestamp,
					assignees_timestamp,
					milestone,
					assignees,
				};

				self.issues.lock().unwrap().entry(key).or_default().insert(number, issue_data);
			}
		}

		// Load sub-issue relationships
		if let Some(sub_issue_arr) = state.get("sub_issues").and_then(|v| v.as_array()) {
			let mut sub_issues = self.sub_issues.lock().unwrap();
			for rel in sub_issue_arr {
				let owner = rel.get("owner").and_then(|v| v.as_str()).ok_or("missing owner")?;
				let repo = rel.get("repo").and_then(|v| v.as_str()).ok_or("missing repo")?;
				let parent = rel.get("parent").and_then(|v| v.as_u64()).ok_or("missing parent")?;
				let children: Vec<u64> = rel
					.get("children")
					.and_then(|v| v.as_array())
					.map(|arr| arr.iter().filter_map(|v| v.as_u64()).collect())
					.unwrap_or_default();

				let key = RepoKey::new(owner, repo);
				sub_issues.entry(key).or_default().insert(parent, children);
			}
		}

		// Load comments
		if let Some(comments_arr) = state.get("comments").and_then(|v| v.as_array()) {
			let mut comments = self.comments.lock().unwrap();
			for comment in comments_arr {
				let owner = comment.get("owner").and_then(|v| v.as_str()).ok_or("missing owner")?;
				let repo = comment.get("repo").and_then(|v| v.as_str()).ok_or("missing repo")?;
				let issue_number = comment.get("issue_number").and_then(|v| v.as_u64()).ok_or("missing issue_number")?;
				let comment_id = comment.get("comment_id").and_then(|v| v.as_u64()).ok_or("missing comment_id")?;
				let body = comment.get("body").and_then(|v| v.as_str()).unwrap_or("");
				let owner_login = comment.get("owner_login").and_then(|v| v.as_str()).unwrap_or("mock_user");

				let key = RepoKey::new(owner, repo);
				// Read timestamps from JSON - required if tests use seeded timestamps
				let created_at = comment.get("created_at").and_then(|v| v.as_str()).expect("created_at required in mock JSON").to_string();
				let updated_at = comment.get("updated_at").and_then(|v| v.as_str()).expect("updated_at required in mock JSON").to_string();
				let comment_data = MockCommentData {
					id: comment_id,
					issue_number,
					body: body.to_string(),
					owner_login: owner_login.to_string(),
					created_at,
					updated_at,
				};

				comments.entry(key).or_default().insert(comment_id, comment_data);
				self.next_comment_id.fetch_max(comment_id + 1, Ordering::SeqCst);
			}
		}

		{
			let node = |dep: &serde_json::Value, side: &str| -> Result<(RepoKey, u64), String> {
				let v = dep.get(side).ok_or(format!("missing {side}"))?;
				let owner = v.get("owner").and_then(|v| v.as_str()).ok_or("missing owner")?;
				let repo = v.get("repo").and_then(|v| v.as_str()).ok_or("missing repo")?;
				let number = v.get("number").and_then(|v| v.as_u64()).ok_or("missing number")?;
				Ok((RepoKey::new(owner, repo), number))
			};
			let parse = |dep: &serde_json::Value| -> Result<MockDependency, String> {
				Ok(MockDependency {
					blocked: node(dep, "blocked")?,
					blocker: node(dep, "blocker")?,
					at: dep.get("at").and_then(|v| v.as_str()).map(|s| s.parse().expect("valid timestamp in mock JSON")),
				})
			};
			for (field, list) in [("dependencies", &self.dependencies), ("unlinked", &self.unlinked)] {
				if let Some(arr) = state.get(field).and_then(|v| v.as_array()) {
					let mut list = list.lock().unwrap();
					for dep in arr {
						list.push(parse(dep)?);
					}
				}
			}
			if let Some(arr) = state.get("transferred").and_then(|v| v.as_array()) {
				let mut transferred = self.transferred.lock().unwrap();
				for t in arr {
					transferred.insert(node(t, "from")?, node(t, "to")?);
				}
			}
			if let Some(arr) = state.get("prs").and_then(|v| v.as_array()) {
				let mut prs = self.prs.lock().unwrap();
				for pr in arr {
					let issue = node(pr, "issue")?;
					let url = pr.get("url").and_then(|v| v.as_str()).ok_or("missing url")?.to_string();
					let state = pr.get("state").and_then(|v| v.as_str()).ok_or("missing state")?.parse().map_err(|e| format!("pr state: {e}"))?;
					prs.entry(issue).or_default().push(crate::LinkedPr { url, state });
				}
			}
		}

		if let Some(arr) = state.get("pull_requests").and_then(|v| v.as_array()) {
			let mut pull_requests = self.pull_requests.lock().unwrap();
			for entry in arr {
				let owner = entry.get("owner").and_then(|v| v.as_str()).ok_or("missing owner")?;
				let repo = entry.get("repo").and_then(|v| v.as_str()).ok_or("missing repo")?;
				let pr: tedi_core::PullRequest = serde_json::from_value(entry.get("pr").ok_or("missing pr")?.clone()).map_err(|e| format!("pull request: {e}"))?;
				pull_requests.entry(RepoKey::new(owner, repo)).or_default().insert(pr.number, pr);
			}
		}

		if let Some(label_arr) = state.get("labels").and_then(|v| v.as_array()) {
			let mut labels = self.labels.lock().unwrap();
			for entry in label_arr {
				let owner = entry.get("owner").and_then(|v| v.as_str()).ok_or("missing owner")?;
				let repo = entry.get("repo").and_then(|v| v.as_str()).ok_or("missing repo")?;
				let names = entry.get("names").and_then(|v| v.as_array()).ok_or("missing names")?;
				labels.insert(
					RepoKey::new(owner, repo),
					names.iter().map(|n| n.as_str().map(str::to_owned).ok_or("label names are strings")).collect::<Result<_, _>>()?,
				);
			}
		}

		if let Some(arr) = state.get("files").and_then(|v| v.as_array()) {
			let mut files = self.files.lock().unwrap();
			for f in arr {
				let owner = f.get("owner").and_then(|v| v.as_str()).ok_or("missing owner")?;
				let repo = f.get("repo").and_then(|v| v.as_str()).ok_or("missing repo")?;
				let path = f.get("path").and_then(|v| v.as_str()).ok_or("missing path")?;
				let commits = f.get("commits").and_then(|v| v.as_array()).ok_or("missing commits")?;
				let commits = commits
					.iter()
					.enumerate()
					.map(|(i, c)| {
						Ok(FileCommit {
							sha: format!("{path}@{i}"),
							author: c.get("author").and_then(|v| v.as_str()).ok_or("missing author")?.to_string(),
							at: c.get("at").and_then(|v| v.as_str()).ok_or("missing at")?.parse().map_err(|e| format!("commit time: {e}"))?,
							content: c.get("content").and_then(|v| v.as_str()).map(str::to_owned),
						})
					})
					.collect::<Result<_, String>>()?;
				files.insert((RepoKey::new(owner, repo), path.to_string()), commits);
			}
		}
		if let Some(arr) = state.get("proposals").and_then(|v| v.as_array()) {
			let mut proposals = self.proposals.lock().unwrap();
			for p in arr {
				let owner = p.get("owner").and_then(|v| v.as_str()).ok_or("missing owner")?;
				let repo = p.get("repo").and_then(|v| v.as_str()).ok_or("missing repo")?;
				let number = p.get("number").and_then(|v| v.as_u64()).ok_or("missing number")?;
				let proposal = serde_json::from_value(p.get("proposal").ok_or("missing proposal")?.clone()).map_err(|e| format!("proposal: {e}"))?;
				proposals.push((RepoKey::new(owner, repo), number, proposal));
			}
		}

		// Load milestones
		if let Some(ms_arr) = state.get("milestones").and_then(|v| v.as_array()) {
			let mut milestones = self.milestones.lock().unwrap();
			for m in ms_arr {
				let owner = m.get("owner").and_then(|v| v.as_str()).ok_or("missing owner")?;
				let repo = m.get("repo").and_then(|v| v.as_str()).ok_or("missing repo")?;
				let number = m.get("number").and_then(|v| v.as_u64()).ok_or("missing number")?;
				let title = m.get("title").and_then(|v| v.as_str()).unwrap_or("").to_string();
				let state_str = m.get("state").and_then(|v| v.as_str()).unwrap_or("open").to_string();
				let description = m.get("description").and_then(|v| v.as_str()).map(|s| s.to_string());
				let parse_ts = |field: &str| -> Option<jiff::Timestamp> { m.get(field).and_then(|v| v.as_str()).map(|s| s.parse().expect("valid timestamp in mock JSON")) };
				let due_on = parse_ts("due_on");
				let updated_at = parse_ts("updated_at");

				let key = RepoKey::new(owner, repo);
				milestones.entry(key).or_default().insert(
					number,
					MockMilestoneData {
						number,
						title,
						state: state_str,
						due_on,
						description,
						updated_at,
					},
				);
			}
		}

		Ok(())
	}

	/// Add an issue to the mock state
	#[cfg(test)]
	#[expect(clippy::too_many_arguments)]
	pub fn add_issue(&self, repo_info: RepoInfo, number: u64, title: &str, body: &str, state: &str, labels: Vec<&str>, owner_login: &str, timestamp: Option<jiff::Timestamp>) {
		let key = RepoKey::from(repo_info);
		let id = self.next_issue_id.fetch_add(1, Ordering::SeqCst);

		let issue = MockIssueData {
			number,
			id,
			title: title.to_string(),
			body: body.to_string(),
			state: state.to_string(),
			state_reason: None,
			labels: labels.into_iter().map(|s| s.to_string()).collect(),
			owner_login: owner_login.to_string(),
			title_timestamp: timestamp,
			description_timestamp: timestamp,
			labels_timestamp: timestamp,
			state_timestamp: timestamp,
			assignees_timestamp: timestamp,
			milestone: None,
			assignees: Vec::new(),
		};

		let mut issues = self.issues.lock().unwrap();
		issues.entry(key).or_default().insert(number, issue);
	}

	/// Add a comment to an issue
	#[cfg(test)]
	pub fn add_comment(&self, repo_info: RepoInfo, issue_number: u64, comment_id: u64, body: &str, owner_login: &str, timestamp: jiff::Timestamp) {
		let key = RepoKey::from(repo_info);

		let ts_str = timestamp.to_string();
		let comment = MockCommentData {
			id: comment_id,
			issue_number,
			body: body.to_string(),
			owner_login: owner_login.to_string(),
			created_at: ts_str.clone(),
			updated_at: ts_str,
		};

		let mut comments = self.comments.lock().unwrap();
		comments.entry(key).or_default().insert(comment_id, comment);
	}

	/// Add a sub-issue relationship
	#[cfg(test)]
	pub fn add_sub_issue_relation(&self, repo_info: RepoInfo, parent_number: u64, child_number: u64) {
		let key = RepoKey::from(repo_info);

		let mut sub_issues = self.sub_issues.lock().unwrap();
		sub_issues.entry(key).or_default().entry(parent_number).or_default().push(child_number);
	}

	/// Get the call log for debugging
	#[cfg(test)]
	pub fn get_call_log(&self) -> Vec<String> {
		self.call_log.lock().unwrap().clone()
	}

	/// Clear the call log
	#[cfg(test)]
	pub fn clear_call_log(&self) {
		self.call_log.lock().unwrap().clear();
	}

	/// Writes the whole state back in the shape [`load_state_json`](Self::load_state_json) reads.
	fn persist(&self) {
		use serde_json::{Value, json};
		let Some(path) = &self.state_file else { return };
		let node = |(key, number): &(RepoKey, u64)| json!({ "owner": key.owner, "repo": key.repo, "number": number });
		let ts = |t: Option<jiff::Timestamp>| t.map(|t| t.to_string());

		let mut issues: Vec<Value> = Vec::new();
		for (key, repo_issues) in self.issues.lock().unwrap().iter() {
			for i in repo_issues.values() {
				issues.push(json!({
					"owner": key.owner, "repo": key.repo, "number": i.number, "title": i.title, "body": i.body,
					"state": i.state, "state_reason": i.state_reason, "owner_login": i.owner_login, "milestone": i.milestone,
					"labels": i.labels, "assignees": i.assignees,
					"title_timestamp": ts(i.title_timestamp), "description_timestamp": ts(i.description_timestamp),
					"labels_timestamp": ts(i.labels_timestamp), "state_timestamp": ts(i.state_timestamp),
					"assignees_timestamp": ts(i.assignees_timestamp),
				}));
			}
		}
		// resource ids follow load order
		issues.sort_by_key(|i| (i["owner"].to_string(), i["repo"].to_string(), i["number"].as_u64()));

		let mut sub_issues: Vec<Value> = Vec::new();
		for (key, relations) in self.sub_issues.lock().unwrap().iter() {
			for (parent, children) in relations {
				sub_issues.push(json!({ "owner": key.owner, "repo": key.repo, "parent": parent, "children": children }));
			}
		}
		let mut comments: Vec<Value> = Vec::new();
		for (key, repo_comments) in self.comments.lock().unwrap().iter() {
			for c in repo_comments.values() {
				comments.push(json!({
					"owner": key.owner, "repo": key.repo, "issue_number": c.issue_number, "comment_id": c.id, "body": c.body,
					"owner_login": c.owner_login, "created_at": c.created_at, "updated_at": c.updated_at,
				}));
			}
		}
		let dependency = |d: &MockDependency| json!({ "blocked": node(&d.blocked), "blocker": node(&d.blocker), "at": ts(d.at) });
		let dependencies: Vec<Value> = self.dependencies.lock().unwrap().iter().map(dependency).collect();
		let unlinked: Vec<Value> = self.unlinked.lock().unwrap().iter().map(dependency).collect();
		let mut prs: Vec<Value> = Vec::new();
		for (issue, linked) in self.prs.lock().unwrap().iter() {
			for pr in linked {
				prs.push(json!({ "issue": node(issue), "url": pr.url, "state": pr.state }));
			}
		}
		let pull_requests: Vec<Value> = self
			.pull_requests
			.lock()
			.unwrap()
			.iter()
			.flat_map(|(key, prs)| prs.values().map(move |pr| json!({ "owner": key.owner, "repo": key.repo, "pr": pr })))
			.collect();
		let transferred: Vec<Value> = self.transferred.lock().unwrap().iter().map(|(from, to)| json!({ "from": node(from), "to": node(to) })).collect();
		let labels: Vec<Value> = self
			.labels
			.lock()
			.unwrap()
			.iter()
			.map(|(key, names)| json!({ "owner": key.owner, "repo": key.repo, "names": names }))
			.collect();
		let mut milestones: Vec<Value> = Vec::new();
		for (key, repo_milestones) in self.milestones.lock().unwrap().iter() {
			for m in repo_milestones.values() {
				milestones.push(json!({
					"owner": key.owner, "repo": key.repo, "number": m.number, "title": m.title, "state": m.state,
					"description": m.description, "due_on": ts(m.due_on), "updated_at": ts(m.updated_at),
				}));
			}
		}

		let files: Vec<Value> = self
			.files
			.lock()
			.unwrap()
			.iter()
			.map(|((key, path), commits)| {
				let commits: Vec<Value> = commits.iter().map(|c| json!({ "author": c.author, "at": c.at.to_string(), "content": c.content })).collect();
				json!({ "owner": key.owner, "repo": key.repo, "path": path, "commits": commits })
			})
			.collect();
		let proposals: Vec<Value> = self
			.proposals
			.lock()
			.unwrap()
			.iter()
			.map(|(key, number, proposal)| json!({ "owner": key.owner, "repo": key.repo, "number": number, "proposal": proposal }))
			.collect();

		let state = json!({
			"issues": issues, "sub_issues": sub_issues, "comments": comments, "dependencies": dependencies, "unlinked": unlinked,
			"prs": prs, "pull_requests": pull_requests, "transferred": transferred, "labels": labels, "milestones": milestones,
			"files": files, "proposals": proposals,
		});
		// renamed into place, so another process never reads it half-written
		let tmp = path.with_extension(format!("{}.tmp", std::process::id()));
		std::fs::write(&tmp, serde_json::to_string_pretty(&state).expect("mock state is plain json")).expect("mock state file is writable");
		std::fs::rename(&tmp, path).expect("mock state file is writable");
	}

	fn log_call(&self, call: &str) {
		// the file is absent until the test makes the edit
		if let Ok(path) = std::env::var(ENV_MOCK_REMOTE_EDIT)
			&& let Ok(content) = std::fs::read_to_string(&path)
		{
			self.load_state_json(&content).expect("mock remote edit must be valid mock state");
			std::fs::remove_file(&path).expect("remote edit applies once");
		}
		if let Ok(path) = std::env::var(ENV_MOCK_CALL_LOG) {
			use std::io::Write as _;
			let mut log = std::fs::OpenOptions::new().create(true).append(true).open(&path).expect("mock call log is writable");
			writeln!(log, "{call}").expect("mock call log is writable");
		}
		self.call_log.lock().unwrap().push(call.to_string());
	}

	/// Get mutable access to an issue, returning an error if not found
	fn with_issue_mut<F, R>(&self, repo: RepoInfo, issue_number: u64, f: F) -> Result<R, GithubError>
	where
		F: FnOnce(&mut MockIssueData) -> R, {
		let key = RepoKey::new(repo.owner().expect("github repo"), repo.repo());
		let mut issues = self.issues.lock().unwrap();
		let repo_issues = issues
			.get_mut(&key)
			.ok_or_else(|| GithubError::new_other(format!("Repository not found: {}/{}", repo.owner().expect("github repo"), repo.repo())))?;
		let issue = repo_issues
			.get_mut(&issue_number)
			.ok_or_else(|| GithubError::new_other(format!("Issue not found: #{issue_number}")))?;
		Ok(f(issue))
	}

	/// Get mutable access to a comment, returning an error if not found
	fn with_comment_mut<F, R>(&self, repo: RepoInfo, comment_id: u64, f: F) -> Result<R, GithubError>
	where
		F: FnOnce(&mut MockCommentData) -> R, {
		let key = RepoKey::new(repo.owner().expect("github repo"), repo.repo());
		let mut comments = self.comments.lock().unwrap();
		let repo_comments = comments
			.get_mut(&key)
			.ok_or_else(|| GithubError::new_other(format!("Repository not found: {}/{}", repo.owner().expect("github repo"), repo.repo())))?;
		let comment = repo_comments
			.get_mut(&comment_id)
			.ok_or_else(|| GithubError::new_other(format!("Comment not found: {comment_id}")))?;
		Ok(f(comment))
	}

	fn within_rate_limit(&self) -> Result<(), GithubError> {
		match self.rate_limited_after_pages {
			Some(pages) if self.pages_listed.load(Ordering::SeqCst) >= pages => Err(GithubError::new_api(
				reqwest::StatusCode::FORBIDDEN,
				"API rate limit exceeded".to_string(),
				"mock budget".to_string(),
			)),
			_ => Ok(()),
		}
	}

	fn full_issue(&self, key: &RepoKey, number: u64) -> Result<FetchedIssue, GithubError> {
		let data = self
			.issues
			.lock()
			.unwrap()
			.get(key)
			.and_then(|m| m.get(&number))
			.cloned()
			.ok_or_else(|| GithubError::new_api(reqwest::StatusCode::NOT_FOUND, format!("no issue #{number} in {key:?}"), "fetch issue".to_string()))?;
		let comments = self
			.comments
			.lock()
			.unwrap()
			.get(key)
			.into_iter()
			.flat_map(|m| m.values())
			.filter(|c| c.issue_number == number)
			.map(|c| GithubComment {
				id: c.id,
				body: if c.body.is_empty() { None } else { Some(c.body.clone()) },
				user: GithubUser { login: c.owner_login.clone() },
				created_at: c.created_at.clone(),
				updated_at: c.updated_at.clone(),
			})
			.collect();
		let (children, parent) = {
			let sub_issues = self.sub_issues.lock().unwrap();
			let repo_sub_issues = sub_issues.get(key);
			let children = repo_sub_issues.and_then(|m| m.get(&number)).cloned().unwrap_or_default();
			let parent = repo_sub_issues.and_then(|m| m.iter().find_map(|(p, c)| c.contains(&number).then_some(*p)));
			(children, parent)
		};
		let sub_issues = {
			let issues = self.issues.lock().unwrap();
			children
				.iter()
				.filter_map(|n| issues.get(key).and_then(|m| m.get(n)))
				.map(|c| SubIssue {
					id: c.id,
					number: c.number,
					state_reason: c.state_reason.clone(),
				})
				.collect()
		};
		let this = (key.clone(), number);
		let (blocked_by, blocking, linked_at) = {
			let dependencies = self.dependencies.lock().unwrap();
			let linked_at = dependencies
				.iter()
				.chain(self.unlinked.lock().unwrap().iter())
				.filter(|d| d.blocked == this)
				.filter_map(|d| d.at)
				.max();
			(
				dependencies.iter().filter(|d| d.blocked == this).map(|d| d.blocker.0.link(d.blocker.1)).collect(),
				dependencies.iter().filter(|d| d.blocker == this).map(|d| d.blocked.0.link(d.blocked.1)).collect(),
				linked_at,
			)
		};
		Ok(FetchedIssue {
			issue: self.convert_issue_data(key, &data),
			comments,
			sub_issues,
			parent,
			timeline: GraphqlTimeline {
				title: data.title_timestamp,
				description: data.description_timestamp.max(linked_at),
				labels: data.labels_timestamp,
				state: data.state_timestamp,
				assignees: data.assignees_timestamp,
				blocked_by,
				blocking,
				prs: self.prs.lock().unwrap().get(&this).cloned().unwrap_or_default(),
			},
			// an untimed mock issue: older than any cursor, which lists it anyway
			updated_at: data.updated().unwrap_or(jiff::Timestamp::UNIX_EPOCH),
		})
	}

	fn convert_issue_data(&self, key: &RepoKey, data: &MockIssueData) -> GithubIssue {
		GithubIssue {
			id: data.id,
			number: data.number,
			title: data.title.clone(),
			body: if data.body.is_empty() { None } else { Some(data.body.clone()) },
			labels: data.labels.iter().map(|name| GithubLabel { name: name.clone() }).collect(),
			user: GithubUser { login: data.owner_login.clone() },
			state: data.state.clone(),
			state_reason: data.state_reason.clone(),
			assignees: data.assignees.iter().map(|login| GithubUser { login: login.clone() }).collect(),
			pull_request: None,
			html_url: key.link(data.number).to_string(),
		}
	}

	fn find_by_id(&self, id: u64) -> Result<(RepoKey, u64), GithubError> {
		self.issues
			.lock()
			.unwrap()
			.iter()
			.find_map(|(key, issues)| issues.values().find(|i| i.id == id).map(|i| (key.clone(), i.number)))
			.ok_or_else(|| GithubError::new_other(format!("no issue with id {id}")))
	}

	/// Next free issue number in `repo`, counting what's stored locally too.
	fn next_number(&self, repo: RepoInfo) -> u64 {
		let max_from_memory = self.issues.lock().unwrap().get(&RepoKey::from(repo)).and_then(|m| m.keys().max().copied()).unwrap_or(0);
		let max_from_meta = Local::load_project_meta(repo).issues.keys().max().copied().unwrap_or(0);
		scan_max_issue_number(repo).max(max_from_meta).max(max_from_memory) + 1
	}
}

/// Scan the project directory for the maximum issue number in filenames.
/// Looks at files/dirs matching pattern `{number}_-_*` or just `{number}`.
fn scan_max_issue_number(repo_info: RepoInfo) -> u64 {
	fn scan_dir(path: &Path, max: &mut u64) {
		let Ok(entries) = std::fs::read_dir(path) else {
			return;
		};
		for entry in entries.flatten() {
			let name = entry.file_name();
			let name_str = name.to_string_lossy();

			// Skip hidden files and special files
			if name_str.starts_with('.') {
				continue;
			}

			// Extract issue number from name: either "{number}_-_..." or just "{number}"
			let number_part = if let Some(sep_pos) = name_str.find("_-_") {
				&name_str[..sep_pos]
			} else {
				// Strip .md or .md.bak extension if present
				name_str.strip_suffix(".md.bak").or_else(|| name_str.strip_suffix(".md")).unwrap_or(&name_str)
			};

			if let Ok(num) = number_part.parse::<u64>() {
				*max = (*max).max(num);
			}

			// Recurse into directories
			let entry_path = entry.path();
			if entry_path.is_dir() {
				scan_dir(&entry_path, max);
			}
		}
	}

	let project_dir = Local::project_dir(repo_info);
	let mut max = 0u64;
	scan_dir(&project_dir, &mut max);
	max
}

/// Internal representation of an issue in the mock
#[derive(Clone, Debug)]
struct MockIssueData {
	number: u64,
	id: u64,
	title: String,
	body: String,
	state: String,
	state_reason: Option<String>,
	labels: Vec<String>,
	owner_login: String,
	/// Timestamp for title changes
	title_timestamp: Option<jiff::Timestamp>,
	/// Timestamp for description/body changes
	description_timestamp: Option<jiff::Timestamp>,
	/// Timestamp for label changes
	labels_timestamp: Option<jiff::Timestamp>,
	/// Timestamp for state changes (open/closed)
	state_timestamp: Option<jiff::Timestamp>,
	assignees_timestamp: Option<jiff::Timestamp>,
	milestone: Option<u64>,
	assignees: Vec<String>,
}

impl MockIssueData {
	fn updated(&self) -> Option<jiff::Timestamp> {
		[
			self.title_timestamp,
			self.description_timestamp,
			self.labels_timestamp,
			self.state_timestamp,
			self.assignees_timestamp,
		]
		.into_iter()
		.flatten()
		.max()
	}
}

/// `blocked` is blocked by `blocker`, linked at `at`.
#[derive(Clone, Debug)]
struct MockDependency {
	blocked: (RepoKey, u64),
	blocker: (RepoKey, u64),
	at: Option<jiff::Timestamp>,
}

impl RepoKey {
	fn link(&self, number: u64) -> crate::IssueLink {
		crate::IssueLink::in_project(RepoInfo::new(&self.owner, &self.repo), number)
	}

	//#[codestyle::skip(swappable-args)]
	fn new(owner: &str, repo: &str) -> Self {
		Self {
			owner: owner.to_string(),
			repo: repo.to_string(),
		}
	}
}

/// Internal representation of a milestone in the mock
#[derive(Clone, Debug)]
struct MockMilestoneData {
	number: u64,
	title: String,
	state: String,
	due_on: Option<jiff::Timestamp>,
	description: Option<String>,
	updated_at: Option<jiff::Timestamp>,
}

impl MockMilestoneData {
	fn to_github(&self) -> GithubMilestone {
		GithubMilestone {
			number: self.number,
			title: self.title.clone(),
			state: self.state.clone(),
			due_on: self.due_on,
			description: self.description.clone(),
			updated_at: self.updated_at,
		}
	}
}

/// Internal representation of a comment in the mock
#[derive(Clone, Debug)]
struct MockCommentData {
	id: u64,
	issue_number: u64,
	body: String,
	owner_login: String,
	created_at: String,
	updated_at: String,
}

/// Key for looking up issues/comments by owner/repo
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
struct RepoKey {
	owner: String,
	repo: String,
}

impl From<RepoInfo> for RepoKey {
	fn from(info: RepoInfo) -> Self {
		Self::new(info.owner().expect("github repo"), info.repo())
	}
}

#[async_trait]
impl GithubClient for MockGithubClient {
	#[instrument(skip_all)]
	async fn fetch_authenticated_user(&self) -> Result<String, GithubError> {
		tracing::info!(target: "mock_github", "fetch_authenticated_user");
		self.log_call("fetch_authenticated_user()");
		Ok(self.user_login.clone())
	}

	#[instrument(skip_all, fields(issue_number))]
	async fn fetch_issue(&self, repo: RepoInfo, issue_number: u64) -> Result<GithubIssue, GithubError> {
		let owner = repo.owner().expect("github repo");
		let repo_name = repo.repo();
		tracing::info!(target: "mock_github", owner, repo_name, issue_number, "fetch_issue");
		self.log_call(&format!("fetch_issue({owner}, {repo_name}, {issue_number})"));

		let (key, issue_number) = match self.transferred.lock().unwrap().get(&(RepoKey::new(owner, repo_name), issue_number)) {
			Some(to) => to.clone(),
			None => (RepoKey::new(owner, repo_name), issue_number),
		};
		let issues = self.issues.lock().unwrap();

		let repo_issues = issues.get(&key).ok_or_else(|| GithubError::new_other(format!("Repository not found: {owner}/{repo_name}")))?;

		let issue_data = repo_issues
			.get(&issue_number)
			.ok_or_else(|| GithubError::new_other(format!("Issue not found: #{issue_number}")))?;

		Ok(self.convert_issue_data(&key, issue_data))
	}

	#[instrument(skip_all, fields(issue_number))]
	async fn fetch_full_issue(&self, repo: RepoInfo, issue_number: u64) -> Result<FetchedIssue, GithubError> {
		let (owner, repo_name) = (repo.owner().expect("github repo"), repo.repo());
		tracing::info!(target: "mock_github", owner, repo_name, issue_number, "fetch_full_issue");
		self.log_call(&format!("fetch_full_issue({owner}, {repo_name}, {issue_number})"));
		self.within_rate_limit()?;
		let (key, number) = match self.transferred.lock().unwrap().get(&(RepoKey::new(owner, repo_name), issue_number)) {
			Some(to) => to.clone(),
			None => (RepoKey::new(owner, repo_name), issue_number),
		};
		self.full_issue(&key, number)
	}

	#[instrument(skip_all, fields(issue_number))]
	async fn update_issue_body(&self, repo: RepoInfo, issue_number: u64, body: &str) -> Result<(), GithubError> {
		let owner = repo.owner().expect("github repo");
		let repo_name = repo.repo();
		tracing::info!(target: "mock_github", owner, repo_name, issue_number, "update_issue_body");
		self.log_call(&format!("update_issue_body({owner}, {repo_name}, {issue_number}, <body>)"));
		self.with_issue_mut(repo, issue_number, |issue| issue.body = body.to_string()).inspect(|()| self.persist())
	}

	async fn update_issue_title(&self, repo: RepoInfo, issue_number: u64, title: &str) -> Result<(), GithubError> {
		let (owner, repo_name) = (repo.owner().expect("github repo"), repo.repo());
		self.log_call(&format!("update_issue_title({owner}, {repo_name}, {issue_number}, {title})"));
		self.with_issue_mut(repo, issue_number, |issue| issue.title = title.to_string()).inspect(|()| self.persist())
	}

	#[instrument(skip_all, fields(issue_number, state))]
	async fn update_issue_state(&self, repo: RepoInfo, issue_number: u64, state: &crate::CloseState) -> Result<(), GithubError> {
		let owner = repo.owner().expect("github repo");
		let repo_name = repo.repo();
		let (state, reason) = (state.to_github_state(), state.to_github_state_reason());
		tracing::info!(target: "mock_github", owner, repo_name, issue_number, state, "update_issue_state");
		match reason {
			Some(reason) => self.log_call(&format!("update_issue_state({owner}, {repo_name}, {issue_number}, {state}/{reason})")),
			None => self.log_call(&format!("update_issue_state({owner}, {repo_name}, {issue_number}, {state})")),
		}
		self.with_issue_mut(repo, issue_number, |issue| {
			issue.state = state.to_string();
			issue.state_reason = reason.map(str::to_owned);
		})
		.inspect(|()| self.persist())
	}

	#[instrument(skip_all, fields(issue_number))]
	async fn set_labels(&self, repo: RepoInfo, issue_number: u64, labels: &[String]) -> Result<(), GithubError> {
		let owner = repo.owner().expect("github repo");
		let repo_name = repo.repo();
		tracing::info!(target: "mock_github", owner, repo_name, issue_number, ?labels, "set_labels");
		self.log_call(&format!("set_labels({owner}, {repo_name}, {issue_number}, {labels:?})"));
		self.with_issue_mut(repo, issue_number, |issue| issue.labels = labels.to_vec()).inspect(|()| self.persist())
	}

	#[instrument(skip_all, fields(comment_id))]
	async fn update_comment(&self, repo: RepoInfo, comment_id: u64, body: &str) -> Result<(), GithubError> {
		let owner = repo.owner().expect("github repo");
		let repo_name = repo.repo();
		tracing::info!(target: "mock_github", owner, repo_name, comment_id, "update_comment");
		self.log_call(&format!("update_comment({owner}, {repo_name}, {comment_id}, <body>)"));
		self.with_comment_mut(repo, comment_id, |comment| comment.body = body.to_string()).inspect(|()| self.persist())
	}

	#[instrument(skip_all, fields(issue_number))]
	async fn create_comment(&self, repo: RepoInfo, issue_number: u64, body: &str) -> Result<GithubComment, GithubError> {
		let owner = repo.owner().expect("github repo");
		let repo_name = repo.repo();
		tracing::info!(target: "mock_github", owner, repo_name, issue_number, "create_comment");
		self.log_call(&format!("create_comment({owner}, {repo_name}, {issue_number}, {body:?})"));

		let key = RepoKey::new(owner, repo_name);
		let comment_id = self.next_comment_id.fetch_add(1, Ordering::SeqCst);

		let now = jiff::Timestamp::now().to_string();
		let comment = MockCommentData {
			id: comment_id,
			issue_number,
			body: body.to_string(),
			owner_login: self.user_login.clone(),
			created_at: now.clone(),
			updated_at: now.clone(),
		};
		self.comments.lock().unwrap().entry(key).or_default().insert(comment_id, comment);
		self.persist();

		Ok(GithubComment {
			id: comment_id,
			body: Some(body.to_string()),
			user: GithubUser { login: self.user_login.clone() },
			created_at: now.clone(),
			updated_at: now,
		})
	}

	#[instrument(skip_all, fields(comment_id))]
	async fn delete_comment(&self, repo: RepoInfo, comment_id: u64) -> Result<(), GithubError> {
		let owner = repo.owner().expect("github repo");
		let repo_name = repo.repo();
		tracing::info!(target: "mock_github", owner, repo_name, comment_id, "delete_comment");
		self.log_call(&format!("delete_comment({owner}, {repo_name}, {comment_id})"));

		let key = RepoKey::new(owner, repo_name);
		if let Some(repo_comments) = self.comments.lock().unwrap().get_mut(&key) {
			repo_comments.remove(&comment_id);
		}
		self.persist();

		Ok(())
	}

	#[instrument(skip_all, fields(title))]
	async fn create_issue(&self, repo: RepoInfo, title: &str, body: &str) -> Result<CreatedIssue, GithubError> {
		let owner = repo.owner().expect("github repo");
		let repo_name = repo.repo();
		tracing::info!(target: "mock_github", owner, repo_name, title, "create_issue");
		self.log_call(&format!("create_issue({owner}, {repo_name}, {title}, {body:?})"));

		let key = RepoKey::new(owner, repo_name);
		let id = self.next_issue_id.fetch_add(1, Ordering::SeqCst);
		let number = self.next_number(repo);

		let now = Some(jiff::Timestamp::now());
		let issue = MockIssueData {
			number,
			id,
			title: title.to_string(),
			body: body.to_string(),
			state: "open".to_string(),
			state_reason: None,
			labels: Vec::new(),
			owner_login: self.user_login.clone(),
			title_timestamp: now,
			description_timestamp: now,
			labels_timestamp: now,
			state_timestamp: now,
			assignees_timestamp: now,
			milestone: None,
			assignees: Vec::new(),
		};

		self.issues.lock().unwrap().entry(key).or_default().insert(number, issue);
		self.persist();

		Ok(CreatedIssue {
			id,
			number,
			html_url: format!("https://github.com/{owner}/{repo_name}/issues/{number}"),
		})
	}

	#[instrument(skip_all, fields(parent_issue_number, child_issue_id))]
	async fn add_sub_issue(&self, repo: RepoInfo, parent_issue_number: u64, child_issue_id: u64) -> Result<(), GithubError> {
		let owner = repo.owner().expect("github repo");
		let repo_name = repo.repo();
		tracing::info!(target: "mock_github", owner, repo_name, parent_issue_number, child_issue_id, "add_sub_issue");
		self.log_call(&format!("add_sub_issue({owner}, {repo_name}, parent={parent_issue_number}, child_id={child_issue_id})"));

		let key = RepoKey::new(owner, repo_name);

		// Find the issue number that matches the child_issue_id
		let child_number = {
			let issues = self.issues.lock().unwrap();
			let repo_issues = issues.get(&key).ok_or_else(|| GithubError::new_other(format!("Repository not found: {owner}/{repo_name}")))?;

			repo_issues
				.values()
				.find(|i| i.id == child_issue_id)
				.map(|i| i.number)
				.ok_or_else(|| GithubError::new_other(format!("Child issue with id {child_issue_id} not found")))?
		};

		self.sub_issues.lock().unwrap().entry(key).or_default().entry(parent_issue_number).or_default().push(child_number);
		self.persist();

		Ok(())
	}

	#[instrument(skip_all, fields(parent_issue_number, child_issue_id))]
	async fn remove_sub_issue(&self, repo: RepoInfo, parent_issue_number: u64, child_issue_id: u64) -> Result<(), GithubError> {
		let owner = repo.owner().expect("github repo");
		let repo_name = repo.repo();
		tracing::info!(target: "mock_github", owner, repo_name, parent_issue_number, child_issue_id, "remove_sub_issue");
		self.log_call(&format!("remove_sub_issue({owner}, {repo_name}, parent={parent_issue_number}, child_id={child_issue_id})"));

		let key = RepoKey::new(owner, repo_name);
		let child_number = {
			let issues = self.issues.lock().unwrap();
			let repo_issues = issues.get(&key).ok_or_else(|| GithubError::new_other(format!("Repository not found: {owner}/{repo_name}")))?;
			repo_issues
				.values()
				.find(|i| i.id == child_issue_id)
				.map(|i| i.number)
				.ok_or_else(|| GithubError::new_other(format!("Child issue with id {child_issue_id} not found")))?
		};

		{
			let mut sub_issues = self.sub_issues.lock().unwrap();
			let children = sub_issues
				.get_mut(&key)
				.and_then(|m| m.get_mut(&parent_issue_number))
				.ok_or_else(|| GithubError::new_other(format!("#{parent_issue_number} has no sub-issues")))?;
			let before = children.len();
			children.retain(|&n| n != child_number);
			if children.len() == before {
				return Err(GithubError::new_other(format!("#{child_number} is not a sub-issue of #{parent_issue_number}")));
			}
		}
		self.persist();
		Ok(())
	}

	async fn add_blocked_by(&self, repo: RepoInfo, issue_number: u64, blocker_id: u64) -> Result<(), GithubError> {
		let blocker = self.find_by_id(blocker_id)?;
		self.log_call(&format!(
			"add_blocked_by({repo}#{issue_number}, blocker={}#{})",
			RepoInfo::new(&blocker.0.owner, &blocker.0.repo),
			blocker.1
		));
		let blocked = (RepoKey::from(repo), issue_number);
		let mut dependencies = self.dependencies.lock().unwrap();
		if dependencies.iter().any(|d| d.blocked == blocked && d.blocker == blocker) {
			return Err(GithubError::new_api(
				reqwest::StatusCode::UNPROCESSABLE_ENTITY,
				"dependency already exists".into(),
				"Failed to add blocked-by dependency".into(),
			));
		}
		dependencies.push(MockDependency { blocked, blocker, at: None });
		drop(dependencies);
		self.persist();
		Ok(())
	}

	async fn remove_blocked_by(&self, repo: RepoInfo, issue_number: u64, blocker_id: u64) -> Result<(), GithubError> {
		let blocker = self.find_by_id(blocker_id)?;
		self.log_call(&format!(
			"remove_blocked_by({repo}#{issue_number}, blocker={}#{})",
			RepoInfo::new(&blocker.0.owner, &blocker.0.repo),
			blocker.1
		));
		let blocked = (RepoKey::from(repo), issue_number);
		let mut dependencies = self.dependencies.lock().unwrap();
		let before = dependencies.len();
		dependencies.retain(|d| !(d.blocked == blocked && d.blocker == blocker));
		if dependencies.len() == before {
			return Err(GithubError::new_api(
				reqwest::StatusCode::NOT_FOUND,
				"no such dependency".into(),
				"Failed to remove blocked-by dependency".into(),
			));
		}
		drop(dependencies);
		self.persist();
		Ok(())
	}

	#[instrument(skip_all, fields(issue_number, ?milestone))]
	async fn set_issue_milestone(&self, repo: RepoInfo, issue_number: u64, milestone: Option<u64>) -> Result<(), GithubError> {
		let owner = repo.owner().expect("github repo");
		let repo_name = repo.repo();
		tracing::info!(target: "mock_github", owner, repo_name, issue_number, ?milestone, "set_issue_milestone");
		self.log_call(&format!("set_issue_milestone({owner}, {repo_name}, {issue_number}, {milestone:?})"));
		if let Some(issue) = self.issues.lock().unwrap().get_mut(&RepoKey::new(owner, repo_name)).and_then(|m| m.get_mut(&issue_number)) {
			issue.milestone = milestone;
		}
		self.persist();
		Ok(())
	}

	#[instrument(skip_all)]
	async fn repo_exists(&self, repo: RepoInfo) -> Result<bool, GithubError> {
		let owner = repo.owner().expect("github repo");
		let repo_name = repo.repo();
		tracing::info!(target: "mock_github", owner, repo_name, "repo_exists");
		self.log_call(&format!("repo_exists({owner}, {repo_name})"));

		let key = RepoKey::new(owner, repo_name);
		Ok(self.issues.lock().unwrap().contains_key(&key) || self.labels.lock().unwrap().contains_key(&key))
	}

	// Milestones are served from in-memory state seeded via the `milestones` array of {app}_MOCK_STATE.
	// The {app}_MOCK_MILESTONE file shortcut (see sprints.rs) bypasses the client for the simplest tests.
	async fn list_milestones(&self, repo: RepoInfo) -> Result<Vec<GithubMilestone>, GithubError> {
		self.log_call("list_milestones");
		let key = RepoKey::from(repo);
		let milestones = self.milestones.lock().unwrap();
		Ok(milestones
			.get(&key)
			.map(|m| m.values().filter(|m| m.state == "open").map(MockMilestoneData::to_github).collect())
			.unwrap_or_default())
	}

	async fn get_milestone(&self, repo: RepoInfo, number: u64) -> Result<GithubMilestone, GithubError> {
		self.log_call(&format!("get_milestone({number})"));
		if let Some(status) = self.fail_status {
			return Err(GithubError::new_api(status, "mock injected failure".to_string(), "get milestone".to_string()));
		}
		let key = RepoKey::from(repo);
		let milestones = self.milestones.lock().unwrap();
		milestones
			.get(&key)
			.and_then(|m| m.get(&number))
			.map(MockMilestoneData::to_github)
			.ok_or_else(|| GithubError::new_api(reqwest::StatusCode::NOT_FOUND, format!("mock has no milestone #{number}"), "get milestone".to_string()))
	}

	async fn list_milestone_issues(&self, repo: RepoInfo, milestone_number: u64) -> Result<Vec<GithubIssue>, GithubError> {
		self.log_call(&format!("list_milestone_issues({milestone_number})"));
		let key = RepoKey::new(repo.owner().expect("github repo"), repo.repo());
		let issues = self.issues.lock().unwrap();
		Ok(issues
			.get(&key)
			.map(|m| m.values().filter(|i| i.milestone == Some(milestone_number)).map(|i| self.convert_issue_data(&key, i)).collect())
			.unwrap_or_default())
	}

	async fn create_milestone(&self, _repo: RepoInfo, title: &str, _description: &str, _closed: bool) -> Result<(), GithubError> {
		self.log_call(&format!("create_milestone({title})"));
		Ok(())
	}

	async fn update_milestone(&self, repo: RepoInfo, number: u64, description: &str, due_on: Option<jiff::Timestamp>) -> Result<(), GithubError> {
		self.log_call(&format!("update_milestone({number})"));
		if let Some(status) = self.fail_status {
			return Err(GithubError::new_api(status, "mock injected failure".to_string(), "update milestone".to_string()));
		}
		if let Some(m) = self.milestones.lock().unwrap().get_mut(&RepoKey::from(repo)).and_then(|m| m.get_mut(&number)) {
			m.description = Some(description.to_string());
			if due_on.is_some() {
				m.due_on = due_on;
			}
			m.updated_at = Some(jiff::Timestamp::now());
		}
		self.persist();
		Ok(())
	}

	/// Like GitHub: comments and assignees travel, labels only where the target defines them, and the
	/// sub-issue relations it was part of are dropped.
	async fn transfer_issue(&self, repo: RepoInfo, issue_number: u64, target: RepoInfo) -> Result<u64, GithubError> {
		let (owner, repo_name) = (repo.owner().expect("github repo"), repo.repo());
		self.log_call(&format!("transfer_issue({owner}, {repo_name}, {issue_number}, {target})"));
		if repo.owner() != target.owner() {
			return Err(GithubError::new_other(format!("can't transfer across owners: {repo} → {target}")));
		}
		let number = self.next_number(target);
		let target_labels = self.labels.lock().unwrap().get(&RepoKey::from(target)).cloned().unwrap_or_default();
		let mut issue = {
			let mut issues = self.issues.lock().unwrap();
			issues
				.get_mut(&RepoKey::from(repo))
				.and_then(|m| m.remove(&issue_number))
				.ok_or_else(|| GithubError::new_other(format!("Issue not found: #{issue_number}")))?
		};
		issue.number = number;
		issue.labels.retain(|l| target_labels.contains(l));
		issue.milestone = None;
		self.issues.lock().unwrap().entry(RepoKey::from(target)).or_default().insert(number, issue);
		let (from, to) = ((RepoKey::from(repo), issue_number), (RepoKey::from(target), number));
		for dep in self.dependencies.lock().unwrap().iter_mut() {
			for end in [&mut dep.blocked, &mut dep.blocker] {
				if *end == from {
					*end = to.clone();
				}
			}
		}
		self.transferred.lock().unwrap().insert(from, to);

		let moved: Vec<MockCommentData> = {
			let mut comments = self.comments.lock().unwrap();
			match comments.get_mut(&RepoKey::from(repo)) {
				Some(c) => {
					let ids: Vec<u64> = c.values().filter(|c| c.issue_number == issue_number).map(|c| c.id).collect();
					ids.iter().map(|id| c.remove(id).expect("listed just above")).collect()
				}
				None => Vec::new(),
			}
		};
		let mut comments = self.comments.lock().unwrap();
		let target_comments = comments.entry(RepoKey::from(target)).or_default();
		for mut c in moved {
			c.issue_number = number;
			target_comments.insert(c.id, c);
		}
		drop(comments);

		if let Some(relations) = self.sub_issues.lock().unwrap().get_mut(&RepoKey::from(repo)) {
			relations.remove(&issue_number);
			for children in relations.values_mut() {
				children.retain(|&c| c != issue_number);
			}
		}
		self.persist();
		Ok(number)
	}

	async fn file_history(&self, repo: RepoInfo, path: &str, branch: &str) -> Result<Vec<FileCommit>, GithubError> {
		self.log_call(&format!("file_history({repo}, {path}, {branch})"));
		Ok(self.files.lock().unwrap().get(&(RepoKey::from(repo), path.to_string())).cloned().unwrap_or_default()) // a file never committed has no history
	}

	async fn propose_file(&self, repo: RepoInfo, proposal: &FileProposal) -> Result<String, GithubError> {
		self.log_call(&format!("propose_file({repo}, {})", proposal.head));
		let number = {
			let mut proposals = self.proposals.lock().unwrap();
			let number = proposals.len() as u64 + 1;
			proposals.push((RepoKey::from(repo), number, proposal.clone()));
			number
		};
		self.persist();
		Ok(format!("https://github.com/{repo}/pull/{number}"))
	}

	/// Seeded labels are names only: each wears GitHub's default grey.
	/// Whoever the repo's issues name.
	async fn list_assignable(&self, repo: RepoInfo) -> Result<Vec<String>, GithubError> {
		self.log_call(&format!("list_assignable({repo})"));
		let issues = self.issues.lock().unwrap();
		let logins: BTreeSet<String> = issues
			.get(&RepoKey::from(repo))
			.into_iter()
			.flat_map(|m| m.values())
			.flat_map(|i| i.assignees.iter().chain([&i.owner_login]).cloned())
			.collect();
		Ok(logins.into_iter().collect())
	}

	async fn request_review(&self, repo: RepoInfo, number: u64, reviewer: &str, on: bool) -> Result<(), GithubError> {
		self.log_call(&format!("request_review({repo}, {number}, {reviewer}, {on})"));
		{
			let mut prs = self.pull_requests.lock().unwrap();
			let pr = prs
				.get_mut(&RepoKey::from(repo))
				.and_then(|m| m.get_mut(&number))
				.ok_or_else(|| GithubError::new_other(format!("mock has no pull request #{number}")))?;
			pr.requested.retain(|r| r != reviewer);
			if on {
				pr.requested.push(reviewer.to_string());
			}
		}
		self.persist();
		Ok(())
	}

	async fn list_labels(&self, repo: RepoInfo) -> Result<BTreeMap<String, Color>, GithubError> {
		self.log_call(&format!("list_labels({repo})"));
		let labels = self.labels.lock().unwrap();
		Ok(labels
			.get(&RepoKey::from(repo))
			.into_iter()
			.flatten()
			.map(|name| (name.clone(), Color([0xed, 0xed, 0xed])))
			.collect())
	}

	async fn list_org_repos(&self, org: &str) -> Result<Vec<RepoInfo>, GithubError> {
		self.log_call(&format!("list_org_repos({org})"));
		let mut repos: Vec<RepoInfo> = self
			.issues
			.lock()
			.unwrap()
			.keys()
			.chain(self.labels.lock().unwrap().keys())
			.filter(|k| k.owner == org)
			.map(|k| RepoInfo::new(&k.owner, &k.repo))
			.collect();
		repos.sort_by_key(|r| r.to_string());
		repos.dedup();
		Ok(repos)
	}

	/// An issue counts as updated at its newest field timestamp; one with none always does. Two a
	/// page, so every pull test pages.
	async fn list_repo_issues(&self, repo: RepoInfo, since: Option<jiff::Timestamp>, after: Option<&str>) -> Result<IssuePage, GithubError> {
		const PAGE: usize = 2;
		self.log_call(&format!("list_repo_issues({repo}, {since:?}, {after:?})"));
		self.within_rate_limit()?;
		let key = RepoKey::from(repo);
		let mut listed: Vec<(jiff::Timestamp, u64)> = self
			.issues
			.lock()
			.unwrap()
			.get(&key)
			.into_iter()
			.flat_map(|m| m.values())
			.filter(|i| match (since, i.updated()) {
				(Some(since), Some(updated)) => updated >= since,
				_ => true,
			})
			.map(|i| (i.updated().unwrap_or(jiff::Timestamp::UNIX_EPOCH), i.number))
			.collect();
		listed.sort_unstable();
		let start: usize = after.map(|a| a.parse().expect("a cursor this mock handed out")).unwrap_or(0);
		let issues = listed.iter().skip(start).take(PAGE).map(|(_, n)| self.full_issue(&key, *n)).collect::<Result<_, _>>()?;
		self.pages_listed.fetch_add(1, Ordering::SeqCst);
		Ok(IssuePage {
			issues,
			next: (start + PAGE < listed.len()).then(|| (start + PAGE).to_string()),
		})
	}

	/// Two a page, so every pull test pages.
	async fn list_repo_prs(&self, repo: RepoInfo, after: Option<&str>) -> Result<PrPage, GithubError> {
		const PAGE: usize = 2;
		self.log_call(&format!("list_repo_prs({repo}, {after:?})"));
		self.within_rate_limit()?;
		let mut listed: Vec<tedi_core::PullRequest> = self
			.pull_requests
			.lock()
			.unwrap()
			.get(&RepoKey::from(repo))
			.into_iter()
			.flat_map(|m| m.values().cloned())
			.collect();
		listed.sort_by_key(|pr| (std::cmp::Reverse(pr.updated_at), pr.number));
		let start: usize = after.map(|a| a.parse().expect("a cursor this mock handed out")).unwrap_or(0);
		Ok(PrPage {
			next: (start + PAGE < listed.len()).then(|| (start + PAGE).to_string()),
			prs: listed.into_iter().skip(start).take(PAGE).collect(),
		})
	}

	async fn set_assignees(&self, repo: RepoInfo, issue_number: u64, assignees: &[String]) -> Result<(), GithubError> {
		let (owner, repo_name) = (repo.owner().expect("github repo"), repo.repo());
		self.log_call(&format!("set_assignees({owner}, {repo_name}, {issue_number}, {assignees:?})"));
		self.with_issue_mut(repo, issue_number, |issue| issue.assignees = assignees.to_vec()).inspect(|()| self.persist())
	}
}

#[cfg(test)]
mod tests {
	use insta::{assert_debug_snapshot, assert_snapshot};

	use super::*;

	/// Test timestamp - a fixed point for unit tests
	fn test_ts() -> jiff::Timestamp {
		jiff::Timestamp::from_second(1704067200).unwrap() // 2024-01-01 00:00:00 UTC
	}

	#[tokio::test]
	async fn test_mock_basic_operations() {
		let client = MockGithubClient::new("testuser");
		let repo = RepoInfo::new("owner", "repo");

		// Add an issue
		client.add_issue(repo, 123, "Test Issue", "Body content", "open", vec!["bug"], "testuser", Some(test_ts()));

		// Fetch it
		let issue = client.fetch_issue(repo, 123).await.unwrap();
		assert_eq!(issue.number, 123);
		assert_eq!(issue.title, "Test Issue");
		assert_eq!(issue.body, Some("Body content".to_string()));
		assert_eq!(issue.state, "open");

		// Update body
		client.update_issue_body(repo, 123, "New body").await.unwrap();
		let issue = client.fetch_issue(repo, 123).await.unwrap();
		assert_eq!(issue.body, Some("New body".to_string()));

		// Update state
		client.update_issue_state(repo, 123, &crate::CloseState::Closed).await.unwrap();
		let issue = client.fetch_issue(repo, 123).await.unwrap();
		assert_eq!(issue.state, "closed");
	}

	#[tokio::test]
	async fn test_mock_sub_issues() {
		let client = MockGithubClient::new("testuser");
		let repo = RepoInfo::new("owner", "repo");

		// Add parent and child issues
		client.add_issue(repo, 1, "Parent Issue", "", "open", vec![], "testuser", Some(test_ts()));
		client.add_issue(repo, 2, "Child Issue", "", "open", vec![], "testuser", Some(test_ts()));

		// Add sub-issue relationship
		client.add_sub_issue_relation(repo, 1, 2);

		// Fetch sub-issues
		let sub_issues = client.fetch_full_issue(repo, 1).await.unwrap().sub_issues;
		assert_debug_snapshot!(format!("{sub_issues:?}"), @r#""[SubIssue { id: 1001, number: 2, state_reason: None }]""#);
	}

	#[tokio::test]
	async fn test_mock_create_issue() {
		let client = MockGithubClient::new("testuser");
		let repo = RepoInfo::new("owner", "repo");

		let created = client.create_issue(repo, "New Issue", "Issue body").await.unwrap();
		assert!(created.number > 0);
		assert!(created.html_url.contains("owner/repo/issues"));

		// Verify it exists
		let issue = client.fetch_issue(repo, created.number).await.unwrap();
		assert_snapshot!(issue.title, "New Issue", @"New Issue");
	}

	#[tokio::test]
	async fn test_mock_comments() {
		let client = MockGithubClient::new("testuser");
		let repo = RepoInfo::new("owner", "repo");

		client.add_issue(repo, 1, "Issue", "", "open", vec![], "testuser", Some(test_ts()));
		client.add_comment(repo, 1, 100, "First comment", "testuser", test_ts());
		client.add_comment(repo, 1, 101, "Second comment", "other", test_ts());

		let comments = client.fetch_full_issue(repo, 1).await.unwrap().comments;
		assert_eq!(comments.len(), 2);

		// Delete a comment
		client.delete_comment(repo, 100).await.unwrap();
		let comments = client.fetch_full_issue(repo, 1).await.unwrap().comments;
		assert_eq!(comments.len(), 1);
	}

	#[tokio::test]
	async fn test_mock_call_log() {
		let client = MockGithubClient::new("testuser");
		let repo = RepoInfo::new("owner", "repo");

		client.add_issue(repo, 1, "Issue", "", "open", vec![], "testuser", Some(test_ts()));
		let _ = client.fetch_issue(repo, 1).await;
		let _ = client.fetch_full_issue(repo, 1).await;

		let log = client.get_call_log();
		assert_eq!(log.len(), 2);
		assert!(log[0].contains("fetch_issue"));
		assert!(log[1].contains("fetch_full_issue"));

		client.clear_call_log();
		assert!(client.get_call_log().is_empty());
	}

	#[tokio::test]
	async fn test_mock_parent_issue() {
		let client = MockGithubClient::new("testuser");
		let repo = RepoInfo::new("owner", "repo");

		// Add parent and child issues
		client.add_issue(repo, 1, "Parent Issue", "", "open", vec![], "testuser", Some(test_ts()));
		client.add_issue(repo, 2, "Child Issue", "", "open", vec![], "testuser", Some(test_ts()));
		client.add_issue(repo, 3, "Grandchild Issue", "", "open", vec![], "testuser", Some(test_ts()));

		// Add sub-issue relationships
		client.add_sub_issue_relation(repo, 1, 2);
		client.add_sub_issue_relation(repo, 2, 3);

		assert_eq!(client.fetch_full_issue(repo, 1).await.unwrap().parent, None);
		assert_eq!(client.fetch_full_issue(repo, 2).await.unwrap().parent, Some(1));
		assert_eq!(client.fetch_full_issue(repo, 3).await.unwrap().parent, Some(2));
	}
}
