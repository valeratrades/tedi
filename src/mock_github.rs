//! Mock Github client for testing purposes.
//!
//! This module provides a mock implementation of the GithubClient trait that stores
//! all data in memory and can be used for integration testing without hitting the real API.

use std::{
	collections::HashMap,
	sync::{
		Mutex,
		atomic::{AtomicU64, Ordering},
	},
};

use async_trait::async_trait;
use tracing::instrument;
use v_utils::prelude::*;

use crate::{
	github::{CreatedIssue, GithubClient, GithubComment, GithubIssue, GithubLabel, GithubUser, RepoInfo},
	open_interactions::local::Local,
};

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
	comments: Mutex<HashMap<RepoKey, HashMap<u64, MockCommentData>>>,

	/// Sub-issue relationships: parent_issue_number -> vec of child issue numbers
	sub_issues: Mutex<HashMap<RepoKey, HashMap<u64, Vec<u64>>>>,

	/// Call log for debugging
	call_log: Mutex<Vec<String>>,
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
			call_log: Mutex::new(Vec::new()),
		};

		// Load initial state from file if {PKG_NAME}_MOCK_STATE is set (integration tests)
		if let Ok(state_file) = std::env::var(ENV_MOCK_STATE)
			&& let Ok(content) = std::fs::read_to_string(&state_file)
		{
			if let Err(e) = client.load_state_json(&content) {
				eprintln!("[mock] Failed to load state from {state_file}: {e}");
			} else {
				eprintln!("[mock] Loaded state from {state_file}");
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

				let labels: Vec<String> = issue
					.get("labels")
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
		};

		let mut issues = self.issues.lock().unwrap();
		issues.entry(key).or_default().insert(number, issue);
	}

	/// Add a comment to an issue
	#[cfg(test)]
	#[expect(clippy::too_many_arguments, reason = "test helper, extra verbosity is fine")]
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

	fn log_call(&self, call: &str) {
		self.call_log.lock().unwrap().push(call.to_string());
	}

	/// Get mutable access to an issue, returning an error if not found
	fn with_issue_mut<F, R>(&self, repo: RepoInfo, issue_number: u64, f: F) -> Result<R>
	where
		F: FnOnce(&mut MockIssueData) -> R, {
		let key = RepoKey::new(repo.owner(), repo.repo());
		let mut issues = self.issues.lock().unwrap();
		let repo_issues = issues.get_mut(&key).ok_or_else(|| eyre!("Repository not found: {}/{}", repo.owner(), repo.repo()))?;
		let issue = repo_issues.get_mut(&issue_number).ok_or_else(|| eyre!("Issue not found: #{issue_number}"))?;
		Ok(f(issue))
	}

	/// Get mutable access to a comment, returning an error if not found
	fn with_comment_mut<F, R>(&self, repo: RepoInfo, comment_id: u64, f: F) -> Result<R>
	where
		F: FnOnce(&mut MockCommentData) -> R, {
		let key = RepoKey::new(repo.owner(), repo.repo());
		let mut comments = self.comments.lock().unwrap();
		let repo_comments = comments.get_mut(&key).ok_or_else(|| eyre!("Repository not found: {}/{}", repo.owner(), repo.repo()))?;
		let comment = repo_comments.get_mut(&comment_id).ok_or_else(|| eyre!("Comment not found: {comment_id}"))?;
		Ok(f(comment))
	}

	fn convert_issue_data(&self, data: &MockIssueData) -> GithubIssue {
		GithubIssue {
			number: data.number,
			title: data.title.clone(),
			body: if data.body.is_empty() { None } else { Some(data.body.clone()) },
			labels: data.labels.iter().map(|name| GithubLabel { name: name.clone() }).collect(),
			user: GithubUser { login: data.owner_login.clone() },
			state: data.state.clone(),
			state_reason: data.state_reason.clone(),
		}
	}
}

/// Scan the project directory for the maximum issue number in filenames.
/// Looks at files/dirs matching pattern `{number}_-_*` or just `{number}`.
fn scan_max_issue_number(repo_info: RepoInfo) -> u64 {
	fn scan_dir(path: &std::path::Path, max: &mut u64) {
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

/// Environment variable name for mock state file (integration tests)
const ENV_MOCK_STATE: &str = concat!(env!("CARGO_PKG_NAME"), "_MOCK_STATE");

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

impl RepoKey {
	fn new(owner: &str, repo: &str) -> Self {
		Self {
			owner: owner.to_string(),
			repo: repo.to_string(),
		}
	}
}

impl From<RepoInfo> for RepoKey {
	fn from(info: RepoInfo) -> Self {
		Self::new(info.owner(), info.repo())
	}
}

#[async_trait]
impl GithubClient for MockGithubClient {
	#[instrument(skip(self), name = "MockGithubClient::fetch_authenticated_user")]
	async fn fetch_authenticated_user(&self) -> Result<String> {
		tracing::info!(target: "mock_github", "fetch_authenticated_user");
		self.log_call("fetch_authenticated_user()");
		Ok(self.user_login.clone())
	}

	#[instrument(skip(self), name = "MockGithubClient::fetch_issue")]
	async fn fetch_issue(&self, repo: RepoInfo, issue_number: u64) -> Result<GithubIssue> {
		let owner = repo.owner();
		let repo_name = repo.repo();
		tracing::info!(target: "mock_github", owner, repo_name, issue_number, "fetch_issue");
		self.log_call(&format!("fetch_issue({owner}, {repo_name}, {issue_number})"));

		let key = RepoKey::new(owner, repo_name);
		let issues = self.issues.lock().unwrap();

		let repo_issues = issues.get(&key).ok_or_else(|| eyre!("Repository not found: {owner}/{repo_name}"))?;

		let issue_data = repo_issues.get(&issue_number).ok_or_else(|| eyre!("Issue not found: #{issue_number}"))?;

		Ok(self.convert_issue_data(issue_data))
	}

	#[instrument(skip(self), name = "MockGithubClient::fetch_comments")]
	async fn fetch_comments(&self, repo: RepoInfo, issue_number: u64) -> Result<Vec<GithubComment>> {
		let owner = repo.owner();
		let repo_name = repo.repo();
		tracing::info!(target: "mock_github", owner, repo_name, issue_number, "fetch_comments");
		self.log_call(&format!("fetch_comments({owner}, {repo_name}, {issue_number})"));

		let key = RepoKey::new(owner, repo_name);
		let comments = self.comments.lock().unwrap();

		let repo_comments = match comments.get(&key) {
			Some(c) => c,
			None => return Ok(Vec::new()),
		};

		let issue_comments: Vec<GithubComment> = repo_comments
			.values()
			.filter(|c| c.issue_number == issue_number)
			.map(|c| GithubComment {
				id: c.id,
				body: if c.body.is_empty() { None } else { Some(c.body.clone()) },
				user: GithubUser { login: c.owner_login.clone() },
				created_at: c.created_at.clone(),
				updated_at: c.updated_at.clone(),
			})
			.collect();

		Ok(issue_comments)
	}

	#[instrument(skip(self), name = "MockGithubClient::fetch_sub_issues")]
	async fn fetch_sub_issues(&self, repo: RepoInfo, issue_number: u64) -> Result<Vec<GithubIssue>> {
		let owner = repo.owner();
		let repo_name = repo.repo();
		tracing::info!(target: "mock_github", owner, repo_name, issue_number, "fetch_sub_issues");
		self.log_call(&format!("fetch_sub_issues({owner}, {repo_name}, {issue_number})"));

		let key = RepoKey::new(owner, repo_name);

		let sub_issue_numbers = {
			let sub_issues = self.sub_issues.lock().unwrap();
			match sub_issues.get(&key).and_then(|m| m.get(&issue_number)) {
				Some(numbers) => numbers.clone(),
				None => return Ok(Vec::new()),
			}
		};

		let issues = self.issues.lock().unwrap();
		let repo_issues = match issues.get(&key) {
			Some(i) => i,
			None => return Ok(Vec::new()),
		};

		let result: Vec<GithubIssue> = sub_issue_numbers
			.iter()
			.filter_map(|num| repo_issues.get(num).map(|data| self.convert_issue_data(data)))
			.collect();

		Ok(result)
	}

	#[instrument(skip(self, body), name = "MockGithubClient::update_issue_body")]
	async fn update_issue_body(&self, repo: RepoInfo, issue_number: u64, body: &str) -> Result<()> {
		let owner = repo.owner();
		let repo_name = repo.repo();
		tracing::info!(target: "mock_github", owner, repo_name, issue_number, "update_issue_body");
		self.log_call(&format!("update_issue_body({owner}, {repo_name}, {issue_number}, <body>)"));
		self.with_issue_mut(repo, issue_number, |issue| issue.body = body.to_string())
	}

	#[instrument(skip(self), name = "MockGithubClient::update_issue_state")]
	async fn update_issue_state(&self, repo: RepoInfo, issue_number: u64, state: &str) -> Result<()> {
		let owner = repo.owner();
		let repo_name = repo.repo();
		tracing::info!(target: "mock_github", owner, repo_name, issue_number, state, "update_issue_state");
		self.log_call(&format!("update_issue_state({owner}, {repo_name}, {issue_number}, {state})"));
		self.with_issue_mut(repo, issue_number, |issue| issue.state = state.to_string())
	}

	#[instrument(skip(self, body), name = "MockGithubClient::update_comment")]
	async fn update_comment(&self, repo: RepoInfo, comment_id: u64, body: &str) -> Result<()> {
		let owner = repo.owner();
		let repo_name = repo.repo();
		tracing::info!(target: "mock_github", owner, repo_name, comment_id, "update_comment");
		self.log_call(&format!("update_comment({owner}, {repo_name}, {comment_id}, <body>)"));
		self.with_comment_mut(repo, comment_id, |comment| comment.body = body.to_string())
	}

	#[instrument(skip(self, body), name = "MockGithubClient::create_comment")]
	async fn create_comment(&self, repo: RepoInfo, issue_number: u64, body: &str) -> Result<()> {
		let owner = repo.owner();
		let repo_name = repo.repo();
		tracing::info!(target: "mock_github", owner, repo_name, issue_number, "create_comment");
		self.log_call(&format!("create_comment({owner}, {repo_name}, {issue_number}, <body>)"));

		let key = RepoKey::new(owner, repo_name);
		let comment_id = self.next_comment_id.fetch_add(1, Ordering::SeqCst);

		let now = jiff::Timestamp::now().to_string();
		let comment = MockCommentData {
			id: comment_id,
			issue_number,
			body: body.to_string(),
			owner_login: self.user_login.clone(),
			created_at: now.clone(),
			updated_at: now,
		};

		let mut comments = self.comments.lock().unwrap();
		comments.entry(key).or_default().insert(comment_id, comment);

		Ok(())
	}

	#[instrument(skip(self), name = "MockGithubClient::delete_comment")]
	async fn delete_comment(&self, repo: RepoInfo, comment_id: u64) -> Result<()> {
		let owner = repo.owner();
		let repo_name = repo.repo();
		tracing::info!(target: "mock_github", owner, repo_name, comment_id, "delete_comment");
		self.log_call(&format!("delete_comment({owner}, {repo_name}, {comment_id})"));

		let key = RepoKey::new(owner, repo_name);
		let mut comments = self.comments.lock().unwrap();

		if let Some(repo_comments) = comments.get_mut(&key) {
			repo_comments.remove(&comment_id);
		}

		Ok(())
	}

	#[instrument(skip(self, body), name = "MockGithubClient::create_issue")]
	async fn create_issue(&self, repo: RepoInfo, title: &str, body: &str) -> Result<CreatedIssue> {
		let owner = repo.owner();
		let repo_name = repo.repo();
		tracing::info!(target: "mock_github", owner, repo_name, title, "create_issue");
		self.log_call(&format!("create_issue({owner}, {repo_name}, {title}, <body>)"));

		let key = RepoKey::new(owner, repo_name);
		let id = self.next_issue_id.fetch_add(1, Ordering::SeqCst);

		// Get next issue number: check in-memory store, filesystem, and meta
		let number = {
			let max_from_memory = self.issues.lock().unwrap().get(&key).map(|m| m.keys().max().copied().unwrap_or(0)).unwrap_or(0);
			let max_from_files = scan_max_issue_number(repo);
			let project_meta = Local::load_project_meta(repo);
			let max_from_meta = project_meta.issues.keys().max().copied().unwrap_or(0);
			max_from_files.max(max_from_meta).max(max_from_memory) + 1
		};

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
		};

		let mut issues = self.issues.lock().unwrap();
		issues.entry(key).or_default().insert(number, issue);

		Ok(CreatedIssue {
			id,
			number,
			html_url: format!("https://github.com/{owner}/{repo_name}/issues/{number}"),
		})
	}

	#[instrument(skip(self), name = "MockGithubClient::add_sub_issue")]
	async fn add_sub_issue(&self, repo: RepoInfo, parent_issue_number: u64, child_issue_id: u64) -> Result<()> {
		let owner = repo.owner();
		let repo_name = repo.repo();
		tracing::info!(target: "mock_github", owner, repo_name, parent_issue_number, child_issue_id, "add_sub_issue");
		self.log_call(&format!("add_sub_issue({owner}, {repo_name}, parent={parent_issue_number}, child_id={child_issue_id})"));

		let key = RepoKey::new(owner, repo_name);

		// Find the issue number that matches the child_issue_id
		let child_number = {
			let issues = self.issues.lock().unwrap();
			let repo_issues = issues.get(&key).ok_or_else(|| eyre!("Repository not found: {owner}/{repo_name}"))?;

			repo_issues
				.values()
				.find(|i| i.id == child_issue_id)
				.map(|i| i.number)
				.ok_or_else(|| eyre!("Child issue with id {child_issue_id} not found"))?
		};

		let mut sub_issues = self.sub_issues.lock().unwrap();
		sub_issues.entry(key).or_default().entry(parent_issue_number).or_default().push(child_number);

		Ok(())
	}

	#[instrument(skip(self), name = "MockGithubClient::find_issue_by_title")]
	async fn find_issue_by_title(&self, repo: RepoInfo, title: &str) -> Result<Option<u64>> {
		let owner = repo.owner();
		let repo_name = repo.repo();
		tracing::info!(target: "mock_github", owner, repo_name, title, "find_issue_by_title");
		self.log_call(&format!("find_issue_by_title({owner}, {repo_name}, {title})"));

		let key = RepoKey::new(owner, repo_name);
		let issues = self.issues.lock().unwrap();

		let repo_issues = match issues.get(&key) {
			Some(i) => i,
			None => return Ok(None),
		};

		for issue in repo_issues.values() {
			if issue.title == title {
				return Ok(Some(issue.number));
			}
		}

		Ok(None)
	}

	#[instrument(skip(self), name = "MockGithubClient::issue_exists")]
	async fn issue_exists(&self, repo: RepoInfo, issue_number: u64) -> Result<bool> {
		let owner = repo.owner();
		let repo_name = repo.repo();
		tracing::info!(target: "mock_github", owner, repo_name, issue_number, "issue_exists");
		self.log_call(&format!("issue_exists({owner}, {repo_name}, {issue_number})"));

		let key = RepoKey::new(owner, repo_name);
		let issues = self.issues.lock().unwrap();

		if let Some(repo_issues) = issues.get(&key) {
			return Ok(repo_issues.contains_key(&issue_number));
		}

		Ok(false)
	}

	#[instrument(skip(self), name = "MockGithubClient::fetch_parent_issue")]
	async fn fetch_parent_issue(&self, repo: RepoInfo, issue_number: u64) -> Result<Option<GithubIssue>> {
		let owner = repo.owner();
		let repo_name = repo.repo();
		tracing::info!(target: "mock_github", owner, repo_name, issue_number, "fetch_parent_issue");
		self.log_call(&format!("fetch_parent_issue({owner}, {repo_name}, {issue_number})"));

		let key = RepoKey::new(owner, repo_name);

		// Find the parent by searching through sub_issues relationships
		let parent_number = {
			let sub_issues = self.sub_issues.lock().unwrap();
			if let Some(repo_sub_issues) = sub_issues.get(&key) {
				// Find which parent has this issue as a child
				repo_sub_issues
					.iter()
					.find_map(|(parent, children)| if children.contains(&issue_number) { Some(*parent) } else { None })
			} else {
				None
			}
		};

		match parent_number {
			Some(parent_num) => {
				let issues = self.issues.lock().unwrap();
				let repo_issues = issues.get(&key).ok_or_else(|| eyre!("Repository not found: {owner}/{repo_name}"))?;
				let parent_data = repo_issues.get(&parent_num).ok_or_else(|| eyre!("Parent issue not found: #{parent_num}"))?;
				Ok(Some(self.convert_issue_data(parent_data)))
			}
			None => Ok(None),
		}
	}

	#[instrument(skip(self), name = "MockGithubClient::fetch_timeline_timestamps")]
	async fn fetch_timeline_timestamps(&self, repo: RepoInfo, issue_number: u64) -> Result<crate::github::GraphqlTimelineTimestamps> {
		let owner = repo.owner();
		let repo_name = repo.repo();
		tracing::info!(target: "mock_github", owner, repo_name, issue_number, "fetch_timeline_timestamps");
		self.log_call(&format!("fetch_timeline_timestamps({owner}, {repo_name}, {issue_number})"));

		let issues = self.issues.lock().unwrap();
		let key = RepoKey::new(owner, repo_name);

		if let Some(repo_issues) = issues.get(&key)
			&& let Some(issue) = repo_issues.get(&issue_number)
		{
			return Ok(crate::github::GraphqlTimelineTimestamps {
				title: issue.title_timestamp,
				description: issue.description_timestamp,
				labels: issue.labels_timestamp,
				state: issue.state_timestamp,
			});
		}

		Ok(crate::github::GraphqlTimelineTimestamps::default())
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
		client.update_issue_state(repo, 123, "closed").await.unwrap();
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
		let sub_issues = client.fetch_sub_issues(repo, 1).await.unwrap();
		assert_debug_snapshot!(format!("{sub_issues:?}"), @r#""[GithubIssue { number: 2, title: \"Child Issue\", body: None, labels: [], user: GithubUser { login: \"testuser\" }, state: \"open\", state_reason: None }]""#);
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

		let comments = client.fetch_comments(repo, 1).await.unwrap();
		assert_eq!(comments.len(), 2);

		// Delete a comment
		client.delete_comment(repo, 100).await.unwrap();
		let comments = client.fetch_comments(repo, 1).await.unwrap();
		assert_eq!(comments.len(), 1);
	}

	#[tokio::test]
	async fn test_mock_call_log() {
		let client = MockGithubClient::new("testuser");
		let repo = RepoInfo::new("owner", "repo");

		client.add_issue(repo, 1, "Issue", "", "open", vec![], "testuser", Some(test_ts()));
		let _ = client.fetch_issue(repo, 1).await;
		let _ = client.fetch_comments(repo, 1).await;

		let log = client.get_call_log();
		assert_eq!(log.len(), 2);
		assert!(log[0].contains("fetch_issue"));
		assert!(log[1].contains("fetch_comments"));

		client.clear_call_log();
		assert!(client.get_call_log().is_empty());
	}

	#[tokio::test]
	async fn test_mock_fetch_parent_issue() {
		let client = MockGithubClient::new("testuser");
		let repo = RepoInfo::new("owner", "repo");

		// Add parent and child issues
		client.add_issue(repo, 1, "Parent Issue", "", "open", vec![], "testuser", Some(test_ts()));
		client.add_issue(repo, 2, "Child Issue", "", "open", vec![], "testuser", Some(test_ts()));
		client.add_issue(repo, 3, "Grandchild Issue", "", "open", vec![], "testuser", Some(test_ts()));

		// Add sub-issue relationships
		client.add_sub_issue_relation(repo, 1, 2);
		client.add_sub_issue_relation(repo, 2, 3);

		// Root issue has no parent
		let parent = client.fetch_parent_issue(repo, 1).await.unwrap();
		assert!(parent.is_none());

		// Child issue has parent
		let parent = client.fetch_parent_issue(repo, 2).await.unwrap();
		assert!(parent.is_some());
		assert_eq!(parent.unwrap().number, 1);

		// Grandchild has child as parent
		let parent = client.fetch_parent_issue(repo, 3).await.unwrap();
		assert!(parent.is_some());
		assert_eq!(parent.unwrap().number, 2);
	}
}
