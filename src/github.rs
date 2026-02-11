use std::sync::Arc;

use async_trait::async_trait;
use reqwest::Client;
use serde::Deserialize;
use v_utils::prelude::*;

pub use crate::RepoInfo;

pub type BoxedGithubClient = Arc<dyn GithubClient>;
/// Trait defining all Github API operations.
/// This allows for both real API calls and mock implementations for testing.
#[async_trait]
pub trait GithubClient: Send + Sync {
	/// Fetch the authenticated user's login name
	async fn fetch_authenticated_user(&self) -> Result<String>;

	/// Fetch a single issue by number
	async fn fetch_issue(&self, repo: RepoInfo, issue_number: u64) -> Result<GithubIssue>;

	/// Fetch all comments on an issue
	async fn fetch_comments(&self, repo: RepoInfo, issue_number: u64) -> Result<Vec<GithubComment>>;

	/// Fetch all sub-issues of an issue
	async fn fetch_sub_issues(&self, repo: RepoInfo, issue_number: u64) -> Result<Vec<GithubIssue>>;

	/// Update an issue's body
	async fn update_issue_body(&self, repo: RepoInfo, issue_number: u64, body: &str) -> Result<()>;

	/// Update an issue's state (open/closed)
	async fn update_issue_state(&self, repo: RepoInfo, issue_number: u64, state: &str) -> Result<()>;

	/// Update a comment's body
	async fn update_comment(&self, repo: RepoInfo, comment_id: u64, body: &str) -> Result<()>;

	/// Create a new comment on an issue
	async fn create_comment(&self, repo: RepoInfo, issue_number: u64, body: &str) -> Result<()>;

	/// Delete a comment
	async fn delete_comment(&self, repo: RepoInfo, comment_id: u64) -> Result<()>;

	/// Create a new issue
	async fn create_issue(&self, repo: RepoInfo, title: &str, body: &str) -> Result<CreatedIssue>;

	/// Add a sub-issue to a parent issue
	/// Note: `child_issue_id` is the resource ID (not the issue number)
	async fn add_sub_issue(&self, repo: RepoInfo, parent_issue_number: u64, child_issue_id: u64) -> Result<()>;

	/// Find an issue by exact title match
	#[allow(dead_code)]
	async fn find_issue_by_title(&self, repo: RepoInfo, title: &str) -> Result<Option<u64>>;

	/// Check if an issue exists by number
	#[allow(dead_code)]
	async fn issue_exists(&self, repo: RepoInfo, issue_number: u64) -> Result<bool>;

	/// Fetch the parent issue of a sub-issue (returns None if issue has no parent)
	async fn fetch_parent_issue(&self, repo: RepoInfo, issue_number: u64) -> Result<Option<GithubIssue>>;

	/// Fetch timestamps from GraphQL timeline API for title, description, and label changes.
	/// All fields are optional because GitHub only retains timeline events for 90 days.
	/// Note: Comment timestamps should be extracted from GithubComment's created_at/updated_at fields.
	async fn fetch_timeline_timestamps(&self, repo: RepoInfo, issue_number: u64) -> Result<GraphqlTimelineTimestamps>;

	/// Check if a repository exists and is accessible (we have at least read access)
	async fn repo_exists(&self, repo: RepoInfo) -> Result<bool>;
}
#[derive(Clone, Debug, Deserialize)]
pub struct GithubIssue {
	pub number: u64,
	pub title: String,
	pub body: Option<String>,
	pub labels: Vec<GithubLabel>,
	pub user: GithubUser,
	pub state: String,
	/// Reason for the state (e.g., "completed", "not_planned", "duplicate")
	/// Only present for closed issues.
	pub state_reason: Option<String>,
}

#[derive(Clone, Debug, Deserialize)]
pub struct GithubLabel {
	pub name: String,
}

#[derive(Clone, Debug, Deserialize)]
pub struct GithubUser {
	pub login: String,
}

#[derive(Clone, Debug, Deserialize)]
pub struct GithubComment {
	pub id: u64,
	pub body: Option<String>,
	pub user: GithubUser,
	/// When the comment was created (ISO 8601 format)
	pub created_at: String,
	/// When the comment was last updated (ISO 8601 format)
	pub updated_at: String,
}

/// Response from Github when creating an issue
#[derive(Debug, Deserialize)]
pub struct CreatedIssue {
	pub id: u64,
	pub number: u64,
	pub html_url: String,
}

/// Timestamps from GraphQL timeline API for issue field changes.
/// All fields are optional because GitHub's timeline only retains events for 90 days.
/// Note: Comment timestamps are fetched via REST API (in GithubComment), not here.
#[derive(Clone, Debug, Default)]
pub struct GraphqlTimelineTimestamps {
	/// Most recent title change (from RenamedTitleEvent)
	pub title: Option<jiff::Timestamp>,
	/// Most recent body/description edit (from issue's updatedAt, which is imprecise)
	pub description: Option<jiff::Timestamp>,
	/// Most recent label change (from LabeledEvent/UnlabeledEvent)
	pub labels: Option<jiff::Timestamp>,
	/// Most recent state change (from ClosedEvent/ReopenedEvent)
	pub state: Option<jiff::Timestamp>,
}

//==============================================================================
// Github Client Trait
//==============================================================================

//==============================================================================
// Real Github Client Implementation
//==============================================================================

/// Real Github API client that makes HTTP requests
pub struct RealGithubClient {
	http_client: Client,
	github_token: String,
}

impl RealGithubClient {
	pub fn new(github_token: String) -> Self {
		Self {
			http_client: Client::new(),
			github_token,
		}
	}

	fn request(&self, method: reqwest::Method, url: &str) -> reqwest::RequestBuilder {
		self.http_client
			.request(method, url)
			.header("User-Agent", "Rust Github Client")
			.header("Authorization", format!("token {}", self.github_token))
	}

	fn get(&self, url: &str) -> reqwest::RequestBuilder {
		self.request(reqwest::Method::GET, url)
	}

	fn post(&self, url: &str) -> reqwest::RequestBuilder {
		self.request(reqwest::Method::POST, url)
	}

	fn patch(&self, url: &str) -> reqwest::RequestBuilder {
		self.request(reqwest::Method::PATCH, url)
	}

	fn delete(&self, url: &str) -> reqwest::RequestBuilder {
		self.request(reqwest::Method::DELETE, url)
	}

	/// Send a PATCH request with JSON body, returning an error on non-success status
	async fn patch_json(&self, url: &str, json: &serde_json::Value, error_context: &str) -> Result<()> {
		let res = self.patch(url).json(json).send().await?;

		if !res.status().is_success() {
			let status = res.status();
			let body = res.text().await.unwrap_or_default();
			bail!("{error_context}: {status} - {body}");
		}

		Ok(())
	}

	/// Send a POST request with JSON body, returning an error on non-success status
	async fn post_json(&self, url: &str, json: &serde_json::Value, error_context: &str) -> Result<()> {
		let res = self.post(url).json(json).send().await?;

		if !res.status().is_success() {
			let status = res.status();
			let body = res.text().await.unwrap_or_default();
			bail!("{error_context}: {status} - {body}");
		}

		Ok(())
	}
}

#[async_trait]
impl GithubClient for RealGithubClient {
	async fn fetch_authenticated_user(&self) -> Result<String> {
		let res = self.get("https://api.github.com/user").send().await?;

		if !res.status().is_success() {
			let status = res.status();
			let body = res.text().await.unwrap_or_default();
			bail!("Failed to fetch authenticated user: {status} - {body}");
		}

		let user = res.json::<GithubUser>().await?;
		Ok(user.login)
	}

	async fn fetch_issue(&self, repo: RepoInfo, issue_number: u64) -> Result<GithubIssue> {
		let url = format!("https://api.github.com/repos/{}/{}/issues/{issue_number}", repo.owner(), repo.repo());
		let res = self.get(&url).send().await?;

		if !res.status().is_success() {
			let status = res.status();
			let body = res.text().await.unwrap_or_default();
			bail!("Failed to fetch issue: {status} - {body}");
		}

		let issue = res.json::<GithubIssue>().await?;
		Ok(issue)
	}

	async fn fetch_comments(&self, repo: RepoInfo, issue_number: u64) -> Result<Vec<GithubComment>> {
		let url = format!("https://api.github.com/repos/{}/{}/issues/{issue_number}/comments", repo.owner(), repo.repo());
		let res = self.get(&url).send().await?;

		if !res.status().is_success() {
			let status = res.status();
			let body = res.text().await.unwrap_or_default();
			bail!("Failed to fetch comments: {status} - {body}");
		}

		let comments = res.json::<Vec<GithubComment>>().await?;
		Ok(comments)
	}

	async fn fetch_sub_issues(&self, repo: RepoInfo, issue_number: u64) -> Result<Vec<GithubIssue>> {
		let url = format!("https://api.github.com/repos/{}/{}/issues/{issue_number}/sub_issues", repo.owner(), repo.repo());
		let res = self.get(&url).send().await?;

		if !res.status().is_success() {
			// Sub-issues API might not be available or issue has no sub-issues
			// Return empty vec instead of erroring
			return Ok(Vec::new());
		}

		let sub_issues = res.json::<Vec<GithubIssue>>().await?;
		Ok(sub_issues)
	}

	async fn update_issue_body(&self, repo: RepoInfo, issue_number: u64, body: &str) -> Result<()> {
		let url = format!("https://api.github.com/repos/{}/{}/issues/{issue_number}", repo.owner(), repo.repo());
		self.patch_json(&url, &serde_json::json!({ "body": body }), "Failed to update issue body").await
	}

	async fn update_issue_state(&self, repo: RepoInfo, issue_number: u64, state: &str) -> Result<()> {
		let url = format!("https://api.github.com/repos/{}/{}/issues/{issue_number}", repo.owner(), repo.repo());
		self.patch_json(&url, &serde_json::json!({ "state": state }), "Failed to update issue state").await
	}

	async fn update_comment(&self, repo: RepoInfo, comment_id: u64, body: &str) -> Result<()> {
		let url = format!("https://api.github.com/repos/{}/{}/issues/comments/{comment_id}", repo.owner(), repo.repo());
		self.patch_json(&url, &serde_json::json!({ "body": body }), "Failed to update comment").await
	}

	async fn create_comment(&self, repo: RepoInfo, issue_number: u64, body: &str) -> Result<()> {
		let url = format!("https://api.github.com/repos/{}/{}/issues/{issue_number}/comments", repo.owner(), repo.repo());
		self.post_json(&url, &serde_json::json!({ "body": body }), "Failed to create comment").await
	}

	async fn delete_comment(&self, repo: RepoInfo, comment_id: u64) -> Result<()> {
		let url = format!("https://api.github.com/repos/{}/{}/issues/comments/{comment_id}", repo.owner(), repo.repo());
		let res = self.delete(&url).send().await?;

		if !res.status().is_success() {
			let status = res.status();
			let body = res.text().await.unwrap_or_default();
			bail!("Failed to delete comment: {status} - {body}");
		}

		Ok(())
	}

	async fn create_issue(&self, repo: RepoInfo, title: &str, body: &str) -> Result<CreatedIssue> {
		let url = format!("https://api.github.com/repos/{}/{}/issues", repo.owner(), repo.repo());
		let res = self.post(&url).json(&serde_json::json!({ "title": title, "body": body })).send().await?;

		if !res.status().is_success() {
			let status = res.status();
			let body = res.text().await.unwrap_or_default();
			bail!("Failed to create issue: {status} - {body}");
		}

		let issue = res.json::<CreatedIssue>().await?;
		Ok(issue)
	}

	async fn add_sub_issue(&self, repo: RepoInfo, parent_issue_number: u64, child_issue_id: u64) -> Result<()> {
		let url = format!("https://api.github.com/repos/{}/{}/issues/{parent_issue_number}/sub_issues", repo.owner(), repo.repo());
		self.post_json(&url, &serde_json::json!({ "sub_issue_id": child_issue_id }), "Failed to add sub-issue").await
	}

	async fn find_issue_by_title(&self, repo: RepoInfo, title: &str) -> Result<Option<u64>> {
		// Search for issues with this title (search in open and closed)
		let encoded_title = urlencoding::encode(title);
		let url = format!("https://api.github.com/search/issues?q=repo:{}/{}+in:title+{encoded_title}", repo.owner(), repo.repo());
		let res = self.get(&url).send().await?;

		if !res.status().is_success() {
			return Ok(None);
		}

		#[derive(Deserialize)]
		struct SearchResult {
			items: Vec<SearchItem>,
		}
		#[derive(Deserialize)]
		struct SearchItem {
			number: u64,
			title: String,
		}

		let result: SearchResult = res.json().await?;

		// Find exact title match
		for item in result.items {
			if item.title == title {
				return Ok(Some(item.number));
			}
		}

		Ok(None)
	}

	async fn issue_exists(&self, repo: RepoInfo, issue_number: u64) -> Result<bool> {
		let url = format!("https://api.github.com/repos/{}/{}/issues/{issue_number}", repo.owner(), repo.repo());
		let res = self.get(&url).send().await?;
		Ok(res.status().is_success())
	}

	async fn fetch_parent_issue(&self, repo: RepoInfo, issue_number: u64) -> Result<Option<GithubIssue>> {
		let url = format!("https://api.github.com/repos/{}/{}/issues/{issue_number}/parent", repo.owner(), repo.repo());
		let res = self.get(&url).send().await?;

		if res.status() == reqwest::StatusCode::NOT_FOUND {
			// Issue has no parent
			return Ok(None);
		}

		if !res.status().is_success() {
			let status = res.status();
			let body = res.text().await.unwrap_or_default();
			bail!("Failed to fetch parent issue: {status} - {body}");
		}

		let parent = res.json::<GithubIssue>().await?;
		Ok(Some(parent))
	}

	async fn fetch_timeline_timestamps(&self, repo: RepoInfo, issue_number: u64) -> Result<GraphqlTimelineTimestamps> {
		// GraphQL query to fetch timeline events for timestamp extraction.
		// We query for:
		// - RenamedTitleEvent: title changes
		// - LabeledEvent/UnlabeledEvent: label changes
		// - ClosedEvent/ReopenedEvent: state changes
		// - The issue body's updatedAt for description changes
		//
		// Note: GitHub timeline only retains events for 90 days, so all timestamps are optional.
		// Comment timestamps are fetched via REST API (GithubComment has created_at/updated_at).
		let query = r#"
			query($owner: String!, $repo: String!, $number: Int!) {
				repository(owner: $owner, name: $repo) {
					issue(number: $number) {
						lastEditedAt
						createdAt
						timelineItems(last: 100, itemTypes: [RENAMED_TITLE_EVENT, LABELED_EVENT, UNLABELED_EVENT, CLOSED_EVENT, REOPENED_EVENT]) {
							nodes {
								__typename
								... on RenamedTitleEvent {
									createdAt
								}
								... on LabeledEvent {
									createdAt
								}
								... on UnlabeledEvent {
									createdAt
								}
								... on ClosedEvent {
									createdAt
								}
								... on ReopenedEvent {
									createdAt
								}
							}
						}
					}
				}
			}
		"#;

		let variables = serde_json::json!({
			"owner": repo.owner(),
			"repo": repo.repo(),
			"number": issue_number as i64
		});

		let body = serde_json::json!({
			"query": query,
			"variables": variables
		});

		let res = self.post("https://api.github.com/graphql").json(&body).send().await?;

		if !res.status().is_success() {
			let status = res.status();
			let body = res.text().await.unwrap_or_default();
			bail!("Failed to fetch timeline timestamps via GraphQL: {status} - {body}");
		}

		let response: serde_json::Value = res.json().await?;

		// Check for GraphQL errors
		if let Some(errors) = response.get("errors") {
			bail!("GraphQL errors: {errors}");
		}

		let mut timestamps = GraphqlTimelineTimestamps::default();

		// Extract body edit timestamp (description).
		// `lastEditedAt` is null if the body was never edited after creation, so fall back to `createdAt`.
		let description_ts = response
			.pointer("/data/repository/issue/lastEditedAt")
			.and_then(|v| v.as_str())
			.or_else(|| response.pointer("/data/repository/issue/createdAt").and_then(|v| v.as_str()));
		if let Some(ts_str) = description_ts {
			timestamps.description = ts_str.parse().ok();
		}

		// Process timeline items
		if let Some(nodes) = response.pointer("/data/repository/issue/timelineItems/nodes").and_then(|v| v.as_array()) {
			for node in nodes {
				let Some(typename) = node.get("__typename").and_then(|v| v.as_str()) else {
					tracing::warn!("GraphQL timeline node missing __typename: {node:?}");
					continue;
				};

				match typename {
					"RenamedTitleEvent" =>
						if let Some(created_at) = node.get("createdAt").and_then(|v| v.as_str()) {
							let ts: Option<jiff::Timestamp> = created_at.parse().ok();
							if ts > timestamps.title {
								timestamps.title = ts;
							}
						},
					"LabeledEvent" | "UnlabeledEvent" =>
						if let Some(created_at) = node.get("createdAt").and_then(|v| v.as_str()) {
							let ts: Option<jiff::Timestamp> = created_at.parse().ok();
							if ts > timestamps.labels {
								timestamps.labels = ts;
							}
						},
					"ClosedEvent" | "ReopenedEvent" =>
						if let Some(created_at) = node.get("createdAt").and_then(|v| v.as_str()) {
							let ts: Option<jiff::Timestamp> = created_at.parse().ok();
							if ts > timestamps.state {
								timestamps.state = ts;
							}
						},
					_ => {}
				}
			}
		}

		Ok(timestamps)
	}

	async fn repo_exists(&self, repo: RepoInfo) -> Result<bool> {
		let url = format!("https://api.github.com/repos/{}/{}", repo.owner(), repo.repo());
		let res = self.get(&url).send().await?;
		Ok(res.status().is_success())
	}
}

//==============================================================================
// Convenience type alias for boxed client
//==============================================================================

//==============================================================================
// Global client storage
//==============================================================================

/// Thread-local storage for the GitHub client.
/// Set once at startup, then accessed globally by sink operations.
pub mod client {
	use std::cell::RefCell;

	use super::BoxedGithubClient;

	thread_local! {
		static CLIENT: RefCell<Option<BoxedGithubClient>> = const { RefCell::new(None) };
	}

	/// Set the global GitHub client. Must be called before any sink operations.
	pub fn set(client: BoxedGithubClient) {
		CLIENT.with(|c| *c.borrow_mut() = Some(client));
	}

	/// Get the global GitHub client.
	/// Panics if not set - this is a programming error.
	pub fn get() -> BoxedGithubClient {
		CLIENT.with(|c| c.borrow().clone().expect("GitHub client not initialized - call github::client::set() first"))
	}
}

//==============================================================================
// Utility functions (URL parsing, etc.) - These don't need the trait
//==============================================================================

/// Parse a Github issue URL and extract owner, repo, and issue number.
/// Supports formats like:
/// - https://github.com/owner/repo/issues/123
/// - github.com/owner/repo/issues/123
/// - git@github.com:owner/repo (returns repo info, issue number parsing will fail)
/// - ssh://git@github.com/owner/repo.git (returns repo info, issue number parsing will fail)
pub fn parse_github_issue_url(url: &str) -> Result<(String, String, u64)> {
	let url = url.trim();

	// Try SSH format first: git@github.com:owner/repo.git or git@github.com:owner/repo
	// SSH URLs don't support issue numbers directly, but we parse them for consistency
	if let Some(path) = url.strip_prefix("git@github.com:") {
		// SSH format doesn't have issue numbers - this is an error for issue URLs
		bail!(
			"SSH URL format doesn't support issue numbers. Use HTTPS format: https://github.com/{}/issues/NUMBER",
			path.strip_suffix(".git").unwrap_or(path)
		);
	}

	// Try ssh:// format: ssh://git@github.com/owner/repo.git
	if let Some(path) = url.strip_prefix("ssh://git@github.com/") {
		bail!(
			"SSH URL format doesn't support issue numbers. Use HTTPS format: https://github.com/{}/issues/NUMBER",
			path.strip_suffix(".git").unwrap_or(path)
		);
	}

	// Remove protocol prefix if present (https://, http://)
	let path = url.strip_prefix("https://").or_else(|| url.strip_prefix("http://")).unwrap_or(url);

	// Remove github.com prefix
	let path = path.strip_prefix("github.com/").ok_or_else(|| eyre!("URL must be a Github URL: {url}"))?;

	// Split by /
	let parts: Vec<&str> = path.split('/').collect();

	if parts.len() < 4 || parts[2] != "issues" {
		bail!("Invalid Github issue URL format. Expected: https://github.com/owner/repo/issues/123");
	}

	let owner = parts[0].to_string();
	let repo = parts[1].to_string();
	let issue_number: u64 = parts[3].parse().map_err(|_| eyre!("Invalid issue number: {}", parts[3]))?;

	Ok((owner, repo, issue_number))
}

/// Check if a string looks like a Github issue URL specifically
pub fn is_github_issue_url(s: &str) -> bool {
	let s = s.trim();
	s.contains("github.com/") && s.contains("/issues/")
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn test_parse_github_issue_url() {
		// Standard HTTPS URL
		let (owner, repo, num) = parse_github_issue_url("https://github.com/owner/repo/issues/123").unwrap();
		assert_eq!(owner, "owner");
		assert_eq!(repo, "repo");
		assert_eq!(num, 123);

		// Without protocol
		let (owner, repo, num) = parse_github_issue_url("github.com/owner/repo/issues/456").unwrap();
		assert_eq!(owner, "owner");
		assert_eq!(repo, "repo");
		assert_eq!(num, 456);

		// HTTP URL
		let (owner, repo, num) = parse_github_issue_url("http://github.com/owner/repo/issues/789").unwrap();
		assert_eq!(owner, "owner");
		assert_eq!(repo, "repo");
		assert_eq!(num, 789);

		// With trailing whitespace
		let (owner, repo, num) = parse_github_issue_url("  https://github.com/owner/repo/issues/123  ").unwrap();
		assert_eq!(owner, "owner");
		assert_eq!(repo, "repo");
		assert_eq!(num, 123);
	}

	#[test]
	fn test_parse_github_issue_url_errors() {
		// Not a Github URL
		assert!(parse_github_issue_url("https://gitlab.com/owner/repo/issues/123").is_err());

		// Not an issues URL
		assert!(parse_github_issue_url("https://github.com/owner/repo/pull/123").is_err());

		// Invalid issue number
		assert!(parse_github_issue_url("https://github.com/owner/repo/issues/abc").is_err());

		// Missing parts
		assert!(parse_github_issue_url("https://github.com/owner").is_err());
	}

	#[test]
	fn test_parse_github_issue_url_ssh_error() {
		// SSH URLs should give a helpful error message
		let result = parse_github_issue_url("git@github.com:owner/repo.git");
		assert!(result.is_err());
		let err = result.unwrap_err().to_string();
		assert!(err.contains("SSH URL format doesn't support issue numbers"));
		assert!(err.contains("owner/repo"));

		// ssh:// format
		let result = parse_github_issue_url("ssh://git@github.com/owner/repo.git");
		assert!(result.is_err());
		let err = result.unwrap_err().to_string();
		assert!(err.contains("SSH URL format doesn't support issue numbers"));
	}

	#[test]
	fn test_is_github_issue_url() {
		// Valid issue URLs
		assert!(is_github_issue_url("https://github.com/owner/repo/issues/123"));
		assert!(is_github_issue_url("github.com/owner/repo/issues/456"));
		assert!(is_github_issue_url("http://github.com/owner/repo/issues/789"));

		// Not issue URLs
		assert!(!is_github_issue_url("https://github.com/owner/repo"));
		assert!(!is_github_issue_url("git@github.com:owner/repo.git"));
		assert!(!is_github_issue_url("https://github.com/owner/repo/pull/123"));
		assert!(!is_github_issue_url("https://gitlab.com/owner/repo/issues/123"));
	}
}
