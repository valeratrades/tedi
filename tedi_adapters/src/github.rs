use std::{collections::BTreeMap, sync::Arc};

use async_trait::async_trait;
use reqwest::Client;
use serde::Deserialize;
pub use tedi_core::RepoInfo;
use tedi_core::{CloseState, labels::Color};
use v_utils::macros::wrap_err;

mod auth;

use color_eyre::eyre::{Result, bail, eyre};

/// Everything `FetchedIssue` holds. A connection is read whole or not at all: one cut at its page
/// would read as removals.
const FULL_ISSUE: &str = r#"
fragment Full on Issue {
	databaseId number title body url state stateReason updatedAt lastEditedAt createdAt
	author { login }
	labels(first: 100) { totalCount nodes { name } }
	assignees(first: 100) { totalCount nodes { login } }
	comments(first: 100) { totalCount nodes { databaseId body author { login } createdAt updatedAt } }
	subIssues(first: 100) { totalCount nodes { databaseId number stateReason } }
	parent { number }
	blockedBy(first: 100) { totalCount nodes { url } }
	blocking(first: 100) { totalCount nodes { url } }
	closedByPullRequestsReferences(first: 100, includeClosedPrs: true) { totalCount nodes { url isDraft state } }
	timelineItems(last: 100, itemTypes: [RENAMED_TITLE_EVENT, LABELED_EVENT, UNLABELED_EVENT, CLOSED_EVENT, REOPENED_EVENT, BLOCKED_BY_ADDED_EVENT, BLOCKED_BY_REMOVED_EVENT, ASSIGNED_EVENT, UNASSIGNED_EVENT, CONNECTED_EVENT, DISCONNECTED_EVENT]) {
		nodes {
			__typename
			... on RenamedTitleEvent { createdAt }
			... on LabeledEvent { createdAt }
			... on UnlabeledEvent { createdAt }
			... on ClosedEvent { createdAt }
			... on ReopenedEvent { createdAt }
			... on BlockedByAddedEvent { createdAt }
			... on BlockedByRemovedEvent { createdAt }
			... on AssignedEvent { createdAt }
			... on UnassignedEvent { createdAt }
			... on ConnectedEvent { subject { ... on PullRequest { url isDraft state } } }
			... on DisconnectedEvent { subject { ... on PullRequest { url isDraft state } } }
		}
	}
}"#;

pub type BoxedGithubClient = Arc<dyn GithubClient>;

/// Trait defining all Github API operations.
/// This allows for both real API calls and mock implementations for testing.
#[async_trait]
pub trait GithubClient: Send + Sync {
	/// Fetch the authenticated user's login name
	async fn fetch_authenticated_user(&self) -> Result<String, GithubError>;

	/// Fetch a single issue by number
	async fn fetch_issue(&self, repo: RepoInfo, issue_number: u64) -> Result<GithubIssue, GithubError>;

	/// Everything the store keeps of an issue, in one GraphQL read.
	async fn fetch_full_issue(&self, repo: RepoInfo, issue_number: u64) -> Result<FetchedIssue, GithubError>;

	/// Update an issue's body
	async fn update_issue_body(&self, repo: RepoInfo, issue_number: u64, body: &str) -> Result<(), GithubError>;

	/// Update an issue's title
	async fn update_issue_title(&self, repo: RepoInfo, issue_number: u64, title: &str) -> Result<(), GithubError>;

	/// Update an issue's state, closed ones with their reason
	async fn update_issue_state(&self, repo: RepoInfo, issue_number: u64, state: &CloseState) -> Result<(), GithubError>;

	/// Update a comment's body
	async fn update_comment(&self, repo: RepoInfo, comment_id: u64, body: &str) -> Result<(), GithubError>;

	/// Create a new comment on an issue
	async fn create_comment(&self, repo: RepoInfo, issue_number: u64, body: &str) -> Result<GithubComment, GithubError>;

	/// Delete a comment
	async fn delete_comment(&self, repo: RepoInfo, comment_id: u64) -> Result<(), GithubError>;

	//#[codestyle::skip(swappable-args)]
	/// Create a new issue
	async fn create_issue(&self, repo: RepoInfo, title: &str, body: &str) -> Result<CreatedIssue, GithubError>;

	//#[codestyle::skip(swappable-args)]
	/// Add a sub-issue to a parent issue
	/// Note: `child_issue_id` is the resource ID (not the issue number)
	async fn add_sub_issue(&self, repo: RepoInfo, parent_issue_number: u64, child_issue_id: u64) -> Result<(), GithubError>;

	//#[codestyle::skip(swappable-args)]
	/// Note: `child_issue_id` is the resource ID (not the issue number)
	async fn remove_sub_issue(&self, repo: RepoInfo, parent_issue_number: u64, child_issue_id: u64) -> Result<(), GithubError>;

	//#[codestyle::skip(swappable-args)]
	/// Mark an issue as blocked by another, in any repo. `blocker_id` is the resource ID (not the issue number)
	async fn add_blocked_by(&self, repo: RepoInfo, issue_number: u64, blocker_id: u64) -> Result<(), GithubError>;

	//#[codestyle::skip(swappable-args)]
	/// `blocker_id` is the resource ID (not the issue number)
	async fn remove_blocked_by(&self, repo: RepoInfo, issue_number: u64, blocker_id: u64) -> Result<(), GithubError>;

	/// Replace all labels on an issue.
	async fn set_labels(&self, repo: RepoInfo, issue_number: u64, labels: &[String]) -> Result<(), GithubError>;

	/// Set or clear the milestone on an issue.
	/// Pass `Some(number)` to assign, `None` to unassign.
	async fn set_issue_milestone(&self, repo: RepoInfo, issue_number: u64, milestone: Option<u64>) -> Result<(), GithubError>;

	/// Check if a repository exists and is accessible (we have at least read access)
	async fn repo_exists(&self, repo: RepoInfo) -> Result<bool, GithubError>;

	/// List all milestones (open and closed) for a repository.
	async fn list_milestones(&self, repo: RepoInfo) -> Result<Vec<GithubMilestone>, GithubError>;

	/// Fetch a single milestone by number.
	async fn get_milestone(&self, repo: RepoInfo, number: u64) -> Result<GithubMilestone, GithubError>;

	/// List all issues (open and closed) assigned to a milestone.
	async fn list_milestone_issues(&self, repo: RepoInfo, milestone_number: u64) -> Result<Vec<GithubIssue>, GithubError>;

	//#[codestyle::skip(swappable-args)]
	/// Create a milestone. `closed` opens it in the `closed` state (used for archival).
	async fn create_milestone(&self, repo: RepoInfo, title: &str, description: &str, closed: bool) -> Result<(), GithubError>;

	/// Update a milestone's description and, optionally, its due date.
	async fn update_milestone(&self, repo: RepoInfo, number: u64, description: &str, due_on: Option<jiff::Timestamp>) -> Result<(), GithubError>;

	/// Transfer an issue to `target` (same owner only). Returns its number there.
	async fn transfer_issue(&self, repo: RepoInfo, issue_number: u64, target: RepoInfo) -> Result<u64, GithubError>;

	/// Every label defined in a repository, by name.
	async fn list_labels(&self, repo: RepoInfo) -> Result<BTreeMap<String, Color>, GithubError>;

	/// Replace the issue's assignees. A login the repo can't take is dropped by GitHub, not an error.
	async fn set_assignees(&self, repo: RepoInfo, issue_number: u64, assignees: &[String]) -> Result<(), GithubError>;

	/// Logins the repo's issues can be assigned to.
	async fn list_assignable(&self, repo: RepoInfo) -> Result<Vec<String>, GithubError>;

	/// Asks `reviewer` (a login, or a team as `org/team`) for a review of pull request `number`, or withdraws the ask.
	async fn request_review(&self, repo: RepoInfo, number: u64, reviewer: &str, on: bool) -> Result<(), GithubError>;

	/// The org's repos, archived ones aside.
	async fn list_org_repos(&self, org: &str) -> Result<Vec<RepoInfo>, GithubError>;

	/// The page from `after` of every issue updated at or after `since`, open and closed, least recently updated first.
	async fn list_repo_issues(&self, repo: RepoInfo, since: Option<jiff::Timestamp>, after: Option<&str>) -> Result<IssuePage, GithubError>;

	/// The page from `after` of every pull request, most recently updated first.
	async fn list_repo_prs(&self, repo: RepoInfo, after: Option<&str>) -> Result<PrPage, GithubError>;

	//#[codestyle::skip(swappable-args)]
	/// Every commit on `branch` touching `path`, oldest first, each with the file as it left it.
	async fn file_history(&self, repo: RepoInfo, path: &str, branch: &str) -> Result<Vec<FileCommit>, GithubError>;

	/// `proposal` opened as a pull request; its url.
	async fn propose_file(&self, repo: RepoInfo, proposal: &FileProposal) -> Result<String, GithubError>;
}

#[derive(Clone, Debug)]
pub struct FetchedIssue {
	pub issue: GithubIssue,
	pub comments: Vec<GithubComment>,
	pub sub_issues: Vec<SubIssue>,
	pub parent: Option<u64>,
	pub timeline: GraphqlTimeline,
	pub updated_at: jiff::Timestamp,
}

#[derive(Clone, Debug)]
pub struct SubIssue {
	pub id: u64,
	pub number: u64,
	pub state_reason: Option<String>,
}

#[derive(Clone, Debug)]
pub struct PrPage {
	pub prs: Vec<tedi_core::PullRequest>,
	/// `None` on the last page
	pub next: Option<String>,
}

#[derive(Clone, Debug)]
pub struct IssuePage {
	pub issues: Vec<FetchedIssue>,
	/// `None` on the last page
	pub next: Option<String>,
}

/// A pull request onto `base` from a new branch `head` whose one change sets `path` to `content`.
#[derive(Clone, Debug, Deserialize, serde::Serialize)]
pub struct FileProposal {
	pub base: String,
	pub head: String,
	pub path: String,
	pub content: String,
	pub title: String,
	pub body: String,
}

#[derive(Clone, Debug)]
pub struct FileCommit {
	pub sha: String,
	/// The GitHub login, else the name the commit carries.
	pub author: String,
	pub at: jiff::Timestamp,
	/// None: the commit removed the file.
	pub content: Option<String>,
}

/// A milestone as returned by the GitHub REST API.
#[derive(Clone, Debug, Deserialize)]
pub struct GithubMilestone {
	pub number: u64,
	pub title: String,
	pub state: String,
	pub due_on: Option<jiff::Timestamp>,
	pub description: Option<String>,
	/// Last modification time (coarse: GitHub exposes no per-field milestone timestamps).
	#[serde(default)]
	pub updated_at: Option<jiff::Timestamp>,
}
/// Error type for GitHub API operations.
#[wrap_err]
#[derive(Debug, thiserror::Error)]
pub enum GithubError {
	/// HTTP request failed (network, TLS, timeout, etc.)
	#[foreign]
	Request(reqwest::Error),

	/// Signing the GitHub App's JWT failed.
	#[foreign]
	Jwt(jsonwebtoken::errors::Error),

	/// GitHub API returned a non-success status code.
	#[leaf]
	#[error("{context}: {status} - {body}")]
	Api { status: reqwest::StatusCode, body: String, context: String },

	/// GraphQL-level errors in the response body.
	#[leaf]
	#[error("GraphQL errors: {msg}")]
	Graphql { msg: String },

	/// Client not initialized.
	#[leaf]
	#[error("GitHub client not initialized. Is the config file missing `github_token` or `github_app`?")]
	NotInitialized,

	/// Generic error (for mocks and other non-HTTP contexts).
	#[leaf]
	#[error("{msg}")]
	Other { msg: String },
}

impl GithubError {
	/// Transient = worth retrying / deferring to a local save: a network/TLS/timeout drop, or a
	/// 5xx / 429 from the API. Everything else (4xx, GraphQL, NotInitialized, Other) is permanent —
	/// surface it loudly rather than silently deferring.
	pub fn is_transient(&self) -> bool {
		match self {
			GithubError::Request { .. } => true,
			GithubError::Api { status, .. } => status.is_server_error() || *status == reqwest::StatusCode::TOO_MANY_REQUESTS,
			_ => false,
		}
	}
}

#[derive(Clone, Debug, Deserialize)]
pub struct GithubIssue {
	/// Database id — what the sub-issue endpoints address an issue by.
	pub id: u64,
	pub number: u64,
	pub title: String,
	pub body: Option<String>,
	pub labels: Vec<GithubLabel>,
	pub user: GithubUser,
	pub state: String,
	/// Reason for the state (e.g., "completed", "not_planned", "duplicate")
	/// Only present for closed issues.
	pub state_reason: Option<String>,
	pub assignees: Vec<GithubUser>,
	/// Present on a pull request: the issues API serves those too.
	pub pull_request: Option<serde::de::IgnoredAny>,
	/// Where it lives now: an issue moved since is served from its old address too.
	pub html_url: String,
	pub created_at: jiff::Timestamp,
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
	pub created_at: jiff::Timestamp,
}

/// When issue fields last changed, and the issue's dependency links.
/// Timestamps are optional because GitHub's timeline only retains events for 90 days.
#[derive(Clone, Debug, Default)]
pub struct GraphqlTimeline {
	/// Most recent title change (from RenamedTitleEvent)
	pub title: Option<jiff::Timestamp>,
	/// Most recent body edit or `blocked by` change: tedi's blockers hold both
	pub description: Option<jiff::Timestamp>,
	/// Most recent label change (from LabeledEvent/UnlabeledEvent)
	pub labels: Option<jiff::Timestamp>,
	/// Most recent state change (from ClosedEvent/ReopenedEvent)
	pub state: Option<jiff::Timestamp>,
	pub assignees: Option<jiff::Timestamp>,
	pub blocked_by: Vec<tedi_core::IssueLink>,
	pub blocking: Vec<tedi_core::IssueLink>,
	/// Closing references, plus PRs connected by hand and not disconnected since.
	pub prs: Vec<tedi_core::LinkedPr>,
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
	auth: auth::Auth,
}

impl RealGithubClient {
	pub fn new(github_token: String) -> Self {
		Self {
			http_client: crate::client_builder().build().expect("default client config"),
			auth: auth::Auth::Token(github_token),
		}
	}

	/// As a GitHub App: each account it acts on through that account's installation.
	pub fn app(id: u64, private_key: &str) -> Result<Self, GithubError> {
		Ok(Self {
			http_client: crate::client_builder().build().expect("default client config"),
			auth: auth::Auth::App(auth::App::try_new(id, private_key)?),
		})
	}

	/// The account is the one the url addresses (`/repos/{owner}/…`, `/orgs/{org}/…`).
	fn request(&self, method: reqwest::Method, url: &str) -> Request<'_> {
		let path = url.strip_prefix("https://api.github.com/").expect("REST calls go to the API");
		let owner = path
			.split_once('/')
			.and_then(|(scope, rest)| matches!(scope, "repos" | "orgs").then(|| rest.split('/').next()).flatten());
		self.request_as(method, url, owner)
	}

	fn request_as(&self, method: reqwest::Method, url: &str, owner: Option<&str>) -> Request<'_> {
		Request {
			client: self,
			owner: owner.map(str::to_owned),
			builder: self.http_client.request(method, url).header("User-Agent", "Rust Github Client"),
		}
	}

	fn get(&self, url: &str) -> Request<'_> {
		self.request(reqwest::Method::GET, url)
	}

	fn post(&self, url: &str) -> Request<'_> {
		self.request(reqwest::Method::POST, url)
	}

	fn patch(&self, url: &str) -> Request<'_> {
		self.request(reqwest::Method::PATCH, url)
	}

	fn delete(&self, url: &str) -> Request<'_> {
		self.request(reqwest::Method::DELETE, url)
	}

	//#[codestyle::skip(swappable-args)]
	/// Send a PATCH request with JSON body, returning an error on non-success status
	async fn patch_json(&self, url: &str, json: &serde_json::Value, error_context: &str) -> Result<(), GithubError> {
		let res = self.patch(url).json(json).send().await?;

		if !res.status().is_success() {
			let status = res.status();
			let body = res.text().await.unwrap_or_default();
			return Err(GithubError::new_api(status, body, error_context.to_string()));
		}

		Ok(())
	}

	//#[codestyle::skip(swappable-args)]
	/// Run a GraphQL request, surfacing both HTTP and GraphQL-level errors. `repo` is what it acts on, so an App picks its owner's installation.
	async fn graphql(&self, repo: RepoInfo, query: &str, variables: serde_json::Value, error_context: &str) -> Result<serde_json::Value, GithubError> {
		let res = self
			.request_as(reqwest::Method::POST, "https://api.github.com/graphql", Some(repo.owner().expect("github repo")))
			.json(&serde_json::json!({ "query": query, "variables": variables }))
			.send()
			.await?;
		if !res.status().is_success() {
			let status = res.status();
			let body = res.text().await.unwrap_or_default();
			return Err(GithubError::new_api(status, body, error_context.to_string()));
		}
		let response: serde_json::Value = res.json().await?;
		if let Some(errors) = response.get("errors") {
			return Err(GithubError::new_graphql(errors.to_string()));
		}
		Ok(response)
	}

	//#[codestyle::skip(swappable-args)]
	/// Send a POST request with JSON body, returning an error on non-success status
	async fn post_json(&self, url: &str, json: &serde_json::Value, error_context: &str) -> Result<(), GithubError> {
		let res = self.post(url).json(json).send().await?;

		if !res.status().is_success() {
			let status = res.status();
			let body = res.text().await.unwrap_or_default();
			return Err(GithubError::new_api(status, body, error_context.to_string()));
		}

		Ok(())
	}
}

/// Decorates any `GithubClient`, absorbing momentary blips (5xx/429/network) by retrying before
/// they surface, and capping requests in flight so concurrent loads stay under GitHub's secondary
/// rate limit. Wrapping the client once covers every method — no per-call site edits.
pub struct RetryingGithubClient {
	inner: BoxedGithubClient,
	in_flight: tokio::sync::Semaphore,
}
impl RetryingGithubClient {
	pub fn new(inner: BoxedGithubClient, max_in_flight: usize) -> Self {
		assert!(max_in_flight > 0, "a zero cap would deadlock every request");
		Self {
			inner,
			in_flight: tokio::sync::Semaphore::new(max_in_flight),
		}
	}
}

/// Cheap reachability probe: is GitHub responding? A transient error (5xx/429/network) ⇒ `false`
/// (defer the run to local offline mode); any HTTP response — even a 4xx — ⇒ `true`. Probes
/// `/user` (no repo needed) through the global client, so it inherits the retry decorator.
pub async fn reachable() -> bool {
	match client::get() {
		Ok(c) => match c.fetch_authenticated_user().await {
			Ok(_) => true,
			Err(e) => !e.is_transient(),
		},
		Err(_) => false,
	}
}

/// Parse a Github issue URL and extract owner, repo, and issue number.
/// Supports formats like:
/// - <https://github.com/owner/repo/issues/123>
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

/// A request whose token is picked at `send`, by the account it acts on.
struct Request<'a> {
	client: &'a RealGithubClient,
	owner: Option<String>,
	builder: reqwest::RequestBuilder,
}
impl Request<'_> {
	fn json<T: serde::Serialize + ?Sized>(self, json: &T) -> Self {
		Self {
			builder: self.builder.json(json),
			..self
		}
	}

	async fn send(self) -> Result<reqwest::Response, GithubError> {
		let token = self.client.auth.token(&self.client.http_client, self.owner.as_deref()).await?;
		Ok(self.builder.bearer_auth(token).send().await?)
	}
}

#[async_trait]
impl GithubClient for RealGithubClient {
	async fn fetch_authenticated_user(&self) -> Result<String, GithubError> {
		if let auth::Auth::App(app) = &self.auth {
			return app.login(&self.http_client).await;
		}
		let res = self.get("https://api.github.com/user").send().await?;

		if !res.status().is_success() {
			let status = res.status();
			let body = res.text().await.unwrap_or_default();
			return Err(GithubError::new_api(status, body, "Failed to fetch authenticated user".to_string()));
		}

		let user = res.json::<GithubUser>().await?;
		Ok(user.login)
	}

	async fn fetch_issue(&self, repo: RepoInfo, issue_number: u64) -> Result<GithubIssue, GithubError> {
		let url = format!("https://api.github.com/repos/{}/{}/issues/{issue_number}", repo.owner().expect("github repo"), repo.repo());
		let res = self.get(&url).send().await?;

		if !res.status().is_success() {
			let status = res.status();
			let body = res.text().await.unwrap_or_default();
			return Err(GithubError::new_api(status, body, "Failed to fetch issue".to_string()));
		}

		let issue = res.json::<GithubIssue>().await?;
		Ok(issue)
	}

	async fn fetch_full_issue(&self, repo: RepoInfo, issue_number: u64) -> Result<FetchedIssue, GithubError> {
		let query = format!("query($owner: String!, $repo: String!, $number: Int!) {{ repository(owner: $owner, name: $repo) {{ issue(number: $number) {{ ...Full }} }} }} {FULL_ISSUE}");
		let variables = serde_json::json!({ "owner": repo.owner().expect("github repo"), "repo": repo.repo(), "number": issue_number as i64 });
		let context = format!("fetch {repo}#{issue_number}");
		let mut response = self.graphql(repo, &query, variables, &context).await.map_err(|e| not_found_as_404(e, &context))?;
		let issue = response.pointer_mut("/data/repository/issue").map(serde_json::Value::take).unwrap_or_default();
		if issue.is_null() {
			return Err(GithubError::new_api(reqwest::StatusCode::NOT_FOUND, response.to_string(), context));
		}
		serde_json::from_value::<GqlIssue>(issue)
			.map_err(|e| GithubError::new_graphql(format!("{context}: {e}")))?
			.into_fetched()
	}

	async fn update_issue_body(&self, repo: RepoInfo, issue_number: u64, body: &str) -> Result<(), GithubError> {
		let url = format!("https://api.github.com/repos/{}/{}/issues/{issue_number}", repo.owner().expect("github repo"), repo.repo());
		self.patch_json(&url, &serde_json::json!({ "body": body }), "Failed to update issue body").await
	}

	async fn update_issue_title(&self, repo: RepoInfo, issue_number: u64, title: &str) -> Result<(), GithubError> {
		let url = format!("https://api.github.com/repos/{}/{}/issues/{issue_number}", repo.owner().expect("github repo"), repo.repo());
		self.patch_json(&url, &serde_json::json!({ "title": title }), "Failed to update issue title").await
	}

	async fn update_issue_state(&self, repo: RepoInfo, issue_number: u64, state: &CloseState) -> Result<(), GithubError> {
		let url = format!("https://api.github.com/repos/{}/{}/issues/{issue_number}", repo.owner().expect("github repo"), repo.repo());
		let json = serde_json::json!({ "state": state.to_github_state(), "state_reason": state.to_github_state_reason() });
		self.patch_json(&url, &json, "Failed to update issue state").await
	}

	async fn set_labels(&self, repo: RepoInfo, issue_number: u64, labels: &[String]) -> Result<(), GithubError> {
		let url = format!("https://api.github.com/repos/{}/{}/issues/{issue_number}", repo.owner().expect("github repo"), repo.repo());
		self.patch_json(&url, &serde_json::json!({ "labels": labels }), "Failed to set labels").await
	}

	async fn set_issue_milestone(&self, repo: RepoInfo, issue_number: u64, milestone: Option<u64>) -> Result<(), GithubError> {
		let url = format!("https://api.github.com/repos/{}/{}/issues/{issue_number}", repo.owner().expect("github repo"), repo.repo());
		let json = serde_json::json!({ "milestone": milestone });
		self.patch_json(&url, &json, "Failed to set issue milestone").await
	}

	async fn update_comment(&self, repo: RepoInfo, comment_id: u64, body: &str) -> Result<(), GithubError> {
		let url = format!("https://api.github.com/repos/{}/{}/issues/comments/{comment_id}", repo.owner().expect("github repo"), repo.repo());
		self.patch_json(&url, &serde_json::json!({ "body": body }), "Failed to update comment").await
	}

	async fn create_comment(&self, repo: RepoInfo, issue_number: u64, body: &str) -> Result<GithubComment, GithubError> {
		let url = format!(
			"https://api.github.com/repos/{}/{}/issues/{issue_number}/comments",
			repo.owner().expect("github repo"),
			repo.repo()
		);
		let res = self.post(&url).json(&serde_json::json!({ "body": body })).send().await?;
		if !res.status().is_success() {
			let status = res.status();
			let body = res.text().await.unwrap_or_default();
			return Err(GithubError::new_api(status, body, "Failed to create comment".to_string()));
		}
		Ok(res.json::<GithubComment>().await?)
	}

	async fn delete_comment(&self, repo: RepoInfo, comment_id: u64) -> Result<(), GithubError> {
		let url = format!("https://api.github.com/repos/{}/{}/issues/comments/{comment_id}", repo.owner().expect("github repo"), repo.repo());
		let res = self.delete(&url).send().await?;

		if !res.status().is_success() {
			let status = res.status();
			let body = res.text().await.unwrap_or_default();
			return Err(GithubError::new_api(status, body, "Failed to delete comment".to_string()));
		}

		Ok(())
	}

	async fn create_issue(&self, repo: RepoInfo, title: &str, body: &str) -> Result<CreatedIssue, GithubError> {
		let url = format!("https://api.github.com/repos/{}/{}/issues", repo.owner().expect("github repo"), repo.repo());
		let res = self.post(&url).json(&serde_json::json!({ "title": title, "body": body })).send().await?;

		if !res.status().is_success() {
			let status = res.status();
			let body = res.text().await.unwrap_or_default();
			return Err(GithubError::new_api(status, body, "Failed to create issue".to_string()));
		}

		let issue = res.json::<CreatedIssue>().await?;
		Ok(issue)
	}

	async fn add_sub_issue(&self, repo: RepoInfo, parent_issue_number: u64, child_issue_id: u64) -> Result<(), GithubError> {
		let url = format!(
			"https://api.github.com/repos/{}/{}/issues/{parent_issue_number}/sub_issues",
			repo.owner().expect("github repo"),
			repo.repo()
		);
		self.post_json(&url, &serde_json::json!({ "sub_issue_id": child_issue_id }), "Failed to add sub-issue").await
	}

	async fn remove_sub_issue(&self, repo: RepoInfo, parent_issue_number: u64, child_issue_id: u64) -> Result<(), GithubError> {
		let url = format!(
			"https://api.github.com/repos/{}/{}/issues/{parent_issue_number}/sub_issue",
			repo.owner().expect("github repo"),
			repo.repo()
		);
		let res = self.delete(&url).json(&serde_json::json!({ "sub_issue_id": child_issue_id })).send().await?;

		if !res.status().is_success() {
			let status = res.status();
			let body = res.text().await.unwrap_or_default();
			return Err(GithubError::new_api(status, body, "Failed to remove sub-issue".to_string()));
		}

		Ok(())
	}

	async fn add_blocked_by(&self, repo: RepoInfo, issue_number: u64, blocker_id: u64) -> Result<(), GithubError> {
		let url = format!(
			"https://api.github.com/repos/{}/{}/issues/{issue_number}/dependencies/blocked_by",
			repo.owner().expect("github repo"),
			repo.repo()
		);
		self.post_json(&url, &serde_json::json!({ "issue_id": blocker_id }), "Failed to add blocked-by dependency").await
	}

	async fn remove_blocked_by(&self, repo: RepoInfo, issue_number: u64, blocker_id: u64) -> Result<(), GithubError> {
		let url = format!(
			"https://api.github.com/repos/{}/{}/issues/{issue_number}/dependencies/blocked_by/{blocker_id}",
			repo.owner().expect("github repo"),
			repo.repo()
		);
		let res = self.delete(&url).send().await?;
		if !res.status().is_success() {
			let status = res.status();
			let body = res.text().await.unwrap_or_default();
			return Err(GithubError::new_api(status, body, "Failed to remove blocked-by dependency".to_string()));
		}
		Ok(())
	}

	async fn repo_exists(&self, repo: RepoInfo) -> Result<bool, GithubError> {
		let url = format!("https://api.github.com/repos/{}/{}", repo.owner().expect("github repo"), repo.repo());
		let res = self.get(&url).send().await?;
		Ok(res.status().is_success())
	}

	async fn list_milestones(&self, repo: RepoInfo) -> Result<Vec<GithubMilestone>, GithubError> {
		let url = format!("https://api.github.com/repos/{}/{}/milestones?per_page=100", repo.owner().expect("github repo"), repo.repo());
		let res = self.get(&url).send().await?;
		if !res.status().is_success() {
			let status = res.status();
			let body = res.text().await.unwrap_or_default();
			return Err(GithubError::new_api(status, body, "list milestones".to_string()));
		}
		Ok(res.json::<Vec<GithubMilestone>>().await?)
	}

	async fn get_milestone(&self, repo: RepoInfo, number: u64) -> Result<GithubMilestone, GithubError> {
		let url = format!("https://api.github.com/repos/{}/{}/milestones/{number}", repo.owner().expect("github repo"), repo.repo());
		let res = self.get(&url).send().await?;
		if !res.status().is_success() {
			let status = res.status();
			let body = res.text().await.unwrap_or_default();
			return Err(GithubError::new_api(status, body, "get milestone".to_string()));
		}
		Ok(res.json::<GithubMilestone>().await?)
	}

	async fn list_milestone_issues(&self, repo: RepoInfo, milestone_number: u64) -> Result<Vec<GithubIssue>, GithubError> {
		let url = format!(
			"https://api.github.com/repos/{}/{}/issues?milestone={milestone_number}&state=all&per_page=100",
			repo.owner().expect("github repo"),
			repo.repo()
		);
		let res = self.get(&url).send().await?;
		if !res.status().is_success() {
			let status = res.status();
			let body = res.text().await.unwrap_or_default();
			return Err(GithubError::new_api(status, body, "list milestone issues".to_string()));
		}
		Ok(res.json::<Vec<GithubIssue>>().await?)
	}

	async fn create_milestone(&self, repo: RepoInfo, title: &str, description: &str, closed: bool) -> Result<(), GithubError> {
		let url = format!("https://api.github.com/repos/{}/{}/milestones", repo.owner().expect("github repo"), repo.repo());
		let mut body = serde_json::json!({ "title": title, "description": description });
		if closed {
			body["state"] = serde_json::Value::String("closed".to_string());
		}
		self.post_json(&url, &body, "create milestone").await
	}

	async fn update_milestone(&self, repo: RepoInfo, number: u64, description: &str, due_on: Option<jiff::Timestamp>) -> Result<(), GithubError> {
		let url = format!("https://api.github.com/repos/{}/{}/milestones/{number}", repo.owner().expect("github repo"), repo.repo());
		let mut body = serde_json::json!({ "description": description });
		if let Some(date) = due_on {
			body["due_on"] = serde_json::Value::String(date.to_string());
		}
		self.patch_json(&url, &body, "update milestone").await
	}

	async fn transfer_issue(&self, repo: RepoInfo, issue_number: u64, target: RepoInfo) -> Result<u64, GithubError> {
		let query = r#"
			query($owner: String!, $repo: String!, $number: Int!, $target: String!) {
				repository(owner: $owner, name: $repo) { issue(number: $number) { id } }
				target: repository(owner: $owner, name: $target) { id }
			}
		"#;
		let ids = self
			.graphql(
				repo,
				query,
				serde_json::json!({ "owner": repo.owner().expect("github repo"), "repo": repo.repo(), "number": issue_number as i64, "target": target.repo() }),
				"resolve transfer ids",
			)
			.await?;
		let id = |pointer: &str| {
			ids.pointer(pointer)
				.and_then(|v| v.as_str())
				.map(str::to_owned)
				.ok_or_else(|| GithubError::new_graphql(format!("no {pointer} in {ids}")))
		};
		let mutation = r#"
			mutation($issue: ID!, $target: ID!) {
				transferIssue(input: { issueId: $issue, repositoryId: $target }) { issue { number } }
			}
		"#;
		let moved = self
			.graphql(
				repo,
				mutation,
				serde_json::json!({ "issue": id("/data/repository/issue/id")?, "target": id("/data/target/id")? }),
				"transfer issue",
			)
			.await?;
		moved
			.pointer("/data/transferIssue/issue/number")
			.and_then(|v| v.as_u64())
			.ok_or_else(|| GithubError::new_graphql(format!("transferIssue returned no number: {moved}")))
	}

	async fn list_org_repos(&self, org: &str) -> Result<Vec<RepoInfo>, GithubError> {
		#[derive(Deserialize)]
		struct Repo {
			name: String,
			archived: bool,
		}
		let mut repos = Vec::new();
		for page in 1.. {
			let res = self.get(&format!("https://api.github.com/orgs/{org}/repos?per_page=100&page={page}")).send().await?;
			if !res.status().is_success() {
				let status = res.status();
				let body = res.text().await.unwrap_or_default();
				return Err(GithubError::new_api(status, body, format!("list repos of {org}")));
			}
			let batch = res.json::<Vec<Repo>>().await?;
			let last = batch.len() < 100;
			repos.extend(batch.into_iter().filter(|r| !r.archived).map(|r| RepoInfo::new(org, &r.name)));
			if last {
				break;
			}
		}
		Ok(repos)
	}

	async fn list_repo_issues(&self, repo: RepoInfo, since: Option<jiff::Timestamp>, after: Option<&str>) -> Result<IssuePage, GithubError> {
		// 50 a page: GitHub charges a page by the connections it opens, ~4 points here
		let query = format!(
			"query($owner: String!, $repo: String!, $since: DateTime, $after: String) {{ repository(owner: $owner, name: $repo) {{ issues(first: 50, after: $after, orderBy: {{field: UPDATED_AT, direction: ASC}}, filterBy: {{since: $since}}) {{ pageInfo {{ hasNextPage endCursor }} nodes {{ ...Full }} }} }} }} {FULL_ISSUE}"
		);
		let variables = serde_json::json!({ "owner": repo.owner().expect("github repo"), "repo": repo.repo(), "since": since, "after": after });
		let context = format!("list issues of {repo}");
		let mut response = self.graphql(repo, &query, variables, &context).await.map_err(|e| not_found_as_404(e, &context))?;
		#[derive(Deserialize)]
		#[serde(rename_all = "camelCase")]
		struct PageInfo {
			has_next_page: bool,
			end_cursor: Option<String>,
		}
		#[derive(Deserialize)]
		#[serde(rename_all = "camelCase")]
		struct Page {
			page_info: PageInfo,
			nodes: Vec<GqlIssue>,
		}
		let page: Page = serde_json::from_value(response.pointer_mut("/data/repository/issues").map(serde_json::Value::take).unwrap_or_default())
			.map_err(|e| GithubError::new_graphql(format!("{context}: {e} in {response}")))?;
		Ok(IssuePage {
			issues: page.nodes.into_iter().map(GqlIssue::into_fetched).collect::<Result<_, _>>()?,
			next: match page.page_info.has_next_page {
				true => Some(
					page.page_info
						.end_cursor
						.ok_or_else(|| GithubError::new_graphql(format!("{context}: a next page without a cursor")))?,
				),
				false => None,
			},
		})
	}

	async fn list_repo_prs(&self, repo: RepoInfo, after: Option<&str>) -> Result<PrPage, GithubError> {
		let query = "query($owner: String!, $repo: String!, $after: String) { repository(owner: $owner, name: $repo) { pullRequests(first: 50, after: $after, orderBy: {field: UPDATED_AT, direction: DESC}) { pageInfo { hasNextPage endCursor } nodes {
			number title body state isDraft createdAt updatedAt closedAt
			author { login }
			labels(first: 100) { totalCount nodes { name } }
			assignees(first: 100) { totalCount nodes { login } }
			closingIssuesReferences(first: 100) { totalCount nodes { url } }
			reviewRequests(first: 100) { totalCount nodes { requestedReviewer { ... on User { login } ... on Bot { login } ... on Mannequin { login } ... on Team { combinedSlug } } } }
			latestReviews(first: 100) { totalCount nodes { state author { login } } }
		} } } }";
		let variables = serde_json::json!({ "owner": repo.owner().expect("github repo"), "repo": repo.repo(), "after": after });
		let context = format!("list pull requests of {repo}");
		let mut response = self.graphql(repo, query, variables, &context).await.map_err(|e| not_found_as_404(e, &context))?;
		#[derive(Deserialize)]
		#[serde(rename_all = "camelCase")]
		struct PageInfo {
			has_next_page: bool,
			end_cursor: Option<String>,
		}
		#[derive(Deserialize)]
		#[serde(rename_all = "camelCase")]
		struct Page {
			page_info: PageInfo,
			nodes: Vec<GqlPullRequest>,
		}
		let page: Page = serde_json::from_value(response.pointer_mut("/data/repository/pullRequests").map(serde_json::Value::take).unwrap_or_default())
			.map_err(|e| GithubError::new_graphql(format!("{context}: {e} in {response}")))?;
		Ok(PrPage {
			prs: page.nodes.into_iter().map(GqlPullRequest::into_pr).collect::<Result<_, _>>()?,
			next: match page.page_info.has_next_page {
				true => Some(
					page.page_info
						.end_cursor
						.ok_or_else(|| GithubError::new_graphql(format!("{context}: a next page without a cursor")))?,
				),
				false => None,
			},
		})
	}

	async fn file_history(&self, repo: RepoInfo, path: &str, branch: &str) -> Result<Vec<FileCommit>, GithubError> {
		let query = "query($owner: String!, $repo: String!, $ref: String!, $path: String!, $after: String) { repository(owner: $owner, name: $repo) { object(expression: $ref) { ... on Commit { history(path: $path, first: 100, after: $after) { pageInfo { hasNextPage endCursor } nodes {
			oid committedDate
			author { name user { login } }
			file(path: $path) { object { ... on Blob { text } } }
		} } } } } }";
		#[derive(Deserialize)]
		#[serde(rename_all = "camelCase")]
		struct PageInfo {
			has_next_page: bool,
			end_cursor: Option<String>,
		}
		#[derive(Deserialize)]
		#[serde(rename_all = "camelCase")]
		struct Page {
			page_info: PageInfo,
			nodes: Vec<Node>,
		}
		#[derive(Deserialize)]
		#[serde(rename_all = "camelCase")]
		struct Node {
			oid: String,
			committed_date: jiff::Timestamp,
			author: Author,
			file: Option<File>,
		}
		#[derive(Deserialize)]
		struct Author {
			name: String,
			user: Option<User>,
		}
		#[derive(Deserialize)]
		struct User {
			login: String,
		}
		#[derive(Deserialize)]
		struct File {
			object: Blob,
		}
		#[derive(Deserialize)]
		struct Blob {
			text: String,
		}
		let context = format!("history of {path} on {repo}@{branch}");
		let mut commits = Vec::new();
		let mut after: Option<String> = None;
		loop {
			let variables = serde_json::json!({ "owner": repo.owner().expect("github repo"), "repo": repo.repo(), "ref": branch, "path": path, "after": after });
			let mut response = self.graphql(repo, query, variables, &context).await.map_err(|e| not_found_as_404(e, &context))?;
			let page = response
				.pointer_mut("/data/repository/object/history")
				.map(serde_json::Value::take)
				.ok_or_else(|| GithubError::new_graphql(format!("{context}: no branch {branch} in {response}")))?;
			let page: Page = serde_json::from_value(page).map_err(|e| GithubError::new_graphql(format!("{context}: {e}")))?;
			commits.extend(page.nodes.into_iter().map(|n| FileCommit {
				sha: n.oid,
				author: n.author.user.map_or(n.author.name, |u| u.login),
				at: n.committed_date,
				content: n.file.map(|f| f.object.text),
			}));
			match page.page_info.has_next_page {
				true =>
					after = Some(
						page.page_info
							.end_cursor
							.ok_or_else(|| GithubError::new_graphql(format!("{context}: a next page without a cursor")))?,
					),
				false => break,
			}
		}
		commits.reverse(); // GitHub lists newest first
		Ok(commits)
	}

	async fn propose_file(&self, repo: RepoInfo, proposal: &FileProposal) -> Result<String, GithubError> {
		use base64::Engine as _;
		let FileProposal {
			base,
			head,
			path,
			content,
			title,
			body,
		} = proposal;
		let api = format!("https://api.github.com/repos/{}/{}", repo.owner().expect("github repo"), repo.repo());
		async fn read<T: serde::de::DeserializeOwned>(res: reqwest::Response, context: &str) -> Result<T, GithubError> {
			if !res.status().is_success() {
				let status = res.status();
				let body = res.text().await.unwrap_or_default();
				return Err(GithubError::new_api(status, body, context.to_string()));
			}
			Ok(res.json().await?)
		}
		#[derive(Deserialize)]
		struct Ref {
			object: Sha,
		}
		#[derive(Deserialize)]
		struct Sha {
			sha: String,
		}
		#[derive(Deserialize)]
		struct Pull {
			html_url: String,
		}
		let at: Ref = read(self.get(&format!("{api}/git/ref/heads/{base}")).send().await?, &format!("read {base} of {repo}")).await?;
		let _: serde_json::Value = read(
			self.post(&format!("{api}/git/refs"))
				.json(&serde_json::json!({ "ref": format!("refs/heads/{head}"), "sha": at.object.sha }))
				.send()
				.await?,
			&format!("branch {head} off {base} of {repo}"),
		)
		.await?;
		let blob: Sha = read(self.get(&format!("{api}/contents/{path}?ref={base}")).send().await?, &format!("read {path} of {repo}")).await?;
		let _: serde_json::Value = read(
			self.request(reqwest::Method::PUT, &format!("{api}/contents/{path}"))
				.json(&serde_json::json!({ "message": title, "content": base64::engine::general_purpose::STANDARD.encode(content), "sha": blob.sha, "branch": head }))
				.send()
				.await?,
			&format!("write {path} on {head} of {repo}"),
		)
		.await?;
		let pull: Pull = read(
			self.post(&format!("{api}/pulls"))
				.json(&serde_json::json!({ "title": title, "head": head, "base": base, "body": body }))
				.send()
				.await?,
			&format!("open a pull request onto {base} of {repo}"),
		)
		.await?;
		Ok(pull.html_url)
	}

	async fn list_labels(&self, repo: RepoInfo) -> Result<BTreeMap<String, Color>, GithubError> {
		#[derive(Deserialize)]
		struct Label {
			name: String,
			color: Color,
		}
		let mut labels = BTreeMap::new();
		for page in 1.. {
			let url = format!(
				"https://api.github.com/repos/{}/{}/labels?per_page=100&page={page}",
				repo.owner().expect("github repo"),
				repo.repo()
			);
			let res = self.get(&url).send().await?;
			if !res.status().is_success() {
				let status = res.status();
				let body = res.text().await.unwrap_or_default();
				return Err(GithubError::new_api(status, body, "list labels".to_string()));
			}
			let page = res.json::<Vec<Label>>().await?;
			let last = page.len() < 100;
			labels.extend(page.into_iter().map(|l| (l.name, l.color)));
			if last {
				break;
			}
		}
		Ok(labels)
	}

	async fn list_assignable(&self, repo: RepoInfo) -> Result<Vec<String>, GithubError> {
		let mut logins = Vec::new();
		for page in 1.. {
			let url = format!(
				"https://api.github.com/repos/{}/{}/assignees?per_page=100&page={page}",
				repo.owner().expect("github repo"),
				repo.repo()
			);
			let res = self.get(&url).send().await?;
			if !res.status().is_success() {
				let status = res.status();
				let body = res.text().await.unwrap_or_default();
				return Err(GithubError::new_api(status, body, "list assignable users".to_string()));
			}
			let page = res.json::<Vec<GithubUser>>().await?;
			let last = page.len() < 100;
			logins.extend(page.into_iter().map(|u| u.login));
			if last {
				break;
			}
		}
		Ok(logins)
	}

	async fn request_review(&self, repo: RepoInfo, number: u64, reviewer: &str, on: bool) -> Result<(), GithubError> {
		let url = format!(
			"https://api.github.com/repos/{}/{}/pulls/{number}/requested_reviewers",
			repo.owner().expect("github repo"),
			repo.repo()
		);
		let json = match reviewer.split_once('/') {
			Some((_, team)) => serde_json::json!({ "reviewers": [], "team_reviewers": [team] }),
			None => serde_json::json!({ "reviewers": [reviewer] }),
		};
		if on {
			return self.post_json(&url, &json, "Failed to request a review").await;
		}
		let res = self.delete(&url).json(&json).send().await?;
		if !res.status().is_success() {
			let status = res.status();
			let body = res.text().await.unwrap_or_default();
			return Err(GithubError::new_api(status, body, "Failed to withdraw a review request".to_string()));
		}
		Ok(())
	}

	/// Through the add/remove endpoints rather than a PATCH, which rejects the whole set over one login the repo can't take.
	async fn set_assignees(&self, repo: RepoInfo, issue_number: u64, assignees: &[String]) -> Result<(), GithubError> {
		let url = format!(
			"https://api.github.com/repos/{}/{}/issues/{issue_number}/assignees",
			repo.owner().expect("github repo"),
			repo.repo()
		);
		let current: Vec<String> = self.fetch_issue(repo, issue_number).await?.assignees.into_iter().map(|u| u.login).collect();
		let gone: Vec<&String> = current.iter().filter(|a| !assignees.contains(a)).collect();
		if !gone.is_empty() {
			let res = self.delete(&url).json(&serde_json::json!({ "assignees": gone })).send().await?;
			if !res.status().is_success() {
				let status = res.status();
				let body = res.text().await.unwrap_or_default();
				return Err(GithubError::new_api(status, body, "Failed to remove assignees".to_string()));
			}
		}
		let new: Vec<&String> = assignees.iter().filter(|a| !current.contains(a)).collect();
		match new.is_empty() {
			true => Ok(()),
			false => self.post_json(&url, &serde_json::json!({ "assignees": new }), "Failed to add assignees").await,
		}
	}
}

//==============================================================================
// GraphQL issue reads
//==============================================================================

/// GraphQL names a missing repo or issue `NOT_FOUND` where REST answers 404.
fn not_found_as_404(e: GithubError, context: &str) -> GithubError {
	match &e {
		GithubError::Graphql { msg, .. } if msg.contains(r#""type":"NOT_FOUND""#) => GithubError::new_api(reqwest::StatusCode::NOT_FOUND, msg.clone(), context.to_string()),
		_ => e,
	}
}

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
struct Connection<T> {
	total_count: usize,
	nodes: Vec<T>,
}
impl<T> Connection<T> {
	fn whole(self, what: &str, number: u64) -> Result<Vec<T>, GithubError> {
		match self.nodes.len() == self.total_count {
			true => Ok(self.nodes),
			false => Err(GithubError::new_other(format!("#{number} has {} {what}, more than one read takes", self.total_count))),
		}
	}
}

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
struct GqlComment {
	database_id: u64,
	body: String,
	author: Option<GithubUser>,
	created_at: String,
	updated_at: String,
}

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
struct GqlSubIssue {
	database_id: u64,
	number: u64,
	state_reason: Option<String>,
}

#[derive(Deserialize)]
struct GqlNumber {
	number: u64,
}

#[derive(Deserialize)]
struct GqlUrl {
	url: String,
}

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
struct GqlPr {
	url: String,
	is_draft: bool,
	state: String,
}
impl TryFrom<GqlPr> for tedi_core::LinkedPr {
	type Error = GithubError;

	fn try_from(pr: GqlPr) -> Result<Self, GithubError> {
		let state = match (pr.state.as_str(), pr.is_draft) {
			("MERGED", _) => tedi_core::PrState::Merged,
			("CLOSED", _) => tedi_core::PrState::Closed,
			("OPEN", true) => tedi_core::PrState::Draft,
			("OPEN", false) => tedi_core::PrState::Open,
			(other, _) => return Err(GithubError::new_graphql(format!("unknown pull request state {other} of {}", pr.url))),
		};
		Ok(tedi_core::LinkedPr { url: pr.url, state })
	}
}

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
struct GqlPullRequest {
	number: u64,
	title: String,
	body: String,
	state: String,
	is_draft: bool,
	created_at: jiff::Timestamp,
	updated_at: jiff::Timestamp,
	closed_at: Option<jiff::Timestamp>,
	author: Option<GithubUser>,
	labels: Connection<GithubLabel>,
	assignees: Connection<GithubUser>,
	closing_issues_references: Connection<GqlUrl>,
	review_requests: Connection<GqlReviewRequest>,
	latest_reviews: Connection<GqlReview>,
}
impl GqlPullRequest {
	fn into_pr(self) -> Result<tedi_core::PullRequest, GithubError> {
		let n = self.number;
		let state = match (self.state.as_str(), self.is_draft) {
			("MERGED", _) => tedi_core::PrState::Merged,
			("CLOSED", _) => tedi_core::PrState::Closed,
			("OPEN", true) => tedi_core::PrState::Draft,
			("OPEN", false) => tedi_core::PrState::Open,
			(other, _) => return Err(GithubError::new_graphql(format!("unknown pull request state {other} of #{n}"))),
		};
		Ok(tedi_core::PullRequest {
			number: n,
			title: self.title,
			body: self.body,
			state,
			author: self.author.map_or_else(|| "ghost".into(), |a| a.login), // a deleted account, as REST names it
			assignees: self.assignees.whole("assignees", n)?.into_iter().map(|u| u.login).collect(),
			labels: self.labels.whole("labels", n)?.into_iter().map(|l| l.name).collect(),
			closes: self
				.closing_issues_references
				.whole("closing issues", n)?
				.into_iter()
				.map(|u| tedi_core::IssueLink::parse(&u.url).ok_or_else(|| GithubError::new_graphql(format!("#{n} closes {}, which is not an issue", u.url))))
				.collect::<Result<_, _>>()?,
			requested: self
				.review_requests
				.whole("review requests", n)?
				.into_iter()
				.filter_map(|r| Some(r.requested_reviewer?.login))
				.collect(),
			reviews: self
				.latest_reviews
				.whole("reviews", n)?
				.into_iter()
				.filter(|r| r.state != "PENDING") // the token user's own draft, not submitted
				.map(|r| {
					let verdict = r.state.parse().map_err(|_| GithubError::new_graphql(format!("unknown review state {} on #{n}", r.state)))?;
					Ok((r.author.map_or_else(|| "ghost".into(), |a| a.login), verdict))
				})
				.collect::<Result<_, GithubError>>()?,
			updated_at: self.updated_at,
			created_at: Some(self.created_at),
			closed_at: self.closed_at,
		})
	}
}

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
struct GqlReviewRequest {
	/// Null on a team the token can't see.
	requested_reviewer: Option<GqlReviewer>,
}
#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
struct GqlReviewer {
	#[serde(alias = "combinedSlug")]
	login: String,
}
#[derive(Deserialize)]
struct GqlReview {
	state: String,
	author: Option<GithubUser>,
}

#[derive(Deserialize)]
struct Nodes<T> {
	nodes: Vec<T>,
}

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
struct GqlIssue {
	database_id: u64,
	number: u64,
	title: String,
	body: String,
	url: String,
	state: String,
	state_reason: Option<String>,
	updated_at: jiff::Timestamp,
	last_edited_at: Option<jiff::Timestamp>,
	created_at: jiff::Timestamp,
	author: Option<GithubUser>,
	labels: Connection<GithubLabel>,
	assignees: Connection<GithubUser>,
	comments: Connection<GqlComment>,
	sub_issues: Connection<GqlSubIssue>,
	parent: Option<GqlNumber>,
	blocked_by: Connection<GqlUrl>,
	blocking: Connection<GqlUrl>,
	closed_by_pull_requests_references: Connection<GqlPr>,
	timeline_items: Nodes<serde_json::Value>,
}
impl GqlIssue {
	fn into_fetched(self) -> Result<FetchedIssue, GithubError> {
		let n = self.number;
		// a deleted account: REST names it `ghost`, GraphQL leaves it null
		let ghost = || GithubUser { login: "ghost".into() };
		let links = |c: Connection<GqlUrl>, what: &str| -> Result<Vec<tedi_core::IssueLink>, GithubError> {
			c.whole(what, n)?
				.into_iter()
				.map(|u| tedi_core::IssueLink::parse(&u.url).ok_or_else(|| GithubError::new_graphql(format!("#{n}'s {what} {} is not an issue", u.url))))
				.collect()
		};
		let mut timeline = GraphqlTimeline {
			// never edited since it was opened
			description: Some(self.last_edited_at.unwrap_or(self.created_at)),
			blocked_by: links(self.blocked_by, "blocked by")?,
			blocking: links(self.blocking, "blocking")?,
			prs: self
				.closed_by_pull_requests_references
				.whole("closing pull requests", n)?
				.into_iter()
				.map(tedi_core::LinkedPr::try_from)
				.collect::<Result<_, _>>()?,
			..Default::default()
		};
		for node in self.timeline_items.nodes {
			let typename = node
				.get("__typename")
				.and_then(|v| v.as_str())
				.ok_or_else(|| GithubError::new_graphql(format!("#{n}: timeline node without __typename: {node}")))?;
			if let ("ConnectedEvent" | "DisconnectedEvent", Some(subject)) = (typename, node.get("subject"))
				&& subject.get("url").is_some()
			{
				let linked = tedi_core::LinkedPr::try_from(serde_json::from_value::<GqlPr>(subject.clone()).map_err(|e| GithubError::new_graphql(format!("#{n}: {e} in {subject}")))?)?;
				let at = timeline.prs.iter().position(|p| p.url == linked.url);
				match (typename, at) {
					("ConnectedEvent", None) => timeline.prs.push(linked),
					("DisconnectedEvent", Some(at)) => _ = timeline.prs.remove(at),
					_ => {}
				}
				continue;
			}
			let field = match typename {
				"RenamedTitleEvent" => &mut timeline.title,
				"LabeledEvent" | "UnlabeledEvent" => &mut timeline.labels,
				// tedi's blockers hold both the body and the `blocked by` links
				"BlockedByAddedEvent" | "BlockedByRemovedEvent" => &mut timeline.description,
				"AssignedEvent" | "UnassignedEvent" => &mut timeline.assignees,
				"ClosedEvent" | "ReopenedEvent" => &mut timeline.state,
				_ => continue, // a subject that is an issue, not a pull request, comes back as `{}`
			};
			let at: jiff::Timestamp = node
				.get("createdAt")
				.and_then(|v| v.as_str())
				.and_then(|s| s.parse().ok())
				.ok_or_else(|| GithubError::new_graphql(format!("#{n}: {typename} without a createdAt: {node}")))?;
			*field = (*field).max(Some(at));
		}
		Ok(FetchedIssue {
			issue: GithubIssue {
				id: self.database_id,
				number: n,
				title: self.title,
				body: (!self.body.is_empty()).then_some(self.body),
				labels: self.labels.whole("labels", n)?,
				user: self.author.unwrap_or_else(ghost),
				state: self.state.to_lowercase(),
				state_reason: self.state_reason.map(|r| r.to_lowercase()),
				assignees: self.assignees.whole("assignees", n)?,
				pull_request: None,
				html_url: self.url,
				created_at: self.created_at,
			},
			comments: self
				.comments
				.whole("comments", n)?
				.into_iter()
				.map(|c| GithubComment {
					id: c.database_id,
					body: (!c.body.is_empty()).then_some(c.body),
					user: c.author.unwrap_or_else(ghost),
					created_at: c.created_at,
					updated_at: c.updated_at,
				})
				.collect(),
			sub_issues: self
				.sub_issues
				.whole("sub-issues", n)?
				.into_iter()
				.map(|s| SubIssue {
					id: s.database_id,
					number: s.number,
					state_reason: s.state_reason.map(|r| r.to_lowercase()),
				})
				.collect(),
			parent: self.parent.map(|p| p.number),
			timeline,
			updated_at: self.updated_at,
		})
	}
}

//==============================================================================
// Retrying decorator
//==============================================================================

/// Re-run `$call` up to three extra times on a transient error, backing off 200→400→800ms.
/// `$call` is re-evaluated each attempt (all our arg types are `Copy` or `&`-borrowed).
macro_rules! retrying {
	($self:ident.inner.$method:ident($($arg:expr),*)) => {{
		let attempt = || async {
			let _permit = $self.in_flight.acquire().await.expect("semaphore is never closed");
			$self.inner.$method($($arg),*).await
		};
		let mut result = attempt().await;
		for delay_ms in [200u64, 400, 800] {
			match &result {
				Err(e) if GithubError::is_transient(e) => {
					tracing::warn!("transient GitHub error, retrying in {delay_ms}ms: {e}");
					tokio::time::sleep(std::time::Duration::from_millis(delay_ms)).await;
					result = attempt().await;
				}
				_ => break,
			}
		}
		result
	}};
}

#[async_trait]
impl GithubClient for RetryingGithubClient {
	async fn fetch_authenticated_user(&self) -> Result<String, GithubError> {
		retrying!(self.inner.fetch_authenticated_user())
	}

	async fn fetch_issue(&self, repo: RepoInfo, issue_number: u64) -> Result<GithubIssue, GithubError> {
		retrying!(self.inner.fetch_issue(repo, issue_number))
	}

	async fn fetch_full_issue(&self, repo: RepoInfo, issue_number: u64) -> Result<FetchedIssue, GithubError> {
		retrying!(self.inner.fetch_full_issue(repo, issue_number))
	}

	async fn update_issue_body(&self, repo: RepoInfo, issue_number: u64, body: &str) -> Result<(), GithubError> {
		retrying!(self.inner.update_issue_body(repo, issue_number, body))
	}

	async fn update_issue_title(&self, repo: RepoInfo, issue_number: u64, title: &str) -> Result<(), GithubError> {
		retrying!(self.inner.update_issue_title(repo, issue_number, title))
	}

	async fn update_issue_state(&self, repo: RepoInfo, issue_number: u64, state: &CloseState) -> Result<(), GithubError> {
		retrying!(self.inner.update_issue_state(repo, issue_number, state))
	}

	async fn update_comment(&self, repo: RepoInfo, comment_id: u64, body: &str) -> Result<(), GithubError> {
		retrying!(self.inner.update_comment(repo, comment_id, body))
	}

	async fn create_comment(&self, repo: RepoInfo, issue_number: u64, body: &str) -> Result<GithubComment, GithubError> {
		retrying!(self.inner.create_comment(repo, issue_number, body))
	}

	async fn delete_comment(&self, repo: RepoInfo, comment_id: u64) -> Result<(), GithubError> {
		retrying!(self.inner.delete_comment(repo, comment_id))
	}

	async fn create_issue(&self, repo: RepoInfo, title: &str, body: &str) -> Result<CreatedIssue, GithubError> {
		retrying!(self.inner.create_issue(repo, title, body))
	}

	async fn add_sub_issue(&self, repo: RepoInfo, parent_issue_number: u64, child_issue_id: u64) -> Result<(), GithubError> {
		retrying!(self.inner.add_sub_issue(repo, parent_issue_number, child_issue_id))
	}

	async fn remove_sub_issue(&self, repo: RepoInfo, parent_issue_number: u64, child_issue_id: u64) -> Result<(), GithubError> {
		retrying!(self.inner.remove_sub_issue(repo, parent_issue_number, child_issue_id))
	}

	async fn add_blocked_by(&self, repo: RepoInfo, issue_number: u64, blocker_id: u64) -> Result<(), GithubError> {
		retrying!(self.inner.add_blocked_by(repo, issue_number, blocker_id))
	}

	async fn remove_blocked_by(&self, repo: RepoInfo, issue_number: u64, blocker_id: u64) -> Result<(), GithubError> {
		retrying!(self.inner.remove_blocked_by(repo, issue_number, blocker_id))
	}

	async fn set_labels(&self, repo: RepoInfo, issue_number: u64, labels: &[String]) -> Result<(), GithubError> {
		retrying!(self.inner.set_labels(repo, issue_number, labels))
	}

	async fn set_issue_milestone(&self, repo: RepoInfo, issue_number: u64, milestone: Option<u64>) -> Result<(), GithubError> {
		retrying!(self.inner.set_issue_milestone(repo, issue_number, milestone))
	}

	async fn repo_exists(&self, repo: RepoInfo) -> Result<bool, GithubError> {
		retrying!(self.inner.repo_exists(repo))
	}

	async fn list_milestones(&self, repo: RepoInfo) -> Result<Vec<GithubMilestone>, GithubError> {
		retrying!(self.inner.list_milestones(repo))
	}

	async fn get_milestone(&self, repo: RepoInfo, number: u64) -> Result<GithubMilestone, GithubError> {
		retrying!(self.inner.get_milestone(repo, number))
	}

	async fn list_milestone_issues(&self, repo: RepoInfo, milestone_number: u64) -> Result<Vec<GithubIssue>, GithubError> {
		retrying!(self.inner.list_milestone_issues(repo, milestone_number))
	}

	async fn create_milestone(&self, repo: RepoInfo, title: &str, description: &str, closed: bool) -> Result<(), GithubError> {
		retrying!(self.inner.create_milestone(repo, title, description, closed))
	}

	async fn update_milestone(&self, repo: RepoInfo, number: u64, description: &str, due_on: Option<jiff::Timestamp>) -> Result<(), GithubError> {
		retrying!(self.inner.update_milestone(repo, number, description, due_on))
	}

	async fn transfer_issue(&self, repo: RepoInfo, issue_number: u64, target: RepoInfo) -> Result<u64, GithubError> {
		retrying!(self.inner.transfer_issue(repo, issue_number, target))
	}

	async fn list_labels(&self, repo: RepoInfo) -> Result<BTreeMap<String, Color>, GithubError> {
		retrying!(self.inner.list_labels(repo))
	}

	async fn set_assignees(&self, repo: RepoInfo, issue_number: u64, assignees: &[String]) -> Result<(), GithubError> {
		retrying!(self.inner.set_assignees(repo, issue_number, assignees))
	}

	async fn list_assignable(&self, repo: RepoInfo) -> Result<Vec<String>, GithubError> {
		retrying!(self.inner.list_assignable(repo))
	}

	async fn request_review(&self, repo: RepoInfo, number: u64, reviewer: &str, on: bool) -> Result<(), GithubError> {
		retrying!(self.inner.request_review(repo, number, reviewer, on))
	}

	async fn list_org_repos(&self, org: &str) -> Result<Vec<RepoInfo>, GithubError> {
		retrying!(self.inner.list_org_repos(org))
	}

	async fn list_repo_issues(&self, repo: RepoInfo, since: Option<jiff::Timestamp>, after: Option<&str>) -> Result<IssuePage, GithubError> {
		retrying!(self.inner.list_repo_issues(repo, since, after))
	}

	async fn list_repo_prs(&self, repo: RepoInfo, after: Option<&str>) -> Result<PrPage, GithubError> {
		retrying!(self.inner.list_repo_prs(repo, after))
	}

	async fn file_history(&self, repo: RepoInfo, path: &str, branch: &str) -> Result<Vec<FileCommit>, GithubError> {
		retrying!(self.inner.file_history(repo, path, branch))
	}

	async fn propose_file(&self, repo: RepoInfo, proposal: &FileProposal) -> Result<String, GithubError> {
		retrying!(self.inner.propose_file(repo, proposal))
	}
}

//==============================================================================
// Global client storage
//==============================================================================

/// Process-wide GitHub client: set once at startup, shared by every thread that syncs.
pub mod client {
	use std::sync::RwLock;

	use super::BoxedGithubClient;

	static CLIENT: RwLock<Option<BoxedGithubClient>> = RwLock::new(None);

	/// Must be called before any sink operations.
	pub fn set(client: BoxedGithubClient) {
		*CLIENT.write().expect("never held across a panic") = Some(client);
	}

	pub fn get() -> Result<BoxedGithubClient, super::GithubError> {
		CLIENT.read().expect("never held across a panic").clone().ok_or_else(super::GithubError::new_not_initialized)
	}
}

//==============================================================================
// Utility functions (URL parsing, etc.) - These don't need the trait
//==============================================================================

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
