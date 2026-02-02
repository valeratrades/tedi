//! Git and issue setup extensions for TestContext.
//!
//! Provides simple methods for setting up test scenarios:
//!
//! ```ignore
//! let ctx = TestContext::build("");
//!
//! // Set up local file (uncommitted) with timestamps from seed 50
//! ctx.local(&issue, Some(50));
//!
//! // Set up consensus state (committed to git) with timestamps from seed -30
//! ctx.consensus(&issue, Some(-30));
//!
//! // Set up mock remote (Github API responses) with timestamps from seed -30
//! ctx.remote(&issue, Some(-30));
//!
//! // All methods are additive - can call multiple times:
//! ctx.remote(&issue1, Some(1));
//! ctx.remote(&issue2, Some(2)); // Adds to mock, doesn't replace
//!
//! // Typical sync test: local newer than consensus/remote
//! ctx.consensus(&base, Some(-40));       // older
//! ctx.local(&modified, Some(60));        // newer (wins in merge)
//! ctx.remote(&remote_version, Some(-40)); // same as consensus
//! ```
//!
//! ## Timestamp Seeds
//!
//! Seeds must be in range -100..=100. Each seed produces:
//! 1. Pseudo-random scatter per field (±12h, deterministic per seed+field_index)
//! 2. Deterministic offset: -100 → -12h, 0 → base, +100 → +12h
//!
//! Higher seed = newer timestamps = wins in merge conflicts.
//! Use `None` for no timestamps (legacy behavior).
//!
//! Owner/repo/number are extracted from the Issue's identity (IssueLink).
//! If no link exists, defaults are used: owner="owner", repo="repo", number=1.

use std::{cell::RefCell, collections::HashSet};

use tedi::{
	Issue, IssueTimestamps,
	local::{Consensus, IssueMeta, Local, Submitted},
	sink::Sink,
};
use v_fixtures::fs_standards::git::Git;

use super::TestContext;

/// Seed for deterministic timestamp generation. Must be in range -100..=100.
#[derive(Clone, Copy, Debug, derive_more::Deref, derive_more::DerefMut, derive_more::Display, Eq, derive_more::Into, PartialEq)]
pub struct Seed(i64);

impl Seed {
	pub fn new(value: i64) -> Self {
		assert!((-100..=100).contains(&value), "seed must be in range -100..=100, got {value}");
		Self(value)
	}
}

impl From<i8> for Seed {
	fn from(value: i8) -> Self {
		Self(value as i64)
	}
}

/// Generate timestamps from a seed value.
///
/// Seed must be in range -100..=100. Each timestamp field gets:
/// 1. A pseudo-random offset in ±12h range (deterministic per seed+index)
/// 2. A deterministic offset based on seed: -100 → -12h, 0 → 0, +100 → +12h
///
/// This means same seed produces same timestamps, but different fields have different
/// random-looking values. Higher seed = newer timestamps = wins in merge conflicts.
///
/// Index mapping for fields:
/// - title: -2
/// - description: -1
/// - labels: 0
/// - comments: 1 (aggregate timestamp for "most recent comment")
pub fn timestamps_from_seed(seed: Seed) -> IssueTimestamps {
	IssueTimestamps {
		title: Some(timestamp_for_field(seed, -2)),
		description: Some(timestamp_for_field(seed, -1)),
		labels: Some(timestamp_for_field(seed, 0)),
		state: Some(timestamp_for_field(seed, 1)),
		comments: vec![],
	}
}
/// Set timestamps on an issue and all its children.
pub fn set_timestamps(issue: &mut Issue, seed: Seed) {
	let timestamps = timestamps_from_seed(seed);
	for node in issue.iter_mut() {
		node.identity.mut_linked_issue_meta().unwrap().timestamps = timestamps.clone();
	}
}
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
/// Extension trait for git and issue setup operations.
#[allow(async_fn_in_trait)]
pub trait GitExt {
	/// Initialize git in the issues directory.
	fn init_git(&self) -> Git;

	/// Write issue to local file (uncommitted). Additive - can call multiple times.
	/// Panics if same (owner, repo, number) is submitted twice.
	///
	/// If `seed` is provided, timestamps are generated from it and written to `.meta.json`.
	async fn local(&self, issue: &Issue, seed: Option<Seed>);

	/// Write issue and commit to git as consensus state. Additive - can call multiple times.
	/// Panics if same (owner, repo, number) is submitted twice.
	///
	/// If `seed` is provided, timestamps are generated from it and written to `.meta.json`.
	async fn consensus(&self, issue: &Issue, seed: Option<Seed>);

	/// Set up mock Github API to return this issue. Additive - can call multiple times.
	/// Handles sub-issues automatically.
	/// Panics if same (owner, repo, number) is submitted twice.
	///
	/// If `seed` is provided, timestamps are generated from it for the mock response.
	fn remote(&self, issue: &Issue, seed: Option<Seed>);
}
/// Base timestamp: 2001-09-11 12:00:00 UTC (midday).
const BASE_TIMESTAMP_SECS: i64 = 1000209600;

/// 12 hours in seconds - the range for randomization.
const HALF_DAY_SECS: i64 = 12 * 60 * 60;

/// Generate a timestamp for a specific field index.
///
/// Combines pseudo-random scatter (from seed+index) with deterministic offset (from seed).
fn timestamp_for_field(seed: Seed, field_index: i64) -> jiff::Timestamp {
	// Pseudo-random scatter: hash seed+index to get a value in ±12h range
	let random_offset = pseudo_random_offset(seed, field_index);

	// Deterministic offset: seed maps linearly to ±12h
	// -100 → -12h, 0 → 0, +100 → +12h
	let deterministic_offset = (*seed * HALF_DAY_SECS) / 100;

	let total_offset = random_offset + deterministic_offset;
	jiff::Timestamp::from_second(BASE_TIMESTAMP_SECS + total_offset).expect("valid timestamp")
}

/// Simple pseudo-random number generator seeded by seed+index.
/// Returns a value in range [-HALF_DAY_SECS, +HALF_DAY_SECS].
fn pseudo_random_offset(seed: Seed, index: i64) -> i64 {
	// Combine seed and index into a single value, then hash it
	let combined = (*seed as u64).wrapping_mul(31).wrapping_add(index as u64);
	// Simple xorshift-style mixing
	let mut x = combined;
	x ^= x << 13;
	x ^= x >> 7;
	x ^= x << 17;
	// Map to [-HALF_DAY_SECS, +HALF_DAY_SECS]
	let normalized = (x % (2 * HALF_DAY_SECS as u64 + 1)) as i64;
	normalized - HALF_DAY_SECS
}

/// Default owner for test issues without a link
const DEFAULT_OWNER: &str = "owner";
/// Default repo for test issues without a link
const DEFAULT_REPO: &str = "repo";
/// Default issue number for test issues without a link
const DEFAULT_NUMBER: u64 = 1;

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

impl GitExt for TestContext {
	fn init_git(&self) -> Git {
		let git = Git::init(self.xdg.data_dir().join("issues"));
		// Use diff3 conflict style for consistent snapshots across environments
		git.run(&["config", "merge.conflictStyle", "diff3"]).expect("git config merge.conflictStyle failed");
		git
	}

	async fn local(&self, issue: &Issue, seed: Option<Seed>) {
		let (owner, repo, number) = extract_issue_coords(issue);
		let key = (owner.clone(), repo.clone(), number);

		with_state(self, |state| {
			if state.local_issues.contains(&key) {
				panic!("local() called twice for same issue: {owner}/{repo}#{number}");
			}
			state.local_issues.insert(key);
		});

		// Set issues dir override and sink using actual library implementation
		self.set_issues_dir_override();
		let mut issue_clone = issue.clone();
		<Issue as Sink<Submitted>>::sink(&mut issue_clone, None).await.expect("local sink failed");

		// Write timestamps to .meta.json if seed provided
		if let Some(seed) = seed {
			let timestamps = timestamps_from_seed(seed);
			let meta = IssueMeta { timestamps };
			Local::save_issue_meta(tedi::RepoInfo::new(&owner, &repo), number, &meta).expect("save_issue_meta failed");
		}
	}

	async fn consensus(&self, issue: &Issue, seed: Option<Seed>) {
		let (owner, repo, number) = extract_issue_coords(issue);
		let key = (owner.clone(), repo.clone(), number);

		with_state(self, |state| {
			if state.consensus_issues.contains(&key) {
				panic!("consensus() called twice for same issue: {owner}/{repo}#{number}");
			}
			state.consensus_issues.insert(key);
		});

		// Ensure git is initialized (Sink<Consensus> needs it)
		self.init_git();

		// Set issues dir override and sink using actual library implementation
		self.set_issues_dir_override();
		let mut issue_clone = issue.clone();
		<Issue as Sink<Consensus>>::sink(&mut issue_clone, None).await.expect("consensus sink failed");

		// Write timestamps to .meta.json if seed provided
		if let Some(seed) = seed {
			let timestamps = timestamps_from_seed(seed);
			let meta = IssueMeta { timestamps };
			Local::save_issue_meta(tedi::RepoInfo::new(&owner, &repo), number, &meta).expect("save_issue_meta failed");
		}
	}

	fn remote(&self, issue: &Issue, seed: Option<Seed>) {
		let (owner, repo, number) = extract_issue_coords(issue);
		let key = (owner.clone(), repo.clone(), number);

		let timestamps = seed.map(timestamps_from_seed);

		with_state(self, |state| {
			if state.remote_issue_ids.contains(&key) {
				panic!("remote() called twice for same issue: {owner}/{repo}#{number}");
			}

			// Recursively add issue and all its children
			add_issue_recursive(state, tedi::RepoInfo::new(&owner, &repo), number, None, issue, timestamps.as_ref());
		});

		// Rebuild and write mock state
		self.rebuild_mock_state();
	}
}

impl TestContext {
	/// Set the issues directory override for `Local::issues_dir()`.
	///
	/// Uses the thread_local mock mechanism to isolate test filesystem state.
	/// This is preferred over `set_xdg_env` as it doesn't modify global process env vars.
	pub(crate) fn set_issues_dir_override(&self) {
		tedi::mocks::set_issues_dir(self.xdg.data_dir().join("issues"));
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
					// Add timestamps if provided
					if let Some(ts) = &i.timestamps {
						if let Some(t) = ts.title {
							json["title_timestamp"] = serde_json::Value::String(t.to_string());
						}
						if let Some(t) = ts.description {
							json["description_timestamp"] = serde_json::Value::String(t.to_string());
						}
						if let Some(t) = ts.labels {
							json["labels_timestamp"] = serde_json::Value::String(t.to_string());
						}
						if let Some(t) = ts.state {
							json["state_timestamp"] = serde_json::Value::String(t.to_string());
						}
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
					// Use provided timestamp or default to base timestamp
					let ts = c.timestamp.unwrap_or_else(|| jiff::Timestamp::from_second(BASE_TIMESTAMP_SECS).unwrap());
					serde_json::json!({
						"owner": c.owner,
						"repo": c.repo,
						"issue_number": c.issue_number,
						"comment_id": c.comment_id,
						"body": c.body,
						"owner_login": c.owner_login,
						"created_at": ts.to_string(),
						"updated_at": ts.to_string()
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
/// Uses `issue.identity.link()` from the library, with test-specific fallback defaults
/// for unlinked issues (owner="owner", repo="repo", number=1).
fn extract_issue_coords(issue: &Issue) -> (String, String, u64) {
	if let Some(link) = issue.identity.link() {
		(link.owner().to_string(), link.repo().to_string(), link.number())
	} else {
		(DEFAULT_OWNER.to_string(), DEFAULT_REPO.to_string(), DEFAULT_NUMBER)
	}
}

/// Recursively add an issue and all its children to the mock state.
/// Transforms library `Issue` types into mock JSON state for the test mock server.
fn add_issue_recursive(state: &mut GitState, repo_info: tedi::RepoInfo, number: u64, parent_number: Option<u64>, issue: &Issue, timestamps: Option<&IssueTimestamps>) {
	let owner = repo_info.owner();
	let repo = repo_info.repo();
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
		timestamps: timestamps.cloned(),
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
	// Use the per-comment timestamps from IssueTimestamps if available
	let comment_timestamps = timestamps.map(|ts| &ts.comments);
	for (i, comment) in issue.contents.comments.iter().skip(1).enumerate() {
		if let Some(id) = comment.identity.id() {
			let comment_owner_login = comment.identity.user().expect("comment identity must have user - use @user format in test fixtures").to_string();
			let comment_ts = comment_timestamps.and_then(|ts| ts.get(i).copied());
			state.remote_comments.push(MockComment {
				owner: owner.to_string(),
				repo: repo.to_string(),
				issue_number: number,
				comment_id: id,
				body: comment.body.render(),
				owner_login: comment_owner_login,
				timestamp: comment_ts,
			});
		}
	}

	// Recursively add children (they inherit the same timestamps)
	for child in &issue.children {
		let child_number = child.git_id().expect("child issue must have number for remote mock state");
		add_issue_recursive(state, repo_info, child_number, Some(number), child, timestamps);
	}
}

/// Intermediate type for building mock JSON state. Stores owner/repo per-entry
/// because the mock JSON format requires them (unlike library types where they're implicit).
struct MockIssue {
	owner: String,
	repo: String,
	number: u64,
	title: String,
	body: String,
	state: String,
	state_reason: Option<String>,
	owner_login: String,
	/// Timestamps for this issue (if provided via seed)
	timestamps: Option<IssueTimestamps>,
}

/// Intermediate type for building mock JSON state. Stores owner/repo/issue_number per-entry
/// because the mock JSON format requires them (unlike library types where they're implicit).
struct MockComment {
	owner: String,
	repo: String,
	issue_number: u64,
	comment_id: u64,
	body: String,
	owner_login: String,
	/// Timestamp for this comment (if provided via seed)
	timestamp: Option<jiff::Timestamp>,
}

struct SubIssueRelation {
	owner: String,
	repo: String,
	parent: u64,
	child: u64,
}
