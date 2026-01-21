//! Main command entry point for the open subcommand.

use std::path::Path;

use clap::Args;
use tedi::{
	Issue, IssueLink, LazyIssue,
	local::{ExactMatchLevel, Local, Submitted},
	sink::Sink,
};
use v_utils::prelude::*;

use super::{
	remote::{Remote, RemoteSource},
	sync::{MergeMode, Side, SyncOptions},
	touch::{TouchPathResult, create_pending_issue, create_virtual_issue, parse_touch_path},
};
use crate::{
	config::LiveSettings,
	github,
	open_interactions::{Modifier, modify_and_sync_issue},
};

/// Open a Github issue in $EDITOR.
///
/// Issue files support a blockers section for tracking sub-tasks. Add a `# Blockers` marker
/// in the issue body. Content after this marker until the next sub-issue
/// or comment is treated as blockers, using the same format as standalone blocker files.
///
/// Shorthand: Use `!b` on its own line to auto-expand to `# Blockers`.
#[derive(Args, Debug)]
pub struct OpenArgs {
	/// Github issue URL (e.g., https://github.com/owner/repo/issues/123) OR a search pattern for local issue files
	/// With --touch: path format is workspace/project/{issue.md, issue/sub-issue.md}
	/// If omitted, opens fzf on all local issue files.
	pub url_or_pattern: Option<String>,

	/// Use exact matching in fzf. Can be specified multiple times:
	/// -e: exact terms (space-separated; exact matches, but no regex)
	/// -ee: regex pattern (substring match)
	/// -eee: regex pattern (full line match, auto-anchored)
	#[arg(short = 'e', long, action = clap::ArgAction::Count)]
	pub exact: u8,

	/// Create or open an issue from a path. Path format: workspace/project/issue[.md]
	/// For sub-issues: workspace/project/parent/child (parent must exist on Github)
	/// If issue already exists locally, opens it. Otherwise creates on Github first.
	#[arg(short, long)]
	pub touch: bool,

	/// Open the most recently modified issue file
	#[arg(short, long)]
	pub last: bool,

	/// Fetch latest from Github before opening. If remote differs from local,
	/// prompts: [s]kip (use local), [o]verwrite (use remote), [m]erge (attempt merge)
	#[arg(short, long)]
	pub pull: bool,

	/// Use the current blocker issue file (from `todo blocker set`)
	/// If no pattern provided, opens the current blocker issue.
	#[arg(short, long)]
	pub blocker: bool,

	/// Like --blocker, but also sets the opened issue as active if different from current.
	/// Opens the current blocker issue (or pattern match), and if that issue belongs to
	/// a different project than the currently active one, sets it as the active project.
	#[arg(long)]
	pub blocker_set: bool,

	/// Force through conflicts by taking the source side.
	/// When opening via local path: takes local version.
	/// When opening via Github URL: takes remote version.
	#[arg(short, long)]
	pub force: bool,

	/// Reset to source state, ignoring any local/remote changes.
	/// Overwrites everything with current source without syncing.
	/// When opening via local path: keeps local as-is (skips sync).
	/// When opening via Github URL: overwrites local with remote.
	#[arg(short, long)]
	pub reset: bool,
}

#[tracing::instrument(level = "debug", skip(settings))]
pub async fn open_command(settings: &LiveSettings, args: OpenArgs, offline: bool) -> Result<()> {
	tracing::debug!("open_command entered, blocker={}", args.blocker);
	let _ = settings; // settings still available if needed in future

	// Validate and convert exact match level
	let exact = ExactMatchLevel::try_from(args.exact).map_err(|e| eyre!(e))?;

	// Build merge mode from args with the given side preference
	let build_merge_mode = |prefer: Side| -> Option<MergeMode> {
		if args.reset {
			Some(MergeMode::Reset { prefer })
		} else if args.force {
			Some(MergeMode::Force { prefer })
		} else {
			None
		}
	};

	// Helper to create sync opts based on side preference
	// --pull flag OR URL mode: prefer Remote side for --force/--reset
	// Local file without --pull: prefer Local side for --force/--reset
	let make_sync_opts = |prefer_remote: bool| {
		let prefer = if prefer_remote { Side::Remote } else { Side::Local };
		SyncOptions::new(build_merge_mode(prefer), prefer_remote || args.pull)
	};

	// Local file paths: prefer Local unless --pull is specified
	let local_sync_opts = || make_sync_opts(args.pull);

	// URL mode and explicit --pull: prefer Remote
	let remote_sync_opts = || make_sync_opts(true);

	// Handle --blocker and --blocker-set modes: use current blocker issue file if no pattern provided
	let open_at_blocker = args.blocker || args.blocker_set;
	let input = if open_at_blocker && args.url_or_pattern.is_none() {
		// Get current blocker issue path if it exists
		// --blocker requires it to exist; --blocker-set just opens fzf if not set
		if let Some(blocker_path) = crate::blocker_interactions::integration::get_current_blocker_issue() {
			blocker_path.to_string_lossy().to_string()
		} else if args.blocker {
			bail!("No blocker issue set. Use `todo blocker set <pattern>` first.")
		} else {
			// --blocker-set without current blocker: use empty pattern for fzf
			String::new()
		}
	} else {
		args.url_or_pattern.as_deref().unwrap_or("").trim().to_string()
	};
	let input = input.as_str();

	// Resolve the issue file path and sync options based on mode
	let (issue_file_path, sync_opts, effective_offline) = if args.last {
		// Handle --last mode: open the most recently modified issue file
		let all_files = Local::search_issue_files("")?;
		if all_files.is_empty() {
			bail!("No issue files found. Use a Github URL to fetch an issue first.");
		}
		// Files are already sorted by modification time (most recent first)
		(all_files[0].clone(), local_sync_opts(), offline)
	} else if args.touch {
		// Handle --touch mode
		let touch_result = parse_touch_path(input)?;

		match touch_result {
			TouchPathResult::Found(ancestry) => {
				// Found existing issue - look up the file path
				let path = Local::find_issue_file_by_ancestry(&ancestry).ok_or_else(|| eyre!("Issue matched but file not found for ancestry: {ancestry:?}"))?;
				let project_is_virtual = Local::is_virtual_project(ancestry.owner(), ancestry.repo());
				println!("Found existing issue: {path:?}");
				(path, local_sync_opts(), offline || project_is_virtual)
			}
			TouchPathResult::Create { title, ancestry } => {
				// Need to create a new issue
				let project_is_virtual = Local::is_virtual_project(ancestry.owner(), ancestry.repo());

				if project_is_virtual {
					// Virtual project: stays local forever
					println!("Project {}/{} is virtual (no Github remote)", ancestry.owner(), ancestry.repo());
					(create_virtual_issue(&title, &ancestry)?, local_sync_opts(), true)
				} else {
					// Real project: create issue in memory, open editor, only save if user makes changes
					let (issue, path) = create_pending_issue(&title, &ancestry)?;
					open_new_issue(issue, &path, local_sync_opts()).await?;
					return Ok(());
				}
			}
		}
	} else if github::is_github_issue_url(input) {
		// Github URL mode: unified with --pull behavior
		// URL opening implies pull=true and prefers Remote for --force/--reset
		if offline {
			bail!("Cannot fetch issue from URL in offline mode");
		}

		let (owner, repo, issue_number) = github::parse_github_issue_url(input)?;

		// Check if we already have this issue locally
		let existing_path = Local::find_issue_file(&owner, &repo, Some(issue_number), "", &[]);

		let issue_file_path = if let Some(path) = existing_path {
			// File exists locally - proceed with unified sync (like --pull)
			println!("Found existing local file, will sync with remote...");
			path
		} else {
			// File doesn't exist - fetch and create it
			println!("Fetching issue #{issue_number} from {owner}/{repo}...");

			// Load from GitHub (lineage=None means it will be fetched if needed)
			let url = format!("https://github.com/{owner}/{repo}/issues/{issue_number}");
			let link = IssueLink::parse(&url).expect("valid URL");
			let source = RemoteSource::new(link);
			let mut issue = <Issue as LazyIssue<Remote>>::load(source).await?;

			// Write to local filesystem
			<Issue as Sink<Submitted>>::sink(&mut issue, None).await?;

			// Find the path where it was written
			let path = Local::find_issue_file(&owner, &repo, Some(issue_number), &issue.contents.title, &[]).expect("just wrote the issue");
			println!("Stored issue at: {path:?}");

			// Commit the fetched state as the consensus baseline
			use super::consensus::commit_issue_changes;
			commit_issue_changes(&path, &owner, &repo, issue_number, Some("initial fetch"))?;

			path
		};

		// URL mode uses remote_sync_opts: pull=true, --force/--reset prefer Remote
		(issue_file_path, remote_sync_opts(), offline)
	} else {
		// Check if input is an existing file path (absolute or relative)
		let input_path = Path::new(input);
		if input_path.exists() && input_path.is_file() {
			// Direct file path - open it
			(input_path.to_path_buf(), local_sync_opts(), offline)
		} else {
			// Local search mode: always pass all files to fzf, let it handle filtering
			let all_files = Local::search_issue_files("")?;
			if all_files.is_empty() {
				bail!("No issue files found. Use a Github URL to fetch an issue first.");
			}
			let issue_file_path = match Local::choose_issue_with_fzf(&all_files, input, exact)? {
				Some(path) => path,
				None => bail!("No issue selected"),
			};

			(issue_file_path, local_sync_opts(), offline)
		}
	};

	// Open the local issue file for editing
	// If using blocker mode, open at the last blocker position
	modify_and_sync_issue(&issue_file_path, effective_offline, Modifier::Editor { open_at_blocker }, sync_opts).await?;

	// If --blocker-set was used, set this issue as the current blocker issue
	if args.blocker_set {
		crate::blocker_interactions::integration::set_current_blocker_issue(&issue_file_path)?;
		println!("Set current blocker issue to: {}", issue_file_path.display());
	}

	Ok(())
}
