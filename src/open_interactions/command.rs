//! Main command entry point for the open subcommand.

use std::path::Path;

use clap::Args;
use tedi::{
	Issue, IssueIndex, IssueLink, IssueSelector, LazyIssue, RepoInfo, github,
	local::{Consensus, ExactMatchLevel, FsReader, Local, LocalFs, LocalIssueSource, LocalPath},
	remote::RemoteSource,
	sink::Sink,
};
use v_utils::prelude::*;

use super::{
	sync::{MergeMode, Modifier, Side, SyncOptions, modify_and_sync_issue},
	touch::parse_touch_path,
};
use crate::{MockType, config::LiveSettings};

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
	#[arg(long)] // no short version, as it introduces ambiguity against `--parent`
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

	/// Create missing parent components (like mkdir -p). Used with --touch.
	/// Without value or with "remote": creates missing repo on GitHub.
	/// With "virtual": creates missing repo as virtual (local-only).
	#[arg(long, value_name = "TYPE", default_missing_value = "default", num_args = 0..=1)]
	pub parent: Option<ProjectType>, // no short version, as it introduces ambiguity against `--pull`
}

/// Type of parent to create when using --parent flag
#[derive(Clone, Copy, Debug, Default, Eq, PartialEq, clap::ValueEnum)]
pub enum ProjectType {
	/// Require repo to exist on GitHub (default)
	#[default]
	#[value(name = "default")]
	Default,
	/// Create missing repo as virtual (local-only)
	Virtual,
}

#[tracing::instrument(level = "debug", skip(settings))]
pub async fn open_command(settings: &LiveSettings, args: OpenArgs, offline: bool, mock: Option<MockType>) -> Result<()> {
	tracing::debug!("open_command entered, blocker={}", args.blocker);

	// Helper to create the appropriate modifier based on mock type
	let make_modifier = |open_at_blocker: bool| -> Modifier {
		match mock {
			Some(MockType::GhostEdit) => Modifier::MockGhostEdit,
			_ => Modifier::Editor { open_at_blocker },
		}
	};
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
		if let Some(source) = crate::blocker_interactions::integration::IssueSource::current() {
			source.display_relative()
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

	// Handle --touch mode first and separately
	if args.touch {
		let source = parse_touch_path(input, args.parent, offline).await?;
		let is_create = !source.local_path.clone().resolve_parent(FsReader)?.search().is_ok();

		let issue = if is_create {
			let index = source.index().clone();
			let project_is_virtual = Local::is_virtual_project(index.repo_info());
			Issue::pending_from_descriptor(&index, project_is_virtual)
		} else {
			Issue::load(source).await?
		};
		let project_is_virtual = issue.identity.is_virtual;

		if is_create {
			if !issue.identity.parent_index.index().is_empty() {
				println!("Creating pending sub-issue: {}", issue.contents.title);
			} else {
				println!("Creating pending issue: {}", issue.contents.title);
			}
			if !project_is_virtual {
				println!("Issue will be created on Github when you save and sync.");
			}
		} else {
			println!("Found existing issue: {}", issue.contents.title);
		}

		modify_and_sync_issue(issue, offline || project_is_virtual, make_modifier(open_at_blocker), local_sync_opts()).await?;
		return Ok(());
	}

	// Resolve issue and sync options based on mode
	let (issue, sync_opts, effective_offline) = if args.last {
		// Handle --last mode: open the most recently modified issue file
		let path = Local::most_recent_issue_file()?.ok_or_else(|| eyre!("No issue files found. Use a Github URL to fetch an issue first."))?;
		let source = LocalIssueSource::<FsReader>::build_from_path(&path).await?;
		let issue = Issue::load(source).await?;
		(issue, local_sync_opts(), offline)
	} else if github::is_github_issue_url(input) {
		// Github URL mode: unified with --pull behavior
		// URL opening implies pull=true and prefers Remote for --force/--reset
		if offline {
			bail!("Cannot fetch issue from URL in offline mode");
		}

		let (owner, repo, issue_number) = github::parse_github_issue_url(input)?;

		// Check if we already have this issue locally
		let index = IssueIndex::root(RepoInfo::new(&owner, &repo), IssueSelector::GitId(issue_number));
		let existing_path = LocalPath::from(index)
			.resolve_parent(FsReader)
			.ok()
			.and_then(|resolved| resolved.search().ok())
			.map(|resolved| resolved.path());

		let issue = if let Some(path) = existing_path {
			// File exists locally - proceed with unified sync (like --pull)
			println!("Found existing local file, will sync with remote...");
			let source = LocalIssueSource::<FsReader>::build_from_path(&path).await?;
			Issue::load(source).await?
		} else {
			// File doesn't exist - fetch and create it
			println!("Fetching issue #{issue_number} from {owner}/{repo}...");

			// Load from GitHub (lineage=None means it will be fetched if needed)
			let url = format!("https://github.com/{owner}/{repo}/issues/{issue_number}");
			let link = IssueLink::parse(&url).expect("valid URL");
			let source = RemoteSource::build(link)?;
			let mut issue = Issue::load(source).await?;

			// Write to local filesystem
			<Issue as Sink<LocalFs>>::sink(&mut issue, None).await?;

			println!("Stored issue");

			// Commit the fetched state as the consensus baseline
			<Issue as Sink<Consensus>>::sink(&mut issue, None).await?;

			issue
		};

		// URL mode uses remote_sync_opts: pull=true, --force/--reset prefer Remote
		(issue, remote_sync_opts(), offline)
	} else {
		// Check if input is an existing file path (absolute or relative)
		let input_path = Path::new(input);
		let issue_file_path = if input_path.exists() && input_path.is_file() {
			// Direct file path - open it
			input_path.to_path_buf()
		} else {
			// Local search mode: use fzf for interactive selection
			Local::fzf_issue(input, exact)?
		};

		let source = LocalIssueSource::<FsReader>::build_from_path(&issue_file_path).await?;
		let issue = Issue::load(source).await?;
		(issue, local_sync_opts(), offline)
	};

	// Open the issue for editing
	modify_and_sync_issue(issue, effective_offline, make_modifier(open_at_blocker), sync_opts).await?;

	// TODO: --blocker-set needs issue file path, but we no longer track it here
	// if args.blocker_set {
	// 	crate::blocker_interactions::integration::set_current_blocker_issue(&issue_file_path)?;
	// 	println!("Set current blocker issue to: {}", issue_file_path.display());
	// }

	Ok(())
}
