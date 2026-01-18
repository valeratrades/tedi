//! Local filesystem storage for issues.
//!
//! This module handles all local storage concerns:
//! - File paths and naming conventions (files.rs)
//! - Project and issue metadata in .meta.json (meta.rs)
//!
//! The local storage structure:
//! ```text
//! XDG_DATA_HOME/todo/issues/{owner}/{repo}/
//!   .meta.json                           # Project + per-issue metadata
//!   {number}_-_{title}.md                # Flat issue file
//!   {number}_-_{title}/                  # Directory format (when has children)
//!     __main__.md                        # The issue content
//!     {child_number}_-_{child_title}.md  # Child issues
//! ```

mod files;
mod meta;

// Re-export file operations
pub use files::{
	ExactMatchLevel,
	MAIN_ISSUE_FILENAME,
	build_ancestry_path,
	choose_issue_with_fzf,
	// Path parsing
	extract_owner_repo_from_path,
	// File operations
	find_issue_file,
	get_issue_dir_path,
	get_issue_file_path,
	get_main_file_path,
	// Path construction
	issues_dir,
	sanitize_title_for_filename,
	search_issue_files,
};
// Re-export metadata operations
pub use meta::{
	allocate_virtual_issue_number,
	ensure_virtual_project,
	// Virtual project support
	is_virtual_project,
	// Path identity parsing (extracts info from file path)
	parse_path_identity,
};
