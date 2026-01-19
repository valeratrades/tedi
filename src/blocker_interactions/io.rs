//! CLI definitions and entry point for blocker commands.
//!
//! This module handles:
//! - CLI argument parsing
//! - Dispatching to the integration module for issue-based blockers

use clap::{Args, Subcommand};
use color_eyre::eyre::Result;
use tedi::DisplayFormat;

use super::clockify::{HaltArgs, ResumeArgs};

#[derive(Args, Clone, Debug)]
pub struct BlockerArgs {
	#[command(subcommand)]
	pub command: Command,
	/// Output format for list command
	#[arg(short, long, default_value = "nested")]
	pub format: DisplayFormat,
}

#[derive(Clone, Debug, Subcommand)]
pub enum Command {
	/// Append a blocker
	/// # NB
	/// adds one and only one blocker. The structure is **not** a tree for a reason:
	/// - it forces prioritization (high leverage)
	/// - solving top 1 thing can often unlock many smaller ones for free
	Add {
		name: String,
		/// Mark as urgent (adds to owner-level urgent.md)
		#[arg(short = 'u', long)]
		urgent: bool,
	},
	/// Pop the last one
	Pop,
	/// Full list of blockers down from the main task
	List,
	/// Compactly show the last entry
	Current {
		/// Show fully-qualified path with project prepended
		#[arg(short = 'f', long)]
		fully_qualified: bool,
	},
	/// Open issue file with $EDITOR
	Open {
		/// Optional pattern to open a different issue
		pattern: Option<String>,
		/// Set the opened issue as current after exiting the editor
		#[arg(short = 's', long)]
		set_after: bool,
		/// Open the urgent file instead
		#[arg(short = 'u', long)]
		urgent: bool,
	},
	/// Set the current issue for blocker operations
	Set {
		/// Pattern to match issue file
		pattern: String,
	},
	/// Resume tracking time on the current blocker task via Clockify
	Resume(ResumeArgs),
	/// Pause tracking time via Clockify
	Halt(HaltArgs),
}

pub async fn main(args: BlockerArgs, offline: bool) -> Result<()> {
	super::integration::main_integrated(args.command, args.format, offline).await
}
