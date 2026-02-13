//! CLI definitions and entry point for blocker commands.
//!
//! This module handles:
//! - CLI argument parsing
//! - Dispatching to the integration module for issue-based blockers

use clap::{Args, Subcommand};
use color_eyre::eyre::Result;

use super::clockify::{HaltArgs, ResumeArgs};

pub async fn main(args: BlockerArgs, offline: bool) -> Result<()> {
	super::integration::main_integrated(args.command, offline).await
}
#[derive(Args, Clone, Debug)]
pub struct BlockerArgs {
	#[command(subcommand)]
	pub command: Command,
}

#[derive(Clone, Debug, Subcommand)]
pub enum Command {
	/// Append a blocker
	Add {
		name: String,
		/// Nest as a child of the current deepest item
		#[arg(short = 'n', long)]
		nested: bool,
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
	/// Set the current issue for blocker operations (clears revolver)
	Set {
		/// Pattern to match issue file
		pattern: String,
	},
	/// Add an issue to the blocker rotation without changing current selection
	SetMore {
		/// Pattern to match issue file
		pattern: String,
	},
	/// Step forward in the blocker rotation
	Toggle,
	/// Manage the blocker rotation
	#[command(subcommand)]
	Revolver(RevolverCommand),
	/// Resume tracking time on the current blocker task via Clockify
	Resume(ResumeArgs),
	/// Pause tracking time via Clockify
	Halt(HaltArgs),
	/// Show the current clockify project name (repo/title)
	CurrentProject,
}

#[derive(Clone, Debug, Subcommand)]
pub enum RevolverCommand {
	/// List entries in the blocker rotation
	List,
	/// Open the revolver cache file in $EDITOR
	Open,
}
