//! Parse markdown and output events
//!
//! Run with: cargo run --example md_parse [path]

use std::{env, fs, path::Path};

use pulldown_cmark::{Options, Parser};

fn main() {
	let path = env::args().nth(1).unwrap_or_else(|| "examples/test_issue.md".into());

	let content = fs::read_to_string(&path).expect("Failed to read file");

	println!("=== Parsing: {path} ===\n");

	let options = Options::ENABLE_TASKLISTS | Options::ENABLE_STRIKETHROUGH;
	let parser = Parser::new_ext(&content, options);
	let events: Vec<_> = parser.collect();

	println!("=== Events ({}) ===\n", events.len());

	let mut events_str = String::new();
	for (i, event) in events.iter().enumerate() {
		let line = format!("[{i:3}] {event:?}\n");
		print!("{line}");
		events_str.push_str(&line);
	}

	// Write events to .events.rs.bak file next to input
	let input_path = Path::new(&path);
	let events_path = input_path.with_extension("events.rs.bak");
	fs::write(&events_path, &events_str).expect("Failed to write events file");
	eprintln!("\nWrote events to {}", events_path.display());
}
