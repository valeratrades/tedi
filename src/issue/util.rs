//! Small utility functions for issue processing.
//XXX: fundamentally flawed concept, is up for deprecation.
// Markers should be taking care of all parts of parsing themselves

use super::Marker;

/// Check if a line is a blockers section marker.
/// Recognized formats (case-insensitive):
/// - `# Blockers` (preferred, what `!b` expands to)
/// - `<!--blockers-->` (legacy, still supported)
/// - `#{1,6} Blockers` (any header level)
/// - `**Blockers**` (with optional trailing `:`)
pub fn is_blockers_marker(line: &str) -> bool {
	// Use Marker enum for standard formats
	if matches!(Marker::decode(line), Some(Marker::BlockersSection(_))) {
		return true;
	}
	// Also support **Blockers** format (not in Marker enum as it's non-standard)
	let lower = line.trim().to_ascii_lowercase();
	lower.starts_with("**blockers**")
}

/// Normalize issue content by converting space-based indentation to tab-based.
/// This ensures content edited in editors that convert tabs to spaces can be properly parsed.
///
/// The function detects the first non-marker indented line to determine the space-per-indent ratio,
/// then normalizes all indentation to tabs.
pub fn normalize_issue_indentation(content: &str) -> String {
	// Detect spaces-per-indent by finding the first indented line that starts with spaces
	let spaces_per_indent = content
		.lines()
		.find_map(|line| {
			if line.starts_with(' ') && !line.trim().is_empty() {
				// Count leading spaces
				let spaces = line.len() - line.trim_start_matches(' ').len();
				// Common indent sizes: 2, 3, 4, 8
				if spaces >= 2 {
					Some(spaces.min(8)) // Cap at 8 to avoid issues with very deep indents
				} else {
					None
				}
			} else {
				None
			}
		})
		.unwrap_or(4); // Default to 4 spaces per indent if we can't detect

	content
		.lines()
		.map(|line| {
			if line.is_empty() {
				return String::new();
			}

			// Count leading whitespace and convert to appropriate number of tabs
			let mut chars = line.chars().peekable();
			let mut space_count = 0;
			let mut tab_count = 0;

			while let Some(&ch) = chars.peek() {
				match ch {
					'\t' => {
						tab_count += 1;
						chars.next();
					}
					' ' => {
						space_count += 1;
						chars.next();
					}
					_ => break,
				}
			}

			// Convert spaces to tabs
			let extra_tabs = space_count / spaces_per_indent;
			let total_tabs = tab_count + extra_tabs;
			let remaining_spaces = space_count % spaces_per_indent;

			// Reconstruct the line with normalized indentation
			let rest: String = chars.collect();
			let mut result = "\t".repeat(total_tabs);
			// Keep any remaining spaces (less than one indent level)
			result.push_str(&" ".repeat(remaining_spaces));
			result.push_str(&rest);
			result
		})
		.collect::<Vec<_>>()
		.join("\n")
}
