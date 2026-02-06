//! Error types for parsing issue files.
//!
//! Uses miette for rich diagnostics with source code spans.

#![allow(unused_assignments)] // Fields are read by miette's derive macro via attributes

use std::path::PathBuf;

use miette::{NamedSource, SourceSpan};
use tracing_error::SpanTrace;

/// Error type for issue file parsing.
///
/// Contains pre-rendered miette diagnostic and SpanTrace captured at error creation time.
#[derive(Debug, thiserror::Error)]
#[error("{rendered}\n\n{spantrace}")]
pub struct ParseError {
	rendered: String,
	spantrace: SpanTrace,
}

impl ParseError {
	fn from_diagnostic(diag: ParseDiagnostic) -> Self {
		let rendered = format!("{:?}", miette::Report::new(diag));
		Self {
			rendered,
			spantrace: SpanTrace::capture(),
		}
	}

	pub fn empty_file() -> Self {
		Self::from_diagnostic(ParseDiagnostic::EmptyFile)
	}

	pub fn invalid_title(src: NamedSource<String>, span: SourceSpan, detail: String) -> Self {
		Self::from_diagnostic(ParseDiagnostic::InvalidTitle { src, span, detail })
	}

	pub fn missing_url_marker(src: NamedSource<String>, span: SourceSpan) -> Self {
		Self::from_diagnostic(ParseDiagnostic::MissingUrlMarker { src, span })
	}

	pub fn malformed_url_marker(src: NamedSource<String>, span: SourceSpan) -> Self {
		Self::from_diagnostic(ParseDiagnostic::MalformedUrlMarker { src, span })
	}

	pub fn bad_indentation(src: NamedSource<String>, span: SourceSpan, expected_tabs: usize) -> Self {
		Self::from_diagnostic(ParseDiagnostic::BadIndentation { src, span, expected_tabs })
	}

	pub fn invalid_checkbox(src: NamedSource<String>, span: SourceSpan, content: String) -> Self {
		Self::from_diagnostic(ParseDiagnostic::InvalidCheckbox { src, span, content })
	}

	pub fn invalid_duplicate_reference(src: NamedSource<String>, span: SourceSpan, issue_number: u64) -> Self {
		Self::from_diagnostic(ParseDiagnostic::InvalidDuplicateReference { src, span, issue_number })
	}
}

/// Error when converting IssueIndex to a git number path.
/// Occurs when a Title selector is encountered but only GitId selectors are valid.
#[derive(Debug, miette::Diagnostic, thiserror::Error)]
#[error("cannot convert IssueIndex to git number path: contains Title selector")]
#[diagnostic(
	code(tedi::index::title_in_git_path),
	help("git_num_path requires all selectors to be GitId (issue numbers). Title selectors indicate pending issues that haven't been synced to GitHub yet.")
)]
pub struct TitleInGitPathError {
	#[source_code]
	pub index_display: NamedSource<String>,
	#[label("expected GitId, found Title selector")]
	pub span: SourceSpan,
}
/// Holds source content and filename for error reporting.
#[derive(Clone, Debug)]
pub struct ParseContext {
	pub content: String,
	pub filename: PathBuf,
}
impl ParseContext {
	pub fn new(content: String, filename: PathBuf) -> Self {
		Self { content, filename }
	}

	/// Create a NamedSource for miette diagnostics.
	pub fn named_source(&self) -> NamedSource<String> {
		NamedSource::new(self.filename.display().to_string(), self.content.clone())
	}

	/// Get byte offset for a given line number (1-indexed).
	pub fn line_offset(&self, line_num: usize) -> usize {
		self.content.lines().take(line_num.saturating_sub(1)).map(|l| l.len() + 1).sum()
	}

	/// Get span for an entire line (1-indexed line number).
	pub fn line_span(&self, line_num: usize) -> SourceSpan {
		let offset = self.line_offset(line_num);
		let len = self
			.content
			.lines()
			.nth(line_num.saturating_sub(1))
			.unwrap_or_else(|| panic!("line {line_num} out of bounds for content with {} lines", self.content.lines().count()))
			.len();
		(offset, len).into()
	}
}

/// Internal miette diagnostic for nice error rendering with source highlighting.
#[derive(Debug, miette::Diagnostic, thiserror::Error)]
enum ParseDiagnostic {
	#[error("file is empty")]
	#[diagnostic(code(tedi::parse::empty_file))]
	EmptyFile,

	#[error("invalid title line")]
	#[diagnostic(code(tedi::parse::invalid_title), help("title must be formatted as: '- [ ] Title <!-- url -->' or '- [x] Title <!-- url -->'"))]
	InvalidTitle {
		#[source_code]
		src: NamedSource<String>,
		#[label("expected checkbox prefix '- [ ] ' or '- [x] '")]
		span: SourceSpan,
		detail: String,
	},

	#[error("missing URL marker in title")]
	#[diagnostic(code(tedi::parse::missing_url_marker), help("title line must contain a URL marker: '<!-- url -->' or '<!--immutable url -->'"))]
	MissingUrlMarker {
		#[source_code]
		src: NamedSource<String>,
		#[label("expected '<!-- url -->' after title")]
		span: SourceSpan,
	},

	#[error("malformed URL marker")]
	#[diagnostic(code(tedi::parse::malformed_url_marker), help("URL marker must be: '<!-- url -->' with closing '-->'"))]
	MalformedUrlMarker {
		#[source_code]
		src: NamedSource<String>,
		#[label("unclosed or malformed comment marker")]
		span: SourceSpan,
	},

	#[error("unexpected indentation")]
	#[diagnostic(code(tedi::parse::bad_indent), help("indentation must use tabs, not spaces. configure your editor to preserve tabs in .tedi files"))]
	BadIndentation {
		#[source_code]
		src: NamedSource<String>,
		#[label("expected {expected_tabs} tab(s) of indentation")]
		span: SourceSpan,
		expected_tabs: usize,
	},

	#[error("invalid checkbox content: '{content}'")]
	#[diagnostic(
		code(tedi::parse::invalid_checkbox),
		help("valid checkbox values are: ' ' (open), 'x' (closed), '-' (not planned), or a number like '123' (duplicate of issue #123)")
	)]
	InvalidCheckbox {
		#[source_code]
		src: NamedSource<String>,
		#[label("unrecognized checkbox value")]
		span: SourceSpan,
		content: String,
	},

	#[error("duplicate reference to non-existent issue #{issue_number}")]
	#[diagnostic(code(tedi::parse::invalid_duplicate), help("the referenced issue does not exist in this repository"))]
	InvalidDuplicateReference {
		#[source_code]
		src: NamedSource<String>,
		#[label("issue #{issue_number} not found")]
		span: SourceSpan,
		issue_number: u64,
	},
}
