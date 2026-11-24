//! Error types for parsing issue files.
//!
//! Uses miette for rich diagnostics with source code spans.

#![allow(unused_assignments)] // Fields are read by miette's derive macro via attributes

use std::{backtrace::Backtrace, path::PathBuf};

use miette::{LabeledSpan, NamedSource, SourceSpan};
use tracing_error::SpanTrace;
use v_utils::macros::wrap_err;

use crate::{
	MilestoneLink,
	chain::{Edge, Kind, Node},
};

/// An ownership the chain forbids, drawn.
#[derive(Debug, thiserror::Error)]
#[error("{rendered}")]
pub struct ChainError {
	rendered: String,
}
impl From<ChainDiagnostic> for ChainError {
	fn from(diag: ChainDiagnostic) -> Self {
		use miette::ReportHandler as _;

		let handler = miette::MietteHandlerOpts::new()
			.context_lines(usize::MAX) // the whole chain, unlabelled links too
			.wrap_lines(false) // a wrapped URL opens nowhere
			.build();
		Self {
			rendered: std::fmt::from_fn(|f| handler.debug(&diag, f)).to_string(),
		}
	}
}

/// Error type for issue file parsing.
///
/// Contains pre-rendered miette diagnostic and SpanTrace captured at error creation time.
#[derive(Debug, thiserror::Error)]
#[error("{rendered}\n\n{spantrace}")]
pub struct ParseError {
	rendered: String,
	spantrace: SpanTrace,
	backtrace: Backtrace,
}
impl ParseError {
	fn from_diagnostic(diag: ParseDiagnostic) -> Self {
		let rendered = format!("{:?}", miette::Report::new(diag));
		Self {
			rendered,
			spantrace: SpanTrace::capture(),
			backtrace: Backtrace::capture(),
		}
	}

	pub fn empty_file(path: PathBuf) -> Self {
		Self::from_diagnostic(ParseDiagnostic::EmptyFile { path })
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

	pub fn child_not_in_hollow(src: NamedSource<String>, span: SourceSpan, issue_number: u64) -> Self {
		Self::from_diagnostic(ParseDiagnostic::ChildNotInHollow { src, span, issue_number })
	}

	pub fn invalid_composition(src: NamedSource<String>, span: SourceSpan, detail: String) -> Self {
		Self::from_diagnostic(ParseDiagnostic::InvalidComposition { src, span, detail })
	}
}

/// Error type for issue composition and structural invariant violations.
#[wrap_err]
#[derive(Debug, thiserror::Error)]
pub enum IssueError {
	/// A git-linked child (GitId selector) exists in virtual representation but not in HollowIssue.
	///
	/// This means either:
	/// - Internal bug: we constructed the HollowIssue incorrectly and missed a child
	/// - User error: user manually embedded an issue with a `<!-- @user url -->` marker, which is not permitted
	#[leaf]
	#[error("git-linked child #{issue_number} present in virtual but missing from HollowIssue: {detail}")]
	ErroneousComposition { issue_number: u64, detail: String },
	#[leaf]
	#[error("a completed issue can't have open descendants — close them too, or reopen it:\n{detail}")]
	OpenUnderCompleted { detail: String },
	#[leaf]
	#[error("{detail}")]
	LabelScheme { detail: String },
	#[leaf]
	#[error("an issue can't own a milestone — it owns issues and blockers; own the issue from the milestone instead:\n{detail}")]
	OwnsMilestone { detail: String },
}

/// Error when converting IssueIndex to a git number path.
/// Occurs when a Title selector is encountered but only GitId selectors are valid.
#[wrap_err]
#[derive(Debug, miette::Diagnostic, thiserror::Error)]
#[error("cannot convert IssueIndex to git number path: contains Title selector")]
#[diagnostic(
	code(tedi::index::title_in_git_path),
	help("git_num_path requires all selectors to be GitId (issue numbers). Title selectors indicate pending issues that haven't been synced to GitHub yet.")
)]
pub struct TitleInGitPathError {
	#[source_code]
	index_display: NamedSource<String>,
	#[label("expected GitId, found Title selector")]
	span: SourceSpan,
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

	/// Find the line containing a substring (searching from `start_line`, 1-indexed) and return its span.
	/// Falls back to `start_line` if not found.
	pub fn find_line_span(&self, needle: &str, start_line: usize) -> SourceSpan {
		for (i, line) in self.content.lines().enumerate().skip(start_line.saturating_sub(1)) {
			if line.contains(needle) {
				return self.line_span(i + 1);
			}
		}
		self.line_span(start_line)
	}
}

#[derive(Debug, miette::Diagnostic, thiserror::Error)]
pub(crate) enum ChainDiagnostic {
	#[error("{title}")]
	#[diagnostic(code("tedi::chain::loop"))]
	Loop {
		title: String,
		#[source_code]
		src: NamedSource<String>,
		#[label(collection)]
		labels: Vec<LabeledSpan>,
		#[help]
		help: String,
	},
	#[error("{title}")]
	#[diagnostic(code(tedi::chain::repo_twice))]
	RepoTwice {
		title: String,
		#[source_code]
		src: NamedSource<String>,
		#[label(collection)]
		labels: Vec<LabeledSpan>,
		#[help]
		help: String,
	},
	#[error("{title}")]
	#[diagnostic(code(tedi::chain::root))]
	Root {
		title: String,
		#[source_code]
		src: NamedSource<String>,
		#[label(collection)]
		labels: Vec<LabeledSpan>,
		#[help]
		help: String,
	},
	#[error("{} is not stored here, so what it owns is unknown", link.as_str())]
	#[diagnostic(code(tedi::chain::unknown), help("own it online: it is fetched with everything it contains"))]
	Unknown { link: MilestoneLink },
}
impl ChainDiagnostic {
	pub(crate) fn unknown(link: &MilestoneLink) -> Self {
		Self::Unknown { link: link.clone() }
	}

	/// `nodes`: each owns the next, the last the first.
	pub(crate) fn looped(nodes: &[Node], edge: Edge) -> Self {
		let (first, last) = (&nodes[0], &nodes[nodes.len() - 1]);
		let mut drawing = Drawing::default();
		drawing.line("╭─► ", first);
		for (i, node) in nodes.iter().enumerate().skip(1) {
			drawing.line(&format!("│   {}└─► ", "    ".repeat(i - 1)), node);
		}
		drawing.line("", format!("╰{}┘", "─".repeat(4 * nodes.len() - 1)));
		let closing = nodes.len();
		let labels = match edge {
			Edge::Added if nodes.len() == 1 => vec![drawing.label(closing, "added by this edit")],
			Edge::Added => vec![drawing.label(1, "added by this edit"), drawing.label(closing, &format!("already contains {first}"))],
			Edge::Held => vec![drawing.label(closing, &format!("contains {first}"))],
		};
		let would = if edge == Edge::Added { "would own" } else { "owns" };
		Self::Loop {
			title: format!("{} {would} itself", named(first)),
			src: drawing.source(),
			labels,
			help: match edge {
				Edge::Added if nodes.len() == 1 => "drop the new link".into(),
				Edge::Added => format!("drop the new link, or {} from {}", first.link.repo_info(), last.link.as_str()),
				Edge::Held => format!("drop one of its links, in {}", urls(nodes)),
			},
		}
	}

	/// `nodes`: each owns the next, the first and the last of one repo; `added`: the first below the edge.
	pub(crate) fn repo_twice(nodes: &[Node], added: usize, edge: Edge) -> Self {
		let last = nodes.len() - 1;
		let repo = nodes[0].link.repo_info();
		let drawing = Drawing::of_chain(nodes);
		let mut labels = vec![drawing.label(0, &format!("{repo} is here"))];
		match edge {
			Edge::Added if added == last => labels.push(drawing.label(last, "added by this edit, and here again")),
			Edge::Added => labels.extend([drawing.label(added, "added by this edit"), drawing.label(last, "and here again")]),
			Edge::Held => labels.push(drawing.label(last, "and here again")),
		}
		Self::RepoTwice {
			title: match edge {
				Edge::Added => format!("{} can't own {}: {repo} would be on one chain twice", named(&nodes[added - 1]), named(&nodes[added])),
				Edge::Held => format!("{repo} is on one chain twice"),
			},
			src: drawing.source(),
			labels,
			help: match edge {
				Edge::Added => "drop the new link: within a repo, milestones relate through their shape (`v1.0` → `v1.1`)".into(),
				Edge::Held => format!("within a repo, milestones relate through their shape (`v1.0` → `v1.1`): drop a link, in {}", urls(&nodes[..last])),
			},
		}
	}

	/// `nodes`: an owner, and the root it can't own.
	pub(crate) fn root(nodes: &[Node], kind: Kind, edge: Edge) -> Self {
		let drawing = Drawing::of_chain(nodes);
		let why = match kind {
			Kind::Sprint => "nothing owns a sprint",
			Kind::Bottlenecks => "only a sprint owns Bottlenecks",
			Kind::Milestone => unreachable!("a milestone is no root"),
		};
		Self::Root {
			title: format!("{} can't own {}: {why}", named(&nodes[0]), named(&nodes[1])),
			labels: match edge {
				Edge::Added => vec![drawing.label(1, "added by this edit")],
				Edge::Held => Vec::new(),
			},
			src: drawing.source(),
			help: match edge {
				Edge::Added => "drop the new link".into(),
				Edge::Held => format!("drop the link, in {}", urls(&nodes[..1])),
			},
		}
	}
}

/// A chain as text, and where in it each line's own text is.
#[derive(Default)]
struct Drawing {
	text: String,
	spans: Vec<(usize, usize)>,
}
impl Drawing {
	fn of_chain(nodes: &[Node]) -> Self {
		let mut drawing = Self::default();
		for (i, node) in nodes.iter().enumerate() {
			let lead = if i == 0 { String::new() } else { format!("{}└─► ", "    ".repeat(i - 1)) };
			drawing.line(&lead, node);
		}
		drawing
	}

	fn line(&mut self, lead: &str, text: impl std::fmt::Display) {
		let text = text.to_string();
		self.text.push_str(lead);
		self.spans.push((self.text.len(), text.len()));
		self.text.push_str(&text);
		self.text.push('\n');
	}

	fn label(&self, line: usize, says: &str) -> LabeledSpan {
		let (offset, len) = self.spans[line];
		LabeledSpan::new(Some(says.into()), offset, len)
	}

	fn source(self) -> NamedSource<String> {
		NamedSource::new("ownership", self.text)
	}
}

fn named(node: &Node) -> String {
	format!("{} {:?}", node.link.repo_info(), node.title)
}

fn urls(nodes: &[Node]) -> String {
	nodes.iter().map(|n| n.link.as_str()).collect::<Vec<_>>().join(", ")
}

/// Internal miette diagnostic for nice error rendering with source highlighting.
#[derive(Debug, miette::Diagnostic, thiserror::Error)]
enum ParseDiagnostic {
	#[error("issue file is empty, so it no longer defines an issue: {}", path.display())]
	#[diagnostic(
		code(tedi::parse::empty_file),
		help("a link (e.g. in a sprint or the urgent list) still points at this issue; restore its `- [ ] Title` line, or delete the file and the links to it")
	)]
	EmptyFile { path: PathBuf },

	#[error("invalid title line: {detail}")]
	#[diagnostic(code(tedi::parse::invalid_title), help("title must be formatted as: '- [ ] Title <!-- url -->' or '- [x] Title <!-- url -->'"))]
	InvalidTitle {
		#[source_code]
		src: NamedSource<String>,
		#[label("{detail}")]
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
		help("valid checkbox values are: ' ' (open), '.' (partial), '?' (maybe), 'x' (closed), '-' (not planned), or a number like '123' (duplicate of issue #123)")
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

	#[error("git-linked child issue #{issue_number} not found in HollowIssue.children")]
	#[diagnostic(
		code(tedi::parse::child_not_in_hollow),
		help("when parsing a git-linked child issue, it must exist in the provided HollowIssue.children map. This issue may have been renamed or the local state is out of sync.")
	)]
	ChildNotInHollow {
		#[source_code]
		src: NamedSource<String>,
		#[label("issue #{issue_number} not found in HollowIssue.children")]
		span: SourceSpan,
		issue_number: u64,
	},

	#[error("invalid composition after blockers section: {detail}")]
	#[diagnostic(
		code(tedi::parse::invalid_composition),
		help("after the blockers section, only checkbox list items (child issues) are allowed. Plain text or non-checkbox lists cannot follow blockers.")
	)]
	InvalidComposition {
		#[source_code]
		src: NamedSource<String>,
		#[label("{detail}")]
		span: SourceSpan,
		detail: String,
	},
}
