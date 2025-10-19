//! Diagnostic reporting utilities for the Oats compiler.
//!
//! This module provides lightweight, rustc-style error reporting functions
//! that emit colored diagnostic messages to stderr. The implementation
//! focuses on clear, actionable error messages with source context and
//! optional span-based highlighting.
//!
//! # Design Philosophy
//!
//! The diagnostic system is intentionally minimal to avoid complexity while
//! providing sufficient detail for developers to understand and fix issues.
//! All functions support optional file paths and source text for context,
//! with fallback behavior when information is unavailable.
//!
//! # Error Formatting
//!
//! - **Error messages**: Red "error:" prefix with clear description
//! - **File locations**: Rust-style "filename:line:column" format
//! - **Source context**: Up to 6 lines of surrounding code
//! - **Notes and hints**: Blue "note:" and green "help:" annotations
//! - **Span highlighting**: Caret markers pointing to specific columns

/// Prints a compact, rustc-style diagnostic message to stderr.
///
/// This function emits an error message with optional file context and source
/// code preview. The output uses ANSI colors for visual distinction and follows
/// Rust compiler diagnostic conventions for familiarity.
///
/// # Arguments
/// * `file` - Optional file path to display in the diagnostic header
/// * `source` - Optional source text to show for context (first 6 lines)
/// * `message` - Primary error message to display
/// * `note` - Optional additional note to append
///
/// # Example Output
/// ```text
/// error: missing semicolon after statement
///   --> main.oats:5:12
///    5 | let x = 42
///      |           ^
/// note: try adding ';' after this expression
/// ```
pub fn report_error(file: Option<&str>, source: Option<&str>, message: &str, note: Option<&str>) {
    // Use ANSI red escape sequence for error highlighting
    let red = "\x1b[31m";
    let reset = "\x1b[0m";

    if let Some(path) = file {
        let sanitized_path = sanitize_file_path(path);
        eprintln!("{}error{}: {}", red, reset, message);
        eprintln!("  --> {}", sanitized_path);
    } else {
        eprintln!("{}error{}: {}", red, reset, message);
    }

    if let Some(src) = source {
        // Display up to first 6 lines for quick context
        for (i, line) in src.lines().enumerate().take(6) {
            eprintln!("{:4} | {}", i + 1, line);
        }
    }

    if let Some(note) = note {
        // Use ANSI blue escape sequence for note highlighting
        let blue = "\x1b[34m";
        eprintln!("{}note{}: {}", blue, reset, note);
    }

    // Provide helpful suggestion for common syntax errors
    if message.contains("missing semicolon") {
        let green = "\x1b[32m";
        eprintln!(
            "{}help{}: try adding a trailing ';' to this statement",
            green, reset
        );
    }
}

/// Prints a diagnostic error and returns an `anyhow::Error` for early termination.
///
/// This convenience function combines error reporting with error propagation,
/// allowing callers to emit a diagnostic message and immediately return an
/// error via the `?` operator. The function delegates to `report_error` for
/// consistent formatting and then wraps the message in an `anyhow::Error`.
///
/// # Arguments
/// * `file` - Optional file path for diagnostic context
/// * `source` - Optional source text for code preview
/// * `message` - Primary error message
/// * `note` - Optional additional diagnostic note
///
/// # Returns
/// Always returns `Err(anyhow::Error)` containing the error message
pub fn report_error_and_bail<T>(
    file: Option<&str>,
    source: Option<&str>,
    message: &str,
    note: Option<&str>,
) -> anyhow::Result<T> {
    report_error(file, source, message, note);
    Err(anyhow::anyhow!("{}", message))
}

/// Prints a span-aware diagnostic with caret highlighting at the error location.
///
/// This function provides precise error reporting by computing line and column
/// positions from byte offsets and displaying a caret marker pointing to the
/// exact location of the error. The output format follows rustc conventions
/// for familiar, actionable diagnostics.
///
/// # Arguments
/// * `file` - Optional file path to display in the diagnostic header
/// * `source` - Source text containing the error (required for span calculation)
/// * `span_start` - Zero-based byte index of the error location
/// * `message` - Primary error message to display
/// * `note` - Optional additional diagnostic note
///
/// # Behavior
/// The function converts the byte offset to line:column coordinates and displays
/// the relevant source line with a caret (^) marker positioned under the error.
/// If the span calculation fails, fallback coordinates are used to ensure
/// graceful degradation.
pub fn report_error_span(
    file: Option<&str>,
    source: &str,
    span_start: usize,
    message: &str,
    note: Option<&str>,
) {
    let red = "\x1b[31m";
    let reset = "\x1b[0m";

    // Convert byte offset to line and column coordinates
    let mut byte_idx = 0usize;
    let mut line_no = 1usize;
    let mut col = 0usize;
    let mut found = false;
    for (lineno, line) in source.lines().enumerate() {
        let line_len = line.len() + 1; // Account for newline character
        if span_start >= byte_idx && span_start < byte_idx + line_len {
            line_no = lineno + 1;
            col = span_start - byte_idx;
            found = true;
            break;
        }
        byte_idx += line_len;
    }
    if !found {
        // Use fallback coordinates if span calculation fails
        line_no = source.lines().count();
        col = 0;
    }

    if let Some(path) = file {
        let sanitized_path = sanitize_file_path(path);
        eprintln!("{}error{}: {}", red, reset, message);
        eprintln!("  --> {}:{}:{}", sanitized_path, line_no, col + 1);
    } else {
        eprintln!("{}error{}: {}", red, reset, message);
    }

    // Display context lines with the error line highlighted
    let lines: Vec<&str> = source.lines().collect();
    let total = lines.len();
    let idx = if line_no == 0 { 0 } else { line_no - 1 };
    let start = idx.saturating_sub(1);
    let end = if idx + 1 < total { idx + 1 } else { total - 1 };

    for (i, line) in lines.iter().enumerate().take(end + 1).skip(start) {
        eprintln!("{:4} | {}", i + 1, line);
        if i == idx {
            // Position caret marker under the error column
            let mut caret = String::new();
            for _ in 0..col {
                caret.push(' ');
            }
            caret.push('^');
            eprintln!("     | {}", caret);
        }
    }

    if let Some(note) = note {
        let blue = "\x1b[34m";
        eprintln!("{}note{}: {}", blue, reset, note);
    }

    if message.contains("missing semicolon") {
        let green = "\x1b[32m";
        eprintln!(
            "{}help{}: add a trailing ';' to the highlighted statement",
            green, reset
        );
    }
}

/// Reports a span-aware diagnostic error and returns an `anyhow::Error` for termination.
///
/// This convenience function combines span-aware error reporting with error
/// propagation, allowing callers to emit a precise diagnostic with caret
/// highlighting and immediately return an error. The function delegates to
/// `report_error_span` for consistent formatting.
///
/// # Arguments
/// * `file` - Optional file path for diagnostic context
/// * `source` - Source text containing the error location
/// * `span_start` - Zero-based byte index of the error location
/// * `message` - Primary error message
/// * `note` - Optional additional diagnostic note
///
/// # Returns
/// Always returns `Err(anyhow::Error)` with enhanced error text including hints
pub fn report_error_span_and_bail<T>(
    file: Option<&str>,
    source: &str,
    span_start: usize,
    message: &str,
    note: Option<&str>,
) -> anyhow::Result<T> {
    report_error_span(file, source, span_start, message, note);
    // Enhance error text with contextual hints for test assertion compatibility
    let mut err_text = message.to_string();
    if message.contains("missing semicolon") {
        err_text.push_str(" -- hint: add a trailing ';' to this statement");
    }
    Err(anyhow::anyhow!("{}", err_text))
}

/// Structured diagnostic container for propagating compiler errors.
///
/// This type provides a uniform way to collect and propagate diagnostic
/// information through the compilation pipeline. The `Diagnostic` struct
/// supports both simple error messages and span-aware diagnostics with
/// precise source location information.
///
/// # Design
///
/// The diagnostic system uses this container to decouple error detection
/// from error emission, allowing the compiler to collect multiple errors
/// before deciding how to present them to the user. The optional `span_start`
/// field enables precise error highlighting when source text is available.
#[derive(Debug, Clone)]
pub struct Diagnostic {
    /// Primary error message describing the issue
    pub message: String,
    /// Optional file path where the error occurred
    pub file: Option<String>,
    /// Optional additional context or suggestion
    pub note: Option<String>,
    /// Optional byte offset into source text for span-aware highlighting.
    /// When present and source text is provided to `emit_diagnostic`,
    /// the system displays a caret-highlighted diagnostic instead of
    /// basic file header context.
    pub span_start: Option<usize>,
}

impl Diagnostic {
    /// Creates a simple diagnostic with only an error message.
    ///
    /// This constructor is suitable for general error cases where precise
    /// source location information is not available or not required.
    /// Additional context can be added later using the struct fields.
    ///
    /// # Arguments
    /// * `msg` - Error message describing the issue
    pub fn simple(msg: impl Into<String>) -> Self {
        Diagnostic {
            message: msg.into(),
            file: None,
            note: None,
            span_start: None,
        }
    }

    /// Creates a span-aware diagnostic with precise source location.
    ///
    /// This constructor enables precise error highlighting by providing a
    /// byte offset into the source text. The diagnostic system will compute
    /// line and column coordinates from this offset and display a caret
    /// marker at the exact error location.
    ///
    /// # Arguments
    /// * `msg` - Error message describing the issue
    /// * `span_start` - Zero-based byte index into the source text
    pub fn simple_with_span(msg: impl Into<String>, span_start: usize) -> Self {
        Diagnostic {
            message: msg.into(),
            file: None,
            note: None,
            span_start: Some(span_start),
        }
    }
}

/// Emits a diagnostic using the appropriate formatting based on available information.
///
/// This function serves as the central emission point for `Diagnostic` instances,
/// automatically selecting between span-aware and basic diagnostic formatting
/// based on the presence of source text and span information. The function
/// respects the global diagnostics enable/disable state for testing scenarios.
///
/// # Arguments
/// * `d` - Diagnostic instance containing error information
/// * `source` - Optional source text for span-aware highlighting
pub fn emit_diagnostic(d: &Diagnostic, source: Option<&str>) {
    if DIAGNOSTICS_ENABLED.load(Ordering::SeqCst) {
        // Select appropriate diagnostic format based on available information.
        // Span-aware diagnostics provide precise error highlighting when both
        // source text and byte offset are available.
        if let (Some(span), Some(src)) = (d.span_start, source) {
            report_error_span(d.file.as_deref(), src, span, &d.message, d.note.as_deref());
        } else {
            report_error(d.file.as_deref(), source, &d.message, d.note.as_deref());
        }
    }
}

use std::sync::atomic::{AtomicBool, Ordering};

static DIAGNOSTICS_ENABLED: AtomicBool = AtomicBool::new(true);

/// Sanitizes file paths to prevent information disclosure.
///
/// This function removes potentially sensitive path components such as
/// absolute paths, home directory references, or system-specific information.
/// Only the filename is preserved to prevent leaking system structure.
///
/// # Arguments
/// * `path` - The file path to sanitize
///
/// # Returns
/// A sanitized version of the path safe for diagnostic output
fn sanitize_file_path(path: &str) -> String {
    // Extract only the filename to prevent path disclosure
    std::path::Path::new(path)
        .file_name()
        .and_then(|n| n.to_str())
        .unwrap_or("file")
        .to_string()
}

/// Temporarily suppresses diagnostic output for testing scenarios.
///
/// This function provides a mechanism to disable diagnostic printing within
/// a specific scope, allowing tests to verify error detection without
/// cluttering stderr output. The returned guard automatically restores the
/// previous diagnostic state when dropped.
///
/// # Usage
/// ```rust
/// use oatsc::diagnostics;
/// let _guard = diagnostics::suppress();
/// // Diagnostics are now silenced within this scope
/// // Guard automatically restores previous state on drop
/// ```
///
/// # Returns
/// A `SuppressGuard` that restores diagnostic output when dropped
pub fn suppress() -> SuppressGuard {
    let prev = DIAGNOSTICS_ENABLED.swap(false, Ordering::SeqCst);
    SuppressGuard { prev }
}

/// RAII guard that manages diagnostic output state.
///
/// This guard type ensures that diagnostic suppression is properly scoped
/// and automatically restored when the guard goes out of scope. The guard
/// should not be manually manipulated; it implements `Drop` to handle
/// state restoration automatically.
pub struct SuppressGuard {
    prev: bool,
}

impl Drop for SuppressGuard {
    fn drop(&mut self) {
        DIAGNOSTICS_ENABLED.store(self.prev, Ordering::SeqCst);
    }
}
