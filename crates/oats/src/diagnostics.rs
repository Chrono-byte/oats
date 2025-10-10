// Print a compact, rustc-like diagnostic to stderr.
//
// This is intentionally lightweight: it prints an "error:" header in red,
// the file path, and up to a few source lines as context. We can expand
// this later with real spans and caret markers.
pub fn report_error(file: Option<&str>, source: Option<&str>, message: &str, note: Option<&str>) {
    // ANSI red for "error"
    let red = "\x1b[31m";
    let reset = "\x1b[0m";

    if let Some(path) = file {
        eprintln!("{}error{}: {}", red, reset, message);
        eprintln!("  --> {}", path);
    } else {
        eprintln!("{}error{}: {}", red, reset, message);
    }

    if let Some(src) = source {
        // print up to first 6 lines for quick context
        for (i, line) in src.lines().enumerate().take(6) {
            eprintln!("{:4} | {}", i + 1, line);
        }
    }

    if let Some(note) = note {
        // ANSI blue for note
        let blue = "\x1b[34m";
        eprintln!("{}note{}: {}", blue, reset, note);
    }

    // If the message hints at a common fix, print a short help line.
    if message.contains("missing semicolon") {
        let green = "\x1b[32m";
        eprintln!(
            "{}help{}: try adding a trailing ';' to this statement",
            green, reset
        );
    }
}

// Convenience that prints an error then returns an anyhow::Error for callers
// who want to terminate via `?`.
pub fn report_error_and_bail<T>(
    file: Option<&str>,
    source: Option<&str>,
    message: &str,
    note: Option<&str>,
) -> anyhow::Result<T> {
    report_error(file, source, message, note);
    Err(anyhow::anyhow!("{}", message))
}

// Print an error for a specific byte-span within `source` with a caret
// pointing at the column. `span_start` and `span_end` are byte indices
// into `source` (0-based). If `file` is provided, it is printed in the
// header.
pub fn report_error_span(
    file: Option<&str>,
    source: &str,
    span_start: usize,
    message: &str,
    note: Option<&str>,
) {
    let red = "\x1b[31m";
    let reset = "\x1b[0m";

    // Compute line/column
    let mut byte_idx = 0usize;
    let mut line_no = 1usize;
    let mut col = 0usize;
    let mut found = false;
    for (lineno, line) in source.lines().enumerate() {
        let line_len = line.len() + 1; // include newline
        if span_start >= byte_idx && span_start < byte_idx + line_len {
            line_no = lineno + 1;
            col = span_start - byte_idx;
            found = true;
            break;
        }
        byte_idx += line_len;
    }
    if !found {
        // fallback
        line_no = source.lines().count();
        col = 0;
    }

    if let Some(path) = file {
        eprintln!("{}error{}: {}", red, reset, message);
        eprintln!("  --> {}:{}:{}", path, line_no, col + 1);
    } else {
        eprintln!("{}error{}: {}", red, reset, message);
    }

    // Print a couple of context lines: previous, current, next
    let lines: Vec<&str> = source.lines().collect();
    let total = lines.len();
    let idx = if line_no == 0 { 0 } else { line_no - 1 };
    let start = idx.saturating_sub(1);
    let end = if idx + 1 < total { idx + 1 } else { total - 1 };

    for (i, line) in lines.iter().enumerate().take(end + 1).skip(start) {
        eprintln!("{:4} | {}", i + 1, line);
        if i == idx {
            // caret under column
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

// Convenience that reports a span-aware error and returns anyhow::Error.
pub fn report_error_span_and_bail<T>(
    file: Option<&str>,
    source: &str,
    span_start: usize,
    message: &str,
    note: Option<&str>,
) -> anyhow::Result<T> {
    report_error_span(file, source, span_start, message, note);
    // Include a short suggestion in the returned error string for tests to assert on.
    let mut err_text = message.to_string();
    if message.contains("missing semicolon") {
        err_text.push_str(" -- hint: add a trailing ';' to this statement");
    }
    Err(anyhow::anyhow!("{}", err_text))
}

// Simple Diagnostic container used by lowering to propagate structured
// errors up to a single emission site.
#[derive(Debug, Clone)]
pub struct Diagnostic {
    pub message: String,
    pub file: Option<String>,
    pub note: Option<String>,
    // Optional byte-index into the source text where the error occurred.
    // When present and a source string is supplied to `emit_diagnostic`,
    // the diagnostics system will show a span-aware message with a
    // caret pointing at the correct column instead of printing the file
    // head.
    pub span_start: Option<usize>,
}

impl Diagnostic {
    pub fn simple(msg: impl Into<String>) -> Self {
        Diagnostic {
            message: msg.into(),
            file: None,
            note: None,
            span_start: None,
        }
    }

    /// Create a simple diagnostic that includes a byte-offset span into
    /// the source text. `span_start` is a 0-based byte index into the
    /// source; the reporter will compute the line/column from this index.
    pub fn simple_with_span(msg: impl Into<String>, span_start: usize) -> Self {
        Diagnostic {
            message: msg.into(),
            file: None,
            note: None,
            span_start: Some(span_start),
        }
    }
}

// Emit the diagnostic via the existing lightweight printer.
pub fn emit_diagnostic(d: &Diagnostic, source: Option<&str>) {
    if DIAGNOSTICS_ENABLED.load(Ordering::SeqCst) {
        // If we have a concrete span and source text, use the span-aware
        // reporter so the caret points at the correct column. Fall back to
        // the simpler header+context printer otherwise.
        if let (Some(start), Some(src)) = (d.span_start, source) {
            report_error_span(d.file.as_deref(), src, start, &d.message, d.note.as_deref());
        } else {
            report_error(d.file.as_deref(), source, &d.message, d.note.as_deref());
        }
    }
}

use std::sync::atomic::{AtomicBool, Ordering};

static DIAGNOSTICS_ENABLED: AtomicBool = AtomicBool::new(true);

/// Suppress diagnostic printing for the current scope. Returns a guard that
/// restores the previous enabled state when dropped. Tests can call
/// `let _g = diagnostics::suppress();` to silence stderr output while still
/// allowing callers to inspect returned Errors/Diagnostics.
pub fn suppress() -> SuppressGuard {
    let prev = DIAGNOSTICS_ENABLED.swap(false, Ordering::SeqCst);
    SuppressGuard { prev }
}

/// Internal guard type returned by `suppress()`.
pub struct SuppressGuard {
    prev: bool,
}

impl Drop for SuppressGuard {
    fn drop(&mut self) {
        DIAGNOSTICS_ENABLED.store(self.prev, Ordering::SeqCst);
    }
}
