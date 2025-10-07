/// Print a compact, rustc-like diagnostic to stderr.
///
/// This is intentionally lightweight: it prints an "error:" header in red,
/// the file path, and up to a few source lines as context. We can expand
/// this later with real spans and caret markers.
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
}

/// Convenience that prints an error then returns an anyhow::Error for callers
/// who want to terminate via `?`.
pub fn report_error_and_bail<T>(
    file: Option<&str>,
    source: Option<&str>,
    message: &str,
    note: Option<&str>,
) -> anyhow::Result<T> {
    report_error(file, source, message, note);
    Err(anyhow::anyhow!("{}", message))
}

/// Print an error for a specific byte-span within `source` with a caret
/// pointing at the column. `span_start` and `span_end` are byte indices
/// into `source` (0-based). If `file` is provided, it is printed in the
/// header.
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
    let start = if idx >= 1 { idx - 1 } else { 0 };
    let end = if idx + 1 < total { idx + 1 } else { total - 1 };

    for i in start..=end {
        eprintln!("{:4} | {}", i + 1, lines[i]);
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
}

/// Convenience that reports a span-aware error and returns anyhow::Error.
pub fn report_error_span_and_bail<T>(
    file: Option<&str>,
    source: &str,
    span_start: usize,
    message: &str,
    note: Option<&str>,
) -> anyhow::Result<T> {
    report_error_span(file, source, span_start, message, note);
    Err(anyhow::anyhow!("{}", message))
}
