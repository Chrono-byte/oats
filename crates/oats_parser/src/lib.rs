//! Oats Parser
//!
//! A clean, expressive parser for the Oats language using chumsky.

mod common;
mod expr;
mod function;
mod parsed;
mod stmt;
pub mod tokenizer;
mod types;

use chumsky::prelude::*;
use oats_ast::*;

pub use parsed::{parse_module_with_metadata, ParsedModule};

/// Information about a delimiter mismatch error
struct DelimiterError {
    /// The closing delimiter that caused the error
    closing_delim: char,
    /// Position of the closing delimiter (byte offset)
    closing_pos: usize,
    /// Position of the last matched opening delimiter (byte offset), if any
    opening_pos: Option<usize>,
    /// Whether this is a missing opening delimiter (true) or unclosed delimiter (false)
    is_missing_opening: bool,
}

/// Scan the source code to find delimiter mismatches.
///
/// Returns information about unmatched delimiters, prioritizing errors near
/// the end of the input where parsing likely failed.
///
/// Note: This is a simple scanner that doesn't handle strings or comments.
/// For more accurate results, it should be integrated with the tokenizer.
fn scan_delimiter_errors(input: &str) -> Option<DelimiterError> {
    let mut stack: Vec<(char, usize)> = Vec::new(); // (delimiter, position)
    let mut last_unmatched_closing: Option<(char, usize, Option<usize>)> = None;

    for (pos, ch) in input.char_indices() {
        match ch {
            '(' | '[' | '{' => {
                stack.push((ch, pos));
            }
            ')' => {
                if let Some((opening, opening_pos)) = stack.pop() {
                    if opening != '(' {
                        // Mismatched delimiter type - keep the first one we find
                        if last_unmatched_closing.is_none() {
                            last_unmatched_closing = Some((ch, pos, Some(opening_pos)));
                        }
                    }
                } else {
                    // No matching opening delimiter
                    if last_unmatched_closing.is_none() {
                        last_unmatched_closing = Some((ch, pos, None));
                    }
                }
            }
            ']' => {
                if let Some((opening, opening_pos)) = stack.pop() {
                    if opening != '[' {
                        // Mismatched delimiter type
                        if last_unmatched_closing.is_none() {
                            last_unmatched_closing = Some((ch, pos, Some(opening_pos)));
                        }
                    }
                } else {
                    // No matching opening delimiter
                    if last_unmatched_closing.is_none() {
                        last_unmatched_closing = Some((ch, pos, None));
                    }
                }
            }
            '}' => {
                if let Some((opening, opening_pos)) = stack.pop() {
                    if opening != '{' {
                        // Mismatched delimiter type
                        if last_unmatched_closing.is_none() {
                            last_unmatched_closing = Some((ch, pos, Some(opening_pos)));
                        }
                    }
                } else {
                    // No matching opening delimiter
                    if last_unmatched_closing.is_none() {
                        last_unmatched_closing = Some((ch, pos, None));
                    }
                }
            }
            _ => {}
        }
    }

    if let Some((closing, closing_pos, opening_pos)) = last_unmatched_closing {
        Some(DelimiterError {
            closing_delim: closing,
            closing_pos,
            opening_pos,
            is_missing_opening: opening_pos.is_none(),
        })
    } else if !stack.is_empty() {
        // Unclosed delimiter at the end
        let (opening, opening_pos) = stack.last().copied().unwrap();
        Some(DelimiterError {
            closing_delim: match opening {
                '(' => ')',
                '[' => ']',
                '{' => '}',
                _ => return None,
            },
            closing_pos: input.len(),
            opening_pos: Some(opening_pos),
            is_missing_opening: false,
        })
    } else {
        None
    }
}

/// Convert a byte offset to line and column coordinates.
fn byte_to_line_col(input: &str, byte_offset: usize) -> (usize, usize) {
    let mut line = 1;
    let mut col = 0;
    let mut byte_idx = 0;

    for ch in input.chars() {
        if byte_idx >= byte_offset {
            break;
        }
        if ch == '\n' {
            line += 1;
            col = 0;
        } else {
            col += 1;
        }
        byte_idx += ch.len_utf8();
    }

    (line, col)
}

/// Get a line of source code by line number (1-indexed).
fn get_line(input: &str, line_no: usize) -> Option<&str> {
    input.lines().nth(line_no - 1)
}

/// Format a delimiter error with context showing both the error location
/// and the related opening delimiter.
fn format_delimiter_error(err: &DelimiterError, input: &str) -> String {
    let (closing_line, closing_col) = byte_to_line_col(input, err.closing_pos);
    let closing_line_str = get_line(input, closing_line).unwrap_or("");

    let mut msg = if err.is_missing_opening {
        let opening_char = match err.closing_delim {
            ')' => '(',
            ']' => '[',
            '}' => '{',
            _ => return format!("unexpected closing delimiter `{}`", err.closing_delim),
        };
        format!(
            "missing open `{}` for a `{}` delimiter",
            opening_char, err.closing_delim
        )
    } else {
        format!(
            "unclosed delimiter `{}`",
            match err.closing_delim {
                ')' => '(',
                ']' => '[',
                '}' => '{',
                _ => return format!("unclosed delimiter"),
            }
        )
    };

    // Add file location (using generic filename, actual path will be added by compiler layer)
    msg.push_str(&format!("\n  --> file.oats:{}:{}", closing_line, closing_col + 1));
    msg.push_str("\n   |");

    // Show opening delimiter location if available
    if let Some(opening_pos) = err.opening_pos {
        let (opening_line, opening_col) = byte_to_line_col(input, opening_pos);
        if let Some(opening_line_str) = get_line(input, opening_line) {
            msg.push_str(&format!("\n{:3} | {}", opening_line, opening_line_str));
            // Calculate the position of the opening delimiter
            let opening_caret_pos = opening_col.min(opening_line_str.chars().count());
            let opening_caret = " ".repeat(opening_caret_pos) + "-";
            msg.push_str(&format!(
                "\n   | {} the last matched opening delimiter",
                opening_caret
            ));
        }
    }

    // Show closing delimiter location
    msg.push_str(&format!("\n{:3} | {}", closing_line, closing_line_str));
    let caret_pos = closing_col.min(closing_line_str.chars().count());
    let caret = " ".repeat(caret_pos) + "^";
    if err.is_missing_opening {
        msg.push_str(&format!(
            "\n   | {} missing open `{}` for this delimiter",
            caret,
            match err.closing_delim {
                ')' => '(',
                ']' => '[',
                '}' => '{',
                _ => '?',
            }
        ));
    } else {
        msg.push_str(&format!(
            "\n   | {} unclosed delimiter",
            caret
        ));
    }

    msg
}

/// Format a chumsky error into a readable message.
///
/// This function attempts to extract meaningful information from chumsky errors
/// by using the Display trait and extracting span information when available.
/// For delimiter errors, it provides enhanced diagnostics showing both the error
/// location and the related opening delimiter.
fn format_chumsky_error<E: std::fmt::Display + std::fmt::Debug>(err: &E, input: &str) -> String {
    // Try to get span information if the error has it
    // For chumsky 0.11.2, errors may have different structures
    let err_str = format!("{}", err);
    let err_debug = format!("{:?}", err);

    // Check for delimiter errors first
    if let Some(delim_err) = scan_delimiter_errors(input) {
        return format_delimiter_error(&delim_err, input);
    }

    // If the error message is generic ("error", "EmptyErr", etc.), provide more context
    let is_generic = err_str == "error"
        || err_str == "EmptyErr"
        || err_str.contains("Empty")
        || err_str.len() < 10; // Very short error messages are likely not helpful

    if is_generic {
        // Try to find where the error occurred by looking at the end of input
        // This is a reasonable assumption for "Empty" errors which typically mean
        // the parser expected something but found end of input
        let mut msg = "unexpected end of input".to_string();

        // Calculate line and column for end of input
        let mut line_no = 1;
        let mut col = 0;
        for (_idx, ch) in input.char_indices() {
            if ch == '\n' {
                line_no += 1;
                col = 0;
            } else {
                col += 1;
            }
        }

        msg.push_str(&format!(" at line {}, column {}", line_no, col + 1));

        // Get the last line for context
        if let Some(last_line) = input.lines().last() {
            if !last_line.trim().is_empty() {
                msg.push_str(&format!("\n  {} | {}", line_no, last_line));
                msg.push_str(&format!("\n     | {}^", " ".repeat(col)));
            }
        }

        // If the debug output has more information, append it
        if err_debug != err_str && !err_debug.contains("EmptyErr") {
            msg.push_str(&format!(" ({})", err_debug));
        }

        msg
    } else {
        // Use the error's Display implementation, which should be more informative
        err_str
    }
}

/// Parse a string into an Oats AST Module.
pub fn parse_module(input: &str) -> Result<Module, String> {
    module_parser()
        .parse(input)
        .into_result()
        .map_err(|errors| {
            if errors.is_empty() {
                "Parse error".to_string()
            } else {
                let mut msg = format!("Parse error: found {} error(s)\n", errors.len());
                for (i, err) in errors.iter().take(5).enumerate() {
                    msg.push_str(&format!(
                        "  Error {}: {}\n",
                        i + 1,
                        format_chumsky_error(err, input)
                    ));
                }
                if errors.len() > 5 {
                    msg.push_str(&format!("  ... and {} more error(s)\n", errors.len() - 5));
                }
                msg
            }
        })
}

/// Top-level module parser.
///
/// A module is a sequence of statements, terminated by end of input.
fn module_parser<'a>() -> impl Parser<'a, &'a str, Module> + 'a {
    recursive(|stmt| stmt::stmt_parser(stmt).boxed())
        .repeated()
        .collect::<Vec<_>>()
        .then_ignore(end())
        .map_with(|body, extra| Module {
            body,
            span: extra.span().into(),
        })
        .labelled("module")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_export_function() {
        let input = "export function main(): void { return; }";
        let result = parse_module(input);
        assert!(
            result.is_ok(),
            "export function should parse: {:?}",
            result.err()
        );
        let module = result.unwrap();
        assert_eq!(module.body.len(), 1);
        if let Stmt::FnDecl(fn_decl) = &module.body[0] {
            assert_eq!(fn_decl.ident.sym, "main");
        } else {
            panic!("Expected FnDecl, got {:?}", module.body[0]);
        }
    }

    #[test]
    fn test_simple_function() {
        let input = "function main(): void {}";
        let result = parse_module(input);
        assert!(result.is_ok());
    }

    #[test]
    fn test_export_function_from_file() {
        let content = std::fs::read_to_string("../examples/proper_tests/basic_types.oats")
            .or_else(|_| std::fs::read_to_string("examples/proper_tests/basic_types.oats"))
            .expect("Failed to read test file");
        let result = parse_module(&content);
        assert!(
            result.is_ok(),
            "export function from file should parse: {:?}",
            result.err()
        );
        let module = result.unwrap();
        assert!(
            module.body.len() > 0,
            "Module should have at least one statement"
        );
        if let Stmt::FnDecl(fn_decl) = &module.body[0] {
            assert_eq!(fn_decl.ident.sym, "main");
        } else {
            panic!("Expected FnDecl, got {:?}", module.body[0]);
        }
    }
}
