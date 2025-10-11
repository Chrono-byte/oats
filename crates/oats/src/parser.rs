//! Oats parser utilities
//!
//! This module wraps `deno_ast` parsing and enforces a small set of
//! project-specific diagnostics and stylistic constraints (for example,
//! requiring semicolons and banning `var`). The parser returns a
//! `ParsedModule` containing both the parsed AST and the original source
//! text, which are used by later passes to report span-based diagnostics.
//!
//! # Resource Limits (Security Hardening)
//!
//! To prevent denial-of-service attacks, the parser enforces the following limits:
//!
//! - **MAX_SOURCE_SIZE**: Maximum source file size (default: 10 MB)
//!   - Configurable via `OATS_MAX_SOURCE_BYTES` environment variable
//!   - Prevents memory exhaustion from extremely large input files
//!
//! - **Runtime Recursion Depth**: Maximum recursion depth during execution (32 levels)
//!   - Enforced in the runtime (`crates/runtime/src/lib.rs::MAX_RECURSION_DEPTH`)
//!   - Prevents stack overflow from deeply recursive functions
//!   - Not configurable (hard limit)
//!
//! These limits provide defense-in-depth against malicious or malformed input.

use crate::diagnostics;
use anyhow::Result;
use deno_ast::swc::ast;
use deno_ast::{MediaType, ParseParams, ParsedSource, SourceTextInfo, parse_module};
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use url::Url;

/// Maximum source file size in bytes (default: 10 MB)
/// Override with OATS_MAX_SOURCE_BYTES environment variable
static MAX_SOURCE_SIZE: AtomicUsize = AtomicUsize::new(10 * 1024 * 1024);

/// Flag indicating limits have been initialized
static LIMITS_INITIALIZED: AtomicBool = AtomicBool::new(false);

/// Initialize parser resource limits from environment variables
fn init_parser_limits() {
    if LIMITS_INITIALIZED.load(Ordering::Relaxed) {
        return;
    }

    // Parse OATS_MAX_SOURCE_BYTES (default: 10 MB)
    if let Ok(val) = std::env::var("OATS_MAX_SOURCE_BYTES")
        && let Ok(limit) = val.parse::<usize>()
    {
        MAX_SOURCE_SIZE.store(limit, Ordering::Relaxed);
    }

    LIMITS_INITIALIZED.store(true, Ordering::Relaxed);
}

pub struct ParsedModule {
    pub parsed: ParsedSource,
    // Original source text (no preprocessing)
    pub source: String,
}

/// Parse a TypeScript source string into a `ParsedModule` and run
/// lightweight project-specific checks.
///
/// The function performs the following additional checks beyond parsing:
/// - Enforces maximum source size limit to prevent resource exhaustion
/// - Enforces that certain statement kinds end with semicolons (conservative
///   style choice to simplify span handling).
/// - Rejects usage of the `var` keyword in favor of `let`/`const`.
///
/// # Arguments
/// * `source_code` - source text to parse
/// * `file_path` - optional path used to create a `file://` URL for diagnostics
///
/// # Security
/// Checks source size against MAX_SOURCE_SIZE before parsing to prevent DoS.
pub fn parse_oats_module(source_code: &str, file_path: Option<&str>) -> Result<ParsedModule> {
    // Initialize resource limits on first call
    init_parser_limits();

    // SECURITY: Check source size limit before parsing
    let max_size = MAX_SOURCE_SIZE.load(Ordering::Relaxed);
    if source_code.len() > max_size {
        anyhow::bail!(
            "Source file too large: {} bytes (limit: {} bytes). \
             Set OATS_MAX_SOURCE_BYTES to increase.",
            source_code.len(),
            max_size
        );
    }

    // Strip BOM (Byte Order Mark) if present - deno_ast requires this
    // UTF-8 BOM is 0xEF 0xBB 0xBF
    let source_without_bom = if source_code.starts_with('\u{FEFF}') {
        &source_code[3..]
    } else {
        source_code
    };

    let sti = SourceTextInfo::from_string(source_without_bom.to_string());
    // Use the provided file_path (if any) to create a proper file:// URL so
    // diagnostics and span-based tooling can show accurate paths. Fall back
    // to a generic placeholder when no path is available.
    let specifier = if let Some(p) = file_path {
        // Try to canonicalize to an absolute path where possible; if that
        // fails, still attempt to produce a file:// URL from the provided
        // path string.
        match std::fs::canonicalize(p) {
            Ok(abs) => Url::from_file_path(abs)
                .map_err(|()| anyhow::anyhow!("failed to convert path to file URL: {}", p))?,
            Err(_) => Url::from_file_path(p).unwrap_or_else(|_| {
                // Last-resort: treat as a plain file URL using the original
                // string (not ideal but better than a constant placeholder).
                Url::parse("file://file.ts").unwrap()
            }),
        }
    } else {
        Url::parse("file://file.ts")?
    };

    let params = ParseParams {
        specifier,
        text: sti.text().clone(),
        media_type: MediaType::TypeScript,
        // Capture tokens so we can validate semicolon presence if needed.
        capture_tokens: true,
        scope_analysis: false,
        maybe_syntax: None,
    };

    let parsed = parse_module(params)?;

    // Enforce semicolons for statement-terminated constructs (conservative)
    // This behavior mirrors languages like Rust/C++ where semicolons are
    // required after expression statements, returns, and declarations.
    fn stmt_requires_semicolon(stmt: &ast::Stmt) -> bool {
        match stmt {
            ast::Stmt::Expr(_) => true,
            ast::Stmt::Decl(ast::Decl::Var(_)) => true,
            ast::Stmt::Return(_) => true,
            ast::Stmt::Break(_) => true,
            ast::Stmt::Continue(_) => true,
            ast::Stmt::Throw(_) => true,
            ast::Stmt::Debugger(_) => true,
            // Empty stmt is just a semicolon and is fine
            _ => false,
        }
    }

    fn span_hi_usize(span: &deno_ast::swc::common::Span) -> usize {
        // BytePos is a wrapper around u32
        span.hi.0 as usize
    }

    // Helper: scan forward from span.hi to find the next non-whitespace,
    // non-comment character and check whether it's a semicolon. This
    // handles trailing comments (// or /* */) more robustly.
    fn has_trailing_semicolon(
        span: &deno_ast::swc::common::Span,
        source: &str,
        parsed: &ParsedSource,
    ) -> bool {
        let hi = span_hi_usize(span);
        let lo = span.lo.0 as usize;
        let bytes = source.as_bytes();
        let len = bytes.len();
        // If a semicolon appears anywhere inside the span, consider it present.
        if lo < len {
            let end = if hi > len { len } else { hi };
            if lo < end && bytes[lo..end].contains(&b';') {
                return true;
            }
        }
        // quick checks: if semicolon is exactly at hi or immediately before
        if hi < len && bytes[hi] == b';' {
            return true;
        }
        let prev_idx = hi.saturating_sub(1);
        if hi > 0 && prev_idx < len && bytes[prev_idx] == b';' {
            return true;
        }
        // If tokens were captured, try a token-based check: find the token
        // immediately after the statement span and see if it's a semicolon.
        use swc_ecma_lexer::token::Token as SwcToken;
        let tok_slice = parsed.tokens();
        if !tok_slice.is_empty() {
            // tokens is a Vec of Token objects with spans; find the first
            // token whose span.lo >= span.hi (i.e., token after statement)
            for tok in tok_slice.iter() {
                // TokenAndSpan has `span` with lo/hi
                let tspan = tok.span;
                let tlo = tspan.lo.0 as usize;
                if tlo >= hi {
                    // Prefer matching token kind directly
                    if tok.token == SwcToken::Semi {
                        return true;
                    }
                    break;
                }
            }
        }
        let mut i = if hi >= len { len } else { hi };
        // skip whitespace and comments forward
        while i < bytes.len() {
            // skip ASCII whitespace
            if bytes[i] == b' ' || bytes[i] == b'\t' || bytes[i] == b'\r' || bytes[i] == b'\n' {
                i += 1;
                continue;
            }
            // line comment // -> skip to end of line
            if i + 1 < bytes.len() && bytes[i] == b'/' && bytes[i + 1] == b'/' {
                i += 2;
                while i < bytes.len() && bytes[i] != b'\n' {
                    i += 1;
                }
                continue;
            }
            // block comment /* ... */
            if i + 1 < bytes.len() && bytes[i] == b'/' && bytes[i + 1] == b'*' {
                i += 2;
                while i + 1 < bytes.len() {
                    if bytes[i] == b'*' && bytes[i + 1] == b'/' {
                        i += 2;
                        break;
                    }
                    i += 1;
                }
                continue;
            }
            // found a non-whitespace/comment char
            return bytes[i] == b';';
        }
        false
    }

    // Walk top-level module items and function bodies
    for item in parsed.program_ref().body() {
        if let deno_ast::ModuleItemRef::Stmt(stmt) = item {
            if stmt_requires_semicolon(stmt) {
                // Get stmt span and inspect source
                let span = match stmt {
                    ast::Stmt::Expr(es) => es.span,
                    ast::Stmt::Decl(ast::Decl::Var(v)) => v.span,
                    ast::Stmt::Return(r) => r.span,
                    ast::Stmt::Break(b) => b.span,
                    ast::Stmt::Continue(c) => c.span,
                    ast::Stmt::Throw(t) => t.span,
                    ast::Stmt::Debugger(d) => d.span,
                    _ => continue,
                };
                if !has_trailing_semicolon(&span, source_code, &parsed) {
                    let start = span.lo.0 as usize;
                    return diagnostics::report_error_span_and_bail(
                        file_path,
                        source_code,
                        start,
                        "missing semicolon: statements must end with ';'",
                        Some(
                            "oats requires semicolons like Rust/C++: add a trailing ';' to this statement.",
                        ),
                    );
                }
            }
            // Reject usage of the legacy `var` keyword: require `let` or `const`.
            if let ast::Stmt::Decl(ast::Decl::Var(vdecl)) = stmt
                && matches!(vdecl.kind, deno_ast::swc::ast::VarDeclKind::Var)
            {
                let start = vdecl.span.lo.0 as usize;
                return diagnostics::report_error_span_and_bail(
                    file_path,
                    source_code,
                    start,
                    "the `var` keyword is not allowed; use `let` or `const` instead",
                    Some(
                        "`var` is disallowed in this project; use `let` for mutable locals or `const` for immutable ones.",
                    ),
                );
            }
            // If it's a function decl, also inspect its body
            if let ast::Stmt::Decl(ast::Decl::Fn(fdecl)) = stmt
                && let Some(body) = &fdecl.function.body
            {
                for s in &body.stmts {
                    if stmt_requires_semicolon(s) {
                        let span = match s {
                            ast::Stmt::Expr(es) => es.span,
                            ast::Stmt::Decl(ast::Decl::Var(v)) => v.span,
                            ast::Stmt::Return(r) => r.span,
                            ast::Stmt::Break(b) => b.span,
                            ast::Stmt::Continue(c) => c.span,
                            ast::Stmt::Throw(t) => t.span,
                            ast::Stmt::Debugger(d) => d.span,
                            _ => continue,
                        };
                        if !has_trailing_semicolon(&span, source_code, &parsed) {
                            let start = span.lo.0 as usize;
                            return diagnostics::report_error_span_and_bail(
                                file_path,
                                source_code,
                                start,
                                "missing semicolon: statements must end with ';'",
                                Some(
                                    "oats requires semicolons like Rust/C++: add a trailing ';' to this statement.",
                                ),
                            );
                        }
                        // Reject `var` inside function bodies as well
                        if let ast::Stmt::Decl(ast::Decl::Var(vdecl)) = s
                            && matches!(vdecl.kind, deno_ast::swc::ast::VarDeclKind::Var)
                        {
                            let start = vdecl.span.lo.0 as usize;
                            return diagnostics::report_error_span_and_bail(
                                file_path,
                                source_code,
                                start,
                                "the `var` keyword is not allowed; use `let` or `const` instead",
                                Some(
                                    "`var` is disallowed in this project; use `let` for mutable locals or `const` for immutable ones.",
                                ),
                            );
                        }
                    }
                }
            }
        }
        if let deno_ast::ModuleItemRef::ModuleDecl(deno_ast::swc::ast::ModuleDecl::ExportDecl(
            decl,
        )) = item
        {
            // export declarations that are vars should also be terminated
            if let ast::Decl::Var(vdecl) = &decl.decl {
                // Reject var in export decls as well
                if matches!(vdecl.kind, deno_ast::swc::ast::VarDeclKind::Var) {
                    let start = vdecl.span.lo.0 as usize;
                    return diagnostics::report_error_span_and_bail(
                        file_path,
                        source_code,
                        start,
                        "the `var` keyword is not allowed; use `let` or `const` instead",
                        Some(
                            "`var` is disallowed in this project; use `let` for mutable locals or `const` for immutable ones.",
                        ),
                    );
                }
                if !has_trailing_semicolon(&vdecl.span, source_code, &parsed) {
                    let start = vdecl.span.lo.0 as usize;
                    return diagnostics::report_error_span_and_bail(
                        file_path,
                        source_code,
                        start,
                        "missing semicolon after export declaration",
                        Some(
                            "oats requires semicolons like Rust/C++: add a trailing ';' to this export declaration.",
                        ),
                    );
                }
            }
            if let ast::Decl::Fn(fdecl) = &decl.decl
                && let Some(body) = &fdecl.function.body
            {
                for s in &body.stmts {
                    if stmt_requires_semicolon(s) {
                        let span = match s {
                            ast::Stmt::Expr(es) => es.span,
                            ast::Stmt::Decl(ast::Decl::Var(v)) => v.span,
                            ast::Stmt::Return(r) => r.span,
                            ast::Stmt::Break(b) => b.span,
                            ast::Stmt::Continue(c) => c.span,
                            ast::Stmt::Throw(t) => t.span,
                            ast::Stmt::Debugger(d) => d.span,
                            _ => continue,
                        };
                        if !has_trailing_semicolon(&span, source_code, &parsed) {
                            let start = span.lo.0 as usize;
                            return diagnostics::report_error_span_and_bail(
                                file_path,
                                source_code,
                                start,
                                "missing semicolon: statements must end with ';'",
                                Some(
                                    "oats requires semicolons like Rust/C++: add a trailing ';' to this statement.",
                                ),
                            );
                        }
                    }
                }
            }
        }
    }
    Ok(ParsedModule {
        parsed,
        source: source_code.to_string(),
    })
}
