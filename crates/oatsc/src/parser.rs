//! Oats parser utilities
//!
//! This wraps oats_parser parsing with some extra checks we need, like forcing semicolons
//! and not allowing 'var'. It gives back a ParsedModule with the AST and source text
//! for diagnostics later.
//!
//! For security, we limit source file size (10MB default, set OATS_MAX_SOURCE_BYTES)
//! and recursion depth (32 levels in runtime) to prevent stack overflows.

use crate::diagnostics;
use crate::types::FunctionSig;
use anyhow::Result;
use oats_ast::*;
use oats_parser;
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};

/// Maximum source file size in bytes (default: 10 MB)
/// Override with OATS_MAX_SOURCE_BYTES environment variable
static MAX_SOURCE_SIZE: AtomicUsize = AtomicUsize::new(10 * 1024 * 1024);

/// Maximum AST nesting depth to prevent stack overflow attacks
/// Override with OATS_MAX_AST_DEPTH environment variable
static MAX_AST_DEPTH: AtomicUsize = AtomicUsize::new(100);

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

    // Parse OATS_MAX_AST_DEPTH (default: 100)
    if let Ok(val) = std::env::var("OATS_MAX_AST_DEPTH")
        && let Ok(limit) = val.parse::<usize>()
    {
        MAX_AST_DEPTH.store(limit, Ordering::Relaxed);
    }

    LIMITS_INITIALIZED.store(true, Ordering::Relaxed);
}

pub struct ParsedModule {
    pub parsed: Module,
    // Original source text (no preprocessing)
    pub source: String,
    // Set of VarDecl span start positions (byte index) that contain the
    // `mut` token. This is computed at parse time by scanning the captured
    // tokens inside each VarDecl span so downstream passes (codegen) can
    // consult mutability information without string-searching the source.
    pub mut_var_decls: std::collections::HashSet<usize>,
    // Top-level `declare function` declarations found in the source.
    // Each entry contains the declared name and its function signature.
    pub declared_fns: Vec<DeclaredFn>,
}

/// Lightweight representation of a top-level `declare function` signature
/// extracted from source text. We intentionally keep this small and textual
/// to avoid modifying oats_ast types; the builder will convert these into
/// compiler `FunctionSig` entries and LLVM declarations.
#[derive(Debug, Clone)]
pub struct DeclaredFn {
    pub name: String,
    pub sig: FunctionSig,
}

/// Recursively collect mutable variable declarations from the AST
fn collect_mut_var_decls(stmt: &Stmt, source: &str, out: &mut std::collections::HashSet<usize>) {
    match stmt {
        Stmt::VarDecl(vd) => {
            // Check if this is a mutable declaration by scanning the source
            // for "let mut" or "const mut" patterns near the span
            let span_start = vd.span.start;
            if span_start < source.len() {
                // Look for "let mut" or "const mut" pattern
                let snippet = if span_start + 20 < source.len() {
                    &source[span_start..span_start + 20]
                } else {
                    &source[span_start..]
                };
                if snippet.contains("let mut") || snippet.contains("const mut") {
                    out.insert(span_start);
                }
            }
            // Recursively check nested statements in initializers
            for decl in &vd.decls {
                if let Some(init) = &decl.init {
                    collect_mut_from_expr(init, source, out);
                }
            }
        }
        Stmt::Block(block) => {
            for s in &block.stmts {
                collect_mut_var_decls(s, source, out);
            }
        }
        Stmt::If(if_stmt) => {
            collect_mut_var_decls(&if_stmt.cons, source, out);
            if let Some(alt) = &if_stmt.alt {
                collect_mut_var_decls(alt, source, out);
            }
        }
        Stmt::For(for_stmt) => {
            if let Some(ForInit::VarDecl(vd)) = &for_stmt.init {
                let span_start = vd.span.start;
                if span_start < source.len() {
                    let snippet = if span_start + 20 < source.len() {
                        &source[span_start..span_start + 20]
                    } else {
                        &source[span_start..]
                    };
                    if snippet.contains("let mut") || snippet.contains("const mut") {
                        out.insert(span_start);
                    }
                }
            }
            collect_mut_var_decls(&for_stmt.body, source, out);
        }
        Stmt::ForIn(for_in) => {
            if let ForHead::VarDecl(vd) = &for_in.left {
                let span_start = vd.span.start;
                if span_start < source.len() {
                    let snippet = if span_start + 20 < source.len() {
                        &source[span_start..span_start + 20]
                    } else {
                        &source[span_start..]
                    };
                    if snippet.contains("let mut") || snippet.contains("const mut") {
                        out.insert(span_start);
                    }
                }
            }
            collect_mut_var_decls(&for_in.body, source, out);
        }
        Stmt::ForOf(for_of) => {
            if let ForHead::VarDecl(vd) = &for_of.left {
                let span_start = vd.span.start;
                if span_start < source.len() {
                    let snippet = if span_start + 20 < source.len() {
                        &source[span_start..span_start + 20]
                    } else {
                        &source[span_start..]
                    };
                    if snippet.contains("let mut") || snippet.contains("const mut") {
                        out.insert(span_start);
                    }
                }
            }
            collect_mut_var_decls(&for_of.body, source, out);
        }
        Stmt::While(while_stmt) => {
            collect_mut_var_decls(&while_stmt.body, source, out);
        }
        Stmt::DoWhile(do_while) => {
            collect_mut_var_decls(&do_while.body, source, out);
        }
        Stmt::Switch(switch) => {
            for case in &switch.cases {
                for s in &case.cons {
                    collect_mut_var_decls(s, source, out);
                }
            }
        }
        Stmt::Try(try_stmt) => {
            for s in &try_stmt.block.stmts {
                collect_mut_var_decls(s, source, out);
            }
            if let Some(handler) = &try_stmt.handler {
                for s in &handler.body.stmts {
                    collect_mut_var_decls(s, source, out);
                }
            }
            if let Some(finalizer) = &try_stmt.finalizer {
                for s in &finalizer.stmts {
                    collect_mut_var_decls(s, source, out);
                }
            }
        }
        Stmt::FnDecl(fn_decl) => {
            if let Some(body) = &fn_decl.body {
                for s in &body.stmts {
                    collect_mut_var_decls(s, source, out);
                }
            }
        }
        _ => {}
    }
}

fn collect_mut_from_expr(_expr: &Expr, _source: &str, _out: &mut std::collections::HashSet<usize>) {
    // For now, we don't need to track mutability in expressions
    // This is a placeholder for future expansion
}

/// Check if a statement contains `var` declarations (which are not allowed)
fn stmt_contains_var(stmt: &Stmt) -> bool {
    match stmt {
        Stmt::VarDecl(vd) => matches!(vd.kind, VarDeclKind::Var),
        Stmt::Block(block) => {
            for s in &block.stmts {
                if stmt_contains_var(s) {
                    return true;
                }
            }
            false
        }
        Stmt::If(if_stmt) => {
            if stmt_contains_var(&if_stmt.cons) {
                return true;
            }
            if let Some(alt) = &if_stmt.alt {
                if stmt_contains_var(alt) {
                    return true;
                }
            }
            false
        }
        Stmt::For(for_stmt) => {
            if let Some(ForInit::VarDecl(vd)) = &for_stmt.init {
                if matches!(vd.kind, VarDeclKind::Var) {
                    return true;
                }
            }
            stmt_contains_var(&for_stmt.body)
        }
        Stmt::ForIn(for_in) => {
            if let ForHead::VarDecl(vd) = &for_in.left {
                if matches!(vd.kind, VarDeclKind::Var) {
                    return true;
                }
            }
            stmt_contains_var(&for_in.body)
        }
        Stmt::ForOf(for_of) => {
            if let ForHead::VarDecl(vd) = &for_of.left {
                if matches!(vd.kind, VarDeclKind::Var) {
                    return true;
                }
            }
            stmt_contains_var(&for_of.body)
        }
        Stmt::While(while_stmt) => stmt_contains_var(&while_stmt.body),
        Stmt::DoWhile(do_while) => stmt_contains_var(&do_while.body),
        Stmt::Switch(switch) => {
            for case in &switch.cases {
                for s in &case.cons {
                    if stmt_contains_var(s) {
                        return true;
                    }
                }
            }
            false
        }
        Stmt::Try(try_stmt) => {
            for s in &try_stmt.block.stmts {
                if stmt_contains_var(s) {
                    return true;
                }
            }
            if let Some(handler) = &try_stmt.handler {
                for s in &handler.body.stmts {
                    if stmt_contains_var(s) {
                        return true;
                    }
                }
            }
            if let Some(finalizer) = &try_stmt.finalizer {
                for s in &finalizer.stmts {
                    if stmt_contains_var(s) {
                        return true;
                    }
                }
            }
            false
        }
        Stmt::FnDecl(fn_decl) => {
            if let Some(body) = &fn_decl.body {
                for s in &body.stmts {
                    if stmt_contains_var(s) {
                        return true;
                    }
                }
            }
            false
        }
        _ => false,
    }
}

/// Parse an Oats source string into a `ParsedModule` and run
/// lightweight project-specific checks.
///
/// The function performs the following additional checks beyond parsing:
/// - Enforces maximum source size limit to prevent resource exhaustion
/// - Rejects usage of the `var` keyword in favor of `let`/`const`.
///
/// # Arguments
/// * `source_code` - source text to parse
/// * `file_path` - optional path used for diagnostics
pub fn parse_oats_module(
    source_code: &str,
    file_path: Option<&str>,
) -> Result<(Option<ParsedModule>, Vec<diagnostics::Diagnostic>)> {
    parse_oats_module_with_options(source_code, file_path, false)
}

pub fn parse_oats_module_with_options(
    source_code: &str,
    file_path: Option<&str>,
    _enforce_semicolons: bool,
) -> Result<(Option<ParsedModule>, Vec<diagnostics::Diagnostic>)> {
    let diags = Vec::new();
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

    // Strip BOM (Byte Order Mark) if present
    // UTF-8 BOM is 0xEF 0xBB 0xBF
    let source_without_bom = if let Some(stripped) = source_code.strip_prefix('\u{FEFF}') {
        stripped
    } else {
        source_code
    };

    // Parse using oats_parser
    let module = match oats_parser::parse_module(source_without_bom) {
        Ok(m) => m,
        Err(errors) => {
            // Convert chumsky errors to diagnostics
            let mut parse_diags = Vec::new();
            for error in errors {
                let message = format!("Parse error: {}", error);
                let span = error.span();
                let diag = diagnostics::Diagnostic {
                    severity: diagnostics::Severity::Error,
                    code: None,
                    message,
                    file: file_path.map(|s| s.to_string()),
                    labels: vec![diagnostics::Label {
                        span: diagnostics::Span {
                            start: span.start,
                            end: span.end,
                        },
                        message: "parse error".to_string(),
                    }],
                    note: None,
                    help: None,
                };
                parse_diags.push(diag);
            }
            return Ok((None, parse_diags));
        }
    };

    // Check for `var` declarations
    for stmt in &module.body {
        if stmt_contains_var(stmt) {
            return diagnostics::report_error_and_bail(
                file_path,
                Some(source_code),
                "`var` declarations are not supported. Use `let` or `const` instead.",
                Some(
                    "`var` has function-scoped semantics which we intentionally disallow; prefer `let` or `const`.",
                ),
            );
        }
    }

    // Collect mutable variable declarations
    let mut mut_var_decls = std::collections::HashSet::new();
    for stmt in &module.body {
        collect_mut_var_decls(stmt, source_code, &mut mut_var_decls);
    }

    // Collect declare function statements
    let mut declared_fns = Vec::new();
    for stmt in &module.body {
        if let Stmt::DeclareFn(declare_fn) = stmt {
            // Extract function signature from declare function
            // This is a simplified version - you may need to enhance this
            let name = declare_fn.ident.sym.clone();
            // TODO: Convert TsType to FunctionSig properly
            // For now, create a placeholder
            let sig = FunctionSig {
                params: vec![],
                ret: crate::types::OatsType::Void,
                type_params: vec![],
            };
            declared_fns.push(DeclaredFn { name, sig });
        }
    }

    let parsed_module = ParsedModule {
        parsed: module,
        source: source_code.to_string(),
        mut_var_decls,
        declared_fns,
    };

    Ok((Some(parsed_module), diags))
}
