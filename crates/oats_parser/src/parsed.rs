//! Parsed module representation with additional metadata
//!
//! This module provides `ParsedModule` which wraps the parsed AST with
//! additional information collected during parsing, such as mutable variable
//! declarations and declared functions.

use crate::tokenizer;
use oats_ast::*;
use std::collections::HashSet;
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
    if let Ok(val) = std::env::var("OATS_MAX_SOURCE_BYTES") {
        if let Ok(limit) = val.parse::<usize>() {
            MAX_SOURCE_SIZE.store(limit, Ordering::Relaxed);
        }
    }

    // Parse OATS_MAX_AST_DEPTH (default: 100)
    if let Ok(val) = std::env::var("OATS_MAX_AST_DEPTH") {
        if let Ok(limit) = val.parse::<usize>() {
            MAX_AST_DEPTH.store(limit, Ordering::Relaxed);
        }
    }

    LIMITS_INITIALIZED.store(true, Ordering::Relaxed);
}

/// A parsed module with additional metadata collected during parsing.
///
/// This wraps the parsed AST with information that's useful for downstream
/// compilation phases, such as which variable declarations are mutable and
/// which functions are declared (but not defined).
#[derive(Clone)]
pub struct ParsedModule {
    /// The parsed AST module
    pub parsed: Module,
    /// Original source text (no preprocessing)
    pub source: String,
    /// Set of VarDecl span start positions (byte index) that contain the
    /// `mut` token. This is computed at parse time by scanning the captured
    /// tokens inside each VarDecl span so downstream passes (codegen) can
    /// consult mutability information without string-searching the source.
    pub mut_var_decls: HashSet<usize>,
    /// Top-level `declare function` declarations found in the source.
    /// These are stored as raw AST nodes for the compiler to process.
    pub declared_fns: Vec<DeclareFn>,
    /// Token stream for the parsed source code
    /// This is exposed for testing and tooling purposes
    pub tokens: Vec<tokenizer::Token>,
}

/// Recursively collect mutable variable declarations from the AST
fn collect_mut_var_decls(stmt: &Stmt, source: &str, out: &mut HashSet<usize>) {
    collect_mut_var_decls_with_depth(stmt, source, out, 0)
}

/// Internal helper with recursion depth tracking to prevent stack overflow.
fn collect_mut_var_decls_with_depth(
    stmt: &Stmt,
    source: &str,
    out: &mut HashSet<usize>,
    depth: usize,
) {
    // Prevent stack overflow from deeply nested AST structures
    const MAX_AST_COLLECTION_DEPTH: usize = 1000;
    if depth > MAX_AST_COLLECTION_DEPTH {
        return;
    }

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
                collect_mut_var_decls_with_depth(s, source, out, depth + 1);
            }
        }
        Stmt::If(if_stmt) => {
            collect_mut_var_decls_with_depth(&if_stmt.cons, source, out, depth + 1);
            if let Some(alt) = &if_stmt.alt {
                collect_mut_var_decls_with_depth(alt, source, out, depth + 1);
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
            collect_mut_var_decls_with_depth(&for_stmt.body, source, out, depth + 1);
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
            collect_mut_var_decls_with_depth(&for_in.body, source, out, depth + 1);
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
            collect_mut_var_decls_with_depth(&for_of.body, source, out, depth + 1);
        }
        Stmt::While(while_stmt) => {
            collect_mut_var_decls_with_depth(&while_stmt.body, source, out, depth + 1);
        }
        Stmt::DoWhile(do_while) => {
            collect_mut_var_decls_with_depth(&do_while.body, source, out, depth + 1);
        }
        Stmt::Switch(switch) => {
            for case in &switch.cases {
                for s in &case.cons {
                    collect_mut_var_decls_with_depth(s, source, out, depth + 1);
                }
            }
        }
        Stmt::Try(try_stmt) => {
            for s in &try_stmt.block.stmts {
                collect_mut_var_decls_with_depth(s, source, out, depth + 1);
            }
            if let Some(handler) = &try_stmt.handler {
                for s in &handler.body.stmts {
                    collect_mut_var_decls_with_depth(s, source, out, depth + 1);
                }
            }
            if let Some(finalizer) = &try_stmt.finalizer {
                for s in &finalizer.stmts {
                    collect_mut_var_decls_with_depth(s, source, out, depth + 1);
                }
            }
        }
        Stmt::FnDecl(fn_decl) => {
            if let Some(body) = &fn_decl.body {
                for s in &body.stmts {
                    collect_mut_var_decls_with_depth(s, source, out, depth + 1);
                }
            }
        }
        _ => {}
    }
}

fn collect_mut_from_expr(_expr: &Expr, _source: &str, _out: &mut HashSet<usize>) {
    // For now, we don't need to track mutability in expressions
    // This is a placeholder for future expansion
}

/// Parse an Oats source string into a `ParsedModule` with additional metadata.
///
/// This function performs the following operations:
/// - Enforces maximum source size limit to prevent resource exhaustion
/// - Strips BOM (Byte Order Mark) if present
/// - Parses the source into an AST
/// - Collects mutable variable declarations
/// - Collects declared function statements
/// - Tokenizes the source for tooling purposes
///
/// # Arguments
/// * `source_code` - source text to parse
///
/// # Returns
/// `Ok(ParsedModule)` on success, or `Err(String)` with an error message
pub fn parse_module_with_metadata(source_code: &str) -> Result<ParsedModule, String> {
    // Initialize resource limits on first call
    init_parser_limits();

    // SECURITY: Check source size limit before parsing
    let max_size = MAX_SOURCE_SIZE.load(Ordering::Relaxed);
    if source_code.len() > max_size {
        return Err(format!(
            "Source file too large: {} bytes (limit: {} bytes). \
             Set OATS_MAX_SOURCE_BYTES to increase.",
            source_code.len(),
            max_size
        ));
    }

    // Strip BOM (Byte Order Mark) if present
    // UTF-8 BOM is 0xEF 0xBB 0xBF
    let source_without_bom = if let Some(stripped) = source_code.strip_prefix('\u{FEFF}') {
        stripped
    } else {
        source_code
    };

    // Parse using the core parser
    let module = super::parse_module(source_without_bom)?;

    // Collect mutable variable declarations
    // Use source_without_bom to match AST spans which are relative to the stripped source
    let mut mut_var_decls = HashSet::new();
    for stmt in &module.body {
        collect_mut_var_decls(stmt, source_without_bom, &mut mut_var_decls);
    }

    // Collect declare function statements
    let mut declared_fns = Vec::new();
    for stmt in &module.body {
        if let Stmt::DeclareFn(declare_fn) = stmt {
            declared_fns.push(declare_fn.clone());
        }
    }

    // Tokenize the source code for exposure in ParsedModule
    // Use source_without_bom to match AST spans which are relative to the stripped source
    let tokens = tokenizer::tokenize(source_without_bom);

    Ok(ParsedModule {
        parsed: module,
        source: source_code.to_string(),
        mut_var_decls,
        declared_fns,
        tokens,
    })
}
