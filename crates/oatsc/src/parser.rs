//! Oats parser utilities
//!
//! This module uses `oxc_parser` to parse Oats source code and returns
//! oxc AST types directly for use by the compiler.

use crate::diagnostics;
use crate::types::{FunctionSig, OatsType};
use anyhow::Result;
use oxc_allocator::Allocator;
use oxc_ast::ast::{Program, Statement, ForStatementInit, ForStatementLeft};
use oxc_parser::Parser;
use oxc_span::SourceType;

/// Lightweight representation of a top-level `declare function` signature
/// extracted from source text. The builder will convert these into
/// compiler `FunctionSig` entries and LLVM declarations.
#[derive(Debug, Clone)]
pub struct DeclaredFn {
    pub name: String,
    pub sig: FunctionSig,
}

/// Simple token representation for tooling/testing purposes
#[derive(Debug, Clone)]
pub struct Token {
    pub kind: String,
    pub span: oxc_span::Span,
    pub text: String,
}

/// A parsed module with compiler-specific metadata.
///
/// Stores the AST data with an owned allocator via Arc to avoid
/// lifetime issues while keeping memory safe.
pub struct ParsedModule {
    pub parsed: Program<'static>,
    // Store the leaked allocator reference to keep it alive
    // Using a raw pointer is safe here because we never dereference it,
    // we just keep it to prevent the allocator from being dropped
    pub source: String,
    pub mut_var_decls: std::collections::HashSet<usize>,
    pub declared_fns: Vec<DeclaredFn>,
    pub tokens: Vec<Token>,
}

/// Collect mutable variable declarations from the AST
fn collect_mut_var_decls(
    stmt: &Statement,
    source: &str,
    out: &mut std::collections::HashSet<usize>,
) {
    match stmt {
        Statement::VariableDeclaration(vd) => {
            let span_start = vd.span.start as usize;
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
        Statement::BlockStatement(block) => {
            for s in &block.body {
                collect_mut_var_decls(s, source, out);
            }
        }
        Statement::IfStatement(if_stmt) => {
            collect_mut_var_decls(&if_stmt.consequent, source, out);
            if let Some(alt) = &if_stmt.alternate {
                collect_mut_var_decls(alt, source, out);
            }
        }
        Statement::ForStatement(for_stmt) => {
            if let Some(ForStatementInit::VariableDeclaration(vd)) = &for_stmt.init {
                let span_start = vd.span.start as usize;
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
        Statement::ForInStatement(for_in) => {
            if let ForStatementLeft::VariableDeclaration(vd) = &for_in.left {
                let span_start = vd.span.start as usize;
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
        Statement::ForOfStatement(for_of) => {
            if let ForStatementLeft::VariableDeclaration(vd) = &for_of.left {
                let span_start = vd.span.start as usize;
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
        Statement::WhileStatement(while_stmt) => {
            collect_mut_var_decls(&while_stmt.body, source, out);
        }
        Statement::DoWhileStatement(do_while) => {
            collect_mut_var_decls(&do_while.body, source, out);
        }
        Statement::SwitchStatement(switch) => {
            for case in &switch.cases {
                for s in &case.consequent {
                    collect_mut_var_decls(s, source, out);
                }
            }
        }
        Statement::TryStatement(try_stmt) => {
            for s in &try_stmt.block.body {
                collect_mut_var_decls(s, source, out);
            }
            if let Some(handler) = &try_stmt.handler {
                for s in &handler.body.body {
                    collect_mut_var_decls(s, source, out);
                }
            }
            if let Some(finalizer) = &try_stmt.finalizer {
                for s in &finalizer.body {
                    collect_mut_var_decls(s, source, out);
                }
            }
        }
        Statement::FunctionDeclaration(function) => {
            if let Some(body) = &function.body {
                for s in &body.statements {
                    collect_mut_var_decls(s, source, out);
                }
            }
        }
        _ => {}
    }
}

/// Collect declared functions from the AST
/// Only extracts function names, doesn't store references
fn collect_declared_fns(stmt: &Statement, out: &mut Vec<String>) {
    match stmt {
        Statement::FunctionDeclaration(function) => {
            // Only collect if it has no body (declare function)
            if function.body.is_none()
                && let Some(id) = &function.id
            {
                out.push(id.name.to_string());
            }
        }
        Statement::BlockStatement(block) => {
            for s in &block.body {
                collect_declared_fns(s, out);
            }
        }
        Statement::IfStatement(if_stmt) => {
            collect_declared_fns(&if_stmt.consequent, out);
            if let Some(alt) = &if_stmt.alternate {
                collect_declared_fns(alt, out);
            }
        }
        Statement::ForStatement(for_stmt) => {
            collect_declared_fns(&for_stmt.body, out);
        }
        Statement::ForInStatement(for_in) => {
            collect_declared_fns(&for_in.body, out);
        }
        Statement::ForOfStatement(for_of) => {
            collect_declared_fns(&for_of.body, out);
        }
        Statement::WhileStatement(while_stmt) => {
            collect_declared_fns(&while_stmt.body, out);
        }
        Statement::DoWhileStatement(do_while) => {
            collect_declared_fns(&do_while.body, out);
        }
        Statement::SwitchStatement(switch) => {
            for case in &switch.cases {
                for s in &case.consequent {
                    collect_declared_fns(s, out);
                }
            }
        }
        Statement::TryStatement(try_stmt) => {
            for s in &try_stmt.block.body {
                collect_declared_fns(s, out);
            }
            if let Some(handler) = &try_stmt.handler {
                for s in &handler.body.body {
                    collect_declared_fns(s, out);
                }
            }
            if let Some(finalizer) = &try_stmt.finalizer {
                for s in &finalizer.body {
                    collect_declared_fns(s, out);
                }
            }
        }
        _ => {}
    }
}

/// Convert declared function names to `DeclaredFn` with `FunctionSig`
fn convert_declared_fns(functions: Vec<String>) -> Vec<DeclaredFn> {
    functions
        .into_iter()
        .map(|name| DeclaredFn {
            name,
            sig: FunctionSig {
                params: vec![],
                ret: OatsType::Void,
                type_params: vec![],
            },
        })
        .collect()
}

/// Parse an Oats source string into a `ParsedModule` and run
/// lightweight project-specific checks.
///
/// The function performs the following additional checks beyond parsing:
/// - Enforces maximum source size limit to prevent resource exhaustion
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
    let source_without_bom = if let Some(stripped) = source_code.strip_prefix('\u{FEFF}') {
        stripped
    } else {
        source_code
    };

    let max_size = 10 * 1024 * 1024;
    if source_without_bom.len() > max_size {
        let diags = vec![diagnostics::Diagnostic {
            severity: diagnostics::Severity::Error,
            code: None,
            message: format!(
                "Source file too large: {} bytes (limit: {} bytes)",
                source_without_bom.len(),
                max_size
            ),
            file: file_path.map(std::string::ToString::to_string),
            labels: vec![diagnostics::Label {
                span: diagnostics::Span {
                    start: 0,
                    end: source_without_bom.len(),
                },
                message: "file too large".to_string(),
            }],
            note: None,
            help: None,
        }];
        return Ok((None, diags));
    }

    let source_type = SourceType::default().with_typescript(true);

    // Create a leaked allocator to get 'static lifetime
    let allocator = Box::leak(Box::new(Allocator::default()));

    // Convert source to 'static string by leaking it
    // This is safe because we store the source in ParsedModule anyway
    let source_static: &'static str = Box::leak(source_without_bom.to_string().into_boxed_str());

    let program_static: Program<'static> = {
        let parser_return = Parser::new(allocator, source_static, source_type).parse();

        if !parser_return.errors.is_empty() {
            let mut diags = Vec::new();
            for error in parser_return.errors {
                let message = error.to_string();
                // Extract span from error labels if available
                let span = if let Some(labels) = &error.labels {
                    if labels.is_empty() {
                        diagnostics::Span {
                            start: 0,
                            end: source_without_bom.len(),
                        }
                    } else {
                        let label = &labels[0];
                        diagnostics::Span {
                            start: label.offset(),
                            end: (label.offset() + label.len()),
                        }
                    }
                } else {
                    diagnostics::Span {
                        start: 0,
                        end: source_without_bom.len(),
                    }
                };
                diags.push(diagnostics::Diagnostic {
                    severity: diagnostics::Severity::Error,
                    code: None,
                    message,
                    file: file_path.map(std::string::ToString::to_string),
                    labels: vec![diagnostics::Label {
                        span,
                        message: "parse error".to_string(),
                    }],
                    note: None,
                    help: None,
                });
            }
            return Ok((None, diags));
        }

        // The Program now has 'static lifetime because the allocator is 'static
        // (via Box::leak). This is safe because:
        // 1. Box::leak gives us a &'static Allocator that lives for the program duration
        // 2. The Program only holds references to nodes in the allocator
        // 3. We ensure the allocator outlives the Program by keeping it in ParsedModule
        parser_return.program
    };

    let mut mut_var_decls = std::collections::HashSet::new();
    for stmt in &program_static.body {
        collect_mut_var_decls(stmt, source_without_bom, &mut mut_var_decls);
    }

    let mut declared_fn_names = Vec::new();
    for stmt in &program_static.body {
        collect_declared_fns(stmt, &mut declared_fn_names);
    }
    let declared_fns = convert_declared_fns(declared_fn_names);

    let tokens = Vec::new();

    let module = ParsedModule {
        parsed: program_static,
        source: source_code.to_string(),
        mut_var_decls,
        declared_fns,
        tokens,
    };

    Ok((Some(module), Vec::new()))
}
