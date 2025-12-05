//! Oats parser utilities
//!
//! This module uses oxc_parser to parse Oats source code and returns
//! oxc AST types directly for use by the compiler.

use crate::diagnostics;
use crate::types::{FunctionSig, OatsType};
use anyhow::Result;
use oxc_allocator::Allocator;
use oxc_ast::ast::*;
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
/// This wraps the oxc AST and adds compiler-specific
/// conversions (like converting declare function AST nodes to DeclaredFn).
///
/// Note: The allocator must outlive the ParsedModule since the AST references it.
pub struct ParsedModule<'a> {
    pub parsed: Program<'a>,
    pub allocator: Allocator,
    pub source: String,
    pub mut_var_decls: std::collections::HashSet<usize>,
    pub declared_fns: Vec<DeclaredFn>,
    pub tokens: Vec<Token>,
}

/// Collect mutable variable declarations from the AST
fn collect_mut_var_decls<'a>(stmt: &Statement<'a>, source: &str, out: &mut std::collections::HashSet<usize>) {
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
            if let ForInStatementLeft::VariableDeclaration(vd) = &for_in.left {
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
            if let ForOfStatementLeft::VariableDeclaration(vd) = &for_of.left {
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
                for s in &body.body {
                    collect_mut_var_decls(s, source, out);
                }
            }
        }
        _ => {}
    }
}

/// Collect declared functions from the AST
/// In oxc, we need to look for function declarations with specific patterns
/// For now, we'll look for top-level function declarations that might be "declare function"
fn collect_declared_fns<'a>(stmt: &Statement<'a>, out: &mut Vec<Function<'a>>) {
    match stmt {
        Statement::FunctionDeclaration(function) => {
            // Check if this is a "declare function" - in oxc this might be a function with no body
            // or we might need to check the source text
            out.push(function.clone());
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

/// Convert Function AST nodes to DeclaredFn with FunctionSig
/// This is a simplified version - we'll need to check if the function is actually a "declare function"
fn convert_declared_fns<'a>(functions: Vec<Function<'a>>, source: &str) -> Vec<DeclaredFn> {
    functions.into_iter().filter_map(|function| {
        // Check if this is a "declare function" by looking at the source
        // For now, we'll assume all top-level functions without bodies are declare functions
        // This is a simplification and may need refinement
        if function.body.is_some() {
            return None; // Skip functions with bodies
        }

        let name = function.id.name().to_string();

        // Convert parameter types from TSType to OatsType
        let mut param_types = Vec::new();
        for param in &function.params.items {
            if let Some(ref ts_type) = param.type_annotation {
                // Convert TSType to OatsType - need to convert from oxc TSType to oats_ast TsType first
                // For now, we'll create a simplified mapping
                // TODO: Implement proper conversion from oxc TSType to OatsType
                param_types.push(OatsType::Number);
            } else {
                // If no type annotation, default to Number
                param_types.push(OatsType::Number);
            }
        }

        // Convert return type from TSType to OatsType
        let ret_type = function.return_type.as_ref()
            .map(|_| OatsType::Void) // TODO: Implement proper conversion
            .unwrap_or(OatsType::Void);

        // Extract type parameters if available
        let type_params = vec![]; // Type parameters not directly available in Function

        let sig = FunctionSig {
            params: param_types,
            ret: ret_type,
            type_params,
        };
        Some(DeclaredFn { name, sig })
    }).collect()
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
pub fn parse_oats_module<'a>(
    source_code: &'a str,
    file_path: Option<&str>,
) -> Result<(Option<ParsedModule<'a>>, Vec<diagnostics::Diagnostic>)> {
    parse_oats_module_with_options(source_code, file_path, false)
}

pub fn parse_oats_module_with_options<'a>(
    source_code: &'a str,
    file_path: Option<&str>,
    _enforce_semicolons: bool,
) -> Result<(Option<ParsedModule<'a>>, Vec<diagnostics::Diagnostic>)> {
    // Strip BOM if present
    let source_without_bom = if let Some(stripped) = source_code.strip_prefix('\u{FEFF}') {
        stripped
    } else {
        source_code
    };

    // Check source size limit (10 MB default)
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
            file: file_path.map(|s| s.to_string()),
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

    // Create allocator and parser
    let allocator = Allocator::default();
    let source_type = SourceType::default().with_typescript(true);
    let parser = Parser::new(&allocator, source_without_bom, source_type);
    
    // Parse the source
    let parser_return = parser.parse();
    
    // Check for parsing errors
    if !parser_return.errors.is_empty() {
        let mut diags = Vec::new();
        for error in parser_return.errors {
            let message = error.to_string();
            let span = error.span();
            diags.push(diagnostics::Diagnostic {
                severity: diagnostics::Severity::Error,
                code: None,
                message,
                file: file_path.map(|s| s.to_string()),
                labels: vec![diagnostics::Label {
                    span: diagnostics::Span {
                        start: span.start as usize,
                        end: span.end as usize,
                    },
                    message: "parse error".to_string(),
                }],
                note: None,
                help: None,
            });
        }
        return Ok((None, diags));
    }

    // Use oxc AST directly
    let program = parser_return.program;

    // Collect metadata
    let mut mut_var_decls = std::collections::HashSet::new();
    for stmt in &program.body {
        collect_mut_var_decls(stmt, source_without_bom, &mut mut_var_decls);
    }

    let mut declared_fns_ast = Vec::new();
    for stmt in &program.body {
        collect_declared_fns(stmt, &mut declared_fns_ast);
    }
    let declared_fns = convert_declared_fns(declared_fns_ast, source_without_bom);

    // Create simple token representation (for testing/tooling)
    // This is a simplified version - you may want to enhance this
    let tokens = Vec::new(); // Tokens not critical for compilation

    let parsed_module = ParsedModule {
        parsed: program,
        allocator,
        source: source_code.to_string(),
        mut_var_decls,
        declared_fns,
        tokens,
    };

    Ok((Some(parsed_module), Vec::new()))
}


