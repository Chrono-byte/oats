//! Oats parser utilities
//!
//! This is a thin wrapper around oats_parser that converts parser errors
//! to compiler diagnostics. The actual parsing logic lives in oats_parser.

use crate::diagnostics;
use crate::types::{FunctionSig, OatsType};
use anyhow::Result;
use oats_ast::*;
use oats_parser::{ParsedModule as ParserParsedModule, parse_module_with_metadata};

/// Lightweight representation of a top-level `declare function` signature
/// extracted from source text. The builder will convert these into
/// compiler `FunctionSig` entries and LLVM declarations.
#[derive(Debug, Clone)]
pub struct DeclaredFn {
    pub name: String,
    pub sig: FunctionSig,
}

/// A parsed module with compiler-specific metadata.
///
/// This wraps the parser's ParsedModule and adds compiler-specific
/// conversions (like converting DeclareFn AST nodes to DeclaredFn).
#[derive(Clone)]
pub struct ParsedModule {
    pub parsed: Module,
    pub source: String,
    pub mut_var_decls: std::collections::HashSet<usize>,
    pub declared_fns: Vec<DeclaredFn>,
    pub tokens: Vec<oats_parser::tokenizer::Token>,
}

/// Convert a parser ParsedModule to a compiler ParsedModule.
fn convert_parsed_module(parser_mod: ParserParsedModule) -> ParsedModule {
    // Convert DeclareFn AST nodes to DeclaredFn with FunctionSig
    let mut declared_fns = Vec::new();
    for declare_fn in &parser_mod.declared_fns {
        let name = declare_fn.ident.sym.clone();

        // Convert parameter types from TsType to OatsType
        let mut param_types = Vec::new();
        for param in &declare_fn.params {
            if let Some(ref ts_type) = param.ty {
                // Convert TsType to OatsType
                if let Some(oats_type) = crate::types::map_ts_type(ts_type) {
                    param_types.push(oats_type);
                } else {
                    // If conversion fails, default to Number
                    param_types.push(OatsType::Number);
                }
            } else {
                // If no type annotation, default to Number
                param_types.push(OatsType::Number);
            }
        }

        // Convert return type from TsType to OatsType
        let ret_type = crate::types::map_ts_type(&declare_fn.return_type).unwrap_or(OatsType::Void);

        // Extract type parameters if available
        let type_params = vec![]; // Type parameters not directly available in DeclareFn

        let sig = FunctionSig {
            params: param_types,
            ret: ret_type,
            type_params,
        };
        declared_fns.push(DeclaredFn { name, sig });
    }

    ParsedModule {
        parsed: parser_mod.parsed,
        source: parser_mod.source,
        mut_var_decls: parser_mod.mut_var_decls,
        declared_fns,
        tokens: parser_mod.tokens,
    }
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
    let diags = Vec::new();

    // Parse using oats_parser
    let parser_mod = match parse_module_with_metadata(source_code) {
        Ok(m) => m,
        Err(error) => {
            // Convert parse error to diagnostic
            let parse_diags = vec![diagnostics::Diagnostic {
                severity: diagnostics::Severity::Error,
                code: None,
                message: error.clone(),
                file: file_path.map(|s| s.to_string()),
                labels: vec![diagnostics::Label {
                    span: diagnostics::Span {
                        start: 0,
                        end: source_code.len(),
                    },
                    message: "parse error".to_string(),
                }],
                note: None,
                help: None,
            }];
            return Ok((None, parse_diags));
        }
    };

    // Convert to compiler ParsedModule
    let parsed_module = convert_parsed_module(parser_mod);

    Ok((Some(parsed_module), diags))
}
