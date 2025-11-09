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
                    msg.push_str(&format!("  Error {}: {:?}\n", i + 1, err));
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
