//! Oats Parser
//!
//! This crate implements a parser for the Oats language using chumsky.
//! It takes a string input and produces an `oats_ast::Module`.
//!
//! The parser is organized following Locality of Behavior principles:
//! - Related parsing logic is grouped together in modules
//! - Common utilities are reused across modules
//! - Each module focuses on a specific aspect of the language

mod class;
mod common;
mod expr;
mod function;
mod stmt;
mod types;

use chumsky::prelude::*;
use oats_ast::*;

/// Parse a string into an Oats AST Module.
pub fn parse_module(input: &str) -> Result<Module, Vec<Simple<char>>> {
    let parser = module_parser();
    parser.parse(input)
}

/// Parser for the top-level module.
///
/// A module consists of zero or more statements.
fn module_parser() -> impl Parser<char, Module, Error = Simple<char>> {
    recursive(|stmt| stmt::stmt_parser(stmt))
        .repeated()
        .collect::<Vec<_>>()
        .then_ignore(end())
        .map_with_span(|body, span| Module {
            body,
            span: span.into(),
        })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_simple_function() {
        let input = "function main(): void {}";
        let result = parse_module(input);
        if let Err(errors) = &result {
            for e in errors {
                println!("Parse error: {:?}", e);
            }
        }
        assert!(result.is_ok());
        let module = result.unwrap();
        assert_eq!(module.body.len(), 1);
        if let Stmt::FnDecl(fn_decl) = &module.body[0] {
            assert_eq!(fn_decl.ident.sym, "main");
            assert!(fn_decl.params.is_empty());
            assert!(matches!(
                fn_decl.return_type,
                Some(TsType::TsKeywordType(TsKeywordType::TsVoidKeyword))
            ));
        } else {
            panic!("Expected FnDecl");
        }
    }

    #[test]
    fn test_function_with_body() {
        let input = "function add(a: number, b: number): number { return a + b; }";
        let result = parse_module(input);
        if let Err(errors) = &result {
            for e in errors {
                println!("Parse error: {:?}", e);
            }
        }
        assert!(result.is_ok());
        let module = result.unwrap();
        assert_eq!(module.body.len(), 1);
        if let Stmt::FnDecl(fn_decl) = &module.body[0] {
            assert_eq!(fn_decl.ident.sym, "add");
            assert_eq!(fn_decl.params.len(), 2);
            assert!(fn_decl.body.is_some());
        } else {
            panic!("Expected FnDecl");
        }
    }

    #[test]
    fn test_var_decl() {
        let input = "let x: number = 5;";
        let result = parse_module(input);
        if let Err(errors) = &result {
            for e in errors {
                println!("Parse error: {:?}", e);
            }
        }
        assert!(result.is_ok());
        let module = result.unwrap();
        assert_eq!(module.body.len(), 1);
        if let Stmt::VarDecl(var_decl) = &module.body[0] {
            assert!(matches!(var_decl.kind, VarDeclKind::Let));
            assert_eq!(var_decl.decls.len(), 1);
        } else {
            panic!("Expected VarDecl");
        }
    }

    #[test]
    fn test_let_mut() {
        let input = "let mut x: number = 5;";
        let result = parse_module(input);
        if let Err(errors) = &result {
            for e in errors {
                println!("Parse error: {:?}", e);
            }
        }
        assert!(result.is_ok());
    }

    #[test]
    fn test_binary_expr() {
        let input = "let x: number = 3 + 5;";
        let result = parse_module(input);
        assert!(result.is_ok());
    }

    #[test]
    fn test_if_stmt() {
        let input = "if (x > 0) { return x; }";
        let result = parse_module(input);
        assert!(result.is_ok());
    }

    #[test]
    fn test_for_loop() {
        let input = "for (let mut i: number = 0; i < 10; i = i + 1) { console.log(i); }";
        let result = parse_module(input);
        if let Err(errors) = &result {
            for e in errors {
                println!("Parse error: {:?}", e);
            }
        }
        assert!(result.is_ok());
    }
}
