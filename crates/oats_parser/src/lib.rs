//! Oats Parser
//!
//! This crate implements a parser for the Oats language using chumsky.
//! It takes a string input and produces an `oats_ast::Module`.
//!
//! The parser is organized following Locality of Behavior principles:
//! - Related parsing logic is grouped together in modules
//! - Common utilities are reused across modules
//! - Each module focuses on a specific aspect of the language
//!
//! Modern chumsky best practices (0.11.2):
//! - Use `extra` for context and error handling
//! - Avoid unnecessary cloning with better recursion patterns
//! - Leverage combinators for cleaner code
//! - Use `labelled()` for better error messages

mod class;
mod common;
mod expr;
mod function;
mod process;
mod stmt;
pub mod tokenizer;
mod types;

use chumsky::prelude::*;
use oats_ast::*;

/// Parse a string into an Oats AST Module.
///
/// Returns a Module on success or a vector of parse errors on failure.
pub fn parse_module(input: &str) -> Result<Module, String> {
    module_parser()
        .parse(input)
        .into_result()
        .map_err(|_errors| "Parse error".to_string())
}

/// Parser for the top-level module.
///
/// A module consists of zero or more statements followed by EOF.
/// This is the entry point for parsing Oats programs.
fn module_parser<'a>() -> impl Parser<'a, &'a str, Module> + 'a {
    // Use recursive to create mutually recursive expr/stmt parsers
    // Both expr and stmt parsers are boxed to allow cloning within the recursive functions
    recursive(|stmt_param| {
        let expr_parser = recursive(|expr_param| {
            let stmt_inner = stmt::stmt_parser_inner(expr_param.clone(), stmt_param.clone()).boxed();
            expr::expr_parser_inner(expr_param, stmt_inner).boxed()
        })
        .boxed();
        stmt::stmt_parser_inner(expr_parser, stmt_param).boxed()
    })
    .padded()
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
    fn test_parse_simple_function() {
        let input = "function main(): void {}";
        let result = parse_module(input);
        if let Err(e) = &result {
            println!("Parse error: {:?}", e);
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
        if let Err(e) = &result {
            println!("Parse error: {:?}", e);
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
        if let Err(e) = &result {
            println!("Parse error: {:?}", e);
        }
        assert!(result.is_ok());
        let module = result.unwrap();
        assert_eq!(module.body.len(), 1);
        if let Stmt::VarDecl(var_decl) = &module.body[0] {
            assert!(matches!(var_decl.kind, VarDeclKind::Let { mutable: false }));
            assert_eq!(var_decl.decls.len(), 1);
        } else {
            panic!("Expected VarDecl");
        }
    }

    #[test]
    fn test_let_mut() {
        let input = "let mut x: number = 5;";
        let result = parse_module(input);
        if let Err(e) = &result {
            println!("Parse error: {:?}", e);
        }
        assert!(result.is_ok());
    }

    #[test]
    fn test_binary_expr() {
        let input = "let x: number = 3 + 5;";
        let result = parse_module(input);
        if let Err(e) = &result {
            println!("Parse error: {:?}", e);
        }
        assert!(result.is_ok());
    }

    #[test]
    fn test_if_stmt() {
        let input = "if (x > 0) { return x; }";
        let result = parse_module(input);
        if let Err(e) = &result {
            println!("Parse error: {:?}", e);
        }
        assert!(result.is_ok());
    }

    #[test]
    fn test_for_loop() {
        let input = "for (let mut i: number = 0; i < 10; i = i + 1) { console.log(i); }";
        let result = parse_module(input);
        if let Err(e) = &result {
            println!("Parse error: {:?}", e);
        }
        assert!(result.is_ok());
    }

    #[test]
    fn test_var_decl_let_mut() {
        let input = "let mut z = 100;";
        let result = parse_module(input);
        assert!(result.is_ok());
        let module = result.unwrap();
        assert_eq!(module.body.len(), 1);
        if let Stmt::VarDecl(var_decl) = &module.body[0] {
            assert!(matches!(var_decl.kind, VarDeclKind::Let { mutable: true }));
            assert_eq!(var_decl.decls.len(), 1);
            if let Pat::Ident(ident) = &var_decl.decls[0].name {
                assert_eq!(ident.sym, "z");
            } else {
                panic!("Expected identifier pattern");
            }
            assert!(matches!(
                var_decl.decls[0].init,
                Some(Expr::Lit(Lit::I64(100)))
            ));
        } else {
            panic!("Expected VarDecl");
        }
    }

    #[test]
    fn test_function_with_body_and_var() {
        let input = "function test(): void { let x = 5; }";
        let result = parse_module(input);
        assert!(result.is_ok());
        let module = result.unwrap();
        assert_eq!(module.body.len(), 1);
        if let Stmt::FnDecl(fn_decl) = &module.body[0] {
            assert_eq!(fn_decl.ident.sym, "test");
            assert!(fn_decl.params.is_empty());
            assert!(matches!(
                fn_decl.return_type,
                Some(TsType::TsKeywordType(TsKeywordType::TsVoidKeyword))
            ));
            // Check that the body contains a statement
            assert!(fn_decl.body.is_some());
            let body = fn_decl.body.as_ref().unwrap();
            assert_eq!(body.stmts.len(), 1);
            if let Stmt::VarDecl(var_decl) = &body.stmts[0] {
                assert!(matches!(var_decl.kind, VarDeclKind::Let { mutable: false }));
                assert_eq!(var_decl.decls.len(), 1);
                if let Pat::Ident(ident) = &var_decl.decls[0].name {
                    assert_eq!(ident.sym, "x");
                } else {
                    panic!("Expected identifier pattern");
                }
                assert!(matches!(
                    var_decl.decls[0].init,
                    Some(Expr::Lit(Lit::I64(5)))
                ));
            } else {
                panic!("Expected VarDecl in function body");
            }
        } else {
            panic!("Expected FnDecl");
        }
    }
}
