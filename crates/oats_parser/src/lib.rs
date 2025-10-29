//! Oats Parser
//!
//! This crate implements a parser for the Oats language using chumsky.
//! It takes a string input and produces an `oats_ast::Module`.

use chumsky::prelude::*;
use oats_ast::*;

/// Parse a string into an Oats AST Module.
pub fn parse_module(input: &str) -> Result<Module, Vec<Simple<char>>> {
    let parser = module_parser();
    parser.parse(input)
}

/// Parser for the top-level module.
fn module_parser() -> impl Parser<char, Module, Error = Simple<char>> {
    stmt_parser()
        .repeated()
        .collect::<Vec<_>>()
        .map(|body| Module {
            body,
            span: 0..0, // TODO: proper span
        })
        .then_ignore(end())
}

/// Parser for statements.
fn stmt_parser() -> impl Parser<char, Stmt, Error = Simple<char>> {
    choice((
        declare_fn_parser().map(Stmt::DeclareFn),
        fn_decl_parser().map(Stmt::FnDecl),
    ))
}

/// Parser for declare function.
fn declare_fn_parser() -> impl Parser<char, DeclareFn, Error = Simple<char>> {
    text::keyword("declare")
        .padded()
        .ignore_then(text::keyword("function"))
        .padded()
        .ignore_then(ident_parser())
        .then(
            param_parser()
                .separated_by(just(',').padded())
                .collect::<Vec<_>>()
                .delimited_by(just('(').padded(), just(')').padded()),
        )
        .then(just(':').padded().ignore_then(ts_type_parser()))
        .then_ignore(just(';').padded())
        .map(|((ident, params), return_type)| DeclareFn {
            ident,
            params,
            return_type,
            span: 0..0, // TODO
        })
}

/// Parser for function declarations.
fn fn_decl_parser() -> impl Parser<char, FnDecl, Error = Simple<char>> {
    text::keyword("function")
        .padded()
        .ignore_then(ident_parser())
        .then(
            param_parser()
                .separated_by(just(',').padded())
                .collect::<Vec<_>>()
                .delimited_by(just('(').padded(), just(')').padded()),
        )
        .then(just(':').padded().ignore_then(ts_type_parser()).or_not())
        .then(
            just('{')
                .padded()
                .ignore_then(just('}').padded())
                .map(|_| None::<BlockStmt>),
        )
        .map(|(((ident, params), return_type), body)| FnDecl {
            ident,
            params,
            body,
            return_type,
            span: 0..0, // TODO: spans
        })
}

/// Parser for identifiers.
fn ident_parser() -> impl Parser<char, Ident, Error = Simple<char>> {
    text::ident().map(|sym| Ident {
        sym,
        span: 0..0, // TODO
    })
}

/// Parser for function parameters.
fn param_parser() -> impl Parser<char, Param, Error = Simple<char>> {
    ident_parser()
        .then_ignore(just(':').padded())
        .then(ts_type_parser())
        .map(|(ident, ty)| Param {
            pat: Pat::Ident(ident),
            ty: Some(ty),
            span: 0..0, // TODO
        })
}

/// Parser for TypeScript types (minimal).
fn ts_type_parser() -> impl Parser<char, TsType, Error = Simple<char>> {
    choice((
        text::keyword("number").map(|_| TsType::TsKeywordType(TsKeywordType::TsNumberKeyword)),
        text::keyword("string").map(|_| TsType::TsKeywordType(TsKeywordType::TsStringKeyword)),
        text::keyword("boolean").map(|_| TsType::TsKeywordType(TsKeywordType::TsBooleanKeyword)),
        text::keyword("void").map(|_| TsType::TsKeywordType(TsKeywordType::TsVoidKeyword)),
    ))
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
    fn test_declare_function() {
        let input = "declare function print_str(s: string): void;";
        let result = parse_module(input);
        assert!(result.is_ok());
        let module = result.unwrap();
        assert_eq!(module.body.len(), 1);
        if let Stmt::DeclareFn(declare_fn) = &module.body[0] {
            assert_eq!(declare_fn.ident.sym, "print_str");
            assert_eq!(declare_fn.params.len(), 1);
            let Pat::Ident(ident) = &declare_fn.params[0].pat;
            assert_eq!(ident.sym, "s");
            assert!(matches!(
                declare_fn.params[0].ty,
                Some(TsType::TsKeywordType(TsKeywordType::TsStringKeyword))
            ));
            assert!(matches!(
                declare_fn.return_type,
                TsType::TsKeywordType(TsKeywordType::TsVoidKeyword)
            ));
        } else {
            panic!("Expected DeclareFn");
        }
    }
}
