//! Function-related parsers
//!
//! This module groups all function parsing logic together for Locality of Behavior.
//! Includes function declarations, function expressions, and arrow functions.

use super::common;
use super::types;
use chumsky::prelude::*;
use oats_ast::*;

/// Parser for declare function statements.
///
/// Pattern: `declare function name(params): returnType;`
pub fn declare_fn_parser() -> impl Parser<char, DeclareFn, Error = Simple<char>> {
    text::keyword("declare")
        .padded()
        .ignore_then(text::keyword("function"))
        .padded()
        .ignore_then(common::ident_parser())
        .then(common::param_list_parser())
        .then(just(':').padded().ignore_then(types::ts_type_parser()))
        .then_ignore(just(';').padded())
        .map_with_span(|((ident, params), return_type), span| DeclareFn {
            ident,
            params,
            return_type,
            span,
        })
}

/// Parser for function declarations.
///
/// Pattern: `async? function name(params): returnType? { body }`
pub fn fn_decl_parser(
    stmt: impl Parser<char, Stmt, Error = Simple<char>> + Clone,
) -> impl Parser<char, FnDecl, Error = Simple<char>> {
    text::keyword("async")
        .padded()
        .or_not()
        .then(text::keyword("function").padded())
        .then(common::ident_parser())
        .then(common::param_list_parser())
        .then(common::optional_type_annotation())
        .then(common::optional_block_parser(stmt))
        .map_with_span(
            |(((((is_async, _), ident), params), return_type), body), span| FnDecl {
                ident,
                params,
                body,
                return_type,
                is_async: is_async.is_some(),
                span,
            },
        )
}

/// Parser for function expressions.
///
/// Pattern: `async? function name?(params): returnType? { body }`
pub fn fn_expr_parser(
    stmt: impl Parser<char, Stmt, Error = Simple<char>> + Clone,
) -> impl Parser<char, Expr, Error = Simple<char>> {
    text::keyword("async")
        .padded()
        .or_not()
        .then(text::keyword("function").padded())
        .then(common::ident_parser().or_not())
        .then(common::param_list_parser())
        .then(common::optional_type_annotation())
        .then(common::optional_block_parser(stmt))
        .map_with_span(
            |(((((is_async, _), ident), params), return_type), body), span| {
                let span_range: std::ops::Range<usize> = span;
                Expr::Fn(FnExpr {
                    ident,
                    function: Function {
                        is_async: is_async.is_some(),
                        params,
                        body,
                        return_type,
                        span: span_range.clone(),
                    },
                    span: span_range,
                })
            },
        )
}

/// Parser for arrow function expressions.
///
/// Pattern: `(params) => body` or `param => body`
pub fn arrow_expr_parser(
    expr: impl Parser<char, Expr, Error = Simple<char>>,
    stmt: impl Parser<char, Stmt, Error = Simple<char>> + Clone,
) -> impl Parser<char, Expr, Error = Simple<char>> {
    // Arrow function parameters - can be single param or parenthesized list
    let params = common::pat_parser()
        .separated_by(just(',').padded())
        .collect::<Vec<_>>()
        .delimited_by(just('(').padded(), just(')').padded())
        .or(common::pat_parser().map(|p| vec![p]));

    params
        .then(common::optional_type_annotation())
        .then(just("=>").padded())
        .then(choice((
            // Block body
            common::block_parser(stmt).map(ArrowBody::Block),
            // Expression body
            expr.map(|e| ArrowBody::Expr(Box::new(e))),
        )))
        .map_with_span(|(((params, return_type), _), body), span| {
            Expr::Arrow(ArrowExpr {
                params,
                body,
                return_type,
                span,
            })
        })
}
