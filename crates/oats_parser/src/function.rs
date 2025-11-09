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
pub fn declare_fn_parser<'a>() -> impl Parser<'a, &'a str, DeclareFn> {
    text::keyword("declare")
        .padded()
        .ignore_then(text::keyword("function"))
        .padded()
        .ignore_then(common::ident_parser())
        .then(common::param_list_parser())
        .then(just(":").padded().ignore_then(types::ts_type_parser()))
        .then_ignore(just(";").padded())
        .map_with(|((ident, params), return_type), extra| DeclareFn {
            ident,
            params,
            return_type,
            span: extra.span().into(),
        })
}

/// Parser for function declarations.
///
/// Pattern: `async? function*? name(params): returnType? { body }`
pub fn fn_decl_parser<'a>(
    stmt: impl Parser<'a, &'a str, Stmt> + Clone,
) -> impl Parser<'a, &'a str, FnDecl> {
    text::keyword("async")
        .padded()
        .or_not()
        .then(text::keyword("function").padded())
        .then(just("*").padded().or_not())
        .then(common::ident_parser())
        .then(common::param_list_parser())
        .then(common::optional_type_annotation())
        .then(common::optional_block_parser(stmt))
        .map_with(
            |((((((is_async, _), is_generator), ident), params), return_type), body), extra| {
                FnDecl {
                    ident,
                    params,
                    body,
                    return_type,
                    is_async: is_async.is_some(),
                    is_generator: is_generator.is_some(),
                    span: extra.span().into(),
                }
            },
        )
}

/// Parser for function expressions.
///
/// Pattern: `async? function*? name?(params): returnType? { body }`
pub fn fn_expr_parser<'a>(
    stmt: impl Parser<'a, &'a str, Stmt> + Clone,
) -> impl Parser<'a, &'a str, Expr> {
    text::keyword("async")
        .padded()
        .or_not()
        .then(text::keyword("function").padded())
        .then(just("*").padded().or_not())
        .then(common::ident_parser().or_not())
        .then(common::param_list_parser())
        .then(common::optional_type_annotation())
        .then(common::optional_block_parser(stmt))
        .map_with(
            |((((((is_async, _), is_generator), ident), params), return_type), body), extra| {
                let span_range: std::ops::Range<usize> = extra.span().into();
                Expr::Fn(FnExpr {
                    ident,
                    function: Function {
                        is_async: is_async.is_some(),
                        is_generator: is_generator.is_some(),
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
pub fn arrow_expr_parser<'a>(
    expr: impl Parser<'a, &'a str, Expr>,
    stmt: impl Parser<'a, &'a str, Stmt> + Clone,
) -> impl Parser<'a, &'a str, Expr> {
    // Arrow function parameters - can be single param or parenthesized list
    let params = common::pat_parser()
        .separated_by(just(",").padded())
        .collect::<Vec<_>>()
        .delimited_by(just("(").padded(), just(")").padded())
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
        .map_with(|(((params, return_type), _), body), extra| {
            Expr::Arrow(ArrowExpr {
                params,
                body,
                return_type,
                span: extra.span().into(),
            })
        })
}
