//! Function-related parsers

use super::common;
use chumsky::prelude::*;
use oats_ast::*;

/// Parser for function declarations.
///
/// Supports:
/// - `async function name() {}`
/// - `function* name() {}` (generator)
/// - `async function* name() {}` (async generator)
/// - `function name(): type {}`
pub fn fn_decl_parser<'a>(
    stmt: impl Parser<'a, &'a str, Stmt> + Clone + 'a,
) -> impl Parser<'a, &'a str, FnDecl> + 'a {
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
        .labelled("function declaration")
}
