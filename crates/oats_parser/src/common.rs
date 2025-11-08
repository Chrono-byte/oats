//! Common parsing utilities
//!
//! This module contains reusable parser components used throughout the parser.
//! Following Locality of Behavior, these are grouped together as they represent
//! the fundamental building blocks of the language.

use chumsky::prelude::*;
use oats_ast::*;

/// Parser for identifiers.
pub fn ident_parser() -> impl Parser<char, Ident, Error = Simple<char>> {
    text::ident().map_with_span(|sym, span: std::ops::Range<usize>| Ident {
        sym,
        span: span.into(),
    })
}

/// Parser for patterns.
pub fn pat_parser() -> impl Parser<char, Pat, Error = Simple<char>> {
    ident_parser().map(Pat::Ident)
}

/// Parser for function parameters.
///
/// Parameters follow the pattern: `ident: type?`
pub fn param_parser() -> impl Parser<char, Param, Error = Simple<char>> {
    pat_parser()
        .then(
            just(':')
                .padded()
                .ignore_then(super::types::ts_type_parser())
                .or_not(),
        )
        .map_with_span(|(pat, ty), span| Param {
            pat,
            ty,
            span: span.into(),
        })
}

/// Parser for a comma-separated list of parameters in parentheses.
///
/// Reusable pattern: `(param1, param2, ...)`
pub fn param_list_parser() -> impl Parser<char, Vec<Param>, Error = Simple<char>> {
    param_parser()
        .separated_by(just(',').padded())
        .collect::<Vec<_>>()
        .delimited_by(just('(').padded(), just(')').padded())
}

/// Parser for literals (strings, numbers, booleans, null).
pub fn literal_parser() -> impl Parser<char, Expr, Error = Simple<char>> {
    choice((
        // String literals with escape sequence support
        just('"')
            .ignore_then(
                filter(|c| *c != '"' && *c != '\\')
                    .or(just('\\').ignore_then(choice((
                        just('"').to('"'),
                        just('\\').to('\\'),
                        just('/').to('/'),
                        just('n').to('\n'),
                        just('r').to('\r'),
                        just('t').to('\t'),
                        just('b').to('\x08'),
                        just('f').to('\x0C'),
                        // Unicode escape sequences: \uXXXX
                        just('u')
                            .ignore_then(
                                filter(|c: &char| c.is_ascii_hexdigit())
                                    .repeated()
                                    .exactly(4)
                                    .collect::<String>(),
                            )
                            .try_map(|hex, span| {
                                u32::from_str_radix(&hex, 16)
                                    .ok()
                                    .and_then(char::from_u32)
                                    .ok_or(Simple::custom(span, "Invalid unicode escape sequence"))
                            }),
                    ))))
                    .repeated()
                    .collect::<String>(),
            )
            .then_ignore(just('"'))
            .map_with_span(|s, _span| Expr::Lit(Lit::Str(s))),
        // Number literals
        text::int(10)
            .then(just('.').then(text::digits(10)).or_not())
            .map(|(int_part, frac_part)| {
                let num_str = if let Some((_, frac)) = frac_part {
                    format!("{}.{}", int_part, frac)
                } else {
                    int_part
                };
                num_str.parse::<f64>().unwrap_or(0.0)
            })
            .map_with_span(|n, _span| Expr::Lit(Lit::Num(n))),
        // Boolean literals
        text::keyword("true")
            .padded()
            .map_with_span(|_, _span| Expr::Lit(Lit::Bool(true))),
        text::keyword("false")
            .padded()
            .map_with_span(|_, _span| Expr::Lit(Lit::Bool(false))),
        // Null literal
        text::keyword("null")
            .padded()
            .map_with_span(|_, _span| Expr::Lit(Lit::Null)),
    ))
}

/// Parser for an optional type annotation followed by a colon.
///
/// Pattern: `: type?`
pub fn optional_type_annotation() -> impl Parser<char, Option<TsType>, Error = Simple<char>> {
    just(':')
        .padded()
        .ignore_then(super::types::ts_type_parser())
        .or_not()
}

/// Parser for a block statement (statements in braces).
///
/// Reusable pattern for parsing `{ stmt1; stmt2; ... }`
pub fn block_parser(
    stmt: impl Parser<char, Stmt, Error = Simple<char>> + Clone,
) -> impl Parser<char, BlockStmt, Error = Simple<char>> {
    stmt.repeated()
        .collect::<Vec<_>>()
        .delimited_by(just('{').padded(), just('}').padded())
        .map_with_span(|stmts, span| BlockStmt {
            stmts,
            span: span.into(),
        })
}

/// Parser for an optional block statement (empty braces return None).
pub fn optional_block_parser(
    stmt: impl Parser<char, Stmt, Error = Simple<char>> + Clone,
) -> impl Parser<char, Option<BlockStmt>, Error = Simple<char>> {
    block_parser(stmt.clone()).map(Some).or(just('{')
        .padded()
        .ignore_then(just('}').padded())
        .map(|_| None))
}
