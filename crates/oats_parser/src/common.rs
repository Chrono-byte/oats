//! Common parsing utilities
//!
//! This module contains reusable parser components used throughout the parser.
//! Following Locality of Behavior, these are grouped together as they represent
//! the fundamental building blocks of the language.

use chumsky::prelude::*;
use oats_ast::*;

/// Parser for identifiers.
pub fn ident_parser() -> impl Parser<char, Ident, Error = Simple<char>> {
    text::ident().map_with_span(|sym, span: std::ops::Range<usize>| Ident { sym, span })
}

/// Parser for patterns.
///
/// Supports:
/// - Identifiers: `x`
/// - Array patterns: `[a, b, c]`, `[a, ...rest]`
/// - Object patterns: `{a, b}`, `{a: x, b: y}`, `{...rest}`
/// - Rest patterns: `...rest` (in arrays/objects)
pub fn pat_parser() -> impl Parser<char, Pat, Error = Simple<char>> {
    recursive(|pat| {
        let ident = ident_parser().map(Pat::Ident);

        // Rest pattern: ...pat
        let rest = just("...")
            .padded()
            .ignore_then(pat.clone())
            .map_with_span(|arg, span| {
                Pat::Rest(RestPat {
                    arg: Box::new(arg),
                    span,
                })
            });

        // Array pattern: [pat1, pat2, ...rest?]
        let array = pat
            .clone()
            .separated_by(just(',').padded())
            .collect::<Vec<_>>()
            .delimited_by(just('[').padded(), just(']').padded())
            .map_with_span(|elems, span| {
                Pat::Array(ArrayPat {
                    elems: elems.into_iter().map(Some).collect(),
                    span,
                })
            });

        // Object pattern property: key: pat or key (shorthand)
        let obj_prop = choice((
            // Rest in object: ...ident
            just("...")
                .padded()
                .ignore_then(ident_parser())
                .map_with_span(|arg, span| ObjectPatProp::Rest { arg, span }),
            // Key-value: key: pat
            choice((
                just('"')
                    .ignore_then(filter(|c| *c != '"').repeated().collect::<String>())
                    .then_ignore(just('"'))
                    .map(PropName::Str),
                ident_parser().map(PropName::Ident),
            ))
            .then(just(':').padded().ignore_then(pat.clone()))
            .map_with_span(|(key, value), span| ObjectPatProp::KeyValue { key, value, span }),
            // Shorthand: ident (same as ident: ident)
            ident_parser()
                .then(just(':').padded().ignore_then(pat.clone()).or_not())
                .map_with_span(|(key_ident, opt_value), span| {
                    if let Some(value) = opt_value {
                        ObjectPatProp::KeyValue {
                            key: PropName::Ident(key_ident.clone()),
                            value,
                            span,
                        }
                    } else {
                        // Shorthand: just use the ident as both key and value pattern
                        ObjectPatProp::KeyValue {
                            key: PropName::Ident(key_ident.clone()),
                            value: Pat::Ident(key_ident),
                            span,
                        }
                    }
                }),
        ));

        // Object pattern: {prop1, prop2, ...rest?}
        let object = obj_prop
            .separated_by(just(',').padded())
            .collect::<Vec<_>>()
            .delimited_by(just('{').padded(), just('}').padded())
            .map_with_span(|props, span| Pat::Object(ObjectPat { props, span }));

        choice((rest, array, object, ident))
    })
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
        .map_with_span(|(pat, ty), span| Param { pat, ty, span })
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
            .map_with_span(|n, _span| Expr::Lit(Lit::F64(n))),
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
        .map_with_span(|stmts, span| BlockStmt { stmts, span })
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
