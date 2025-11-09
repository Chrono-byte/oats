//! Common parsing utilities
//!
//! This module contains reusable parser components used throughout the parser.
//! Following Locality of Behavior, these are grouped together as they represent
//! the fundamental building blocks of the language.

use chumsky::prelude::*;
use oats_ast::*;

/// Parser for identifiers.
pub fn ident_parser<'a>() -> impl Parser<'a, &'a str, Ident> {
    // Custom identifier parser for &str input
    // Identifiers start with a letter, underscore, or dollar sign, followed by letters, digits, underscores, or dollar signs
    any()
        .filter(|c: &char| c.is_alphabetic() || *c == '_' || *c == '$')
        .then(
            any()
                .filter(|c: &char| c.is_alphanumeric() || *c == '_' || *c == '$')
                .repeated()
                .collect::<String>(),
        )
        .map_with(|(first, rest), extra| {
            let sym = format!("{}{}", first, rest);
            Ident { sym, span: <std::ops::Range<usize>>::from(extra.span()) }
        })
}

/// Parser for patterns.
///
/// Supports:
/// - Identifiers: `x`
/// - Array patterns: `[a, b, c]`, `[a, ...rest]`
/// - Object patterns: `{a, b}`, `{a: x, b: y}`, `{...rest}`
/// - Rest patterns: `...rest` (in arrays/objects)
pub fn pat_parser<'a>() -> impl Parser<'a, &'a str, Pat> {
    recursive(|pat| {
        // Inline ident parsing to avoid Clone issues
        let ident = any()
            .filter(|c: &char| c.is_alphabetic() || *c == '_' || *c == '$')
            .then(
                any()
                    .filter(|c: &char| c.is_alphanumeric() || *c == '_' || *c == '$')
                    .repeated()
                    .collect::<String>(),
            )
            .map_with(|(first, rest), extra| {
                let sym = format!("{}{}", first, rest);
                Pat::Ident(Ident { sym, span: <std::ops::Range<usize>>::from(extra.span()) })
            });

        // Rest pattern: ...pat
        let rest = just("...")
            .padded()
            .ignore_then(pat.clone())
            .map_with(|arg, extra| {
                Pat::Rest(RestPat {
                    arg: Box::new(arg),
                    span: <std::ops::Range<usize>>::from(extra.span()),
                })
            });

        // Array pattern: [pat1, pat2, ...rest?]
        let array = pat
            .clone()
            .separated_by(just(",").padded())
            .collect::<Vec<_>>()
            .delimited_by(just("[").padded(), just("]").padded())
            .map_with(|elems, extra| {
                Pat::Array(ArrayPat {
                    elems: elems.into_iter().map(Some).collect(),
                    span: <std::ops::Range<usize>>::from(extra.span()),
                })
            });

        // Object pattern property: key: pat or key (shorthand)
        let obj_prop = choice((
            // Rest in object: ...ident
            just("...")
                .padded()
                .ignore_then(
                    any()
                        .filter(|c: &char| c.is_alphabetic() || *c == '_' || *c == '$')
                        .then(
                            any()
                                .filter(|c: &char| c.is_alphanumeric() || *c == '_' || *c == '$')
                                .repeated()
                                .collect::<String>(),
                        )
                        .map_with(|(first, rest), extra| {
                            let sym = format!("{}{}", first, rest);
                            Ident { sym, span: <std::ops::Range<usize>>::from(extra.span()) }
                        })
                )
                .map_with(|arg: Ident, extra| {
                    ObjectPatProp::Rest {
                        arg,
                        span: <std::ops::Range<usize>>::from(extra.span())
                    }
                }),
            // Key-value: key: pat
            choice((
                just("\"")
                    .ignore_then(any().filter(|c: &char| *c != '"').repeated().collect::<String>())
                    .then_ignore(just("\""))
                    .map(PropName::Str),
                any()
                    .filter(|c: &char| c.is_alphabetic() || *c == '_' || *c == '$')
                    .then(
                        any()
                            .filter(|c: &char| c.is_alphanumeric() || *c == '_' || *c == '$')
                            .repeated()
                            .collect::<String>(),
                    )
                    .map_with(|(first, rest), extra| {
                        let sym = format!("{}{}", first, rest);
                        PropName::Ident(Ident { sym, span: <std::ops::Range<usize>>::from(extra.span()) })
                    }),
            ))
            .then(just(":").padded().ignore_then(pat.clone()))
            .map_with(|(key, value), extra| ObjectPatProp::KeyValue { key, value, span: <std::ops::Range<usize>>::from(extra.span()) }),
            // Shorthand: ident (same as ident: ident)
            any()
                .filter(|c: &char| c.is_alphabetic() || *c == '_' || *c == '$')
                .then(
                    any()
                        .filter(|c: &char| c.is_alphanumeric() || *c == '_' || *c == '$')
                        .repeated()
                        .collect::<String>(),
                )
                .map_with(|(first, rest), extra| {
                    let sym = format!("{}{}", first, rest);
                    Ident { sym, span: <std::ops::Range<usize>>::from(extra.span()) }
                })
                .then(just(":").padded().ignore_then(pat.clone()).or_not())
                .map_with(|(key_ident, opt_value), extra| {
                    if let Some(value) = opt_value {
                        ObjectPatProp::KeyValue {
                            key: PropName::Ident(key_ident.clone()),
                            value,
                            span: <std::ops::Range<usize>>::from(extra.span()),
                        }
                    } else {
                        // Shorthand: just use the ident as both key and value pattern
                        ObjectPatProp::KeyValue {
                            key: PropName::Ident(key_ident.clone()),
                            value: Pat::Ident(key_ident),
                            span: <std::ops::Range<usize>>::from(extra.span()),
                        }
                    }
                }),
        ));

        // Object pattern: {prop1, prop2, ...rest?}
        let object = obj_prop
            .separated_by(just(",").padded())
            .collect::<Vec<_>>()
            .delimited_by(just("{").padded(), just("}").padded())
            .map_with(|props, extra| Pat::Object(ObjectPat { props, span: <std::ops::Range<usize>>::from(extra.span()) }));

        choice((rest, array, object, ident))
    })
}

/// Parser for function parameters.
///
/// Parameters follow the pattern: `ident: type?`
pub fn param_parser<'a>() -> impl Parser<'a, &'a str, Param> {
    pat_parser()
        .then(
            just(":")
                .padded()
                .ignore_then(super::types::ts_type_parser())
                .or_not(),
        )
        .map_with(|(pat, ty), extra| Param { pat, ty, span: extra.span().into() })
}

/// Parser for a comma-separated list of parameters in parentheses.
///
/// Reusable pattern: `(param1, param2, ...)`
pub fn param_list_parser<'a>() -> impl Parser<'a, &'a str, Vec<Param>> {
    param_parser()
        .separated_by(just(",").padded())
        .collect::<Vec<_>>()
        .delimited_by(just("(").padded(), just(")").padded())
}

/// Parser for literals (strings, numbers, booleans, null).
pub fn literal_parser<'a>() -> impl Parser<'a, &'a str, Expr> {
    choice((
        // String literals with escape sequence support
        just("\"")
            .ignore_then(
                any().filter(|c: &char| *c != '"' && *c != '\\')
                    .or(just('\\').ignore_then(choice((
                        just("\"").to('"'),
                        just('\\').to('\\'),
                        just("/").to('/'),
                        just("n").to('\n'),
                        just("r").to('\r'),
                        just("t").to('\t'),
                        just("b").to('\x08'),
                        just("f").to('\x0C'),
                        // Unicode escape sequences: \uXXXX
                        just("u")
                            .ignore_then(
                                any().filter(|c: &char| c.is_ascii_hexdigit())
                                    .repeated()
                                    .exactly(4)
                                    .collect::<String>(),
                            )
                            .map(|hex| {
                                u32::from_str_radix(&hex, 16)
                                    .ok()
                                    .and_then(char::from_u32)
                                    .unwrap_or('\u{FFFD}') // Replacement character on invalid
                            }),
                    ))))
                    .repeated()
                    .collect::<String>(),
            )
            .then_ignore(just("\""))
            .map_with(|s, _extra| Expr::Lit(Lit::Str(s))),
        // Number literals - parse as single string then convert
        any()
            .filter(|c: &char| c.is_ascii_digit())
            .repeated()
            .at_least(1)
            .collect::<String>()
            .then(
                just(".")
                    .then(any().filter(|c: &char| c.is_ascii_digit()).repeated().at_least(1).collect::<String>())
                    .map(|(_dot, frac)| frac)
                    .or_not()
            )
            .map(|(int_str, frac_opt)| {
                let num_str = match frac_opt {
                    Some(frac) => format!("{}.{}", int_str, frac),
                    None => int_str,
                };
                num_str.parse::<f64>().unwrap_or(0.0)
            })
            .map_with(|n, _extra| Expr::Lit(Lit::F64(n))),
        // Boolean literals
        text::keyword("true")
            .padded()
            .map_with(|_, _extra| Expr::Lit(Lit::Bool(true))),
        text::keyword("false")
            .padded()
            .map_with(|_, _extra| Expr::Lit(Lit::Bool(false))),
        // Null literal
        text::keyword("null")
            .padded()
            .map_with(|_, _extra| Expr::Lit(Lit::Null)),
    ))
}

/// Parser for an optional type annotation followed by a colon.
///
/// Pattern: `: type?`
pub fn optional_type_annotation<'a>() -> impl Parser<'a, &'a str, Option<TsType>> {
    just(":")
        .padded()
        .ignore_then(super::types::ts_type_parser())
        .or_not()
}

/// Parser for a block statement (statements in braces).
///
/// Reusable pattern for parsing `{ stmt1; stmt2; ... }`
pub fn block_parser<'a>(
    stmt: impl Parser<'a, &'a str, Stmt> + Clone,
) -> impl Parser<'a, &'a str, BlockStmt> {
    stmt.repeated()
        .collect::<Vec<_>>()
        .delimited_by(just("{").padded(), just("}").padded())
        .map_with(|stmts, extra| BlockStmt { stmts, span: extra.span().into() })
}

/// Parser for an optional block statement (empty braces return None).
pub fn optional_block_parser<'a>(
    stmt: impl Parser<'a, &'a str, Stmt> + Clone,
) -> impl Parser<'a, &'a str, Option<BlockStmt>> {
    block_parser(stmt.clone()).map(Some).or(just("{")
        .padded()
        .ignore_then(just("}").padded())
        .map(|_| None))
}
