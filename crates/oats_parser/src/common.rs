//! Common parsing utilities

use chumsky::prelude::*;
use oats_ast::*;

/// Parser for identifiers.
///
/// Identifiers start with a letter, underscore, or dollar sign,
/// followed by any number of letters, digits, underscores, or dollar signs.
pub fn ident_parser<'a>() -> impl Parser<'a, &'a str, Ident> + 'a {
    any()
        .filter(|c: &char| c.is_alphabetic() || *c == '_' || *c == '$')
        .then(
            any()
                .filter(|c: &char| c.is_alphanumeric() || *c == '_' || *c == '$')
                .repeated()
                .collect::<String>(),
        )
        .map_with(|(first, rest), extra| {
            let sym: String = format!("{}{}", first, rest);
            Ident {
                sym,
                span: <std::ops::Range<usize>>::from(extra.span()),
            }
        })
        .labelled("identifier")
}

/// Parser for patterns (currently just identifiers).
pub fn pat_parser<'a>() -> impl Parser<'a, &'a str, Pat> + 'a {
    ident_parser().map(Pat::Ident).labelled("pattern")
}

/// Parser for parameter lists: `(param1, param2, ...)`
pub fn param_list_parser<'a>() -> impl Parser<'a, &'a str, Vec<Param>> + 'a {
    param_parser()
        .separated_by(just(",").padded())
        .allow_trailing()
        .collect::<Vec<_>>()
        .delimited_by(just("(").padded(), just(")").padded())
        .labelled("parameter list")
}

/// Parser for a single parameter: `ident: type` or just `ident`
fn param_parser<'a>() -> impl Parser<'a, &'a str, Param> + 'a {
    ident_parser()
        .then(
            just(":")
                .padded()
                .ignore_then(super::types::ts_type_parser())
                .or_not(),
        )
        .map_with(|(pat, ty), extra| Param {
            pat: Pat::Ident(pat),
            ty,
            span: extra.span().into(),
        })
        .labelled("parameter")
}

/// Parser for an optional type annotation: `: type`
pub fn optional_type_annotation<'a>() -> impl Parser<'a, &'a str, Option<TsType>> + 'a {
    just(":")
        .padded()
        .ignore_then(super::types::ts_type_parser())
        .or_not()
        .labelled("type annotation")
}

/// Parser for a block statement: `{ stmt1; stmt2; ... }`
pub fn block_parser<'a>(
    stmt: impl Parser<'a, &'a str, Stmt> + Clone + 'a,
) -> impl Parser<'a, &'a str, BlockStmt> + 'a {
    stmt.repeated()
        .collect::<Vec<_>>()
        .delimited_by(just("{").padded(), just("}").padded())
        .map_with(|stmts, extra| BlockStmt {
            stmts,
            span: extra.span().into(),
        })
        .labelled("block")
}

/// Parser for an optional block statement.
pub fn optional_block_parser<'a>(
    stmt: impl Parser<'a, &'a str, Stmt> + Clone + 'a,
) -> impl Parser<'a, &'a str, Option<BlockStmt>> + 'a {
    block_parser(stmt).or_not()
}
