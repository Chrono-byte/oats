//! Type parsers

use chumsky::prelude::*;
use oats_ast::*;

/// Parser for TypeScript-style types.
pub fn ts_type_parser<'a>() -> impl Parser<'a, &'a str, TsType> + 'a {
    recursive(|ty| {
        choice((
            // Keyword types
            text::keyword("number")
                .padded()
                .to(TsType::TsKeywordType(TsKeywordType::TsNumberKeyword)),
            text::keyword("string")
                .padded()
                .to(TsType::TsKeywordType(TsKeywordType::TsStringKeyword)),
            text::keyword("boolean")
                .padded()
                .to(TsType::TsKeywordType(TsKeywordType::TsBooleanKeyword)),
            text::keyword("void")
                .padded()
                .to(TsType::TsKeywordType(TsKeywordType::TsVoidKeyword)),
            // Array types: type[]
            ty.clone()
                .then_ignore(just("[").padded())
                .then_ignore(just("]").padded())
                .map_with(|elem_type, extra| {
                    let elem_type: TsType = elem_type;
                    TsType::TsArrayType(TsArrayType {
                        elem_type: Box::new(elem_type),
                        span: <std::ops::Range<usize>>::from(extra.span()),
                    })
                }),
            // Parenthesized type: (type)
            ty.delimited_by(just("(").padded(), just(")").padded()),
        ))
        .labelled("type")
    })
}
