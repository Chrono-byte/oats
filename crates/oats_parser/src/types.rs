//! Type annotation parsers
//!
//! This module contains parsers for Oats type annotations.
//! All type-related parsing logic is grouped here for Locality of Behavior.

use chumsky::prelude::*;
use oats_ast::*;

/// Parser for Oats types.
///
/// Supports:
/// - Keyword types: `number`, `string`, `boolean`, `void`
/// - Type references: `MyType`, `MyType<T, U>`
/// - Array types: `number[]`, `string[][]`
/// - Union types: `T | U`
/// - Intersection types: `T & U`
pub fn ts_type_parser() -> impl Parser<char, TsType, Error = Simple<char>> {
    recursive(|ty| {
        // Base types (lowest precedence)
        let base = choice((
            ts_keyword_type_parser(),
            ts_type_ref_parser(ty.clone()),
            ts_tuple_type_parser(ty.clone()),
            ts_function_type_parser(ty.clone()),
            ts_type_lit_parser(ty.clone()),
            just('(')
                .padded()
                .ignore_then(ty.clone())
                .then_ignore(just(')').padded()),
        ));

        // Array suffix: [] or [][]
        let array_suffix = just('[')
            .padded()
            .ignore_then(just(']').padded())
            .map_with_span(|_, span| span);

        let with_array = base.then(array_suffix.repeated()).map_with_span(
            |(mut base_ty, suffixes), base_span| {
                let mut current_span = base_span;
                for suffix_span in suffixes {
                    // Update span to include the array suffix
                    current_span = current_span.start..suffix_span.end;
                    base_ty = TsType::TsArrayType(TsArrayType {
                        elem_type: Box::new(base_ty),
                        span: current_span.clone(),
                    });
                }
                base_ty
            },
        );

        // Intersection types: T & U (higher precedence than union)
        // Use recursive ty parser to avoid clone
        let intersection = with_array
            .then(just('&').padded().ignore_then(ty.clone()).repeated())
            .map_with_span(|(first, rest), span| {
                if rest.is_empty() {
                    first
                } else {
                    let mut types = vec![first];
                    types.extend(rest);
                    TsType::TsIntersectionType(TsIntersectionType { types, span })
                }
            });

        // Union types: T | U (lowest precedence)
        // Use recursive ty parser to avoid clone
        intersection
            .then(just('|').padded().ignore_then(ty.clone()).repeated())
            .map_with_span(|(first, rest), span| {
                if rest.is_empty() {
                    first
                } else {
                    let mut types = vec![first];
                    types.extend(rest);
                    TsType::TsUnionType(TsUnionType { types, span })
                }
            })
    })
}

/// Parser for keyword types.
fn ts_keyword_type_parser() -> impl Parser<char, TsType, Error = Simple<char>> {
    choice((
        text::keyword("number").map(|_| TsType::TsKeywordType(TsKeywordType::TsNumberKeyword)),
        text::keyword("string").map(|_| TsType::TsKeywordType(TsKeywordType::TsStringKeyword)),
        text::keyword("boolean").map(|_| TsType::TsKeywordType(TsKeywordType::TsBooleanKeyword)),
        text::keyword("void").map(|_| TsType::TsKeywordType(TsKeywordType::TsVoidKeyword)),
    ))
}

/// Parser for type references (user-defined types).
///
/// Pattern: `MyType` or `MyType<T, U>`
fn ts_type_ref_parser(
    ty: impl Parser<char, TsType, Error = Simple<char>> + Clone,
) -> impl Parser<char, TsType, Error = Simple<char>> {
    super::common::ident_parser()
        .then(
            // Generic type parameters: <T, U>
            ty.clone()
                .separated_by(just(',').padded())
                .collect::<Vec<_>>()
                .delimited_by(just('<').padded(), just('>').padded())
                .or_not(),
        )
        .map_with_span(|(ident, type_params), span| {
            TsType::TsTypeRef(TsTypeRef {
                type_name: TsEntityName::Ident(ident.clone()),
                type_params: type_params.map(|params| TsTypeParamInstantiation {
                    params,
                    span: span.clone(),
                }),
                span: ident.span,
            })
        })
}

/// Parser for tuple types.
///
/// Pattern: `[T, U, V]`
fn ts_tuple_type_parser(
    ty: impl Parser<char, TsType, Error = Simple<char>> + Clone,
) -> impl Parser<char, TsType, Error = Simple<char>> {
    ty.clone()
        .separated_by(just(',').padded())
        .collect::<Vec<_>>()
        .delimited_by(just('[').padded(), just(']').padded())
        .map_with_span(|elem_types, span| TsType::TsTupleType(TsTupleType { elem_types, span }))
}

/// Parser for function types.
///
/// Pattern: `(a: number, b: string) => number`
fn ts_function_type_parser(
    ty: impl Parser<char, TsType, Error = Simple<char>> + Clone,
) -> impl Parser<char, TsType, Error = Simple<char>> {
    super::common::param_list_parser()
        .then_ignore(just("=>").padded())
        .then(ty)
        .map_with_span(|(params, return_type), span| {
            TsType::TsFunctionType(TsFunctionType {
                params,
                return_type: Box::new(return_type),
                span,
            })
        })
}

/// Parser for type literals (object types).
///
/// Pattern: `{ prop: type, method(): type, [key: string]: type }`
fn ts_type_lit_parser(
    ty: impl Parser<char, TsType, Error = Simple<char>> + Clone,
) -> impl Parser<char, TsType, Error = Simple<char>> {
    ts_type_element_parser(ty.clone())
        .separated_by(just(',').padded())
        .collect::<Vec<_>>()
        .delimited_by(just('{').padded(), just('}').padded())
        .map_with_span(|members, span| TsType::TsTypeLit(TsTypeLit { members, span }))
}

/// Parser for type elements in a type literal.
fn ts_type_element_parser(
    ty: impl Parser<char, TsType, Error = Simple<char>> + Clone,
) -> impl Parser<char, TsTypeElement, Error = Simple<char>> {
    choice((
        // Index signature: [key: string]: type
        just('[')
            .padded()
            .ignore_then(super::common::ident_parser())
            .then(just(':').padded().ignore_then(ty.clone()))
            .then_ignore(just(']').padded())
            .then(just(':').padded().ignore_then(ty.clone()))
            .then(
                text::keyword("readonly")
                    .padded()
                    .to(true)
                    .or_not()
                    .map(|opt| opt.unwrap_or(false)),
            )
            .then_ignore(just(';').padded().or_not())
            .map_with_span(|(((key_name, key_type), value_type), readonly), span| {
                TsTypeElement::IndexSignature(IndexSignature {
                    key_name,
                    key_type,
                    value_type,
                    readonly,
                    span,
                })
            }),
        // Method signature: name(params): returnType
        super::common::ident_parser()
            .then(just('?').padded().or_not())
            .then(super::common::param_list_parser())
            .then(just(':').padded().ignore_then(ty.clone()))
            .then_ignore(just(';').padded().or_not())
            .map_with_span(|(((ident, optional), params), return_type), span| {
                TsTypeElement::Method(TsMethodSignature {
                    ident,
                    params,
                    return_type,
                    optional: optional.is_some(),
                    span,
                })
            }),
        // Property signature: readonly? name?: type
        text::keyword("readonly")
            .padded()
            .or_not()
            .then(super::common::ident_parser())
            .then(just('?').padded().or_not())
            .then(just(':').padded().ignore_then(ty.clone()))
            .then_ignore(just(';').padded().or_not())
            .map_with_span(|(((readonly, ident), optional), ty), span| {
                TsTypeElement::Property(TsPropertySignature {
                    ident,
                    ty,
                    optional: optional.is_some(),
                    readonly: readonly.is_some(),
                    span,
                })
            }),
    ))
}
