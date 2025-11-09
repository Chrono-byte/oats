//! Type annotation parsers
//!
//! This module contains parsers for Oats type annotations.
//! All type-related parsing logic is grouped here for Locality of Behavior.
//!
//! Type parsing handles:
//! - Keyword types: `number`, `string`, `boolean`, `void`
//! - Type references: `MyType`, `MyType<T, U>`
//! - Array types: `number[]`, `string[][]`
//! - Union types: `T | U`
//! - Intersection types: `T & U`
//! - Function types: `(a: T) => U`
//! - Tuple types: `[T, U, V]`
//! - Type literals: `{ prop: T, method(): U }`

use chumsky::prelude::*;
use oats_ast::*;

/// Parser for Oats types with proper precedence handling.
///
/// Precedence (lowest to highest):
/// 1. Union types: `T | U`
/// 2. Intersection types: `T & U`
/// 3. Array types: `T[]`, `T[][]`
/// 4. Base types: keywords, references, literals, functions, tuples, parenthesized
pub fn ts_type_parser<'a>() -> impl Parser<'a, &'a str, TsType> + 'a {
    recursive(|ty| {
        // Base parser with array suffix handling
        let base = ts_type_base(ty)
            .then(
                just("[")
                    .padded()
                    .ignore_then(just("]").padded())
                    .repeated()
                    .collect::<Vec<_>>(),
            )
            .map_with(|(mut ty, count), extra| {
                for _ in 0..count.len() {
                    ty = TsType::TsArrayType(TsArrayType {
                        elem_type: Box::new(ty),
                        span: extra.span().into(),
                    });
                }
                ty
            })
            .boxed();

        // Intersection types: T & U & V
        base.clone()
            .then(
                just("&")
                    .padded()
                    .ignore_then(base.clone())
                    .repeated()
                    .collect::<Vec<_>>(),
            )
            .map_with(|(first, rest), extra| {
                if rest.is_empty() {
                    first
                } else {
                    let mut types = vec![first];
                    types.extend(rest);
                    TsType::TsIntersectionType(TsIntersectionType {
                        types,
                        span: extra.span().into(),
                    })
                }
            })
            .boxed()
            // Union types: T | U | V (lowest precedence)
            .then(
                just("|")
                    .padded()
                    .ignore_then(
                        base.clone()
                            .then(
                                just("&")
                                    .padded()
                                    .ignore_then(base.clone())
                                    .repeated()
                                    .collect::<Vec<_>>(),
                            )
                            .map_with(|(first, rest), extra| {
                                if rest.is_empty() {
                                    first
                                } else {
                                    let mut types = vec![first];
                                    types.extend(rest);
                                    TsType::TsIntersectionType(TsIntersectionType {
                                        types,
                                        span: extra.span().into(),
                                    })
                                }
                            })
                            .boxed()
                    )
                    .repeated()
                    .collect::<Vec<_>>(),
            )
            .map_with(|(first, rest), extra| {
                if rest.is_empty() {
                    first
                } else {
                    let mut types = vec![first];
                    types.extend(rest);
                    TsType::TsUnionType(TsUnionType {
                        types,
                        span: extra.span().into(),
                    })
                }
            })
    })
    .labelled("type")
}

/// Parse base types without postfix operators.
fn ts_type_base<'a>(
    ty: impl Parser<'a, &'a str, TsType> + Clone + 'a,
) -> impl Parser<'a, &'a str, TsType> + 'a {
    choice((
        // Keywords: number, string, boolean, void
        ts_keyword_type(),
        // Type references: MyType or MyType<T, U>
        super::common::ident_parser()
            .then(
                just("<")
                    .padded()
                    .ignore_then(
                        ty.clone()
                            .separated_by(just(",").padded())
                            .collect::<Vec<_>>(),
                    )
                    .then_ignore(just(">").padded())
                    .or_not(),
            )
            .map_with(|(ident, type_params), extra| {
                TsType::TsTypeRef(TsTypeRef {
                    type_name: TsEntityName::Ident(ident.clone()),
                    type_params: type_params.map(|params| TsTypeParamInstantiation {
                        params,
                        span: extra.span().into(),
                    }),
                    span: ident.span,
                })
            }),
        // Function types: (a: T, b: U) => V
        just("(")
            .padded()
            .ignore_then(
                super::common::ident_parser()
                    .then(
                        just(":")
                            .padded()
                            .ignore_then(ty.clone())
                            .or_not(),
                    )
                    .map_with(|(pat, ty), extra| Param {
                        pat: Pat::Ident(pat),
                        ty,
                        span: extra.span().into(),
                    })
                    .separated_by(just(",").padded())
                    .collect::<Vec<_>>(),
            )
            .then_ignore(just(")").padded())
            .then_ignore(just("=>").padded())
            .then(ty.clone())
            .map_with(|(params, return_type), extra| {
                TsType::TsFunctionType(TsFunctionType {
                    params,
                    return_type: Box::new(return_type),
                    span: extra.span().into(),
                })
            }),
        // Tuple types: [T, U, V]
        just("[")
            .padded()
            .ignore_then(
                ty.clone()
                    .separated_by(just(",").padded())
                    .collect::<Vec<_>>(),
            )
            .then_ignore(just("]").padded())
            .map_with(|elem_types, extra| {
                TsType::TsTupleType(TsTupleType {
                    elem_types,
                    span: extra.span().into(),
                })
            }),
        // Type literals: { prop: T, method(): U, [key: string]: V }
        just("{")
            .padded()
            .ignore_then(
                ts_type_element(ty.clone())
                    .separated_by(just(",").padded())
                    .collect::<Vec<_>>(),
            )
            .then_ignore(just("}").padded())
            .map_with(|members, extra| {
                TsType::TsTypeLit(TsTypeLit {
                    members,
                    span: extra.span().into(),
                })
            }),
        // Parenthesized type
        just("(")
            .padded()
            .ignore_then(ty)
            .then_ignore(just(")").padded()),
    ))
}

/// Parser for keyword types: `number`, `string`, `boolean`, `void`.
fn ts_keyword_type<'a>() -> impl Parser<'a, &'a str, TsType> + 'a {
    choice((
        text::keyword("number").to(TsType::TsKeywordType(TsKeywordType::TsNumberKeyword)),
        text::keyword("string").to(TsType::TsKeywordType(TsKeywordType::TsStringKeyword)),
        text::keyword("boolean").to(TsType::TsKeywordType(TsKeywordType::TsBooleanKeyword)),
        text::keyword("void").to(TsType::TsKeywordType(TsKeywordType::TsVoidKeyword)),
    ))
}

/// Parser for type elements in a type literal: properties, methods, and index signatures.
fn ts_type_element<'a>(
    ty: impl Parser<'a, &'a str, TsType> + Clone + 'a,
) -> impl Parser<'a, &'a str, TsTypeElement> + 'a {
    choice((
        // Index signature: [key: string]: type;
        just("[")
            .padded()
            .ignore_then(super::common::ident_parser())
            .then(just(":").padded().ignore_then(ty.clone()))
            .then_ignore(just("]").padded())
            .then(just(":").padded().ignore_then(ty.clone()))
            .then(
                text::keyword("readonly")
                    .padded()
                    .to(true)
                    .or_not()
                    .map(|opt| opt.unwrap_or(false)),
            )
            .then_ignore(just(";").padded().or_not())
            .map_with(|(((key_name, key_type), value_type), readonly), extra| {
                TsTypeElement::IndexSignature(IndexSignature {
                    key_name,
                    key_type,
                    value_type,
                    readonly,
                    span: extra.span().into(),
                })
            })
            .labelled("index signature"),
        // Method signature: name?(params): returnType;
        super::common::ident_parser()
            .then(just("?").padded().or_not())
            .then(
                just("(")
                    .padded()
                    .ignore_then(
                        super::common::ident_parser()
                            .then(
                                just(":")
                                    .padded()
                                    .ignore_then(ty.clone())
                                    .or_not(),
                            )
                            .map_with(|(pat, ty), extra| Param {
                                pat: Pat::Ident(pat),
                                ty,
                                span: extra.span().into(),
                            })
                            .separated_by(just(",").padded())
                            .collect::<Vec<_>>(),
                    )
                    .then_ignore(just(")").padded()),
            )
            .then(just(":").padded().ignore_then(ty.clone()))
            .then_ignore(just(";").padded().or_not())
            .map_with(|(((ident, optional), params), return_type), extra| {
                TsTypeElement::Method(TsMethodSignature {
                    ident,
                    params,
                    return_type,
                    optional: optional.is_some(),
                    span: extra.span().into(),
                })
            })
            .labelled("method signature"),
        // Property signature: readonly? name?: type;
        text::keyword("readonly")
            .padded()
            .or_not()
            .then(super::common::ident_parser())
            .then(just("?").padded().or_not())
            .then(just(":").padded().ignore_then(ty))
            .then_ignore(just(";").padded().or_not())
            .map_with(|(((readonly, ident), optional), ty), extra| {
                TsTypeElement::Property(TsPropertySignature {
                    ident,
                    ty,
                    optional: optional.is_some(),
                    readonly: readonly.is_some(),
                    span: extra.span().into(),
                })
            })
            .labelled("property signature"),
    ))
}
