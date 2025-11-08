//! Type annotation parsers
//!
//! This module contains parsers for TypeScript-style type annotations.
//! All type-related parsing logic is grouped here for Locality of Behavior.

use chumsky::prelude::*;
use oats_ast::*;

/// Parser for TypeScript types.
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
            just('(').padded().ignore_then(ty.clone()).then_ignore(just(')').padded()),
        ));
        
        // Array suffix: [] or [][]
        let array_suffix = just('[')
            .padded()
            .ignore_then(just(']').padded())
            .map_with_span(|_, span| span);
        
        let with_array = base.then(array_suffix.repeated())
            .map_with_span(|(mut base_ty, suffixes), base_span| {
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
            });

        // Intersection types: T & U (higher precedence than union)
        // Use recursive ty parser to avoid clone
        let intersection = with_array
            .then(
                just('&')
                    .padded()
                    .ignore_then(ty.clone())
                    .repeated()
            )
            .map_with_span(|(first, rest), span| {
                if rest.is_empty() {
                    first
                } else {
                    let mut types = vec![first];
                    types.extend(rest);
                    TsType::TsIntersectionType(TsIntersectionType {
                        types,
                        span: span.into(),
                    })
                }
            });

        // Union types: T | U (lowest precedence)
        // Use recursive ty parser to avoid clone
        intersection
            .then(
                just('|')
                    .padded()
                    .ignore_then(ty.clone())
                    .repeated()
            )
            .map_with_span(|(first, rest), span| {
                if rest.is_empty() {
                    first
                } else {
                    let mut types = vec![first];
                    types.extend(rest);
                    TsType::TsUnionType(TsUnionType {
                        types,
                        span: span.into(),
                    })
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
                .or_not()
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
        .map_with_span(|elem_types, span| {
            TsType::TsTupleType(TsTupleType {
                elem_types,
                span: span.into(),
            })
        })
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
                span: span.into(),
            })
        })
}

