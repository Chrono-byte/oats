//! Type parsers

use super::common;
use chumsky::prelude::*;
use oats_ast::*;

/// Parser for TypeScript-style types.
/// Workaround: Clone ty only once and reuse to avoid construction-time cycles.
pub fn ts_type_parser<'a>() -> impl Parser<'a, &'a str, TsType> + 'a {
    recursive(|ty| {
        // Clone ty ONCE at the top and reuse - this is key to avoiding cycles
        let ty_clone = ty.clone().boxed();

        // Base type parser - all use the same ty_clone
        let base_type = choice((
            keyword_type_parser().boxed(),
            type_ref_parser(ty_clone.clone()).boxed(),
            tuple_type_parser(ty_clone.clone()).boxed(),
            type_literal_parser(ty_clone.clone()).boxed(),
            function_type_parser(ty_clone.clone()).boxed(),
            ty_clone.clone().delimited_by(just("(").padded(), just(")").padded()).boxed(),
        ));

        // Primary type: base + array suffixes
        let primary = base_type
            .then(
                just("[").padded().then(just("]").padded()).repeated().collect::<Vec<_>>(),
            )
            .map_with(|(mut elem_type, array_suffixes), extra| {
                for _ in array_suffixes {
                    elem_type = TsType::TsArrayType(TsArrayType {
                        elem_type: Box::new(elem_type),
                        span: extra.span().into(),
                    });
                }
                elem_type
            })
            .boxed();

        // Intersection: primary & primary & ...
        let intersection = primary
            .clone()
            .then(
                just("&")
                    .padded()
                    .then(primary)
                    .repeated()
                    .collect::<Vec<_>>(),
            )
            .map_with(|(first, rest), extra| {
                if rest.is_empty() {
                    first
                } else {
                    let mut types = vec![first];
                    for (_, ty) in rest {
                        types.push(ty);
                    }
                    TsType::TsIntersectionType(TsIntersectionType {
                        types,
                        span: extra.span().into(),
                    })
                }
            })
            .boxed();

        // Union: intersection | intersection | ...
        intersection
            .clone()
            .then(
                just("|")
                    .padded()
                    .then(intersection)
                    .repeated()
                    .collect::<Vec<_>>(),
            )
            .map_with(|(first, rest), extra| {
                if rest.is_empty() {
                    first
                } else {
                    let mut types = vec![first];
                    for (_, ty) in rest {
                        types.push(ty);
                    }
                    TsType::TsUnionType(TsUnionType {
                        types,
                        span: extra.span().into(),
                    })
                }
            })
            .labelled("type")
            .boxed()
    })
}

/// Parser for intersection types: type1 & type2 & type3
fn intersection_type_parser<'a>(
    ty: impl Parser<'a, &'a str, TsType> + Clone + 'a,
) -> impl Parser<'a, &'a str, TsType> + 'a {
    primary_type_parser(ty.clone())
        .then(
            just("&")
                .padded()
                .then(primary_type_parser(ty.clone()))
                .repeated()
                .collect::<Vec<_>>(),
        )
        .map_with(|(first, rest), extra| {
            if rest.is_empty() {
                first
            } else {
                let mut types = vec![first];
                for (_, ty) in rest {
                    types.push(ty);
                }
                TsType::TsIntersectionType(TsIntersectionType {
                    types,
                    span: extra.span().into(),
                })
            }
        })
        .labelled("intersection type")
}

/// Parser for primary types (non-union, non-intersection)
fn primary_type_parser<'a>(
    ty: impl Parser<'a, &'a str, TsType> + Clone + 'a,
) -> impl Parser<'a, &'a str, TsType> + 'a {
    // Parse base type, then optionally apply array suffixes
    // Use base_type_parser which includes parenthesized types
    base_type_parser(ty.clone())
        .then(
            just("[").padded().then(just("]").padded()).repeated().collect::<Vec<_>>(),
        )
        .map_with(|(mut elem_type, array_suffixes), extra| {
            // Apply array suffixes from right to left
            for _ in array_suffixes {
                elem_type = TsType::TsArrayType(TsArrayType {
                    elem_type: Box::new(elem_type),
                    span: extra.span().into(),
                });
            }
            elem_type
        })
        .labelled("primary type")
}

/// Parser for keyword types: number, string, boolean, void
fn keyword_type_parser<'a>() -> impl Parser<'a, &'a str, TsType> + 'a {
    choice((
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
    ))
    .labelled("keyword type")
}

/// Parser for type references: TypeName or TypeName<T1, T2>
fn type_ref_parser<'a>(
    ty: impl Parser<'a, &'a str, TsType> + Clone + 'a,
) -> impl Parser<'a, &'a str, TsType> + 'a {
    // Parse identifier, but make sure it's not a keyword
    common::ident_parser()
        .then(
            // Optional type parameters: <T1, T2>
            // Use the full type parser for type parameters, but only if we see <
            just("<")
                .padded()
                .ignore_then(
                    ty.clone()
                        .separated_by(just(",").padded())
                        .allow_trailing()
                        .collect::<Vec<_>>(),
                )
                .then_ignore(just(">").padded())
                .map(|params| TsTypeParamInstantiation {
                    params,
                    span: 0..0,
                })
                .or_not(),
        )
        .map_with(|(ident, type_params), extra| {
            TsType::TsTypeRef(TsTypeRef {
                type_name: TsEntityName::Ident(ident),
                type_params,
                span: extra.span().into(),
            })
        })
        .labelled("type reference")
}

/// Parser for parenthesized types: (type)
/// Matches the expression parser pattern: paren_expr_parser(expr) calls the full recursive expr.
fn paren_type_parser<'a>(
    ty: impl Parser<'a, &'a str, TsType> + Clone + 'a,
) -> impl Parser<'a, &'a str, TsType> + 'a {
    ty.delimited_by(just("(").padded(), just(")").padded())
        .labelled("parenthesized type")
}

/// Parser for base types (without array suffixes)
fn base_type_parser<'a>(
    ty: impl Parser<'a, &'a str, TsType> + Clone + 'a,
) -> impl Parser<'a, &'a str, TsType> + 'a {
    choice((
        // Keyword types (no nested types, so safe)
        keyword_type_parser().boxed(),
        // Type references: TypeName or TypeName<T1, T2>
        // For type parameters, use the full type parser (ty) - this is OK because
        // type parameters are inside <>, not parentheses, so no cycle
        type_ref_parser(ty.clone()).boxed(),
        // Tuple types: [type1, type2, ...]
        // For tuple elements, use the full type parser (ty) - this is OK because
        // tuple elements are inside [], not parentheses, so no cycle
        tuple_type_parser(ty.clone()).boxed(),
        // Type literals: { prop: type, ... }
        // For property types, use the full type parser (ty) - this is OK because
        // property types are inside {}, not parentheses, so no cycle
        type_literal_parser(ty.clone()).boxed(),
        // Function types: (params) => type
        // For return type, use the full type parser (ty) - this is OK because
        // function types use =>, not parentheses for grouping, so no cycle
        function_type_parser(ty.clone()).boxed(),
        // Parenthesized types: (type)
        // Use a separate function (like expression parser's paren_expr_parser) to match the pattern
        paren_type_parser(ty.clone()).boxed(),
    ))
    .labelled("base type")
}

/// Parser for tuple types: [type1, type2, ...]
fn tuple_type_parser<'a>(
    ty: impl Parser<'a, &'a str, TsType> + Clone + 'a,
) -> impl Parser<'a, &'a str, TsType> + 'a {
    ty.clone()
        .separated_by(just(",").padded())
        .allow_trailing()
        .collect::<Vec<_>>()
        .delimited_by(just("[").padded(), just("]").padded())
        .map_with(|elem_types, extra| {
            TsType::TsTupleType(TsTupleType {
                elem_types,
                span: extra.span().into(),
            })
        })
        .labelled("tuple type")
}

/// Parser for type literals: { prop: type, ... }
fn type_literal_parser<'a>(
    ty: impl Parser<'a, &'a str, TsType> + Clone + 'a,
) -> impl Parser<'a, &'a str, TsType> + 'a {
    type_element_parser(ty)
        .separated_by(just(",").padded())
        .allow_trailing()
        .collect::<Vec<_>>()
        .delimited_by(just("{").padded(), just("}").padded())
        .map_with(|members, extra| {
            TsType::TsTypeLit(TsTypeLit {
                members,
                span: extra.span().into(),
            })
        })
        .labelled("type literal")
}

/// Parser for type elements in type literals
fn type_element_parser<'a>(
    ty: impl Parser<'a, &'a str, TsType> + Clone + 'a,
) -> impl Parser<'a, &'a str, TsTypeElement> + 'a {
    choice((
        // Property signature: name: type or name?: type or readonly name: type
        text::keyword("readonly")
            .padded()
            .or_not()
            .then(common::ident_parser())
            .then(just("?").padded().or_not())
            .then(just(":").padded().ignore_then(ty.clone()))
            .map_with(|(((readonly, ident), optional), ty), extra| {
                TsTypeElement::Property(TsPropertySignature {
                    ident,
                    ty,
                    optional: optional.is_some(),
                    readonly: readonly.is_some(),
                    span: extra.span().into(),
                })
            }),
        // Index signature: [key: type]: type
        just("[")
            .padded()
            .ignore_then(common::ident_parser())
            .then(just(":").padded().ignore_then(ty.clone()))
            .then_ignore(just("]").padded())
            .then(just(":").padded().ignore_then(ty.clone()))
            .map_with(|((key_name, key_type), value_type), extra| {
                TsTypeElement::IndexSignature(IndexSignature {
                    key_name,
                    key_type,
                    value_type,
                    readonly: false,
                    span: extra.span().into(),
                })
            }),
    ))
    .labelled("type element")
}

/// Parser for function types: (params) => type
fn function_type_parser<'a>(
    ty: impl Parser<'a, &'a str, TsType> + Clone + 'a,
) -> impl Parser<'a, &'a str, TsType> + 'a {
    common::param_list_parser()
        .then(just("=>").padded().ignore_then(ty.clone()))
        .map_with(|(params, return_type), extra| {
            TsType::TsFunctionType(TsFunctionType {
                params,
                return_type: Box::new(return_type),
                span: extra.span().into(),
            })
        })
        .labelled("function type")
}
