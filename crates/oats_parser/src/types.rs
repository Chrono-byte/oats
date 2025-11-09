//! Type parsers

use super::common;
use chumsky::prelude::*;
use oats_ast::*;

/// Parser limits to prevent stack overflow from pathological inputs.
const MAX_TYPE_CHAIN_LENGTH: usize = 1024;

/// Parser for TypeScript-style types.
/// Uses a single clone of the recursive parser to avoid stack overflow and compilation hangs.
pub fn ts_type_parser<'a>() -> impl Parser<'a, &'a str, TsType> + 'a {
    recursive(|ty| {
        // Clone ty ONCE and box it immediately to reduce type complexity.
        // This prevents exponential type growth during compilation that causes memory leaks.
        // By boxing early, we ensure all uses share the same boxed type rather than
        // creating multiple distinct cloned types.
        let ty_boxed = ty.clone().boxed();

        let base_type = choice((
            keyword_type_parser().boxed(),
            type_ref_parser(ty_boxed.clone()).boxed(),
            tuple_type_parser(ty_boxed.clone()).boxed(),
            type_literal_parser(ty_boxed.clone()).boxed(),
            function_type_parser(ty_boxed.clone()).boxed(),
            ty_boxed
                .clone()
                .delimited_by(just("(").padded(), just(")").padded())
                .boxed(),
        ));

        // Primary type: base + array suffixes
        let primary = base_type
            .then(
                just("[")
                    .padded()
                    .then(just("]").padded())
                    .repeated()
                    .at_most(MAX_TYPE_CHAIN_LENGTH)
                    .collect::<Vec<_>>(),
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
                    .then(primary.clone())
                    .repeated()
                    .at_most(MAX_TYPE_CHAIN_LENGTH)
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
                    .at_most(MAX_TYPE_CHAIN_LENGTH)
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
                .map(|params| TsTypeParamInstantiation { params, span: 0..0 })
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_keyword_types() {
        // Test using Parser::parse as shown in the chumsky guide
        let parser = ts_type_parser();

        // number keyword
        let result = parser.parse("number").into_result();
        assert!(result.is_ok(), "number should parse successfully");
        if let Ok(TsType::TsKeywordType(TsKeywordType::TsNumberKeyword)) = result {
            // Correct type
        } else {
            panic!("Expected TsNumberKeyword, got {:?}", result);
        }

        // string keyword
        let result = parser.parse("string").into_result();
        assert!(result.is_ok(), "string should parse successfully");

        // boolean keyword
        let result = parser.parse("boolean").into_result();
        assert!(result.is_ok(), "boolean should parse successfully");

        // void keyword
        let result = parser.parse("void").into_result();
        assert!(result.is_ok(), "void should parse successfully");
    }

    #[test]
    fn test_type_references() {
        let parser = ts_type_parser();

        // Simple type reference
        let result = parser.parse("MyType").into_result();
        assert!(result.is_ok(), "MyType should parse successfully");

        // Type reference with type parameters
        let result = parser.parse("Array<string>").into_result();
        assert!(result.is_ok(), "Array<string> should parse successfully");

        // Multiple type parameters
        let result = parser.parse("Map<string, number>").into_result();
        assert!(
            result.is_ok(),
            "Map<string, number> should parse successfully"
        );
    }

    #[test]
    fn test_array_types() {
        let parser = ts_type_parser();

        // Single array
        let result = parser.parse("number[]").into_result();
        assert!(result.is_ok(), "number[] should parse successfully");

        // Nested arrays
        let result = parser.parse("number[][]").into_result();
        assert!(result.is_ok(), "number[][] should parse successfully");
    }

    #[test]
    fn test_union_types() {
        let parser = ts_type_parser();

        // Union type
        let result = parser.parse("string | number").into_result();
        assert!(result.is_ok(), "string | number should parse successfully");

        // Multiple unions
        let result = parser.parse("string | number | boolean").into_result();
        assert!(
            result.is_ok(),
            "string | number | boolean should parse successfully"
        );
    }

    #[test]
    fn test_intersection_types() {
        let parser = ts_type_parser();

        // Intersection type
        let result = parser.parse("A & B").into_result();
        assert!(result.is_ok(), "A & B should parse successfully");
    }

    #[test]
    fn test_tuple_types() {
        let parser = ts_type_parser();

        // Tuple type
        let result = parser.parse("[string, number]").into_result();
        assert!(result.is_ok(), "[string, number] should parse successfully");

        // Tuple with trailing comma
        let result = parser.parse("[string, number,]").into_result();
        assert!(
            result.is_ok(),
            "[string, number,] should parse successfully"
        );
    }

    #[test]
    fn test_type_literals() {
        let parser = ts_type_parser();

        // Simple type literal
        let result = parser.parse("{ name: string }").into_result();
        assert!(
            result.is_ok(),
            "{{ name: string }} should parse successfully"
        );

        // Type literal with optional property
        let result = parser.parse("{ name?: string }").into_result();
        assert!(
            result.is_ok(),
            "{{ name?: string }} should parse successfully"
        );

        // Type literal with readonly property
        let result = parser.parse("{ readonly name: string }").into_result();
        assert!(
            result.is_ok(),
            "{{ readonly name: string }} should parse successfully"
        );
    }

    #[test]
    fn test_function_types() {
        let parser = ts_type_parser();

        // Function type
        let result = parser.parse("() => number").into_result();
        assert!(result.is_ok(), "() => number should parse successfully");

        // Function type with parameters
        let result = parser.parse("(x: number) => string").into_result();
        assert!(
            result.is_ok(),
            "(x: number) => string should parse successfully"
        );
    }

    #[test]
    fn test_using_check_method() {
        let parser = ts_type_parser();

        // Using Parser::check to validate input without getting the output
        let result = parser.check("number");
        assert!(!result.has_errors(), "number should be valid");

        let result = parser.check("invalid type syntax!!!");
        assert!(result.has_errors(), "invalid syntax should produce errors");
    }

    #[test]
    fn test_complex_types() {
        let parser = ts_type_parser();

        // Complex nested type
        let result = parser
            .parse("Array<{ name: string, age: number }>")
            .into_result();
        assert!(
            result.is_ok(),
            "Complex nested type should parse successfully"
        );

        // Union of arrays
        let result = parser.parse("string[] | number[]").into_result();
        assert!(result.is_ok(), "Union of arrays should parse successfully");

        // Function returning union
        let result = parser.parse("() => string | number").into_result();
        assert!(
            result.is_ok(),
            "Function returning union should parse successfully"
        );
    }

    #[test]
    fn test_parenthesized_types() {
        let parser = ts_type_parser();

        // Parenthesized type
        let result = parser.parse("(string | number)").into_result();
        assert!(
            result.is_ok(),
            "(string | number) should parse successfully"
        );

        // Parenthesized type in union
        let result = parser.parse("(string | number) | boolean").into_result();
        assert!(
            result.is_ok(),
            "(string | number) | boolean should parse successfully"
        );
    }
}
