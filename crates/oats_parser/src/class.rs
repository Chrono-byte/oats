//! Class-related parsers
//!
//! This module groups all class parsing logic together for Locality of Behavior.
//! Includes class declarations, constructors, methods, and fields.

use super::common;
use chumsky::prelude::*;
use oats_ast::*;

/// Parser for class declarations.
///
/// Pattern: `class Name extends SuperClass? { members }`
pub fn class_decl_parser<'a>(
    expr: impl Parser<'a, &'a str, Expr> + Clone,
    stmt: impl Parser<'a, &'a str, Stmt> + Clone,
) -> impl Parser<'a, &'a str, ClassDecl> {
    text::keyword("class")
        .padded()
        .ignore_then(common::ident_parser())
        .then(
            text::keyword("extends")
                .padded()
                .ignore_then(expr.clone())
                .or_not(),
        )
        .then(
            just("{")
                .padded()
                .ignore_then(
                    class_member_parser(stmt.clone())
                        .repeated()
                        .collect::<Vec<_>>(),
                )
                .then_ignore(just("}").padded()),
        )
        .map_with(|((ident, super_class), body), extra| ClassDecl {
            ident,
            super_class,
            body,
            span: extra.span().into(),
        })
}

/// Parser for class members (constructor, method, or field).
pub fn class_member_parser<'a>(
    stmt: impl Parser<'a, &'a str, Stmt> + Clone,
) -> impl Parser<'a, &'a str, ClassMember> {
    choice((
        constructor_parser(stmt.clone()).map(ClassMember::Constructor),
        method_parser(stmt.clone()).map(ClassMember::Method),
        field_parser().map(ClassMember::Field),
    ))
}

/// Parser for constructors.
///
/// Pattern: `constructor(params) { body }`
fn constructor_parser<'a>(
    stmt: impl Parser<'a, &'a str, Stmt> + Clone,
) -> impl Parser<'a, &'a str, ConstructorDecl> {
    text::keyword("constructor")
        .padded()
        .ignore_then(common::param_list_parser())
        .then(common::optional_block_parser(stmt))
        .map_with(|(params, body), extra| ConstructorDecl { params, body, span: extra.span().into() })
}

/// Parser for methods.
///
/// Pattern: `methodName(params): returnType? { body }`
fn method_parser<'a>(
    stmt: impl Parser<'a, &'a str, Stmt> + Clone,
) -> impl Parser<'a, &'a str, MethodDecl> {
    common::ident_parser()
        .then(common::param_list_parser())
        .then(common::optional_type_annotation())
        .then(common::optional_block_parser(stmt))
        .map_with(|(((ident, params), return_type), body), extra| MethodDecl {
            ident,
            params,
            body,
            return_type,
            span: extra.span().into(),
        })
}

/// Parser for fields.
///
/// Pattern: `fieldName: type?;`
fn field_parser<'a>() -> impl Parser<'a, &'a str, FieldDecl> {
    common::ident_parser()
        .then(common::optional_type_annotation())
        .then_ignore(just(";").padded())
        .map_with(|(ident, ty), extra| FieldDecl { ident, ty, span: extra.span().into() })
}
