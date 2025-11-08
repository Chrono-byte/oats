//! Class-related parsers
//!
//! This module groups all class parsing logic together for Locality of Behavior.
//! Includes class declarations, constructors, methods, and fields.

use super::common;
use super::expr;
use chumsky::prelude::*;
use oats_ast::*;

/// Parser for class declarations.
///
/// Pattern: `class Name extends SuperClass? { members }`
pub fn class_decl_parser(
    stmt: impl Parser<char, Stmt, Error = Simple<char>> + Clone,
) -> impl Parser<char, ClassDecl, Error = Simple<char>> {
    text::keyword("class")
        .padded()
        .ignore_then(common::ident_parser())
        .then(
            text::keyword("extends")
                .padded()
                .ignore_then(expr::expr_parser())
                .or_not(),
        )
        .then(
            just('{')
                .padded()
                .ignore_then(
                    class_member_parser(stmt.clone())
                        .repeated()
                        .collect::<Vec<_>>(),
                )
                .then_ignore(just('}').padded()),
        )
        .map_with_span(|((ident, super_class), body), span| ClassDecl {
            ident,
            super_class,
            body,
            span,
        })
}

/// Parser for class members (constructor, method, or field).
pub fn class_member_parser(
    stmt: impl Parser<char, Stmt, Error = Simple<char>> + Clone,
) -> impl Parser<char, ClassMember, Error = Simple<char>> {
    choice((
        constructor_parser(stmt.clone()).map(ClassMember::Constructor),
        method_parser(stmt.clone()).map(ClassMember::Method),
        field_parser().map(ClassMember::Field),
    ))
}

/// Parser for constructors.
///
/// Pattern: `constructor(params) { body }`
fn constructor_parser(
    stmt: impl Parser<char, Stmt, Error = Simple<char>> + Clone,
) -> impl Parser<char, ConstructorDecl, Error = Simple<char>> {
    text::keyword("constructor")
        .padded()
        .ignore_then(common::param_list_parser())
        .then(common::optional_block_parser(stmt))
        .map_with_span(|(params, body), span| ConstructorDecl { params, body, span })
}

/// Parser for methods.
///
/// Pattern: `methodName(params): returnType? { body }`
fn method_parser(
    stmt: impl Parser<char, Stmt, Error = Simple<char>> + Clone,
) -> impl Parser<char, MethodDecl, Error = Simple<char>> {
    common::ident_parser()
        .then(common::param_list_parser())
        .then(common::optional_type_annotation())
        .then(common::optional_block_parser(stmt))
        .map_with_span(|(((ident, params), return_type), body), span| MethodDecl {
            ident,
            params,
            body,
            return_type,
            span,
        })
}

/// Parser for fields.
///
/// Pattern: `fieldName: type?;`
fn field_parser() -> impl Parser<char, FieldDecl, Error = Simple<char>> {
    common::ident_parser()
        .then(common::optional_type_annotation())
        .then_ignore(just(';').padded())
        .map_with_span(|(ident, ty), span| FieldDecl { ident, ty, span })
}
