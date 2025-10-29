//! Oats Parser
//!
//! This crate implements a parser for the Oats language using chumsky.
//! It takes a string input and produces an `oats_ast::Module`.

use chumsky::prelude::*;
use oats_ast::*;

/// Parse a string into an Oats AST Module.
pub fn parse_module(input: &str) -> Result<Module, Vec<Simple<char>>> {
    let parser = module_parser();
    parser.parse(input)
}

/// Parser for the top-level module.
fn module_parser() -> impl Parser<char, Module, Error = Simple<char>> {
    stmt_parser()
        .repeated()
        .collect::<Vec<_>>()
        .map(|body| Module {
            body,
            span: 0..0, // TODO: proper span
        })
        .then_ignore(end())
}

/// Parser for statements.
fn stmt_parser() -> impl Parser<char, Stmt, Error = Simple<char>> {
    choice((
        declare_fn_parser().map(Stmt::DeclareFn),
        class_decl_parser().map(Stmt::ClassDecl),
        fn_decl_parser().map(Stmt::FnDecl),
    ))
}

/// Parser for declare function.
fn declare_fn_parser() -> impl Parser<char, DeclareFn, Error = Simple<char>> {
    text::keyword("declare")
        .padded()
        .ignore_then(text::keyword("function"))
        .padded()
        .ignore_then(ident_parser())
        .then(
            param_parser()
                .separated_by(just(',').padded())
                .collect::<Vec<_>>()
                .delimited_by(just('(').padded(), just(')').padded()),
        )
        .then(just(':').padded().ignore_then(ts_type_parser()))
        .then_ignore(just(';').padded())
        .map(|((ident, params), return_type)| DeclareFn {
            ident,
            params,
            return_type,
            span: 0..0, // TODO
        })
}

/// Parser for function declarations.
fn fn_decl_parser() -> impl Parser<char, FnDecl, Error = Simple<char>> {
    text::keyword("function")
        .padded()
        .ignore_then(ident_parser())
        .then(
            param_parser()
                .separated_by(just(',').padded())
                .collect::<Vec<_>>()
                .delimited_by(just('(').padded(), just(')').padded()),
        )
        .then(just(':').padded().ignore_then(ts_type_parser()).or_not())
        .then(
            just('{')
                .padded()
                .ignore_then(just('}').padded())
                .map(|_| None::<BlockStmt>),
        )
        .map(|(((ident, params), return_type), body)| FnDecl {
            ident,
            params,
            body,
            return_type,
            span: 0..0, // TODO: spans
        })
}

/// Parser for class declarations.
fn class_decl_parser() -> impl Parser<char, ClassDecl, Error = Simple<char>> {
    text::keyword("class")
        .padded()
        .ignore_then(ident_parser())
        .then(
            just("extends")
                .padded()
                .ignore_then(ident_parser())
                .map(|ident| Some(Expr::Ident(ident))) // TODO: proper expr
                .or_not()
                .map(|opt| opt.flatten()),
        )
        .then(
            just('{')
                .padded()
                .ignore_then(class_member_parser().repeated().collect::<Vec<_>>())
                .then_ignore(just('}').padded()),
        )
        .map(|((ident, super_class), body)| ClassDecl {
            ident,
            super_class,
            body,
            span: 0..0, // TODO
        })
}

/// Parser for class members.
fn class_member_parser() -> impl Parser<char, ClassMember, Error = Simple<char>> {
    choice((
        field_parser().map(ClassMember::Field),
        constructor_parser().map(ClassMember::Constructor),
        method_parser().map(ClassMember::Method),
    ))
}

/// Parser for fields.
fn field_parser() -> impl Parser<char, FieldDecl, Error = Simple<char>> {
    ident_parser()
        .then_ignore(just(':').padded())
        .then(ts_type_parser())
        .then_ignore(just(';').padded())
        .map(|(ident, ty)| FieldDecl {
            ident,
            ty: Some(ty),
            span: 0..0, // TODO
        })
}

/// Parser for constructor.
fn constructor_parser() -> impl Parser<char, ConstructorDecl, Error = Simple<char>> {
    text::keyword("constructor")
        .padded()
        .ignore_then(
            param_parser()
                .separated_by(just(',').padded())
                .collect::<Vec<_>>()
                .delimited_by(just('(').padded(), just(')').padded()),
        )
        .then(
            just('{')
                .padded()
                .ignore_then(just('}').padded())
                .map(|_| None::<BlockStmt>), // TODO: parse body
        )
        .map(|(params, body)| ConstructorDecl {
            params,
            body,
            span: 0..0, // TODO
        })
}

/// Parser for methods.
fn method_parser() -> impl Parser<char, MethodDecl, Error = Simple<char>> {
    ident_parser()
        .then(
            param_parser()
                .separated_by(just(',').padded())
                .collect::<Vec<_>>()
                .delimited_by(just('(').padded(), just(')').padded()),
        )
        .then(just(':').padded().ignore_then(ts_type_parser()).or_not())
        .then(
            just('{')
                .padded()
                .ignore_then(just('}').padded())
                .map(|_| None::<BlockStmt>), // TODO: parse body
        )
        .map(|(((ident, params), return_type), body)| MethodDecl {
            ident,
            params,
            body,
            return_type,
            span: 0..0, // TODO
        })
}

/// Parser for identifiers.
fn ident_parser() -> impl Parser<char, Ident, Error = Simple<char>> {
    text::ident().map(|sym| Ident {
        sym,
        span: 0..0, // TODO
    })
}

/// Parser for function parameters.
fn param_parser() -> impl Parser<char, Param, Error = Simple<char>> {
    ident_parser()
        .then_ignore(just(':').padded())
        .then(ts_type_parser())
        .map(|(ident, ty)| Param {
            pat: Pat::Ident(ident),
            ty: Some(ty),
            span: 0..0, // TODO
        })
}

/// Parser for TypeScript types.
fn ts_type_parser() -> impl Parser<char, TsType, Error = Simple<char>> {
    recursive(|ts_type| {
        let array = ts_type
            .clone()
            .then_ignore(just('[').padded())
            .then_ignore(just(']').padded())
            .map(|elem_type| TsType::TsArrayType(TsArrayType {
                elem_type: Box::new(elem_type),
                span: 0..0, // TODO
            }));
        choice((
            array,
            ts_type_ref_parser(),
            ts_keyword_type_parser(),
        ))
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

/// Parser for type references.
fn ts_type_ref_parser() -> impl Parser<char, TsType, Error = Simple<char>> {
    ident_parser()
        .map(|ident| TsType::TsTypeRef(TsTypeRef {
            type_name: TsEntityName::Ident(ident),
            type_params: None,
            span: 0..0, // TODO
        }))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_simple_function() {
        let input = "function main(): void {}";
        let result = parse_module(input);
        if let Err(errors) = &result {
            for e in errors {
                println!("Parse error: {:?}", e);
            }
        }
        assert!(result.is_ok());
        let module = result.unwrap();
        assert_eq!(module.body.len(), 1);
        if let Stmt::FnDecl(fn_decl) = &module.body[0] {
            assert_eq!(fn_decl.ident.sym, "main");
            assert!(fn_decl.params.is_empty());
            assert!(matches!(
                fn_decl.return_type,
                Some(TsType::TsKeywordType(TsKeywordType::TsVoidKeyword))
            ));
        } else {
            panic!("Expected FnDecl");
        }
    }

    #[test]
    fn test_declare_function() {
        let input = "declare function print_str(s: string): void;";
        let result = parse_module(input);
        assert!(result.is_ok());
        let module = result.unwrap();
        assert_eq!(module.body.len(), 1);
        if let Stmt::DeclareFn(declare_fn) = &module.body[0] {
            assert_eq!(declare_fn.ident.sym, "print_str");
            assert_eq!(declare_fn.params.len(), 1);
            let Pat::Ident(ident) = &declare_fn.params[0].pat;
            assert_eq!(ident.sym, "s");
            assert!(matches!(
                declare_fn.params[0].ty,
                Some(TsType::TsKeywordType(TsKeywordType::TsStringKeyword))
            ));
            assert!(matches!(
                declare_fn.return_type,
                TsType::TsKeywordType(TsKeywordType::TsVoidKeyword)
            ));
        } else {
            panic!("Expected DeclareFn");
        }
    }

    #[test]
    fn test_class_decl() {
        let input = "class Animal { }";
        let result = parse_module(input);
        assert!(result.is_ok());
        let module = result.unwrap();
        assert_eq!(module.body.len(), 1);
        if let Stmt::ClassDecl(class_decl) = &module.body[0] {
            assert_eq!(class_decl.ident.sym, "Animal");
            assert!(class_decl.super_class.is_none());
            assert!(class_decl.body.is_empty());
        } else {
            panic!("Expected ClassDecl");
        }
    }

    #[test]
    fn test_class_decl_with_super() {
        let input = "class Dog extends Animal { }";
        let result = parse_module(input);
        assert!(result.is_ok());
        let module = result.unwrap();
        assert_eq!(module.body.len(), 1);
        if let Stmt::ClassDecl(class_decl) = &module.body[0] {
            assert_eq!(class_decl.ident.sym, "Dog");
            if let Some(Expr::Ident(ident)) = &class_decl.super_class {
                assert_eq!(ident.sym, "Animal");
            } else {
                panic!("Expected super class");
            }
            assert!(class_decl.body.is_empty());
        } else {
            panic!("Expected ClassDecl");
        }
    }

    #[test]
    fn test_field_decl() {
        let input = "class Person { name: string; age: number; }";
        let result = parse_module(input);
        assert!(result.is_ok());
        let module = result.unwrap();
        assert_eq!(module.body.len(), 1);
        if let Stmt::ClassDecl(class_decl) = &module.body[0] {
            assert_eq!(class_decl.ident.sym, "Person");
            assert!(class_decl.super_class.is_none());
            assert_eq!(class_decl.body.len(), 2);
            if let ClassMember::Field(field_decl) = &class_decl.body[0] {
                assert_eq!(field_decl.ident.sym, "name");
                assert!(matches!(
                    field_decl.ty,
                    Some(TsType::TsKeywordType(TsKeywordType::TsStringKeyword))
                ));
            } else {
                panic!("Expected FieldDecl");
            }
            if let ClassMember::Field(field_decl) = &class_decl.body[1] {
                assert_eq!(field_decl.ident.sym, "age");
                assert!(matches!(
                    field_decl.ty,
                    Some(TsType::TsKeywordType(TsKeywordType::TsNumberKeyword))
                ));
            } else {
                panic!("Expected FieldDecl");
            }
        } else {
            panic!("Expected ClassDecl");
        }
    }

    #[test]
    fn test_field_decl_with_array_type() {
        let input = "class Node { value: number[]; }";
        let result = parse_module(input);
        assert!(result.is_ok());
        let module = result.unwrap();
        assert_eq!(module.body.len(), 1);
        if let Stmt::ClassDecl(class_decl) = &module.body[0] {
            assert_eq!(class_decl.ident.sym, "Node");
            assert_eq!(class_decl.body.len(), 1);
            if let ClassMember::Field(field_decl) = &class_decl.body[0] {
                assert_eq!(field_decl.ident.sym, "value");
                assert!(matches!(
                    field_decl.ty,
                    Some(TsType::TsArrayType(TsArrayType { .. }))
                ));
            } else {
                panic!("Expected FieldDecl");
            }
        } else {
            panic!("Expected ClassDecl");
        }
    }

    #[test]
    fn test_constructor_decl() {
        let input = "class Point { constructor(x: number, y: number) {} }";
        let result = parse_module(input);
        assert!(result.is_ok());
        let module = result.unwrap();
        assert_eq!(module.body.len(), 1);
        if let Stmt::ClassDecl(class_decl) = &module.body[0] {
            assert_eq!(class_decl.ident.sym, "Point");
            assert!(class_decl.super_class.is_none());
            assert_eq!(class_decl.body.len(), 1);
            if let ClassMember::Constructor(constructor_decl) = &class_decl.body[0] {
                assert_eq!(constructor_decl.params.len(), 2);
                assert!(matches!(
                    constructor_decl.params[0].ty,
                    Some(TsType::TsKeywordType(TsKeywordType::TsNumberKeyword))
                ));
                assert!(matches!(
                    constructor_decl.params[1].ty,
                    Some(TsType::TsKeywordType(TsKeywordType::TsNumberKeyword))
                ));
            } else {
                panic!("Expected ConstructorDecl");
            }
        } else {
            panic!("Expected ClassDecl");
        }
    }

    #[test]
    fn test_method_decl() {
        let input = "class Calculator { add(a: number, b: number): number {} }";
        let result = parse_module(input);
        assert!(result.is_ok());
        let module = result.unwrap();
        assert_eq!(module.body.len(), 1);
        if let Stmt::ClassDecl(class_decl) = &module.body[0] {
            assert_eq!(class_decl.ident.sym, "Calculator");
            assert!(class_decl.super_class.is_none());
            assert_eq!(class_decl.body.len(), 1);
            if let ClassMember::Method(method_decl) = &class_decl.body[0] {
                assert_eq!(method_decl.ident.sym, "add");
                assert_eq!(method_decl.params.len(), 2);
                assert!(matches!(
                    method_decl.return_type,
                    Some(TsType::TsKeywordType(TsKeywordType::TsNumberKeyword))
                ));
            } else {
                panic!("Expected MethodDecl");
            }
        } else {
            panic!("Expected ClassDecl");
        }
    }

    #[test]
    fn test_class_declaration() {
        let input = "class Dog extends Animal {}";
        let result = parse_module(input);
        assert!(result.is_ok());
        let module = result.unwrap();
        assert_eq!(module.body.len(), 1);
        if let Stmt::ClassDecl(class_decl) = &module.body[0] {
            assert_eq!(class_decl.ident.sym, "Dog");
            if let Some(Expr::Ident(ident)) = &class_decl.super_class {
                assert_eq!(ident.sym, "Animal");
            } else {
                panic!("Expected super class");
            }
            assert!(class_decl.body.is_empty());
        } else {
            panic!("Expected ClassDecl");
        }
    }
}
