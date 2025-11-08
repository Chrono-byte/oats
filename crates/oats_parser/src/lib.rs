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
        .map_with_span(|body, span| Module { body, span })
        .then_ignore(end())
}

/// Parser for statements.
fn stmt_parser() -> impl Parser<char, Stmt, Error = Simple<char>> {
    recursive(|stmt| {
        choice((
            declare_fn_parser().map(Stmt::DeclareFn),
            class_decl_parser(stmt.clone()).map(Stmt::ClassDecl),
            fn_decl_parser(stmt.clone()).map(Stmt::FnDecl),
            var_decl_parser().map(Stmt::VarDecl),
            expr_stmt_parser().map(Stmt::ExprStmt),
            if_stmt_parser(stmt.clone()),
        ))
    })
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
        .map_with_span(|((ident, params), return_type), span| DeclareFn {
            ident,
            params,
            return_type,
            span,
        })
}

/// Parser for function declarations.
fn fn_decl_parser(
    stmt: impl Parser<char, Stmt, Error = Simple<char>> + Clone,
) -> impl Parser<char, FnDecl, Error = Simple<char>> {
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
        .then(block_stmt_parser(stmt).map(Some))
        .map_with_span(|(((ident, params), return_type), body), span| FnDecl {
            ident,
            params,
            body,
            return_type,
            span,
        })
}

/// Parser for class declarations.
fn class_decl_parser(
    stmt: impl Parser<char, Stmt, Error = Simple<char>> + Clone,
) -> impl Parser<char, ClassDecl, Error = Simple<char>> {
    text::keyword("class")
        .padded()
        .ignore_then(ident_parser())
        .then(
            text::keyword("extends")
                .padded()
                .ignore_then(ident_parser().map(Expr::Ident))
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

/// Parser for class members.
fn class_member_parser(
    stmt: impl Parser<char, Stmt, Error = Simple<char>> + Clone,
) -> impl Parser<char, ClassMember, Error = Simple<char>> {
    choice((
        constructor_parser(stmt.clone()).map(ClassMember::Constructor),
        method_parser(stmt.clone()).map(ClassMember::Method),
        field_parser().map(ClassMember::Field),
    ))
}

/// Parser for constructor.
fn constructor_parser(
    stmt: impl Parser<char, Stmt, Error = Simple<char>> + Clone,
) -> impl Parser<char, ConstructorDecl, Error = Simple<char>> {
    text::keyword("constructor")
        .padded()
        .ignore_then(
            param_parser()
                .separated_by(just(',').padded())
                .collect::<Vec<_>>()
                .delimited_by(just('(').padded(), just(')').padded()),
        )
        .then(block_stmt_parser(stmt).map(Some))
        .map_with_span(|(params, body), span| ConstructorDecl { params, body, span })
}

/// Parser for method.
fn method_parser(
    stmt: impl Parser<char, Stmt, Error = Simple<char>> + Clone,
) -> impl Parser<char, MethodDecl, Error = Simple<char>> {
    ident_parser()
        .then(
            param_parser()
                .separated_by(just(',').padded())
                .collect::<Vec<_>>()
                .delimited_by(just('(').padded(), just(')').padded()),
        )
        .then(just(':').padded().ignore_then(ts_type_parser()).or_not())
        .then(block_stmt_parser(stmt).map(Some))
        .map_with_span(|(((ident, params), return_type), body), span| MethodDecl {
            ident,
            params,
            body,
            return_type,
            span,
        })
}

/// Parser for field.
fn field_parser() -> impl Parser<char, FieldDecl, Error = Simple<char>> {
    ident_parser()
        .then(just(':').padded().ignore_then(ts_type_parser()).or_not())
        .then_ignore(just(';').padded())
        .map_with_span(|(ident, ty), span| FieldDecl { ident, ty, span })
}

/// Parser for identifiers.
fn ident_parser() -> impl Parser<char, Ident, Error = Simple<char>> {
    text::ident().map_with_span(|sym, span| Ident { sym, span })
}

/// Parser for literals.
fn lit_parser() -> impl Parser<char, Lit, Error = Simple<char>> {
    let float_parser = text::int(10)
        .then(just('.'))
        .then(text::int(10))
        .map(|((int_part, _), dec_part)| {
            let s = format!("{}.{}", int_part, dec_part);
            s.parse::<f64>().unwrap()
        })
        .then(text::ident().or_not())
        .try_map(|(f, suffix), span| {
            let ty = suffix.as_deref().unwrap_or("f64");
            match ty {
                "f32" => Ok(Lit::F32(f as f32)),
                "f64" => Ok(Lit::F64(f)),
                _ => Err(Simple::custom(span, "unknown float suffix")),
            }
        });
    let int_parser = text::int(10).then(text::ident().or_not()).try_map(
        |(num, suffix): (String, Option<String>), span: std::ops::Range<usize>| {
            let ty = suffix.as_deref().unwrap_or("i64");
            match ty {
                "i8" => Ok(Lit::I8(num.parse().unwrap())),
                "i16" => Ok(Lit::I16(num.parse().unwrap())),
                "i32" => Ok(Lit::I32(num.parse().unwrap())),
                "i64" => Ok(Lit::I64(num.parse().unwrap())),
                "i128" => Ok(Lit::I128(num.parse().unwrap())),
                "isize" => Ok(Lit::ISize(num.parse().unwrap())),
                "u8" => Ok(Lit::U8(num.parse().unwrap())),
                "u16" => Ok(Lit::U16(num.parse().unwrap())),
                "u32" => Ok(Lit::U32(num.parse().unwrap())),
                "u64" => Ok(Lit::U64(num.parse().unwrap())),
                "u128" => Ok(Lit::U128(num.parse().unwrap())),
                "usize" => Ok(Lit::USize(num.parse().unwrap())),
                _ => Err(Simple::custom(span, "unknown integer suffix")),
            }
        },
    );
    choice((
        float_parser,
        int_parser,
        just('"')
            .ignore_then(take_until(just('"')))
            .map(|(chars, _)| Lit::Str(chars.into_iter().collect())),
        text::keyword("true").map(|_| Lit::Bool(true)),
        text::keyword("false").map(|_| Lit::Bool(false)),
        text::keyword("null").map(|_| Lit::Null),
    ))
}

/// Parser for expressions.
fn expr_parser() -> impl Parser<char, Expr, Error = Simple<char>> {
    recursive(|expr| {
        let primary = choice((
            lit_parser().map(Expr::Lit),
            ident_parser().map(Expr::Ident),
            expr.clone()
                .delimited_by(just('(').padded(), just(')').padded())
                .map(|e| {
                    Expr::Paren(ParenExpr {
                        expr: Box::new(e),
                        span: 0..0,
                    })
                }), // TODO span
        ));

        primary.then(
            choice((
                just('+').padded().map(|_| BinaryOp::Plus),
                just('-').padded().map(|_| BinaryOp::Minus),
                just('*').padded().map(|_| BinaryOp::Mul),
                just('/').padded().map(|_| BinaryOp::Div),
                just("==").padded().map(|_| BinaryOp::EqEq),
                just("!=").padded().map(|_| BinaryOp::NotEq),
                just('<').padded().map(|_| BinaryOp::Lt),
                just("<=").padded().map(|_| BinaryOp::LtEq),
                just('>').padded().map(|_| BinaryOp::Gt),
                just(">=").padded().map(|_| BinaryOp::GtEq),
            ))
            .then(expr)
            .repeated(),
        ).foldl(|left, (op, right)| Expr::Bin(BinExpr {
            op,
            left: Box::new(left),
            right: Box::new(right),
            span: 0..0, // TODO
        }))
    })
}

/// Parser for function parameters.
fn param_parser() -> impl Parser<char, Param, Error = Simple<char>> {
    ident_parser()
        .then_ignore(just(':').padded())
        .then(ts_type_parser())
        .map_with_span(|(ident, ty), span| Param {
            pat: Pat::Ident(ident),
            ty: Some(ty),
            span,
        })
}

/// Parser for variable declarators.
fn var_declarator_parser() -> impl Parser<char, VarDeclarator, Error = Simple<char>> {
    ident_parser()
        .then(just('=').padded().ignore_then(expr_parser()).or_not())
        .map_with_span(|(name, init), span| VarDeclarator {
            name: Pat::Ident(name),
            init,
            span,
        })
}

/// Parser for variable declarations.
fn var_decl_parser() -> impl Parser<char, VarDecl, Error = Simple<char>> {
    choice((
        text::keyword("let")
            .padded()
            .then(text::keyword("mut").or_not())
            .map(|(_, mut_opt)| VarDeclKind::Let {
                mutable: mut_opt.is_some(),
            }),
        text::keyword("const").map(|_| VarDeclKind::Const),
    ))
    .padded()
    .then(
        var_declarator_parser()
            .separated_by(just(',').padded())
            .collect::<Vec<_>>(),
    )
    .then_ignore(just(';').padded())
    .map_with_span(|(kind, decls), span| VarDecl { kind, decls, span })
}

/// Parser for if statements.
fn if_stmt_parser(
    stmt: impl Parser<char, Stmt, Error = Simple<char>> + Clone,
) -> impl Parser<char, Stmt, Error = Simple<char>> {
    text::keyword("if")
        .padded()
        .ignore_then(expr_parser().delimited_by(just('(').padded(), just(')').padded()))
        .then(stmt.clone())
        .then(text::keyword("else").padded().ignore_then(stmt).or_not())
        .map_with_span(|((test, cons), alt), span| Stmt::If(IfStmt {
            test,
            cons: Box::new(cons),
            alt: alt.map(Box::new),
            span,
        }))
}

/// Parser for TypeScript types.
fn ts_type_parser() -> impl Parser<char, TsType, Error = Simple<char>> {
    let base = choice((ts_keyword_type_parser(), ts_type_ref_parser()));
    let array_suffix = just('[')
        .padded()
        .ignore_then(just(']').padded())
        .repeated();
    base.then(array_suffix).map(|(mut ty, suffixes)| {
        for _ in suffixes {
            ty = TsType::TsArrayType(TsArrayType {
                elem_type: Box::new(ty),
                span: 0..0, // TODO
            });
        }
        ty
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
    ident_parser().map_with_span(|ident, span| {
        TsType::TsTypeRef(TsTypeRef {
            type_name: TsEntityName::Ident(ident),
            type_params: None,
            span,
        })
    })
}

/// Parser for expression statements.
fn expr_stmt_parser() -> impl Parser<char, ExprStmt, Error = Simple<char>> {
    expr_parser()
        .then_ignore(just(';').padded())
        .map_with_span(|expr, span| ExprStmt { expr, span })
}

/// Parser for block statements.
fn block_stmt_parser(
    stmt: impl Parser<char, Stmt, Error = Simple<char>> + Clone,
) -> impl Parser<char, BlockStmt, Error = Simple<char>> {
    stmt.repeated()
        .collect::<Vec<_>>()
        .delimited_by(just('{').padded(), just('}').padded())
        .map_with_span(|stmts, span| BlockStmt {
            stmts,
            span,
        })
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
    fn test_var_decl() {
        let input = "let x = 5;";
        let result = parse_module(input);
        assert!(result.is_ok());
        let module = result.unwrap();
        assert_eq!(module.body.len(), 1);
        if let Stmt::VarDecl(var_decl) = &module.body[0] {
            assert!(matches!(var_decl.kind, VarDeclKind::Let { mutable: false }));
            assert_eq!(var_decl.decls.len(), 1);
            let Pat::Ident(ident) = &var_decl.decls[0].name;
            assert_eq!(ident.sym, "x");
            assert!(matches!(
                var_decl.decls[0].init,
                Some(Expr::Lit(Lit::I64(5)))
            ));
        } else {
            panic!("Expected VarDecl");
        }
    }

    #[test]
    fn test_var_decl_string() {
        let input = "let y = \"hello\";";
        let result = parse_module(input);
        assert!(result.is_ok());
        let module = result.unwrap();
        assert_eq!(module.body.len(), 1);
        if let Stmt::VarDecl(var_decl) = &module.body[0] {
            assert!(matches!(var_decl.kind, VarDeclKind::Let { mutable: false }));
            assert_eq!(var_decl.decls.len(), 1);
            let Pat::Ident(ident) = &var_decl.decls[0].name;
            assert_eq!(ident.sym, "y");
            if let Some(Expr::Lit(Lit::Str(s))) = &var_decl.decls[0].init {
                assert_eq!(s, "hello");
            } else {
                panic!("Expected string literal");
            }
        } else {
            panic!("Expected VarDecl");
        }
    }

    #[test]
    fn test_var_decl_float() {
        let input = "let pi = 3.14;";
        let result = parse_module(input);
        assert!(result.is_ok());
        let module = result.unwrap();
        assert_eq!(module.body.len(), 1);
        if let Stmt::VarDecl(var_decl) = &module.body[0] {
            assert!(matches!(var_decl.kind, VarDeclKind::Let { mutable: false }));
            assert_eq!(var_decl.decls.len(), 1);
            let Pat::Ident(ident) = &var_decl.decls[0].name;
            assert_eq!(ident.sym, "pi");
            assert!(matches!(
                var_decl.decls[0].init,
                Some(Expr::Lit(Lit::F64(3.14)))
            ));
        } else {
            panic!("Expected VarDecl");
        }
    }

    #[test]
    fn test_var_decl_i32() {
        let input = "let x = 42i32;";
        let result = parse_module(input);
        assert!(result.is_ok());
        let module = result.unwrap();
        assert_eq!(module.body.len(), 1);
        if let Stmt::VarDecl(var_decl) = &module.body[0] {
            assert!(matches!(var_decl.kind, VarDeclKind::Let { mutable: false }));
            assert_eq!(var_decl.decls.len(), 1);
            let Pat::Ident(ident) = &var_decl.decls[0].name;
            assert_eq!(ident.sym, "x");
            assert!(matches!(
                var_decl.decls[0].init,
                Some(Expr::Lit(Lit::I32(42)))
            ));
        } else {
            panic!("Expected VarDecl");
        }
    }

    #[test]
    fn test_var_decl_i8() {
        let input = "let y = 10i8;";
        let result = parse_module(input);
        assert!(result.is_ok());
        let module = result.unwrap();
        assert_eq!(module.body.len(), 1);
        if let Stmt::VarDecl(var_decl) = &module.body[0] {
            assert!(matches!(var_decl.kind, VarDeclKind::Let { mutable: false }));
            assert_eq!(var_decl.decls.len(), 1);
            let Pat::Ident(ident) = &var_decl.decls[0].name;
            assert_eq!(ident.sym, "y");
            assert!(matches!(
                var_decl.decls[0].init,
                Some(Expr::Lit(Lit::I8(10)))
            ));
        } else {
            panic!("Expected VarDecl");
        }
    }

    #[test]
    fn test_expr_stmt() {
        let input = "5;";
        let result = parse_module(input);
        assert!(result.is_ok());
        let module = result.unwrap();
        assert_eq!(module.body.len(), 1);
        if let Stmt::ExprStmt(expr_stmt) = &module.body[0] {
            assert!(matches!(expr_stmt.expr, Expr::Lit(Lit::I64(5))));
        } else {
            panic!("Expected ExprStmt");
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

    #[test]
    fn test_class() {
        let input = "class Node { constructor(value: number) {} }";
        let result = parse_module(input);
        assert!(result.is_ok());
        let module = result.unwrap();
        assert_eq!(module.body.len(), 1);
        if let Stmt::ClassDecl(class_decl) = &module.body[0] {
            assert_eq!(class_decl.ident.sym, "Node");
            assert!(class_decl.super_class.is_none());
            assert_eq!(class_decl.body.len(), 1);
            if let ClassMember::Constructor(constructor) = &class_decl.body[0] {
                assert_eq!(constructor.params.len(), 1);
                let Pat::Ident(ident) = &constructor.params[0].pat;
                assert_eq!(ident.sym, "value");
                assert!(matches!(
                    constructor.params[0].ty,
                    Some(TsType::TsKeywordType(TsKeywordType::TsNumberKeyword))
                ));
            } else {
                panic!("Expected Constructor");
            }
        } else {
            panic!("Expected ClassDecl");
        }
    }

    #[test]
    fn test_nested_array_type() {
        let input = "class Node { value: number[][]; }";
        let result = parse_module(input);
        assert!(result.is_ok());
        let module = result.unwrap();
        assert_eq!(module.body.len(), 1);
        if let Stmt::ClassDecl(class_decl) = &module.body[0] {
            assert_eq!(class_decl.ident.sym, "Node");
            assert_eq!(class_decl.body.len(), 1);
            if let ClassMember::Field(field_decl) = &class_decl.body[0] {
                assert_eq!(field_decl.ident.sym, "value");
                // Should be number[][]
                if let Some(TsType::TsArrayType(outer)) = &field_decl.ty {
                    if let TsType::TsArrayType(inner) = &*outer.elem_type {
                        assert!(matches!(
                            &*inner.elem_type,
                            TsType::TsKeywordType(TsKeywordType::TsNumberKeyword)
                        ));
                    } else {
                        panic!("Expected inner array");
                    }
                } else {
                    panic!("Expected outer array");
                }
            } else {
                panic!("Expected FieldDecl");
            }
        } else {
            panic!("Expected ClassDecl");
        }
    }

    #[test]
    fn test_var_decl_let_mut() {
        let input = "let mut z = 100;";
        let result = parse_module(input);
        assert!(result.is_ok());
        let module = result.unwrap();
        assert_eq!(module.body.len(), 1);
        if let Stmt::VarDecl(var_decl) = &module.body[0] {
            assert!(matches!(var_decl.kind, VarDeclKind::Let { mutable: true }));
            assert_eq!(var_decl.decls.len(), 1);
            let Pat::Ident(ident) = &var_decl.decls[0].name;
            assert_eq!(ident.sym, "z");
            assert!(matches!(
                var_decl.decls[0].init,
                Some(Expr::Lit(Lit::I64(100)))
            ));
        } else {
            panic!("Expected VarDecl");
        }
    }

    #[test]
    fn test_function_with_body() {
        let input = "function test(): void { let x = 5; }";
        let result = parse_module(input);
        assert!(result.is_ok());
        let module = result.unwrap();
        assert_eq!(module.body.len(), 1);
        if let Stmt::FnDecl(fn_decl) = &module.body[0] {
            assert_eq!(fn_decl.ident.sym, "test");
            assert!(fn_decl.params.is_empty());
            assert!(matches!(
                fn_decl.return_type,
                Some(TsType::TsKeywordType(TsKeywordType::TsVoidKeyword))
            ));
            // Check that the body contains a statement
            assert!(fn_decl.body.is_some());
            let body = fn_decl.body.as_ref().unwrap();
            assert_eq!(body.stmts.len(), 1);
            if let Stmt::VarDecl(var_decl) = &body.stmts[0] {
                assert!(matches!(var_decl.kind, VarDeclKind::Let { mutable: false }));
                assert_eq!(var_decl.decls.len(), 1);
                let Pat::Ident(ident) = &var_decl.decls[0].name;
                assert_eq!(ident.sym, "x");
                assert!(matches!(
                    var_decl.decls[0].init,
                    Some(Expr::Lit(Lit::I64(5)))
                ));
            } else {
                panic!("Expected VarDecl in function body");
            }
        } else {
            panic!("Expected FnDecl");
        }
    }
}
