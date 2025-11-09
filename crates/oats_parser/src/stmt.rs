//! Statement parsers

use super::common;
use super::expr;
use super::function;
use chumsky::prelude::*;
use oats_ast::*;

/// Statement parser.
///
/// Handles all statement types in the Oats language.
pub fn stmt_parser<'a>(
    stmt: impl Parser<'a, &'a str, Stmt> + Clone + 'a,
) -> impl Parser<'a, &'a str, Stmt> + 'a {
    let expr = expr::expr_parser(stmt.clone()).boxed();
    let stmt_inner = stmt;

    choice((
        // Export statements (must come first to match "export function")
        export_stmt_parser(stmt_inner.clone(), expr.clone()).boxed(),
        // Import statements
        import_stmt_parser().map(Stmt::Import).boxed(),
        // Declare function statements
        declare_fn_parser().map(Stmt::DeclareFn).boxed(),
        // Type alias declarations
        type_alias_parser().map(Stmt::TypeAlias).boxed(),
        // Interface declarations
        interface_decl_parser(stmt_inner.clone())
            .map(Stmt::InterfaceDecl)
            .boxed(),
        // Enum declarations
        enum_decl_parser().map(Stmt::EnumDecl).boxed(),
        // Namespace declarations
        namespace_decl_parser(stmt_inner.clone())
            .map(Stmt::NamespaceDecl)
            .boxed(),
        // Class declarations
        class_decl_parser(stmt_inner.clone(), expr.clone())
            .map(Stmt::ClassDecl)
            .boxed(),
        // Function declarations
        function::fn_decl_parser(stmt_inner.clone())
            .map(Stmt::FnDecl)
            .boxed(),
        // Variable declarations
        var_decl_parser(expr.clone()).map(Stmt::VarDecl).boxed(),
        // If statements
        if_stmt_parser(stmt_inner.clone(), expr.clone())
            .map(Stmt::If)
            .boxed(),
        // For statements
        for_stmt_parser(stmt_inner.clone(), expr.clone())
            .map(|s| Stmt::For(Box::new(s)))
            .boxed(),
        // For-in statements
        for_in_stmt_parser(stmt_inner.clone(), expr.clone())
            .map(Stmt::ForIn)
            .boxed(),
        // For-of statements
        for_of_stmt_parser(stmt_inner.clone(), expr.clone())
            .map(Stmt::ForOf)
            .boxed(),
        // While statements
        while_stmt_parser(stmt_inner.clone(), expr.clone())
            .map(Stmt::While)
            .boxed(),
        // Do-while statements
        do_while_stmt_parser(stmt_inner.clone(), expr.clone())
            .map(Stmt::DoWhile)
            .boxed(),
        // Switch statements
        switch_stmt_parser(stmt_inner.clone(), expr.clone())
            .map(Stmt::Switch)
            .boxed(),
        // Try statements
        try_stmt_parser(stmt_inner.clone(), expr.clone())
            .map(Stmt::Try)
            .boxed(),
        // Throw statements
        throw_stmt_parser(expr.clone()).map(Stmt::Throw).boxed(),
        // Return statements
        return_stmt_parser(expr.clone()).map(Stmt::Return).boxed(),
        // Break statements
        break_stmt_parser().map(Stmt::Break).boxed(),
        // Continue statements
        continue_stmt_parser().map(Stmt::Continue).boxed(),
        // Debugger statements
        debugger_stmt_parser().map(Stmt::Debugger).boxed(),
        // Labeled statements
        labeled_stmt_parser(stmt_inner.clone())
            .map(Stmt::Labeled)
            .boxed(),
        // Block statements
        common::block_parser(stmt_inner.clone())
            .map(Stmt::Block)
            .boxed(),
        // Expression statements
        expr_stmt_parser(expr.clone()).map(Stmt::ExprStmt).boxed(),
    ))
    .padded()
}

/// Parser for export statements.
///
/// Supports:
/// - `export function ...`
/// - `export const/let ...`
/// - `export class ...`
/// - `export interface ...`
/// - `export enum ...`
/// - `export namespace ...`
pub fn export_stmt_parser<'a>(
    stmt: impl Parser<'a, &'a str, Stmt> + Clone + 'a,
    expr: impl Parser<'a, &'a str, Expr> + Clone + 'a,
) -> impl Parser<'a, &'a str, Stmt> + 'a {
    text::keyword("export")
        .padded()
        .ignore_then(choice((
            // Export function
            function::fn_decl_parser(stmt.clone())
                .map(Stmt::FnDecl)
                .boxed(),
            // Export class
            class_decl_parser(stmt.clone(), expr.clone())
                .map(Stmt::ClassDecl)
                .boxed(),
            // Export interface
            interface_decl_parser(stmt.clone())
                .map(Stmt::InterfaceDecl)
                .boxed(),
            // Export enum
            enum_decl_parser().map(Stmt::EnumDecl).boxed(),
            // Export namespace
            namespace_decl_parser(stmt.clone())
                .map(Stmt::NamespaceDecl)
                .boxed(),
            // Export variable
            var_decl_parser(expr.clone()).map(Stmt::VarDecl).boxed(),
        )))
        .labelled("export statement")
}

/// Parser for import statements: `import { ... } from "module"`
fn import_stmt_parser<'a>() -> impl Parser<'a, &'a str, ImportStmt> + 'a {
    text::keyword("import")
        .padded()
        .ignore_then(choice((
            // Named imports: { name1, name2 as alias2 }
            just("{")
                .padded()
                .ignore_then(
                    import_specifier_parser()
                        .separated_by(just(",").padded())
                        .allow_trailing()
                        .collect::<Vec<_>>(),
                )
                .then_ignore(just("}").padded()),
            // Default import: defaultName
            common::ident_parser().map(|ident| {
                vec![ImportSpecifier::Default {
                    local: ident,
                    span: 0..0,
                }]
            }),
            // Namespace import: * as name
            just("*")
                .padded()
                .ignore_then(text::keyword("as").padded())
                .ignore_then(common::ident_parser())
                .map(|ident| {
                    vec![ImportSpecifier::Namespace {
                        local: ident,
                        span: 0..0,
                    }]
                }),
        )))
        .then(text::keyword("from").padded())
        .then(choice((
            just('"')
                .ignore_then(string_content_parser('"'))
                .then_ignore(just('"')),
            just('\'')
                .ignore_then(string_content_parser('\''))
                .then_ignore(just('\'')),
        )))
        .then_ignore(just(";").padded())
        .map_with(|((specifiers, _), source), extra| ImportStmt {
            specifiers,
            source,
            span: extra.span().into(),
        })
        .labelled("import statement")
}

/// Parser for import specifiers: `name` or `name as alias`
fn import_specifier_parser<'a>() -> impl Parser<'a, &'a str, ImportSpecifier> + 'a {
    common::ident_parser()
        .then(
            text::keyword("as")
                .padded()
                .ignore_then(common::ident_parser())
                .or_not(),
        )
        .map_with(|(imported, local), extra| {
            if let Some(local) = local {
                ImportSpecifier::Named {
                    local,
                    imported: Some(imported),
                    span: extra.span().into(),
                }
            } else {
                ImportSpecifier::Named {
                    local: imported.clone(),
                    imported: None,
                    span: extra.span().into(),
                }
            }
        })
}

/// Helper for string content parsing (reused from expr)
fn string_content_parser<'a>(quote: char) -> impl Parser<'a, &'a str, String> + 'a {
    any()
        .filter(move |c: &char| *c != quote && *c != '\\')
        .or(just('\\').ignore_then(choice((
            just('n').to('\n'),
            just('t').to('\t'),
            just('r').to('\r'),
            just('\\').to('\\'),
            just('"').to('"'),
            just('\'').to('\''),
            just('0').to('\0'),
        ))))
        .repeated()
        .collect::<String>()
}

/// Parser for declare function statements: `declare function name(): type;`
fn declare_fn_parser<'a>() -> impl Parser<'a, &'a str, DeclareFn> + 'a {
    text::keyword("declare")
        .padded()
        .ignore_then(text::keyword("function").padded())
        .ignore_then(common::ident_parser())
        .then(common::param_list_parser())
        .then(
            just(":")
                .padded()
                .ignore_then(super::types::ts_type_parser()),
        )
        .then_ignore(just(";").padded())
        .map_with(|((ident, params), return_type), extra| DeclareFn {
            ident,
            params,
            return_type,
            span: extra.span().into(),
        })
        .labelled("declare function")
}

/// Parser for type alias: `type Name<T> = Type;`
fn type_alias_parser<'a>() -> impl Parser<'a, &'a str, TypeAlias> + 'a {
    text::keyword("type")
        .padded()
        .ignore_then(common::ident_parser())
        .then(
            // Type parameters: <T, U extends V = Default>
            just("<")
                .padded()
                .ignore_then(
                    type_param_parser()
                        .separated_by(just(",").padded())
                        .allow_trailing()
                        .collect::<Vec<_>>(),
                )
                .then_ignore(just(">").padded())
                .or_not(),
        )
        .then(
            just("=")
                .padded()
                .ignore_then(super::types::ts_type_parser()),
        )
        .then_ignore(just(";").padded())
        .map_with(|((ident, type_params), ty), extra| TypeAlias {
            ident,
            type_params,
            ty,
            span: extra.span().into(),
        })
        .labelled("type alias")
}

/// Parser for type parameters: `T extends U = V`
fn type_param_parser<'a>() -> impl Parser<'a, &'a str, TsTypeParam> + 'a {
    common::ident_parser()
        .then(
            text::keyword("extends")
                .padded()
                .ignore_then(super::types::ts_type_parser())
                .or_not(),
        )
        .then(
            just("=")
                .padded()
                .ignore_then(super::types::ts_type_parser())
                .or_not(),
        )
        .map_with(|((ident, constraint), default), extra| TsTypeParam {
            ident,
            constraint,
            default,
            span: extra.span().into(),
        })
}

/// Parser for interface declarations: `interface Name extends Base { members }`
fn interface_decl_parser<'a>(
    _stmt: impl Parser<'a, &'a str, Stmt> + Clone + 'a,
) -> impl Parser<'a, &'a str, InterfaceDecl> + 'a {
    text::keyword("interface")
        .padded()
        .ignore_then(common::ident_parser())
        .then(
            just("<")
                .padded()
                .ignore_then(
                    type_param_parser()
                        .separated_by(just(",").padded())
                        .allow_trailing()
                        .collect::<Vec<_>>(),
                )
                .then_ignore(just(">").padded())
                .or_not(),
        )
        .then(
            text::keyword("extends")
                .padded()
                .ignore_then(
                    // Interface extends uses TsTypeRef
                    common::ident_parser()
                        .map(|ident| TsTypeRef {
                            type_name: TsEntityName::Ident(ident.clone()),
                            type_params: None,
                            span: ident.span,
                        })
                        .separated_by(just(",").padded())
                        .allow_trailing()
                        .collect::<Vec<_>>(),
                )
                .or_not(),
        )
        .then(interface_body_parser())
        .map_with(
            |(((ident, type_params), extends), body), extra| InterfaceDecl {
                ident,
                type_params,
                extends: extends.unwrap_or_default(),
                body,
                span: extra.span().into(),
            },
        )
        .labelled("interface declaration")
}

/// Parser for interface body: `{ members }`
fn interface_body_parser<'a>() -> impl Parser<'a, &'a str, Vec<InterfaceMember>> + 'a {
    interface_member_parser()
        .separated_by(just(",").padded())
        .allow_trailing()
        .collect::<Vec<_>>()
        .delimited_by(just("{").padded(), just("}").padded())
        .labelled("interface body")
}

/// Parser for interface members
fn interface_member_parser<'a>() -> impl Parser<'a, &'a str, InterfaceMember> + 'a {
    choice((
        // Property: name: type or name?: type or readonly name: type
        text::keyword("readonly")
            .padded()
            .or_not()
            .then(common::ident_parser())
            .then(just("?").padded().or_not())
            .then(just(":").padded().ignore_then(super::types::ts_type_parser()))
            .then_ignore(just(";").padded().or_not())
            .map_with(|(((readonly, ident), optional), ty), extra| {
                InterfaceMember::Property(InterfaceProperty {
                    ident,
                    ty,
                    optional: optional.is_some(),
                    readonly: readonly.is_some(),
                    span: extra.span().into(),
                })
            }),
        // Method: name(params): type or name?(params): type
        common::ident_parser()
            .then(just("?").padded().or_not())
            .then(common::param_list_parser())
            .then(just(":").padded().ignore_then(super::types::ts_type_parser()))
            .then_ignore(just(";").padded().or_not())
            .map_with(|(((ident, optional), params), return_type), extra| {
                InterfaceMember::Method(InterfaceMethod {
                    ident,
                    params,
                    return_type,
                    optional: optional.is_some(),
                    span: extra.span().into(),
                })
            }),
        // Index signature: [key: type]: type
        just("[")
            .padded()
            .ignore_then(common::ident_parser())
            .then(just(":").padded().ignore_then(super::types::ts_type_parser()))
            .then_ignore(just("]").padded())
            .then(just(":").padded().ignore_then(super::types::ts_type_parser()))
            .then_ignore(just(";").padded().or_not())
            .map_with(|((key_name, key_type), value_type), extra| {
                InterfaceMember::IndexSignature(IndexSignature {
                    key_name,
                    key_type,
                    value_type,
                    readonly: false,
                    span: extra.span().into(),
                })
            }),
    ))
    .labelled("interface member")
}

/// Parser for enum declarations: `enum Name { Variant1, Variant2 }`
fn enum_decl_parser<'a>() -> impl Parser<'a, &'a str, EnumDecl> + 'a {
    text::keyword("enum")
        .padded()
        .ignore_then(common::ident_parser())
        .then(
            enum_member_parser()
                .separated_by(just(",").padded())
                .allow_trailing()
                .collect::<Vec<_>>()
                .delimited_by(just("{").padded(), just("}").padded()),
        )
        .map_with(|(ident, members), extra| EnumDecl {
            ident,
            members,
            span: extra.span().into(),
        })
        .labelled("enum declaration")
}

/// Parser for enum members: `Variant` or `Variant(Type1, Type2)`
fn enum_member_parser<'a>() -> impl Parser<'a, &'a str, EnumMember> + 'a {
    common::ident_parser()
        .then(
            // Optional tuple-like fields: (Type1, Type2)
            super::types::ts_type_parser()
                .separated_by(just(",").padded())
                .allow_trailing()
                .collect::<Vec<_>>()
                .delimited_by(just("(").padded(), just(")").padded())
                .or_not(),
        )
        .map_with(|(ident, fields), extra| EnumMember {
            ident,
            fields,
            span: extra.span().into(),
        })
}

/// Parser for namespace declarations: `namespace Name { body }`
fn namespace_decl_parser<'a>(
    stmt: impl Parser<'a, &'a str, Stmt> + Clone + 'a,
) -> impl Parser<'a, &'a str, NamespaceDecl> + 'a {
    text::keyword("namespace")
        .padded()
        .ignore_then(common::ident_parser())
        .then(
            stmt.repeated()
                .collect::<Vec<_>>()
                .delimited_by(just("{").padded(), just("}").padded()),
        )
        .map_with(|(ident, body), extra| NamespaceDecl {
            ident,
            body,
            span: extra.span().into(),
        })
        .labelled("namespace declaration")
}

/// Parser for class declarations: `class Name extends Base { members }`
fn class_decl_parser<'a>(
    stmt: impl Parser<'a, &'a str, Stmt> + Clone + 'a,
    expr: impl Parser<'a, &'a str, Expr> + Clone + 'a,
) -> impl Parser<'a, &'a str, ClassDecl> + 'a {
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
            // Class body: { members }
            class_member_parser(stmt.clone(), expr.clone())
                .repeated()
                .collect::<Vec<_>>()
                .delimited_by(just("{").padded(), just("}").padded()),
        )
        .map_with(|((ident, super_class), body), extra| ClassDecl {
            ident,
            super_class,
            body,
            span: extra.span().into(),
        })
        .labelled("class declaration")
}

/// Parser for class members
fn class_member_parser<'a>(
    stmt: impl Parser<'a, &'a str, Stmt> + Clone + 'a,
    _expr: impl Parser<'a, &'a str, Expr> + Clone + 'a,
) -> impl Parser<'a, &'a str, ClassMember> + 'a {
    choice((
        // Constructor: constructor(params) { body }
        text::keyword("constructor")
            .padded()
            .ignore_then(common::param_list_parser())
            .then(common::optional_block_parser(stmt.clone()))
            .map_with(|(params, body), extra| {
                ClassMember::Constructor(ConstructorDecl {
                    params,
                    body,
                    span: extra.span().into(),
                })
            }),
        // Method: name(params): type { body }
        // Also supports access modifiers: public, private, protected
        choice((
            text::keyword("public").padded().to(()),
            text::keyword("private").padded().to(()),
            text::keyword("protected").padded().to(()),
        ))
        .or_not()
        .ignore_then(common::ident_parser())
        .then(common::param_list_parser())
        .then(common::optional_type_annotation())
        .then(common::optional_block_parser(stmt.clone()))
        .map_with(|(((ident, params), return_type), body), extra| {
            ClassMember::Method(MethodDecl {
                ident,
                params,
                body,
                return_type,
                span: extra.span().into(),
            })
        }),
        // Field: name: type; or public/private name: type;
        choice((
            text::keyword("public").padded().to(()),
            text::keyword("private").padded().to(()),
            text::keyword("protected").padded().to(()),
        ))
        .or_not()
        .ignore_then(common::ident_parser())
        .then(common::optional_type_annotation())
        .then_ignore(just(";").padded())
        .map_with(|(ident, ty), extra| {
            ClassMember::Field(FieldDecl {
                ident,
                ty,
                span: extra.span().into(),
            })
        }),
    ))
}

/// Parser for variable declarations.
///
/// Supports:
/// - `let name: type = value;`
/// - `let mut name: type = value;`
/// - `const name: type = value;`
fn var_decl_parser<'a>(
    expr: impl Parser<'a, &'a str, Expr> + Clone + 'a,
) -> impl Parser<'a, &'a str, VarDecl> + 'a {
    let kind = choice((
        // `let mut`
        text::keyword("let")
            .padded()
            .ignore_then(text::keyword("mut").padded())
            .to(VarDeclKind::Let { mutable: true }),
        // `let`
        text::keyword("let")
            .padded()
            .to(VarDeclKind::Let { mutable: false }),
        // `const`
        text::keyword("const").padded().to(VarDeclKind::Const),
    ));

    kind.then(
        var_declarator_parser(expr.clone())
            .separated_by(just(",").padded())
            .allow_trailing()
            .collect::<Vec<_>>(),
    )
    .then_ignore(just(";").padded())
    .map_with(|(kind, decls), extra| VarDecl {
        kind,
        decls,
        span: extra.span().into(),
    })
    .labelled("variable declaration")
}

/// Parser for variable declarators: `name: type = value`
fn var_declarator_parser<'a>(
    expr: impl Parser<'a, &'a str, Expr> + Clone + 'a,
) -> impl Parser<'a, &'a str, VarDeclarator> + 'a {
    common::pat_parser()
        .then(common::optional_type_annotation())
        .then(just("=").padded().ignore_then(expr.clone()).or_not())
        .map_with(|((pat, ty), init), extra| VarDeclarator {
            name: pat,
            ty,
            init,
            span: extra.span().into(),
        })
        .labelled("variable declarator")
}

/// Parser for if statements: `if (expr) stmt else stmt`
fn if_stmt_parser<'a>(
    stmt: impl Parser<'a, &'a str, Stmt> + Clone + 'a,
    expr: impl Parser<'a, &'a str, Expr> + Clone + 'a,
) -> impl Parser<'a, &'a str, IfStmt> + 'a {
    text::keyword("if")
        .padded()
        .ignore_then(
            expr.clone()
                .delimited_by(just("(").padded(), just(")").padded()),
        )
        .then(stmt.clone().map(Box::new))
        .then(
            text::keyword("else")
                .padded()
                .ignore_then(stmt.clone().map(Box::new))
                .or_not(),
        )
        .map_with(|((test, cons), alt), extra| IfStmt {
            test,
            cons,
            alt,
            span: extra.span().into(),
        })
        .labelled("if statement")
}

/// Parser for for statements: `for (init; test; update) body`
fn for_stmt_parser<'a>(
    stmt: impl Parser<'a, &'a str, Stmt> + Clone + 'a,
    expr: impl Parser<'a, &'a str, Expr> + Clone + 'a,
) -> impl Parser<'a, &'a str, ForStmt> + 'a {
    text::keyword("for")
        .padded()
        .ignore_then(
            just("(")
                .padded()
                .ignore_then(
                    choice((
                        // Variable declaration
                        var_decl_parser(expr.clone()).map(ForInit::VarDecl),
                        // Expression (simplified - just parse an expression)
                        expr.clone().map(ForInit::Expr),
                    ))
                    .or_not(),
                )
                .then(expr.clone().or_not())
                .then_ignore(just(";").padded())
                .then(expr.clone().or_not())
                .then_ignore(just(")").padded()),
        )
        .then(stmt.clone().map(Box::new))
        .map_with(|(((init, test), update), body), extra| {
            let span: std::ops::Range<usize> = <std::ops::Range<usize>>::from(extra.span());
            ForStmt {
                init,
                test,
                update,
                body,
                span,
            }
        })
        .labelled("for statement")
}

/// Parser for for-in statements: `for (left in right) body`
fn for_in_stmt_parser<'a>(
    stmt: impl Parser<'a, &'a str, Stmt> + Clone + 'a,
    expr: impl Parser<'a, &'a str, Expr> + Clone + 'a,
) -> impl Parser<'a, &'a str, ForInStmt> + 'a {
    text::keyword("for")
        .padded()
        .ignore_then(
            just("(")
                .padded()
                .ignore_then(choice((
                    // Variable declaration
                    var_decl_parser(expr.clone()).map(ForHead::VarDecl),
                    // Pattern
                    common::pat_parser().map(ForHead::Pat),
                )))
                .then(text::keyword("in").padded().ignore_then(expr.clone()))
                .then_ignore(just(")").padded()),
        )
        .then(stmt.clone().map(Box::new))
        .map_with(|((left, right), body), extra| ForInStmt {
            left,
            right,
            body,
            span: extra.span().into(),
        })
        .labelled("for-in statement")
}

/// Parser for for-of statements: `for (left of right) body`
fn for_of_stmt_parser<'a>(
    stmt: impl Parser<'a, &'a str, Stmt> + Clone + 'a,
    expr: impl Parser<'a, &'a str, Expr> + Clone + 'a,
) -> impl Parser<'a, &'a str, ForOfStmt> + 'a {
    text::keyword("for")
        .padded()
        .ignore_then(
            just("(")
                .padded()
                .ignore_then(choice((
                    var_decl_parser(expr.clone()).map(ForHead::VarDecl),
                    common::pat_parser().map(ForHead::Pat),
                )))
                .then(text::keyword("of").padded().ignore_then(expr.clone()))
                .then_ignore(just(")").padded()),
        )
        .then(stmt.clone().map(Box::new))
        .map_with(|((left, right), body), extra| ForOfStmt {
            left,
            right,
            body,
            span: extra.span().into(),
        })
        .labelled("for-of statement")
}

/// Parser for while statements: `while (expr) body`
fn while_stmt_parser<'a>(
    stmt: impl Parser<'a, &'a str, Stmt> + Clone + 'a,
    expr: impl Parser<'a, &'a str, Expr> + Clone + 'a,
) -> impl Parser<'a, &'a str, WhileStmt> + 'a {
    text::keyword("while")
        .padded()
        .ignore_then(
            expr.clone()
                .delimited_by(just("(").padded(), just(")").padded()),
        )
        .then(stmt.clone().map(Box::new))
        .map_with(|(test, body), extra| WhileStmt {
            test,
            body,
            span: extra.span().into(),
        })
        .labelled("while statement")
}

/// Parser for do-while statements: `do body while (expr);`
fn do_while_stmt_parser<'a>(
    stmt: impl Parser<'a, &'a str, Stmt> + Clone + 'a,
    expr: impl Parser<'a, &'a str, Expr> + Clone + 'a,
) -> impl Parser<'a, &'a str, DoWhileStmt> + 'a {
    text::keyword("do")
        .padded()
        .ignore_then(stmt.clone().map(Box::new))
        .then(text::keyword("while").padded())
        .then(
            expr.clone()
                .delimited_by(just("(").padded(), just(")").padded()),
        )
        .then_ignore(just(";").padded())
        .map_with(|((body, _), test), extra| DoWhileStmt {
            body,
            test,
            span: extra.span().into(),
        })
        .labelled("do-while statement")
}

/// Parser for switch statements: `switch (expr) { cases }`
fn switch_stmt_parser<'a>(
    stmt: impl Parser<'a, &'a str, Stmt> + Clone + 'a,
    expr: impl Parser<'a, &'a str, Expr> + Clone + 'a,
) -> impl Parser<'a, &'a str, SwitchStmt> + 'a {
    text::keyword("switch")
        .padded()
        .ignore_then(
            expr.clone()
                .delimited_by(just("(").padded(), just(")").padded()),
        )
        .then(
            switch_case_parser(stmt.clone(), expr.clone())
                .repeated()
                .collect::<Vec<_>>()
                .delimited_by(just("{").padded(), just("}").padded()),
        )
        .map_with(|(discriminant, cases), extra| SwitchStmt {
            discriminant,
            cases,
            span: extra.span().into(),
        })
        .labelled("switch statement")
}

/// Parser for switch cases: `case expr: stmts` or `default: stmts`
fn switch_case_parser<'a>(
    stmt: impl Parser<'a, &'a str, Stmt> + Clone + 'a,
    expr: impl Parser<'a, &'a str, Expr> + Clone + 'a,
) -> impl Parser<'a, &'a str, SwitchCase> + 'a {
    choice((
        // Case with test
        text::keyword("case")
            .padded()
            .ignore_then(expr.clone())
            .then_ignore(just(":").padded())
            .then(stmt.clone().repeated().collect::<Vec<_>>())
            .map_with(|(test, cons), extra| SwitchCase {
                test: Some(test),
                cons,
                span: extra.span().into(),
            }),
        // Default case
        text::keyword("default")
            .padded()
            .then_ignore(just(":").padded())
            .then(stmt.clone().repeated().collect::<Vec<_>>())
            .map_with(|(_, cons), extra| SwitchCase {
                test: None,
                cons,
                span: extra.span().into(),
            }),
    ))
}

/// Parser for try statements: `try { } catch (e) { } finally { }`
fn try_stmt_parser<'a>(
    stmt: impl Parser<'a, &'a str, Stmt> + Clone + 'a,
    _expr: impl Parser<'a, &'a str, Expr> + Clone + 'a,
) -> impl Parser<'a, &'a str, TryStmt> + 'a {
    text::keyword("try")
        .padded()
        .ignore_then(common::block_parser(stmt.clone()))
        .then(
            text::keyword("catch")
                .padded()
                .ignore_then(
                    just("(")
                        .padded()
                        .ignore_then(common::pat_parser().or_not())
                        .then_ignore(just(")").padded()),
                )
                .then(common::block_parser(stmt.clone()))
                .map(|(param, body)| CatchClause {
                    param,
                    body,
                    span: 0..0,
                })
                .or_not(),
        )
        .then(
            text::keyword("finally")
                .padded()
                .ignore_then(common::block_parser(stmt.clone()))
                .or_not(),
        )
        .map_with(|((block, handler), finalizer), extra| TryStmt {
            block,
            handler,
            finalizer,
            span: extra.span().into(),
        })
        .labelled("try statement")
}

/// Parser for throw statements: `throw expr;`
fn throw_stmt_parser<'a>(
    expr: impl Parser<'a, &'a str, Expr> + Clone + 'a,
) -> impl Parser<'a, &'a str, ThrowStmt> + 'a {
    text::keyword("throw")
        .padded()
        .ignore_then(expr.clone())
        .then_ignore(just(";").padded())
        .map_with(|arg, extra| ThrowStmt {
            arg,
            span: extra.span().into(),
        })
        .labelled("throw statement")
}

/// Parser for return statements: `return expr;` or `return;`
fn return_stmt_parser<'a>(
    expr: impl Parser<'a, &'a str, Expr> + Clone + 'a,
) -> impl Parser<'a, &'a str, ReturnStmt> + 'a {
    text::keyword("return")
        .padded()
        .ignore_then(expr.clone().or_not())
        .then_ignore(just(";").padded())
        .map_with(|arg, extra| ReturnStmt {
            arg,
            span: extra.span().into(),
        })
        .labelled("return statement")
}

/// Parser for break statements: `break;` or `break label;`
fn break_stmt_parser<'a>() -> impl Parser<'a, &'a str, BreakStmt> + 'a {
    text::keyword("break")
        .padded()
        .ignore_then(common::ident_parser().or_not())
        .then_ignore(just(";").padded())
        .map_with(|label, extra| BreakStmt {
            label,
            span: extra.span().into(),
        })
        .labelled("break statement")
}

/// Parser for continue statements: `continue;` or `continue label;`
fn continue_stmt_parser<'a>() -> impl Parser<'a, &'a str, ContinueStmt> + 'a {
    text::keyword("continue")
        .padded()
        .ignore_then(common::ident_parser().or_not())
        .then_ignore(just(";").padded())
        .map_with(|label, extra| ContinueStmt {
            label,
            span: extra.span().into(),
        })
        .labelled("continue statement")
}

/// Parser for debugger statements: `debugger;`
fn debugger_stmt_parser<'a>() -> impl Parser<'a, &'a str, DebuggerStmt> + 'a {
    text::keyword("debugger")
        .padded()
        .then_ignore(just(";").padded())
        .map_with(|_, extra| {
            let span: std::ops::Range<usize> = <std::ops::Range<usize>>::from(extra.span());
            DebuggerStmt { span }
        })
        .labelled("debugger statement")
}

/// Parser for labeled statements: `label: stmt`
fn labeled_stmt_parser<'a>(
    stmt: impl Parser<'a, &'a str, Stmt> + Clone + 'a,
) -> impl Parser<'a, &'a str, LabeledStmt> + 'a {
    common::ident_parser()
        .then_ignore(just(":").padded())
        .then(stmt.clone().map(Box::new))
        .map_with(|(label, body), extra| LabeledStmt {
            label,
            body,
            span: extra.span().into(),
        })
        .labelled("labeled statement")
}

/// Parser for expression statements: `expr;`
fn expr_stmt_parser<'a>(
    expr: impl Parser<'a, &'a str, Expr> + Clone + 'a,
) -> impl Parser<'a, &'a str, ExprStmt> + 'a {
    expr.then_ignore(just(";").padded())
        .map_with(|expr, extra| ExprStmt {
            expr,
            span: extra.span().into(),
        })
        .labelled("expression statement")
}
