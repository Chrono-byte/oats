//! Statement parsers
//!
//! This module groups all statement parsing logic together for Locality of Behavior.
//! Statements are the top-level constructs in the language.

use super::class;
use super::common;
use super::function;
use chumsky::prelude::*;
use oats_ast::*;

/// Public API - creates stmt parser with internal expr parser
/// This is used internally by expr_parser, not directly
pub fn stmt_parser<'a>(
    expr: impl Parser<'a, &'a str, Expr> + Clone,
    stmt: impl Parser<'a, &'a str, Stmt> + Clone,
) -> impl Parser<'a, &'a str, Stmt> {
    stmt_parser_inner(expr, stmt)
}

/// Core statement parser implementation
pub fn stmt_parser_inner<'a>(
    expr: impl Parser<'a, &'a str, Expr> + Clone,
    stmt: impl Parser<'a, &'a str, Stmt> + Clone,
) -> impl Parser<'a, &'a str, Stmt> {
    // Split into two choices to avoid chumsky's 26-variant limit
    let first_choice = choice((
        import_stmt_parser().map(Stmt::Import),
        export_stmt_parser(stmt.clone(), expr.clone()),
        type_alias_parser().map(Stmt::TypeAlias),
        interface_decl_parser(stmt.clone()).map(Stmt::InterfaceDecl),
        enum_decl_parser().map(Stmt::EnumDecl),
        namespace_decl_parser(stmt.clone()).map(Stmt::NamespaceDecl),
        function::declare_fn_parser().map(Stmt::DeclareFn),
        class::class_decl_parser(expr.clone(), stmt.clone()).map(Stmt::ClassDecl),
        function::fn_decl_parser(stmt.clone()).map(Stmt::FnDecl),
        var_decl_parser(expr.clone()).map(Stmt::VarDecl),
        return_stmt_parser(expr.clone()).map(Stmt::Return),
        break_stmt_parser().map(Stmt::Break),
        continue_stmt_parser().map(Stmt::Continue),
    ));

    let second_choice = choice((
        if_stmt_parser(stmt.clone(), expr.clone()).map(Stmt::If),
        for_stmt_parser(stmt.clone(), expr.clone()).map(|for_stmt| Stmt::For(Box::new(for_stmt))),
        for_in_stmt_parser(stmt.clone(), expr.clone()).map(Stmt::ForIn),
        for_of_stmt_parser(stmt.clone(), expr.clone()).map(Stmt::ForOf),
        while_stmt_parser(stmt.clone(), expr.clone()).map(Stmt::While),
        do_while_stmt_parser(stmt.clone(), expr.clone()).map(Stmt::DoWhile),
        switch_stmt_parser(stmt.clone(), expr.clone()).map(Stmt::Switch),
        try_stmt_parser(stmt.clone()).map(Stmt::Try),
        throw_stmt_parser(expr.clone()).map(Stmt::Throw),
        debugger_stmt_parser().map(Stmt::Debugger),
        labeled_stmt_parser(stmt.clone()).map(Stmt::Labeled),
        common::block_parser(stmt.clone()).map(Stmt::Block),
        expr_stmt_parser(expr.clone()).map(Stmt::ExprStmt),
    ));

    choice((first_choice, second_choice))
}

/// Parser for import statements.
///
/// Patterns:
/// - `import { a, b as c } from "module";`
/// - `import * as ns from "module";`
/// - `import defaultName from "module";`
/// - `import defaultName, { a, b } from "module";`
pub fn import_stmt_parser<'a>() -> impl Parser<'a, &'a str, ImportStmt> {
    text::keyword("import")
        .padded()
        .ignore_then(choice((
            // Namespace import: import * as ns from "module"
            just("*")
                .padded()
                .ignore_then(text::keyword("as").padded())
                .ignore_then(common::ident_parser())
                .map_with(|local, extra| vec![ImportSpecifier::Namespace { local, span: extra.span().into() }]),
            // Default import: import defaultName from "module"
            common::ident_parser()
                .then(
                    just(",")
                        .padded()
                        .ignore_then(
                            // Named imports: { a, b as c }
                            import_named_specifiers(),
                        )
                        .or_not(),
                )
                .map_with(|(default, named), extra| {
                    let mut specifiers = vec![ImportSpecifier::Default {
                        local: default,
                        span: extra.span().into(),
                    }];
                    if let Some(mut named_specs) = named {
                        specifiers.append(&mut named_specs);
                    }
                    specifiers
                }),
            // Named imports only: import { a, b as c } from "module"
            import_named_specifiers(),
        )))
        .then(
            text::keyword("from").padded().ignore_then(
                // String literal for module path
                just("\"")
                    .ignore_then(any().filter(|c: &char| *c != '"').repeated().collect::<String>())
                    .then_ignore(just("\""))
                    .or(just('\'')
                        .ignore_then(any().filter(|c: &char| *c != '\'').repeated().collect::<String>())
                        .then_ignore(just('\''))),
            ),
        )
        .then_ignore(just(";").padded())
        .map_with(|(specifiers, source), extra| {
            // Update spans for specifiers (spans are already set correctly from the parsers)
            ImportStmt {
                specifiers,
                source,
                span: extra.span().into(),
            }
        })
}

/// Parser for named import specifiers.
///
/// Pattern: `{ a, b as c, ... }`
fn import_named_specifiers<'a>() -> impl Parser<'a, &'a str, Vec<ImportSpecifier>> {
    choice((
        // Named specifier: a or a as b
        common::ident_parser()
            .then(
                text::keyword("as")
                    .padded()
                    .ignore_then(common::ident_parser())
                    .or_not(),
            )
            .map_with(|(imported, local), extra| {
                let has_local = local.is_some();
                let imported_clone = imported.clone();
                let local_ident = local.unwrap_or(imported_clone);
                ImportSpecifier::Named {
                    local: local_ident,
                    imported: if has_local { Some(imported) } else { None },
                    span: extra.span().into(),
                }
            }),
    ))
    .separated_by(just(",").padded())
    .collect::<Vec<_>>()
    .delimited_by(just("{").padded(), just("}").padded())
}

/// Parser for export statements.
///
/// Pattern: `export function ...` or `export let ...`
pub fn export_stmt_parser<'a>(
    stmt: impl Parser<'a, &'a str, Stmt> + Clone,
    expr: impl Parser<'a, &'a str, Expr> + Clone,
) -> impl Parser<'a, &'a str, Stmt> {
    text::keyword("export").padded().ignore_then(choice((
        function::fn_decl_parser(stmt).map(Stmt::FnDecl),
        var_decl_parser(expr.clone()).map(Stmt::VarDecl),
    )))
}

/// Parser for variable declarations.
///
/// Pattern: `let|const name: type? = value?;`
pub fn var_decl_parser<'a>(
    expr: impl Parser<'a, &'a str, Expr> + Clone,
) -> impl Parser<'a, &'a str, VarDecl> {
    let kind = choice((
        text::keyword("let").map(|_| VarDeclKind::Let { mutable: false }),
        text::keyword("const").map(|_| VarDeclKind::Const),
    ))
    .padded();

    // Support "let mut" syntax
    let mut_kind = text::keyword("let")
        .padded()
        .ignore_then(text::keyword("mut").padded())
        .map(|_| VarDeclKind::Let { mutable: true });

    let kind = choice((mut_kind, kind));

    kind.then(
        var_declarator_parser(expr.clone())
            .separated_by(just(",").padded())
            .collect::<Vec<_>>(),
    )
    .then_ignore(just(";").padded())
    .map_with(|(kind, decls), extra| VarDecl { kind, decls, span: extra.span().into() })
}

/// Parser for variable declarators.
///
/// Pattern: `name: type? = value?`
fn var_declarator_parser<'a>(
    expr: impl Parser<'a, &'a str, Expr> + Clone,
) -> impl Parser<'a, &'a str, VarDeclarator> {
    common::pat_parser()
        .then(common::optional_type_annotation())
        .then(just("=").padded().ignore_then(expr.clone()).or_not())
        .map_with(|((pat, ty), init), extra| VarDeclarator {
            name: pat,
            ty,
            init,
            span: extra.span().into(),
        })
}

/// Parser for return statements.
///
/// Pattern: `return expr?;`
pub fn return_stmt_parser<'a>(
    expr: impl Parser<'a, &'a str, Expr> + Clone,
) -> impl Parser<'a, &'a str, ReturnStmt> {
    text::keyword("return")
        .padded()
        .ignore_then(expr.clone().or_not())
        .then_ignore(just(";").padded())
        .map_with(|arg, extra| ReturnStmt { arg, span: extra.span().into() })
}

/// Parser for break statements.
///
/// Pattern: `break label?;`
pub fn break_stmt_parser<'a>() -> impl Parser<'a, &'a str, BreakStmt> {
    text::keyword("break")
        .padded()
        .ignore_then(common::ident_parser().or_not())
        .then_ignore(just(";").padded())
        .map_with(|label, _extra| BreakStmt { label, span: _extra.span().into() })
}

/// Parser for continue statements.
///
/// Pattern: `continue label?;`
pub fn continue_stmt_parser<'a>() -> impl Parser<'a, &'a str, ContinueStmt> {
    text::keyword("continue")
        .padded()
        .ignore_then(common::ident_parser().or_not())
        .then_ignore(just(";").padded())
        .map_with(|label, _extra| ContinueStmt { label, span: _extra.span().into() })
}

/// Parser for if statements.
///
/// Pattern: `if (condition) stmt else stmt?`
pub fn if_stmt_parser<'a>(
    stmt: impl Parser<'a, &'a str, Stmt> + Clone,
    expr: impl Parser<'a, &'a str, Expr> + Clone,
) -> impl Parser<'a, &'a str, IfStmt> {
    text::keyword("if")
        .padded()
        .ignore_then(expr.clone().delimited_by(just("(").padded(), just(")").padded()))
        .then(stmt.clone().map(Box::new))
        .then(
            text::keyword("else")
                .padded()
                .ignore_then(stmt.map(Box::new))
                .or_not(),
        )
        .map_with(|((test, cons), alt), extra| IfStmt {
            test,
            cons,
            alt,
            span: extra.span().into(),
        })
}

/// Parser for for statements.
///
/// Pattern: `for (init?; test?; update?) stmt`
pub fn for_stmt_parser<'a>(
    stmt: impl Parser<'a, &'a str, Stmt> + Clone,
    expr: impl Parser<'a, &'a str, Expr> + Clone,
) -> impl Parser<'a, &'a str, ForStmt> {
    text::keyword("for")
        .padded()
        .ignore_then(just("(").padded())
        .ignore_then(
            choice((
                var_decl_parser(expr.clone()).map(ForInit::VarDecl),
                expr.clone().map(ForInit::Expr),
            ))
            .or_not(),
        )
        .then_ignore(just(";").padded())
        .then(expr.clone().or_not())
        .then_ignore(just(";").padded())
        .then(expr.clone().or_not())
        .then_ignore(just(")").padded())
        .then(stmt.map(Box::new))
        .map_with(|(((init, test), update), body), extra| ForStmt {
            init,
            test,
            update,
            body,
            span: extra.span().into(),
        })
}

/// Parser for while statements.
///
/// Pattern: `while (condition) stmt`
pub fn while_stmt_parser<'a>(
    stmt: impl Parser<'a, &'a str, Stmt> + Clone,
    expr: impl Parser<'a, &'a str, Expr> + Clone,
) -> impl Parser<'a, &'a str, WhileStmt> {
    text::keyword("while")
        .padded()
        .ignore_then(expr.clone().delimited_by(just("(").padded(), just(")").padded()))
        .then(stmt.map(Box::new))
        .map_with(|(test, body), extra| WhileStmt { test, body, span: extra.span().into() })
}

/// Parser for do-while statements.
///
/// Pattern: `do stmt while (condition);`
pub fn do_while_stmt_parser<'a>(
    stmt: impl Parser<'a, &'a str, Stmt> + Clone,
    expr: impl Parser<'a, &'a str, Expr> + Clone,
) -> impl Parser<'a, &'a str, DoWhileStmt> {
    text::keyword("do")
        .padded()
        .ignore_then(stmt.map(Box::new))
        .then(
            text::keyword("while").padded().ignore_then(
                expr.clone().delimited_by(just("(").padded(), just(")").padded()),
            ),
        )
        .then_ignore(just(";").padded())
        .map_with(|(body, test), extra| DoWhileStmt { body, test, span: extra.span().into() })
}

/// Parser for for-in statements.
///
/// Pattern: `for (left in right) stmt`
pub fn for_in_stmt_parser<'a>(
    stmt: impl Parser<'a, &'a str, Stmt> + Clone,
    expr: impl Parser<'a, &'a str, Expr> + Clone,
) -> impl Parser<'a, &'a str, ForInStmt> {
    text::keyword("for")
        .padded()
        .ignore_then(just("(").padded())
        .ignore_then(choice((
            var_decl_parser(expr.clone()).map(ForHead::VarDecl),
            common::pat_parser().map(ForHead::Pat),
        )))
        .then_ignore(text::keyword("in").padded())
        .then(expr.clone())
        .then_ignore(just(")").padded())
        .then(stmt.map(Box::new))
        .map_with(|((left, right), body), extra| ForInStmt {
            left,
            right,
            body,
            span: extra.span().into(),
        })
}

/// Parser for for-of statements.
///
/// Pattern: `for (left of right) stmt`
pub fn for_of_stmt_parser<'a>(
    stmt: impl Parser<'a, &'a str, Stmt> + Clone,
    expr: impl Parser<'a, &'a str, Expr> + Clone,
) -> impl Parser<'a, &'a str, ForOfStmt> {
    text::keyword("for")
        .padded()
        .ignore_then(just("(").padded())
        .ignore_then(choice((
            var_decl_parser(expr.clone()).map(ForHead::VarDecl),
            common::pat_parser().map(ForHead::Pat),
        )))
        .then_ignore(text::keyword("of").padded())
        .then(expr.clone())
        .then_ignore(just(")").padded())
        .then(stmt.map(Box::new))
        .map_with(|((left, right), body), extra| ForOfStmt {
            left,
            right,
            body,
            span: extra.span().into(),
        })
}

/// Parser for switch statements.
///
/// Pattern: `switch (expr) { case ... default: ... }`
pub fn switch_stmt_parser<'a>(
    stmt: impl Parser<'a, &'a str, Stmt> + Clone,
    expr: impl Parser<'a, &'a str, Expr> + Clone,
) -> impl Parser<'a, &'a str, SwitchStmt> {
    text::keyword("switch")
        .padded()
        .ignore_then(expr.clone().delimited_by(just("(").padded(), just(")").padded()))
        .then(
            just("{")
                .padded()
                .ignore_then(
                    switch_case_parser(stmt.clone(), expr.clone())
                        .repeated()
                        .collect::<Vec<_>>(),
                )
                .then_ignore(just("}").padded()),
        )
        .map_with(|(discriminant, cases), extra| SwitchStmt {
            discriminant,
            cases,
            span: extra.span().into(),
        })
}

/// Parser for switch cases.
///
/// Pattern: `case expr: stmts` or `default: stmts`
fn switch_case_parser<'a>(
    stmt: impl Parser<'a, &'a str, Stmt> + Clone,
    expr: impl Parser<'a, &'a str, Expr> + Clone,
) -> impl Parser<'a, &'a str, SwitchCase> {
    choice((
        text::keyword("case")
            .padded()
            .ignore_then(expr.clone())
            .then_ignore(just(":").padded())
            .map(Some),
        text::keyword("default")
            .padded()
            .then_ignore(just(":").padded())
            .map(|_| None),
    ))
    .then(stmt.repeated().collect::<Vec<_>>())
    .map_with(|(test, cons), extra| SwitchCase { test, cons, span: extra.span().into() })
}

/// Parser for try statements.
///
/// Pattern: `try { } catch (e?) { } finally { }?`
pub fn try_stmt_parser<'a>(
    stmt: impl Parser<'a, &'a str, Stmt> + Clone,
) -> impl Parser<'a, &'a str, TryStmt> {
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
                .map_with(|(param, body), extra| CatchClause { param, body, span: extra.span().into() })
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
}

/// Parser for throw statements.
///
/// Pattern: `throw expr;`
pub fn throw_stmt_parser<'a>(
    expr: impl Parser<'a, &'a str, Expr> + Clone,
) -> impl Parser<'a, &'a str, ThrowStmt> {
    text::keyword("throw")
        .padded()
        .ignore_then(expr.clone())
        .then_ignore(just(";").padded())
        .map_with(|arg, extra| ThrowStmt { arg, span: extra.span().into() })
}

/// Parser for debugger statements.
///
/// Pattern: `debugger;`
pub fn debugger_stmt_parser<'a>() -> impl Parser<'a, &'a str, DebuggerStmt> {
    text::keyword("debugger")
        .padded()
        .then_ignore(just(";").padded())
        .map_with(|_, extra| DebuggerStmt { span: <std::ops::Range<usize>>::from(extra.span()) })
}

/// Parser for labeled statements.
///
/// Pattern: `label: stmt`
pub fn labeled_stmt_parser<'a>(
    stmt: impl Parser<'a, &'a str, Stmt> + Clone,
) -> impl Parser<'a, &'a str, LabeledStmt> {
    common::ident_parser()
        .then_ignore(just(":").padded())
        .then(stmt.map(Box::new))
        .map_with(|(label, body), extra| LabeledStmt { label, body, span: extra.span().into() })
}

/// Parser for expression statements.
///
/// Pattern: `expr;`
pub fn expr_stmt_parser<'a>(
    expr: impl Parser<'a, &'a str, Expr> + Clone,
) -> impl Parser<'a, &'a str, ExprStmt> {
    expr
        .then_ignore(just(";").padded())
        .map_with(|expr, extra| ExprStmt { expr, span: extra.span().into() })
}

/// Parser for type alias declarations.
///
/// Pattern: `type Name<T?> = Type;`
pub fn type_alias_parser<'a>() -> impl Parser<'a, &'a str, TypeAlias> {
    text::keyword("type")
        .padded()
        .ignore_then(common::ident_parser())
        .then(
            // Type parameters: <T, U extends V = Default>
            just("<")
                .padded()
                .ignore_then(
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
                        .separated_by(just(",").padded())
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
}

/// Parser for interface declarations.
///
/// Pattern: `interface Name<T?> extends Base? { members }`
pub fn interface_decl_parser<'a>(
    stmt: impl Parser<'a, &'a str, Stmt> + Clone,
) -> impl Parser<'a, &'a str, InterfaceDecl> {
    text::keyword("interface")
        .padded()
        .ignore_then(common::ident_parser())
        .then(
            // Type parameters
            just("<")
                .padded()
                .ignore_then(
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
                        .separated_by(just(",").padded())
                        .collect::<Vec<_>>(),
                )
                .then_ignore(just(">").padded())
                .or_not(),
        )
        .then(
            text::keyword("extends")
                .padded()
                .ignore_then(
                    super::types::ts_type_parser()
                        .separated_by(just(",").padded())
                        .collect::<Vec<_>>(),
                )
                .or_not(),
        )
        .then(interface_body_parser(stmt))
        .map_with(|(((ident, type_params), extends), body), extra| {
            // Convert extends types to TsTypeRef (simplified - assumes they're type refs)
            let extends_refs = extends
                .unwrap_or_default()
                .into_iter()
                .filter_map(|ty| {
                    if let TsType::TsTypeRef(tr) = ty {
                        Some(tr)
                    } else {
                        None
                    }
                })
                .collect();
            InterfaceDecl {
                ident,
                type_params,
                extends: extends_refs,
                body,
                span: extra.span().into(),
            }
        })
}

/// Parser for interface body.
fn interface_body_parser<'a>(
    stmt: impl Parser<'a, &'a str, Stmt> + Clone,
) -> impl Parser<'a, &'a str, Vec<InterfaceMember>> {
    interface_member_parser(stmt)
        .repeated()
        .collect::<Vec<_>>()
        .delimited_by(just("{").padded(), just("}").padded())
}

/// Parser for interface members.
fn interface_member_parser<'a>(
    _stmt: impl Parser<'a, &'a str, Stmt> + Clone,
) -> impl Parser<'a, &'a str, InterfaceMember> {
    choice((
        // Index signature: [key: string]: type
        just("[")
            .padded()
            .ignore_then(common::ident_parser())
            .then(
                just(":")
                    .padded()
                    .ignore_then(super::types::ts_type_parser()),
            )
            .then_ignore(just("]").padded())
            .then(
                just(":")
                    .padded()
                    .ignore_then(super::types::ts_type_parser()),
            )
            .then(
                text::keyword("readonly")
                    .padded()
                    .to(true)
                    .or_not()
                    .map(|opt| opt.unwrap_or(false)),
            )
            .then_ignore(just(";").padded())
            .map_with(|(((key_name, key_type), value_type), readonly), extra| {
                InterfaceMember::IndexSignature(IndexSignature {
                    key_name,
                    key_type,
                    value_type,
                    readonly,
                    span: extra.span().into(),
                })
            }),
        // Method: name(params): returnType
        common::ident_parser()
            .then(just("?").padded().or_not())
            .then(common::param_list_parser())
            .then(
                just(":")
                    .padded()
                    .ignore_then(super::types::ts_type_parser()),
            )
            .then_ignore(just(";").padded())
            .map_with(|(((ident, optional), params), return_type), extra| {
                InterfaceMember::Method(InterfaceMethod {
                    ident,
                    params,
                    return_type,
                    optional: optional.is_some(),
                    span: extra.span().into(),
                })
            }),
        // Property: readonly? name?: type
        text::keyword("readonly")
            .padded()
            .or_not()
            .then(common::ident_parser())
            .then(just("?").padded().or_not())
            .then(
                just(":")
                    .padded()
                    .ignore_then(super::types::ts_type_parser()),
            )
            .then_ignore(just(";").padded())
            .map_with(|(((readonly, ident), optional), ty), extra| {
                InterfaceMember::Property(InterfaceProperty {
                    ident,
                    ty,
                    optional: optional.is_some(),
                    readonly: readonly.is_some(),
                    span: extra.span().into(),
                })
            }),
    ))
}

/// Parser for enum declarations.
///
/// Pattern: `enum Name { Member = value?, ... }`
pub fn enum_decl_parser<'a>() -> impl Parser<'a, &'a str, EnumDecl> {
    text::keyword("enum")
        .padded()
        .ignore_then(common::ident_parser())
        .then(
            enum_member_parser()
                .separated_by(just(",").padded())
                .collect::<Vec<_>>()
                .delimited_by(just("{").padded(), just("}").padded()),
        )
        .map_with(|(ident, members), extra| EnumDecl {
            ident,
            members,
            span: extra.span().into(),
        })
}

/// Parser for enum members (Rust-like variants).
///
/// Supports:
/// - `VariantName` - unit variant with no data
/// - `VariantName(Type1, Type2, ...)` - tuple-like variant with associated data
fn enum_member_parser<'a>() -> impl Parser<'a, &'a str, EnumMember> {
    use super::types;

    common::ident_parser()
        .then(
            // Optional tuple of types: (Type1, Type2, ...)
            types::ts_type_parser()
                .separated_by(just(",").padded())
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

/// Parser for namespace declarations.
///
/// Pattern: `namespace Name { body }`
pub fn namespace_decl_parser<'a>(
    stmt: impl Parser<'a, &'a str, Stmt> + Clone,
) -> impl Parser<'a, &'a str, NamespaceDecl> {
    text::keyword("namespace")
        .padded()
        .ignore_then(common::ident_parser())
        .then(common::block_parser(stmt))
        .map_with(|(ident, body), extra| NamespaceDecl {
            ident,
            body: body.stmts,
            span: extra.span().into(),
        })
}
