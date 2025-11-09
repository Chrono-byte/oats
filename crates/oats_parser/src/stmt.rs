//! Statement parsers
//!
//! This module groups all statement parsing logic together for Locality of Behavior.
//! Statements are the top-level constructs in the language.

use super::class;
use super::common;
use super::expr;
use super::function;
use chumsky::prelude::*;
use oats_ast::*;

/// Parser for statements.
///
/// This is the main dispatcher that routes to specific statement parsers.
pub fn stmt_parser(
    stmt: impl Parser<char, Stmt, Error = Simple<char>> + Clone,
) -> impl Parser<char, Stmt, Error = Simple<char>> {
    // Split into two choices to avoid chumsky's 26-variant limit
    let first_choice = choice((
        import_stmt_parser().map(Stmt::Import),
        export_stmt_parser(stmt.clone()),
        type_alias_parser().map(Stmt::TypeAlias),
        interface_decl_parser(stmt.clone()).map(Stmt::InterfaceDecl),
        enum_decl_parser().map(Stmt::EnumDecl),
        namespace_decl_parser(stmt.clone()).map(Stmt::NamespaceDecl),
        function::declare_fn_parser().map(Stmt::DeclareFn),
        class::class_decl_parser(stmt.clone()).map(Stmt::ClassDecl),
        function::fn_decl_parser(stmt.clone()).map(Stmt::FnDecl),
        var_decl_parser().map(Stmt::VarDecl),
        return_stmt_parser().map(Stmt::Return),
        break_stmt_parser().map(Stmt::Break),
        continue_stmt_parser().map(Stmt::Continue),
    ));

    let second_choice = choice((
        if_stmt_parser(stmt.clone()).map(Stmt::If),
        for_stmt_parser(stmt.clone()).map(|for_stmt| Stmt::For(Box::new(for_stmt))),
        for_in_stmt_parser(stmt.clone()).map(Stmt::ForIn),
        for_of_stmt_parser(stmt.clone()).map(Stmt::ForOf),
        while_stmt_parser(stmt.clone()).map(Stmt::While),
        do_while_stmt_parser(stmt.clone()).map(Stmt::DoWhile),
        switch_stmt_parser(stmt.clone()).map(Stmt::Switch),
        try_stmt_parser(stmt.clone()).map(Stmt::Try),
        throw_stmt_parser().map(Stmt::Throw),
        debugger_stmt_parser().map(Stmt::Debugger),
        labeled_stmt_parser(stmt.clone()).map(Stmt::Labeled),
        common::block_parser(stmt.clone()).map(Stmt::Block),
        expr_stmt_parser().map(Stmt::ExprStmt),
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
pub fn import_stmt_parser() -> impl Parser<char, ImportStmt, Error = Simple<char>> {
    text::keyword("import")
        .padded()
        .ignore_then(choice((
            // Namespace import: import * as ns from "module"
            just('*')
                .padded()
                .ignore_then(text::keyword("as").padded())
                .ignore_then(common::ident_parser())
                .map_with_span(|local, span| vec![ImportSpecifier::Namespace { local, span }]),
            // Default import: import defaultName from "module"
            common::ident_parser()
                .then(
                    just(',')
                        .padded()
                        .ignore_then(
                            // Named imports: { a, b as c }
                            import_named_specifiers(),
                        )
                        .or_not(),
                )
                .map_with_span(|(default, named), span| {
                    let mut specifiers = vec![ImportSpecifier::Default {
                        local: default,
                        span,
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
                just('"')
                    .ignore_then(filter(|c| *c != '"').repeated().collect::<String>())
                    .then_ignore(just('"'))
                    .or(just('\'')
                        .ignore_then(filter(|c| *c != '\'').repeated().collect::<String>())
                        .then_ignore(just('\''))),
            ),
        )
        .then_ignore(just(';').padded())
        .map_with_span(|(specifiers, source), span| {
            // Update spans for specifiers (spans are already set correctly from the parsers)
            ImportStmt {
                specifiers,
                source,
                span,
            }
        })
}

/// Parser for named import specifiers.
///
/// Pattern: `{ a, b as c, ... }`
fn import_named_specifiers() -> impl Parser<char, Vec<ImportSpecifier>, Error = Simple<char>> {
    choice((
        // Named specifier: a or a as b
        common::ident_parser()
            .then(
                text::keyword("as")
                    .padded()
                    .ignore_then(common::ident_parser())
                    .or_not(),
            )
            .map_with_span(|(imported, local), span| {
                let has_local = local.is_some();
                let imported_clone = imported.clone();
                let local_ident = local.unwrap_or(imported_clone);
                ImportSpecifier::Named {
                    local: local_ident,
                    imported: if has_local { Some(imported) } else { None },
                    span,
                }
            }),
    ))
    .separated_by(just(',').padded())
    .collect::<Vec<_>>()
    .delimited_by(just('{').padded(), just('}').padded())
}

/// Parser for export statements.
///
/// Pattern: `export function ...` or `export let ...`
pub fn export_stmt_parser(
    stmt: impl Parser<char, Stmt, Error = Simple<char>> + Clone,
) -> impl Parser<char, Stmt, Error = Simple<char>> {
    text::keyword("export").padded().ignore_then(choice((
        function::fn_decl_parser(stmt).map(Stmt::FnDecl),
        var_decl_parser().map(Stmt::VarDecl),
    )))
}

/// Parser for variable declarations.
///
/// Pattern: `let|const name: type? = value?;`
pub fn var_decl_parser() -> impl Parser<char, VarDecl, Error = Simple<char>> {
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
        var_declarator_parser()
            .separated_by(just(',').padded())
            .collect::<Vec<_>>(),
    )
    .then_ignore(just(';').padded())
    .map_with_span(|(kind, decls), span| VarDecl { kind, decls, span })
}

/// Parser for variable declarators.
///
/// Pattern: `name: type? = value?`
fn var_declarator_parser() -> impl Parser<char, VarDeclarator, Error = Simple<char>> {
    common::pat_parser()
        .then(common::optional_type_annotation())
        .then(just('=').padded().ignore_then(expr::expr_parser()).or_not())
        .map_with_span(|((pat, ty), init), span| VarDeclarator {
            name: pat,
            ty,
            init,
            span,
        })
}

/// Parser for return statements.
///
/// Pattern: `return expr?;`
pub fn return_stmt_parser() -> impl Parser<char, ReturnStmt, Error = Simple<char>> {
    text::keyword("return")
        .padded()
        .ignore_then(expr::expr_parser().or_not())
        .then_ignore(just(';').padded())
        .map_with_span(|arg, span| ReturnStmt { arg, span })
}

/// Parser for break statements.
///
/// Pattern: `break label?;`
pub fn break_stmt_parser() -> impl Parser<char, BreakStmt, Error = Simple<char>> {
    text::keyword("break")
        .padded()
        .ignore_then(common::ident_parser().or_not())
        .then_ignore(just(';').padded())
        .map_with_span(|label, _span| BreakStmt { label, span: _span })
}

/// Parser for continue statements.
///
/// Pattern: `continue label?;`
pub fn continue_stmt_parser() -> impl Parser<char, ContinueStmt, Error = Simple<char>> {
    text::keyword("continue")
        .padded()
        .ignore_then(common::ident_parser().or_not())
        .then_ignore(just(';').padded())
        .map_with_span(|label, _span| ContinueStmt { label, span: _span })
}

/// Parser for if statements.
///
/// Pattern: `if (condition) stmt else stmt?`
pub fn if_stmt_parser(
    stmt: impl Parser<char, Stmt, Error = Simple<char>> + Clone,
) -> impl Parser<char, IfStmt, Error = Simple<char>> {
    text::keyword("if")
        .padded()
        .ignore_then(expr::expr_parser().delimited_by(just('(').padded(), just(')').padded()))
        .then(stmt.clone().map(Box::new))
        .then(
            text::keyword("else")
                .padded()
                .ignore_then(stmt.map(Box::new))
                .or_not(),
        )
        .map_with_span(|((test, cons), alt), span| IfStmt {
            test,
            cons,
            alt,
            span,
        })
}

/// Parser for for statements.
///
/// Pattern: `for (init?; test?; update?) stmt`
pub fn for_stmt_parser(
    stmt: impl Parser<char, Stmt, Error = Simple<char>> + Clone,
) -> impl Parser<char, ForStmt, Error = Simple<char>> {
    text::keyword("for")
        .padded()
        .ignore_then(just('(').padded())
        .ignore_then(
            choice((
                var_decl_parser().map(ForInit::VarDecl),
                expr::expr_parser().map(ForInit::Expr),
            ))
            .or_not(),
        )
        .then_ignore(just(';').padded())
        .then(expr::expr_parser().or_not())
        .then_ignore(just(';').padded())
        .then(expr::expr_parser().or_not())
        .then_ignore(just(')').padded())
        .then(stmt.map(Box::new))
        .map_with_span(|(((init, test), update), body), span| ForStmt {
            init,
            test,
            update,
            body,
            span,
        })
}

/// Parser for while statements.
///
/// Pattern: `while (condition) stmt`
pub fn while_stmt_parser(
    stmt: impl Parser<char, Stmt, Error = Simple<char>> + Clone,
) -> impl Parser<char, WhileStmt, Error = Simple<char>> {
    text::keyword("while")
        .padded()
        .ignore_then(expr::expr_parser().delimited_by(just('(').padded(), just(')').padded()))
        .then(stmt.map(Box::new))
        .map_with_span(|(test, body), span| WhileStmt { test, body, span })
}

/// Parser for do-while statements.
///
/// Pattern: `do stmt while (condition);`
pub fn do_while_stmt_parser(
    stmt: impl Parser<char, Stmt, Error = Simple<char>> + Clone,
) -> impl Parser<char, DoWhileStmt, Error = Simple<char>> {
    text::keyword("do")
        .padded()
        .ignore_then(stmt.map(Box::new))
        .then(
            text::keyword("while").padded().ignore_then(
                expr::expr_parser().delimited_by(just('(').padded(), just(')').padded()),
            ),
        )
        .then_ignore(just(';').padded())
        .map_with_span(|(body, test), span| DoWhileStmt { body, test, span })
}

/// Parser for for-in statements.
///
/// Pattern: `for (left in right) stmt`
pub fn for_in_stmt_parser(
    stmt: impl Parser<char, Stmt, Error = Simple<char>> + Clone,
) -> impl Parser<char, ForInStmt, Error = Simple<char>> {
    text::keyword("for")
        .padded()
        .ignore_then(just('(').padded())
        .ignore_then(choice((
            var_decl_parser().map(ForHead::VarDecl),
            common::pat_parser().map(ForHead::Pat),
        )))
        .then_ignore(text::keyword("in").padded())
        .then(expr::expr_parser())
        .then_ignore(just(')').padded())
        .then(stmt.map(Box::new))
        .map_with_span(|((left, right), body), span| ForInStmt {
            left,
            right,
            body,
            span,
        })
}

/// Parser for for-of statements.
///
/// Pattern: `for (left of right) stmt`
pub fn for_of_stmt_parser(
    stmt: impl Parser<char, Stmt, Error = Simple<char>> + Clone,
) -> impl Parser<char, ForOfStmt, Error = Simple<char>> {
    text::keyword("for")
        .padded()
        .ignore_then(just('(').padded())
        .ignore_then(choice((
            var_decl_parser().map(ForHead::VarDecl),
            common::pat_parser().map(ForHead::Pat),
        )))
        .then_ignore(text::keyword("of").padded())
        .then(expr::expr_parser())
        .then_ignore(just(')').padded())
        .then(stmt.map(Box::new))
        .map_with_span(|((left, right), body), span| ForOfStmt {
            left,
            right,
            body,
            span,
        })
}

/// Parser for switch statements.
///
/// Pattern: `switch (expr) { case ... default: ... }`
pub fn switch_stmt_parser(
    stmt: impl Parser<char, Stmt, Error = Simple<char>> + Clone,
) -> impl Parser<char, SwitchStmt, Error = Simple<char>> {
    text::keyword("switch")
        .padded()
        .ignore_then(expr::expr_parser().delimited_by(just('(').padded(), just(')').padded()))
        .then(
            just('{')
                .padded()
                .ignore_then(
                    switch_case_parser(stmt.clone())
                        .repeated()
                        .collect::<Vec<_>>(),
                )
                .then_ignore(just('}').padded()),
        )
        .map_with_span(|(discriminant, cases), span| SwitchStmt {
            discriminant,
            cases,
            span,
        })
}

/// Parser for switch cases.
///
/// Pattern: `case expr: stmts` or `default: stmts`
fn switch_case_parser(
    stmt: impl Parser<char, Stmt, Error = Simple<char>> + Clone,
) -> impl Parser<char, SwitchCase, Error = Simple<char>> {
    choice((
        text::keyword("case")
            .padded()
            .ignore_then(expr::expr_parser())
            .then_ignore(just(':').padded())
            .map(Some),
        text::keyword("default")
            .padded()
            .then_ignore(just(':').padded())
            .map(|_| None),
    ))
    .then(stmt.repeated().collect::<Vec<_>>())
    .map_with_span(|(test, cons), span| SwitchCase { test, cons, span })
}

/// Parser for try statements.
///
/// Pattern: `try { } catch (e?) { } finally { }?`
pub fn try_stmt_parser(
    stmt: impl Parser<char, Stmt, Error = Simple<char>> + Clone,
) -> impl Parser<char, TryStmt, Error = Simple<char>> {
    text::keyword("try")
        .padded()
        .ignore_then(common::block_parser(stmt.clone()))
        .then(
            text::keyword("catch")
                .padded()
                .ignore_then(
                    just('(')
                        .padded()
                        .ignore_then(common::pat_parser().or_not())
                        .then_ignore(just(')').padded()),
                )
                .then(common::block_parser(stmt.clone()))
                .map_with_span(|(param, body), span| CatchClause { param, body, span })
                .or_not(),
        )
        .then(
            text::keyword("finally")
                .padded()
                .ignore_then(common::block_parser(stmt.clone()))
                .or_not(),
        )
        .map_with_span(|((block, handler), finalizer), span| TryStmt {
            block,
            handler,
            finalizer,
            span,
        })
}

/// Parser for throw statements.
///
/// Pattern: `throw expr;`
pub fn throw_stmt_parser() -> impl Parser<char, ThrowStmt, Error = Simple<char>> {
    text::keyword("throw")
        .padded()
        .ignore_then(expr::expr_parser())
        .then_ignore(just(';').padded())
        .map_with_span(|arg, span| ThrowStmt { arg, span })
}

/// Parser for debugger statements.
///
/// Pattern: `debugger;`
pub fn debugger_stmt_parser() -> impl Parser<char, DebuggerStmt, Error = Simple<char>> {
    text::keyword("debugger")
        .padded()
        .then_ignore(just(';').padded())
        .map_with_span(|_, span: std::ops::Range<usize>| DebuggerStmt { span })
}

/// Parser for labeled statements.
///
/// Pattern: `label: stmt`
pub fn labeled_stmt_parser(
    stmt: impl Parser<char, Stmt, Error = Simple<char>> + Clone,
) -> impl Parser<char, LabeledStmt, Error = Simple<char>> {
    common::ident_parser()
        .then_ignore(just(':').padded())
        .then(stmt.map(Box::new))
        .map_with_span(|(label, body), span| LabeledStmt { label, body, span })
}

/// Parser for expression statements.
///
/// Pattern: `expr;`
pub fn expr_stmt_parser() -> impl Parser<char, ExprStmt, Error = Simple<char>> {
    expr::expr_parser()
        .then_ignore(just(';').padded())
        .map_with_span(|expr, span| ExprStmt { expr, span })
}

/// Parser for type alias declarations.
///
/// Pattern: `type Name<T?> = Type;`
pub fn type_alias_parser() -> impl Parser<char, TypeAlias, Error = Simple<char>> {
    text::keyword("type")
        .padded()
        .ignore_then(common::ident_parser())
        .then(
            // Type parameters: <T, U extends V = Default>
            just('<')
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
                            just('=')
                                .padded()
                                .ignore_then(super::types::ts_type_parser())
                                .or_not(),
                        )
                        .map_with_span(|((ident, constraint), default), span| TsTypeParam {
                            ident,
                            constraint,
                            default,
                            span,
                        })
                        .separated_by(just(',').padded())
                        .collect::<Vec<_>>(),
                )
                .then_ignore(just('>').padded())
                .or_not(),
        )
        .then(
            just('=')
                .padded()
                .ignore_then(super::types::ts_type_parser()),
        )
        .then_ignore(just(';').padded())
        .map_with_span(|((ident, type_params), ty), span| TypeAlias {
            ident,
            type_params,
            ty,
            span,
        })
}

/// Parser for interface declarations.
///
/// Pattern: `interface Name<T?> extends Base? { members }`
pub fn interface_decl_parser(
    stmt: impl Parser<char, Stmt, Error = Simple<char>> + Clone,
) -> impl Parser<char, InterfaceDecl, Error = Simple<char>> {
    text::keyword("interface")
        .padded()
        .ignore_then(common::ident_parser())
        .then(
            // Type parameters
            just('<')
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
                            just('=')
                                .padded()
                                .ignore_then(super::types::ts_type_parser())
                                .or_not(),
                        )
                        .map_with_span(|((ident, constraint), default), span| TsTypeParam {
                            ident,
                            constraint,
                            default,
                            span,
                        })
                        .separated_by(just(',').padded())
                        .collect::<Vec<_>>(),
                )
                .then_ignore(just('>').padded())
                .or_not(),
        )
        .then(
            text::keyword("extends")
                .padded()
                .ignore_then(
                    super::types::ts_type_parser()
                        .separated_by(just(',').padded())
                        .collect::<Vec<_>>(),
                )
                .or_not(),
        )
        .then(interface_body_parser(stmt))
        .map_with_span(|(((ident, type_params), extends), body), span| {
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
                span,
            }
        })
}

/// Parser for interface body.
fn interface_body_parser(
    stmt: impl Parser<char, Stmt, Error = Simple<char>> + Clone,
) -> impl Parser<char, Vec<InterfaceMember>, Error = Simple<char>> {
    interface_member_parser(stmt)
        .repeated()
        .collect::<Vec<_>>()
        .delimited_by(just('{').padded(), just('}').padded())
}

/// Parser for interface members.
fn interface_member_parser(
    _stmt: impl Parser<char, Stmt, Error = Simple<char>> + Clone,
) -> impl Parser<char, InterfaceMember, Error = Simple<char>> {
    choice((
        // Index signature: [key: string]: type
        just('[')
            .padded()
            .ignore_then(common::ident_parser())
            .then(
                just(':')
                    .padded()
                    .ignore_then(super::types::ts_type_parser()),
            )
            .then_ignore(just(']').padded())
            .then(
                just(':')
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
            .then_ignore(just(';').padded())
            .map_with_span(|(((key_name, key_type), value_type), readonly), span| {
                InterfaceMember::IndexSignature(IndexSignature {
                    key_name,
                    key_type,
                    value_type,
                    readonly,
                    span,
                })
            }),
        // Method: name(params): returnType
        common::ident_parser()
            .then(just('?').padded().or_not())
            .then(common::param_list_parser())
            .then(
                just(':')
                    .padded()
                    .ignore_then(super::types::ts_type_parser()),
            )
            .then_ignore(just(';').padded())
            .map_with_span(|(((ident, optional), params), return_type), span| {
                InterfaceMember::Method(InterfaceMethod {
                    ident,
                    params,
                    return_type,
                    optional: optional.is_some(),
                    span,
                })
            }),
        // Property: readonly? name?: type
        text::keyword("readonly")
            .padded()
            .or_not()
            .then(common::ident_parser())
            .then(just('?').padded().or_not())
            .then(
                just(':')
                    .padded()
                    .ignore_then(super::types::ts_type_parser()),
            )
            .then_ignore(just(';').padded())
            .map_with_span(|(((readonly, ident), optional), ty), span| {
                InterfaceMember::Property(InterfaceProperty {
                    ident,
                    ty,
                    optional: optional.is_some(),
                    readonly: readonly.is_some(),
                    span,
                })
            }),
    ))
}

/// Parser for enum declarations.
///
/// Pattern: `enum Name { Member = value?, ... }`
pub fn enum_decl_parser() -> impl Parser<char, EnumDecl, Error = Simple<char>> {
    text::keyword("enum")
        .padded()
        .ignore_then(common::ident_parser())
        .then(
            enum_member_parser()
                .separated_by(just(',').padded())
                .collect::<Vec<_>>()
                .delimited_by(just('{').padded(), just('}').padded()),
        )
        .map_with_span(|(ident, members), span| EnumDecl {
            ident,
            members,
            span,
        })
}

/// Parser for enum members (Rust-like variants).
///
/// Supports:
/// - `VariantName` - unit variant with no data
/// - `VariantName(Type1, Type2, ...)` - tuple-like variant with associated data
fn enum_member_parser() -> impl Parser<char, EnumMember, Error = Simple<char>> {
    use super::types;

    common::ident_parser()
        .then(
            // Optional tuple of types: (Type1, Type2, ...)
            types::ts_type_parser()
                .separated_by(just(',').padded())
                .collect::<Vec<_>>()
                .delimited_by(just('(').padded(), just(')').padded())
                .or_not(),
        )
        .map_with_span(|(ident, fields), span| EnumMember {
            ident,
            fields,
            span
        })
}

/// Parser for namespace declarations.
///
/// Pattern: `namespace Name { body }`
pub fn namespace_decl_parser(
    stmt: impl Parser<char, Stmt, Error = Simple<char>> + Clone,
) -> impl Parser<char, NamespaceDecl, Error = Simple<char>> {
    text::keyword("namespace")
        .padded()
        .ignore_then(common::ident_parser())
        .then(common::block_parser(stmt))
        .map_with_span(|(ident, body), span| NamespaceDecl {
            ident,
            body: body.stmts,
            span,
        })
}
