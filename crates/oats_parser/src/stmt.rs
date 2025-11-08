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
    choice((
        export_stmt_parser(stmt.clone()),
        function::declare_fn_parser().map(Stmt::DeclareFn),
        class::class_decl_parser(stmt.clone()).map(Stmt::ClassDecl),
        function::fn_decl_parser(stmt.clone()).map(Stmt::FnDecl),
        var_decl_parser().map(Stmt::VarDecl),
        return_stmt_parser().map(Stmt::Return),
        break_stmt_parser().map(Stmt::Break),
        continue_stmt_parser().map(Stmt::Continue),
        if_stmt_parser(stmt.clone()).map(Stmt::If),
        for_stmt_parser(stmt.clone()).map(Stmt::For),
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
    ))
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
/// Pattern: `let|const|var|let mut name: type? = value?;`
pub fn var_decl_parser() -> impl Parser<char, VarDecl, Error = Simple<char>> {
    let kind = choice((
        text::keyword("let").map(|_| VarDeclKind::Let),
        text::keyword("const").map(|_| VarDeclKind::Const),
        text::keyword("var").map(|_| VarDeclKind::Var),
    ))
    .padded();

    // Support "let mut" syntax
    let mut_kind = text::keyword("let")
        .padded()
        .ignore_then(text::keyword("mut").padded())
        .map(|_| VarDeclKind::Let);

    let kind = choice((mut_kind, kind));

    kind.then(
        var_declarator_parser()
            .separated_by(just(',').padded())
            .collect::<Vec<_>>(),
    )
    .then_ignore(just(';').padded())
    .map_with_span(|(kind, decls), span| VarDecl {
        kind,
        decls,
        span: span.into(),
    })
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
            span: span.into(),
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
        .map_with_span(|arg, span| ReturnStmt {
            arg,
            span: span.into(),
        })
}

/// Parser for break statements.
///
/// Pattern: `break label?;`
pub fn break_stmt_parser() -> impl Parser<char, BreakStmt, Error = Simple<char>> {
    text::keyword("break")
        .padded()
        .ignore_then(common::ident_parser().or_not())
        .then_ignore(just(';').padded())
        .map_with_span(|label, _span| BreakStmt {
            label,
            span: _span.into(),
        })
}

/// Parser for continue statements.
///
/// Pattern: `continue label?;`
pub fn continue_stmt_parser() -> impl Parser<char, ContinueStmt, Error = Simple<char>> {
    text::keyword("continue")
        .padded()
        .ignore_then(common::ident_parser().or_not())
        .then_ignore(just(';').padded())
        .map_with_span(|label, _span| ContinueStmt {
            label,
            span: _span.into(),
        })
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
            span: span.into(),
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
            span: span.into(),
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
        .map_with_span(|(test, body), span| WhileStmt {
            test,
            body,
            span: span.into(),
        })
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
        .map_with_span(|(body, test), span| DoWhileStmt {
            body,
            test,
            span: span.into(),
        })
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
            span: span.into(),
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
            span: span.into(),
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
            span: span.into(),
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
    .map_with_span(|(test, cons), span| SwitchCase {
        test,
        cons,
        span: span.into(),
    })
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
                .map_with_span(|(param, body), span| CatchClause {
                    param,
                    body,
                    span: span.into(),
                })
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
            span: span.into(),
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
        .map_with_span(|arg, span| ThrowStmt {
            arg,
            span: span.into(),
        })
}

/// Parser for debugger statements.
///
/// Pattern: `debugger;`
pub fn debugger_stmt_parser() -> impl Parser<char, DebuggerStmt, Error = Simple<char>> {
    text::keyword("debugger")
        .padded()
        .then_ignore(just(';').padded())
        .map_with_span(|_, span: std::ops::Range<usize>| DebuggerStmt { span: span.into() })
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
        .map_with_span(|(label, body), span| LabeledStmt {
            label,
            body,
            span: span.into(),
        })
}

/// Parser for expression statements.
///
/// Pattern: `expr;`
pub fn expr_stmt_parser() -> impl Parser<char, ExprStmt, Error = Simple<char>> {
    expr::expr_parser()
        .then_ignore(just(';').padded())
        .map_with_span(|expr, span| ExprStmt {
            expr,
            span: span.into(),
        })
}
