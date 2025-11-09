//! Process model expression parsers
//!
//! This module provides parsers for process-related expressions:
//! spawn, send, receive, link, monitor, exit, self, whereis, register, unregister

use chumsky::prelude::*;
use oats_ast::*;

/// Parser for spawn expression: spawn() or spawn(name, priority)
pub fn spawn_expr_parser<'a>(
    expr: impl Parser<'a, &'a str, Expr> + Clone,
) -> impl Parser<'a, &'a str, Expr> {
    text::keyword("spawn")
        .padded()
        .ignore_then(
            expr.clone()
                .separated_by(just(",").padded())
                .collect::<Vec<_>>()
                .delimited_by(just("(").padded(), just(")").padded())
                .map(|args: Vec<_>| {
                    let name = args.get(0).cloned();
                    let priority = args.get(1).cloned();
                    (name, priority)
                })
                .or(just("(").padded().ignore_then(just(")").padded()).map(|_| (None, None))),
        )
        .map_with(|(name, priority), extra| {
            Expr::Spawn(SpawnExpr {
                name: name.map(Box::new),
                priority: priority.map(Box::new),
                span: extra.span().into(),
            })
        })
}

/// Parser for send expression: send(to, message)
pub fn send_expr_parser<'a>(
    expr: impl Parser<'a, &'a str, Expr> + Clone,
) -> impl Parser<'a, &'a str, Expr> {
    text::keyword("send")
        .padded()
        .ignore_then(
            expr.clone()
                .then_ignore(just(",").padded())
                .then(expr.clone())
                .delimited_by(just("(").padded(), just(")").padded()),
        )
        .map_with(|(to, message), extra| {
            Expr::Send(SendExpr {
                to: Box::new(to),
                message: Box::new(message),
                span: extra.span().into(),
            })
        })
}

/// Parser for receive expression: receive() or receive(type_id)
pub fn receive_expr_parser<'a>(
    expr: impl Parser<'a, &'a str, Expr> + Clone,
) -> impl Parser<'a, &'a str, Expr> {
    text::keyword("receive")
        .padded()
        .ignore_then(
            just("(")
                .padded()
                .ignore_then(
                    expr.clone()
                        .map(Some)
                        .or_not()
                        .then_ignore(just(")").padded()),
                ),
        )
        .map_with(|type_id, extra| {
            Expr::Receive(ReceiveExpr {
                type_id: type_id.flatten().map(Box::new),
                span: extra.span().into(),
            })
        })
}

/// Parser for process self expression: self()
pub fn process_self_expr_parser<'a>() -> impl Parser<'a, &'a str, Expr> {
    text::keyword("self")
        .padded()
        .ignore_then(just("(").padded().ignore_then(just(")").padded()))
        .map_with(|_, extra| Expr::ProcessSelf(ProcessSelfExpr { span: <std::ops::Range<usize>>::from(extra.span()) }))
}

/// Parser for process exit expression: exit() or exit(reason) or exit(pid, reason)
pub fn process_exit_expr_parser<'a>(
    expr: impl Parser<'a, &'a str, Expr> + Clone,
) -> impl Parser<'a, &'a str, Expr> {
    text::keyword("exit")
        .padded()
        .ignore_then(
            expr.clone()
                .separated_by(just(",").padded())
                .collect::<Vec<_>>()
                .delimited_by(just("(").padded(), just(")").padded())
                .map(|args| {
                    if args.len() == 1 {
                        (None, Some(args[0].clone()))
                    } else if args.len() == 2 {
                        (Some(args[0].clone()), Some(args[1].clone()))
                    } else {
                        (None, None)
                    }
                })
                .or(just("(").padded().ignore_then(just(")").padded()).map(|_| (None, None))),
        )
        .map_with(|(pid, reason), extra| {
            Expr::ProcessExit(ProcessExitExpr {
                pid: pid.map(Box::new),
                reason: reason.map(Box::new),
                span: extra.span().into(),
            })
        })
}

/// Parser for process link expression: link(pid1, pid2)
pub fn process_link_expr_parser<'a>(
    expr: impl Parser<'a, &'a str, Expr> + Clone,
) -> impl Parser<'a, &'a str, Expr> {
    text::keyword("link")
        .padded()
        .ignore_then(
            expr.clone()
                .then_ignore(just(",").padded())
                .then(expr.clone())
                .delimited_by(just("(").padded(), just(")").padded()),
        )
        .map_with(|(pid1, pid2), extra| {
            Expr::ProcessLink(ProcessLinkExpr {
                pid1: Box::new(pid1),
                pid2: Box::new(pid2),
                span: extra.span().into(),
            })
        })
}

/// Parser for process unlink expression: unlink(pid1, pid2)
pub fn process_unlink_expr_parser<'a>(
    expr: impl Parser<'a, &'a str, Expr> + Clone,
) -> impl Parser<'a, &'a str, Expr> {
    text::keyword("unlink")
        .padded()
        .ignore_then(
            expr.clone()
                .then_ignore(just(",").padded())
                .then(expr.clone())
                .delimited_by(just("(").padded(), just(")").padded()),
        )
        .map_with(|(pid1, pid2), extra| {
            Expr::ProcessUnlink(ProcessUnlinkExpr {
                pid1: Box::new(pid1),
                pid2: Box::new(pid2),
                span: extra.span().into(),
            })
        })
}

/// Parser for process monitor expression: monitor(target)
pub fn process_monitor_expr_parser<'a>(
    expr: impl Parser<'a, &'a str, Expr> + Clone,
) -> impl Parser<'a, &'a str, Expr> {
    text::keyword("monitor")
        .padded()
        .ignore_then(
            just("(")
                .padded()
                .ignore_then(expr.clone())
                .then_ignore(just(")").padded()),
        )
        .map_with(|target, extra| {
            Expr::ProcessMonitor(ProcessMonitorExpr {
                target: Box::new(target),
                span: extra.span().into(),
            })
        })
}

/// Parser for process demonitor expression: demonitor(monitor_ref)
pub fn process_demonitor_expr_parser<'a>(
    expr: impl Parser<'a, &'a str, Expr> + Clone,
) -> impl Parser<'a, &'a str, Expr> {
    text::keyword("demonitor")
        .padded()
        .ignore_then(
            just("(")
                .padded()
                .ignore_then(expr.clone())
                .then_ignore(just(")").padded()),
        )
        .map_with(|monitor_ref, extra| {
            Expr::ProcessDemonitor(ProcessDemonitorExpr {
                monitor_ref: Box::new(monitor_ref),
                span: extra.span().into(),
            })
        })
}

/// Parser for process whereis expression: whereis(name)
pub fn process_whereis_expr_parser<'a>(
    expr: impl Parser<'a, &'a str, Expr> + Clone,
) -> impl Parser<'a, &'a str, Expr> {
    text::keyword("whereis")
        .padded()
        .ignore_then(
            just("(")
                .padded()
                .ignore_then(expr.clone())
                .then_ignore(just(")").padded()),
        )
        .map_with(|name, extra| {
            Expr::ProcessWhereis(ProcessWhereisExpr {
                name: Box::new(name),
                span: extra.span().into(),
            })
        })
}

/// Parser for process register expression: register(pid, name)
pub fn process_register_expr_parser<'a>(
    expr: impl Parser<'a, &'a str, Expr> + Clone,
) -> impl Parser<'a, &'a str, Expr> {
    text::keyword("register")
        .padded()
        .ignore_then(
            expr.clone()
                .then_ignore(just(",").padded())
                .then(expr.clone())
                .delimited_by(just("(").padded(), just(")").padded()),
        )
        .map_with(|(pid, name), extra| {
            Expr::ProcessRegister(ProcessRegisterExpr {
                pid: Box::new(pid),
                name: Box::new(name),
                span: extra.span().into(),
            })
        })
}

/// Parser for process unregister expression: unregister(name)
pub fn process_unregister_expr_parser<'a>(
    expr: impl Parser<'a, &'a str, Expr> + Clone,
) -> impl Parser<'a, &'a str, Expr> {
    text::keyword("unregister")
        .padded()
        .ignore_then(
            just("(")
                .padded()
                .ignore_then(expr.clone())
                .then_ignore(just(")").padded()),
        )
        .map_with(|name, extra| {
            Expr::ProcessUnregister(ProcessUnregisterExpr {
                name: Box::new(name),
                span: extra.span().into(),
            })
        })
}
