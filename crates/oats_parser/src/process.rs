//! Process model expression parsers
//!
//! This module provides parsers for process-related expressions:
//! spawn, send, receive, link, monitor, exit, self, whereis, register, unregister

use chumsky::prelude::*;
use oats_ast::*;

/// Parser for spawn expression: spawn() or spawn(name, priority)
pub fn spawn_expr_parser(
    expr: impl Parser<char, Expr, Error = Simple<char>> + Clone,
) -> impl Parser<char, Expr, Error = Simple<char>> {
    text::keyword("spawn")
        .padded()
        .ignore_then(
            expr.clone()
                .separated_by(just(',').padded())
                .delimited_by(just('(').padded(), just(')').padded())
                .map(|args| {
                    let name = args.get(0).cloned();
                    let priority = args.get(1).cloned();
                    (name, priority)
                })
                .or(just('(').padded().ignore_then(just(')').padded()).map(|_| (None, None))),
        )
        .map_with_span(|(name, priority), span| {
            Expr::Spawn(SpawnExpr {
                name: name.map(Box::new),
                priority: priority.map(Box::new),
                span,
            })
        })
}

/// Parser for send expression: send(to, message)
pub fn send_expr_parser(
    expr: impl Parser<char, Expr, Error = Simple<char>> + Clone,
) -> impl Parser<char, Expr, Error = Simple<char>> {
    text::keyword("send")
        .padded()
        .ignore_then(
            expr.clone()
                .separated_by(just(',').padded())
                .delimited_by(just('(').padded(), just(')').padded())
                .collect::<Vec<_>>()
                .try_map(|args, span| {
                    if args.len() == 2 {
                        Ok((args[0].clone(), args[1].clone()))
                    } else {
                        Err(Simple::custom(span, "send requires exactly 2 arguments"))
                    }
                }),
        )
        .map_with_span(|(to, message), span| {
            Expr::Send(SendExpr {
                to: Box::new(to),
                message: Box::new(message),
                span,
            })
        })
}

/// Parser for receive expression: receive() or receive(type_id)
pub fn receive_expr_parser(
    expr: impl Parser<char, Expr, Error = Simple<char>> + Clone,
) -> impl Parser<char, Expr, Error = Simple<char>> {
    text::keyword("receive")
        .padded()
        .ignore_then(
            just('(')
                .padded()
                .ignore_then(
                    expr.clone()
                        .map(Some)
                        .or_not()
                        .then_ignore(just(')').padded()),
                ),
        )
        .map_with_span(|type_id, span| {
            Expr::Receive(ReceiveExpr {
                type_id: type_id.flatten().map(Box::new),
                span,
            })
        })
}

/// Parser for process self expression: self()
pub fn process_self_expr_parser() -> impl Parser<char, Expr, Error = Simple<char>> {
    text::keyword("self")
        .padded()
        .ignore_then(just('(').padded().ignore_then(just(')').padded()))
        .map_with_span(|_, span| Expr::ProcessSelf(ProcessSelfExpr { span }))
}

/// Parser for process exit expression: exit() or exit(reason) or exit(pid, reason)
pub fn process_exit_expr_parser(
    expr: impl Parser<char, Expr, Error = Simple<char>> + Clone,
) -> impl Parser<char, Expr, Error = Simple<char>> {
    text::keyword("exit")
        .padded()
        .ignore_then(
            expr.clone()
                .separated_by(just(',').padded())
                .delimited_by(just('(').padded(), just(')').padded())
                .collect::<Vec<_>>()
                .map(|args| {
                    if args.len() == 1 {
                        (None, Some(args[0].clone()))
                    } else if args.len() == 2 {
                        (Some(args[0].clone()), Some(args[1].clone()))
                    } else {
                        (None, None)
                    }
                })
                .or(just('(').padded().ignore_then(just(')').padded()).map(|_| (None, None))),
        )
        .map_with_span(|(pid, reason), span| {
            Expr::ProcessExit(ProcessExitExpr {
                pid: pid.map(Box::new),
                reason: reason.map(Box::new),
                span,
            })
        })
}

/// Parser for process link expression: link(pid1, pid2)
pub fn process_link_expr_parser(
    expr: impl Parser<char, Expr, Error = Simple<char>> + Clone,
) -> impl Parser<char, Expr, Error = Simple<char>> {
    text::keyword("link")
        .padded()
        .ignore_then(
            expr.clone()
                .separated_by(just(',').padded())
                .delimited_by(just('(').padded(), just(')').padded())
                .collect::<Vec<_>>()
                .try_map(|args, span| {
                    if args.len() == 2 {
                        Ok((args[0].clone(), args[1].clone()))
                    } else {
                        Err(Simple::custom(span, "link requires exactly 2 arguments"))
                    }
                }),
        )
        .map_with_span(|(pid1, pid2), span| {
            Expr::ProcessLink(ProcessLinkExpr {
                pid1: Box::new(pid1),
                pid2: Box::new(pid2),
                span,
            })
        })
}

/// Parser for process unlink expression: unlink(pid1, pid2)
pub fn process_unlink_expr_parser(
    expr: impl Parser<char, Expr, Error = Simple<char>> + Clone,
) -> impl Parser<char, Expr, Error = Simple<char>> {
    text::keyword("unlink")
        .padded()
        .ignore_then(
            expr.clone()
                .separated_by(just(',').padded())
                .delimited_by(just('(').padded(), just(')').padded())
                .collect::<Vec<_>>()
                .try_map(|args, span| {
                    if args.len() == 2 {
                        Ok((args[0].clone(), args[1].clone()))
                    } else {
                        Err(Simple::custom(span, "unlink requires exactly 2 arguments"))
                    }
                }),
        )
        .map_with_span(|(pid1, pid2), span| {
            Expr::ProcessUnlink(ProcessUnlinkExpr {
                pid1: Box::new(pid1),
                pid2: Box::new(pid2),
                span,
            })
        })
}

/// Parser for process monitor expression: monitor(target)
pub fn process_monitor_expr_parser(
    expr: impl Parser<char, Expr, Error = Simple<char>> + Clone,
) -> impl Parser<char, Expr, Error = Simple<char>> {
    text::keyword("monitor")
        .padded()
        .ignore_then(
            just('(')
                .padded()
                .ignore_then(expr.clone())
                .then_ignore(just(')').padded()),
        )
        .map_with_span(|target, span| {
            Expr::ProcessMonitor(ProcessMonitorExpr {
                target: Box::new(target),
                span,
            })
        })
}

/// Parser for process demonitor expression: demonitor(monitor_ref)
pub fn process_demonitor_expr_parser(
    expr: impl Parser<char, Expr, Error = Simple<char>> + Clone,
) -> impl Parser<char, Expr, Error = Simple<char>> {
    text::keyword("demonitor")
        .padded()
        .ignore_then(
            just('(')
                .padded()
                .ignore_then(expr.clone())
                .then_ignore(just(')').padded()),
        )
        .map_with_span(|monitor_ref, span| {
            Expr::ProcessDemonitor(ProcessDemonitorExpr {
                monitor_ref: Box::new(monitor_ref),
                span,
            })
        })
}

/// Parser for process whereis expression: whereis(name)
pub fn process_whereis_expr_parser(
    expr: impl Parser<char, Expr, Error = Simple<char>> + Clone,
) -> impl Parser<char, Expr, Error = Simple<char>> {
    text::keyword("whereis")
        .padded()
        .ignore_then(
            just('(')
                .padded()
                .ignore_then(expr.clone())
                .then_ignore(just(')').padded()),
        )
        .map_with_span(|name, span| {
            Expr::ProcessWhereis(ProcessWhereisExpr {
                name: Box::new(name),
                span,
            })
        })
}

/// Parser for process register expression: register(pid, name)
pub fn process_register_expr_parser(
    expr: impl Parser<char, Expr, Error = Simple<char>> + Clone,
) -> impl Parser<char, Expr, Error = Simple<char>> {
    text::keyword("register")
        .padded()
        .ignore_then(
            expr.clone()
                .separated_by(just(',').padded())
                .delimited_by(just('(').padded(), just(')').padded())
                .collect::<Vec<_>>()
                .try_map(|args, span| {
                    if args.len() == 2 {
                        Ok((args[0].clone(), args[1].clone()))
                    } else {
                        Err(Simple::custom(span, "register requires exactly 2 arguments"))
                    }
                }),
        )
        .map_with_span(|(pid, name), span| {
            Expr::ProcessRegister(ProcessRegisterExpr {
                pid: Box::new(pid),
                name: Box::new(name),
                span,
            })
        })
}

/// Parser for process unregister expression: unregister(name)
pub fn process_unregister_expr_parser(
    expr: impl Parser<char, Expr, Error = Simple<char>> + Clone,
) -> impl Parser<char, Expr, Error = Simple<char>> {
    text::keyword("unregister")
        .padded()
        .ignore_then(
            just('(')
                .padded()
                .ignore_then(expr.clone())
                .then_ignore(just(')').padded()),
        )
        .map_with_span(|name, span| {
            Expr::ProcessUnregister(ProcessUnregisterExpr {
                name: Box::new(name),
                span,
            })
        })
}
