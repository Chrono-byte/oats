//! Expression parsers with proper operator precedence

use super::common;
use chumsky::prelude::*;
use oats_ast::*;

/// Parser limits to prevent stack overflow from pathological inputs.
/// These are conservative caps on operator chain lengths.
const MAX_OPERATOR_CHAIN_LENGTH: usize = 1024;

/// Expression parser with proper precedence handling.
///
/// Uses recursive descent with precedence levels.
pub fn expr_parser<'a>(
    expr: impl Parser<'a, &'a str, Expr> + Clone + 'a, // <-- Add this `expr` placeholder
    stmt: impl Parser<'a, &'a str, Stmt> + Clone + 'a,
) -> impl Parser<'a, &'a str, Expr> + 'a {
    // The body is now just the implementation, NOT the recursive call
    assignment_expr_parser(expr, stmt)
}

/// Assignment expressions: `left = right`, `left += right`, etc.
fn assignment_expr_parser<'a>(
    expr: impl Parser<'a, &'a str, Expr> + Clone + 'a,
    stmt: impl Parser<'a, &'a str, Stmt> + Clone + 'a,
) -> impl Parser<'a, &'a str, Expr> + 'a {
    conditional_expr_parser(expr.clone(), stmt.clone())
        .then(
            choice((
                just("=").padded().to(AssignOp::Eq),
                just("+=").padded().to(AssignOp::PlusEq),
                just("-=").padded().to(AssignOp::MinusEq),
                just("*=").padded().to(AssignOp::MulEq),
                just("/=").padded().to(AssignOp::DivEq),
                just("%=").padded().to(AssignOp::ModEq),
                just("<<=").padded().to(AssignOp::LShiftEq),
                just(">>=").padded().to(AssignOp::RShiftEq),
                just(">>>=").padded().to(AssignOp::URShiftEq),
                just("&=").padded().to(AssignOp::BitwiseAndEq),
                just("|=").padded().to(AssignOp::BitwiseOrEq),
                just("^=").padded().to(AssignOp::BitwiseXorEq),
                just("**=").padded().to(AssignOp::ExpEq),
            ))
            .then(expr.clone())
            .or_not(),
        )
        .map_with(|(left, op_and_right), extra| {
            if let Some((op, right)) = op_and_right {
                // Convert left to AssignTarget
                let target = match left {
                    Expr::Ident(ident) => AssignTarget::Pat(Pat::Ident(ident)),
                    Expr::Member(member) => AssignTarget::Member(member),
                    Expr::OptionalMember(opt_member) => {
                        // Optional member can't be assigned, but parse it anyway
                        AssignTarget::Member(MemberExpr {
                            obj: opt_member.obj,
                            prop: opt_member.prop,
                            span: opt_member.span,
                        })
                    }
                    _ => {
                        // For other expressions, create a dummy target
                        // This should ideally be an error, but we'll handle it gracefully
                        AssignTarget::Pat(Pat::Ident(Ident {
                            sym: String::new(),
                            span: extra.span().into(),
                        }))
                    }
                };
                Expr::Assign(AssignExpr {
                    op,
                    left: target,
                    right: Box::new(right),
                    span: extra.span().into(),
                })
            } else {
                left
            }
        })
        .labelled("assignment expression")
}

/// Conditional expressions: `test ? cons : alt`
fn conditional_expr_parser<'a>(
    expr: impl Parser<'a, &'a str, Expr> + Clone + 'a,
    stmt: impl Parser<'a, &'a str, Stmt> + Clone + 'a,
) -> impl Parser<'a, &'a str, Expr> + 'a {
    logical_or_expr_parser(expr.clone(), stmt.clone())
        .then(
            just("?")
                .padded()
                .ignore_then(expr.clone())
                .then_ignore(just(":").padded())
                .then(expr.clone())
                .or_not(),
        )
        .map_with(|(test, cons_and_alt), extra| {
            if let Some((cons, alt)) = cons_and_alt {
                Expr::Cond(CondExpr {
                    test: Box::new(test),
                    cons: Box::new(cons),
                    alt: Box::new(alt),
                    span: extra.span().into(),
                })
            } else {
                test
            }
        })
        .labelled("conditional expression")
}

/// Logical OR expressions: `left || right` or `left ?? right`
fn logical_or_expr_parser<'a>(
    expr: impl Parser<'a, &'a str, Expr> + Clone + 'a,
    stmt: impl Parser<'a, &'a str, Stmt> + Clone + 'a,
) -> impl Parser<'a, &'a str, Expr> + 'a {
    logical_and_expr_parser(expr.clone(), stmt.clone())
        .then(
            choice((
                just("||").padded().to(BinaryOp::Or),
                just("??").padded().to(BinaryOp::NullishCoalesce),
            ))
            .then(logical_and_expr_parser(expr.clone(), stmt.clone()))
            .repeated()
            .at_most(MAX_OPERATOR_CHAIN_LENGTH)
            .collect::<Vec<_>>(),
        )
        .map_with(|(mut left, ops), extra| {
            let span: std::ops::Range<usize> = extra.span().into();
            for (op, right) in ops {
                left = Expr::Bin(BinExpr {
                    op,
                    left: Box::new(left),
                    right: Box::new(right),
                    span: span.clone(),
                });
            }
            left
        })
        .labelled("logical OR expression")
}

/// Logical AND expressions: `left && right`
fn logical_and_expr_parser<'a>(
    expr: impl Parser<'a, &'a str, Expr> + Clone + 'a,
    stmt: impl Parser<'a, &'a str, Stmt> + Clone + 'a,
) -> impl Parser<'a, &'a str, Expr> + 'a {
    bitwise_or_expr_parser(expr.clone(), stmt.clone())
        .then(
            just("&&")
                .padded()
                .then(bitwise_or_expr_parser(expr.clone(), stmt.clone()))
                .repeated()
                .at_most(MAX_OPERATOR_CHAIN_LENGTH)
                .collect::<Vec<_>>(),
        )
        .map_with(|(mut left, ops), extra| {
            let span: std::ops::Range<usize> = extra.span().into();
            for (_, right) in ops {
                left = Expr::Bin(BinExpr {
                    op: BinaryOp::And,
                    left: Box::new(left),
                    right: Box::new(right),
                    span: span.clone(),
                });
            }
            left
        })
        .labelled("logical AND expression")
}

/// Bitwise OR expressions: `left | right`
fn bitwise_or_expr_parser<'a>(
    expr: impl Parser<'a, &'a str, Expr> + Clone + 'a,
    stmt: impl Parser<'a, &'a str, Stmt> + Clone + 'a,
) -> impl Parser<'a, &'a str, Expr> + 'a {
    bitwise_xor_expr_parser(expr.clone(), stmt.clone())
        .then(
            just("|")
                .padded()
                .then(bitwise_xor_expr_parser(expr.clone(), stmt.clone()))
                .repeated()
                .at_most(MAX_OPERATOR_CHAIN_LENGTH)
                .collect::<Vec<_>>(),
        )
        .map_with(|(mut left, ops), extra| {
            let span: std::ops::Range<usize> = extra.span().into();
            for (_, right) in ops {
                left = Expr::Bin(BinExpr {
                    op: BinaryOp::BitwiseOr,
                    left: Box::new(left),
                    right: Box::new(right),
                    span: span.clone(),
                });
            }
            left
        })
        .labelled("bitwise OR expression")
}

/// Bitwise XOR expressions: `left ^ right`
fn bitwise_xor_expr_parser<'a>(
    expr: impl Parser<'a, &'a str, Expr> + Clone + 'a,
    stmt: impl Parser<'a, &'a str, Stmt> + Clone + 'a,
) -> impl Parser<'a, &'a str, Expr> + 'a {
    bitwise_and_expr_parser(expr.clone(), stmt.clone())
        .then(
            just("^")
                .padded()
                .then(bitwise_and_expr_parser(expr.clone(), stmt.clone()))
                .repeated()
                .at_most(MAX_OPERATOR_CHAIN_LENGTH)
                .collect::<Vec<_>>(),
        )
        .map_with(|(mut left, ops), extra| {
            let span: std::ops::Range<usize> = extra.span().into();
            for (_, right) in ops {
                left = Expr::Bin(BinExpr {
                    op: BinaryOp::BitwiseXor,
                    left: Box::new(left),
                    right: Box::new(right),
                    span: span.clone(),
                });
            }
            left
        })
        .labelled("bitwise XOR expression")
}

/// Bitwise AND expressions: `left & right`
fn bitwise_and_expr_parser<'a>(
    expr: impl Parser<'a, &'a str, Expr> + Clone + 'a,
    stmt: impl Parser<'a, &'a str, Stmt> + Clone + 'a,
) -> impl Parser<'a, &'a str, Expr> + 'a {
    equality_expr_parser(expr.clone(), stmt.clone())
        .then(
            just("&")
                .padded()
                .then(equality_expr_parser(expr.clone(), stmt.clone()))
                .repeated()
                .at_most(MAX_OPERATOR_CHAIN_LENGTH)
                .collect::<Vec<_>>(),
        )
        .map_with(|(mut left, ops), extra| {
            let span: std::ops::Range<usize> = extra.span().into();
            for (_, right) in ops {
                left = Expr::Bin(BinExpr {
                    op: BinaryOp::BitwiseAnd,
                    left: Box::new(left),
                    right: Box::new(right),
                    span: span.clone(),
                });
            }
            left
        })
        .labelled("bitwise AND expression")
}

/// Equality expressions: `left == right`, `left != right`, etc.
fn equality_expr_parser<'a>(
    expr: impl Parser<'a, &'a str, Expr> + Clone + 'a,
    stmt: impl Parser<'a, &'a str, Stmt> + Clone + 'a,
) -> impl Parser<'a, &'a str, Expr> + 'a {
    relational_expr_parser(expr.clone(), stmt.clone())
        .then(
            choice((
                just("==").padded().to(BinaryOp::EqEq),
                just("!=").padded().to(BinaryOp::NotEq),
                just("===").padded().to(BinaryOp::EqEq), // Using EqEq for ===
                just("!==").padded().to(BinaryOp::NotEq), // Using NotEq for !==
            ))
            .then(relational_expr_parser(expr.clone(), stmt.clone()))
            .or_not(),
        )
        .map_with(|(left, op_and_right), extra| {
            if let Some((op, right)) = op_and_right {
                Expr::Bin(BinExpr {
                    op,
                    left: Box::new(left),
                    right: Box::new(right),
                    span: extra.span().into(),
                })
            } else {
                left
            }
        })
        .labelled("equality expression")
}

/// Relational expressions: `left < right`, `left > right`, etc.
fn relational_expr_parser<'a>(
    expr: impl Parser<'a, &'a str, Expr> + Clone + 'a,
    stmt: impl Parser<'a, &'a str, Stmt> + Clone + 'a,
) -> impl Parser<'a, &'a str, Expr> + 'a {
    shift_expr_parser(expr.clone(), stmt.clone())
        .then(
            choice((
                just("<").padded().to(BinaryOp::Lt),
                just(">").padded().to(BinaryOp::Gt),
                just("<=").padded().to(BinaryOp::LtEq),
                just(">=").padded().to(BinaryOp::GtEq),
            ))
            .then(shift_expr_parser(expr.clone(), stmt.clone()))
            .or_not(),
        )
        .map_with(|(left, op_and_right), extra| {
            if let Some((op, right)) = op_and_right {
                Expr::Bin(BinExpr {
                    op,
                    left: Box::new(left),
                    right: Box::new(right),
                    span: extra.span().into(),
                })
            } else {
                left
            }
        })
        .labelled("relational expression")
}

/// Shift expressions: `left << right`, `left >> right`, `left >>> right`
fn shift_expr_parser<'a>(
    expr: impl Parser<'a, &'a str, Expr> + Clone + 'a,
    stmt: impl Parser<'a, &'a str, Stmt> + Clone + 'a,
) -> impl Parser<'a, &'a str, Expr> + 'a {
    additive_expr_parser(expr.clone(), stmt.clone())
        .then(
            choice((
                just("<<").padded().to(BinaryOp::LShift),
                just(">>").padded().to(BinaryOp::RShift),
                just(">>>").padded().to(BinaryOp::URShift),
            ))
            .then(additive_expr_parser(expr.clone(), stmt.clone()))
            .or_not(),
        )
        .map_with(|(left, op_and_right), extra| {
            if let Some((op, right)) = op_and_right {
                Expr::Bin(BinExpr {
                    op,
                    left: Box::new(left),
                    right: Box::new(right),
                    span: extra.span().into(),
                })
            } else {
                left
            }
        })
        .labelled("shift expression")
}

/// Additive expressions: `left + right`, `left - right`
fn additive_expr_parser<'a>(
    expr: impl Parser<'a, &'a str, Expr> + Clone + 'a,
    stmt: impl Parser<'a, &'a str, Stmt> + Clone + 'a,
) -> impl Parser<'a, &'a str, Expr> + 'a {
    multiplicative_expr_parser(expr.clone(), stmt.clone())
        .then(
            choice((
                just("+").padded().to(BinaryOp::Plus),
                just("-").padded().to(BinaryOp::Minus),
            ))
            .then(multiplicative_expr_parser(expr.clone(), stmt.clone()))
            .repeated()
            .at_most(MAX_OPERATOR_CHAIN_LENGTH)
            .collect::<Vec<_>>(),
        )
        .map_with(|(mut left, ops), extra| {
            let span: std::ops::Range<usize> = extra.span().into();
            for (op, right) in ops {
                left = Expr::Bin(BinExpr {
                    op,
                    left: Box::new(left),
                    right: Box::new(right),
                    span: span.clone(),
                });
            }
            left
        })
        .labelled("additive expression")
}

/// Multiplicative expressions: `left * right`, `left / right`, `left % right`
fn multiplicative_expr_parser<'a>(
    expr: impl Parser<'a, &'a str, Expr> + Clone + 'a,
    stmt: impl Parser<'a, &'a str, Stmt> + Clone + 'a,
) -> impl Parser<'a, &'a str, Expr> + 'a {
    exponentiation_expr_parser(expr.clone(), stmt.clone())
        .then(
            choice((
                just("*").padded().to(BinaryOp::Mul),
                just("/").padded().to(BinaryOp::Div),
                just("%").padded().to(BinaryOp::Mod),
            ))
            .then(exponentiation_expr_parser(expr.clone(), stmt.clone()))
            .repeated()
            .at_most(MAX_OPERATOR_CHAIN_LENGTH)
            .collect::<Vec<_>>(),
        )
        .map_with(|(mut left, ops), extra| {
            let span: std::ops::Range<usize> = extra.span().into();
            for (op, right) in ops {
                left = Expr::Bin(BinExpr {
                    op,
                    left: Box::new(left),
                    right: Box::new(right),
                    span: span.clone(),
                });
            }
            left
        })
        .labelled("multiplicative expression")
}

/// Exponentiation expressions: `left ** right` (right-associative)
fn exponentiation_expr_parser<'a>(
    expr: impl Parser<'a, &'a str, Expr> + Clone + 'a,
    stmt: impl Parser<'a, &'a str, Stmt> + Clone + 'a,
) -> impl Parser<'a, &'a str, Expr> + 'a {
    // Parse a chain of `unary ** unary ** ...` and fold right-to-left iteratively
    unary_expr_parser(expr.clone(), stmt.clone())
        .then(
            just("**")
                .padded()
                .then(unary_expr_parser(expr.clone(), stmt.clone()))
                .repeated()
                .at_most(MAX_OPERATOR_CHAIN_LENGTH)
                .collect::<Vec<_>>(),
        )
        .map_with(|(mut left, ops), extra| {
            let span: std::ops::Range<usize> = extra.span().into();
            // Fold right-to-left: a ** b ** c should be a ** (b ** c)
            // Process ops in reverse to achieve right-associativity
            for (_, right) in ops.into_iter().rev() {
                left = Expr::Bin(BinExpr {
                    op: BinaryOp::Exp,
                    left: Box::new(left),
                    right: Box::new(right),
                    span: span.clone(),
                });
            }
            left
        })
        .labelled("exponentiation expression")
}

/// Unary expressions: `!expr`, `-expr`, `+expr`, `~expr`, etc.
fn unary_expr_parser<'a>(
    expr: impl Parser<'a, &'a str, Expr> + Clone + 'a,
    stmt: impl Parser<'a, &'a str, Stmt> + Clone + 'a,
) -> impl Parser<'a, &'a str, Expr> + 'a {
    // Parse zero or more unary operators
    choice((
        just("!").padded().to(UnaryOp::Not),
        just("-").padded().to(UnaryOp::Minus),
        just("+").padded().to(UnaryOp::Plus),
        just("~").padded().to(UnaryOp::BitwiseNot),
        text::keyword("typeof").padded().to(UnaryOp::TypeOf),
        text::keyword("void").padded().to(UnaryOp::Void),
        text::keyword("delete").padded().to(UnaryOp::Delete),
    ))
    .repeated()
    .collect::<Vec<_>>()
    .then(postfix_expr_parser(expr.clone(), stmt.clone()))
    .map_with(|(ops, mut base), extra| {
        let span: std::ops::Range<usize> = extra.span().into();
        // Apply unary operators in reverse order (right to left)
        for op in ops.into_iter().rev() {
            base = Expr::Unary(UnaryExpr {
                op,
                arg: Box::new(base),
                span: span.clone(),
            });
        }
        base
    })
    .labelled("unary expression")
}

/// Postfix expressions: member access, calls, postfix operators
fn postfix_expr_parser<'a>(
    expr: impl Parser<'a, &'a str, Expr> + Clone + 'a,
    stmt: impl Parser<'a, &'a str, Stmt> + Clone + 'a,
) -> impl Parser<'a, &'a str, Expr> + 'a {
    primary_expr_parser(expr.clone(), stmt.clone())
        .then(
            // Postfix operators and member access/calls
            choice((
                // Postfix increment/decrement
                just("++").padded().map(|_| PostfixOp::Increment),
                just("--").padded().map(|_| PostfixOp::Decrement),
                // Member access: .prop
                just(".")
                    .padded()
                    .ignore_then(common::ident_parser())
                    .map(PostfixOp::Member),
                // Optional member access: ?.prop
                just("?.")
                    .padded()
                    .ignore_then(common::ident_parser())
                    .map(PostfixOp::OptionalMember),
                // Computed member access: [expr]
                just("[")
                    .padded()
                    .ignore_then(expr.clone())
                    .then_ignore(just("]").padded())
                    .map(|expr| PostfixOp::ComputedMember(Box::new(expr))),
                // Optional computed member access: ?.[expr]
                just("?.[")
                    .padded()
                    .ignore_then(expr.clone())
                    .then_ignore(just("]").padded())
                    .map(|expr| PostfixOp::OptionalComputedMember(Box::new(expr))),
                // Call: (args)
                expr.clone()
                    .separated_by(just(",").padded())
                    .allow_trailing()
                    .collect::<Vec<_>>()
                    .delimited_by(just("(").padded(), just(")").padded())
                    .map(PostfixOp::Call),
            ))
            .repeated()
            .collect::<Vec<_>>(),
        )
        .map_with(|(mut base, ops), extra| {
            let span: std::ops::Range<usize> = extra.span().into();
            for op in ops {
                match op {
                    PostfixOp::Increment => {
                        base = Expr::Update(UpdateExpr {
                            op: UpdateOp::Inc,
                            arg: Box::new(base),
                            prefix: false,
                            span: span.clone(),
                        });
                    }
                    PostfixOp::Decrement => {
                        base = Expr::Update(UpdateExpr {
                            op: UpdateOp::Dec,
                            arg: Box::new(base),
                            prefix: false,
                            span: span.clone(),
                        });
                    }
                    PostfixOp::Member(ident) => {
                        base = Expr::Member(MemberExpr {
                            obj: Box::new(base),
                            prop: MemberProp::Ident(ident),
                            span: span.clone(),
                        });
                    }
                    PostfixOp::OptionalMember(ident) => {
                        base = Expr::OptionalMember(OptionalMemberExpr {
                            obj: Box::new(base),
                            prop: MemberProp::Ident(ident),
                            span: span.clone(),
                        });
                    }
                    PostfixOp::ComputedMember(expr) => {
                        base = Expr::Member(MemberExpr {
                            obj: Box::new(base),
                            prop: MemberProp::Computed(expr),
                            span: span.clone(),
                        });
                    }
                    PostfixOp::OptionalComputedMember(expr) => {
                        base = Expr::OptionalMember(OptionalMemberExpr {
                            obj: Box::new(base),
                            prop: MemberProp::Computed(expr),
                            span: span.clone(),
                        });
                    }
                    PostfixOp::Call(args) => {
                        base = Expr::Call(CallExpr {
                            callee: Callee::Expr(Box::new(base)),
                            args,
                            span: span.clone(),
                        });
                    }
                }
            }
            base
        })
        .labelled("postfix expression")
}

/// Helper enum for postfix operations
enum PostfixOp {
    Increment,
    Decrement,
    Member(Ident),
    OptionalMember(Ident),
    ComputedMember(Box<Expr>),
    OptionalComputedMember(Box<Expr>),
    Call(Vec<Expr>),
}

/// Primary expressions: literals, identifiers, this, super, parens, etc.
fn primary_expr_parser<'a>(
    expr: impl Parser<'a, &'a str, Expr> + Clone + 'a,
    stmt: impl Parser<'a, &'a str, Stmt> + Clone + 'a,
) -> impl Parser<'a, &'a str, Expr> + 'a {
    choice((
        // Literals
        literal_parser().boxed(),
        // Identifiers
        ident_parser().boxed(),
        // This expression
        text::keyword("this")
            .padded()
            .map_with(|_, extra| {
                Expr::This(ThisExpr {
                    span: <std::ops::Range<usize>>::from(extra.span()),
                })
            })
            .boxed(),
        // Super expression
        text::keyword("super")
            .padded()
            .map_with(|_, extra| {
                Expr::Super(SuperExpr {
                    span: <std::ops::Range<usize>>::from(extra.span()),
                })
            })
            .boxed(),
        // Parenthesized expressions
        paren_expr_parser(expr.clone()).boxed(),
        // Array literals
        array_literal_parser(expr.clone()).boxed(),
        // Object literals
        object_literal_parser(expr.clone()).boxed(),
        // Function expressions
        function_expr_parser(stmt.clone()).boxed(),
        // Arrow functions
        arrow_expr_parser(expr.clone(), stmt.clone()).boxed(),
        // Template literals
        template_literal_parser(expr.clone()).boxed(),
        // New expressions
        new_expr_parser(expr.clone()).boxed(),
        // Await expressions
        await_expr_parser(expr.clone()).boxed(),
        // Yield expressions
        yield_expr_parser(expr.clone()).boxed(),
    ))
    .labelled("primary expression")
}

// Postfix expressions are handled inline in primary_expr_parser for simplicity

// Binary expressions can be added incrementally

/// Parser for identifiers as expressions.
fn ident_parser<'a>() -> impl Parser<'a, &'a str, Expr> + 'a {
    common::ident_parser()
        .map(Expr::Ident)
        .labelled("identifier")
}

/// Parser for literals.
fn literal_parser<'a>() -> impl Parser<'a, &'a str, Expr> + 'a {
    choice((
        // String literals with proper escape handling
        string_literal_parser().boxed(),
        // Number literals
        number_literal_parser().boxed(),
        // Boolean literals
        text::keyword("true")
            .padded()
            .to(Expr::Lit(Lit::Bool(true))),
        text::keyword("false")
            .padded()
            .to(Expr::Lit(Lit::Bool(false))),
        // Null literal
        text::keyword("null").padded().to(Expr::Lit(Lit::Null)),
    ))
    .labelled("literal")
}

/// Parser for string literals with escape sequence support.
fn string_literal_parser<'a>() -> impl Parser<'a, &'a str, Expr> + 'a {
    choice((
        // Double-quoted strings
        just('"')
            .ignore_then(string_content_parser('"'))
            .then_ignore(just('"')),
        // Single-quoted strings
        just('\'')
            .ignore_then(string_content_parser('\''))
            .then_ignore(just('\'')),
    ))
    .map(|s| Expr::Lit(Lit::Str(s)))
    .labelled("string literal")
}

/// Parser for string content with escape sequences.
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

/// Parser for number literals (integers and floats).
fn number_literal_parser<'a>() -> impl Parser<'a, &'a str, Expr> + 'a {
    text::int(10)
        .map(|s: &str| s.to_string())
        .then(
            just('.')
                .ignore_then(
                    any()
                        .filter(|c: &char| c.is_ascii_digit())
                        .repeated()
                        .at_least(1)
                        .collect::<String>(),
                )
                .or_not(),
        )
        .map(|(int_part, frac_part)| {
            let num_str = match frac_part {
                Some(frac) => format!("{}.{}", int_part, frac),
                None => int_part,
            };
            num_str.parse::<f64>().unwrap_or(0.0)
        })
        .map(|n| Expr::Lit(Lit::F64(n)))
        .labelled("number literal")
}

/// Parser for parenthesized expressions: `(expr)`
fn paren_expr_parser<'a>(
    expr: impl Parser<'a, &'a str, Expr> + Clone + 'a,
) -> impl Parser<'a, &'a str, Expr> + 'a {
    expr.delimited_by(just("(").padded(), just(")").padded())
        .map_with(|expr, extra| {
            Expr::Paren(ParenExpr {
                expr: Box::new(expr),
                span: extra.span().into(),
            })
        })
        .labelled("parenthesized expression")
}

/// Parser for array literals: `[elem1, elem2, ...]`
fn array_literal_parser<'a>(
    expr: impl Parser<'a, &'a str, Expr> + Clone + 'a,
) -> impl Parser<'a, &'a str, Expr> + 'a {
    expr.separated_by(just(",").padded())
        .allow_trailing()
        .collect::<Vec<_>>()
        .map(|elems| elems.into_iter().map(Some).collect())
        .delimited_by(just("[").padded(), just("]").padded())
        .map_with(|elems, extra| {
            Expr::Array(ArrayLit {
                elems,
                span: extra.span().into(),
            })
        })
        .labelled("array literal")
}

/// Parser for object literals: `{ prop1: value1, prop2, ...rest }`
fn object_literal_parser<'a>(
    expr: impl Parser<'a, &'a str, Expr> + Clone + 'a,
) -> impl Parser<'a, &'a str, Expr> + 'a {
    choice((
        // Spread element
        just("...")
            .padded()
            .ignore_then(expr.clone())
            .map(|expr| PropOrSpread::Spread(Box::new(SpreadElement { expr, span: 0..0 }))),
        // Property
        common::ident_parser()
            .then(just(":").padded().ignore_then(expr.clone()).or_not())
            .map(|(key, value)| {
                if let Some(value) = value {
                    PropOrSpread::Prop(Prop::KeyValue(Box::new(KeyValueProp {
                        key: PropName::Ident(key.clone()),
                        value,
                        span: 0..0,
                    })))
                } else {
                    PropOrSpread::Prop(Prop::Shorthand(key))
                }
            }),
    ))
    .separated_by(just(",").padded())
    .allow_trailing()
    .collect::<Vec<_>>()
    .delimited_by(just("{").padded(), just("}").padded())
    .map_with(|props, extra| {
        Expr::Object(ObjectLit {
            props,
            span: extra.span().into(),
        })
    })
    .labelled("object literal")
}

/// Parser for function expressions: `function name() {}` or `function() {}`
fn function_expr_parser<'a>(
    stmt: impl Parser<'a, &'a str, Stmt> + Clone + 'a,
) -> impl Parser<'a, &'a str, Expr> + 'a {
    text::keyword("function")
        .padded()
        .ignore_then(common::ident_parser().or_not())
        .then(common::param_list_parser())
        .then(common::optional_type_annotation())
        .then(common::optional_block_parser(stmt))
        .map_with(|(((ident, params), return_type), body), extra| {
            Expr::Fn(FnExpr {
                ident,
                function: Function {
                    params,
                    body,
                    return_type,
                    span: extra.span().into(),
                    is_async: false,
                    is_generator: false,
                },
                span: extra.span().into(),
            })
        })
        .labelled("function expression")
}

/// Parser for arrow functions: `(params) => expr` or `(params) => { stmts }`
fn arrow_expr_parser<'a>(
    expr: impl Parser<'a, &'a str, Expr> + Clone + 'a,
    stmt: impl Parser<'a, &'a str, Stmt> + Clone + 'a,
) -> impl Parser<'a, &'a str, Expr> + 'a {
    // Parse parameters with optional type annotations for arrow function parameters
    common::param_list_parser()
        .then(common::optional_type_annotation())
        .then(just("=>").padded())
        .then(choice((
            // Block body
            common::block_parser(stmt).map(ArrowBody::Block),
            // Expression body
            expr.map(|e| ArrowBody::Expr(Box::new(e))),
        )))
        .map_with(|(((params, return_type), _), body), extra| {
            Expr::Arrow(ArrowExpr {
                params,
                body,
                return_type,
                span: extra.span().into(),
            })
        })
        .labelled("arrow function")
}

/// Parser for template literals: `\`text ${expr} text\``
fn template_literal_parser<'a>(
    expr: impl Parser<'a, &'a str, Expr> + Clone + 'a,
) -> impl Parser<'a, &'a str, Expr> + 'a {
    just('`')
        .ignore_then(
            // Parse template parts: text segments and ${expr} interleaved
            // Strategy: parse ${expr} first (higher priority), then fall back to text
            choice((
                // Expression: ${expr}
                just("${")
                    .padded()
                    .ignore_then(expr.clone())
                    .then_ignore(just("}").padded())
                    .map(|e| (None, Some(e))),
                // Text: any character except `, stopping before ${ or at `
                // We need to collect chars until we see $ followed by {, or `
                any()
                    .filter(|c: &char| *c != '`')
                    .repeated()
                    .at_least(0)
                    .collect::<String>()
                    .then(
                        // Check if next is ${ - if so, don't consume the $
                        just('$').then(just('{').not()).to(Some('$')).or_not(),
                    )
                    .map(|(text, dollar_opt)| {
                        let mut result = text;
                        // Only add $ if it's not followed by {
                        if let Some(Some(d)) = dollar_opt {
                            result.push(d);
                        }
                        (Some(result), None)
                    }),
            ))
            .repeated()
            .collect::<Vec<_>>(),
        )
        .then_ignore(just('`'))
        .map(|parts: Vec<(Option<String>, Option<Expr>)>| {
            let mut quasis = Vec::new();
            let mut exprs = Vec::new();
            let mut current_text = String::new();

            for (text_opt, expr_opt) in parts {
                if let Some(text) = text_opt {
                    current_text.push_str(&text);
                }
                if let Some(expr) = expr_opt {
                    // Push the accumulated text as a quasi
                    quasis.push(TplElement {
                        raw: current_text.clone(),
                        cooked: Some(current_text.clone()),
                        span: 0..0,
                    });
                    current_text.clear();
                    // Push the expression
                    exprs.push(expr);
                    // Push empty quasi for after the expression
                    quasis.push(TplElement {
                        raw: String::new(),
                        cooked: None,
                        span: 0..0,
                    });
                }
            }
            // Push final text if any
            if !current_text.is_empty() || quasis.is_empty() {
                quasis.push(TplElement {
                    raw: current_text.clone(),
                    cooked: Some(current_text),
                    span: 0..0,
                });
            }
            (quasis, exprs)
        })
        .map_with(|(quasis, exprs), extra| {
            Expr::Tpl(TplExpr {
                quasis,
                exprs,
                span: extra.span().into(),
            })
        })
        .labelled("template literal")
}

/// Parser for new expressions: `new Constructor(args...)`
fn new_expr_parser<'a>(
    expr: impl Parser<'a, &'a str, Expr> + Clone + 'a,
) -> impl Parser<'a, &'a str, Expr> + 'a {
    text::keyword("new")
        .padded()
        .ignore_then(expr.clone())
        .then(
            expr.clone()
                .separated_by(just(",").padded())
                .allow_trailing()
                .collect::<Vec<_>>()
                .delimited_by(just("(").padded(), just(")").padded())
                .or_not(),
        )
        .map_with(|(callee, args), extra| {
            Expr::New(NewExpr {
                callee: Box::new(callee),
                args: args.unwrap_or_default(),
                span: extra.span().into(),
            })
        })
        .labelled("new expression")
}

/// Parser for await expressions: `await expr`
fn await_expr_parser<'a>(
    expr: impl Parser<'a, &'a str, Expr> + Clone + 'a,
) -> impl Parser<'a, &'a str, Expr> + 'a {
    text::keyword("await")
        .padded()
        .ignore_then(expr)
        .map_with(|arg, extra| {
            Expr::Await(AwaitExpr {
                arg: Box::new(arg),
                span: extra.span().into(),
            })
        })
        .labelled("await expression")
}

/// Parser for yield expressions: `yield expr` or `yield* expr`
fn yield_expr_parser<'a>(
    expr: impl Parser<'a, &'a str, Expr> + Clone + 'a,
) -> impl Parser<'a, &'a str, Expr> + 'a {
    text::keyword("yield")
        .padded()
        .then(just("*").padded().or_not())
        .then(expr.clone().or_not())
        .map_with(|((_, delegate), arg), extra| {
            Expr::Yield(YieldExpr {
                arg: arg.map(Box::new),
                delegate: delegate.is_some(),
                span: extra.span().into(),
            })
        })
        .labelled("yield expression")
}
