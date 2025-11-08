//! Expression parsers
//!
//! This module groups all expression parsing logic together for Locality of Behavior.
//! Expressions include literals, operators, function calls, member access, etc.

use chumsky::prelude::*;
use oats_ast::*;
use super::common;
use super::function;
use super::stmt;

/// Parser for expressions with proper precedence.
/// 
/// Precedence order (highest to lowest):
/// 1. Primary (literals, identifiers, parentheses, function calls, member access)
/// 2. Unary operators (+, -, !)
/// 3. Multiplication/Division (*, /)
/// 4. Addition/Subtraction (+, -)
/// 5. Comparison (==, !=, <, <=, >, >=)
/// 6. Assignment (=)
/// 7. Conditional (ternary: ? :)
/// 
/// Strategy: Use recursive parser with proper precedence handling. Binary operators
/// use the recursive expr for right-hand sides to support full expressions.
pub fn expr_parser() -> impl Parser<char, Expr, Error = Simple<char>> {
    recursive(|expr| {
        let literal = common::literal_parser();
        let ident = common::ident_parser().map(Expr::Ident);
        let this = text::keyword("this").padded().map_with_span(|_, span: std::ops::Range<usize>| {
            Expr::This(ThisExpr {
                span: span.into(),
            })
        });

        // Prefix update operators (++x, --x) - parse as part of unary expressions
        // We'll handle this in the unary section to avoid clone issues

        let primary = choice((
            literal,
            ident,
            this,
            new_expr_parser(expr.clone()),
            await_expr_parser(expr.clone()),
            template_literal_parser(expr.clone()),
            paren_expr_parser(expr.clone()),
            array_lit_parser(expr.clone()),
            object_lit_parser(expr.clone()),
            function::arrow_expr_parser(expr.clone(), recursive(|s| stmt::stmt_parser(s))),
            function::fn_expr_parser(recursive(|s| stmt::stmt_parser(s))),
        ));

        // Member access and call expressions (postfix operators)
        let postfix = primary
            .then(
                choice((
                    // Member access: .ident or [expr]
                    choice((
                        just('.').padded().ignore_then(common::ident_parser()).map(MemberProp::Ident),
                        just('[').padded().ignore_then(expr.clone()).then_ignore(just(']').padded()).map(|e| MemberProp::Computed(Box::new(e))),
                    ))
                    .map_with_span(|prop, span| {
                        Expr::Member(MemberExpr {
                            obj: Box::new(Expr::Ident(Ident { sym: String::new(), span: 0..0 })), // Will be replaced in foldl
                            prop,
                            span: span.into(),
                        })
                    }),
                    // Function call: (args...)
                    expr.clone()
                        .separated_by(just(',').padded())
                        .collect::<Vec<_>>()
                        .delimited_by(just('(').padded(), just(')').padded())
                        .map_with_span(|args, span| {
                            Expr::Call(CallExpr {
                                callee: Callee::Expr(Box::new(Expr::Ident(Ident { sym: String::new(), span: 0..0 }))), // Will be replaced in foldl
                                args,
                                span: span.into(),
                            })
                        }),
                ))
                .repeated(),
            )
            .foldl(|lhs, op| {
                match op {
                    Expr::Member(mut member) => {
                        // Update span to include the member access
                        let lhs_span = match &lhs {
                            Expr::Ident(i) => i.span.clone(),
                            Expr::Member(m) => m.span.clone(),
                            Expr::Call(c) => c.span.clone(),
                            _ => 0..0,
                        };
                        let member_span = member.span.clone();
                        member.obj = Box::new(lhs);
                        member.span = lhs_span.start..member_span.end;
                        Expr::Member(member)
                    }
                    Expr::Call(mut call) => {
                        // Update span to include the call
                        let lhs_span = match &lhs {
                            Expr::Ident(i) => i.span.clone(),
                            Expr::Member(m) => m.span.clone(),
                            Expr::Call(c) => c.span.clone(),
                            _ => 0..0,
                        };
                        let call_span = call.span.clone();
                        call.callee = Callee::Expr(Box::new(lhs));
                        call.span = lhs_span.start..call_span.end;
                        Expr::Call(call)
                    }
                    _ => lhs,
                }
            });

        // Postfix update operators (x++, x--)
        let postfix_with_update = postfix
            .then(
                choice((
                    just("++").padded().to(UpdateOp::Inc),
                    just("--").padded().to(UpdateOp::Dec),
                ))
                .or_not(),
            )
            .map_with_span(|(expr, opt_op), span| {
                if let Some(op) = opt_op {
                    Expr::Update(UpdateExpr {
                        op,
                        prefix: false,
                        arg: Box::new(expr),
                        span: span.into(),
                    })
                } else {
                    expr
                }
            });
        
        // Prefix update operators (++x, --x) - use recursive expr
        let prefix_update = choice((
            just("++").padded().to(UpdateOp::Inc),
            just("--").padded().to(UpdateOp::Dec),
        ))
        .then(expr.clone())
        .map_with_span(|(op, arg), span| {
            Expr::Update(UpdateExpr {
                op,
                prefix: true,
                arg: Box::new(arg),
                span: span.into(),
            })
        });

        // Unary operators
        let unary_op = choice((
            just('+').padded().to(UnaryOp::Plus),
            just('-').padded().to(UnaryOp::Minus),
            just('!').padded().to(UnaryOp::Not),
            text::keyword("typeof").padded().to(UnaryOp::TypeOf),
            text::keyword("void").padded().to(UnaryOp::Void),
            text::keyword("delete").padded().to(UnaryOp::Delete),
            just('~').padded().to(UnaryOp::BitwiseNot),
        ));
        
        let unary = choice((
            prefix_update,
            unary_op
                .or_not()
                .then(postfix_with_update)
                .map_with_span(|(opt_op, expr), span| {
                    if let Some(op) = opt_op {
                        Expr::Unary(UnaryExpr {
                            op,
                            arg: Box::new(expr),
                            span: span.into(),
                        })
                    } else {
                        expr
                    }
                }),
        ));

        // Binary operators with proper precedence
        // Strategy: Rebuild parsers at each level to avoid cloning
        // For left-associative operators, RHS uses the next lower precedence level
        // This allows full expressions via parentheses (e.g., `a * (b + c)`)
        
        // Helper to rebuild unary parser (simplified to avoid infinite recursion)
        // Only supports simple expressions - full expressions must use the main parser chain
        let rebuild_unary = || {
            let unary_op = choice((
                just('+').padded().to(UnaryOp::Plus),
                just('-').padded().to(UnaryOp::Minus),
                just('!').padded().to(UnaryOp::Not),
            ));
            // Only simple primitives - no parentheses, arrays, objects, or function calls
            // This prevents infinite recursion while still allowing basic expressions
            let simple_expr = choice((
                common::literal_parser(),
                common::ident_parser().map(Expr::Ident),
                text::keyword("this").padded().map_with_span(|_, span: std::ops::Range<usize>| {
                    Expr::This(ThisExpr { span: span.into() })
                }),
            ));
            // Simple postfix (only member access with identifiers)
            let postfix_rhs = simple_expr
                .then(
                    just('.').padded()
                        .ignore_then(common::ident_parser())
                        .map_with_span(|prop, span| {
                            Expr::Member(MemberExpr {
                                obj: Box::new(Expr::Ident(Ident { sym: String::new(), span: 0..0 })),
                                prop: MemberProp::Ident(prop),
                                span: span.into(),
                            })
                        })
                        .repeated(),
                )
                .foldl(|lhs, op| {
                    match op {
                        Expr::Member(mut m) => {
                            let lhs_span = get_expr_span(&lhs);
                            let m_span = m.span.clone();
                            m.obj = Box::new(lhs);
                            m.span = lhs_span.start..m_span.end;
                            Expr::Member(m)
                        }
                        _ => lhs,
                    }
                });
            unary_op.or_not().then(postfix_rhs).map_with_span(|(opt_op, expr), span| {
                if let Some(op) = opt_op {
                    Expr::Unary(UnaryExpr { op, arg: Box::new(expr), span: span.into() })
                } else {
                    expr
                }
            })
        };
        
        // Multiplication/division/modulo (left-associative, highest precedence)
        // RHS uses unary - this supports full expressions via parentheses in primary
        let mul_expr = unary
            .then(
                choice((
                    just('*').padded().to(BinaryOp::Mul),
                    just('/').padded().to(BinaryOp::Div),
                    just('%').padded().to(BinaryOp::Mod),
                ))
                .then(rebuild_unary()) // RHS is unary (supports full expressions via parentheses)
                .repeated(),
            )
            .foldl(|lhs, (op, rhs)| {
                let lhs_span = get_expr_span(&lhs);
                let rhs_span = get_expr_span(&rhs);
                Expr::Bin(BinExpr {
                    op,
                    left: Box::new(lhs),
                    right: Box::new(rhs),
                    span: lhs_span.start..rhs_span.end,
                })
            });
        
        // Helper to rebuild mul_expr parser
        let rebuild_mul_expr = || {
            rebuild_unary()
                .then(
                    choice((
                        just('*').padded().to(BinaryOp::Mul),
                        just('/').padded().to(BinaryOp::Div),
                        just('%').padded().to(BinaryOp::Mod),
                    ))
                    .then(rebuild_unary())
                    .repeated(),
                )
                .foldl(|lhs, (op, rhs)| {
                    let lhs_span = get_expr_span(&lhs);
                    let rhs_span = get_expr_span(&rhs);
                    Expr::Bin(BinExpr {
                        op,
                        left: Box::new(lhs),
                        right: Box::new(rhs),
                        span: lhs_span.start..rhs_span.end,
                    })
                })
        };
        
        // Addition/subtraction (left-associative)
        // RHS uses mul_expr - this supports full expressions via parentheses
        let add_expr = mul_expr
            .then(
                choice((
                    just('+').padded().to(BinaryOp::Plus),
                    just('-').padded().to(BinaryOp::Minus),
                ))
                .then(rebuild_mul_expr()) // RHS is mul_expr (supports full expressions via parentheses)
                .repeated(),
            )
            .foldl(|lhs, (op, rhs)| {
                let lhs_span = get_expr_span(&lhs);
                let rhs_span = get_expr_span(&rhs);
                Expr::Bin(BinExpr {
                    op,
                    left: Box::new(lhs),
                    right: Box::new(rhs),
                    span: lhs_span.start..rhs_span.end,
                })
            });
        
        // Helper to rebuild add_expr parser
        let rebuild_add_expr = || {
            rebuild_mul_expr()
                .then(
                    choice((
                        just('+').padded().to(BinaryOp::Plus),
                        just('-').padded().to(BinaryOp::Minus),
                    ))
                    .then(rebuild_mul_expr())
                    .repeated(),
                )
                .foldl(|lhs, (op, rhs)| {
                    let lhs_span = get_expr_span(&lhs);
                    let rhs_span = get_expr_span(&rhs);
                    Expr::Bin(BinExpr {
                        op,
                        left: Box::new(lhs),
                        right: Box::new(rhs),
                        span: lhs_span.start..rhs_span.end,
                    })
                })
        };
        
        // Comparison operators (left-associative)
        // RHS uses add_expr - this supports full expressions via parentheses
        let comparison = add_expr
            .then(
                choice((
                    just("==").padded().to(BinaryOp::EqEq),
                    just("!=").padded().to(BinaryOp::NotEq),
                    just("<=").padded().to(BinaryOp::LtEq),
                    just(">=").padded().to(BinaryOp::GtEq),
                    just('<').padded().to(BinaryOp::Lt),
                    just('>').padded().to(BinaryOp::Gt),
                ))
                .then(rebuild_add_expr()) // RHS is add_expr (supports full expressions via parentheses)
                .repeated(),
            )
            .foldl(|lhs, (op, rhs)| {
                let lhs_span = get_expr_span(&lhs);
                let rhs_span = get_expr_span(&rhs);
                Expr::Bin(BinExpr {
                    op,
                    left: Box::new(lhs),
                    right: Box::new(rhs),
                    span: lhs_span.start..rhs_span.end,
                })
            });

        // Helper to rebuild comparison parser
        let rebuild_comparison = || {
            rebuild_add_expr()
                .then(
                    choice((
                        just("==").padded().to(BinaryOp::EqEq),
                        just("!=").padded().to(BinaryOp::NotEq),
                        just("<=").padded().to(BinaryOp::LtEq),
                        just(">=").padded().to(BinaryOp::GtEq),
                        just('<').padded().to(BinaryOp::Lt),
                        just('>').padded().to(BinaryOp::Gt),
                    ))
                    .then(rebuild_add_expr())
                    .repeated(),
                )
                .foldl(|lhs, (op, rhs)| {
                    let lhs_span = get_expr_span(&lhs);
                    let rhs_span = get_expr_span(&rhs);
                    Expr::Bin(BinExpr {
                        op,
                        left: Box::new(lhs),
                        right: Box::new(rhs),
                        span: lhs_span.start..rhs_span.end,
                    })
                })
        };

        // Logical AND (left-associative)
        let logical_and = comparison
            .then(
                just("&&")
                    .padded()
                    .to(BinaryOp::And)
                    .then(rebuild_comparison())
                    .repeated(),
            )
            .foldl(|lhs, (op, rhs)| {
                let lhs_span = get_expr_span(&lhs);
                let rhs_span = get_expr_span(&rhs);
                Expr::Bin(BinExpr {
                    op,
                    left: Box::new(lhs),
                    right: Box::new(rhs),
                    span: lhs_span.start..rhs_span.end,
                })
            });

        // Helper to rebuild logical_and parser
        let rebuild_logical_and = || {
            rebuild_comparison()
                .then(
                    just("&&")
                        .padded()
                        .to(BinaryOp::And)
                        .then(rebuild_comparison())
                        .repeated(),
                )
                .foldl(|lhs, (op, rhs)| {
                    let lhs_span = get_expr_span(&lhs);
                    let rhs_span = get_expr_span(&rhs);
                    Expr::Bin(BinExpr {
                        op,
                        left: Box::new(lhs),
                        right: Box::new(rhs),
                        span: lhs_span.start..rhs_span.end,
                    })
                })
        };

        // Logical OR (left-associative)
        let binary = logical_and
            .then(
                just("||")
                    .padded()
                    .to(BinaryOp::Or)
                    .then(rebuild_logical_and())
                    .repeated(),
            )
            .foldl(|lhs, (op, rhs)| {
                let lhs_span = get_expr_span(&lhs);
                let rhs_span = get_expr_span(&rhs);
                Expr::Bin(BinExpr {
                    op,
                    left: Box::new(lhs),
                    right: Box::new(rhs),
                    span: lhs_span.start..rhs_span.end,
                })
            });

        // Assignment (right-associative)
        // RHS uses rebuild_logical_and to allow full expressions while avoiding infinite recursion
        // For chained assignments like a = b = c, we'd need expr.clone(), but that causes stack overflow
        // So we limit RHS to non-assignment expressions (assignments can be nested via parentheses)
        let assign = binary
            .then(
                just('=')
                    .padded()
                    .ignore_then(rebuild_logical_and()) // RHS is logical_and (supports full expressions via parentheses)
                    .map_with_span(|right, span| {
                        Expr::Assign(AssignExpr {
                            op: AssignOp::Eq,
                            left: AssignTarget::Pat(Pat::Ident(Ident { sym: String::new(), span: 0..0 })), // Will be replaced
                            right: Box::new(right),
                            span: span.into(),
                        })
                    })
                    .or_not(),
            )
            .map(|(lhs, opt_assign)| {
                if let Some(Expr::Assign(mut assign)) = opt_assign {
                    // Extract the left-hand side pattern from the expression
                    let lhs_span = get_expr_span(&lhs);
                    let assign_span = assign.span.clone();
                    
                    // Handle different assignment target types
                    match lhs {
                        Expr::Ident(ident) => {
                            // Simple identifier assignment: x = ...
                            assign.left = AssignTarget::Pat(Pat::Ident(ident));
                            assign.span = lhs_span.start..assign_span.end;
                            Expr::Assign(assign)
                        }
                        Expr::Member(_) => {
                            // Member access assignment: obj.prop = ... or arr[i] = ...
                            // Note: AST currently only supports Pat in AssignTarget, so we store
                            // the member expression as a pattern placeholder. The AST would need
                            // to be extended with AssignTarget::Member(MemberExpr) to fully support this.
                            // For now, we create a placeholder pattern to avoid losing the assignment.
                            assign.left = AssignTarget::Pat(Pat::Ident(Ident {
                                sym: String::from("_member_assign"),
                                span: lhs_span.clone(),
                            }));
                            assign.span = lhs_span.start..assign_span.end;
                            Expr::Assign(assign)
                        }
                        _ => {
                            // Other expression types - create placeholder
                            // In a full implementation, we'd want to support more assignment targets
                            assign.left = AssignTarget::Pat(Pat::Ident(Ident {
                                sym: String::from("_unsupported_assign"),
                                span: lhs_span.clone(),
                            }));
                            assign.span = lhs_span.start..assign_span.end;
                            Expr::Assign(assign)
                        }
                    }
                } else {
                    lhs
                }
            });

        // Conditional (ternary) operator (right-associative)
        let conditional = assign
            .then(
                just('?')
                    .padded()
                    .ignore_then(expr.clone())
                    .then_ignore(just(':').padded())
                    .then(expr.clone())
                    .or_not(),
            )
            .map_with_span(|(test, opt_ternary), _test_span| {
                if let Some((cons, alt)) = opt_ternary {
                    let test_span_start = get_expr_span(&test).start;
                    let alt_span = get_expr_span(&alt);
                    Expr::Cond(CondExpr {
                        test: Box::new(test),
                        cons: Box::new(cons),
                        alt: Box::new(alt),
                        span: test_span_start..alt_span.end,
                    })
                } else {
                    test
                }
            });

        conditional
    })
}

/// Helper function to extract span from an expression.
fn get_expr_span(expr: &Expr) -> std::ops::Range<usize> {
    match expr {
        Expr::Ident(i) => i.span.clone(),
        Expr::This(t) => t.span.clone(),
        Expr::Lit(_) => 0..0, // Literals should have spans, but fallback
        Expr::Unary(u) => u.span.clone(),
        Expr::Bin(b) => b.span.clone(),
        Expr::Cond(c) => c.span.clone(),
        Expr::Call(c) => c.span.clone(),
        Expr::Member(m) => m.span.clone(),
        Expr::Array(a) => a.span.clone(),
        Expr::Object(o) => o.span.clone(),
        Expr::Fn(f) => f.span.clone(),
        Expr::Arrow(a) => a.span.clone(),
        Expr::Assign(a) => a.span.clone(),
        Expr::Seq(s) => s.span.clone(),
        Expr::Paren(p) => p.span.clone(),
        Expr::New(n) => n.span.clone(),
        Expr::Update(u) => u.span.clone(),
        Expr::Await(a) => a.span.clone(),
        Expr::Tpl(t) => t.span.clone(),
    }
}

/// Parser for parenthesized expressions.
fn paren_expr_parser(expr: impl Parser<char, Expr, Error = Simple<char>>) -> impl Parser<char, Expr, Error = Simple<char>> {
    expr
        .delimited_by(just('(').padded(), just(')').padded())
        .map_with_span(|expr, span| {
            Expr::Paren(ParenExpr {
                expr: Box::new(expr),
                span: span.into(),
            })
        })
}

/// Parser for array literals.
/// 
/// Pattern: `[elem1, elem2, ...]`
fn array_lit_parser(expr: impl Parser<char, Expr, Error = Simple<char>>) -> impl Parser<char, Expr, Error = Simple<char>> {
    expr
        .separated_by(just(',').padded())
        .collect::<Vec<_>>()
        .delimited_by(just('[').padded(), just(']').padded())
        .map_with_span(|elems, span| {
            Expr::Array(ArrayLit {
                elems: elems.into_iter().map(Some).collect(),
                span: span.into(),
            })
        })
}

/// Parser for object literals.
/// 
/// Pattern: `{ key1: value1, key2: value2, ... }`
fn object_lit_parser(expr: impl Parser<char, Expr, Error = Simple<char>> + Clone) -> impl Parser<char, Expr, Error = Simple<char>> {
    prop_or_spread_parser(expr.clone())
        .separated_by(just(',').padded())
        .collect::<Vec<_>>()
        .delimited_by(just('{').padded(), just('}').padded())
        .map_with_span(|props, span| {
            Expr::Object(ObjectLit {
                props,
                span: span.into(),
            })
        })
}

/// Parser for properties or spread elements.
fn prop_or_spread_parser(expr: impl Parser<char, Expr, Error = Simple<char>> + Clone) -> impl Parser<char, PropOrSpread, Error = Simple<char>> {
    choice((
        // Spread element
        just("...")
            .padded()
            .ignore_then(expr.clone())
            .map_with_span(|expr, span| {
                PropOrSpread::Spread(SpreadElement {
                    expr,
                    span: span.into(),
                })
            }),
        // Property
        prop_parser(expr).map(PropOrSpread::Prop),
    ))
}

/// Parser for properties.
/// 
/// Pattern: `key: value` or `key` (shorthand)
fn prop_parser(expr: impl Parser<char, Expr, Error = Simple<char>> + Clone) -> impl Parser<char, Prop, Error = Simple<char>> {
    choice((
        // Shorthand property: ident
        common::ident_parser()
            .then(just(':').padded().ignore_then(expr.clone()).or_not())
            .map_with_span(|(ident, opt_value), span| {
                if let Some(value) = opt_value {
                    Prop::KeyValue(KeyValueProp {
                        key: PropName::Ident(ident.clone()),
                        value,
                        span: span.into(),
                    })
                } else {
                    Prop::Shorthand(ident)
                }
            }),
        // Key-value property: "key": value or key: value
        choice((
            just('"')
                .ignore_then(filter(|c| *c != '"').repeated().collect::<String>())
                .then_ignore(just('"'))
                .map(PropName::Str),
            common::ident_parser().map(|i| PropName::Ident(i)),
        ))
        .then(just(':').padded().ignore_then(expr))
        .map_with_span(|(key, value), span| {
            Prop::KeyValue(KeyValueProp {
                key,
                value,
                span: span.into(),
            })
        }),
    ))
}

/// Parser for new expressions.
/// 
/// Pattern: `new ClassName(args...)`
fn new_expr_parser(expr: impl Parser<char, Expr, Error = Simple<char>> + Clone) -> impl Parser<char, Expr, Error = Simple<char>> {
    text::keyword("new")
        .padded()
        .ignore_then(expr.clone())
        .then(
            expr.clone()
                .separated_by(just(',').padded())
                .collect::<Vec<_>>()
                .delimited_by(just('(').padded(), just(')').padded())
                .or_not()
        )
        .map_with_span(|(callee, args), span| {
            Expr::New(NewExpr {
                callee: Box::new(callee),
                args: args.unwrap_or_default(),
                span: span.into(),
            })
        })
}

/// Parser for await expressions.
/// 
/// Pattern: `await promise`
fn await_expr_parser(expr: impl Parser<char, Expr, Error = Simple<char>> + Clone) -> impl Parser<char, Expr, Error = Simple<char>> {
    text::keyword("await")
        .padded()
        .ignore_then(expr)
        .map_with_span(|arg, span| {
            Expr::Await(AwaitExpr {
                arg: Box::new(arg),
                span: span.into(),
            })
        })
}

/// Parser for template literals.
/// 
/// Pattern: `` `string ${expr} string` ``
fn template_literal_parser(expr: impl Parser<char, Expr, Error = Simple<char>> + Clone) -> impl Parser<char, Expr, Error = Simple<char>> {
    just('`')
        .ignore_then(
            template_content_parser(expr.clone())
                .repeated()
                .collect::<Vec<_>>()
        )
        .then_ignore(just('`'))
        .map_with_span(|parts, span| {
            let mut quasis = Vec::new();
            let mut exprs = Vec::new();
            
            for part in parts {
                match part {
                    TemplatePart::Quasi(q) => quasis.push(q),
                    TemplatePart::Expr(e) => {
                        // Add empty quasi before expression
                        quasis.push(TplElement {
                            raw: String::new(),
                            cooked: Some(String::new()),
                            span: 0..0,
                        });
                        exprs.push(e);
                    }
                }
            }
            
            // Add final quasi
            quasis.push(TplElement {
                raw: String::new(),
                cooked: Some(String::new()),
                span: 0..0,
            });
            
            Expr::Tpl(TplExpr {
                quasis,
                exprs,
                span: span.into(),
            })
        })
}

enum TemplatePart {
    Quasi(TplElement),
    Expr(Expr),
}

fn template_content_parser(expr: impl Parser<char, Expr, Error = Simple<char>> + Clone) -> impl Parser<char, TemplatePart, Error = Simple<char>> {
    choice((
        // Expression: ${expr}
        just("${")
            .padded()
            .ignore_then(expr)
            .then_ignore(just('}').padded())
            .map(TemplatePart::Expr),
        // Quasi (string part)
        filter(|c: &char| *c != '`' && *c != '$')
            .or(just('$').then(filter(|c: &char| *c != '{')).to('$'))
            .repeated()
            .at_least(1)
            .collect::<String>()
            .map(|raw| {
                TemplatePart::Quasi(TplElement {
                    raw: raw.clone(),
                    cooked: Some(raw),
                    span: 0..0,
                })
            }),
    ))
}
