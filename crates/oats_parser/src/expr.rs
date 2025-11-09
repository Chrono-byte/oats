//! Expression parsers with proper operator precedence

use super::common;
use chumsky::prelude::*;
use oats_ast::*;

/// Expression parser with proper precedence handling.
///
/// Uses recursive descent with precedence levels.
pub fn expr_parser<'a>(
    stmt: impl Parser<'a, &'a str, Stmt> + Clone + 'a,
) -> impl Parser<'a, &'a str, Expr> + 'a {
    recursive(|expr| {
        // Start with primary expressions
        primary_expr_parser(expr.clone(), stmt.clone()).boxed()
    })
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
    // Parse patterns (identifiers) for arrow function parameters
    common::pat_parser()
        .separated_by(just(",").padded())
        .allow_trailing()
        .collect::<Vec<_>>()
        .delimited_by(just("(").padded(), just(")").padded())
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
    // Simplified template literal parser
    // For now, just parse a basic template with text and expressions
    just('`')
        .ignore_then(
            // Parse template parts: text or ${expr}
            choice((
                // Expression: ${expr}
                just("${")
                    .padded()
                    .ignore_then(expr.clone())
                    .then_ignore(just("}").padded())
                    .map(|e| (Vec::<String>::new(), vec![e])),
                // Text content (simplified - just collect until ` or $)
                any()
                    .filter(|c: &char| *c != '`' && *c != '$')
                    .repeated()
                    .at_least(1)
                    .collect::<String>()
                    .map(|s| (vec![s], Vec::<Expr>::new())),
            ))
            .repeated()
            .collect::<Vec<_>>(),
        )
        .then_ignore(just('`'))
        .map(|parts: Vec<(Vec<String>, Vec<Expr>)>| {
            let mut quasis = Vec::new();
            let mut exprs = Vec::new();
            for (texts, exprs_part) in parts {
                for text in texts {
                    quasis.push(TplElement {
                        raw: text.clone(),
                        cooked: Some(text),
                        span: 0..0,
                    });
                }
                for expr in exprs_part {
                    exprs.push(expr);
                    quasis.push(TplElement {
                        raw: String::new(),
                        cooked: None,
                        span: 0..0,
                    });
                }
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
