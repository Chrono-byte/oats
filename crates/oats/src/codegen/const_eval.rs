use crate::diagnostics::Diagnostic;
use deno_ast::swc::ast;

/// Small enum representing compile-time constant values supported in phase 1.
#[derive(Clone, Debug)]
pub enum ConstValue {
    Number(f64),
    Bool(bool),
    Str(String),
    Array(Vec<ConstValue>),
    Object(std::collections::HashMap<String, ConstValue>),
}

impl ConstValue {
    pub fn as_f64(&self) -> Option<f64> {
        if let ConstValue::Number(n) = self {
            Some(*n)
        } else {
            None
        }
    }
}

/// Evaluate a limited subset of expressions as compile-time constants.
/// Supported: numeric, boolean, string literals, unary -, binary + - * /, comparisons
pub fn eval_const_expr(
    expr: &ast::Expr,
    span_start: usize,
    const_items: &std::collections::HashMap<String, ConstValue>,
) -> Result<ConstValue, Diagnostic> {
    use ast::{BinExpr, BinaryOp, Expr, Lit, UnaryExpr, UnaryOp};

    match expr {
        Expr::Lit(l) => match l {
            Lit::Num(n) => Ok(ConstValue::Number(n.value)),
            Lit::Str(s) => Ok(ConstValue::Str(s.value.to_string())),
            Lit::Bool(b) => Ok(ConstValue::Bool(b.value)),
            _ => Err(Diagnostic::simple_with_span(
                "unsupported literal in const initializer",
                span_start,
            )),
        },
        Expr::Unary(UnaryExpr { op, arg, .. }) => {
            let inner = eval_const_expr(&*arg, span_start, const_items)?;
            match op {
                UnaryOp::Minus => {
                    if let Some(n) = inner.as_f64() {
                        Ok(ConstValue::Number(-n))
                    } else {
                        Err(Diagnostic::simple_with_span(
                            "unary - on non-number in const initializer",
                            span_start,
                        ))
                    }
                }
                UnaryOp::Bang => {
                    if let ConstValue::Bool(b) = inner {
                        Ok(ConstValue::Bool(!b))
                    } else {
                        Err(Diagnostic::simple_with_span(
                            "! on non-bool in const initializer",
                            span_start,
                        ))
                    }
                }
                _ => Err(Diagnostic::simple_with_span(
                    "unsupported unary op in const initializer",
                    span_start,
                )),
            }
        }
        Expr::Bin(BinExpr {
            op, left, right, ..
        }) => {
            let l = eval_const_expr(&*left, span_start, const_items)?;
            let r = eval_const_expr(&*right, span_start, const_items)?;
            use BinaryOp::*;
            match op {
                Add | Sub | Mul | Div => {
                    if let (Some(lf), Some(rf)) = (l.as_f64(), r.as_f64()) {
                        let v = match op {
                            Add => lf + rf,
                            Sub => lf - rf,
                            Mul => lf * rf,
                            Div => lf / rf,
                            _ => unreachable!(),
                        };
                        Ok(ConstValue::Number(v))
                    } else {
                        Err(Diagnostic::simple_with_span(
                            "arithmetic on non-number in const initializer",
                            span_start,
                        ))
                    }
                }
                EqEq | NotEq | Gt | Lt | GtEq | LtEq => {
                    // Comparisons: handle numbers and strings and bools conservatively
                    let res = match (l, r) {
                        (ConstValue::Number(a), ConstValue::Number(b)) => match op {
                            EqEq => a == b,
                            NotEq => a != b,
                            Gt => a > b,
                            Lt => a < b,
                            GtEq => a >= b,
                            LtEq => a <= b,
                            _ => false,
                        },
                        (ConstValue::Str(a), ConstValue::Str(b)) => match op {
                            EqEq => a == b,
                            NotEq => a != b,
                            _ => {
                                return Err(Diagnostic::simple_with_span(
                                    "unsupported string comparison in const initializer",
                                    span_start,
                                ));
                            }
                        },
                        (ConstValue::Bool(a), ConstValue::Bool(b)) => match op {
                            EqEq => a == b,
                            NotEq => a != b,
                            _ => {
                                return Err(Diagnostic::simple_with_span(
                                    "unsupported bool comparison in const initializer",
                                    span_start,
                                ));
                            }
                        },
                        _ => {
                            return Err(Diagnostic::simple_with_span(
                                "mismatched types in comparison in const initializer",
                                span_start,
                            ));
                        }
                    };
                    Ok(ConstValue::Bool(res))
                }
                _ => Err(Diagnostic::simple_with_span(
                    "unsupported binary op in const initializer",
                    span_start,
                )),
            }
        }
        Expr::Ident(id) => {
            let name = id.sym.to_string();
            if let Some(v) = const_items.get(&name) {
                return Ok(v.clone());
            }
            return Err(Diagnostic::simple_with_span(
                "unknown const identifier in const initializer",
                span_start,
            ));
        }
        Expr::Array(arr) => {
            let mut out: Vec<ConstValue> = Vec::new();
            for opt in &arr.elems {
                if let Some(es) = opt {
                    let v = eval_const_expr(&es.expr, span_start, const_items)?;
                    out.push(v);
                } else {
                    return Err(Diagnostic::simple_with_span(
                        "elided array elements not supported in const initializer",
                        span_start,
                    ));
                }
            }
            Ok(ConstValue::Array(out))
        }
        Expr::Object(obj) => {
            use deno_ast::swc::ast::{Prop, PropName, PropOrSpread};
            let mut map = std::collections::HashMap::new();
            for prop in &obj.props {
                if let PropOrSpread::Prop(pb) = prop {
                    if let Prop::KeyValue(kv) = &**pb {
                        if let PropName::Ident(id) = &kv.key {
                            let key = id.sym.to_string();
                            let val = eval_const_expr(&kv.value, span_start, const_items)?;
                            map.insert(key, val);
                        } else {
                            return Err(Diagnostic::simple_with_span(
                                "unsupported object key in const initializer",
                                span_start,
                            ));
                        }
                    } else {
                        return Err(Diagnostic::simple_with_span(
                            "unsupported object property in const initializer",
                            span_start,
                        ));
                    }
                } else {
                    return Err(Diagnostic::simple_with_span(
                        "unsupported spread in const object initializer",
                        span_start,
                    ));
                }
            }
            Ok(ConstValue::Object(map))
        }
        _ => Err(Diagnostic::simple_with_span(
            "expression not supported in const initializer",
            span_start,
        )),
    }
}
