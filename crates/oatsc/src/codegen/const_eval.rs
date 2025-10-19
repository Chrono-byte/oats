use crate::diagnostics::Diagnostic;
use deno_ast::swc::ast;

/// Small enum representing compile-time constant values supported in phase 1.
#[derive(Clone, Debug)]
pub enum ConstValue {
    Number(f64),
    Bool(bool),
    Str(String),
    I64(i64),
    I32(i32),
    I16(i16),
    I8(i8),
    U64(u64),
    U32(u32),
    U16(u16),
    U8(u8),
    F32(f32),
    Char(char),
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

    pub fn is_truthy(&self) -> bool {
        match self {
            ConstValue::Number(n) => *n != 0.0 && !n.is_nan(),
            ConstValue::Bool(b) => *b,
            ConstValue::Str(s) => !s.is_empty(),
            ConstValue::I64(n) => *n != 0,
            ConstValue::I32(n) => *n != 0,
            ConstValue::I16(n) => *n != 0,
            ConstValue::I8(n) => *n != 0,
            ConstValue::U64(n) => *n != 0,
            ConstValue::U32(n) => *n != 0,
            ConstValue::U16(n) => *n != 0,
            ConstValue::U8(n) => *n != 0,
            ConstValue::F32(n) => *n != 0.0 && !n.is_nan(),
            ConstValue::Char(c) => *c != '\0',
            ConstValue::Array(_) | ConstValue::Object(_) => true,
        }
    }
}

/// Evaluate a limited subset of expressions as compile-time constants.
/// Supported: numeric, boolean, string literals, unary -, binary + - * /, comparisons
pub fn eval_const_expr(
    expr: &ast::Expr,
    span_start: usize,
    const_items: &std::collections::HashMap<String, ConstValue>,
) -> crate::diagnostics::DiagnosticResult<ConstValue> {
    use ast::{BinExpr, BinaryOp, Expr, Lit, UnaryExpr, UnaryOp};

    match expr {
        Expr::Lit(l) => match l {
            Lit::Num(n) => Ok(ConstValue::Number(n.value)),
            Lit::Str(s) => Ok(ConstValue::Str(s.value.to_string())),
            Lit::Bool(b) => Ok(ConstValue::Bool(b.value)),
            _ => Err(Diagnostic::simple_with_span_boxed(
                "unsupported literal in const initializer",
                span_start,
            )),
        },
        Expr::Unary(UnaryExpr { op, arg, .. }) => {
            let inner = eval_const_expr(arg, span_start, const_items)?;
            match op {
                UnaryOp::Minus => {
                    if let Some(n) = inner.as_f64() {
                        Ok(ConstValue::Number(-n))
                    } else {
                        Err(Diagnostic::simple_with_span_boxed(
                            "unary - on non-number in const initializer",
                            span_start,
                        ))
                    }
                }
                UnaryOp::Bang => {
                    if let ConstValue::Bool(b) = inner {
                        Ok(ConstValue::Bool(!b))
                    } else {
                        Err(Diagnostic::simple_with_span_boxed(
                            "! on non-bool in const initializer",
                            span_start,
                        ))
                    }
                }
                _ => Err(Diagnostic::simple_with_span_boxed(
                    "unsupported unary op in const initializer",
                    span_start,
                )),
            }
        }
        Expr::Bin(BinExpr {
            op, left, right, ..
        }) => {
            let l = eval_const_expr(left, span_start, const_items)?;
            let r = eval_const_expr(right, span_start, const_items)?;
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
                        Err(Diagnostic::simple_with_span_boxed(
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
                                return Err(Diagnostic::simple_with_span_boxed(
                                    "unsupported string comparison in const initializer",
                                    span_start,
                                ));
                            }
                        },
                        (ConstValue::Bool(a), ConstValue::Bool(b)) => match op {
                            EqEq => a == b,
                            NotEq => a != b,
                            _ => {
                                return Err(Diagnostic::simple_with_span_boxed(
                                    "unsupported bool comparison in const initializer",
                                    span_start,
                                ));
                            }
                        },
                        _ => {
                            return Err(Diagnostic::simple_with_span_boxed(
                                "mismatched types in comparison in const initializer",
                                span_start,
                            ));
                        }
                    };
                    Ok(ConstValue::Bool(res))
                }
                LogicalAnd => {
                    if l.is_truthy() {
                        Ok(r)
                    } else {
                        Ok(l)
                    }
                }
                LogicalOr => {
                    if l.is_truthy() {
                        Ok(l)
                    } else {
                        Ok(r)
                    }
                }
                _ => Err(Diagnostic::simple_with_span_boxed(
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
            Err(Diagnostic::simple_with_span_boxed(
                "unknown const identifier in const initializer",
                span_start,
            ))
        }
        Expr::Array(arr) => {
            let mut out: Vec<ConstValue> = Vec::new();
            for opt in &arr.elems {
                if let Some(es) = opt {
                    let v = eval_const_expr(&es.expr, span_start, const_items)?;
                    out.push(v);
                } else {
                    return Err(Diagnostic::simple_with_span_boxed(
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
                            return Err(Diagnostic::simple_with_span_boxed(
                                "unsupported object key in const initializer",
                                span_start,
                            ));
                        }
                    } else {
                        return Err(Diagnostic::simple_with_span_boxed(
                            "unsupported object property in const initializer",
                            span_start,
                        ));
                    }
                } else {
                    return Err(Diagnostic::simple_with_span_boxed(
                        "unsupported spread in const object initializer",
                        span_start,
                    ));
                }
            }
            Ok(ConstValue::Object(map))
        }
        Expr::Cond(cond) => {
            let test = eval_const_expr(&cond.test, span_start, const_items)?;
            if test.is_truthy() {
                eval_const_expr(&cond.cons, span_start, const_items)
            } else {
                eval_const_expr(&cond.alt, span_start, const_items)
            }
        }
        Expr::Call(call) => {
            // For now, support simple built-in functions
            if let ast::Callee::Expr(callee_expr) = &call.callee
                && let ast::Expr::Member(member) = &**callee_expr
                && let (ast::Expr::Ident(obj), ast::MemberProp::Ident(prop)) =
                    (&*member.obj, &member.prop)
                && obj.sym == "Math"
                && call.args.len() == 1
            {
                let arg = eval_const_expr(&call.args[0].expr, span_start, const_items)?;
                if let Some(n) = arg.as_f64() {
                    match prop.sym.as_str() {
                        "abs" => return Ok(ConstValue::Number(n.abs())),
                        "floor" => return Ok(ConstValue::Number(n.floor())),
                        "ceil" => return Ok(ConstValue::Number(n.ceil())),
                        "round" => return Ok(ConstValue::Number(n.round())),
                        "sqrt" => return Ok(ConstValue::Number(n.sqrt())),
                        _ => {}
                    }
                }
            }
            Err(Diagnostic::simple_with_span_boxed(
                "function calls not supported in const initializer (except Math.*)",
                span_start,
            ))
        }
        _ => Err(Diagnostic::simple_with_span_boxed(
            "expression not supported in const initializer",
            span_start,
        )),
    }
}
