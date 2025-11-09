use crate::diagnostics::{Diagnostic, Severity};
use oats_ast::*;

/// Compile-time constant values.
#[derive(Clone, Debug)]
pub enum ConstValue {
    /// f64 number
    Number(f64),
    /// Boolean
    Bool(bool),
    /// String
    Str(String),
    /// i64 integer
    I64(i64),
    /// i32 integer
    I32(i32),
    /// i16 integer
    I16(i16),
    /// i8 integer
    I8(i8),
    /// u64 integer
    U64(u64),
    /// u32 integer
    U32(u32),
    /// u16 integer
    U16(u16),
    /// u8 integer
    U8(u8),
    /// f32 float
    F32(f32),
    /// Character
    Char(char),
    /// Array of constants
    Array(Vec<ConstValue>),
    /// Object with string keys and constant values
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

/// Evaluates an expression as a compile-time constant.
///
/// Supports: literals, unary `-` and `!`, binary `+`, `-`, `*`, `/`, comparisons,
/// `&&`, `||`, conditionals, arrays, objects, and `Math.*` calls.
///
/// # Parameters
/// - `expr`: Expression to evaluate
/// - `span_start`: Source span start for error reporting
/// - `const_items`: Map of const item names to values (for identifier resolution)
pub fn eval_const_expr(
    expr: &Expr,
    span_start: usize,
    const_items: &std::collections::HashMap<String, ConstValue>,
) -> crate::diagnostics::DiagnosticResult<ConstValue> {
    match expr {
        Expr::Lit(l) => match l {
            Lit::F64(n) => Ok(ConstValue::Number(*n)),
            Lit::F32(n) => Ok(ConstValue::Number(*n as f64)),
            Lit::I64(n) => Ok(ConstValue::I64(*n)),
            Lit::I32(n) => Ok(ConstValue::I32(*n)),
            Lit::I16(n) => Ok(ConstValue::I16(*n)),
            Lit::I8(n) => Ok(ConstValue::I8(*n)),
            Lit::U64(n) => Ok(ConstValue::U64(*n)),
            Lit::U32(n) => Ok(ConstValue::U32(*n)),
            Lit::U16(n) => Ok(ConstValue::U16(*n)),
            Lit::U8(n) => Ok(ConstValue::U8(*n)),
            Lit::ISize(n) => Ok(ConstValue::I64(*n as i64)),
            Lit::USize(n) => Ok(ConstValue::I64(*n as i64)),
            Lit::Str(s) => Ok(ConstValue::Str(s.clone())),
            Lit::Bool(b) => Ok(ConstValue::Bool(*b)),
            _ => Err(Diagnostic::simple_with_span_boxed(
                Severity::Error,
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
                            Severity::Error,
                            "unary - on non-number in const initializer",
                            span_start,
                        ))
                    }
                }
                UnaryOp::Not => {
                    if let ConstValue::Bool(b) = inner {
                        Ok(ConstValue::Bool(!b))
                    } else {
                        Err(Diagnostic::simple_with_span_boxed(
                            Severity::Error,
                            "! on non-bool in const initializer",
                            span_start,
                        ))
                    }
                }
                _ => Err(Diagnostic::simple_with_span_boxed(
                    Severity::Error,
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
            match op {
                BinaryOp::Plus | BinaryOp::Minus | BinaryOp::Mul | BinaryOp::Div => {
                    if let (Some(lf), Some(rf)) = (l.as_f64(), r.as_f64()) {
                        let v = match op {
                            BinaryOp::Plus => lf + rf,
                            BinaryOp::Minus => lf - rf,
                            BinaryOp::Mul => lf * rf,
                            BinaryOp::Div => lf / rf,
                            _ => unreachable!(),
                        };
                        Ok(ConstValue::Number(v))
                    } else {
                        Err(Diagnostic::simple_with_span_boxed(
                            Severity::Error,
                            "arithmetic on non-number in const initializer",
                            span_start,
                        ))
                    }
                }
                BinaryOp::EqEq
                | BinaryOp::NotEq
                | BinaryOp::Gt
                | BinaryOp::Lt
                | BinaryOp::GtEq
                | BinaryOp::LtEq => {
                    // Comparisons: handle numbers and strings and bools conservatively
                    let res = match (l, r) {
                        (ConstValue::Number(a), ConstValue::Number(b)) => match op {
                            BinaryOp::EqEq => a == b,
                            BinaryOp::NotEq => a != b,
                            BinaryOp::Gt => a > b,
                            BinaryOp::Lt => a < b,
                            BinaryOp::GtEq => a >= b,
                            BinaryOp::LtEq => a <= b,
                            _ => false,
                        },
                        (ConstValue::Str(a), ConstValue::Str(b)) => match op {
                            BinaryOp::EqEq => a == b,
                            BinaryOp::NotEq => a != b,
                            _ => {
                                return Err(Diagnostic::simple_with_span_boxed(
                                    Severity::Error,
                                    "unsupported string comparison in const initializer",
                                    span_start,
                                ));
                            }
                        },
                        (ConstValue::Bool(a), ConstValue::Bool(b)) => match op {
                            BinaryOp::EqEq => a == b,
                            BinaryOp::NotEq => a != b,
                            _ => {
                                return Err(Diagnostic::simple_with_span_boxed(
                                    Severity::Error,
                                    "unsupported bool comparison in const initializer",
                                    span_start,
                                ));
                            }
                        },
                        _ => {
                            return Err(Diagnostic::simple_with_span_boxed(
                                Severity::Error,
                                "mismatched types in comparison in const initializer",
                                span_start,
                            ));
                        }
                    };
                    Ok(ConstValue::Bool(res))
                }
                BinaryOp::And => {
                    if l.is_truthy() {
                        Ok(r)
                    } else {
                        Ok(l)
                    }
                }
                BinaryOp::Or => {
                    if l.is_truthy() {
                        Ok(l)
                    } else {
                        Ok(r)
                    }
                }
                _ => Err(Diagnostic::simple_with_span_boxed(
                    Severity::Error,
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
                Severity::Error,
                "unknown const identifier in const initializer",
                span_start,
            ))
        }
        Expr::Array(arr) => {
            let mut out: Vec<ConstValue> = Vec::new();
            for opt in &arr.elems {
                if let Some(es) = opt {
                    let v = eval_const_expr(es, span_start, const_items)?;
                    out.push(v);
                } else {
                    return Err(Diagnostic::simple_with_span_boxed(
                        Severity::Error,
                        "elided array elements not supported in const initializer",
                        span_start,
                    ));
                }
            }
            Ok(ConstValue::Array(out))
        }
        Expr::Object(obj) => {
            let mut map = std::collections::HashMap::new();
            for prop in &obj.props {
                if let PropOrSpread::Prop(pb) = prop {
                    if let Prop::KeyValue(kv) = pb {
                        if let PropName::Ident(id) = &kv.key {
                            let key = id.sym.to_string();
                            let val = eval_const_expr(&kv.value, span_start, const_items)?;
                            map.insert(key, val);
                        } else {
                            return Err(Diagnostic::simple_with_span_boxed(
                                Severity::Error,
                                "unsupported object key in const initializer",
                                span_start,
                            ));
                        }
                    } else {
                        return Err(Diagnostic::simple_with_span_boxed(
                            Severity::Error,
                            "unsupported object property in const initializer",
                            span_start,
                        ));
                    }
                } else {
                    return Err(Diagnostic::simple_with_span_boxed(
                        Severity::Error,
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
            if let Callee::Expr(callee_expr) = &call.callee
                && let Expr::Member(member) = &**callee_expr
                && let (Expr::Ident(obj), MemberProp::Ident(prop)) = (&*member.obj, &member.prop)
                && obj.sym == "Math"
                && call.args.len() == 1
            {
                let arg = eval_const_expr(&call.args[0], span_start, const_items)?;
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
                Severity::Error,
                "function calls not supported in const initializer (except Math.*)",
                span_start,
            ))
        }
        _ => Err(Diagnostic::simple_with_span_boxed(
            Severity::Error,
            "expression not supported in const initializer",
            span_start,
        )),
    }
}
