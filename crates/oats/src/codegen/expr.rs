use crate::diagnostics::Diagnostic;
use inkwell::values::BasicValueEnum;
use inkwell::values::FunctionValue;
use std::collections::HashMap;

use inkwell::AddressSpace;
use inkwell::builder::Builder;
use inkwell::types::BasicType;
use inkwell::types::BasicTypeEnum;
use inkwell::values::{BasicValue, PointerValue};
type LocalEntry<'a> = (PointerValue<'a>, BasicTypeEnum<'a>, bool, bool, bool);
type LocalsStackLocal<'a> = Vec<HashMap<String, LocalEntry<'a>>>;

impl<'a> crate::codegen::CodeGen<'a> {
    // Return true if the given OatsType contains or is a pointer-like type.
    /// Main expression lowering function. Returns Result to provide better error messages
    /// when lowering fails (instead of just returning None).
    pub fn lower_expr(
        &self,
        expr: &deno_ast::swc::ast::Expr,
        function: FunctionValue<'a>,
        param_map: &HashMap<String, u32>,
        locals: &mut LocalsStackLocal<'a>,
    ) -> Result<BasicValueEnum<'a>, Diagnostic> {
        use deno_ast::swc::ast;

        match expr {
            ast::Expr::Bin(bin) => {
                use deno_ast::swc::ast::BinaryOp;
                use inkwell::FloatPredicate;

                // Special-case: typeof <expr> === "string"/"number" patterns
                if let ast::Expr::Unary(unary) = &*bin.left
                    && let deno_ast::swc::ast::UnaryOp::TypeOf = unary.op
                    && let ast::Expr::Lit(deno_ast::swc::ast::Lit::Str(s)) = &*bin.right
                {
                    // Lower inner expression and then check union discriminant if it's a pointer
                    let inner = self.lower_expr(&unary.arg, function, param_map, locals)?;
                    if let BasicValueEnum::PointerValue(pv) = inner {
                        // For pointer-like values, consult the union discriminant helper.
                        // Runtime layout: union-boxed objects expose a discriminant word
                        // (0=number, 1=string, 2=boolean/other). Calling the helper
                        // allows `typeof` guards and comparisons to work on boxed unions.
                        let get_disc = self.get_union_get_discriminant();
                        let cs = self.builder.build_call(get_disc, &[pv.into()], "get_disc");
                        if let Ok(cs) = cs {
                            if let inkwell::Either::Left(bv) = cs.try_as_basic_value() {
                                let disc = bv.into_int_value();
                                let expected = match s.value.as_ref() {
                                    "number" => self.i64_t.const_int(0, false),
                                    "string" => self.i64_t.const_int(1, false),
                                    _ => self.i64_t.const_int(2, false),
                                };
                                if let Ok(cmp_iv) = self.builder.build_int_compare(inkwell::IntPredicate::EQ, disc, expected, "typeof_eq") {
                                    return Ok(cmp_iv.as_basic_value_enum());
                                }
                            }
                        }
                    }
                    // fallback to general lowering below
                }

                let l = self.lower_expr(&bin.left, function, param_map, locals)?;
                let r = self.lower_expr(&bin.right, function, param_map, locals)?;

                // coercion now handled by self.coerce_to_f64

                // Helper to handle float arithmetic
                let float_bin = |builder: &Builder<'a>,
                                 lf: inkwell::values::FloatValue<'a>,
                                 rf: inkwell::values::FloatValue<'a>,
                                 op: &BinaryOp|
                 -> Result<BasicValueEnum<'a>, Diagnostic> {
                    match op {
                        BinaryOp::Add => {
                            let v = builder
                                .build_float_add(lf, rf, "sum")
                                .map_err(|_| Diagnostic::simple("float add failed"))?;
                            Ok(v.as_basic_value_enum())
                        }
                        BinaryOp::Sub => {
                            let v = builder
                                .build_float_sub(lf, rf, "sub")
                                .map_err(|_| Diagnostic::simple("float sub failed"))?;
                            Ok(v.as_basic_value_enum())
                        }
                        BinaryOp::Mul => {
                            let v = builder
                                .build_float_mul(lf, rf, "mul")
                                .map_err(|_| Diagnostic::simple("float mul failed"))?;
                            Ok(v.as_basic_value_enum())
                        }
                        BinaryOp::Div => {
                            let v = builder
                                .build_float_div(lf, rf, "div")
                                .map_err(|_| Diagnostic::simple("float div failed"))?;
                            Ok(v.as_basic_value_enum())
                        }
                        _ => Err(Diagnostic::simple("unsupported binary operation")),
                    }
                };

                // Helper to handle float comparisons -> i1
                let float_cmp = |builder: &Builder<'a>,
                                 lf: inkwell::values::FloatValue<'a>,
                                 rf: inkwell::values::FloatValue<'a>,
                                 op: &BinaryOp|
                 -> Result<BasicValueEnum<'a>, Diagnostic> {
                    let pred = match op {
                        BinaryOp::Lt => FloatPredicate::OLT,
                        BinaryOp::LtEq => FloatPredicate::OLE,
                        BinaryOp::Gt => FloatPredicate::OGT,
                        BinaryOp::GtEq => FloatPredicate::OGE,
                        BinaryOp::EqEq => FloatPredicate::OEQ,
                        BinaryOp::NotEq => FloatPredicate::ONE,
                        _ => return Err(Diagnostic::simple("unsupported comparison operation")),
                    };
                    let iv = builder
                        .build_float_compare(pred, lf, rf, "cmp")
                        .map_err(|_| Diagnostic::simple("float compare failed"))?;
                    Ok(iv.as_basic_value_enum())
                };

                // Try float path by coercing ints/bools to float as needed
                if let (Some(lf), Some(rf)) = (self.coerce_to_f64(l), self.coerce_to_f64(r)) {
                    if let Ok(v) = float_bin(&self.builder, lf, rf, &bin.op) {
                        return Ok(v);
                    }
                    if let Ok(v) = float_cmp(&self.builder, lf, rf, &bin.op) {
                        return Ok(v);
                    }
                }

                // Logical operators: short-circuiting
                if let deno_ast::swc::ast::BinaryOp::LogicalAnd = bin.op {
                    // a && b -> if a truthy then b else a
                    let left_val = l;
                    let cond = self.to_condition_i1(left_val).ok_or_else(|| {
                        Diagnostic::simple("failed to convert to boolean condition")
                    })?;
                    let then_bb = self.context.append_basic_block(function, "and.then");
                    let else_bb = self.context.append_basic_block(function, "and.else");
                    let merge_bb = self.context.append_basic_block(function, "and.merge");
                    if self
                        .builder
                        .build_conditional_branch(cond, then_bb, else_bb)
                        .is_err()
                    {
                        return Err(Diagnostic::simple("expression lowering failed"))?;
                    }
                    // then: evaluate right
                    self.builder.position_at_end(then_bb);
                    let rv = self.lower_expr(&bin.right, function, param_map, locals);
                    if self.builder.get_insert_block().is_some()
                        && self.builder.build_unconditional_branch(merge_bb).is_err()
                    {
                        return Err(Diagnostic::simple("expression lowering failed"))?;
                    }
                    // else: keep left
                    self.builder.position_at_end(else_bb);
                    if self.builder.get_insert_block().is_some() {
                        let _ = self
                            .builder
                            .build_unconditional_branch(merge_bb)
                            .map_err(|_| Diagnostic::simple("LLVM builder error"))?;
                    }
                    self.builder.position_at_end(merge_bb);
                    if let Ok(rval) = rv
                        && let Some(phi) = self.build_phi_merge(then_bb, else_bb, rval, left_val)
                    {
                        return Ok(phi);
                    }
                    return Err(Diagnostic::simple("expression lowering failed"))?;
                }
                if let deno_ast::swc::ast::BinaryOp::LogicalOr = bin.op {
                    // a || b -> if a truthy then a else b
                    let left_val = l;
                    let cond = self.to_condition_i1(left_val).ok_or_else(|| {
                        Diagnostic::simple("failed to convert to boolean condition")
                    })?;
                    let then_bb = self.context.append_basic_block(function, "or.then");
                    let else_bb = self.context.append_basic_block(function, "or.else");
                    let merge_bb = self.context.append_basic_block(function, "or.merge");
                    if self
                        .builder
                        .build_conditional_branch(cond, then_bb, else_bb)
                        .is_err()
                    {
                        return Err(Diagnostic::simple("expression lowering failed"))?;
                    }
                    // then: keep left
                    self.builder.position_at_end(then_bb);
                    if self.builder.get_insert_block().is_some()
                        && self.builder.build_unconditional_branch(merge_bb).is_err()
                    {
                        return Err(Diagnostic::simple("expression lowering failed"))?;
                    }
                    // else: evaluate right
                    self.builder.position_at_end(else_bb);
                    let rv = self.lower_expr(&bin.right, function, param_map, locals);
                    if self.builder.get_insert_block().is_some() {
                        let _ = self
                            .builder
                            .build_unconditional_branch(merge_bb)
                            .map_err(|_| Diagnostic::simple("LLVM builder error"))?;
                    }
                    self.builder.position_at_end(merge_bb);
                    if let Ok(rval) = rv
                        && let Some(phi) = self.build_phi_merge(then_bb, else_bb, left_val, rval)
                    {
                        return Ok(phi);
                    }
                    return Err(Diagnostic::simple("expression lowering failed"))?;
                }

                // Otherwise, handle pointer concat for Add and pointer equality
                match (l, r) {
                    (BasicValueEnum::PointerValue(lp), BasicValueEnum::PointerValue(rp)) => {
                        // Keep string concat behavior for Add
                        if let BinaryOp::Add = bin.op {
                            if let Some(strcat) = self.module.get_function("str_concat") {
                                let call_site = match self.builder.build_call(
                                    strcat,
                                    &[lp.into(), rp.into()],
                                    "concat",
                                ) {
                                    Ok(cs) => cs,
                                    Err(_) => return Err(Diagnostic::simple("operation failed")),
                                };
                                let either = call_site.try_as_basic_value();
                                match either {
                                    inkwell::Either::Left(bv) => Ok(bv),
                                    _ => Err(Diagnostic::simple("operation not supported")),
                                }
                            } else {
                                Err(Diagnostic::simple("unsupported expression"))
                            }
                        } else {
                            Err(Diagnostic::simple("unsupported expression"))
                        }
                    }
                    _ => Err(Diagnostic::simple("operation not supported")),
                }
            }
            ast::Expr::Ident(id) => {
                let name = id.sym.to_string();

                // First, check if the identifier is a function parameter.
                if let Some(idx) = param_map.get(&name)
                    && let Some(pv) = function.get_nth_param(*idx)
                {
                    return Ok(pv);
                }

                // If not a parameter, then it must be a local variable (`let` or `const`).
                if let Some((ptr, ty, initialized, _is_const, _extra)) = self.find_local(locals, &name) {
                    // If not initialized -> TDZ: generate a trap (unreachable)
                    if !initialized {
                        // Emit a call to unreachable to trap at runtime
                        let _ = self.builder.build_unreachable();
                        return Err(Diagnostic::simple("expression lowering failed"))?;
                    }
                    let loaded = match self.builder.build_load(ty, ptr, &name) {
                        Ok(v) => v,
                        Err(_) => return Err(Diagnostic::simple("operation failed")),
                    };
                    return Ok(loaded);
                }
                Err(Diagnostic::simple("unsupported expression"))
            }
            ast::Expr::Call(call) => {
                // Support simple identifier callees and member-callee method calls
                if let ast::Callee::Expr(boxed_expr) = &call.callee {
                    match &**boxed_expr {
                        ast::Expr::Ident(ident) => {
                            let fname = ident.sym.to_string();

                            // Special-case: println(x) -> call runtime print helpers
                            if fname == "println" {
                                if call.args.len() != 1 {
                                    return Err(Diagnostic::simple("expression lowering failed"))?;
                                }
                                let a = &call.args[0];
                                if let Ok(val) =
                                    self.lower_expr(&a.expr, function, param_map, locals)
                                {
                                    match val {
                                        BasicValueEnum::FloatValue(fv) => {
                                            if let Some(print_fn) =
                                                self.module.get_function("print_f64")
                                            {
                                                let _ = self
                                                    .builder
                                                    .build_call(
                                                        print_fn,
                                                        &[fv.into()],
                                                        "print_f64_call",
                                                    )
                                                    .ok();
                                            }
                                            return Err(Diagnostic::simple(
                                                "expression lowering failed",
                                            ))?;
                                        }
                                        BasicValueEnum::PointerValue(pv) => {
                                            if let Some(print_fn) =
                                                self.module.get_function("print_str")
                                            {
                                                let _ = self
                                                    .builder
                                                    .build_call(
                                                        print_fn,
                                                        &[pv.into()],
                                                        "print_str_call",
                                                    )
                                                    .ok();
                                            }
                                            return Err(Diagnostic::simple(
                                                "expression lowering failed",
                                            ))?;
                                        }
                                        _ => return Err(Diagnostic::simple("operation failed")),
                                    }
                                } else {
                                    return Err(Diagnostic::simple("expression lowering failed"))?;
                                }
                            }

                            if let Some(fv) = self.module.get_function(&fname) {
                                // Lower args
                                let mut lowered_args: Vec<inkwell::values::BasicMetadataValueEnum> =
                                    Vec::new();
                                for a in &call.args {
                                    if let Ok(val) =
                                        self.lower_expr(&a.expr, function, param_map, locals)
                                    {
                                        lowered_args.push(val.into());
                                    } else {
                                        return Err(Diagnostic::simple(
                                            "expression lowering failed",
                                        ))?;
                                    }
                                }
                                let cs = match self.builder.build_call(
                                    fv,
                                    &lowered_args,
                                    "call_internal",
                                ) {
                                    Ok(cs) => cs,
                                    Err(_) => return Err(Diagnostic::simple("operation failed")),
                                };
                                let either = cs.try_as_basic_value();
                                match either {
                                    inkwell::Either::Left(bv) => Ok(bv),
                                    _ => Err(Diagnostic::simple("operation not supported")),
                                }
                            } else {
                                Err(Diagnostic::simple("unsupported expression"))
                            }
                        }
                        ast::Expr::Member(member) => {
                            use deno_ast::swc::ast::MemberProp;
                            if let MemberProp::Ident(prop_ident) = &member.prop {
                                let method_name = prop_ident.sym.to_string();

                                // Special-case Math.random() -> call runtime math_random()
                                if method_name == "random" {
                                    // If the object expression is the identifier `Math` and there are no args
                                    if call.args.is_empty()
                                        && let deno_ast::swc::ast::Expr::Ident(ident) = &*member.obj
                                        && ident.sym == "Math"
                                    {
                                        let f = self.get_math_random();
                                        let cs = match self.builder.build_call(
                                            f,
                                            &[],
                                            "math_random_call",
                                        ) {
                                            Ok(cs) => cs,
                                            Err(_) => {
                                                return Err(Diagnostic::simple("operation failed"));
                                            }
                                        };
                                        let either = cs.try_as_basic_value();
                                        if let inkwell::Either::Left(bv) = either {
                                            return Ok(bv);
                                        } else {
                                            return Err(Diagnostic::simple("operation failed"));
                                        }
                                    }
                                }

                                if let Ok(obj_val) =
                                    self.lower_expr(&member.obj, function, param_map, locals)
                                {
                                        // Support weak reference helpers: downgrade() and upgrade()
                                        if method_name == "downgrade" {
                                            // Expect no args
                                            if !call.args.is_empty() {
                                                return Err(Diagnostic::simple("expression lowering failed"))?;
                                            }
                                            if let BasicValueEnum::PointerValue(pv) = obj_val {
                                                let f = self.get_rc_weak_inc();
                                                let _ = self.builder.build_call(f, &[pv.into()], "rc_weak_inc_call");
                                                return Ok(pv.as_basic_value_enum());
                                            } else {
                                                return Err(Diagnostic::simple("downgrade on non-pointer"))?;
                                            }
                                        }
                                        if method_name == "upgrade" {
                                            // Expect no args
                                            if !call.args.is_empty() {
                                                return Err(Diagnostic::simple("expression lowering failed"))?;
                                            }
                                            if let BasicValueEnum::PointerValue(pv) = obj_val {
                                                let f = self.get_rc_weak_upgrade();
                                                let cs = match self.builder.build_call(f, &[pv.into()], "rc_weak_upgrade_call") {
                                                    Ok(cs) => cs,
                                                    Err(_) => return Err(Diagnostic::simple("operation failed")),
                                                };
                                                let either = cs.try_as_basic_value();
                                                if let inkwell::Either::Left(bv) = either {
                                                    return Ok(bv);
                                                } else {
                                                    return Err(Diagnostic::simple("operation not supported"));
                                                }
                                            } else {
                                                return Err(Diagnostic::simple("upgrade on non-pointer"))?;
                                            }
                                        }
                                    // Special-case: arr.push(x) / arr.pop()
                                    if method_name == "push" {
                                        // Expect one argument
                                        if call.args.len() == 1 {
                                            let arg = &call.args[0];
                                            if let Ok(arg_val) = self
                                                .lower_expr(&arg.expr, function, param_map, locals)
                                            {
                                                // Dispatch based on arg type: f64 vs pointer
                                                match arg_val {
                                                    BasicValueEnum::FloatValue(fv) => {
                                                        let f = self.get_array_push_f64();
                                                        let _ = self.builder.build_call(
                                                            f,
                                                            &[obj_val.into(), fv.into()],
                                                            "arr_push_f64",
                                                        );
                                                        return Ok(self
                                                            .context
                                                            .i64_type()
                                                            .const_int(0, false)
                                                            .as_basic_value_enum());
                                                    }
                                                    BasicValueEnum::PointerValue(pv) => {
                                                        let f = self.get_array_push_ptr();
                                                        let _ = self.builder.build_call(
                                                            f,
                                                            &[obj_val.into(), pv.into()],
                                                            "arr_push_ptr",
                                                        );
                                                        return Ok(self
                                                            .context
                                                            .i64_type()
                                                            .const_int(0, false)
                                                            .as_basic_value_enum());
                                                    }
                                                    _ => {
                                                        return Err(Diagnostic::simple(
                                                            "unsupported argument type for push",
                                                        ));
                                                    }
                                                }
                                            }
                                        }
                                    }
                                    if method_name == "pop" {
                                        // Expect no args
                                        if call.args.is_empty() {
                                            // We'll try number pop first, then pointer pop.
                                            // Call array_pop_f64
                                            let fnum = self.get_array_pop_f64();
                                            let cs = match self.builder.build_call(
                                                fnum,
                                                &[obj_val.into()],
                                                "arr_pop_f64",
                                            ) {
                                                Ok(cs) => cs,
                                                Err(_) => {
                                                    return Err(Diagnostic::simple(
                                                        "operation failed",
                                                    ));
                                                }
                                            };
                                            let either = cs.try_as_basic_value();
                                            if let inkwell::Either::Left(bv) = either {
                                                return Ok(bv);
                                            }
                                            // Fallback to pointer pop
                                            let fptr = self.get_array_pop_ptr();
                                            let cs2 = match self.builder.build_call(
                                                fptr,
                                                &[obj_val.into()],
                                                "arr_pop_ptr",
                                            ) {
                                                Ok(cs2) => cs2,
                                                Err(_) => {
                                                    return Err(Diagnostic::simple(
                                                        "operation failed",
                                                    ));
                                                }
                                            };
                                            let either2 = cs2.try_as_basic_value();
                                            if let inkwell::Either::Left(bv2) = either2 {
                                                return Ok(bv2);
                                            }
                                            return Err(Diagnostic::simple("operation failed"));
                                        }
                                    }
                                    // try to find a function named `<Class>_<method>`
                                    for class_name in self.class_fields.borrow().keys() {
                                        let cand = format!("{}_{}", class_name, method_name);
                                        if let Some(method_f) = self.module.get_function(&cand) {
                                            // lower user args
                                            let mut user_args: Vec<
                                                inkwell::values::BasicMetadataValueEnum,
                                            > = Vec::new();
                                            for a in &call.args {
                                                if let Ok(v) = self.lower_expr(
                                                    &a.expr, function, param_map, locals,
                                                ) {
                                                    user_args.push(v.into());
                                                } else {
                                                    return Err(Diagnostic::simple(
                                                        "expression lowering failed",
                                                    ))?;
                                                }
                                            }

                                            // if the method expects an extra param (likely `this`), prepend obj_val
                                            let param_count = method_f.count_params() as usize;
                                            let mut args: Vec<
                                                inkwell::values::BasicMetadataValueEnum,
                                            > = Vec::new();
                                            if param_count > user_args.len() {
                                                args.push(obj_val.into());
                                                args.extend(user_args.into_iter());
                                            } else {
                                                args = user_args;
                                            }

                                            let cs = match self.builder.build_call(
                                                method_f,
                                                &args,
                                                "call_method",
                                            ) {
                                                Ok(cs) => cs,
                                                Err(_) => {
                                                    return Err(Diagnostic::simple(
                                                        "operation failed",
                                                    ));
                                                }
                                            };
                                            let either = cs.try_as_basic_value();
                                            if let inkwell::Either::Left(bv) = either {
                                                return Ok(bv);
                                            } else {
                                                return Err(Diagnostic::simple(
                                                    "expression lowering failed",
                                                ))?;
                                            }
                                        }
                                    }
                                }
                            }
                            Err(Diagnostic::simple("unsupported expression"))
                        }
                        _ => Err(Diagnostic::simple("operation not supported")),
                    }
                } else {
                    Err(Diagnostic::simple("unsupported expression"))
                }
            }
            ast::Expr::Assign(assign) => {
                // support simple assignments `ident = expr` where the left side is an identifier
                if let Some(bid) = assign.left.as_ident() {
                    let name = bid.id.sym.to_string();
                    if let Some((ptr, _ty, _init, is_const, _extra)) = self.find_local(locals, &name) {
                        // Disallow assigning to immutable locals at compile-time
                        if is_const {
                            // Use span-aware diagnostic: assign.span.lo is a BytePos wrapper
                            let span_start = assign.span.lo.0 as usize;
                            let _ = crate::diagnostics::report_error_span_and_bail::<()>(
                                None,
                                self.source,
                                span_start,
                                "assignment to immutable variable",
                                Some(
                                    "This variable was not declared mutable (use `let mut` to make it mutable).",
                                ),
                            );
                            return Err(Diagnostic::simple("expression lowering failed"))?;
                        }
                        if let Ok(val) = self.lower_expr(&assign.right, function, param_map, locals)
                        {
                            // If the local is a pointer type, and previously initialized, decrement old refcount
                            if _ty == self.i8ptr_t.as_basic_type_enum() {
                                // load old value
                                let old =
                                    match self.builder.build_load(self.i8ptr_t, ptr, "old_val") {
                                        Ok(v) => v,
                                        Err(_) => {
                                            return Err(Diagnostic::simple("operation failed"));
                                        }
                                    };
                                // check initialized flag
                                if _init {
                                    let rc_dec = self.get_rc_dec();
                                    let _ = match self.builder.build_call(
                                        rc_dec,
                                        &[old.into()],
                                        "rc_dec_old",
                                    ) {
                                        Ok(cs) => cs,
                                        Err(_) => {
                                            return Err(Diagnostic::simple("operation failed"));
                                        }
                                    };
                                }
                                // store new value
                                let _ = self.builder.build_store(ptr, val);
                                // increment refcount of new value
                                if let BasicValueEnum::PointerValue(newpv) = val {
                                    let rc_inc = self.get_rc_inc();
                                    let _ = match self.builder.build_call(
                                        rc_inc,
                                        &[newpv.into()],
                                        "rc_inc_assign",
                                    ) {
                                        Ok(cs) => cs,
                                        Err(_) => {
                                            return Err(Diagnostic::simple("operation failed"));
                                        }
                                    };
                                }
                            } else {
                                let _ = self.builder.build_store(ptr, val);
                            }
                            // mark initialized after assignment
                            self.set_local_initialized(locals, &name, true);
                            return Ok(val);
                        }
                    }
                }

                // Handle member assignment: obj.field = expr
                // Check if left side is a member expression
                use deno_ast::swc::ast::{AssignTarget, SimpleAssignTarget};
                if let AssignTarget::Simple(SimpleAssignTarget::Member(member)) = &assign.left {
                    // Lower the right-hand side value
                    if let Ok(new_val) = self.lower_expr(&assign.right, function, param_map, locals)
                    {
                        // Only handle dot-member (obj.prop), not computed (obj[expr])
                        use deno_ast::swc::ast::MemberProp;
                        if let MemberProp::Ident(prop_ident) = &member.prop {
                            let field_name = prop_ident.sym.to_string();

                            // Lower the object to get its pointer
                            if let Ok(BasicValueEnum::PointerValue(obj_ptr)) =
                                self.lower_expr(&member.obj, function, param_map, locals)
                            {
                                // Determine the class name from the object expression
                                let mut class_name_opt: Option<String> = None;

                                // Check if obj is `this` or a named parameter
                                if let deno_ast::swc::ast::Expr::Ident(ident) = &*member.obj {
                                    let ident_name = ident.sym.to_string();
                                    if ident_name == "this" {
                                        if let Some(param_types) = self
                                            .fn_param_types
                                            .borrow()
                                            .get(function.get_name().to_str().unwrap_or(""))
                                            && !param_types.is_empty()
                                            && let crate::types::OatsType::NominalStruct(n) =
                                                &param_types[0]
                                        {
                                            class_name_opt = Some(n.clone());
                                        }
                                    } else if let Some(param_idx) = param_map.get(&ident_name)
                                        && let Some(param_types) = self
                                            .fn_param_types
                                            .borrow()
                                            .get(function.get_name().to_str().unwrap_or(""))
                                    {
                                        let idx = *param_idx as usize;
                                        if idx < param_types.len()
                                            && let crate::types::OatsType::NominalStruct(n) =
                                                &param_types[idx]
                                        {
                                            class_name_opt = Some(n.clone());
                                        }
                                    }
                                } else if matches!(&*member.obj, deno_ast::swc::ast::Expr::This(_))
                                    && let Some(param_types) = self
                                        .fn_param_types
                                        .borrow()
                                        .get(function.get_name().to_str().unwrap_or(""))
                                    && !param_types.is_empty()
                                    && let crate::types::OatsType::NominalStruct(n) =
                                        &param_types[0]
                                {
                                    class_name_opt = Some(n.clone());
                                }

                                if let Some(class_name) = class_name_opt {
                                    // Look up field list for this class
                                    if let Some(fields) =
                                        self.class_fields.borrow().get(&class_name)
                                    {
                                        // Find the field by name
                                        if let Some((field_idx, (_fname, field_ty))) = fields
                                            .iter()
                                            .enumerate()
                                            .find(|(_, (n, _))| n == &field_name)
                                        {
                                            // Compute field offset: header (u64) + field_idx * sizeof(ptr)
                                            let hdr_size = self.i64_t.const_int(
                                                std::mem::size_of::<u64>() as u64,
                                                false,
                                            );
                                            let ptr_sz = self.i64_t.const_int(
                                                std::mem::size_of::<usize>() as u64,
                                                false,
                                            );
                                            let idx_const =
                                                self.i64_t.const_int(field_idx as u64, false);
                                            let mul = self
                                                .builder
                                                .build_int_mul(idx_const, ptr_sz, "fld_off_mul")
                                                .map_err(|_| {
                                                    Diagnostic::simple("LLVM builder error")
                                                })?;
                                            let offset = self
                                                .builder
                                                .build_int_add(hdr_size, mul, "fld_off")
                                                .map_err(|_| {
                                                    Diagnostic::simple("LLVM builder error")
                                                })?;
                                            let offset_i32 = self
                                                .builder
                                                .build_int_cast(
                                                    offset,
                                                    self.context.i32_type(),
                                                    "off_i32",
                                                )
                                                .map_err(|_| {
                                                    Diagnostic::simple("LLVM builder error")
                                                })?;

                                            // GEP to field location
                                            let gep_ptr = unsafe {
                                                self.builder.build_gep(
                                                    self.context.i8_type(),
                                                    obj_ptr,
                                                    &[offset_i32],
                                                    "field_i8ptr_store",
                                                )
                                            };
                                            let gep_ptr = match gep_ptr {
                                                Ok(pv) => pv,
                                                Err(_) => {
                                                    return Err(Diagnostic::simple(
                                                        "operation failed",
                                                    ));
                                                }
                                            };

                                            // Store based on field type
                                            match field_ty {
                                                crate::types::OatsType::Number => {
                                                    // Cast to f64 pointer and store
                                                    let f64_ptr = self
                                                        .builder
                                                        .build_pointer_cast(
                                                            gep_ptr,
                                                            self.context
                                                                .ptr_type(AddressSpace::default()),
                                                            "f64_ptr_cast_store",
                                                        )
                                                        .map_err(|_| {
                                                            Diagnostic::simple("LLVM builder error")
                                                        })?;
                                                    let _ =
                                                        self.builder.build_store(f64_ptr, new_val);
                                                    return Ok(new_val);
                                                }
                                                crate::types::OatsType::Union(_) => {
                                                    // Use boxed pointer representation for unions.
                                                    // Always store an i8* pointing to a small heap object produced by union_box_*.
                                                    let slot_ptr_ty = self
                                                        .context
                                                        .ptr_type(AddressSpace::default());
                                                    let slot_ptr =
                                                        match self.builder.build_pointer_cast(
                                                            gep_ptr,
                                                            slot_ptr_ty,
                                                            "slot_ptr_cast_store",
                                                        ) {
                                                            Ok(p) => p,
                                                            Err(_) => {
                                                                return Err(Diagnostic::simple(
                                                                    "operation failed",
                                                                ));
                                                            }
                                                        };

                                                    // Load old boxed union pointer and rc_dec if present
                                                    let old_val = match self.builder.build_load(
                                                        self.i8ptr_t,
                                                        slot_ptr,
                                                        "old_field_val",
                                                    ) {
                                                        Ok(v) => v,
                                                        Err(_) => {
                                                            return Err(Diagnostic::simple(
                                                                "operation failed",
                                                            ));
                                                        }
                                                    };
                                                    if let BasicValueEnum::PointerValue(old_pv) =
                                                        old_val
                                                    {
                                                        let rc_dec = self.get_rc_dec();
                                                        let _ = match self.builder.build_call(
                                                            rc_dec,
                                                            &[old_pv.into()],
                                                            "rc_dec_old_field",
                                                        ) {
                                                            Ok(cs) => cs,
                                                            Err(_) => {
                                                                return Err(Diagnostic::simple(
                                                                    "operation failed",
                                                                ));
                                                            }
                                                        };
                                                    }

                                                    // If new_val is a number, box it with union_box_f64; if pointer, box with union_box_ptr.
                                                    let boxed_new =
                                                        if let BasicValueEnum::FloatValue(fv) =
                                                            new_val
                                                        {
                                                            let box_fn = self.get_union_box_f64();
                                                            let cs = match self.builder.build_call(
                                                                box_fn,
                                                                &[fv.into()],
                                                                "union_box_f64_call",
                                                            ) {
                                                                Ok(cs) => cs,
                                                                Err(_) => {
                                                                    return Err(
                                                                        Diagnostic::simple(
                                                                            "operation failed",
                                                                        ),
                                                                    );
                                                                }
                                                            };
                                                            match cs.try_as_basic_value() {
                                                                inkwell::Either::Left(bv) => bv,
                                                                _ => {
                                                                    return Err(
                                                                        Diagnostic::simple(
                                                                            "operation failed",
                                                                        ),
                                                                    );
                                                                }
                                                            }
                                                        } else if let BasicValueEnum::PointerValue(
                                                            pv,
                                                        ) = new_val
                                                        {
                                                            let box_fn = self.get_union_box_ptr();
                                                            let cs = match self.builder.build_call(
                                                                box_fn,
                                                                &[pv.into()],
                                                                "union_box_ptr_call",
                                                            ) {
                                                                Ok(cs) => cs,
                                                                Err(_) => {
                                                                    return Err(
                                                                        Diagnostic::simple(
                                                                            "operation failed",
                                                                        ),
                                                                    );
                                                                }
                                                            };
                                                            match cs.try_as_basic_value() {
                                                                inkwell::Either::Left(bv) => bv,
                                                                _ => {
                                                                    return Err(
                                                                        Diagnostic::simple(
                                                                            "operation failed",
                                                                        ),
                                                                    );
                                                                }
                                                            }
                                                        } else {
                                                            return Err(Diagnostic::simple(
                                                                "unsupported union payload type",
                                                            ));
                                                        };

                                                    // Store the boxed pointer into the field slot
                                                    let _ = self
                                                        .builder
                                                        .build_store(slot_ptr, boxed_new);
                                                    // Increment refcount of the boxed union object
                                                    if let BasicValueEnum::PointerValue(new_pv) =
                                                        boxed_new
                                                    {
                                                        let rc_inc = self.get_rc_inc();
                                                        let _ = match self.builder.build_call(
                                                            rc_inc,
                                                            &[new_pv.into()],
                                                            "rc_inc_new_field",
                                                        ) {
                                                            Ok(cs) => cs,
                                                            Err(_) => {
                                                                return Err(Diagnostic::simple(
                                                                    "operation failed",
                                                                ));
                                                            }
                                                        };
                                                    }
                                                    return Ok(boxed_new);
                                                }
                                                crate::types::OatsType::String
                                                | crate::types::OatsType::NominalStruct(_)
                                                | crate::types::OatsType::Array(_) => {
                                                    // Cast to pointer type for slot
                                                    let slot_ptr_ty = self
                                                        .context
                                                        .ptr_type(AddressSpace::default());
                                                    let slot_ptr =
                                                        match self.builder.build_pointer_cast(
                                                            gep_ptr,
                                                            slot_ptr_ty,
                                                            "slot_ptr_cast_store",
                                                        ) {
                                                            Ok(p) => p,
                                                            Err(_) => {
                                                                return Err(Diagnostic::simple(
                                                                    "operation failed",
                                                                ));
                                                            }
                                                        };

                                                    // Load old value for RC decrement
                                                    let old_val = match self.builder.build_load(
                                                        self.i8ptr_t,
                                                        slot_ptr,
                                                        "old_field_val",
                                                    ) {
                                                        Ok(v) => v,
                                                        Err(_) => {
                                                            return Err(Diagnostic::simple(
                                                                "operation failed",
                                                            ));
                                                        }
                                                    };

                                                    // Decrement old value's refcount
                                                    if let BasicValueEnum::PointerValue(old_pv) =
                                                        old_val
                                                    {
                                                        let rc_dec = self.get_rc_dec();
                                                        let _ = match self.builder.build_call(
                                                            rc_dec,
                                                            &[old_pv.into()],
                                                            "rc_dec_old_field",
                                                        ) {
                                                            Ok(cs) => cs,
                                                            Err(_) => {
                                                                return Err(Diagnostic::simple(
                                                                    "operation failed",
                                                                ));
                                                            }
                                                        };
                                                    }

                                                    // Store new value
                                                    let _ =
                                                        self.builder.build_store(slot_ptr, new_val);

                                                    // Increment new value's refcount
                                                    if let BasicValueEnum::PointerValue(new_pv) =
                                                        new_val
                                                    {
                                                        let rc_inc = self.get_rc_inc();
                                                        let _ = match self.builder.build_call(
                                                            rc_inc,
                                                            &[new_pv.into()],
                                                            "rc_inc_new_field",
                                                        ) {
                                                            Ok(cs) => cs,
                                                            Err(_) => {
                                                                return Err(Diagnostic::simple(
                                                                    "operation failed",
                                                                ));
                                                            }
                                                        };
                                                    }
                                                    return Ok(new_val);
                                                }
                                                _ => {
                                                    // Unsupported field type
                                                    return Err(Diagnostic::simple(
                                                        "expression lowering failed",
                                                    ))?;
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
                Err(Diagnostic::simple("unsupported expression"))
            }
            ast::Expr::Cond(cond) => {
                // Ternary expression: test ? cons : alt
                // Lower test to an i1
                let test_val = self.lower_expr(&cond.test, function, param_map, locals)?;
                let cond_i1 = match test_val {
                    BasicValueEnum::IntValue(iv) => iv.as_basic_value_enum(),
                    BasicValueEnum::FloatValue(fv) => {
                        // JS-like truthiness for numbers: false for +0, -0, and NaN
                        let zero = self.f64_t.const_float(0.0);
                        let is_not_zero = match self.builder.build_float_compare(
                            inkwell::FloatPredicate::ONE,
                            fv,
                            zero,
                            "neq0",
                        ) {
                            Ok(v) => v,
                            Err(_) => return Err(Diagnostic::simple("operation failed")),
                        };
                        // check not NaN: fv == fv
                        let is_not_nan = match self.builder.build_float_compare(
                            inkwell::FloatPredicate::OEQ,
                            fv,
                            fv,
                            "not_nan",
                        ) {
                            Ok(v) => v,
                            Err(_) => return Err(Diagnostic::simple("operation failed")),
                        };
                        let cond =
                            match self.builder.build_and(is_not_zero, is_not_nan, "num_truth") {
                                Ok(v) => v,
                                Err(_) => return Err(Diagnostic::simple("operation failed")),
                            };
                        cond.as_basic_value_enum()
                    }
                    BasicValueEnum::PointerValue(pv) => {
                        // pointer truthiness: non-null and non-empty string are truthy
                        let is_null = match self.builder.build_is_null(pv, "is_null") {
                            Ok(v) => v,
                            Err(_) => return Err(Diagnostic::simple("operation failed")),
                        };
                        let is_not_null = match self.builder.build_not(is_null, "not_null") {
                            Ok(v) => v,
                            Err(_) => return Err(Diagnostic::simple("operation failed")),
                        };
                        // call strlen(ptr) and check != 0
                        if let Some(strlen_fn) = self.module.get_function("strlen") {
                            let cs = match self.builder.build_call(
                                strlen_fn,
                                &[pv.into()],
                                "strlen_call",
                            ) {
                                Ok(cs) => cs,
                                Err(_) => return Err(Diagnostic::simple("operation failed")),
                            };
                            let either = cs.try_as_basic_value();
                            if let inkwell::Either::Left(bv) = either {
                                let len = bv.into_int_value();
                                let zero64 = self.i64_t.const_int(0, false);
                                let len_nonzero = match self.builder.build_int_compare(
                                    inkwell::IntPredicate::NE,
                                    len,
                                    zero64,
                                    "len_nonzero",
                                ) {
                                    Ok(v) => v,
                                    Err(_) => return Err(Diagnostic::simple("operation failed")),
                                };
                                let cond = match self.builder.build_and(
                                    is_not_null,
                                    len_nonzero,
                                    "ptr_truth",
                                ) {
                                    Ok(v) => v,
                                    Err(_) => return Err(Diagnostic::simple("operation failed")),
                                };
                                return Ok(cond.as_basic_value_enum());
                            }
                        }
                        // fallback: non-null
                        is_not_null.as_basic_value_enum()
                    }
                    _ => return Err(Diagnostic::simple("operation failed")),
                };

                // Create basic blocks
                let then_bb = self.context.append_basic_block(function, "then");
                let else_bb = self.context.append_basic_block(function, "else");
                let merge_bb = self.context.append_basic_block(function, "merge");

                // branch based on cond_i1
                let cond_val = self
                    .to_condition_i1(cond_i1)
                    .ok_or_else(|| Diagnostic::simple("failed to convert to boolean condition"))?;
                self.builder
                    .build_conditional_branch(cond_val, then_bb, else_bb)
                    .map_err(|_| Diagnostic::simple("LLVM builder error"))?;

                // then
                self.builder.position_at_end(then_bb);
                let then_val = self.lower_expr(&cond.cons, function, param_map, locals);
                if self.builder.get_insert_block().is_some() {
                    let _ = self
                        .builder
                        .build_unconditional_branch(merge_bb)
                        .map_err(|_| Diagnostic::simple("LLVM builder error"))?;
                }

                // else
                self.builder.position_at_end(else_bb);
                let else_val = self.lower_expr(&cond.alt, function, param_map, locals);
                if self.builder.get_insert_block().is_some() {
                    let _ = self
                        .builder
                        .build_unconditional_branch(merge_bb)
                        .map_err(|_| Diagnostic::simple("LLVM builder error"))?;
                }

                // merge
                self.builder.position_at_end(merge_bb);
                // If both sides produced values, create a phi via helper
                if let (Ok(tv), Ok(ev)) = (then_val, else_val)
                    && let Some(phi) = self.build_phi_merge(then_bb, else_bb, tv, ev)
                {
                    return Ok(phi);
                }

                Err(Diagnostic::simple("unsupported expression"))
            }
            ast::Expr::Lit(lit) => {
                use deno_ast::swc::ast::Lit;
                match lit {
                    Lit::Num(n) => {
                        let fv = self.f64_t.const_float(n.value);
                        Ok(fv.as_basic_value_enum())
                    }
                    Lit::Bool(b) => {
                        let iv = self.bool_t.const_int(if b.value { 1 } else { 0 }, false);
                        Ok(iv.as_basic_value_enum())
                    }
                    Lit::Str(s) => {
                        let bytes = s.value.as_bytes();
                        let key = String::from_utf8_lossy(bytes).into_owned();
                        // Check cache first (cache stores the computed pointer)
                        if let Some(ptr_val) = self.string_literals.borrow().get(&key) {
                            return Ok(ptr_val.as_basic_value_enum());
                        }

                        // String literal layout with header:
                        // [u64 header][u64 length][N x i8 data]
                        // Header has static bit set (bit 32)
                        let str_len = bytes.len();

                        // Create a struct type: { i64, i64, [N x i8] }
                        let header_ty = self.i64_t;
                        let len_ty = self.i64_t;
                        let data_ty = self.context.i8_type().array_type((str_len + 1) as u32);
                        let struct_ty = self
                            .context
                            .struct_type(&[header_ty.into(), len_ty.into(), data_ty.into()], false);

                        // generate a unique global name for this string literal
                        let id = self.next_str_id.get();
                        let name = format!("strlit.{}", id);
                        self.next_str_id.set(id.wrapping_add(1));
                        let gv = self.module.add_global(struct_ty, None, &name);

                        // Initialize: header = (1 << 32) [static bit], length = str_len, data = bytes
                        let static_header = self.i64_t.const_int(1u64 << 32, false);
                        let length_val = self.i64_t.const_int(str_len as u64, false);
                        let data_val = self.context.const_string(bytes, true);

                        let initializer = self.context.const_struct(
                            &[static_header.into(), length_val.into(), data_val.into()],
                            false,
                        );
                        gv.set_initializer(&initializer);

                        // Return pointer to the data section (offset +16 from base)
                        let zero = self.i32_t.const_int(0, false);
                        let two = self.i32_t.const_int(2, false);
                        let indices = &[zero, two];
                        let gep = unsafe {
                            self.builder.build_gep(
                                struct_ty,
                                gv.as_pointer_value(),
                                indices,
                                "strptr",
                            )
                        };
                        if let Ok(ptr) = gep {
                            // store pointer in cache for future reuse
                            self.string_literals.borrow_mut().insert(key, ptr);
                            return Ok(ptr.as_basic_value_enum());
                        }
                        Err(Diagnostic::simple("unsupported expression"))
                    }
                    _ => Err(Diagnostic::simple("operation not supported")),
                }
            }
            ast::Expr::Array(arr) => {
                // Lower array literal: determine element kinds by lowering each elt
                let mut lowered_elems: Vec<BasicValueEnum> = Vec::new();
                for opt in &arr.elems {
                    if let Some(expr_or_spread) = opt {
                        // ExprOrSpread has .expr
                        if let Ok(ev) =
                            self.lower_expr(&expr_or_spread.expr, function, param_map, locals)
                        {
                            lowered_elems.push(ev);
                        } else {
                            // unsupported element lowering
                            return Err(Diagnostic::simple("expression lowering failed"))?;
                        }
                    } else {
                        // elided element like [ , ] -> treat as undefined -> unsupported
                        return Err(Diagnostic::simple("expression lowering failed"))?;
                    }
                }

                let len = lowered_elems.len() as u64;
                // decide numeric array if every elem is FloatValue
                let all_numbers = lowered_elems
                    .iter()
                    .all(|v| matches!(v, BasicValueEnum::FloatValue(_)));

                // call runtime array_alloc(i64 len, i32 elem_size, i32 elem_is_number)
                let array_alloc_fn = self.get_array_alloc();
                let len_const = self.i64_t.const_int(len, false);
                let (elem_size_const, is_number_const) = if all_numbers {
                    (
                        self.i32_t.const_int(8, false),
                        self.i32_t.const_int(1, false),
                    )
                } else {
                    // pointer-sized elements
                    let ptr_size = 8u64;
                    (
                        self.i32_t.const_int(ptr_size, false),
                        self.i32_t.const_int(0, false),
                    )
                };

                let call_site = match self.builder.build_call(
                    array_alloc_fn,
                    &[
                        len_const.into(),
                        elem_size_const.into(),
                        is_number_const.into(),
                    ],
                    "array_alloc_call",
                ) {
                    Ok(cs) => cs,
                    Err(_) => return Err(Diagnostic::simple("operation failed")),
                };
                let either = call_site.try_as_basic_value();
                let arr_ptr = match either {
                    inkwell::Either::Left(bv) => bv.into_pointer_value(),
                    _ => return Err(Diagnostic::simple("operation failed")),
                };

                // compute data pointer: arr_ptr points at header start; data starts after header+len
                let header_bytes = (std::mem::size_of::<u64>() + std::mem::size_of::<u64>()) as u64;
                // GEP arr_ptr (i8*) by header_bytes to get data start
                let offset_const = self.i32_t.const_int(header_bytes, false);
                let data_i8_res = unsafe {
                    self.builder.build_gep(
                        self.context.i8_type(),
                        arr_ptr,
                        &[offset_const],
                        "arr_data_i8",
                    )
                };
                let data_ptr_i8 = if let Ok(p) = data_i8_res {
                    p
                } else {
                    return Err(Diagnostic::simple("expression lowering failed"))?;
                };
                let array_set_ptr_fn = self.get_array_set_ptr();

                if all_numbers {
                    // For each element, compute byte offset = i * 8, GEP from data_ptr_i8, bitcast to f64* and store
                    for (i, v) in lowered_elems.into_iter().enumerate() {
                        if let BasicValueEnum::FloatValue(fv) = v {
                            let byte_off = (i as u64) * 8u64;
                            let off_const = self.i32_t.const_int(byte_off, false);
                            let elem_i8_res = unsafe {
                                self.builder.build_gep(
                                    self.context.i8_type(),
                                    data_ptr_i8,
                                    &[off_const],
                                    "elem_i8",
                                )
                            };
                            let elem_i8 = if let Ok(p) = elem_i8_res {
                                p
                            } else {
                                return Err(Diagnostic::simple("expression lowering failed"))?;
                            };
                            // bitcast to f64* (unwrap Result returned by pointer cast)
                            let elem_ptr = match self.builder.build_pointer_cast(
                                elem_i8,
                                self.context.ptr_type(AddressSpace::default()),
                                "elem_f64_ptr",
                            ) {
                                Ok(p) => p,
                                Err(_) => return Err(Diagnostic::simple("operation failed")),
                            };
                            let _ = self.builder.build_store(elem_ptr, fv);
                        } else {
                            return Err(Diagnostic::simple("expression lowering failed"))?;
                        }
                    }
                    Ok(arr_ptr.as_basic_value_enum())
                } else {
                    // pointer array: elements stored as machine pointers; element byte offset = i * ptr_size
                    let ptr_size = 8u64;
                    for (i, v) in lowered_elems.into_iter().enumerate() {
                        let byte_off = (i as u64) * ptr_size;
                        let off_const = self.i32_t.const_int(byte_off, false);
                        let elem_i8_res = unsafe {
                            self.builder.build_gep(
                                self.context.i8_type(),
                                data_ptr_i8,
                                &[off_const],
                                "elem_i8",
                            )
                        };
                        let elem_i8 = if let Ok(p) = elem_i8_res {
                            p
                        } else {
                            return Err(Diagnostic::simple("expression lowering failed"))?;
                        };
                        // bitcast to i8** (pointer-to-pointer)
                        let elem_ptr = match self.builder.build_pointer_cast(
                            elem_i8,
                            self.context.ptr_type(AddressSpace::default()),
                            "elem_ptrptr",
                        ) {
                            Ok(p) => p,
                            Err(_) => return Err(Diagnostic::simple("operation failed")),
                        };
                        match v {
                            BasicValueEnum::PointerValue(pv) => {
                                // call runtime array_set_ptr(arr_ptr, idx, pv)
                                let idx_const = self.i64_t.const_int(i as u64, false);
                                match self.builder.build_call(
                                    array_set_ptr_fn,
                                    &[arr_ptr.into(), idx_const.into(), pv.into()],
                                    "array_set_ptr_call",
                                ) {
                                    Ok(_cs) => (),
                                    Err(_) => return Err(Diagnostic::simple("operation failed")),
                                };
                            }
                            BasicValueEnum::IntValue(iv) => {
                                // store integer as-is into pointer slot (coerce as i8*)
                                let _ = self.builder.build_store(elem_ptr, iv);
                            }
                            _ => return Err(Diagnostic::simple("operation failed")),
                        }
                    }
                    Ok(arr_ptr.as_basic_value_enum())
                }
            }
            ast::Expr::This(_) => {
                if let Some((ptr, ty, init, _ignore, _extra)) = self.find_local(locals, "this")
                    && init
                    && let Ok(loaded) = self.builder.build_load(ty, ptr, "this_load")
                {
                    return Ok(loaded);
                }
                if let Some(idx) = param_map.get("this") {
                    if let Some(pv) = function.get_nth_param(*idx) {
                        return Ok(pv);
                    } else {
                        return Err(Diagnostic::simple("expression lowering failed"))?;
                    }
                }
                Err(Diagnostic::simple("unsupported expression"))
            }
            ast::Expr::Member(member) => {
                // Support both computed member access (obj[expr]) and dot-member (obj.prop)
                use deno_ast::swc::ast::MemberProp;
                match &member.prop {
                    MemberProp::Computed(boxed) => {
                        // lower object and index
                        if let Ok(obj_val) =
                            self.lower_expr(&member.obj, function, param_map, locals)
                            && let Ok(idx_val) =
                                self.lower_expr(&boxed.expr, function, param_map, locals)
                        {
                            // only support pointer-array indexing (i8**)
                            if let BasicValueEnum::PointerValue(arr_ptr) = obj_val {
                                // compute index as i64 for runtime helpers
                                let idx_i64 = match idx_val {
                                    BasicValueEnum::IntValue(iv) => self
                                        .builder
                                        .build_int_cast(iv, self.i64_t, "idx_i64")
                                        .map_err(|_| Diagnostic::simple("LLVM builder error"))?,
                                    BasicValueEnum::FloatValue(fv) => self
                                        .builder
                                        .build_float_to_signed_int(fv, self.i64_t, "f2i")
                                        .map_err(|_| Diagnostic::simple("LLVM builder error"))?,
                                    _ => return Err(Diagnostic::simple("operation failed")),
                                };

                                // If index is numeric, call typed runtime helper array_get_f64
                                if matches!(
                                    idx_val,
                                    BasicValueEnum::IntValue(_) | BasicValueEnum::FloatValue(_)
                                ) {
                                    // cast idx to i64
                                    let idx_i64 = match idx_val {
                                        BasicValueEnum::IntValue(iv) => self
                                            .builder
                                            .build_int_cast(iv, self.i64_t, "idx_i64")
                                            .map_err(|_| {
                                                Diagnostic::simple("LLVM builder error")
                                            })?,
                                        BasicValueEnum::FloatValue(fv) => self
                                            .builder
                                            .build_float_to_signed_int(fv, self.i64_t, "f2i_i64")
                                            .map_err(|_| {
                                                Diagnostic::simple("LLVM builder error")
                                            })?,
                                        _ => return Err(Diagnostic::simple("operation failed")),
                                    };
                                    let array_get = self.get_array_get_f64();
                                    let cs = match self.builder.build_call(
                                        array_get,
                                        &[arr_ptr.into(), idx_i64.into()],
                                        "array_get_f64_call",
                                    ) {
                                        Ok(cs) => cs,
                                        Err(_) => {
                                            return Err(Diagnostic::simple("operation failed"));
                                        }
                                    };
                                    let either = cs.try_as_basic_value();
                                    if let inkwell::Either::Left(bv) = either {
                                        return Ok(bv);
                                    }
                                }

                                // fallback: call runtime helper that returns a pointer and rc_inc's it
                                let array_get_ptr_fn = self.get_array_get_ptr();
                                let cs = match self.builder.build_call(
                                    array_get_ptr_fn,
                                    &[arr_ptr.into(), idx_i64.into()],
                                    "array_get_ptr_call",
                                ) {
                                    Ok(cs) => cs,
                                    Err(_) => return Err(Diagnostic::simple("operation failed")),
                                };
                                let either = cs.try_as_basic_value();
                                if let inkwell::Either::Left(bv) = either {
                                    return Ok(bv);
                                }
                            }
                        }
                    }
                    MemberProp::Ident(prop_ident) => {
                        // dot-member access like obj.prop
                        let field_name = prop_ident.sym.to_string();
                        if let Ok(BasicValueEnum::PointerValue(obj_ptr)) =
                            self.lower_expr(&member.obj, function, param_map, locals)
                        {
                            // Determine nominal class name if obj is the `this` parameter
                            // or if function param types carry nominal info.
                            // Check if the object is a param named in param_map
                            // Try to find which param corresponds to this pointer value
                            // If it's `this`, param_map contains "this" -> idx 0 typically.
                            // Use fn_param_types map to get the nominal type for `this`.
                            let mut class_name_opt: Option<String> = None;
                            // Prefer `this` receiver lookup, but also support parameters
                            // that are nominal structs. If member.obj is an Ident and
                            // matches a parameter name, use the declared param type
                            // to infer the nominal class.
                            if let deno_ast::swc::ast::Expr::Ident(ident) = &*member.obj {
                                let ident_name = ident.sym.to_string();
                                // If ident is `this` and function param types exist
                                if ident_name == "this" {
                                    if let Some(param_types) = self
                                        .fn_param_types
                                        .borrow()
                                        .get(function.get_name().to_str().unwrap_or(""))
                                        && !param_types.is_empty()
                                        && let crate::types::OatsType::NominalStruct(n) =
                                            &param_types[0]
                                    {
                                        class_name_opt = Some(n.clone());
                                    }
                                } else if let Some(param_idx) = param_map.get(&ident_name) {
                                    // param_map stores parameter name -> index (u32)
                                    if let Some(param_types) = self
                                        .fn_param_types
                                        .borrow()
                                        .get(function.get_name().to_str().unwrap_or(""))
                                    {
                                        let idx = *param_idx as usize;
                                        if idx < param_types.len()
                                            && let crate::types::OatsType::NominalStruct(n) =
                                                &param_types[idx]
                                        {
                                            class_name_opt = Some(n.clone());
                                        }
                                    }
                                }
                            } else if matches!(&*member.obj, deno_ast::swc::ast::Expr::This(_))
                                && let Some(param_types) = self
                                    .fn_param_types
                                    .borrow()
                                    .get(function.get_name().to_str().unwrap_or(""))
                                && !param_types.is_empty()
                                && let crate::types::OatsType::NominalStruct(n) = &param_types[0]
                            {
                                class_name_opt = Some(n.clone());
                            }

                            if let Some(class_name) = class_name_opt {
                                // lookup field list for this class
                                if let Some(fields) = self.class_fields.borrow().get(&class_name) {
                                    // find index of field
                                    if let Some((field_idx, (_fname, field_ty))) = fields
                                        .iter()
                                        .enumerate()
                                        .find(|(_, (n, _))| n == &field_name)
                                    {
                                        // field storage layout: after u64 header, fields are stored in pointer-sized slots
                                        // compute byte offset = sizeof(u64) + field_idx * sizeof(void*)
                                        let hdr_size = self
                                            .i64_t
                                            .const_int(std::mem::size_of::<u64>() as u64, false);
                                        let ptr_sz = self
                                            .i64_t
                                            .const_int(std::mem::size_of::<usize>() as u64, false);
                                        let idx_const =
                                            self.i64_t.const_int(field_idx as u64, false);
                                        // offset = hdr_size + idx * ptr_sz
                                        let mul = self
                                            .builder
                                            .build_int_mul(idx_const, ptr_sz, "fld_off_mul")
                                            .map_err(|_| {
                                                Diagnostic::simple("LLVM builder error")
                                            })?;
                                        let offset = self
                                            .builder
                                            .build_int_add(hdr_size, mul, "fld_off")
                                            .map_err(|_| {
                                                Diagnostic::simple("LLVM builder error")
                                            })?;
                                        // We need a i32 index sequence for gep on i8 pointer: cast offset to i64->i32 for index
                                        let offset_i64 = offset;
                                        let offset_i32 = self
                                            .builder
                                            .build_int_cast(
                                                offset_i64,
                                                self.context.i32_type(),
                                                "off_i32",
                                            )
                                            .map_err(|_| {
                                                Diagnostic::simple("LLVM builder error")
                                            })?;
                                        // Perform GEP on i8* using i32 index (element type i8, index is byte offset)
                                        let gep_res = unsafe {
                                            self.builder.build_gep(
                                                self.context.i8_type(),
                                                obj_ptr,
                                                &[offset_i32],
                                                "field_i8ptr",
                                            )
                                        };
                                        let gep_ptr = match gep_res {
                                            Ok(pv) => pv,
                                            Err(_) => {
                                                return Err(Diagnostic::simple("operation failed"));
                                            }
                                        };
                                        // Load based on field type
                                        match field_ty {
                                            crate::types::OatsType::Number => {
                                                // Cast to opaque pointer and load as f64
                                                let f64_ptr = self
                                                    .builder
                                                    .build_pointer_cast(
                                                        gep_ptr,
                                                        self.context
                                                            .ptr_type(AddressSpace::default()),
                                                        "f64_ptr_cast",
                                                    )
                                                    .map_err(|_| {
                                                        Diagnostic::simple("LLVM builder error")
                                                    })?;
                                                let loaded = match self.builder.build_load(
                                                    self.f64_t,
                                                    f64_ptr,
                                                    "field_f64_load",
                                                ) {
                                                    Ok(v) => v,
                                                    Err(_) => {
                                                        return Err(Diagnostic::simple(
                                                            "operation failed",
                                                        ));
                                                    }
                                                };
                                                return Ok(loaded.as_basic_value_enum());
                                            }
                                            crate::types::OatsType::String
                                            | crate::types::OatsType::NominalStruct(_)
                                            | crate::types::OatsType::Array(_) => {
                                                // Cast to pointer type and load
                                                let slot_ptr_ty =
                                                    self.context.ptr_type(AddressSpace::default());
                                                let slot_ptr =
                                                    match self.builder.build_pointer_cast(
                                                        gep_ptr,
                                                        slot_ptr_ty,
                                                        "slot_ptr_cast",
                                                    ) {
                                                        Ok(p) => p,
                                                        Err(_) => {
                                                            return Err(Diagnostic::simple(
                                                                "operation failed",
                                                            ));
                                                        }
                                                    };
                                                // load slot (an i8*)
                                                let loaded = match self.builder.build_load(
                                                    self.i8ptr_t,
                                                    slot_ptr,
                                                    "field_load",
                                                ) {
                                                    Ok(v) => v,
                                                    Err(_) => {
                                                        return Err(Diagnostic::simple(
                                                            "operation failed",
                                                        ));
                                                    }
                                                };
                                                return Ok(loaded.as_basic_value_enum());
                                            }
                                            crate::types::OatsType::Union(_) => {
                                                // Unions are stored boxed as i8*; load boxed pointer and unbox to requested payload.
                                                let slot_ptr_ty =
                                                    self.context.ptr_type(AddressSpace::default());
                                                let slot_ptr =
                                                    match self.builder.build_pointer_cast(
                                                        gep_ptr,
                                                        slot_ptr_ty,
                                                        "slot_ptr_cast",
                                                    ) {
                                                        Ok(p) => p,
                                                        Err(_) => {
                                                            return Err(Diagnostic::simple(
                                                                "operation failed",
                                                            ));
                                                        }
                                                    };
                                                let boxed = match self.builder.build_load(
                                                    self.i8ptr_t,
                                                    slot_ptr,
                                                    "union_boxed_load",
                                                ) {
                                                    Ok(v) => v,
                                                    Err(_) => {
                                                        return Err(Diagnostic::simple(
                                                            "operation failed",
                                                        ));
                                                    }
                                                };
                                                // Try to unbox as f64 first
                                                if let BasicValueEnum::PointerValue(boxed_ptr) =
                                                    boxed
                                                {
                                                    let unbox_f = self.get_union_unbox_f64();
                                                    let cs = match self.builder.build_call(
                                                        unbox_f,
                                                        &[boxed_ptr.into()],
                                                        "union_unbox_f64_call",
                                                    ) {
                                                        Ok(cs) => cs,
                                                        Err(_) => {
                                                            return Err(Diagnostic::simple(
                                                                "operation failed",
                                                            ));
                                                        }
                                                    };
                                                    let either = cs.try_as_basic_value();
                                                    if let inkwell::Either::Left(bv) = either {
                                                        return Ok(bv);
                                                    }

                                                    // Fallback to unbox_ptr
                                                    let unbox_p = self.get_union_unbox_ptr();
                                                    let cs2 = match self.builder.build_call(
                                                        unbox_p,
                                                        &[boxed_ptr.into()],
                                                        "union_unbox_ptr_call",
                                                    ) {
                                                        Ok(cs2) => cs2,
                                                        Err(_) => {
                                                            return Err(Diagnostic::simple(
                                                                "operation failed",
                                                            ));
                                                        }
                                                    };
                                                    let either2 = cs2.try_as_basic_value();
                                                    if let inkwell::Either::Left(bv2) = either2 {
                                                        return Ok(bv2);
                                                    }
                                                }
                                                return Err(Diagnostic::simple("operation failed"));
                                            }
                                            _ => {
                                                // Unsupported field type
                                                return Err(Diagnostic::simple(
                                                    "expression lowering failed",
                                                ))?;
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                    MemberProp::PrivateName(_) => {
                        // not supported
                    }
                }
                Err(Diagnostic::simple("unsupported expression"))
            }
            ast::Expr::New(new_expr) => {
                if let ast::Expr::Ident(ident) = &*new_expr.callee {
                    let ctor_name = format!("{}_ctor", ident.sym);
                    if let Some(fv) = self.module.get_function(&ctor_name) {
                        let mut lowered_args: Vec<inkwell::values::BasicMetadataValueEnum> =
                            Vec::new();
                        if let Some(args) = &new_expr.args {
                            for a in args {
                                if let Ok(val) =
                                    self.lower_expr(&a.expr, function, param_map, locals)
                                {
                                    lowered_args.push(val.into());
                                } else {
                                    return Err(Diagnostic::simple("expression lowering failed"))?;
                                }
                            }
                        }
                        let cs = self
                            .builder
                            .build_call(fv, &lowered_args, "new_call")
                            .map_err(|_| Diagnostic::simple("LLVM builder error"))?;
                        let either = cs.try_as_basic_value();
                        match either {
                            inkwell::Either::Left(bv) => Ok(bv),
                            _ => Err(Diagnostic::simple("operation not supported")),
                        }
                    } else {
                        Err(Diagnostic::simple("unsupported expression"))
                    }
                } else {
                    Err(Diagnostic::simple("unsupported expression"))
                }
            }
            ast::Expr::Arrow(arrow) => {
                // For now, only support non-capturing arrow functions
                // They will be lowered to static functions and returned as function pointers

                // TODO: Detect captured variables and implement closure support
                // For Phase 1, we only support arrows that don't capture variables

                // Generate a unique function name for this arrow
                let arrow_fn_name = format!("arrow_fn_{}", self.next_str_id.get());
                self.next_str_id.set(self.next_str_id.get() + 1);

                // Extract parameter types from arrow.params
                let mut param_types = Vec::new();
                for param in &arrow.params {
                    match param {
                        ast::Pat::Ident(ident) => {
                            if let Some(type_ann) = &ident.type_ann {
                                if let Some(mapped) = crate::types::map_ts_type(&type_ann.type_ann)
                                {
                                    param_types.push(mapped);
                                } else {
                                    return Err(Diagnostic::simple(
                                        "Arrow parameter has unsupported type annotation",
                                    ));
                                }
                            } else {
                                return Err(Diagnostic::simple(
                                    "Arrow parameter missing type annotation",
                                ));
                            }
                        }
                        _ => {
                            return Err(Diagnostic::simple(
                                "Arrow function parameter pattern not supported",
                            ));
                        }
                    }
                }

                // Determine return type from arrow.return_type or infer from body
                let ret_type = if let Some(return_type) = &arrow.return_type {
                    if let Some(mapped) = crate::types::map_ts_type(&return_type.type_ann) {
                        mapped
                    } else {
                        return Err(Diagnostic::simple("Arrow return type not supported"));
                    }
                } else {
                    // If no return type annotation, default to Number for now
                    // TODO: Implement type inference
                    crate::types::OatsType::Number
                };

                // Build LLVM function type
                let llvm_param_types: Vec<BasicTypeEnum> = param_types
                    .iter()
                    .map(|t| self.map_type_to_llvm(t))
                    .collect();
                let fn_type = self.build_llvm_fn_type(&llvm_param_types, &ret_type);

                // Create the arrow function
                let arrow_fn = self.module.add_function(&arrow_fn_name, fn_type, None);

                // Save current insert block so we can return to it
                let current_block = self.builder.get_insert_block();

                // Build the function body
                let entry_bb = self.context.append_basic_block(arrow_fn, "entry");
                self.builder.position_at_end(entry_bb);

                // Create parameter map for arrow function
                let mut arrow_param_map = HashMap::new();
                for (idx, param) in arrow.params.iter().enumerate() {
                    if let ast::Pat::Ident(ident) = param {
                        arrow_param_map.insert(ident.id.sym.to_string(), idx as u32);
                    }
                }

                // Create locals stack for arrow function
                let mut arrow_locals = vec![HashMap::new()];

                // Lower the body
                if arrow.body.is_block_stmt() {
                    // Block body: { statements }
                    if let ast::BlockStmtOrExpr::BlockStmt(block) = &*arrow.body {
                        let terminated = self.lower_stmts(
                            &block.stmts,
                            arrow_fn,
                            &arrow_param_map,
                            &mut arrow_locals,
                        );

                        let terminated = terminated?;

                        // If not terminated, add default return
                        if !terminated {
                            match ret_type {
                                crate::types::OatsType::Void => {
                                    let _ = self.builder.build_return(None);
                                }
                                crate::types::OatsType::Number => {
                                    let zero = self.f64_t.const_float(0.0);
                                    let _ = self.builder.build_return(Some(&zero));
                                }
                                _ => {
                                    return Err(Diagnostic::simple(
                                        "Cannot generate default return for arrow function",
                                    ));
                                }
                            }
                        }
                    } else {
                        return Err(Diagnostic::simple("Arrow body type mismatch"));
                    }
                } else {
                    // Expression body: => expr (implicit return)
                    if let ast::BlockStmtOrExpr::Expr(expr) = &*arrow.body {
                        let result =
                            self.lower_expr(expr, arrow_fn, &arrow_param_map, &mut arrow_locals)?;

                        // RC cleanup for locals before return
                        self.emit_rc_dec_for_locals(&mut arrow_locals);

                        let _ = self.builder.build_return(Some(&result));
                    } else {
                        return Err(Diagnostic::simple("Arrow body type mismatch"));
                    }
                }

                // Position back to original function
                if let Some(block) = current_block {
                    self.builder.position_at_end(block);
                }

                // Return the function pointer as a value
                // For now, we return the function pointer directly
                // TODO: Create proper closure struct when capturing is supported
                Ok(arrow_fn
                    .as_global_value()
                    .as_pointer_value()
                    .as_basic_value_enum())
            }
            ast::Expr::Object(obj_lit) => {
                // Lower an object literal to a simple heap-allocated struct.
                // We'll allocate header + N fields (8 bytes each) and store
                // each property value in order. Property names are not stored
                // in the runtime representation (they're positional).

                // Collect lowered values for properties. Support only simple key: expr props.
                let mut field_values: Vec<inkwell::values::BasicValueEnum> = Vec::new();

                for prop in &obj_lit.props {
                    match prop {
                        ast::PropOrSpread::Prop(prop_box) => match &**prop_box {
                            ast::Prop::KeyValue(kv) => {
                                // Only support identifier keys for now
                                if let ast::PropName::Ident(_ident) = &kv.key {
                                    // Lower the value expression
                                    let val =
                                        self.lower_expr(&kv.value, function, param_map, locals)?;
                                    field_values.push(val);
                                } else {
                                    return Err(Diagnostic::simple(
                                        "unsupported object literal key",
                                    ));
                                }
                            }
                            ast::Prop::Assign(assign) => {
                                // shorthand property `{ x }` - lower the identifier value
                                let name = assign.key.sym.to_string();
                                // First check parameters
                                if let Some(idx) = param_map.get(&name) {
                                    if let Some(pv) = function.get_nth_param(*idx) {
                                        let bv = pv.as_basic_value_enum();
                                        field_values.push(bv);
                                    } else {
                                        return Err(Diagnostic::simple(
                                            "failed to find shorthand param",
                                        ));
                                    }
                                } else if let Some((ptr, ty, _init, _is_const, _extra)) = self.find_local(locals, &name)
                                {
                                    // load local value
                                    let loaded = match self.builder.build_load(
                                        ty,
                                        ptr,
                                        &format!("shorthand_{}", name),
                                    ) {
                                        Ok(v) => v,
                                        Err(_) => {
                                            return Err(Diagnostic::simple(
                                                "failed to load shorthand local value",
                                            ));
                                        }
                                    };
                                    field_values.push(loaded);
                                } else {
                                    return Err(Diagnostic::simple(
                                        "shorthand property not found in params or locals",
                                    ));
                                }
                            }
                            _ => {
                                return Err(Diagnostic::simple(
                                    "unsupported object literal property",
                                ));
                            }
                        },
                        ast::PropOrSpread::Spread(_) => {
                            return Err(Diagnostic::simple(
                                "spread properties not supported in object literal",
                            ));
                        }
                    }
                }

                // Allocate object: header (8) + N * 8 bytes
                let header_size = 8u64;
                let field_count = field_values.len();
                let total_size = header_size + (field_count as u64 * 8);

                let malloc_fn = self.get_malloc();
                let size_const = self.i64_t.const_int(total_size, false);
                let call_site = self
                    .builder
                    .build_call(malloc_fn, &[size_const.into()], "obj_malloc")
                    .map_err(|_| Diagnostic::simple("build_call failed"))?;
                let malloc_ret = call_site
                    .try_as_basic_value()
                    .left()
                    .ok_or_else(|| Diagnostic::simple("malloc did not return value"))?
                    .into_pointer_value();

                // Initialize header (refcount=1, no flags)
                let header_ptr = self
                    .builder
                    .build_pointer_cast(malloc_ret, self.i8ptr_t, "hdr_ptr")
                    .map_err(|_| Diagnostic::simple("pointer cast failed"))?;
                let header_val = self.i64_t.const_int(1u64, false);
                let _ = self.builder.build_store(header_ptr, header_val);

                // Store fields sequentially
                for (idx, fv) in field_values.into_iter().enumerate() {
                    let offset = header_size + (idx as u64 * 8);
                    let obj_addr = self
                        .builder
                        .build_ptr_to_int(malloc_ret, self.i64_t, "obj_addr")
                        .map_err(|_| Diagnostic::simple("ptr_to_int failed"))?;
                    let offset_const = self.i64_t.const_int(offset, false);
                    let field_addr = self
                        .builder
                        .build_int_add(obj_addr, offset_const, "field_addr")
                        .map_err(|_| Diagnostic::simple("int_add failed"))?;
                    let field_ptr = self
                        .builder
                        .build_int_to_ptr(field_addr, self.i8ptr_t, "field_ptr")
                        .map_err(|_| Diagnostic::simple("int_to_ptr failed"))?;

                    // If it's a float, box it into a pointer; if pointer already, store it and rc_inc.
                    if fv.get_type().is_float_type() {
                        let box_fn = self.get_union_box_f64();
                        let cs = self
                            .builder
                            .build_call(box_fn, &[fv.into()], "union_box_f64_ctor")
                            .map_err(|_| Diagnostic::simple("box call failed"))?;
                        if let inkwell::Either::Left(bv) = cs.try_as_basic_value() {
                            let boxed_ptr = bv.into_pointer_value();
                            let boxed_bv = inkwell::values::BasicValueEnum::PointerValue(boxed_ptr);
                            let _ = self.builder.build_store(field_ptr, boxed_bv);
                            let rc_inc = self.get_rc_inc();
                            let _ = self.builder.build_call(
                                rc_inc,
                                &[boxed_ptr.into()],
                                "rc_inc_field",
                            );
                        }
                    } else if fv.get_type().is_pointer_type() {
                        let _ = self.builder.build_store(field_ptr, fv);
                        let rc_inc = self.get_rc_inc();
                        let ptr = match fv {
                            inkwell::values::BasicValueEnum::PointerValue(p) => p,
                            _ => unreachable!(),
                        };
                        let _ = self
                            .builder
                            .build_call(rc_inc, &[ptr.into()], "rc_inc_field");
                    } else if fv.get_type().is_int_type() {
                        // Treat integers as numbers (f64) for now: convert to f64 then box
                        let intv = fv.into_int_value();
                        let fconv = self
                            .builder
                            .build_signed_int_to_float(intv, self.f64_t, "i_to_f")
                            .map_err(|_| Diagnostic::simple("int->float cast failed"))?;
                        let box_fn = self.get_union_box_f64();
                        let cs = self
                            .builder
                            .build_call(box_fn, &[fconv.into()], "union_box_f64_ctor")
                            .map_err(|_| Diagnostic::simple("box call failed"))?;
                        if let inkwell::Either::Left(bv) = cs.try_as_basic_value() {
                            let boxed_ptr = bv.into_pointer_value();
                            let boxed_bv = inkwell::values::BasicValueEnum::PointerValue(boxed_ptr);
                            let _ = self.builder.build_store(field_ptr, boxed_bv);
                            let rc_inc = self.get_rc_inc();
                            let _ = self.builder.build_call(
                                rc_inc,
                                &[boxed_ptr.into()],
                                "rc_inc_field",
                            );
                        }
                    } else {
                        // Fallback: attempt to store as is (may fail at runtime)
                        let _ = self.builder.build_store(field_ptr, fv);
                    }
                }

                // Return the base pointer
                Ok(malloc_ret.as_basic_value_enum())
            }
            ast::Expr::Tpl(tpl) => {
                // Template literal: `hello ${name}!`
                // Structure: quasis (string parts) and exprs (interpolated expressions)
                // Build result by concatenating: quasis[0] + str(exprs[0]) + quasis[1] + str(exprs[1]) + ...

                // Ensure str_concat is declared
                self.gen_str_concat();
                let concat_fn = self
                    .module
                    .get_function("str_concat")
                    .ok_or_else(|| Diagnostic::simple("str_concat not found"))?;

                // Helper to create a string literal (with header, same as Lit::Str)
                let create_string_literal =
                    |codegen: &Self, s: &str| -> Result<PointerValue<'a>, Diagnostic> {
                        let bytes = s.as_bytes();
                        let key = s.to_string();

                        // Check cache first
                        if let Some(ptr_val) = codegen.string_literals.borrow().get(&key) {
                            return Ok(*ptr_val);
                        }

                        // String literal layout with header: [u64 header][u64 length][N x i8 data]
                        let str_len = bytes.len();
                        let header_ty = codegen.i64_t;
                        let len_ty = codegen.i64_t;
                        let data_ty = codegen.context.i8_type().array_type((str_len + 1) as u32);
                        let struct_ty = codegen
                            .context
                            .struct_type(&[header_ty.into(), len_ty.into(), data_ty.into()], false);

                        let id = codegen.next_str_id.get();
                        let name = format!("strlit.{}", id);
                        codegen.next_str_id.set(id.wrapping_add(1));
                        let gv = codegen.module.add_global(struct_ty, None, &name);

                        // Initialize with static header
                        let static_header = codegen.i64_t.const_int(1u64 << 32, false);
                        let length_val = codegen.i64_t.const_int(str_len as u64, false);
                        let data_val = codegen.context.const_string(bytes, true);

                        let initializer = codegen.context.const_struct(
                            &[static_header.into(), length_val.into(), data_val.into()],
                            false,
                        );
                        gv.set_initializer(&initializer);

                        // Return pointer to data section (offset +16, field index 2)
                        let zero = codegen.i32_t.const_int(0, false);
                        let two = codegen.i32_t.const_int(2, false);
                        let indices = &[zero, two];
                        let gep = unsafe {
                            codegen.builder.build_gep(
                                struct_ty,
                                gv.as_pointer_value(),
                                indices,
                                "strptr",
                            )
                        };

                        if let Ok(ptr) = gep {
                            codegen.string_literals.borrow_mut().insert(key, ptr);
                            Ok(ptr)
                        } else {
                            Err(Diagnostic::simple("failed to create string literal"))
                        }
                    };

                let mut result: Option<PointerValue<'a>> = None;

                // Template literals have quasis (string parts) and exprs (interpolated expressions)
                // quasis.len() = exprs.len() + 1 (there's always one more quasi)
                for (i, quasi) in tpl.quasis.iter().enumerate() {
                    // Add the string part
                    let quasi_str = quasi.raw.to_string();
                    let quasi_ptr = create_string_literal(self, &quasi_str)?;

                    // Concatenate with result so far
                    result = if let Some(prev) = result {
                        let call_site = self
                            .builder
                            .build_call(
                                concat_fn,
                                &[prev.into(), quasi_ptr.into()],
                                "tpl_concat_quasi",
                            )
                            .map_err(|_| Diagnostic::simple("failed to build call"))?;
                        Some(
                            call_site
                                .try_as_basic_value()
                                .left()
                                .ok_or_else(|| Diagnostic::simple("concat call returned no value"))?
                                .into_pointer_value(),
                        )
                    } else {
                        Some(quasi_ptr)
                    };

                    // If there's a corresponding expression, evaluate it and convert to string
                    if i < tpl.exprs.len() {
                        let expr_val =
                            self.lower_expr(&tpl.exprs[i], function, param_map, locals)?;

                        // Convert to string based on type
                        let expr_str = if expr_val.is_float_value() {
                            // Number: use number_to_string
                            let num_val = expr_val.into_float_value();
                            let num_to_str_fn = self.get_number_to_string();
                            let call_site = self
                                .builder
                                .build_call(num_to_str_fn, &[num_val.into()], "num_to_str")
                                .map_err(|_| Diagnostic::simple("failed to build call"))?;
                            call_site
                                .try_as_basic_value()
                                .left()
                                .ok_or_else(|| Diagnostic::simple("num_to_str returned no value"))?
                                .into_pointer_value()
                        } else if expr_val.is_pointer_value() {
                            // Already a string (or object) - use as-is
                            expr_val.into_pointer_value()
                        } else if expr_val.is_int_value() {
                            // Boolean: convert to "true" or "false"
                            let bool_val = expr_val.into_int_value();
                            let true_str = create_string_literal(self, "true")?;
                            let false_str = create_string_literal(self, "false")?;

                            // Use select to pick the right string
                            self.builder
                                .build_select(bool_val, true_str, false_str, "bool_str")
                                .map_err(|_| Diagnostic::simple("failed to build select"))?
                                .into_pointer_value()
                        } else {
                            return Err(Diagnostic::simple(
                                "unsupported value type in template literal",
                            ));
                        };

                        // Concatenate expression string with result
                        let left = result
                            .ok_or_else(|| Diagnostic::simple("concat left operand missing"))?;
                        let call_site = self
                            .builder
                            .build_call(
                                concat_fn,
                                &[left.into(), expr_str.into()],
                                "tpl_concat_expr",
                            )
                            .map_err(|_| Diagnostic::simple("failed to build call"))?;
                        result = Some(
                            call_site
                                .try_as_basic_value()
                                .left()
                                .ok_or_else(|| Diagnostic::simple("concat call returned no value"))?
                                .into_pointer_value(),
                        );
                    }
                }

                Ok(result
                    .ok_or_else(|| Diagnostic::simple("empty template literal"))?
                    .as_basic_value_enum())
            }
            ast::Expr::Unary(unary) => {
                // Handle unary operators: -, +, !, ~, typeof
                use deno_ast::swc::ast::UnaryOp;

                // Short-circuit typeof since it returns a string literal based on runtime kind
                if matches!(unary.op, UnaryOp::TypeOf) {
                    let inner = self.lower_expr(&unary.arg, function, param_map, locals)?;
                    // If we can coerce to f64 (including union-unboxed numbers), it's a number
                    if self.coerce_to_f64(inner).is_some() {
                        let ptr = self.intern_string_literal("number");
                        return Ok(ptr.as_basic_value_enum());
                    }

                    // If it's a pointer, try to consult the union discriminant helper to
                    // determine the runtime kind (number/string/boolean/object). This
                    // enables typeof guards to work on boxed union values.
                    if let BasicValueEnum::PointerValue(pv) = inner {
                        // If this pointer may be a boxed union, call the discriminant
                        // helper to distinguish number/string/boolean/object at runtime.
                        // This keeps `typeof` semantics consistent for boxed union values.
                        let disc_fn = self.get_union_get_discriminant();
                        let call_site = self
                            .builder
                            .build_call(disc_fn, &[pv.into()], "union_get_disc");
                        let cs = call_site.map_err(|_| Diagnostic::simple("failed to call union_get_discriminant"))?;
                        if let inkwell::Either::Left(bv) = cs.try_as_basic_value() {
                            let disc = bv.into_int_value();
                            // Compare disc to constants: 0 -> number, 1 -> string, 2 -> boolean
                            let zero = self.i64_t.const_int(0, false);
                            let one = self.i64_t.const_int(1, false);
                            let two = self.i64_t.const_int(2, false);

                            let cmp_num = self
                                .builder
                                .build_int_compare(inkwell::IntPredicate::EQ, disc, zero, "disc_eq_num")
                                .map_err(|_| Diagnostic::simple("failed to build int compare for typeof"))?;
                            let cmp_str = self
                                .builder
                                .build_int_compare(inkwell::IntPredicate::EQ, disc, one, "disc_eq_str")
                                .map_err(|_| Diagnostic::simple("failed to build int compare for typeof"))?;
                            let cmp_bool = self
                                .builder
                                .build_int_compare(inkwell::IntPredicate::EQ, disc, two, "disc_eq_bool")
                                .map_err(|_| Diagnostic::simple("failed to build int compare for typeof"))?;

                            let s_num = self.intern_string_literal("number");
                            let s_str = self.intern_string_literal("string");
                            let s_bool = self.intern_string_literal("boolean");
                            let s_obj = self.intern_string_literal("object");

                            // Build nested selects: if num -> s_num else if str -> s_str else if bool -> s_bool else s_obj
                            let sel1 = self
                                .builder
                                .build_select(cmp_num, s_num.as_basic_value_enum(), s_str.as_basic_value_enum(), "sel_num_str")
                                .map_err(|_| Diagnostic::simple("failed to build select for typeof"))?;
                            let sel2 = self
                                .builder
                                .build_select(cmp_bool, s_bool.as_basic_value_enum(), s_obj.as_basic_value_enum(), "sel_bool_obj")
                                .map_err(|_| Diagnostic::simple("failed to build select for typeof"))?;
                            let final_sel = self
                                .builder
                                .build_select(cmp_str, sel1, sel2, "sel_final")
                                .map_err(|_| Diagnostic::simple("failed to build select for typeof"))?;
                            return Ok(final_sel);
                        }
                        // If the discriminant call failed, fall back to "string"
                        let ptr = self.intern_string_literal("string");
                        return Ok(ptr.as_basic_value_enum());
                    }

                    // integer/boolean-like values -> "boolean"
                    if let BasicValueEnum::IntValue(_iv) = inner {
                        let ptr = self.intern_string_literal("boolean");
                        return Ok(ptr.as_basic_value_enum());
                    }

                    let ptr = self.intern_string_literal("object");
                    return Ok(ptr.as_basic_value_enum());
                }

                let arg_val = self.lower_expr(&unary.arg, function, param_map, locals)?;

                match unary.op {
                    UnaryOp::Minus => {
                        // Unary minus: negate the value
                        if let Some(fv) = self.coerce_to_f64(arg_val) {
                            let neg = self
                                .builder
                                .build_float_neg(fv, "neg")
                                .map_err(|_| Diagnostic::simple("LLVM builder error"))?;
                            Ok(neg.as_basic_value_enum())
                        } else {
                            Err(Diagnostic::simple("unary minus requires numeric operand"))
                        }
                    }
                    UnaryOp::Plus => {
                        // Unary plus: coerce to number (no-op for numbers)
                        if let Some(fv) = self.coerce_to_f64(arg_val) {
                            Ok(fv.as_basic_value_enum())
                        } else {
                            Err(Diagnostic::simple("unary plus requires numeric operand"))
                        }
                    }
                    UnaryOp::Bang => {
                        // Logical NOT: convert to boolean and negate
                        let cond = self
                            .to_condition_i1(arg_val)
                            .ok_or_else(|| Diagnostic::simple("failed to convert to boolean"))?;
                        let not = self
                            .builder
                            .build_not(cond, "not")
                            .map_err(|_| Diagnostic::simple("LLVM builder error"))?;
                        Ok(not.as_basic_value_enum())
                    }
                    UnaryOp::Tilde => {
                        // Bitwise NOT: convert to integer, apply NOT, convert back
                        if let Some(fv) = self.coerce_to_f64(arg_val) {
                            // Convert to i32
                            let iv = self
                                .builder
                                .build_float_to_signed_int(fv, self.context.i32_type(), "f2i")
                                .map_err(|_| Diagnostic::simple("LLVM builder error"))?;
                            // Apply bitwise NOT
                            let not_iv = self
                                .builder
                                .build_not(iv, "bnot")
                                .map_err(|_| Diagnostic::simple("LLVM builder error"))?;
                            // Convert back to f64
                            let result_fv = self
                                .builder
                                .build_signed_int_to_float(not_iv, self.f64_t, "i2f")
                                .map_err(|_| Diagnostic::simple("LLVM builder error"))?;
                            Ok(result_fv.as_basic_value_enum())
                        } else {
                            Err(Diagnostic::simple("bitwise NOT requires numeric operand"))
                        }
                    }
                    _ => Err(Diagnostic::simple("unsupported unary operator")),
                }
            }

            ast::Expr::Update(update) => {
                // Handle update operators: ++, --
                // These modify the variable and return the old (postfix) or new (prefix) value
                use deno_ast::swc::ast::UpdateOp;

                // Only support simple identifier updates for now
                if let ast::Expr::Ident(ident) = &*update.arg {
                    let name = ident.sym.to_string();

                    // Find the variable (parameter or local)
                    let var_entry = if param_map.contains_key(&name) {
                        // It's a parameter - we can't update parameters directly
                        // Need to create a local shadow
                        return Err(Diagnostic::simple(
                            "cannot update function parameter directly",
                        ));
                    } else if let Some((ptr, ty, initialized, is_const, _extra)) =
                        self.find_local(locals, &name)
                    {
                        if is_const {
                            return Err(Diagnostic::simple("cannot update immutable variable"));
                        }
                        if !initialized {
                            return Err(Diagnostic::simple("cannot update uninitialized variable"));
                        }
                        Some((ptr, ty))
                    } else {
                        None
                    };

                    if let Some((ptr, ty)) = var_entry {
                        // Load current value
                        let old_val = self
                            .builder
                            .build_load(ty, ptr, &name)
                            .map_err(|_| Diagnostic::simple("LLVM builder error"))?;

                        // Only support numeric updates
                        if let Some(old_fv) = self.coerce_to_f64(old_val) {
                            let one = self.f64_t.const_float(1.0);

                            // Compute new value based on operator
                            let new_fv = match update.op {
                                UpdateOp::PlusPlus => self
                                    .builder
                                    .build_float_add(old_fv, one, "inc")
                                    .map_err(|_| Diagnostic::simple("LLVM builder error"))?,
                                UpdateOp::MinusMinus => self
                                    .builder
                                    .build_float_sub(old_fv, one, "dec")
                                    .map_err(|_| Diagnostic::simple("LLVM builder error"))?,
                            };

                            // Store new value
                            self.builder
                                .build_store(ptr, new_fv)
                                .map_err(|_| Diagnostic::simple("LLVM builder error"))?;

                            // Return old value for postfix, new value for prefix
                            if update.prefix {
                                Ok(new_fv.as_basic_value_enum())
                            } else {
                                Ok(old_fv.as_basic_value_enum())
                            }
                        } else {
                            Err(Diagnostic::simple(
                                "update operators require numeric operand",
                            ))
                        }
                    } else {
                        Err(Diagnostic::simple("variable not found"))
                    }
                } else {
                    Err(Diagnostic::simple(
                        "update operator only supports simple identifiers",
                    ))
                }
            }
            _ => Err(Diagnostic::simple("operation not supported")),
        }
    }
}
