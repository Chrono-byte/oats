use crate::diagnostics::Diagnostic;
use deno_ast::swc::ast::Expr;
use inkwell::values::BasicValueEnum;
use inkwell::values::FunctionValue;
use std::collections::HashMap;

use inkwell::AddressSpace;
use inkwell::builder::Builder;
use inkwell::types::BasicType;
use inkwell::types::BasicTypeEnum;
use inkwell::values::{BasicValue, PointerValue};
type LocalEntry<'a> = (PointerValue<'a>, BasicTypeEnum<'a>, bool, bool);
type LocalsStackLocal<'a> = Vec<HashMap<String, LocalEntry<'a>>>;

impl<'a> crate::codegen::CodeGen<'a> {
    /// Thin adapter that converts the existing Option-based `lower_expr` into
    /// a Result carrying a `Diagnostic` so callers can centrally report errors.
    pub fn lower_expr_result(
        &self,
        expr: &Expr,
        function: FunctionValue<'a>,
        param_map: &HashMap<String, u32>,
        locals: &mut Vec<
            HashMap<
                String,
                (
                    inkwell::values::PointerValue<'a>,
                    inkwell::types::BasicTypeEnum<'a>,
                    bool,
                    bool,
                ),
            >,
        >,
        ctx_name: Option<&str>,
    ) -> Result<BasicValueEnum<'a>, Diagnostic> {
        if let Some(v) = self.lower_expr(expr, function, param_map, locals) {
            Ok(v)
        } else {
            let msg = format!(
                "failed to lower expression in context '{}'",
                ctx_name.unwrap_or("<unknown>")
            );
            Err(Diagnostic::simple(msg))
        }
    }

    pub fn lower_expr(
        &self,
        expr: &deno_ast::swc::ast::Expr,
        function: FunctionValue<'a>,
        param_map: &HashMap<String, u32>,
        locals: &mut LocalsStackLocal<'a>,
    ) -> Option<BasicValueEnum<'a>> {
        use deno_ast::swc::ast;

        match expr {
            ast::Expr::Bin(bin) => {
                use deno_ast::swc::ast::BinaryOp;
                use inkwell::FloatPredicate;

                let l = self.lower_expr(&bin.left, function, param_map, locals)?;
                let r = self.lower_expr(&bin.right, function, param_map, locals)?;

                // coercion now handled by self.coerce_to_f64

                // Helper to handle float arithmetic
                let float_bin = |builder: &Builder<'a>,
                                 lf: inkwell::values::FloatValue<'a>,
                                 rf: inkwell::values::FloatValue<'a>,
                                 op: &BinaryOp|
                 -> Option<BasicValueEnum<'a>> {
                    match op {
                        BinaryOp::Add => {
                            let v = match builder.build_float_add(lf, rf, "sum") {
                                Ok(v) => v,
                                Err(_) => return None,
                            };
                            Some(v.as_basic_value_enum())
                        }
                        BinaryOp::Sub => {
                            let v = match builder.build_float_sub(lf, rf, "sub") {
                                Ok(v) => v,
                                Err(_) => return None,
                            };
                            Some(v.as_basic_value_enum())
                        }
                        BinaryOp::Mul => {
                            let v = match builder.build_float_mul(lf, rf, "mul") {
                                Ok(v) => v,
                                Err(_) => return None,
                            };
                            Some(v.as_basic_value_enum())
                        }
                        BinaryOp::Div => {
                            let v = match builder.build_float_div(lf, rf, "div") {
                                Ok(v) => v,
                                Err(_) => return None,
                            };
                            Some(v.as_basic_value_enum())
                        }
                        _ => None,
                    }
                };

                // Helper to handle float comparisons -> i1
                let float_cmp = |builder: &Builder<'a>,
                                 lf: inkwell::values::FloatValue<'a>,
                                 rf: inkwell::values::FloatValue<'a>,
                                 op: &BinaryOp|
                 -> Option<BasicValueEnum<'a>> {
                    let pred = match op {
                        BinaryOp::Lt => FloatPredicate::OLT,
                        BinaryOp::LtEq => FloatPredicate::OLE,
                        BinaryOp::Gt => FloatPredicate::OGT,
                        BinaryOp::GtEq => FloatPredicate::OGE,
                        BinaryOp::EqEq => FloatPredicate::OEQ,
                        BinaryOp::NotEq => FloatPredicate::ONE,
                        _ => return None,
                    };
                    let iv = match builder.build_float_compare(pred, lf, rf, "cmp") {
                        Ok(iv) => iv,
                        Err(_) => return None,
                    };
                    Some(iv.as_basic_value_enum())
                };

                // Try float path by coercing ints/bools to float as needed
                if let (Some(lf), Some(rf)) = (self.coerce_to_f64(l), self.coerce_to_f64(r)) {
                    if let Some(v) = float_bin(&self.builder, lf, rf, &bin.op) {
                        return Some(v);
                    }
                    if let Some(v) = float_cmp(&self.builder, lf, rf, &bin.op) {
                        return Some(v);
                    }
                }

                // Logical operators: short-circuiting
                if let deno_ast::swc::ast::BinaryOp::LogicalAnd = bin.op {
                    // a && b -> if a truthy then b else a
                    let left_val = l;
                    let cond = self.to_condition_i1(left_val)?;
                    let then_bb = self.context.append_basic_block(function, "and.then");
                    let else_bb = self.context.append_basic_block(function, "and.else");
                    let merge_bb = self.context.append_basic_block(function, "and.merge");
                    if self
                        .builder
                        .build_conditional_branch(cond, then_bb, else_bb).is_err()
                    {
                        return None;
                    }
                    // then: evaluate right
                    self.builder.position_at_end(then_bb);
                    let rv = self.lower_expr(&bin.right, function, param_map, locals);
                    if self.builder.get_insert_block().is_some()
                        && self.builder.build_unconditional_branch(merge_bb).is_err() {
                            return None;
                        }
                    // else: keep left
                    self.builder.position_at_end(else_bb);
                    if self.builder.get_insert_block().is_some() {
                        let _ = self
                            .builder
                            .build_unconditional_branch(merge_bb)
                            .expect("build_unconditional_branch failed");
                    }
                    self.builder.position_at_end(merge_bb);
                    if let Some(rval) = rv
                        && let Some(phi) = self.build_phi_merge(then_bb, else_bb, rval, left_val)
                    {
                        return Some(phi);
                    }
                    return None;
                }
                if let deno_ast::swc::ast::BinaryOp::LogicalOr = bin.op {
                    // a || b -> if a truthy then a else b
                    let left_val = l;
                    let cond = self.to_condition_i1(left_val)?;
                    let then_bb = self.context.append_basic_block(function, "or.then");
                    let else_bb = self.context.append_basic_block(function, "or.else");
                    let merge_bb = self.context.append_basic_block(function, "or.merge");
                    if self
                        .builder
                        .build_conditional_branch(cond, then_bb, else_bb).is_err()
                    {
                        return None;
                    }
                    // then: keep left
                    self.builder.position_at_end(then_bb);
                    if self.builder.get_insert_block().is_some()
                        && self.builder.build_unconditional_branch(merge_bb).is_err() {
                            return None;
                        }
                    // else: evaluate right
                    self.builder.position_at_end(else_bb);
                    let rv = self.lower_expr(&bin.right, function, param_map, locals);
                    if self.builder.get_insert_block().is_some() {
                        let _ = self
                            .builder
                            .build_unconditional_branch(merge_bb)
                            .expect("build_unconditional_branch failed");
                    }
                    self.builder.position_at_end(merge_bb);
                    if let Some(rval) = rv
                        && let Some(phi) = self.build_phi_merge(then_bb, else_bb, left_val, rval)
                    {
                        return Some(phi);
                    }
                    return None;
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
                                    Err(_) => return None,
                                };
                                let either = call_site.try_as_basic_value();
                                match either {
                                    inkwell::Either::Left(bv) => Some(bv),
                                    _ => None,
                                }
                            } else {
                                None
                            }
                        } else {
                            None
                        }
                    }
                    _ => None,
                }
            }
            ast::Expr::Ident(id) => {
                let name = id.sym.to_string();

                // First, check if the identifier is a function parameter.
                if let Some(idx) = param_map.get(&name)
                    && let Some(pv) = function.get_nth_param(*idx) {
                        return Some(pv);
                    }

                // If not a parameter, then it must be a local variable (`let` or `const`).
                if let Some((ptr, ty, initialized, _is_const)) = self.find_local(locals, &name) {
                    // If not initialized -> TDZ: generate a trap (unreachable)
                    if !initialized {
                        // Emit a call to unreachable to trap at runtime
                        let _ = self.builder.build_unreachable();
                        return None;
                    }
                    let loaded = match self.builder.build_load(ty, ptr, &name) {
                        Ok(v) => v,
                        Err(_) => return None,
                    };
                    return Some(loaded);
                }
                None
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
                                    return None;
                                }
                                let a = &call.args[0];
                                if let Some(val) =
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
                                            return None;
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
                                            return None;
                                        }
                                        _ => return None,
                                    }
                                } else {
                                    return None;
                                }
                            }

                            if let Some(fv) = self.module.get_function(&fname) {
                                // Lower args
                                let mut lowered_args: Vec<inkwell::values::BasicMetadataValueEnum> =
                                    Vec::new();
                                for a in &call.args {
                                    if let Some(val) =
                                        self.lower_expr(&a.expr, function, param_map, locals)
                                    {
                                        lowered_args.push(val.into());
                                    } else {
                                        return None;
                                    }
                                }
                                let cs = match self.builder.build_call(
                                    fv,
                                    &lowered_args,
                                    "call_internal",
                                ) {
                                    Ok(cs) => cs,
                                    Err(_) => return None,
                                };
                                let either = cs.try_as_basic_value();
                                match either {
                                    inkwell::Either::Left(bv) => Some(bv),
                                    _ => None,
                                }
                            } else {
                                None
                            }
                        }
                        ast::Expr::Member(member) => {
                            use deno_ast::swc::ast::MemberProp;
                            if let MemberProp::Ident(prop_ident) = &member.prop {
                                let method_name = prop_ident.sym.to_string();
                                if let Some(obj_val) =
                                    self.lower_expr(&member.obj, function, param_map, locals)
                                {
                                    // try to find a function named `<Class>_<method>`
                                    for class_name in self.class_fields.borrow().keys() {
                                        let cand = format!("{}_{}", class_name, method_name);
                                        if let Some(method_f) = self.module.get_function(&cand) {
                                            // lower user args
                                            let mut user_args: Vec<
                                                inkwell::values::BasicMetadataValueEnum,
                                            > = Vec::new();
                                            for a in &call.args {
                                                if let Some(v) = self.lower_expr(
                                                    &a.expr, function, param_map, locals,
                                                ) {
                                                    user_args.push(v.into());
                                                } else {
                                                    return None;
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
                                                Err(_) => return None,
                                            };
                                            let either = cs.try_as_basic_value();
                                            if let inkwell::Either::Left(bv) = either {
                                                return Some(bv);
                                            } else {
                                                return None;
                                            }
                                        }
                                    }
                                }
                            }
                            None
                        }
                        _ => None,
                    }
                } else {
                    None
                }
            }
            ast::Expr::Assign(assign) => {
                // support simple assignments `ident = expr` where the left side is an identifier
                if let Some(bid) = assign.left.as_ident() {
                    let name = bid.id.sym.to_string();
                    if let Some((ptr, _ty, _init, is_const)) = self.find_local(locals, &name) {
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
                            return None;
                        }
                        if let Some(val) =
                            self.lower_expr(&assign.right, function, param_map, locals)
                        {
                            // If the local is a pointer type, and previously initialized, decrement old refcount
                            if _ty == self.i8ptr_t.as_basic_type_enum() {
                                // load old value
                                let old =
                                    match self.builder.build_load(self.i8ptr_t, ptr, "old_val") {
                                        Ok(v) => v,
                                        Err(_) => return None,
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
                                        Err(_) => return None,
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
                                        Err(_) => return None,
                                    };
                                }
                            } else {
                                let _ = self.builder.build_store(ptr, val);
                            }
                            // mark initialized after assignment
                            self.set_local_initialized(locals, &name, true);
                            return Some(val);
                        }
                    }
                }

                // Handle member assignment: obj.field = expr
                // Check if left side is a member expression
                use deno_ast::swc::ast::{AssignTarget, SimpleAssignTarget};
                if let AssignTarget::Simple(SimpleAssignTarget::Member(member)) = &assign.left {
                    // Lower the right-hand side value
                    if let Some(new_val) =
                        self.lower_expr(&assign.right, function, param_map, locals)
                    {
                        // Only handle dot-member (obj.prop), not computed (obj[expr])
                        use deno_ast::swc::ast::MemberProp;
                        if let MemberProp::Ident(prop_ident) = &member.prop {
                            let field_name = prop_ident.sym.to_string();

                            // Lower the object to get its pointer
                            if let Some(BasicValueEnum::PointerValue(obj_ptr)) =
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
                                                .expect("mul failed");
                                            let offset = self
                                                .builder
                                                .build_int_add(hdr_size, mul, "fld_off")
                                                .expect("add failed");
                                            let offset_i32 = self
                                                .builder
                                                .build_int_cast(
                                                    offset,
                                                    self.context.i32_type(),
                                                    "off_i32",
                                                )
                                                .expect("cast off_i32");

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
                                                Err(_) => return None,
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
                                                        .expect("cast f64_ptr failed");
                                                    let _ =
                                                        self.builder.build_store(f64_ptr, new_val);
                                                    return Some(new_val);
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
                                                            Err(_) => return None,
                                                        };

                                                    // Load old value for RC decrement
                                                    let old_val = match self.builder.build_load(
                                                        self.i8ptr_t,
                                                        slot_ptr,
                                                        "old_field_val",
                                                    ) {
                                                        Ok(v) => v,
                                                        Err(_) => return None,
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
                                                            Err(_) => return None,
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
                                                            Err(_) => return None,
                                                        };
                                                    }
                                                    return Some(new_val);
                                                }
                                                _ => {
                                                    // Unsupported field type
                                                    return None;
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
                None
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
                            Err(_) => return None,
                        };
                        // check not NaN: fv == fv
                        let is_not_nan = match self.builder.build_float_compare(
                            inkwell::FloatPredicate::OEQ,
                            fv,
                            fv,
                            "not_nan",
                        ) {
                            Ok(v) => v,
                            Err(_) => return None,
                        };
                        let cond =
                            match self.builder.build_and(is_not_zero, is_not_nan, "num_truth") {
                                Ok(v) => v,
                                Err(_) => return None,
                            };
                        cond.as_basic_value_enum()
                    }
                    BasicValueEnum::PointerValue(pv) => {
                        // pointer truthiness: non-null and non-empty string are truthy
                        let is_null = match self.builder.build_is_null(pv, "is_null") {
                            Ok(v) => v,
                            Err(_) => return None,
                        };
                        let is_not_null = match self.builder.build_not(is_null, "not_null") {
                            Ok(v) => v,
                            Err(_) => return None,
                        };
                        // call strlen(ptr) and check != 0
                        if let Some(strlen_fn) = self.module.get_function("strlen") {
                            let cs = match self.builder.build_call(
                                strlen_fn,
                                &[pv.into()],
                                "strlen_call",
                            ) {
                                Ok(cs) => cs,
                                Err(_) => return None,
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
                                    Err(_) => return None,
                                };
                                let cond = match self.builder.build_and(
                                    is_not_null,
                                    len_nonzero,
                                    "ptr_truth",
                                ) {
                                    Ok(v) => v,
                                    Err(_) => return None,
                                };
                                return Some(cond.as_basic_value_enum());
                            }
                        }
                        // fallback: non-null
                        is_not_null.as_basic_value_enum()
                    }
                    _ => return None,
                };

                // Create basic blocks
                let then_bb = self.context.append_basic_block(function, "then");
                let else_bb = self.context.append_basic_block(function, "else");
                let merge_bb = self.context.append_basic_block(function, "merge");

                // branch based on cond_i1
                let cond_val = self.to_condition_i1(cond_i1)?;
                self.builder
                    .build_conditional_branch(cond_val, then_bb, else_bb)
                    .expect("build_conditional_branch failed");

                // then
                self.builder.position_at_end(then_bb);
                let then_val = self.lower_expr(&cond.cons, function, param_map, locals);
                if self.builder.get_insert_block().is_some() {
                    let _ = self
                        .builder
                        .build_unconditional_branch(merge_bb)
                        .expect("build_unconditional_branch failed");
                }

                // else
                self.builder.position_at_end(else_bb);
                let else_val = self.lower_expr(&cond.alt, function, param_map, locals);
                if self.builder.get_insert_block().is_some() {
                    let _ = self
                        .builder
                        .build_unconditional_branch(merge_bb)
                        .expect("build_unconditional_branch failed");
                }

                // merge
                self.builder.position_at_end(merge_bb);
                // If both sides produced values, create a phi via helper
                if let (Some(tv), Some(ev)) = (then_val, else_val)
                    && let Some(phi) = self.build_phi_merge(then_bb, else_bb, tv, ev)
                {
                    return Some(phi);
                }

                None
            }
            ast::Expr::Lit(lit) => {
                use deno_ast::swc::ast::Lit;
                match lit {
                    Lit::Num(n) => {
                        let fv = self.f64_t.const_float(n.value);
                        Some(fv.as_basic_value_enum())
                    }
                    Lit::Bool(b) => {
                        let iv = self.bool_t.const_int(if b.value { 1 } else { 0 }, false);
                        Some(iv.as_basic_value_enum())
                    }
                    Lit::Str(s) => {
                        let bytes = s.value.as_bytes();
                        let key = String::from_utf8_lossy(bytes).into_owned();
                        // Check cache first (cache stores the computed pointer)
                        if let Some(ptr_val) = self.string_literals.borrow().get(&key) {
                            return Some(ptr_val.as_basic_value_enum());
                        }

                        let array_ty = self.context.i8_type().array_type((bytes.len() + 1) as u32);
                        // generate a unique global name for this string literal
                        let id = self.next_str_id.get();
                        let name = format!("strlit.{}", id);
                        self.next_str_id.set(id.wrapping_add(1));
                        let gv = self.module.add_global(array_ty, None, &name);
                        let const_array = self.context.const_string(bytes, true);
                        gv.set_initializer(&const_array);

                        let zero = self.i32_t.const_int(0, false);
                        let indices = &[zero, zero];
                        let gep = unsafe {
                            self.builder.build_gep(
                                array_ty,
                                gv.as_pointer_value(),
                                indices,
                                "strptr",
                            )
                        };
                        if let Ok(ptr) = gep {
                            // store pointer in cache for future reuse
                            self.string_literals.borrow_mut().insert(key, ptr);
                            return Some(ptr.as_basic_value_enum());
                        }
                        None
                    }
                    _ => None,
                }
            }
            ast::Expr::Array(arr) => {
                // Lower array literal: determine element kinds by lowering each elt
                let mut lowered_elems: Vec<BasicValueEnum> = Vec::new();
                for opt in &arr.elems {
                    if let Some(expr_or_spread) = opt {
                        // ExprOrSpread has .expr
                        if let Some(ev) =
                            self.lower_expr(&expr_or_spread.expr, function, param_map, locals)
                        {
                            lowered_elems.push(ev);
                        } else {
                            // unsupported element lowering
                            return None;
                        }
                    } else {
                        // elided element like [ , ] -> treat as undefined -> unsupported
                        return None;
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
                    Err(_) => return None,
                };
                let either = call_site.try_as_basic_value();
                let arr_ptr = match either {
                    inkwell::Either::Left(bv) => bv.into_pointer_value(),
                    _ => return None,
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
                    return None;
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
                                return None;
                            };
                            // bitcast to f64* (unwrap Result returned by pointer cast)
                            let elem_ptr = match self.builder.build_pointer_cast(
                                elem_i8,
                                self.context.ptr_type(AddressSpace::default()),
                                "elem_f64_ptr",
                            ) {
                                Ok(p) => p,
                                Err(_) => return None,
                            };
                            let _ = self.builder.build_store(elem_ptr, fv);
                        } else {
                            return None;
                        }
                    }
                    Some(arr_ptr.as_basic_value_enum())
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
                            return None;
                        };
                        // bitcast to i8** (pointer-to-pointer)
                        let elem_ptr = match self.builder.build_pointer_cast(
                            elem_i8,
                            self.context.ptr_type(AddressSpace::default()),
                            "elem_ptrptr",
                        ) {
                            Ok(p) => p,
                            Err(_) => return None,
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
                                    Err(_) => return None,
                                };
                            }
                            BasicValueEnum::IntValue(iv) => {
                                // store integer as-is into pointer slot (coerce as i8*)
                                let _ = self.builder.build_store(elem_ptr, iv);
                            }
                            _ => return None,
                        }
                    }
                    Some(arr_ptr.as_basic_value_enum())
                }
            }
            ast::Expr::This(_) => {
                if let Some((ptr, ty, init, _)) = self.find_local(locals, "this")
                    && init
                        && let Ok(loaded) = self.builder.build_load(ty, ptr, "this_load") {
                            return Some(loaded);
                        }
                if let Some(idx) = param_map.get("this") {
                    if let Some(pv) = function.get_nth_param(*idx) {
                        return Some(pv);
                    } else {
                        return None;
                    }
                }
                None
            }
            ast::Expr::Member(member) => {
                // Support both computed member access (obj[expr]) and dot-member (obj.prop)
                use deno_ast::swc::ast::MemberProp;
                match &member.prop {
                    MemberProp::Computed(boxed) => {
                        // lower object and index
                        if let Some(obj_val) =
                            self.lower_expr(&member.obj, function, param_map, locals)
                            && let Some(idx_val) =
                                self.lower_expr(&boxed.expr, function, param_map, locals)
                        {
                            // only support pointer-array indexing (i8**)
                            if let BasicValueEnum::PointerValue(arr_ptr) = obj_val {
                                // compute index as i64 for runtime helpers
                                let idx_i64 = match idx_val {
                                    BasicValueEnum::IntValue(iv) => self
                                        .builder
                                        .build_int_cast(iv, self.i64_t, "idx_i64")
                                        .expect("int cast"),
                                    BasicValueEnum::FloatValue(fv) => self
                                        .builder
                                        .build_float_to_signed_int(fv, self.i64_t, "f2i")
                                        .expect("f2i"),
                                    _ => return None,
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
                                            .expect("cast idx to i64"),
                                        BasicValueEnum::FloatValue(fv) => self
                                            .builder
                                            .build_float_to_signed_int(fv, self.i64_t, "f2i_i64")
                                            .expect("f2i"),
                                        _ => return None,
                                    };
                                    let array_get = self.get_array_get_f64();
                                    let cs = match self.builder.build_call(
                                        array_get,
                                        &[arr_ptr.into(), idx_i64.into()],
                                        "array_get_f64_call",
                                    ) {
                                        Ok(cs) => cs,
                                        Err(_) => return None,
                                    };
                                    let either = cs.try_as_basic_value();
                                    if let inkwell::Either::Left(bv) = either {
                                        return Some(bv);
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
                                    Err(_) => return None,
                                };
                                let either = cs.try_as_basic_value();
                                if let inkwell::Either::Left(bv) = either {
                                    return Some(bv);
                                }
                            }
                        }
                    }
                    MemberProp::Ident(prop_ident) => {
                        // dot-member access like obj.prop
                        let field_name = prop_ident.sym.to_string();
                        if let Some(BasicValueEnum::PointerValue(obj_ptr)) =
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
                                    && let crate::types::OatsType::NominalStruct(n) =
                                        &param_types[0]
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
                                            .expect("mul failed");
                                        let offset = self
                                            .builder
                                            .build_int_add(hdr_size, mul, "fld_off")
                                            .expect("add failed");
                                        // We need a i32 index sequence for gep on i8 pointer: cast offset to i64->i32 for index
                                        let offset_i64 = offset;
                                        let offset_i32 = self
                                            .builder
                                            .build_int_cast(
                                                offset_i64,
                                                self.context.i32_type(),
                                                "off_i32",
                                            )
                                            .expect("cast off_i32");
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
                                            Err(_) => return None,
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
                                                    .expect("cast f64_ptr failed");
                                                let loaded = match self.builder.build_load(
                                                    self.f64_t,
                                                    f64_ptr,
                                                    "field_f64_load",
                                                ) {
                                                    Ok(v) => v,
                                                    Err(_) => return None,
                                                };
                                                return Some(loaded.as_basic_value_enum());
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
                                                        Err(_) => return None,
                                                    };
                                                // load slot (an i8*)
                                                let loaded = match self.builder.build_load(
                                                    self.i8ptr_t,
                                                    slot_ptr,
                                                    "field_load",
                                                ) {
                                                    Ok(v) => v,
                                                    Err(_) => return None,
                                                };
                                                return Some(loaded.as_basic_value_enum());
                                            }
                                            _ => {
                                                // Unsupported field type
                                                return None;
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
                None
            }
            ast::Expr::New(new_expr) => {
                if let ast::Expr::Ident(ident) = &*new_expr.callee {
                    let ctor_name = format!("{}_ctor", ident.sym);
                    if let Some(fv) = self.module.get_function(&ctor_name) {
                        let mut lowered_args: Vec<inkwell::values::BasicMetadataValueEnum> =
                            Vec::new();
                        if let Some(args) = &new_expr.args {
                            for a in args {
                                if let Some(val) =
                                    self.lower_expr(&a.expr, function, param_map, locals)
                                {
                                    lowered_args.push(val.into());
                                } else {
                                    return None;
                                }
                            }
                        }
                        let cs = self
                            .builder
                            .build_call(fv, &lowered_args, "new_call")
                            .expect("build_call failed");
                        let either = cs.try_as_basic_value();
                        match either {
                            inkwell::Either::Left(bv) => Some(bv),
                            _ => None,
                        }
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            _ => None,
        }
    }
}
