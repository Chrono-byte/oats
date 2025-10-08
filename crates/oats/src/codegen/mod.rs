use inkwell::AddressSpace;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::BasicType;
use inkwell::types::BasicTypeEnum;
use inkwell::values::{BasicValue, BasicValueEnum, FunctionValue, PointerValue};
use std::cell::{Cell, RefCell};
use std::collections::HashMap;

type LocalEntry<'a> = (PointerValue<'a>, BasicTypeEnum<'a>, bool, bool);
type LocalsStackLocal<'a> = Vec<HashMap<String, LocalEntry<'a>>>;

pub mod helpers;
/// Mapping from function name -> param OatsTypes vector. Stored so
/// lowering can inspect the declared nominal type of `this` parameters
/// (e.g. to resolve a `this.field` access to a concrete class name).
pub struct CodeGen<'a> {
    pub context: &'a Context,
    pub module: Module<'a>,
    pub builder: Builder<'a>,
    // Monotonic counter used to generate unique names for string literal
    // globals (e.g. `strlit.0`, `strlit.1`, ...). Using `Cell` lets us
    // mutate this counter from &self without requiring &mut.
    pub next_str_id: Cell<u32>,
    // Cache string literal contents to their emitted global values so
    // identical literals are emitted once and reused. Using `RefCell`
    // lets us mutate this from `&self`.
    pub string_literals: RefCell<HashMap<String, PointerValue<'a>>>,
    // Cache commonly used LLVM types to avoid repeated calls into Context
    pub f64_t: inkwell::types::FloatType<'a>,
    pub i64_t: inkwell::types::IntType<'a>,
    pub i32_t: inkwell::types::IntType<'a>,
    pub bool_t: inkwell::types::IntType<'a>,
    pub i8ptr_t: inkwell::types::PointerType<'a>,
    // Cache declared runtime helper functions once added
    pub fn_print_f64: RefCell<Option<FunctionValue<'a>>>,
    pub fn_print_str: RefCell<Option<FunctionValue<'a>>>,
    pub fn_strlen: RefCell<Option<FunctionValue<'a>>>,
    pub fn_malloc: RefCell<Option<FunctionValue<'a>>>,
    pub fn_memcpy: RefCell<Option<FunctionValue<'a>>>,
    pub fn_free: RefCell<Option<FunctionValue<'a>>>,
    pub fn_array_alloc: RefCell<Option<FunctionValue<'a>>>,
    pub fn_rc_inc: RefCell<Option<FunctionValue<'a>>>,
    pub fn_rc_dec: RefCell<Option<FunctionValue<'a>>>,
    /// Map of nominal struct name -> ordered list of (field name, field type)
    /// Populated by the frontend (main.rs) when scanning class declarations.
    pub class_fields: RefCell<HashMap<String, Vec<(String, crate::types::OatsType)>>>,
    /// Mapping from function name -> param OatsTypes vector. Stored so
    /// lowering can inspect the declared nominal type of `this` parameters
    /// (e.g. to resolve a `this.field` access to a concrete class name).
    pub fn_param_types: RefCell<HashMap<String, Vec<crate::types::OatsType>>>,
    /// Preprocessed source text used to map spans for diagnostics
    pub source: &'a str,
}

impl<'a> CodeGen<'a> {
    fn get_array_alloc(&self) -> FunctionValue<'a> {
        if let Some(f) = *self.fn_array_alloc.borrow() {
            return f;
        }
        // declare: i8* @array_alloc(i64, i32, i32)
        let i8ptr = self.i8ptr_t;
        let i64t = self.i64_t;
        let i32t = self.i32_t;
        let fn_type = i8ptr.fn_type(&[i64t.into(), i32t.into(), i32t.into()], false);
        let f = self.module.add_function("array_alloc", fn_type, None);
        *self.fn_array_alloc.borrow_mut() = Some(f);
        f
    }

    pub fn get_rc_inc(&self) -> FunctionValue<'a> {
        if let Some(f) = *self.fn_rc_inc.borrow() {
            return f;
        }
        let voidt = self.context.void_type();
        let i8ptr = self.i8ptr_t;
        let fn_type = voidt.fn_type(&[i8ptr.into()], false);
        let f = self.module.add_function("rc_inc", fn_type, None);
        *self.fn_rc_inc.borrow_mut() = Some(f);
        f
    }

    fn get_rc_dec(&self) -> FunctionValue<'a> {
        if let Some(f) = *self.fn_rc_dec.borrow() {
            return f;
        }
        let voidt = self.context.void_type();
        let i8ptr = self.i8ptr_t;
        let fn_type = voidt.fn_type(&[i8ptr.into()], false);
        let f = self.module.add_function("rc_dec", fn_type, None);
        *self.fn_rc_dec.borrow_mut() = Some(f);
        f
    }

    fn get_array_get_f64(&self) -> FunctionValue<'a> {
        if let Some(f) = self.module.get_function("array_get_f64") {
            return f;
        }
        let f64t = self.context.f64_type();
        let i8ptr = self.i8ptr_t;
        let i64t = self.i64_t;
        let fn_type = f64t.fn_type(&[i8ptr.into(), i64t.into()], false);

        self.module.add_function("array_get_f64", fn_type, None)
    }

    fn get_array_get_ptr(&self) -> FunctionValue<'a> {
        if let Some(f) = self.module.get_function("array_get_ptr") {
            return f;
        }
        let i8ptr = self.i8ptr_t;
        let i64t = self.i64_t;
        let fn_type = i8ptr.fn_type(&[i8ptr.into(), i64t.into()], false);

        self.module.add_function("array_get_ptr", fn_type, None)
    }

    // Borrowing variant: returns a borrowed pointer without incrementing
    // the refcount. Use only when you can guarantee the pointer's lifetime
    // doesn't outlive the array slot, or explicitly call `rc_inc` when
    // converting to an owned reference.
    #[allow(dead_code)]
    fn get_array_get_ptr_borrow(&self) -> FunctionValue<'a> {
        if let Some(f) = self.module.get_function("array_get_ptr_borrow") {
            return f;
        }
        let i8ptr = self.i8ptr_t;
        let i64t = self.i64_t;
        let fn_type = i8ptr.fn_type(&[i8ptr.into(), i64t.into()], false);

        self.module
            .add_function("array_get_ptr_borrow", fn_type, None)
    }

    /// Map of nominal struct name -> ordered list of field names.
    /// Populated by the frontend (main.rs) when scanning class declarations.
    pub fn _class_fields_placeholder(&self) {}

    fn get_array_set_ptr(&self) -> FunctionValue<'a> {
        if let Some(f) = self.module.get_function("array_set_ptr") {
            return f;
        }
        let voidt = self.context.void_type();
        let i8ptr = self.i8ptr_t;
        let i64t = self.i64_t;
        let fn_type = voidt.fn_type(&[i8ptr.into(), i64t.into(), i8ptr.into()], false);

        self.module.add_function("array_set_ptr", fn_type, None)
    }

    /// Thin adapter that converts the existing Option-based `lower_expr` into
    /// a Result carrying a `Diagnostic` so callers can centrally report errors.
    fn lower_expr_result(
        &self,
        expr: &deno_ast::swc::ast::Expr,
        function: inkwell::values::FunctionValue<'a>,
        param_map: &HashMap<String, u32>,
        locals: &mut Vec<
            HashMap<
                String,
                (
                    inkwell::values::PointerValue<'a>,
                    BasicTypeEnum<'a>,
                    bool,
                    bool,
                ),
            >,
        >,
        ctx_name: Option<&str>,
    ) -> Result<inkwell::values::BasicValueEnum<'a>, crate::diagnostics::Diagnostic> {
        if let Some(v) = self.lower_expr(expr, function, param_map, locals) {
            Ok(v)
        } else {
            let msg = format!(
                "failed to lower expression in context '{}'",
                ctx_name.unwrap_or("<unknown>")
            );
            Err(crate::diagnostics::Diagnostic::simple(msg))
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
                    if let Err(_) = self
                        .builder
                        .build_conditional_branch(cond, then_bb, else_bb)
                    {
                        return None;
                    }
                    // then: evaluate right
                    self.builder.position_at_end(then_bb);
                    let rv = self.lower_expr(&bin.right, function, param_map, locals);
                    if self.builder.get_insert_block().is_some() {
                        if let Err(_) = self.builder.build_unconditional_branch(merge_bb) {
                            return None;
                        }
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
                    if let Err(_) = self
                        .builder
                        .build_conditional_branch(cond, then_bb, else_bb)
                    {
                        return None;
                    }
                    // then: keep left
                    self.builder.position_at_end(then_bb);
                    if self.builder.get_insert_block().is_some() {
                        if let Err(_) = self.builder.build_unconditional_branch(merge_bb) {
                            return None;
                        }
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
                // First check locals (alloca slots) by searching scope stack
                if let Some((ptr, ty, initialized, _is_const)) = self.find_local(locals, &name) {
                    // If not initialized -> TDZ: generate a trap (unreachable)
                    if !initialized {
                        // Emit a call to unreachable to trap at runtime
                        let _ = self.builder.build_unreachable();
                        return None;
                    }
                    // build_load signature depends on LLVM version; for newer
                    // inkwell it expects (pointee_ty, ptr, name).
                    let loaded = match self.builder.build_load(ty, ptr, &name) {
                        Ok(v) => v,
                        Err(_) => return None,
                    };
                    return Some(loaded);
                }
                // Fallback to function parameter (should be rare since params are
                // typically allocated into locals by gen_function_ir)
                if let Some(idx) = param_map.get(&name) {
                    if let Some(pv) = function.get_nth_param(*idx) {
                        return Some(pv);
                    } else {
                        return None;
                    }
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
                                let _ = match self.builder.build_call(
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
                if let Some((ptr, ty, init, _)) = self.find_local(locals, "this") {
                    if init {
                        if let Ok(loaded) = self.builder.build_load(ty, ptr, "this_load") {
                            return Some(loaded);
                        }
                    }
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
                            } else if matches!(&*member.obj, deno_ast::swc::ast::Expr::This(_)) {
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
                    let ctor_name = format!("{}_ctor", ident.sym.to_string());
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

    pub fn gen_function_ir(
        &self,
        func_name: &str,
        func_decl: &deno_ast::swc::ast::Function,
        param_types: &[crate::types::OatsType],
        ret_type: &crate::types::OatsType,
        receiver_name: Option<&str>,
    ) -> FunctionValue<'a> {
        let llvm_param_types: Vec<inkwell::types::BasicTypeEnum> = param_types
            .iter()
            .map(|t| self.map_type_to_llvm(t))
            .collect();

        // Build function type, supporting Void return.
        let fn_type = match ret_type {
            crate::types::OatsType::Number => {
                let ft = self.context.f64_type();
                let args: Vec<inkwell::types::BasicTypeEnum> = llvm_param_types.clone();
                ft.fn_type(&args.iter().map(|a| (*a).into()).collect::<Vec<_>>(), false)
            }
            crate::types::OatsType::Boolean => {
                let it = self.bool_t;
                let args: Vec<inkwell::types::BasicTypeEnum> = llvm_param_types.clone();
                it.fn_type(&args.iter().map(|a| (*a).into()).collect::<Vec<_>>(), false)
            }
            crate::types::OatsType::String | crate::types::OatsType::NominalStruct(_) => {
                let pt = self.context.ptr_type(AddressSpace::default());
                let args: Vec<inkwell::types::BasicTypeEnum> = llvm_param_types.clone();
                pt.fn_type(&args.iter().map(|a| (*a).into()).collect::<Vec<_>>(), false)
            }
            crate::types::OatsType::Array(_) => {
                // Arrays are represented as i8* (opaque pointer to runtime array)
                let pt = self.context.ptr_type(AddressSpace::default());
                let args: Vec<inkwell::types::BasicTypeEnum> = llvm_param_types.clone();
                pt.fn_type(&args.iter().map(|a| (*a).into()).collect::<Vec<_>>(), false)
            }
            crate::types::OatsType::Void => {
                let vt = self.context.void_type();
                let args: Vec<inkwell::types::BasicTypeEnum> = llvm_param_types.clone();
                vt.fn_type(&args.iter().map(|a| (*a).into()).collect::<Vec<_>>(), false)
            }
        };

        // Ensure helper runtime functions (like str_concat) are emitted into the module
        self.gen_str_concat();

        let function = self.module.add_function(func_name, fn_type, None);

        // Record the declared parameter types for this function so lowering
        // (e.g. member access for `this`) can inspect the nominal type of
        // the receiver. We store the original OatsType vector under the
        // function name for later lookup.
        self.fn_param_types
            .borrow_mut()
            .insert(func_name.to_string(), param_types.to_vec());

        let entry = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(entry);

        // Build param name -> index map from function declaration params.
        // If a receiver_name is provided, map it to index 0 and shift other
        // param indices by +1 to account for the implicit `this` leading
        // parameter which is present in `param_types` but not in the AST.
        let mut param_map: HashMap<String, u32> = HashMap::new();
        if let Some(rname) = receiver_name {
            param_map.insert(rname.to_string(), 0u32);
        }
        for (i, p) in func_decl.params.iter().enumerate() {
            use deno_ast::swc::ast::Pat;
            if let Pat::Ident(ident) = &p.pat {
                let name = ident.id.sym.to_string();
                // index in LLVM params is offset by 1 if receiver present
                let idx = if receiver_name.is_some() {
                    (i + 1) as u32
                } else {
                    i as u32
                };
                param_map.insert(name, idx);
            }
        }

        // Allocate stack slots for function parameters and store incoming
        // parameter values into them so parameters behave like locals.
        // We create these allocas in the entry block before emitting body.
        let mut locals_stack: Vec<
            HashMap<
                String,
                (
                    inkwell::values::PointerValue<'a>,
                    BasicTypeEnum<'a>,
                    bool,
                    bool,
                ),
            >,
        > = Vec::new();
        locals_stack.push(HashMap::new());
        for (name, idx) in &param_map {
            let i = *idx as usize;
            if i >= llvm_param_types.len() {
                continue;
            }
            let param_ty = llvm_param_types[i];
            // Create an alloca for the parameter's LLVM type
            let alloca = match self.builder.build_alloca(param_ty, name) {
                Ok(a) => a,
                Err(_) => {
                    // If allocation fails at IR build time, insert unreachable and skip this param
                    let _ = self.builder.build_unreachable();
                    continue;
                }
            };
            // Store the incoming parameter into the alloca
            if let Some(pv) = function.get_nth_param(*idx) {
                let _ = self.builder.build_store(alloca, pv);
                // If this parameter is a pointer type, increment refcount for the stored value
                if param_ty == self.i8ptr_t.as_basic_type_enum() {
                    let rc_inc = self.get_rc_inc();
                    let _ = self
                        .builder
                        .build_call(rc_inc, &[pv.into()], "rc_inc_param")
                        .expect("build_call failed");
                }
            } else {
                // Missing param value: skip storing and continue
                continue;
            }
        }

        if let Some(body) = &func_decl.body {
            use deno_ast::swc::ast;
            let mut emitted_return = false;
            // locals_stack holds per-scope HashMaps for local variables
            // (pointer, type, initialized, is_const). The current scope is
            // already present (we pushed param slots earlier).
            for stmt in &body.stmts {
                match stmt {
                    ast::Stmt::Return(ret) => {
                        if let Some(arg) = &ret.arg {
                            // Use result-based lowering so we can emit diagnostics centrally
                            match self.lower_expr_result(
                                arg,
                                function,
                                &param_map,
                                &mut locals_stack,
                                Some(function.get_name().to_str().unwrap_or("")),
                            ) {
                                Ok(val) => {
                                    self.emit_rc_dec_for_locals(&locals_stack);
                                    let _ = self.builder.build_return(Some(&val));
                                }
                                Err(d) => {
                                    // Emit diagnostic and return void to avoid panics
                                    crate::diagnostics::emit_diagnostic(&d, None);
                                    self.emit_rc_dec_for_locals(&locals_stack);
                                    let _ = self.builder.build_return(None);
                                }
                            }
                        } else {
                            let _ = self.builder.build_return(None);
                        }
                        emitted_return = true;
                        break;
                    }
                    ast::Stmt::Expr(expr_stmt) => {
                        use deno_ast::swc::ast;
                        // If this is an assignment statement and the LHS is a `this.field`
                        // pattern, lower it specially so we can compute the field slot and
                        // perform refcount updates. Prefer an AST-based check rather than
                        // scanning the source text for `this.` which is brittle.
                        if let ast::Expr::Assign(assign) = &*expr_stmt.expr {
                            // Try AST-based LHS detection first: left.as_member() -> MemberExpr
                            if let deno_ast::swc::ast::AssignTarget::Simple(simple_target) =
                                &assign.left
                                && let deno_ast::swc::ast::SimpleAssignTarget::Member(member) =
                                    simple_target
                                && let deno_ast::swc::ast::MemberProp::Ident(prop_ident) =
                                    &member.prop
                            {
                                // Ensure object is an identifier named `this` (or a param that maps to a nominal)
                                if let deno_ast::swc::ast::Expr::Ident(obj_ident) = &*member.obj {
                                    let obj_name = obj_ident.sym.to_string();
                                    if obj_name == "this" {
                                        // Lower RHS value
                                        if let Some(val) = self
                                            .lower_expr_result(
                                                &assign.right,
                                                function,
                                                &param_map,
                                                &mut locals_stack,
                                                Some(function.get_name().to_str().unwrap_or("")),
                                            )
                                            .ok()
                                        {
                                            // If we have a `this` parameter, the function param map should
                                            // contain its index (usually 0). Use the incoming parameter
                                            // value rather than the alloca to obtain the object pointer.
                                            if let Some(this_idx) = param_map.get("this")
                                                && let Some(pv) = function.get_nth_param(*this_idx)
                                                && let BasicValueEnum::PointerValue(obj_ptr) = pv
                                            {
                                                // lookup class name via fn_param_types
                                                if let Some(param_types) = self
                                                    .fn_param_types
                                                    .borrow()
                                                    .get(function.get_name().to_str().unwrap_or(""))
                                                    && !param_types.is_empty()
                                                    && let crate::types::OatsType::NominalStruct(n) =
                                                        &param_types[0]
                                                {
                                                    let class_name = n.clone();
                                                    if let Some(fields) =
                                                        self.class_fields.borrow().get(&class_name)
                                                    {
                                                        let field_name = prop_ident.sym.to_string();
                                                        if let Some((
                                                            field_idx,
                                                            (_fname, _field_ty),
                                                        )) = fields
                                                            .iter()
                                                            .enumerate()
                                                            .find(|(_, (n, _))| n == &field_name)
                                                        {
                                                            // compute byte offset = hdr_size + idx * ptr_size
                                                            let hdr_size = self.i64_t.const_int(
                                                                std::mem::size_of::<u64>() as u64,
                                                                false,
                                                            );
                                                            let ptr_sz = self.i64_t.const_int(
                                                                std::mem::size_of::<usize>() as u64,
                                                                false,
                                                            );
                                                            let idx_const = self
                                                                .i64_t
                                                                .const_int(field_idx as u64, false);
                                                            let mul = self
                                                                .builder
                                                                .build_int_mul(
                                                                    idx_const,
                                                                    ptr_sz,
                                                                    "fld_off_mul",
                                                                )
                                                                .expect("mul failed");
                                                            let offset = self
                                                                .builder
                                                                .build_int_add(
                                                                    hdr_size, mul, "fld_off",
                                                                )
                                                                .expect("add failed");
                                                            let offset_i32 = self
                                                                .builder
                                                                .build_int_cast(
                                                                    offset,
                                                                    self.context.i32_type(),
                                                                    "off_i32",
                                                                )
                                                                .expect("cast off_i32");
                                                            // GEP from i8* by byte offset
                                                            let gep_res = unsafe {
                                                                self.builder.build_gep(
                                                                    self.context.i8_type(),
                                                                    obj_ptr,
                                                                    &[offset_i32],
                                                                    "field_i8ptr",
                                                                )
                                                            };
                                                            let gep_ptr = match gep_res {
                                                                Ok(p) => p,
                                                                Err(_) => {
                                                                    let _ = self
                                                                        .builder
                                                                        .build_unreachable();
                                                                    // fallback to normal lowering
                                                                    self.lower_expr(
                                                                        &expr_stmt.expr,
                                                                        function,
                                                                        &param_map,
                                                                        &mut locals_stack,
                                                                    );
                                                                    continue;
                                                                }
                                                            };
                                                            // Cast to pointer slot and store appropriately
                                                            let slot_ptr_ty = self
                                                                .context
                                                                .ptr_type(AddressSpace::default());
                                                            let slot_ptr = match self
                                                                .builder
                                                                .build_pointer_cast(
                                                                    gep_ptr,
                                                                    slot_ptr_ty,
                                                                    "slot_ptr_cast",
                                                                ) {
                                                                Ok(p) => p,
                                                                Err(_) => {
                                                                    let _ = self
                                                                        .builder
                                                                        .build_unreachable();
                                                                    // fallback to normal lowering
                                                                    self.lower_expr(
                                                                        &expr_stmt.expr,
                                                                        function,
                                                                        &param_map,
                                                                        &mut locals_stack,
                                                                    );
                                                                    continue;
                                                                }
                                                            };
                                                            match val {
                                                                BasicValueEnum::PointerValue(
                                                                    newpv,
                                                                ) => {
                                                                    // load old value
                                                                    let old = match self
                                                                        .builder
                                                                        .build_load(
                                                                            self.i8ptr_t,
                                                                            slot_ptr,
                                                                            "old_field",
                                                                        ) {
                                                                        Ok(v) => v,
                                                                        Err(_) => {
                                                                            let _ = self
                                                                                .builder
                                                                                .build_unreachable(
                                                                                );
                                                                            // fallback
                                                                            self.lower_expr(
                                                                                &expr_stmt.expr,
                                                                                function,
                                                                                &param_map,
                                                                                &mut locals_stack,
                                                                            );
                                                                            continue;
                                                                        }
                                                                    };
                                                                    // call rc_dec on old (runtime should handle null)
                                                                    let rc_dec = self.get_rc_dec();
                                                                    let _ = match self
                                                                        .builder
                                                                        .build_call(
                                                                            rc_dec,
                                                                            &[old.into()],
                                                                            "rc_dec_old_field",
                                                                        ) {
                                                                        Ok(_) => (),
                                                                        Err(_) => {
                                                                            let _ = self
                                                                                .builder
                                                                                .build_unreachable(
                                                                                );
                                                                            continue;
                                                                        }
                                                                    };
                                                                    // store new pointer
                                                                    let _ = self
                                                                        .builder
                                                                        .build_store(
                                                                        slot_ptr,
                                                                        newpv.as_basic_value_enum(),
                                                                    );
                                                                    // increment refcount of new value
                                                                    let rc_inc = self.get_rc_inc();
                                                                    let _ = match self
                                                                        .builder
                                                                        .build_call(
                                                                            rc_inc,
                                                                            &[newpv.into()],
                                                                            "rc_inc_field",
                                                                        ) {
                                                                        Ok(_) => (),
                                                                        Err(_) => {
                                                                            let _ = self
                                                                                .builder
                                                                                .build_unreachable(
                                                                                );
                                                                            continue;
                                                                        }
                                                                    };
                                                                }
                                                                BasicValueEnum::FloatValue(fv) => {
                                                                    // cast slot to opaque pointer and store as f64
                                                                    let elem_ptr = match self.builder.build_pointer_cast(
                                                                        gep_ptr,
                                                                        self.context.ptr_type(AddressSpace::default()),
                                                                        "elem_f64_ptr",
                                                                    ) {
                                                                        Ok(p) => p,
                                                                        Err(_) => {
                                                                            let _ = self.builder.build_unreachable();
                                                                            self.lower_expr(
                                                                                &expr_stmt.expr,
                                                                                function,
                                                                                &param_map,
                                                                                &mut locals_stack,
                                                                            );
                                                                            continue;
                                                                        }
                                                                    };
                                                                    let _ = self
                                                                        .builder
                                                                        .build_store(elem_ptr, fv);
                                                                }
                                                                _ => {
                                                                    // unsupported RHS type for member write: fallback to normal lowering
                                                                    let _ = self.lower_expr(
                                                                        &expr_stmt.expr,
                                                                        function,
                                                                        &param_map,
                                                                        &mut locals_stack,
                                                                    );
                                                                }
                                                            }
                                                            // we've handled the assignment; skip default lowering
                                                            continue;
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        } // Fallback: Lower expression for side-effects (e.g., println calls)
                        let _ = self.lower_expr(
                            &expr_stmt.expr,
                            function,
                            &param_map,
                            &mut locals_stack,
                        );
                    }
                    ast::Stmt::Decl(ast::Decl::Var(vdecl)) => {
                        // Evaluate simple variable initializers and create allocas
                        use deno_ast::swc::ast::Pat;
                        // Decide const-ness for the whole declaration.
                        // Use `let` as the mutable option; `const` is immutable.
                        // Parser enforces that `var` is disallowed; codegen no longer emits a diagnostic here.
                        for decl in &vdecl.decls {
                            if let Pat::Ident(ident) = &decl.name {
                                let name = ident.id.sym.to_string();
                                let is_const_decl =
                                    matches!(vdecl.kind, deno_ast::swc::ast::VarDeclKind::Const);
                                // Create an alloca for this local at function entry
                                // and insert it into the current scope as uninitialized
                                if let Some(init) = &decl.init {
                                    if let Some(val) = self
                                        .lower_expr_result(
                                            init,
                                            function,
                                            &param_map,
                                            &mut locals_stack,
                                            Some(function.get_name().to_str().unwrap_or("")),
                                        )
                                        .ok()
                                    {
                                        match val {
                                            BasicValueEnum::FloatValue(fv) => {
                                                let alloca = match self
                                                    .builder
                                                    .build_alloca(self.f64_t, &name)
                                                {
                                                    Ok(a) => a,
                                                    Err(_) => {
                                                        let _ = self.builder.build_unreachable();
                                                        continue;
                                                    }
                                                };
                                                // insert as uninitialized first to model TDZ
                                                self.insert_local_current_scope(
                                                    &mut locals_stack,
                                                    name.clone(),
                                                    alloca,
                                                    self.f64_t.as_basic_type_enum(),
                                                    false,
                                                    is_const_decl,
                                                );
                                                let _ = self.builder.build_store(alloca, fv);
                                                // mark initialized after storing initializer
                                                self.set_local_initialized(
                                                    &mut locals_stack,
                                                    &name,
                                                    true,
                                                );
                                            }
                                            BasicValueEnum::PointerValue(pv) => {
                                                let ptr_ty = self.i8ptr_t;
                                                let alloca = match self
                                                    .builder
                                                    .build_alloca(ptr_ty, &name)
                                                {
                                                    Ok(a) => a,
                                                    Err(_) => {
                                                        let _ = self.builder.build_unreachable();
                                                        continue;
                                                    }
                                                };
                                                self.insert_local_current_scope(
                                                    &mut locals_stack,
                                                    name.clone(),
                                                    alloca,
                                                    ptr_ty.as_basic_type_enum(),
                                                    false,
                                                    is_const_decl,
                                                );
                                                let _ = self.builder.build_store(alloca, pv);
                                                // increment refcount for the newly-stored pointer
                                                let rc_inc = self.get_rc_inc();
                                                let _ = self
                                                    .builder
                                                    .build_call(
                                                        rc_inc,
                                                        &[pv.into()],
                                                        "rc_inc_var_init",
                                                    )
                                                    .expect("build_call failed");
                                                self.set_local_initialized(
                                                    &mut locals_stack,
                                                    &name,
                                                    true,
                                                );
                                            }
                                            BasicValueEnum::IntValue(iv) => {
                                                let boolt = self.bool_t;
                                                let alloca =
                                                    match self.builder.build_alloca(boolt, &name) {
                                                        Ok(a) => a,
                                                        Err(_) => {
                                                            let _ =
                                                                self.builder.build_unreachable();
                                                            continue;
                                                        }
                                                    };
                                                self.insert_local_current_scope(
                                                    &mut locals_stack,
                                                    name.clone(),
                                                    alloca,
                                                    boolt.as_basic_type_enum(),
                                                    false,
                                                    is_const_decl,
                                                );
                                                let _ = self.builder.build_store(alloca, iv);
                                                self.set_local_initialized(
                                                    &mut locals_stack,
                                                    &name,
                                                    true,
                                                );
                                            }
                                            _ => {
                                                // Unsupported initializer type; still insert uninitialized local
                                                // so future references can produce TDZ errors.
                                                let ptr_ty =
                                                    self.context.ptr_type(AddressSpace::default());
                                                let alloca = self
                                                    .builder
                                                    .build_alloca(ptr_ty, &name)
                                                    .expect("build_alloca failed");
                                                self.insert_local_current_scope(
                                                    &mut locals_stack,
                                                    name.clone(),
                                                    alloca,
                                                    ptr_ty.as_basic_type_enum(),
                                                    false,
                                                    is_const_decl,
                                                );
                                            }
                                        }
                                    }
                                } else {
                                    // No initializer: for const this is a syntax error in JS/TS
                                    // For now, insert uninitialized const/local and let later checks catch usage.
                                    let ptr_ty = self.context.ptr_type(AddressSpace::default());
                                    let alloca = match self.builder.build_alloca(ptr_ty, &name) {
                                        Ok(a) => a,
                                        Err(_) => {
                                            let _ = self.builder.build_unreachable();
                                            continue;
                                        }
                                    };
                                    self.insert_local_current_scope(
                                        &mut locals_stack,
                                        name.clone(),
                                        alloca,
                                        ptr_ty.as_basic_type_enum(),
                                        false,
                                        is_const_decl,
                                    );
                                }
                            }
                        }
                    }
                    ast::Stmt::If(if_stmt) => {
                        // Lower an if statement: create then/else/merge blocks and
                        // lower the consequent/alternative as statements.
                        // Lower the test to i1
                        if let Some(test_val) = self
                            .lower_expr_result(
                                &if_stmt.test,
                                function,
                                &param_map,
                                &mut locals_stack,
                                Some(function.get_name().to_str().unwrap_or("")),
                            )
                            .ok()
                        {
                            let cond_i1 = match test_val {
                                BasicValueEnum::IntValue(iv) => iv,
                                BasicValueEnum::FloatValue(fv) => {
                                    let zero = self.context.f64_type().const_float(0.0);

                                    self.builder
                                        .build_float_compare(
                                            inkwell::FloatPredicate::ONE,
                                            fv,
                                            zero,
                                            "if_tcmp",
                                        )
                                        .expect("build_float_compare failed")
                                }
                                _ => {
                                    // unsupported test type -> skip lowering
                                    continue;
                                }
                            };

                            let then_bb = self.context.append_basic_block(function, "if.then");
                            let else_bb = self.context.append_basic_block(function, "if.else");
                            let merge_bb = self.context.append_basic_block(function, "if.merge");

                            self.builder
                                .build_conditional_branch(cond_i1, then_bb, else_bb)
                                .expect("build_conditional_branch failed");

                            // then
                            self.builder.position_at_end(then_bb);
                            match &*if_stmt.cons {
                                ast::Stmt::Block(block) => {
                                    for s in &block.stmts {
                                        match s {
                                            ast::Stmt::Return(ret) => {
                                                if let Some(arg) = &ret.arg {
                                                    if let Some(val) = self
                                                        .lower_expr_result(
                                                            arg,
                                                            function,
                                                            &param_map,
                                                            &mut locals_stack,
                                                            Some(
                                                                function
                                                                    .get_name()
                                                                    .to_str()
                                                                    .unwrap_or(""),
                                                            ),
                                                        )
                                                        .ok()
                                                    {
                                                        self.emit_rc_dec_for_locals(&locals_stack);
                                                        let _ =
                                                            self.builder.build_return(Some(&val));
                                                    } else {
                                                        self.emit_rc_dec_for_locals(&locals_stack);
                                                        let _ = self.builder.build_return(None);
                                                    }
                                                } else {
                                                    let _ = self.builder.build_return(None);
                                                }
                                                emitted_return = true;
                                                break;
                                            }
                                            ast::Stmt::Expr(expr_stmt) => {
                                                let _ = self.lower_expr(
                                                    &expr_stmt.expr,
                                                    function,
                                                    &param_map,
                                                    &mut locals_stack,
                                                );
                                            }
                                            ast::Stmt::Decl(ast::Decl::Var(vdecl)) => {
                                                use deno_ast::swc::ast::Pat;
                                                for decl in &vdecl.decls {
                                                    if let Pat::Ident(ident) = &decl.name {
                                                        let name = ident.id.sym.to_string();
                                                        let is_const_decl = matches!(
                                                            vdecl.kind,
                                                            deno_ast::swc::ast::VarDeclKind::Const
                                                        );
                                                        if let Some(init) = &decl.init
                                                            && let Some(val) = self.lower_expr(
                                                                init,
                                                                function,
                                                                &param_map,
                                                                &mut locals_stack,
                                                            )
                                                        {
                                                            match val {
                                                                BasicValueEnum::FloatValue(fv) => {
                                                                    let alloca = self
                                                                        .builder
                                                                        .build_alloca(
                                                                            self.context.f64_type(),
                                                                            &name,
                                                                        )
                                                                        .expect(
                                                                            "build_alloca failed",
                                                                        );
                                                                    let _ = self
                                                                        .builder
                                                                        .build_store(alloca, fv);
                                                                    self.insert_local_current_scope(&mut locals_stack, name.clone(), alloca, self.f64_t.as_basic_type_enum(), true, is_const_decl);
                                                                }
                                                                BasicValueEnum::PointerValue(
                                                                    pv,
                                                                ) => {
                                                                    let ptr_ty =
                                                                        self.context.ptr_type(
                                                                            AddressSpace::default(),
                                                                        );
                                                                    let alloca = self
                                                                        .builder
                                                                        .build_alloca(ptr_ty, &name)
                                                                        .expect(
                                                                            "build_alloca failed",
                                                                        );
                                                                    let _ = self
                                                                        .builder
                                                                        .build_store(alloca, pv);
                                                                    self.insert_local_current_scope(&mut locals_stack, name.clone(), alloca, ptr_ty.as_basic_type_enum(), true, is_const_decl);
                                                                }
                                                                BasicValueEnum::IntValue(iv) => {
                                                                    let boolt = self.bool_t;
                                                                    let alloca = self
                                                                        .builder
                                                                        .build_alloca(boolt, &name)
                                                                        .expect(
                                                                            "build_alloca failed",
                                                                        );
                                                                    let _ = self
                                                                        .builder
                                                                        .build_store(alloca, iv);
                                                                    self.insert_local_current_scope(&mut locals_stack, name.clone(), alloca, boolt.as_basic_type_enum(), true, is_const_decl);
                                                                }
                                                                _ => {}
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                            _ => {}
                                        }
                                    }
                                }
                                ast::Stmt::Return(ret) => {
                                    if let Some(arg) = &ret.arg {
                                        if let Some(val) = self
                                            .lower_expr_result(
                                                arg,
                                                function,
                                                &param_map,
                                                &mut locals_stack,
                                                Some(function.get_name().to_str().unwrap_or("")),
                                            )
                                            .ok()
                                        {
                                            self.emit_rc_dec_for_locals(&locals_stack);
                                            let _ = self.builder.build_return(Some(&val));
                                        } else {
                                            self.emit_rc_dec_for_locals(&locals_stack);
                                            let _ = self.builder.build_return(None);
                                        }
                                    } else {
                                        let _ = self.builder.build_return(None);
                                    }
                                    emitted_return = true;
                                }
                                ast::Stmt::Expr(expr_stmt) => {
                                    let _ = self.lower_expr(
                                        &expr_stmt.expr,
                                        function,
                                        &param_map,
                                        &mut locals_stack,
                                    );
                                }
                                ast::Stmt::Decl(ast::Decl::Var(vdecl)) => {
                                    use deno_ast::swc::ast::Pat;
                                    for decl in &vdecl.decls {
                                        if let Pat::Ident(ident) = &decl.name {
                                            let name = ident.id.sym.to_string();
                                            let _is_const_decl = matches!(
                                                vdecl.kind,
                                                deno_ast::swc::ast::VarDeclKind::Const
                                            );
                                            if let Some(init) = &decl.init
                                                && let Some(val) = self.lower_expr(
                                                    init,
                                                    function,
                                                    &param_map,
                                                    &mut locals_stack,
                                                )
                                            {
                                                match val {
                                                    BasicValueEnum::FloatValue(fv) => {
                                                        let alloca = self
                                                            .builder
                                                            .build_alloca(self.f64_t, &name)
                                                            .expect("build_alloca failed");
                                                        let _ =
                                                            self.builder.build_store(alloca, fv);
                                                        self.insert_local_current_scope(
                                                            &mut locals_stack,
                                                            name.clone(),
                                                            alloca,
                                                            self.f64_t.as_basic_type_enum(),
                                                            true,
                                                            false,
                                                        );
                                                    }
                                                    BasicValueEnum::PointerValue(pv) => {
                                                        let ptr_ty = self
                                                            .context
                                                            .ptr_type(AddressSpace::default());
                                                        let alloca = self
                                                            .builder
                                                            .build_alloca(ptr_ty, &name)
                                                            .expect("build_alloca failed");
                                                        let _ =
                                                            self.builder.build_store(alloca, pv);
                                                        self.insert_local_current_scope(
                                                            &mut locals_stack,
                                                            name.clone(),
                                                            alloca,
                                                            ptr_ty.as_basic_type_enum(),
                                                            true,
                                                            false,
                                                        );
                                                    }
                                                    BasicValueEnum::IntValue(iv) => {
                                                        let boolt = self.bool_t;
                                                        let alloca = self
                                                            .builder
                                                            .build_alloca(boolt, &name)
                                                            .expect("build_alloca failed");
                                                        let _ =
                                                            self.builder.build_store(alloca, iv);
                                                        self.insert_local_current_scope(
                                                            &mut locals_stack,
                                                            name.clone(),
                                                            alloca,
                                                            boolt.as_basic_type_enum(),
                                                            true,
                                                            false,
                                                        );
                                                    }
                                                    _ => {}
                                                }
                                            }
                                        }
                                    }
                                }
                                _ => {
                                    // unsupported consequent stmt type
                                }
                            }

                            if self.builder.get_insert_block().is_some() {
                                let _ = self
                                    .builder
                                    .build_unconditional_branch(merge_bb)
                                    .expect("build_unconditional_branch failed");
                            }

                            // else
                            self.builder.position_at_end(else_bb);
                            if let Some(alt) = &if_stmt.alt {
                                match &**alt {
                                    ast::Stmt::Block(block) => {
                                        for s in &block.stmts {
                                            match s {
                                                ast::Stmt::Return(ret) => {
                                                    if let Some(arg) = &ret.arg {
                                                        if let Some(val) = self.lower_expr_result(arg, function, &param_map, &mut locals_stack, Some(function.get_name().to_str().unwrap_or(""))).ok() {
                                                            let _ = self
                                                                .builder
                                                                .build_return(Some(&val));
                                                        } else {
                                                            let _ = self.builder.build_return(None);
                                                        }
                                                    } else {
                                                        let _ = self.builder.build_return(None);
                                                    }
                                                    emitted_return = true;
                                                    break;
                                                }
                                                ast::Stmt::Expr(expr_stmt) => {
                                                    let _ = self.lower_expr(
                                                        &expr_stmt.expr,
                                                        function,
                                                        &param_map,
                                                        &mut locals_stack,
                                                    );
                                                }
                                                ast::Stmt::Decl(ast::Decl::Var(vdecl)) => {
                                                    use deno_ast::swc::ast::Pat;
                                                    for decl in &vdecl.decls {
                                                        if let Pat::Ident(ident) = &decl.name {
                                                            let name = ident.id.sym.to_string();
                                                            let is_const_decl = matches!(vdecl.kind, deno_ast::swc::ast::VarDeclKind::Const);
                                                            if let Some(init) = &decl.init
                                                                && let Some(val) = self.lower_expr(
                                                                    init,
                                                                    function,
                                                                    &param_map,
                                                                    &mut locals_stack,
                                                                )
                                                            {
                                                                match val {
                                                                        BasicValueEnum::FloatValue(fv) => {
                                                                            let alloca = self.builder.build_alloca(self.f64_t, &name).expect("build_alloca failed");
                                                                            let _ = self.builder.build_store(alloca, fv);
                                                                            self.insert_local_current_scope(&mut locals_stack, name.clone(), alloca, self.f64_t.as_basic_type_enum(), true, is_const_decl);
                                                                        }
                                                                        BasicValueEnum::PointerValue(pv) => {
                                                                            let ptr_ty = self.context.ptr_type(AddressSpace::default());
                                                                            let alloca = self.builder.build_alloca(ptr_ty, &name).expect("build_alloca failed");
                                                                            let _ = self.builder.build_store(alloca, pv);
                                                                            self.insert_local_current_scope(&mut locals_stack, name.clone(), alloca, ptr_ty.as_basic_type_enum(), true, is_const_decl);
                                                                        }
                                                                        BasicValueEnum::IntValue(iv) => {
                                                                            let boolt = self.bool_t;
                                                                            let alloca = self.builder.build_alloca(boolt, &name).expect("build_alloca failed");
                                                                            let _ = self.builder.build_store(alloca, iv);
                                                                            self.insert_local_current_scope(&mut locals_stack, name.clone(), alloca, boolt.as_basic_type_enum(), true, is_const_decl);
                                                                        }
                                                                        _ => {}
                                                                    }
                                                            }
                                                        }
                                                    }
                                                }
                                                _ => {}
                                            }
                                        }
                                    }
                                    ast::Stmt::Return(ret) => {
                                        if let Some(arg) = &ret.arg {
                                            if let Some(val) = self.lower_expr_result(arg, function, &param_map, &mut locals_stack, Some(function.get_name().to_str().unwrap_or(""))).ok() {
                                                let _ = self.builder.build_return(Some(&val));
                                            } else {
                                                let _ = self.builder.build_return(None);
                                            }
                                        } else {
                                            let _ = self.builder.build_return(None);
                                        }
                                        emitted_return = true;
                                    }
                                    ast::Stmt::Expr(expr_stmt) => {
                                        let _ = self.lower_expr(
                                            &expr_stmt.expr,
                                            function,
                                            &param_map,
                                            &mut locals_stack,
                                        );
                                    }
                                    ast::Stmt::Decl(ast::Decl::Var(vdecl)) => {
                                        use deno_ast::swc::ast::Pat;
                                        for decl in &vdecl.decls {
                                            if let Pat::Ident(ident) = &decl.name {
                                                let name = ident.id.sym.to_string();
                                                let _is_const_decl = matches!(
                                                    vdecl.kind,
                                                    deno_ast::swc::ast::VarDeclKind::Const
                                                );
                                                if let Some(init) = &decl.init
                                                    && let Some(val) = self.lower_expr(
                                                        init,
                                                        function,
                                                        &param_map,
                                                        &mut locals_stack,
                                                    )
                                                {
                                                    match val {
                                                        BasicValueEnum::FloatValue(fv) => {
                                                            let alloca = self
                                                                .builder
                                                                .build_alloca(self.f64_t, &name)
                                                                .expect("build_alloca failed");
                                                            let _ = self
                                                                .builder
                                                                .build_store(alloca, fv);
                                                            self.insert_local_current_scope(
                                                                &mut locals_stack,
                                                                name.clone(),
                                                                alloca,
                                                                self.f64_t.as_basic_type_enum(),
                                                                true,
                                                                false,
                                                            );
                                                        }
                                                        BasicValueEnum::PointerValue(pv) => {
                                                            let ptr_ty = self
                                                                .context
                                                                .ptr_type(AddressSpace::default());
                                                            let alloca = self
                                                                .builder
                                                                .build_alloca(ptr_ty, &name)
                                                                .expect("build_alloca failed");
                                                            let _ = self
                                                                .builder
                                                                .build_store(alloca, pv);
                                                            self.insert_local_current_scope(
                                                                &mut locals_stack,
                                                                name.clone(),
                                                                alloca,
                                                                ptr_ty.as_basic_type_enum(),
                                                                true,
                                                                false,
                                                            );
                                                        }
                                                        BasicValueEnum::IntValue(iv) => {
                                                            let boolt = self.bool_t;
                                                            let alloca = self
                                                                .builder
                                                                .build_alloca(boolt, &name)
                                                                .expect("build_alloca failed");
                                                            let _ = self
                                                                .builder
                                                                .build_store(alloca, iv);
                                                            self.insert_local_current_scope(
                                                                &mut locals_stack,
                                                                name.clone(),
                                                                alloca,
                                                                boolt.as_basic_type_enum(),
                                                                true,
                                                                false,
                                                            );
                                                        }
                                                        _ => {}
                                                    }
                                                }
                                            }
                                        }
                                    }
                                    _ => {
                                        // unsupported alt stmt type
                                    }
                                }
                            }

                            if self.builder.get_insert_block().is_some() {
                                let _ = self
                                    .builder
                                    .build_unconditional_branch(merge_bb)
                                    .expect("build_unconditional_branch failed");
                            }

                            // continue at merge
                            self.builder.position_at_end(merge_bb);
                        }
                    }
                    ast::Stmt::For(forstmt) => {
                        // Build blocks for for-loop: cond -> body -> update -> end
                        let cond_bb = self.context.append_basic_block(function, "for.cond");
                        let body_bb = self.context.append_basic_block(function, "for.body");
                        let update_bb = self.context.append_basic_block(function, "for.update");
                        let end_bb = self.context.append_basic_block(function, "for.end");

                        // init
                        if let Some(init) = &forstmt.init {
                            match init {
                                ast::VarDeclOrExpr::VarDecl(vdecl) => {
                                    for decl in &vdecl.decls {
                                        use deno_ast::swc::ast::Pat;
                                        if let Pat::Ident(ident) = &decl.name {
                                            let name = ident.id.sym.to_string();
                                            let is_const_decl = matches!(
                                                vdecl.kind,
                                                deno_ast::swc::ast::VarDeclKind::Const
                                            );
                                            if let Some(init_expr) = &decl.init {
                                                if let Some(val) = self.lower_expr(
                                                    init_expr,
                                                    function,
                                                    &param_map,
                                                    &mut locals_stack,
                                                ) {
                                                    match val {
                                                        BasicValueEnum::FloatValue(fv) => {
                                                            let alloca = self
                                                                .builder
                                                                .build_alloca(self.f64_t, &name)
                                                                .expect("build_alloca failed");
                                                            self.insert_local_current_scope(
                                                                &mut locals_stack,
                                                                name.clone(),
                                                                alloca,
                                                                self.f64_t.as_basic_type_enum(),
                                                                false,
                                                                is_const_decl,
                                                            );
                                                            let _ = self
                                                                .builder
                                                                .build_store(alloca, fv);
                                                            self.set_local_initialized(
                                                                &mut locals_stack,
                                                                &name,
                                                                true,
                                                            );
                                                        }
                                                        BasicValueEnum::PointerValue(pv) => {
                                                            let ptr_ty = self.i8ptr_t;
                                                            let alloca = self
                                                                .builder
                                                                .build_alloca(ptr_ty, &name)
                                                                .expect("build_alloca failed");
                                                            self.insert_local_current_scope(
                                                                &mut locals_stack,
                                                                name.clone(),
                                                                alloca,
                                                                ptr_ty.as_basic_type_enum(),
                                                                false,
                                                                is_const_decl,
                                                            );
                                                            let _ = self
                                                                .builder
                                                                .build_store(alloca, pv);
                                                            let rc_inc = self.get_rc_inc();
                                                            let _ = self
                                                                .builder
                                                                .build_call(
                                                                    rc_inc,
                                                                    &[pv.into()],
                                                                    "rc_inc_var_init",
                                                                )
                                                                .expect("build_call failed");
                                                            self.set_local_initialized(
                                                                &mut locals_stack,
                                                                &name,
                                                                true,
                                                            );
                                                        }
                                                        BasicValueEnum::IntValue(iv) => {
                                                            let boolt = self.bool_t;
                                                            let alloca = self
                                                                .builder
                                                                .build_alloca(boolt, &name)
                                                                .expect("build_alloca failed");
                                                            self.insert_local_current_scope(
                                                                &mut locals_stack,
                                                                name.clone(),
                                                                alloca,
                                                                boolt.as_basic_type_enum(),
                                                                false,
                                                                is_const_decl,
                                                            );
                                                            let _ = self
                                                                .builder
                                                                .build_store(alloca, iv);
                                                            self.set_local_initialized(
                                                                &mut locals_stack,
                                                                &name,
                                                                true,
                                                            );
                                                        }
                                                        _ => {}
                                                    }
                                                }
                                            } else {
                                                let alloca = self
                                                    .builder
                                                    .build_alloca(self.f64_t, &name)
                                                    .expect("build_alloca failed");
                                                self.insert_local_current_scope(
                                                    &mut locals_stack,
                                                    name.clone(),
                                                    alloca,
                                                    self.f64_t.as_basic_type_enum(),
                                                    false,
                                                    is_const_decl,
                                                );
                                            }
                                        }
                                    }
                                }
                                ast::VarDeclOrExpr::Expr(e) => {
                                    let _ =
                                        self.lower_expr(e, function, &param_map, &mut locals_stack);
                                }
                            }
                        }

                        if self.builder.get_insert_block().is_some() {
                            let _ = self
                                .builder
                                .build_unconditional_branch(cond_bb)
                                .expect("build_unconditional_branch failed");
                        }

                        // cond
                        self.builder.position_at_end(cond_bb);
                        if let Some(test) = &forstmt.test {
                            if let Some(tv) =
                                self.lower_expr(test, function, &param_map, &mut locals_stack)
                            {
                                if let Some(cond) = self.to_condition_i1(tv) {
                                    self.builder
                                        .build_conditional_branch(cond, body_bb, end_bb)
                                        .expect("build_conditional_branch failed");
                                } else {
                                    self.builder
                                        .build_unconditional_branch(end_bb)
                                        .expect("build_unconditional_branch failed");
                                }
                            } else {
                                self.builder
                                    .build_unconditional_branch(end_bb)
                                    .expect("build_unconditional_branch failed");
                            }
                        } else {
                            self.builder
                                .build_unconditional_branch(body_bb)
                                .expect("build_unconditional_branch failed");
                        }

                        // body
                        self.builder.position_at_end(body_bb);
                        locals_stack.push(HashMap::new());
                        let mut body_returned = false;
                        match &*forstmt.body {
                            ast::Stmt::Block(block) => {
                                for s in &block.stmts {
                                    match s {
                                        ast::Stmt::Return(ret) => {
                                            if let Some(arg) = &ret.arg {
                                                if let Some(val) = self
                                                    .lower_expr_result(
                                                        arg,
                                                        function,
                                                        &param_map,
                                                        &mut locals_stack,
                                                        Some(
                                                            function
                                                                .get_name()
                                                                .to_str()
                                                                .unwrap_or(""),
                                                        ),
                                                    )
                                                    .ok()
                                                {
                                                    self.emit_rc_dec_for_locals(&locals_stack);
                                                    let _ = self.builder.build_return(Some(&val));
                                                } else {
                                                    self.emit_rc_dec_for_locals(&locals_stack);
                                                    let _ = self.builder.build_return(None);
                                                }
                                            } else {
                                                let _ = self.builder.build_return(None);
                                            }
                                            body_returned = true;
                                            break;
                                        }
                                        ast::Stmt::Expr(expr_stmt) => {
                                            let _ = self.lower_expr(
                                                &expr_stmt.expr,
                                                function,
                                                &param_map,
                                                &mut locals_stack,
                                            );
                                        }
                                        ast::Stmt::Decl(ast::Decl::Var(vdecl)) => {
                                            use deno_ast::swc::ast::Pat;
                                            for decl in &vdecl.decls {
                                                if let Pat::Ident(ident) = &decl.name {
                                                    let name = ident.id.sym.to_string();
                                                    let _is_const_decl = matches!(
                                                        vdecl.kind,
                                                        deno_ast::swc::ast::VarDeclKind::Const
                                                    );
                                                    if let Some(init) = &decl.init
                                                        && let Some(val) = self.lower_expr(
                                                            init,
                                                            function,
                                                            &param_map,
                                                            &mut locals_stack,
                                                        )
                                                    {
                                                        match val {
                                                            BasicValueEnum::FloatValue(fv) => {
                                                                let alloca = self
                                                                    .builder
                                                                    .build_alloca(self.f64_t, &name)
                                                                    .expect("build_alloca failed");
                                                                let _ = self
                                                                    .builder
                                                                    .build_store(alloca, fv);
                                                                self.insert_local_current_scope(
                                                                    &mut locals_stack,
                                                                    name.clone(),
                                                                    alloca,
                                                                    self.f64_t.as_basic_type_enum(),
                                                                    true,
                                                                    false,
                                                                );
                                                            }
                                                            BasicValueEnum::PointerValue(pv) => {
                                                                let ptr_ty = self.context.ptr_type(
                                                                    AddressSpace::default(),
                                                                );
                                                                let alloca = self
                                                                    .builder
                                                                    .build_alloca(ptr_ty, &name)
                                                                    .expect("build_alloca failed");
                                                                let _ = self
                                                                    .builder
                                                                    .build_store(alloca, pv);
                                                                self.insert_local_current_scope(
                                                                    &mut locals_stack,
                                                                    name.clone(),
                                                                    alloca,
                                                                    ptr_ty.as_basic_type_enum(),
                                                                    true,
                                                                    false,
                                                                );
                                                            }
                                                            BasicValueEnum::IntValue(iv) => {
                                                                let boolt = self.bool_t;
                                                                let alloca = self
                                                                    .builder
                                                                    .build_alloca(boolt, &name)
                                                                    .expect("build_alloca failed");
                                                                let _ = self
                                                                    .builder
                                                                    .build_store(alloca, iv);
                                                                self.insert_local_current_scope(
                                                                    &mut locals_stack,
                                                                    name.clone(),
                                                                    alloca,
                                                                    boolt.as_basic_type_enum(),
                                                                    true,
                                                                    false,
                                                                );
                                                            }
                                                            _ => {}
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                        _ => {}
                                    }
                                }
                            }
                            ast::Stmt::Return(ret) => {
                                if let Some(arg) = &ret.arg {
                                    if let Some(val) = self.lower_expr(
                                        arg,
                                        function,
                                        &param_map,
                                        &mut locals_stack,
                                    ) {
                                        self.emit_rc_dec_for_locals(&locals_stack);
                                        let _ = self.builder.build_return(Some(&val));
                                    } else {
                                        self.emit_rc_dec_for_locals(&locals_stack);
                                        let _ = self.builder.build_return(None);
                                    }
                                } else {
                                    let _ = self.builder.build_return(None);
                                }
                                body_returned = true;
                            }
                            ast::Stmt::Expr(expr_stmt) => {
                                let _ = self.lower_expr(
                                    &expr_stmt.expr,
                                    function,
                                    &param_map,
                                    &mut locals_stack,
                                );
                            }
                            _ => {}
                        }
                        if !body_returned && self.builder.get_insert_block().is_some() {
                            let _ = self
                                .builder
                                .build_unconditional_branch(update_bb)
                                .expect("build_unconditional_branch failed");
                        }
                        locals_stack.pop();

                        // update
                        self.builder.position_at_end(update_bb);
                        if let Some(update) = &forstmt.update {
                            let _ =
                                self.lower_expr(update, function, &param_map, &mut locals_stack);
                        }
                        if self.builder.get_insert_block().is_some() {
                            let _ = self
                                .builder
                                .build_unconditional_branch(cond_bb)
                                .expect("build_unconditional_branch failed");
                        }

                        // continue after loop
                        self.builder.position_at_end(end_bb);
                    }
                    ast::Stmt::ForOf(forof) => {
                        // for (let x of y) { body }
                        // Lower the RHS (iterable) and expect an array-like pointer
                        if let Some(arr_val) =
                            self.lower_expr(&forof.right, function, &param_map, &mut locals_stack)
                            && let BasicValueEnum::PointerValue(arr_ptr) = arr_val
                        {
                            // Setup blocks: cond -> body -> inc -> end
                            let cond_bb = self.context.append_basic_block(function, "forof.cond");
                            let body_bb = self.context.append_basic_block(function, "forof.body");
                            let inc_bb = self.context.append_basic_block(function, "forof.inc");
                            let end_bb = self.context.append_basic_block(function, "forof.end");

                            // create index alloca (i64) in entry of surrounding function
                            let idx_alloca = self
                                .builder
                                .build_alloca(self.i64_t, "forof_idx")
                                .expect("build_alloca failed");
                            let zero = self.i64_t.const_int(0, false);
                            let _ = self.builder.build_store(idx_alloca, zero);

                            // Create a local for the loop variable in the current scope (uninitialized)
                            let loop_var_name = match &forof.left {
                                ast::ForHead::VarDecl(boxed_vd) => {
                                    let vd = &**boxed_vd;
                                    // expect single decl with Ident pattern
                                    if let Some(decl) = vd.decls.first() {
                                        if let ast::Pat::Ident(ident) = &decl.name {
                                            Some(ident.id.sym.to_string())
                                        } else {
                                            None
                                        }
                                    } else {
                                        None
                                    }
                                }
                                ast::ForHead::Pat(boxed_p) => {
                                    if let ast::Pat::Ident(ident) = &**boxed_p {
                                        Some(ident.id.sym.to_string())
                                    } else {
                                        None
                                    }
                                }
                                _ => None,
                            };

                            // allocate placeholder alloca for the loop var (use ptr type to be generic)
                            let mut loop_var_alloca = None;
                            if let Some(name) = &loop_var_name {
                                let ptr_ty = self.i8ptr_t;
                                let alloca = self
                                    .builder
                                    .build_alloca(ptr_ty, name)
                                    .expect("build_alloca failed");
                                // Insert as uninitialized local
                                self.insert_local_current_scope(
                                    &mut locals_stack,
                                    name.clone(),
                                    alloca,
                                    ptr_ty.as_basic_type_enum(),
                                    false,
                                    false,
                                );
                                loop_var_alloca = Some((name.clone(), alloca));
                            }

                            // jump to cond
                            if self.builder.get_insert_block().is_some() {
                                let _ = self
                                    .builder
                                    .build_unconditional_branch(cond_bb)
                                    .expect("build_unconditional_branch failed");
                            }

                            // cond: load length from arr (second u64 at offset sizeof(u64)) and compare idx < len
                            self.builder.position_at_end(cond_bb);
                            // compute len: load u64 at arr_ptr + 8
                            // arr_ptr is i8* ; GEP by 8 to length location
                            let off_const = self
                                .i32_t
                                .const_int(std::mem::size_of::<u64>() as u64, false);
                            let len_i8_res = unsafe {
                                self.builder.build_gep(
                                    self.context.i8_type(),
                                    arr_ptr,
                                    &[off_const],
                                    "len_i8",
                                )
                            };
                            if let Ok(len_i8) = len_i8_res {
                                let len_ptr = self
                                    .builder
                                    .build_pointer_cast(
                                        len_i8,
                                        self.context.ptr_type(AddressSpace::default()),
                                        "len_ptr",
                                    )
                                    .expect("cast len_ptr failed");
                                let len_loaded = self
                                    .builder
                                    .build_load(self.i64_t, len_ptr, "len_load")
                                    .expect("load len failed");
                                let len_i64 = len_loaded.into_int_value();
                                let idx_loaded = self
                                    .builder
                                    .build_load(self.i64_t, idx_alloca, "idx_load")
                                    .expect("load idx failed")
                                    .into_int_value();
                                let cond = self
                                    .builder
                                    .build_int_compare(
                                        inkwell::IntPredicate::ULT,
                                        idx_loaded,
                                        len_i64,
                                        "forof_cond",
                                    )
                                    .expect("build_int_compare failed");
                                self.builder
                                    .build_conditional_branch(cond, body_bb, end_bb)
                                    .expect("build_conditional_branch failed");
                            } else {
                                // failed to compute length -> skip loop
                                self.builder
                                    .build_unconditional_branch(end_bb)
                                    .expect("build_unconditional_branch failed");
                            }

                            // body
                            self.builder.position_at_end(body_bb);
                            locals_stack.push(HashMap::new());

                            // load header to determine element-kind flag (elem_is_number stored in high 32 bits)
                            // header is at arr_ptr as u64
                            let header_ptr = match self.builder.build_pointer_cast(
                                arr_ptr,
                                self.context.ptr_type(AddressSpace::default()),
                                "header_ptr",
                            ) {
                                Ok(p) => p,
                                Err(_) => {
                                    // failed to inspect header -> skip loop body
                                    self.builder
                                        .build_unconditional_branch(end_bb)
                                        .expect("build_unconditional_branch failed");
                                    continue;
                                }
                            };
                            let header_loaded =
                                match self
                                    .builder
                                    .build_load(self.i64_t, header_ptr, "header_load")
                                {
                                    Ok(v) => v.into_int_value(),
                                    Err(_) => {
                                        self.builder
                                            .build_unconditional_branch(end_bb)
                                            .expect("build_unconditional_branch failed");
                                        continue;
                                    }
                                };
                            // shift right by 32
                            let shift_amt = self.i64_t.const_int(32, false);
                            let header_shr = self
                                .builder
                                .build_right_shift(header_loaded, shift_amt, false, "header_shr")
                                .expect("build_right_shift failed");
                            let one = self.i64_t.const_int(1, false);
                            let is_number = self
                                .builder
                                .build_int_compare(
                                    inkwell::IntPredicate::EQ,
                                    header_shr,
                                    one,
                                    "is_number",
                                )
                                .expect("build_int_compare failed");

                            // create blocks for number vs pointer element handling
                            let num_bb =
                                self.context.append_basic_block(function, "forof.elem_num");
                            let ptr_bb =
                                self.context.append_basic_block(function, "forof.elem_ptr");
                            let after_elem_bb = self
                                .context
                                .append_basic_block(function, "forof.after_elem");
                            self.builder
                                .build_conditional_branch(is_number, num_bb, ptr_bb)
                                .expect("build_conditional_branch failed");

                            // number path: call array_get_f64(arr, idx)
                            self.builder.position_at_end(num_bb);
                            let array_get_f64 = self.get_array_get_f64();
                            let idx_loaded = self
                                .builder
                                .build_load(self.i64_t, idx_alloca, "idx_load2")
                                .expect("load idx failed")
                                .into_int_value();
                            let cs = match self.builder.build_call(
                                array_get_f64,
                                &[arr_ptr.into(), idx_loaded.into()],
                                "array_get_f64_call",
                            ) {
                                Ok(cs) => cs,
                                Err(_) => {
                                    self.builder
                                        .build_unconditional_branch(after_elem_bb)
                                        .expect("build_unconditional_branch failed");
                                    continue;
                                }
                            };
                            if let inkwell::Either::Left(bv) = cs.try_as_basic_value() {
                                // store numeric value into loop var: bitcast alloca to f64* and store
                                if let Some((name, alloca)) = &loop_var_alloca {
                                    let elem_f64 = bv.into_float_value();
                                    let elem_ptr = match self.builder.build_pointer_cast(
                                        *alloca,
                                        self.context.ptr_type(AddressSpace::default()),
                                        "elem_f64_ptr",
                                    ) {
                                        Ok(p) => p,
                                        Err(_) => {
                                            self.builder
                                                .build_unconditional_branch(after_elem_bb)
                                                .expect("build_unconditional_branch failed");
                                            continue;
                                        }
                                    };
                                    let _ = self.builder.build_store(elem_ptr, elem_f64);
                                    // mark initialized
                                    self.set_local_initialized(&mut locals_stack, name, true);
                                }
                            }
                            if self.builder.get_insert_block().is_some() {
                                let _ = self
                                    .builder
                                    .build_unconditional_branch(after_elem_bb)
                                    .expect("build_unconditional_branch failed");
                            }

                            // pointer path: call array_get_ptr(arr, idx)
                            self.builder.position_at_end(ptr_bb);
                            let array_get_ptr = self.get_array_get_ptr();
                            let idx_loaded2 = self
                                .builder
                                .build_load(self.i64_t, idx_alloca, "idx_load3")
                                .expect("load idx failed")
                                .into_int_value();
                            let cs2 = match self.builder.build_call(
                                array_get_ptr,
                                &[arr_ptr.into(), idx_loaded2.into()],
                                "array_get_ptr_call",
                            ) {
                                Ok(cs) => cs,
                                Err(_) => {
                                    self.builder
                                        .build_unconditional_branch(after_elem_bb)
                                        .expect("build_unconditional_branch failed");
                                    continue;
                                }
                            };
                            if let inkwell::Either::Left(bv2) = cs2.try_as_basic_value()
                                && let Some((name, alloca)) = &loop_var_alloca
                            {
                                // If already initialized, decref old
                                if let Some((_ptr, _ty, init, _is_const)) =
                                    self.find_local(&locals_stack, name)
                                    && init
                                {
                                    // load old pointer value and call rc_dec
                                    if let Ok(old_loaded) = self.builder.build_load(
                                        self.i8ptr_t.as_basic_type_enum(),
                                        *alloca,
                                        "old_load",
                                    ) && let BasicValueEnum::PointerValue(oldpv) = old_loaded
                                    {
                                        let rc_dec = self.get_rc_dec();
                                        let _ = self
                                            .builder
                                            .build_call(rc_dec, &[oldpv.into()], "rc_dec_old")
                                            .expect("build_call failed");
                                    }
                                }
                                let pv = bv2.into_pointer_value();
                                let _ = self.builder.build_store(*alloca, pv.as_basic_value_enum());
                                // increment refcount for new value
                                let rc_inc = self.get_rc_inc();
                                let _ = self
                                    .builder
                                    .build_call(rc_inc, &[pv.into()], "rc_inc_forof")
                                    .expect("build_call failed");
                                self.set_local_initialized(&mut locals_stack, name, true);
                            }
                            if self.builder.get_insert_block().is_some() {
                                let _ = self
                                    .builder
                                    .build_unconditional_branch(after_elem_bb)
                                    .expect("build_unconditional_branch failed");
                            }

                            // after element retrieval: lower the loop body (user provided)
                            self.builder.position_at_end(after_elem_bb);
                            match &*forof.body {
                                ast::Stmt::Block(block) => {
                                    for s in &block.stmts {
                                        match s {
                                            ast::Stmt::Return(ret) => {
                                                if let Some(arg) = &ret.arg {
                                                    if let Some(val) = self.lower_expr(
                                                        arg,
                                                        function,
                                                        &param_map,
                                                        &mut locals_stack,
                                                    ) {
                                                        self.emit_rc_dec_for_locals(&locals_stack);
                                                        let _ =
                                                            self.builder.build_return(Some(&val));
                                                    } else {
                                                        self.emit_rc_dec_for_locals(&locals_stack);
                                                        let _ = self.builder.build_return(None);
                                                    }
                                                } else {
                                                    let _ = self.builder.build_return(None);
                                                }
                                                break;
                                            }
                                            ast::Stmt::Expr(expr_stmt) => {
                                                let _ = self.lower_expr(
                                                    &expr_stmt.expr,
                                                    function,
                                                    &param_map,
                                                    &mut locals_stack,
                                                );
                                            }
                                            ast::Stmt::Decl(ast::Decl::Var(vdecl)) => {
                                                use deno_ast::swc::ast::Pat;
                                                for decl in &vdecl.decls {
                                                    if let Pat::Ident(ident) = &decl.name {
                                                        let name = ident.id.sym.to_string();
                                                        let is_const_decl = matches!(
                                                            vdecl.kind,
                                                            deno_ast::swc::ast::VarDeclKind::Const
                                                        );
                                                        if let Some(init) = &decl.init
                                                            && let Some(val) = self.lower_expr(
                                                                init,
                                                                function,
                                                                &param_map,
                                                                &mut locals_stack,
                                                            )
                                                        {
                                                            match val {
                                                                BasicValueEnum::FloatValue(fv) => {
                                                                    let alloca = self
                                                                        .builder
                                                                        .build_alloca(
                                                                            self.f64_t, &name,
                                                                        )
                                                                        .expect(
                                                                            "build_alloca failed",
                                                                        );
                                                                    let _ = self
                                                                        .builder
                                                                        .build_store(alloca, fv);
                                                                    self.insert_local_current_scope(&mut locals_stack, name.clone(), alloca, self.f64_t.as_basic_type_enum(), true, is_const_decl);
                                                                }
                                                                BasicValueEnum::PointerValue(
                                                                    pv,
                                                                ) => {
                                                                    let ptr_ty =
                                                                        self.context.ptr_type(
                                                                            AddressSpace::default(),
                                                                        );
                                                                    let alloca = self
                                                                        .builder
                                                                        .build_alloca(ptr_ty, &name)
                                                                        .expect(
                                                                            "build_alloca failed",
                                                                        );
                                                                    let _ = self
                                                                        .builder
                                                                        .build_store(alloca, pv);
                                                                    self.insert_local_current_scope(&mut locals_stack, name.clone(), alloca, ptr_ty.as_basic_type_enum(), true, is_const_decl);
                                                                }
                                                                BasicValueEnum::IntValue(iv) => {
                                                                    let boolt = self.bool_t;
                                                                    let alloca = self
                                                                        .builder
                                                                        .build_alloca(boolt, &name)
                                                                        .expect(
                                                                            "build_alloca failed",
                                                                        );
                                                                    let _ = self
                                                                        .builder
                                                                        .build_store(alloca, iv);
                                                                    self.insert_local_current_scope(&mut locals_stack, name.clone(), alloca, boolt.as_basic_type_enum(), true, is_const_decl);
                                                                }
                                                                _ => {}
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                            _ => {}
                                        }
                                    }
                                }
                                ast::Stmt::Expr(expr_stmt) => {
                                    let _ = self.lower_expr(
                                        &expr_stmt.expr,
                                        function,
                                        &param_map,
                                        &mut locals_stack,
                                    );
                                }
                                _ => {}
                            }

                            // after body, branch to inc
                            if self.builder.get_insert_block().is_some() {
                                let _ = self
                                    .builder
                                    .build_unconditional_branch(inc_bb)
                                    .expect("build_unconditional_branch failed");
                            }

                            // pop inner scope
                            locals_stack.pop();

                            // inc: idx = idx + 1
                            self.builder.position_at_end(inc_bb);
                            let idx_now = self
                                .builder
                                .build_load(self.i64_t, idx_alloca, "idx_now")
                                .expect("load idx failed")
                                .into_int_value();
                            let one64 = self.i64_t.const_int(1, false);
                            let added = self
                                .builder
                                .build_int_add(idx_now, one64, "idx_inc")
                                .expect("build_int_add failed");
                            let _ = self
                                .builder
                                .build_store(idx_alloca, added.as_basic_value_enum());
                            if self.builder.get_insert_block().is_some() {
                                let _ = self
                                    .builder
                                    .build_unconditional_branch(cond_bb)
                                    .expect("build_unconditional_branch failed");
                            }

                            // end: nothing to do
                            self.builder.position_at_end(end_bb);
                        }
                    }
                    ast::Stmt::While(ws) => {
                        let cond_bb = self.context.append_basic_block(function, "while.cond");
                        let body_bb = self.context.append_basic_block(function, "while.body");
                        let end_bb = self.context.append_basic_block(function, "while.end");

                        if self.builder.get_insert_block().is_some() {
                            let _ = self
                                .builder
                                .build_unconditional_branch(cond_bb)
                                .expect("build_unconditional_branch failed");
                        }

                        self.builder.position_at_end(cond_bb);
                        if let Some(tv) =
                            self.lower_expr(&ws.test, function, &param_map, &mut locals_stack)
                        {
                            if let Some(cond) = self.to_condition_i1(tv) {
                                self.builder
                                    .build_conditional_branch(cond, body_bb, end_bb)
                                    .expect("build_conditional_branch failed");
                            } else {
                                self.builder
                                    .build_unconditional_branch(end_bb)
                                    .expect("build_unconditional_branch failed");
                            }
                        } else {
                            self.builder
                                .build_unconditional_branch(end_bb)
                                .expect("build_unconditional_branch failed");
                        }

                        self.builder.position_at_end(body_bb);
                        locals_stack.push(HashMap::new());
                        let mut returned = false;
                        match &*ws.body {
                            ast::Stmt::Block(block) => {
                                for s in &block.stmts {
                                    match s {
                                        ast::Stmt::Return(ret) => {
                                            if let Some(arg) = &ret.arg {
                                                if let Some(val) = self.lower_expr(
                                                    arg,
                                                    function,
                                                    &param_map,
                                                    &mut locals_stack,
                                                ) {
                                                    self.emit_rc_dec_for_locals(&locals_stack);
                                                    let _ = self.builder.build_return(Some(&val));
                                                } else {
                                                    self.emit_rc_dec_for_locals(&locals_stack);
                                                    let _ = self.builder.build_return(None);
                                                }
                                            } else {
                                                let _ = self.builder.build_return(None);
                                            }
                                            returned = true;
                                            break;
                                        }
                                        ast::Stmt::Expr(expr_stmt) => {
                                            let _ = self.lower_expr(
                                                &expr_stmt.expr,
                                                function,
                                                &param_map,
                                                &mut locals_stack,
                                            );
                                        }
                                        _ => {}
                                    }
                                }
                            }
                            ast::Stmt::Return(ret) => {
                                if let Some(arg) = &ret.arg {
                                    if let Some(val) = self.lower_expr(
                                        arg,
                                        function,
                                        &param_map,
                                        &mut locals_stack,
                                    ) {
                                        self.emit_rc_dec_for_locals(&locals_stack);
                                        let _ = self.builder.build_return(Some(&val));
                                    } else {
                                        self.emit_rc_dec_for_locals(&locals_stack);
                                        let _ = self.builder.build_return(None);
                                    }
                                } else {
                                    let _ = self.builder.build_return(None);
                                }
                                returned = true;
                            }
                            ast::Stmt::Expr(expr_stmt) => {
                                let _ = self.lower_expr(
                                    &expr_stmt.expr,
                                    function,
                                    &param_map,
                                    &mut locals_stack,
                                );
                            }
                            _ => {}
                        }
                        if !returned && self.builder.get_insert_block().is_some() {
                            let _ = self
                                .builder
                                .build_unconditional_branch(cond_bb)
                                .expect("build_unconditional_branch failed");
                        }
                        locals_stack.pop();
                        self.builder.position_at_end(end_bb);
                    }
                    ast::Stmt::DoWhile(dws) => {
                        let body_bb = self.context.append_basic_block(function, "dowhile.body");
                        let cond_bb = self.context.append_basic_block(function, "dowhile.cond");
                        let end_bb = self.context.append_basic_block(function, "dowhile.end");

                        if self.builder.get_insert_block().is_some() {
                            let _ = self
                                .builder
                                .build_unconditional_branch(body_bb)
                                .expect("build_unconditional_branch failed");
                        }

                        self.builder.position_at_end(body_bb);
                        locals_stack.push(HashMap::new());
                        let mut returned = false;
                        match &*dws.body {
                            ast::Stmt::Block(block) => {
                                for s in &block.stmts {
                                    match s {
                                        ast::Stmt::Return(ret) => {
                                            if let Some(arg) = &ret.arg {
                                                if let Some(val) = self.lower_expr(
                                                    arg,
                                                    function,
                                                    &param_map,
                                                    &mut locals_stack,
                                                ) {
                                                    self.emit_rc_dec_for_locals(&locals_stack);
                                                    let _ = self.builder.build_return(Some(&val));
                                                } else {
                                                    self.emit_rc_dec_for_locals(&locals_stack);
                                                    let _ = self.builder.build_return(None);
                                                }
                                            } else {
                                                let _ = self.builder.build_return(None);
                                            }
                                            returned = true;
                                            break;
                                        }
                                        ast::Stmt::Expr(expr_stmt) => {
                                            let _ = self.lower_expr(
                                                &expr_stmt.expr,
                                                function,
                                                &param_map,
                                                &mut locals_stack,
                                            );
                                        }
                                        _ => {}
                                    }
                                }
                            }
                            ast::Stmt::Return(ret) => {
                                if let Some(arg) = &ret.arg {
                                    if let Some(val) = self.lower_expr(
                                        arg,
                                        function,
                                        &param_map,
                                        &mut locals_stack,
                                    ) {
                                        self.emit_rc_dec_for_locals(&locals_stack);
                                        let _ = self.builder.build_return(Some(&val));
                                    } else {
                                        self.emit_rc_dec_for_locals(&locals_stack);
                                        let _ = self.builder.build_return(None);
                                    }
                                } else {
                                    let _ = self.builder.build_return(None);
                                }
                                returned = true;
                            }
                            ast::Stmt::Expr(expr_stmt) => {
                                let _ = self.lower_expr(
                                    &expr_stmt.expr,
                                    function,
                                    &param_map,
                                    &mut locals_stack,
                                );
                            }
                            _ => {}
                        }
                        locals_stack.pop();
                        if !returned && self.builder.get_insert_block().is_some() {
                            let _ = self
                                .builder
                                .build_unconditional_branch(cond_bb)
                                .expect("build_unconditional_branch failed");
                        }

                        self.builder.position_at_end(cond_bb);
                        if let Some(tv) =
                            self.lower_expr(&dws.test, function, &param_map, &mut locals_stack)
                        {
                            if let Some(cond) = self.to_condition_i1(tv) {
                                self.builder
                                    .build_conditional_branch(cond, body_bb, end_bb)
                                    .expect("build_conditional_branch failed");
                            } else {
                                self.builder
                                    .build_unconditional_branch(end_bb)
                                    .expect("build_unconditional_branch failed");
                            }
                        } else {
                            self.builder
                                .build_unconditional_branch(end_bb)
                                .expect("build_unconditional_branch failed");
                        }

                        self.builder.position_at_end(end_bb);
                    }
                    _ => {
                        // other statement types not supported in prototype
                    }
                }
            }
            if !emitted_return {
                let _ = self.builder.build_return(None);
            }
        } else {
            let _ = self.builder.build_return(None);
        }

        function
    }

    /// Emit a small C-compatible `main` function in the module.
    ///
    /// The emitted host `main` and `oats_entry` do NOT call into user code.
    /// They simply return 0 when emitted. This ensures building a host
    /// executable will not execute the user's `oats_main` as a side effect.
    ///
    /// Returns true if a host `main` was emitted (the module defines
    /// `oats_main`), false otherwise.
    pub fn emit_host_main(
        &self,
        _param_types: &[crate::types::OatsType],
        _ret_type: &crate::types::OatsType,
    ) -> bool {
        // If main already present, nothing to do
        if self.module.get_function("main").is_some() {
            return true;
        }

        // only emit a host main if the module defines `oats_main`; we don't
        // call into it here, but absence indicates there's nothing to host.
        if self.module.get_function("oats_main").is_none() {
            return false;
        }

        // Build main: int main() which returns an exit code
        let i32t = self.i32_t;
        let main_ty = i32t.fn_type(&[], false);
        let main_fn = self.module.add_function("main", main_ty, None);
        let entry = self.context.append_basic_block(main_fn, "entry");
        self.builder.position_at_end(entry);

        // The host `main` will invoke the user's `oats_main` when the
        // emitted `oats_main` exists and has a zero-argument signature. This
        // keeps the produced binary self-contained (no external rt_main
        // object required) while ensuring we only call the user function
        // when its signature is compatible with the simple invocation.
        if let Some(user_main) = self.module.get_function("oats_main")
            && user_main.count_params() == 0
        {
            // Call oats_main and ignore its return value.
            let _ = self
                .builder
                .build_call(user_main, &[], "call_oats_main_from_main");
        }

        let zero = i32t.const_int(0, false);
        let _ = self.builder.build_return(Some(&zero.as_basic_value_enum()));

        // Emit oats_entry that mirrors main but returns i32 for hosts that
        // call that symbol from a separate object file.
        if self.module.get_function("oats_entry").is_none() {
            let entry_ty = i32t.fn_type(&[], false);
            let entry_fn = self.module.add_function("oats_entry", entry_ty, None);
            let entry_bb = self.context.append_basic_block(entry_fn, "entry");
            self.builder.position_at_end(entry_bb);

            // Emit a simple oats_entry that calls into the user's `oats_main`
            // if it exists. This makes a linked host executable execute the
            // user's program when run. We call `oats_main` with no
            // arguments here and ignore its return value; this is the
            // pragmatic behavior for the current prototype (examples use
            // `oats_main()` with no args). If `oats_main` has a different
            // signature the call will be skipped and oats_entry will return
            // 0 instead.
            if let Some(user_main) = self.module.get_function("oats_main") {
                // Only call if the function type matches a no-arg callable
                // (defensive: check param count is zero).
                if user_main.count_params() == 0 {
                    let _ = self.builder.build_call(user_main, &[], "call_oats_main");
                }
            }

            let zero = i32t.const_int(0, false);
            let _ = self.builder.build_return(Some(&zero.as_basic_value_enum()));
        }

        true
    }
}
