//! Expression lowering helpers
//!
//! This module contains the primary expression lowering routine (`lower_expr`)
//! which traverses the `deno_ast` expression AST and emits corresponding
//! LLVM IR values via the shared `CodeGen` helpers. Expressions are the
//! most complex part of lowering because they must handle type coercions,
//! union boxing/unboxing, short-circuiting control flow (logical &&/||),
//! and runtime helper interactions (array access, string concat, etc.).
//!
//! The implementation intentionally preserves explicit conversions and
//! runtime calls rather than attempting aggressive optimizations. Inline
//! comments explain important choices (for example, when boxing is used
//! for unions and when `rc_inc` / `rc_dec` semantics apply).
//!
//! Design notes and conventions used throughout this module:
//! - ABI/Return values: lowering returns a `BasicValueEnum` that represents
//!   the ABI value for an expression: numeric results are `f64` (float),
//!   pointer-like values are `i8*`. For callers that expect a value from a
//!   void-returning IR call we return a harmless `f64` zero so the surrounding
//!   lowering can remain uniform.
//! - Unions: when a union has any pointer-like arm we choose a pointer
//!   representation for the ABI (i8*). Numeric arms are boxed via
//!   `union_box_f64`/`union_box_ptr` when stored in heap fields or captured.
//! - Last-expression origin: `last_expr_origin_local` is used as a small
//!   heuristic to record that the last-lowered expression came from a named
//!   temporary local (for example a freshly constructed closure). This lets
//!   subsequent lowering paths conservatively recover more static information
//!   (e.g., closure return types) without heavy analysis.

use crate::diagnostics::Diagnostic;
use inkwell::values::BasicValueEnum;
use inkwell::values::FunctionValue;
use std::collections::HashMap;

use inkwell::AddressSpace;
use inkwell::builder::Builder;
use inkwell::types::BasicType;
use inkwell::types::BasicTypeEnum;
use inkwell::values::{BasicValue, PointerValue};
// LocalEntry now includes an Option<String> for an optional nominal type name
type LocalEntry<'a> = (
    PointerValue<'a>,
    BasicTypeEnum<'a>,
    bool,
    bool,
    bool,
    Option<String>,
);
type LocalsStackLocal<'a> = Vec<HashMap<String, LocalEntry<'a>>>;

impl<'a> crate::codegen::CodeGen<'a> {
    /// Main expression lowering function.
    ///
    /// Traverses an AST expression and emits LLVM IR values for the
    /// expression using the `CodeGen`'s `builder` and runtime helper
    /// functions. The returned `BasicValueEnum` represents the ABI value
    /// for the expression (for example `f64` for numbers or `i8*` for
    /// pointer-like values). On error a `Diagnostic` is returned and the
    /// caller decides whether to continue lowering other parts of the
    /// module.
    ///
    /// Contract/Notes:
    /// - The lowering keeps boxing/unboxing explicit: unions with pointer-
    ///   like arms use `i8*` slots and numeric arms may be boxed into
    ///   runtime objects using `union_box_f64`.
    /// - Short-circuiting logical operations create basic blocks and
    ///   phi nodes to merge results when needed.
    /// - `lower_expr` may emit calls to runtime helpers (e.g. `strlen`,
    ///   `array_get_f64`, `str_concat`). Those functions are declared via
    ///   `helpers::declare_libc` or created lazily by `CodeGen` getters.
    ///
    /// # Arguments
    /// * `expr` - AST expression to lower.
    /// * `function` - current LLVM function for block creation.
    /// * `param_map` - map of function parameter names to indices.
    /// * `locals` - mutable lexical locals stack used for allocas and RC.
    ///
    /// # Returns
    /// A lowered `BasicValueEnum` on success, or a `Diagnostic` on failure.
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
                //
                // This pattern is common in guard code. When the left side is
                // a `typeof` unary we can lower the inner expression and, for
                // pointer-like (boxed union) values, consult the runtime
                // discriminant instead of performing a heavier dynamic check.
                // This keeps the emitted IR small for the common guarded
                // branches used in tests and examples.
                if let ast::Expr::Unary(unary) = &*bin.left
                    && let deno_ast::swc::ast::UnaryOp::TypeOf = unary.op
                    && let ast::Expr::Lit(deno_ast::swc::ast::Lit::Str(s)) = &*bin.right
                {
                    // Lower inner expression and then check union discriminant if it's a pointer
                    let inner = self.lower_expr(&unary.arg, function, param_map, locals)?;
                    if let BasicValueEnum::PointerValue(pv) = inner {
                        // For pointer-like values, consult the union discriminant helper.
                        // Runtime layout: union-boxed objects expose a small tag (discriminant)
                        // stored in the allocated container. The helper reads that tag and
                        // returns an i64 so the compiler can cheaply implement `typeof`
                        // without re-running generic dynamic checks. This fast-path is
                        // intentionally optimistic: if the inner value is unboxed or a
                        // different representation, we fall back to the general path
                        // below.
                        let get_disc = self.get_union_get_discriminant();
                        let cs = self.builder.build_call(get_disc, &[pv.into()], "get_disc");
                        if let Ok(cs) = cs
                            && let inkwell::Either::Left(bv) = cs.try_as_basic_value()
                        {
                            let disc = bv.into_int_value();
                            let expected = match s.value.as_ref() {
                                "number" => self.i64_t.const_int(0, false),
                                "string" => self.i64_t.const_int(1, false),
                                _ => self.i64_t.const_int(2, false),
                            };
                            if let Ok(cmp_iv) = self.builder.build_int_compare(
                                inkwell::IntPredicate::EQ,
                                disc,
                                expected,
                                "typeof_eq",
                            ) {
                                return Ok(cmp_iv.as_basic_value_enum());
                            }
                        }
                    }
                }

                let l = self.lower_expr(&bin.left, function, param_map, locals)?;
                let r = self.lower_expr(&bin.right, function, param_map, locals)?;

                // Coercion and unboxing notes:
                // - `coerce_to_f64` will convert ints/bools to f64 where
                //   appropriate and will also attempt to unbox numeric payloads
                //   from union-boxed pointers by calling the runtime `union_unbox_f64`.
                // - We attempt the float-path first because numeric arithmetic is
                //   the most common case and produces simpler IR.
                // - If coercion fails we fall back to pointer/other handling.
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
                //
                // For `&&` and `||` we emit explicit conditional branches and
                // create a merge block with a phi node. The phi node is built
                // by `build_phi_merge` which performs basic coercions when the
                // two sides have differing ABI types (for example f64 vs i64
                // or pointer). This keeps the lowering correct for expressions
                // that return one of the operand values (JS-like semantics).
                //
                // Implementation detail: to preserve JS semantics where `a && b`
                // returns either `a` or `b` we evaluate `a` first, check its
                // truthiness via `to_condition_i1`, and only evaluate `b` when
                // needed. The merge uses a phi node which may need to coerce the
                // two incoming values to a common ABI representation; `build_phi_merge`
                // performs those coercions conservatively (boxing numeric values
                // if the other arm expects a pointer-like value, etc.).
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
                    // then: evaluate right. Note: we don't eagerly `rc_inc` here;
                    // any pointer ownership changes are performed by the
                    // callee/array_get/union_box helpers when producing values.
                    self.builder.position_at_end(then_bb);
                    let rv = self.lower_expr(&bin.right, function, param_map, locals);
                    if self.builder.get_insert_block().is_some()
                        && self.builder.build_unconditional_branch(merge_bb).is_err()
                    {
                        return Err(Diagnostic::simple("expression lowering failed"))?;
                    }
                    // else: keep left (the left expression's value is the
                    // result of the `&&` expression when it's falsy). We branch
                    // to `merge_bb` to join both control-flow paths and use a
                    // phi node to select the correct value.
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
                    // `||` mirrors `&&` but keeps the left value when truthy.
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
                                    _ => Err(Diagnostic::simple(
                                        "operation not supported (bin strcat result)",
                                    )),
                                }
                            } else {
                                Err(Diagnostic::simple_with_span(
                                    "string concatenation helper `str_concat` not found",
                                    bin.span.lo.0 as usize,
                                ))
                            }
                        } else {
                            Err(Diagnostic::simple_with_span(
                                "pointer binary operation not supported",
                                bin.span.lo.0 as usize,
                            ))
                        }
                    }
                    _ => Err(Diagnostic::simple("operation not supported (bin fallback)")),
                }
            }
            // Identifier lookup and TDZ handling
            //
            // Identifiers can refer to function parameters (which are
            // passed in via `param_map`) or to locals created by `let`/`const`.
            // For locals we track an `initialized` flag and trap (emit
            // unreachable) for Temporal Dead Zone reads. We also record the
            // origin local name for the last-lowered expression which helps
            // propagate closure-local typing information.
            ast::Expr::Ident(id) => {
                let name = id.sym.to_string();

                // First, check if the identifier is a function parameter.
                if let Some(idx) = param_map.get(&name)
                    && let Some(pv) = function.get_nth_param(*idx)
                {
                    return Ok(pv);
                }

                // If not a parameter, then it must be a local variable (`let` or `const`).
                if let Some((ptr, ty, initialized, _is_const, _extra, _nominal)) =
                    self.find_local(locals, &name)
                {
                    // If not initialized -> TDZ: generate a trap (unreachable).
                    //
                    // Emitting `unreachable` here is a simple way to guard
                    // against invalid reads in generated code. In future we
                    // could lower a runtime diagnostic or insert a proper
                    // throw.
                    if !initialized {
                        // Emit a call to unreachable to trap at runtime
                        let _ = self.builder.build_unreachable();
                        return Err(Diagnostic::simple("expression lowering failed"))?;
                    }
                    let loaded = match self.builder.build_load(ty, ptr, &name) {
                        Ok(v) => v,
                        Err(_) => return Err(Diagnostic::simple("operation failed")),
                    };
                    // Record that this expression originated from local `name`.
                    self.last_expr_origin_local
                        .borrow_mut()
                        .replace(name.clone());
                    return Ok(loaded);
                }
                Err(Diagnostic::simple_with_span(
                    format!("unknown identifier '{}'", name),
                    id.span.lo.0 as usize,
                ))
            }
            ast::Expr::Call(call) => {
                // Support simple identifier callees and member-callee method calls.
                //
                // We intentionally support a small set of call shapes in the
                // emitter: plain identifier calls (e.g., `foo()`), and
                // member method calls (e.g., `obj.method()`). More complex
                // call expressions (computed callees, dynamic property lookups)
                // can be added when needed.
                use deno_ast::swc::ast::Callee;
                if let Callee::Super(_) = &call.callee {
                    // Lower `super(...)` calls: call parent's `<Parent>_init(this, ...args)`
                    let parent_opt = self.current_class_parent.borrow().clone();
                    let parent = if let Some(p) = parent_opt {
                        p
                    } else {
                        return Err(Diagnostic::simple_with_span(
                            "super used outside of constructor or no parent",
                            call.span.lo.0 as usize,
                        ));
                    };

                    // Find `this` in locals
                    if let Some((this_ptr, this_ty, _init, _is_const, _extra, _nominal)) =
                        self.find_local(locals, "this")
                    {
                        let this_loaded =
                            match self.builder.build_load(this_ty, this_ptr, "this_loaded") {
                                Ok(v) => v,
                                Err(_) => return Err(Diagnostic::simple("operation failed")),
                            };

                        let mut args: Vec<inkwell::values::BasicMetadataValueEnum> = Vec::new();
                        args.push(this_loaded.into());
                        for a in &call.args {
                            if let Ok(v) = self.lower_expr(&a.expr, function, param_map, locals) {
                                args.push(v.into());
                            } else {
                                return Err(Diagnostic::simple("expression lowering failed"))?;
                            }
                        }

                        let init_name = format!("{}_init", parent);
                        if let Some(init_f) = self.module.get_function(&init_name) {
                            let cs = match self.builder.build_call(init_f, &args, "call_super_init")
                            {
                                Ok(cs) => cs,
                                Err(_) => return Err(Diagnostic::simple("operation failed")),
                            };
                            let either = cs.try_as_basic_value();
                            if let inkwell::Either::Left(bv) = either {
                                Ok(bv)
                            } else {
                                let zero = self.f64_t.const_float(0.0);
                                Ok(zero.as_basic_value_enum())
                            }
                        } else {
                            Err(Diagnostic::simple_with_span(
                                format!("super target '{}' not found", init_name),
                                call.span.lo.0 as usize,
                            ))
                        }
                    } else {
                        Err(Diagnostic::simple_with_span(
                            "super used but `this` not found",
                            call.span.lo.0 as usize,
                        ))
                    }
                } else if let ast::Callee::Expr(boxed_expr) = &call.callee {
                    match &**boxed_expr {
                        ast::Expr::Ident(ident) => {
                            let fname = ident.sym.to_string();

                            // Special-case: println(...) -> call runtime print helpers
                            // `println` is a convenience used by tests/examples and
                            // lowered to runtime helpers `print_f64` / `print_str`.
                            if fname == "println" {
                                // Print each argument sequentially. This supports
                                // `println(a, b, c)` by emitting individual
                                // print calls for each argument and returning zero.
                                // Print each argument sequentially without a newline.
                                for a in &call.args {
                                    let val = match self
                                        .lower_expr(&a.expr, function, param_map, locals)
                                    {
                                        Ok(v) => v,
                                        Err(d) => return Err(d)?,
                                    };
                                    match val {
                                        BasicValueEnum::FloatValue(fv) => {
                                            if let Some(print_fn) =
                                                self.module.get_function("print_f64_no_nl")
                                            {
                                                let _ = self
                                                    .builder
                                                    .build_call(
                                                        print_fn,
                                                        &[fv.into()],
                                                        "print_f64_no_nl_call",
                                                    )
                                                    .ok();
                                            }
                                        }
                                        BasicValueEnum::PointerValue(pv) => {
                                            // Attempt to format arrays/tuples specially.
                                            let mut used = false;
                                            if let Some(orig) =
                                                self.last_expr_origin_local.borrow().clone()
                                            {
                                                if let Some((
                                                    _ptr,
                                                    _ty,
                                                    _init,
                                                    _is_const,
                                                    _is_weak,
                                                    nominal,
                                                )) = self.find_local(locals, &orig)
                                                    && let Some(n) = nominal
                                                    && n == "__oats_array"
                                                    && let Some(array_to_string) =
                                                        self.module.get_function("array_to_string")
                                                {
                                                    let cs = self.builder.build_call(
                                                        array_to_string,
                                                        &[pv.into()],
                                                        "array_to_string_call",
                                                    );
                                                    if let Ok(cs) = cs
                                                        && let inkwell::Either::Left(bv) =
                                                            cs.try_as_basic_value()
                                                    {
                                                        let str_ptr = bv.into_pointer_value();
                                                        if let Some(print_fn) = self
                                                            .module
                                                            .get_function("print_str_no_nl")
                                                        {
                                                            let _ = self
                                                                .builder
                                                                .build_call(
                                                                    print_fn,
                                                                    &[str_ptr.into()],
                                                                    "print_str_no_nl_call",
                                                                )
                                                                .ok();
                                                        }
                                                        if let Some(rc_dec_str) =
                                                            self.module.get_function("rc_dec_str")
                                                        {
                                                            let _ = self
                                                                .builder
                                                                .build_call(
                                                                    rc_dec_str,
                                                                    &[str_ptr.into()],
                                                                    "rc_dec_str_call",
                                                                )
                                                                .ok();
                                                        }
                                                        used = true;
                                                    }
                                                }
                                                if !used
                                                    && self
                                                        .class_fields
                                                        .borrow()
                                                        .get(&orig)
                                                        .is_some()
                                                    && let Some(array_to_string) =
                                                        self.module.get_function("array_to_string")
                                                {
                                                    let cs = self.builder.build_call(
                                                        array_to_string,
                                                        &[pv.into()],
                                                        "array_to_string_call",
                                                    );
                                                    if let Ok(cs) = cs
                                                        && let inkwell::Either::Left(bv) =
                                                            cs.try_as_basic_value()
                                                    {
                                                        let str_ptr = bv.into_pointer_value();
                                                        if let Some(print_fn) = self
                                                            .module
                                                            .get_function("print_str_no_nl")
                                                        {
                                                            let _ = self
                                                                .builder
                                                                .build_call(
                                                                    print_fn,
                                                                    &[str_ptr.into()],
                                                                    "print_str_no_nl_call",
                                                                )
                                                                .ok();
                                                        }
                                                        if let Some(rc_dec_str) =
                                                            self.module.get_function("rc_dec_str")
                                                        {
                                                            let _ = self
                                                                .builder
                                                                .build_call(
                                                                    rc_dec_str,
                                                                    &[str_ptr.into()],
                                                                    "rc_dec_str_call",
                                                                )
                                                                .ok();
                                                        }
                                                        used = true;
                                                    }
                                                }
                                                // If still not used, check whether the arg is a function parameter
                                                if !used {
                                                    // If the argument was an identifier, see if it maps to a parameter index
                                                    if let deno_ast::swc::ast::Expr::Ident(ident) =
                                                        &*a.expr
                                                    {
                                                        let arg_name = ident.sym.to_string();
                                                        if let Some(idx) = param_map.get(&arg_name)
                                                        {
                                                            // Look up param types for the current function
                                                            let fname = function
                                                                .get_name()
                                                                .to_str()
                                                                .unwrap_or("<fn>");
                                                            if let Some(param_types) = self
                                                                .fn_param_types
                                                                .borrow()
                                                                .get(fname)
                                                                && let Some(pt) =
                                                                    param_types.get(*idx as usize)
                                                                && matches!(
                                                                    pt,
                                                                    crate::types::OatsType::Array(
                                                                        _
                                                                    )
                                                                )
                                                                && let Some(array_to_string) = self
                                                                    .module
                                                                    .get_function("array_to_string")
                                                            {
                                                                let cs = self.builder.build_call(
                                                                    array_to_string,
                                                                    &[pv.into()],
                                                                    "array_to_string_call",
                                                                );
                                                                if let Ok(cs) = cs
                                                                    && let inkwell::Either::Left(bv) =
                                                                        cs.try_as_basic_value()
                                                                {
                                                                    let str_ptr =
                                                                        bv.into_pointer_value();
                                                                    if let Some(print_fn) =
                                                                        self.module.get_function(
                                                                            "print_str_no_nl",
                                                                        )
                                                                    {
                                                                        let _ = self.builder.build_call(print_fn, &[str_ptr.into()], "print_str_no_nl_call").ok();
                                                                    }
                                                                    if let Some(rc_dec_str) = self
                                                                        .module
                                                                        .get_function("rc_dec_str")
                                                                    {
                                                                        let _ = self
                                                                            .builder
                                                                            .build_call(
                                                                                rc_dec_str,
                                                                                &[str_ptr.into()],
                                                                                "rc_dec_str_call",
                                                                            )
                                                                            .ok();
                                                                    }
                                                                    used = true;
                                                                }
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                            if !used
                                                && let Some(print_fn) =
                                                    self.module.get_function("print_str_no_nl")
                                            {
                                                let _ = self
                                                    .builder
                                                    .build_call(
                                                        print_fn,
                                                        &[pv.into()],
                                                        "print_str_no_nl_call",
                                                    )
                                                    .ok();
                                            }
                                        }
                                        _ => {
                                            return Err(Diagnostic::simple_with_span(
                                                "operation failed",
                                                call.span.lo.0 as usize,
                                            ));
                                        }
                                    }
                                }
                                // After printing all args, emit a single newline
                                if let Some(nl_fn) = self.module.get_function("print_newline") {
                                    let _ = self
                                        .builder
                                        .build_call(nl_fn, &[], "print_newline_call")
                                        .ok();
                                }
                                let zero = self.f64_t.const_float(0.0);
                                return Ok(zero.as_basic_value_enum());
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
                                if let inkwell::Either::Left(bv) = either {
                                    // The call returned a value; propagate it.
                                    Ok(bv)
                                } else {
                                    // The call returned void. To make expression
                                    // contexts uniformly expect a `BasicValueEnum`
                                    // we return a harmless `f64` zero. This keeps
                                    // expression-statement lowering simple: the
                                    // caller can ignore the returned value.
                                    let zero = self.f64_t.const_float(0.0);
                                    Ok(zero.as_basic_value_enum())
                                }
                            } else {
                                Err(Diagnostic::simple(format!(
                                    "unknown or missing function '{}'",
                                    fname
                                )))
                            }
                        }
                        // (super calls handled by Callee::Super branch below)
                        ast::Expr::Member(member) => {
                            use deno_ast::swc::ast::MemberProp;
                            if let MemberProp::Ident(prop_ident) = &member.prop {
                                let method_name = prop_ident.sym.to_string();

                                // Lower `Promise.resolve(x)` by calling the runtime
                                // helper `promise_resolve(i8*) -> i8*` so resolved
                                // promises are represented by runtime objects. If
                                // the argument is a numeric value we box it with
                                // `union_box_f64` so the runtime consistently
                                // receives an `i8*` payload.
                                if method_name == "resolve"
                                    && call.args.len() == 1
                                    && let deno_ast::swc::ast::Expr::Ident(ident) = &*member.obj
                                    && ident.sym == "Promise"
                                {
                                    // Lower the single argument
                                    let arg_val = match self.lower_expr(
                                        &call.args[0].expr,
                                        function,
                                        param_map,
                                        locals,
                                    ) {
                                        Ok(v) => v,
                                        Err(_) => {
                                            return Err(Diagnostic::simple(
                                                "expression lowering failed",
                                            ))?;
                                        }
                                    };

                                    // Ensure we pass an `i8*` payload to the runtime.
                                    let payload_ptr = match arg_val {
                                        BasicValueEnum::PointerValue(pv) => {
                                            pv.as_basic_value_enum()
                                        }
                                        BasicValueEnum::FloatValue(fv) => {
                                            // box f64 -> i8*
                                            let box_fn = self.get_union_box_f64();
                                            let cs = match self.builder.build_call(
                                                box_fn,
                                                &[fv.into()],
                                                "box_f64",
                                            ) {
                                                Ok(cs) => cs,
                                                Err(_) => {
                                                    return Err(Diagnostic::simple(
                                                        "union_box_f64 call failed",
                                                    ))?;
                                                }
                                            };
                                            match cs.try_as_basic_value() {
                                                inkwell::Either::Left(bv) => bv,
                                                _ => {
                                                    return Err(Diagnostic::simple(
                                                        "union_box_f64 returned non-value",
                                                    ))?;
                                                }
                                            }
                                        }
                                        _ => {
                                            // Other ABI shapes not expected here; fall back to returning the lowered value
                                            return Ok(arg_val);
                                        }
                                    };

                                    // Call runtime promise_resolve(payload)
                                    let f = self.get_promise_resolve();
                                    let cs = match self.builder.build_call(
                                        f,
                                        &[payload_ptr.into()],
                                        "promise_resolve_call",
                                    ) {
                                        Ok(cs) => cs,
                                        Err(_) => {
                                            return Err(Diagnostic::simple(
                                                "promise_resolve call failed",
                                            ))?;
                                        }
                                    };
                                    match cs.try_as_basic_value() {
                                        inkwell::Either::Left(bv) => return Ok(bv),
                                        _ => {
                                            return Err(Diagnostic::simple(
                                                "promise_resolve returned non-value",
                                            ))?;
                                        }
                                    }
                                }

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
                                            return Err(Diagnostic::simple(
                                                "expression lowering failed",
                                            ))?;
                                        }
                                        if let BasicValueEnum::PointerValue(pv) = obj_val {
                                            let f = self.get_rc_weak_inc();
                                            let _ = self.builder.build_call(
                                                f,
                                                &[pv.into()],
                                                "rc_weak_inc_call",
                                            );
                                            return Ok(pv.as_basic_value_enum());
                                        } else {
                                            return Err(Diagnostic::simple(
                                                "downgrade on non-pointer",
                                            ))?;
                                        }
                                    }
                                    if method_name == "upgrade" {
                                        // Expect no args
                                        if !call.args.is_empty() {
                                            return Err(Diagnostic::simple(
                                                "expression lowering failed",
                                            ))?;
                                        }
                                        if let BasicValueEnum::PointerValue(pv) = obj_val {
                                            let f = self.get_rc_weak_upgrade();
                                            let cs = match self.builder.build_call(
                                                f,
                                                &[pv.into()],
                                                "rc_weak_upgrade_call",
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
                                                    "operation not supported",
                                                ));
                                            }
                                        } else {
                                            return Err(Diagnostic::simple(
                                                "upgrade on non-pointer",
                                            ))?;
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
                                    // Resolve the nominal class name for the receiver and
                                    // attempt to call the single candidate `<Class>_<method>`.
                                    // Do not scan `class_fields` globally.
                                    let mut class_name_opt: Option<String> = None;
                                    if let deno_ast::swc::ast::Expr::Ident(ident) = &*member.obj {
                                        let ident_name = ident.sym.to_string();
                                        if ident_name == "this" {
                                            let fname = function.get_name().to_str().unwrap_or("");
                                            if let Some(cls) = fname.strip_suffix("_ctor") {
                                                class_name_opt = Some(cls.to_string());
                                            } else if let Some(param_types) =
                                                self.fn_param_types.borrow().get(fname)
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
                                        } else if let Some((
                                            _,
                                            _ty,
                                            _init,
                                            _is_const,
                                            _is_weak,
                                            nominal,
                                        )) = self.find_local(locals, &ident_name)
                                            && let Some(nom) = nominal
                                        {
                                            class_name_opt = Some(nom);
                                        }
                                    } else if matches!(
                                        &*member.obj,
                                        deno_ast::swc::ast::Expr::This(_)
                                    ) {
                                        let fname = function.get_name().to_str().unwrap_or("");
                                        if let Some(cls) = fname.strip_suffix("_ctor") {
                                            class_name_opt = Some(cls.to_string());
                                        } else if let Some(param_types) =
                                            self.fn_param_types.borrow().get(fname)
                                            && !param_types.is_empty()
                                            && let crate::types::OatsType::NominalStruct(n) =
                                                &param_types[0]
                                        {
                                            class_name_opt = Some(n.clone());
                                        }
                                    }

                                    let class_name = if let Some(c) = class_name_opt.clone() {
                                        c
                                    } else {
                                        // Fallback: if we have a registered nominal whose
                                        // method symbol `<Nominal>_<method>` exists in the
                                        // module, prefer that nominal. This is a
                                        // deterministic lookup using the collected
                                        // `class_fields` map and avoids scanning all
                                        // globals or doing dynamic resolution.
                                        let mut picked: Option<String> = None;
                                        for k in self.class_fields.borrow().keys() {
                                            let cand = format!("{}_{}", k, method_name);
                                            if self.module.get_function(&cand).is_some() {
                                                picked = Some(k.clone());
                                                break;
                                            }
                                        }
                                        if let Some(p) = picked {
                                            p
                                        } else {
                                            #[cfg(debug_assertions)]
                                            {
                                                // List module functions and known nominals for debugging
                                                let mut fn_names: Vec<String> = Vec::new();
                                                for f in self.module.get_functions() {
                                                    if let Ok(name) = f.get_name().to_str() {
                                                        fn_names.push(name.to_string());
                                                    }
                                                }
                                                eprintln!(
                                                    "[debug member_call] module_functions={:?}",
                                                    fn_names
                                                );
                                                let keys: Vec<String> = self
                                                    .class_fields
                                                    .borrow()
                                                    .keys()
                                                    .cloned()
                                                    .collect();
                                                eprintln!(
                                                    "[debug member_call] known_nominals={:?}",
                                                    keys
                                                );
                                            }
                                            return Err(Diagnostic::simple(
                                                "unsupported member call: could not infer receiver nominal type",
                                            ));
                                        }
                                    };

                                    let cand = format!("{}_{}", class_name, method_name);
                                    #[cfg(debug_assertions)]
                                    eprintln!("[debug member_call] trying candidate='{}'", cand);
                                    // If the exact candidate exists, use it
                                    if let Some(method_f) = self.module.get_function(&cand) {
                                        // lower user args
                                        let mut user_args: Vec<
                                            inkwell::values::BasicMetadataValueEnum,
                                        > = Vec::new();
                                        for a in &call.args {
                                            if let Ok(v) = self
                                                .lower_expr(&a.expr, function, param_map, locals)
                                            {
                                                user_args.push(v.into());
                                            } else {
                                                return Err(Diagnostic::simple(
                                                    "expression lowering failed",
                                                ))?;
                                            }
                                        }

                                        // if the method expects an extra param (likely `this`), prepend obj_val
                                        let param_count = method_f.count_params() as usize;
                                        let mut args: Vec<inkwell::values::BasicMetadataValueEnum> =
                                            Vec::new();
                                        if param_count > user_args.len() {
                                            args.push(obj_val.into());
                                            args.extend(user_args);
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
                                                return Err(Diagnostic::simple("operation failed"));
                                            }
                                        };
                                        let either = cs.try_as_basic_value();
                                        if let inkwell::Either::Left(bv) = either {
                                            return Ok(bv);
                                        } else {
                                            // Call returned void; return a harmless f64 zero
                                            // so expression contexts can ignore the result.
                                            let zero = self.f64_t.const_float(0.0);
                                            return Ok(zero.as_basic_value_enum());
                                        }
                                    } else {
                                        #[cfg(debug_assertions)]
                                        eprintln!(
                                            "[debug member_call] method not found on nominal '{}': '{}'",
                                            class_name, cand
                                        );
                                        // Fallback: try to find the method on any known nominal
                                        let mut found: Option<(
                                            String,
                                            inkwell::values::FunctionValue,
                                        )> = None;
                                        for k in self.class_fields.borrow().keys() {
                                            let alt = format!("{}_{}", k, method_name);
                                            if let Some(fv) = self.module.get_function(&alt) {
                                                found = Some((k.clone(), fv));
                                                break;
                                            }
                                        }
                                        if let Some((found_nom, method_f)) = found {
                                            #[cfg(debug_assertions)]
                                            eprintln!(
                                                "[debug member_call] falling back to nominal '{}' for method '{}'",
                                                found_nom, method_name
                                            );
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
                                                args.extend(user_args);
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
                                                let zero = self.f64_t.const_float(0.0);
                                                return Ok(zero.as_basic_value_enum());
                                            }
                                        }
                                    }
                                }
                            }
                            // If the callee expression is neither an identifier nor a member
                            // (e.g. a general expression that evaluates to a pointer), try
                            // to lower it and handle closure-object calls below.
                            {
                                // Attempt to lower the callee expression to a value
                                // If it lowered to a pointer, treat it as a closure object
                                if let Ok(BasicValueEnum::PointerValue(callee_ptr)) =
                                    self.lower_expr(boxed_expr, function, param_map, locals)
                                {
                                    // compute offsets for fn_ptr (idx 0) and env_ptr (idx 1)
                                    let header_size = self.i64_t.const_int(8u64, false);
                                    let meta_slot = self.i64_t.const_int(8u64, false);
                                    let ptr_sz = self.i64_t.const_int(8u64, false);
                                    let off_fn = self
                                        .builder
                                        .build_int_add(header_size, meta_slot, "hdr_plus_meta")
                                        .map_err(|_| Diagnostic::simple("LLVM builder error"))?;
                                    let fn_ptr_i8 = self
                                        .i8_ptr_from_offset_i64(callee_ptr, off_fn, "closure_fn_i8")
                                        .map_err(|_| Diagnostic::simple("operation failed"))?;
                                    let off_env = self
                                        .builder
                                        .build_int_add(off_fn, ptr_sz, "off_env")
                                        .map_err(|_| Diagnostic::simple("LLVM builder error"))?;
                                    let env_ptr_i8 = self
                                        .i8_ptr_from_offset_i64(
                                            callee_ptr,
                                            off_env,
                                            "closure_env_i8",
                                        )
                                        .map_err(|_| Diagnostic::simple("operation failed"))?;

                                    let fn_ptr_slot_ty =
                                        self.context.ptr_type(AddressSpace::default());
                                    let fn_ptr_slot = self
                                        .builder
                                        .build_pointer_cast(
                                            fn_ptr_i8,
                                            fn_ptr_slot_ty,
                                            "fn_ptr_slot_cast",
                                        )
                                        .map_err(|_| Diagnostic::simple("LLVM builder error"))?;
                                    let fn_ptr_bv = match self.builder.build_load(
                                        self.i8ptr_t,
                                        fn_ptr_slot,
                                        "loaded_fn_ptr",
                                    ) {
                                        Ok(v) => v,
                                        Err(_) => {
                                            return Err(Diagnostic::simple("operation failed"));
                                        }
                                    };

                                    let env_slot_ty =
                                        self.context.ptr_type(AddressSpace::default());
                                    let env_slot = self
                                        .builder
                                        .build_pointer_cast(
                                            env_ptr_i8,
                                            env_slot_ty,
                                            "env_slot_cast",
                                        )
                                        .map_err(|_| Diagnostic::simple("LLVM builder error"))?;
                                    let env_bv = match self.builder.build_load(
                                        self.i8ptr_t,
                                        env_slot,
                                        "loaded_env_ptr",
                                    ) {
                                        Ok(v) => v,
                                        Err(_) => {
                                            return Err(Diagnostic::simple("operation failed"));
                                        }
                                    };

                                    // Prepare user args lowered
                                    let mut lowered_args: Vec<
                                        inkwell::values::BasicMetadataValueEnum,
                                    > = Vec::new();
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

                                    // Indirect call via loaded function pointer
                                    if let BasicValueEnum::PointerValue(fn_ptr_pv) = fn_ptr_bv {
                                        let mut param_types: Vec<
                                            inkwell::types::BasicMetadataTypeEnum,
                                        > = Vec::new();
                                        param_types.push(self.i8ptr_t.into());
                                        for _ in &lowered_args {
                                            param_types.push(self.i8ptr_t.into());
                                        }
                                        let fn_ty = self.i8ptr_t.fn_type(&param_types, false);

                                        let mut call_args: Vec<
                                            inkwell::values::BasicMetadataValueEnum,
                                        > = Vec::new();
                                        call_args.push(env_bv.into());
                                        call_args.extend(lowered_args);
                                        let cs = match self.builder.build_indirect_call(
                                            fn_ty,
                                            fn_ptr_pv,
                                            &call_args,
                                            "call_closure",
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
                                        let zero = self.f64_t.const_float(0.0);
                                        return Ok(zero.as_basic_value_enum());
                                    }
                                }
                                {
                                    // Emit a diagnostic with context to aid debugging
                                    #[cfg(debug_assertions)]
                                    {
                                        eprintln!(
                                            "[debug unsupported_call] callee_span={} callee_cached=...",
                                            call.span.lo.0
                                        );
                                    }
                                    let d = Diagnostic::simple_with_span(
                                        "unsupported member call or dynamic callee",
                                        call.span.lo.0 as usize,
                                    );
                                    crate::diagnostics::emit_diagnostic(&d, Some(self.source));
                                    Err(d)
                                }
                            }
                        }
                        _ => {
                            // Attempt to lower the callee as a general expression.
                            // This covers calling closure objects (layout: [header][meta][fn_ptr][env_ptr]).
                            if let Ok(callee_val) =
                                self.lower_expr(boxed_expr, function, param_map, locals)
                            {
                                if let BasicValueEnum::PointerValue(callee_ptr) = callee_val {
                                    // compute offsets for fn_ptr (idx 0) and env_ptr (idx 1)
                                    let header_size = self.i64_t.const_int(8u64, false);
                                    let meta_slot = self.i64_t.const_int(8u64, false);
                                    let ptr_sz = self.i64_t.const_int(8u64, false);
                                    let off_fn = self
                                        .builder
                                        .build_int_add(header_size, meta_slot, "hdr_plus_meta")
                                        .map_err(|_| Diagnostic::simple("LLVM builder error"))?;
                                    // fn_ptr offset = header + meta_slot + 0*8 == off_fn
                                    let fn_ptr_i8 = self
                                        .i8_ptr_from_offset_i64(callee_ptr, off_fn, "closure_fn_i8")
                                        .map_err(|_| Diagnostic::simple("operation failed"))?;
                                    // env_ptr offset = off_fn + 8
                                    let off_env = self
                                        .builder
                                        .build_int_add(off_fn, ptr_sz, "off_env")
                                        .map_err(|_| Diagnostic::simple("LLVM builder error"))?;
                                    let env_ptr_i8 = self
                                        .i8_ptr_from_offset_i64(
                                            callee_ptr,
                                            off_env,
                                            "closure_env_i8",
                                        )
                                        .map_err(|_| Diagnostic::simple("operation failed"))?;

                                    // bitcast fn_ptr_i8 to pointer-to-pointer (i8**), load the stored function pointer
                                    let fn_ptr_slot_ty =
                                        self.context.ptr_type(AddressSpace::default());
                                    let fn_ptr_slot = self
                                        .builder
                                        .build_pointer_cast(
                                            fn_ptr_i8,
                                            fn_ptr_slot_ty,
                                            "fn_ptr_slot_cast",
                                        )
                                        .map_err(|_| Diagnostic::simple("LLVM builder error"))?;
                                    let fn_ptr_bv = match self.builder.build_load(
                                        self.i8ptr_t,
                                        fn_ptr_slot,
                                        "loaded_fn_ptr",
                                    ) {
                                        Ok(v) => v,
                                        Err(_) => {
                                            return Err(Diagnostic::simple("operation failed"));
                                        }
                                    };

                                    // bitcast env_ptr_i8 similarly and load env pointer
                                    let env_slot_ty =
                                        self.context.ptr_type(AddressSpace::default());
                                    let env_slot = self
                                        .builder
                                        .build_pointer_cast(
                                            env_ptr_i8,
                                            env_slot_ty,
                                            "env_slot_cast",
                                        )
                                        .map_err(|_| Diagnostic::simple("LLVM builder error"))?;
                                    let env_bv = match self.builder.build_load(
                                        self.i8ptr_t,
                                        env_slot,
                                        "loaded_env_ptr",
                                    ) {
                                        Ok(v) => v,
                                        Err(_) => {
                                            return Err(Diagnostic::simple("operation failed"));
                                        }
                                    };

                                    // Prepare user args lowered
                                    let mut lowered_args: Vec<
                                        inkwell::values::BasicMetadataValueEnum,
                                    > = Vec::new();
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

                                    // First consult static mapping: if this callee expression
                                    // originated from a local that we recorded as holding a
                                    // freshly-created closure, use the statically-known
                                    // return type. Otherwise, fall back to the runtime
                                    // ret_tag stored in the object.
                                    let static_ret =
                                        self.last_expr_origin_local.borrow().clone().and_then(
                                            |n| {
                                                self.closure_local_rettype.borrow().get(&n).cloned()
                                            },
                                        );

                                    if let BasicValueEnum::PointerValue(fn_ptr_pv) = fn_ptr_bv {
                                        // construct parameter list
                                        let mut param_types: Vec<
                                            inkwell::types::BasicMetadataTypeEnum,
                                        > = Vec::new();
                                        param_types.push(self.i8ptr_t.into());
                                        for _ in &lowered_args {
                                            param_types.push(self.i8ptr_t.into());
                                        }

                                        // choose return type
                                        let fn_ty = if let Some(rt) = static_ret {
                                            match rt {
                                                crate::types::OatsType::Number => {
                                                    self.f64_t.fn_type(&param_types, false)
                                                }
                                                crate::types::OatsType::Void => self
                                                    .context
                                                    .void_type()
                                                    .fn_type(&param_types, false),
                                                _ => self.i8ptr_t.fn_type(&param_types, false),
                                            }
                                        } else {
                                            // Fallback: read runtime tag from closure object (idx 2)
                                            let ptr_sz = self.i64_t.const_int(8u64, false);
                                            let off_ret = self
                                                .builder
                                                .build_int_add(off_env, ptr_sz, "off_ret")
                                                .map_err(|_| {
                                                    Diagnostic::simple("LLVM builder error")
                                                })?;
                                            let ret_i64_ptr = self
                                                .i8_ptr_from_offset_i64(
                                                    callee_ptr,
                                                    off_ret,
                                                    "closure_ret_i8",
                                                )
                                                .map_err(|_| {
                                                    Diagnostic::simple("operation failed")
                                                })?;
                                            let ret_ptr_ty = self
                                                .context
                                                .ptr_type(inkwell::AddressSpace::default());
                                            let ret_ptr_cast = self
                                                .builder
                                                .build_pointer_cast(
                                                    ret_i64_ptr,
                                                    ret_ptr_ty,
                                                    "ret_ptr_cast",
                                                )
                                                .map_err(|_| {
                                                    Diagnostic::simple("LLVM builder error")
                                                })?;
                                            let ret_bv = match self.builder.build_load(
                                                self.i64_t,
                                                ret_ptr_cast,
                                                "loaded_ret_tag",
                                            ) {
                                                Ok(v) => v,
                                                Err(_) => {
                                                    return Err(Diagnostic::simple(
                                                        "operation failed",
                                                    ));
                                                }
                                            };
                                            if let BasicValueEnum::IntValue(ret_iv) = ret_bv {
                                                let one = self.i64_t.const_int(1, false);
                                                let two = self.i64_t.const_int(2, false);
                                                let is_one = self
                                                    .builder
                                                    .build_int_compare(
                                                        inkwell::IntPredicate::EQ,
                                                        ret_iv,
                                                        one,
                                                        "ret_is_one",
                                                    )
                                                    .map_err(|_| {
                                                        Diagnostic::simple("LLVM builder error")
                                                    })?;
                                                if is_one.is_const() {
                                                    self.f64_t.fn_type(&param_types, false)
                                                } else {
                                                    let is_two = self
                                                        .builder
                                                        .build_int_compare(
                                                            inkwell::IntPredicate::EQ,
                                                            ret_iv,
                                                            two,
                                                            "ret_is_two",
                                                        )
                                                        .map_err(|_| {
                                                            Diagnostic::simple("LLVM builder error")
                                                        })?;
                                                    if is_two.is_const() {
                                                        self.i8ptr_t.fn_type(&param_types, false)
                                                    } else {
                                                        self.context
                                                            .void_type()
                                                            .fn_type(&param_types, false)
                                                    }
                                                }
                                            } else {
                                                // fallback to pointer return
                                                self.i8ptr_t.fn_type(&param_types, false)
                                            }
                                        };

                                        let fn_ptr_ty =
                                            self.context.ptr_type(inkwell::AddressSpace::default());
                                        let fn_ptr_cast = self
                                            .builder
                                            .build_pointer_cast(
                                                fn_ptr_pv,
                                                fn_ptr_ty,
                                                "fn_ptr_to_fnptr_cast",
                                            )
                                            .map_err(|_| {
                                                Diagnostic::simple("LLVM builder error")
                                            })?;

                                        let mut call_args: Vec<
                                            inkwell::values::BasicMetadataValueEnum,
                                        > = Vec::new();
                                        call_args.push(env_bv.into());
                                        call_args.extend(lowered_args);
                                        let cs = match self.builder.build_indirect_call(
                                            fn_ty,
                                            fn_ptr_cast,
                                            &call_args,
                                            "call_closure",
                                        ) {
                                            Ok(cs) => cs,
                                            Err(_) => {
                                                return Err(Diagnostic::simple("operation failed"));
                                            }
                                        };
                                        let either = cs.try_as_basic_value();
                                        if let inkwell::Either::Left(bv) = either {
                                            // clear origin marker
                                            self.last_expr_origin_local.borrow_mut().take();
                                            return Ok(bv);
                                        }
                                        let zero = self.f64_t.const_float(0.0);
                                        // clear origin marker
                                        self.last_expr_origin_local.borrow_mut().take();
                                        return Ok(zero.as_basic_value_enum());
                                    }
                                    Err(Diagnostic::simple_with_span(
                                        "unsupported closure call (indirect call lowering failed)",
                                        call.span.lo.0 as usize,
                                    ))
                                } else {
                                    Err(Diagnostic::simple_with_span(
                                        "unsupported callee expression",
                                        call.span.lo.0 as usize,
                                    ))
                                }
                            } else {
                                Err(Diagnostic::simple_with_span(
                                    "expression lowering failed",
                                    call.span.lo.0 as usize,
                                ))
                            }
                        }
                    }
                } else {
                    Err(Diagnostic::simple_with_span(
                        "unsupported call expression: callee form not supported",
                        call.span.lo.0 as usize,
                    ))
                }
            }
            // (Duplicate closure-call lowering removed; handled in the primary Call arm above.)
            ast::Expr::Assign(assign) => {
                // support simple assignments `ident = expr` where the left side is an identifier
                if let Some(bid) = assign.left.as_ident() {
                    let name = bid.id.sym.to_string();
                    if let Some((ptr, _ty, _init, is_const, _extra, _nominal)) =
                        self.find_local(locals, &name)
                    {
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
                            // Propagate closure-local return type mapping when RHS
                            // expression originated from a known local that holds a
                            // freshly-created closure (e.g., __closure_tmp).
                            if let Some(orig) = self.last_expr_origin_local.borrow().clone()
                                && let Some(rt) = self.closure_local_rettype.borrow().get(&orig)
                            {
                                self.closure_local_rettype
                                    .borrow_mut()
                                    .insert(name.clone(), rt.clone());
                            }
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
                        // Capture RHS origin (if this RHS expression originated from a local)
                        let rhs_origin_after_rhs = self.last_expr_origin_local.borrow().clone();
                        // Only handle dot-member (obj.prop), not computed (obj[expr])
                        use deno_ast::swc::ast::MemberProp;
                        if let MemberProp::Ident(prop_ident) = &member.prop {
                            let field_name = prop_ident.sym.to_string();

                            // Lower the object to get its pointer
                            let lowered_obj_res =
                                self.lower_expr(&member.obj, function, param_map, locals);
                            // Capture object origin (if object expression originated from a local)
                            let obj_origin_after_obj = self.last_expr_origin_local.borrow().clone();
                            if let Ok(BasicValueEnum::PointerValue(obj_ptr)) = lowered_obj_res {
                                // If both RHS and object have origins, and RHS origin maps to a closure ret type,
                                // synthesize a mapping for this object's field so loads can recover the closure type.
                                if let (Some(obj_orig), Some(rhs_orig)) =
                                    (obj_origin_after_obj.clone(), rhs_origin_after_rhs.clone())
                                    && let Some(rt) =
                                        self.closure_local_rettype.borrow().get(&rhs_orig)
                                {
                                    let field_key = format!("{}.{}", obj_orig, field_name);
                                    self.closure_local_rettype
                                        .borrow_mut()
                                        .insert(field_key, rt.clone());
                                }
                                // Determine the class name from the object expression
                                let mut class_name_opt: Option<String> = None;

                                // Check if obj is `this` or a named parameter
                                if let deno_ast::swc::ast::Expr::Ident(ident) = &*member.obj {
                                    let ident_name = ident.sym.to_string();
                                    if ident_name == "this" {
                                        // If we're inside a constructor function named <Class>_ctor,
                                        // infer the class name from the function name. Otherwise
                                        // fall back to fn_param_types map (receiver typed functions).
                                        let fname = function.get_name().to_str().unwrap_or("");
                                        if let Some(cls) = fname.strip_suffix("_ctor") {
                                            class_name_opt = Some(cls.to_string());
                                        } else if let Some(param_types) =
                                            self.fn_param_types.borrow().get(fname)
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
                                    } else {
                                        // Not a parameter: try to resolve a local variable's nominal
                                        // class by inspecting locals. Do NOT fall back to scanning
                                        // `class_fields` — require the local to carry a nominal
                                        // annotation to be used for member lowering.
                                        if let Some((_, _ty, _init, _is_const, _is_weak, nominal)) =
                                            self.find_local(locals, &ident_name)
                                            && let Some(nom) = nominal
                                        {
                                            class_name_opt = Some(nom);
                                        }
                                    }
                                } else if matches!(&*member.obj, deno_ast::swc::ast::Expr::This(_))
                                {
                                    let fname = function.get_name().to_str().unwrap_or("");
                                    if let Some(cls) = fname.strip_suffix("_ctor") {
                                        class_name_opt = Some(cls.to_string());
                                    } else if let Some(param_types) =
                                        self.fn_param_types.borrow().get(fname)
                                        && !param_types.is_empty()
                                        && let crate::types::OatsType::NominalStruct(n) =
                                            &param_types[0]
                                    {
                                        class_name_opt = Some(n.clone());
                                    }
                                }

                                // Require that we have a nominal class name inferred from
                                // the object expression (from `this`, a typed parameter,
                                // or a local with a nominal). Do not perform fallback
                                // scans over `class_fields` here.
                                let class_name = if let Some(c) = class_name_opt.clone() {
                                    c
                                } else {
                                    let fname = function.get_name().to_str().unwrap_or("<unknown>");
                                    return Err(Diagnostic::simple(format!(
                                        "unsupported member assignment: could not infer class for field '{}' in function '{}'",
                                        field_name, fname
                                    )));
                                };

                                // Look up field list for this class
                                if let Some(fields) = self.class_fields.borrow().get(&class_name) {
                                    // Find the field by name
                                    if let Some((field_idx, (_fname, field_ty))) = fields
                                        .iter()
                                        .enumerate()
                                        .find(|(_, (n, _))| n == &field_name)
                                    {
                                        // Compute field offset: header (u64) + field_idx * sizeof(ptr)
                                        let hdr_size = self
                                            .i64_t
                                            .const_int(std::mem::size_of::<u64>() as u64, false);
                                        let ptr_sz = self
                                            .i64_t
                                            .const_int(std::mem::size_of::<usize>() as u64, false);
                                        let idx_const =
                                            self.i64_t.const_int(field_idx as u64, false);
                                        let mul = self
                                            .builder
                                            .build_int_mul(idx_const, ptr_sz, "fld_off_mul")
                                            .map_err(|_| {
                                                Diagnostic::simple("LLVM builder error")
                                            })?;
                                        // Reserve an 8-byte metadata slot after the header so field
                                        // offsets are computed as: header_size + meta_slot + idx * ptr_size
                                        let meta_slot = self.i64_t.const_int(8u64, false);
                                        let tmp = self
                                            .builder
                                            .build_int_add(hdr_size, meta_slot, "hdr_plus_meta")
                                            .map_err(|_| {
                                                Diagnostic::simple("LLVM builder error")
                                            })?;
                                        let offset = self
                                            .builder
                                            .build_int_add(tmp, mul, "fld_off")
                                            .map_err(|_| {
                                                Diagnostic::simple("LLVM builder error")
                                            })?;
                                        // Cast offset to i64 (we already have it as i64) and compute i8* pointer
                                        let field_ptr = self
                                            .i8_ptr_from_offset_i64(
                                                obj_ptr,
                                                offset,
                                                "field_i8ptr_store",
                                            )
                                            .map_err(|_| Diagnostic::simple("operation failed"))?;

                                        // Store based on field type
                                        match field_ty {
                                            crate::types::OatsType::Number => {
                                                // Coerce RHS to f64 then store into an f64* slot
                                                if let Some(fv) = self.coerce_to_f64(new_val) {
                                                    let f64_ptr_ty = self
                                                        .context
                                                        .ptr_type(AddressSpace::default());
                                                    let f64_ptr = self
                                                        .builder
                                                        .build_pointer_cast(
                                                            field_ptr,
                                                            f64_ptr_ty,
                                                            "f64_ptr_cast_store",
                                                        )
                                                        .map_err(|_| {
                                                            Diagnostic::simple("LLVM builder error")
                                                        })?;
                                                    let _ = self.builder.build_store(
                                                        f64_ptr,
                                                        fv.as_basic_value_enum(),
                                                    );
                                                    return Ok(fv.as_basic_value_enum());
                                                } else {
                                                    return Err(Diagnostic::simple(
                                                        "expected numeric value for number field",
                                                    ));
                                                }
                                            }
                                            crate::types::OatsType::Union(_) => {
                                                // Use boxed pointer representation for unions.
                                                // Always store an i8* pointing to a small heap object produced by union_box_*.
                                                // Slot for unions is stored as an i8* value in memory.
                                                // The slot pointer therefore has type i8** (pointer-to-i8*).
                                                let slot_ptr_ty =
                                                    self.context.ptr_type(AddressSpace::default());
                                                let slot_ptr =
                                                    match self.builder.build_pointer_cast(
                                                        field_ptr,
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
                                                    if let BasicValueEnum::FloatValue(fv) = new_val
                                                    {
                                                        let box_fn = self.get_union_box_f64();
                                                        let cs = match self.builder.build_call(
                                                            box_fn,
                                                            &[fv.into()],
                                                            "union_box_f64_call",
                                                        ) {
                                                            Ok(cs) => cs,
                                                            Err(_) => {
                                                                return Err(Diagnostic::simple(
                                                                    "operation failed",
                                                                ));
                                                            }
                                                        };
                                                        match cs.try_as_basic_value() {
                                                            inkwell::Either::Left(bv) => bv,
                                                            _ => {
                                                                return Err(Diagnostic::simple(
                                                                    "operation failed",
                                                                ));
                                                            }
                                                        }
                                                    } else if let BasicValueEnum::PointerValue(pv) =
                                                        new_val
                                                    {
                                                        let box_fn = self.get_union_box_ptr();
                                                        let cs = match self.builder.build_call(
                                                            box_fn,
                                                            &[pv.into()],
                                                            "union_box_ptr_call",
                                                        ) {
                                                            Ok(cs) => cs,
                                                            Err(_) => {
                                                                return Err(Diagnostic::simple(
                                                                    "operation failed",
                                                                ));
                                                            }
                                                        };
                                                        match cs.try_as_basic_value() {
                                                            inkwell::Either::Left(bv) => bv,
                                                            _ => {
                                                                return Err(Diagnostic::simple(
                                                                    "operation failed",
                                                                ));
                                                            }
                                                        }
                                                    } else {
                                                        return Err(Diagnostic::simple(
                                                            "unsupported union payload type",
                                                        ));
                                                    };

                                                // Store the boxed pointer into the field slot
                                                let _ =
                                                    self.builder.build_store(slot_ptr, boxed_new);
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
                                            | crate::types::OatsType::Array(_)
                                            | crate::types::OatsType::Option(_)
                                            | crate::types::OatsType::Weak(_)
                                            | crate::types::OatsType::Promise(_) => {
                                                // Cast to pointer type for slot
                                                // Slot for ref-like fields (string/struct/array) stores an i8* value.
                                                // The slot pointer therefore has type i8** (pointer-to-i8*).
                                                let slot_ptr_ty =
                                                    self.context.ptr_type(AddressSpace::default());
                                                let slot_ptr =
                                                    match self.builder.build_pointer_cast(
                                                        field_ptr,
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
                                                // Ensure new_val is a pointer before storing
                                                if let BasicValueEnum::PointerValue(new_pv) =
                                                    new_val
                                                {
                                                    let _ =
                                                        self.builder.build_store(slot_ptr, new_val);
                                                    // Increment new value's refcount
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
                                                    return Ok(new_val);
                                                } else {
                                                    return Err(Diagnostic::simple(
                                                        "expected pointer value for reference field",
                                                    ));
                                                }
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
                    } else {
                        // member.obj did not lower to a pointer value — emit a generic diagnostic
                        return Err(Diagnostic::simple(
                            "unsupported member assignment: member object did not lower to a pointer",
                        ));
                    }
                }
                Err(Diagnostic::simple_with_span(
                    "unsupported assignment target or pattern",
                    assign.span.lo.0 as usize,
                ))
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

                Err(Diagnostic::simple_with_span(
                    "unsupported conditional expression (ternary) form",
                    cond.span.lo.0 as usize,
                ))
            }
            ast::Expr::Lit(lit) => {
                use deno_ast::swc::ast::Lit;
                match lit {
                    Lit::Num(n) => {
                        let fv = self.f64_t.const_float(n.value);
                        Ok(fv.as_basic_value_enum())
                    }
                    Lit::Null(_) => {
                        // Represent `null` as a null i8* pointer so it can be
                        // stored into pointer-typed fields (strings, objects, arrays).
                        let null_ptr = self.i8ptr_t.const_null();
                        Ok(null_ptr.as_basic_value_enum())
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
                        Err(Diagnostic::simple_with_span(
                            "failed to lower string literal",
                            s.span.lo.0 as usize,
                        ))
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
                            return Err(Diagnostic::simple_with_span(
                                "expression lowering failed",
                                arr.span.lo.0 as usize,
                            ))?;
                        }
                    } else {
                        // elided element like [ , ] -> treat as undefined -> unsupported
                        return Err(Diagnostic::simple_with_span(
                            "expression lowering failed",
                            arr.span.lo.0 as usize,
                        ))?;
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
                    return Err(Diagnostic::simple_with_span(
                        "expression lowering failed",
                        arr.span.lo.0 as usize,
                    ))?;
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
                                return Err(Diagnostic::simple_with_span(
                                    "expression lowering failed",
                                    arr.span.lo.0 as usize,
                                ))?;
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
                            return Err(Diagnostic::simple_with_span(
                                "expression lowering failed",
                                arr.span.lo.0 as usize,
                            ))?;
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
                            BasicValueEnum::FloatValue(fv) => {
                                // Box numeric payload into a union object and store pointer via array_set_ptr
                                let box_fn = self.get_union_box_f64();
                                let cs = match self.builder.build_call(
                                    box_fn,
                                    &[fv.into()],
                                    "union_box_f64_call",
                                ) {
                                    Ok(cs) => cs,
                                    Err(_) => return Err(Diagnostic::simple("operation failed")),
                                };
                                let boxed_ptr = match cs.try_as_basic_value() {
                                    inkwell::Either::Left(bv) => bv.into_pointer_value(),
                                    _ => return Err(Diagnostic::simple("operation failed")),
                                };
                                let idx_const = self.i64_t.const_int(i as u64, false);
                                match self.builder.build_call(
                                    array_set_ptr_fn,
                                    &[arr_ptr.into(), idx_const.into(), boxed_ptr.into()],
                                    "array_set_ptr_call",
                                ) {
                                    Ok(_cs) => (),
                                    Err(_) => return Err(Diagnostic::simple("operation failed")),
                                };
                            }
                            _ => {
                                return Err(Diagnostic::simple_with_span(
                                    "operation failed",
                                    arr.span.lo.0 as usize,
                                ));
                            }
                        }
                    }
                    Ok(arr_ptr.as_basic_value_enum())
                }
            }
            ast::Expr::This(this_expr) => {
                if let Some((ptr, ty, init, _ignore, _extra, _nominal)) =
                    self.find_local(locals, "this")
                    && init
                    && let Ok(loaded) = self.builder.build_load(ty, ptr, "this_load")
                {
                    return Ok(loaded);
                }
                if let Some(idx) = param_map.get("this") {
                    if let Some(pv) = function.get_nth_param(*idx) {
                        return Ok(pv);
                    } else {
                        return Err(Diagnostic::simple_with_span(
                            "expression lowering failed",
                            this_expr.span.lo.0 as usize,
                        ))?;
                    }
                }
                Err(Diagnostic::simple_with_span(
                    "unresolved `this` reference in this context",
                    this_expr.span.lo.0 as usize,
                ))
            }
            ast::Expr::Member(member) => {
                // Support both computed member access (obj[expr]) and dot-member (obj.prop)
                use deno_ast::swc::ast::MemberProp;
                match &member.prop {
                    MemberProp::Computed(boxed) => {
                        // lower object and index separately so we can produce clearer diagnostics
                        let obj_res = self.lower_expr(&member.obj, function, param_map, locals);
                        let idx_res = self.lower_expr(&boxed.expr, function, param_map, locals);
                        let obj_val = match obj_res {
                            Ok(v) => v,
                            Err(_) => {
                                return Err(Diagnostic::simple_with_span(
                                    "failed to lower member object expression",
                                    member.span.lo.0 as usize,
                                ));
                            }
                        };
                        let idx_val = match idx_res {
                            Ok(v) => v,
                            Err(_) => {
                                return Err(Diagnostic::simple_with_span(
                                    "failed to lower member index expression",
                                    member.span.lo.0 as usize,
                                ));
                            }
                        };

                        // Only support pointer-array indexing (i8**). Try to
                        // infer a nominal class for the object (this may be a
                        // tuple shape we registered earlier) so we can lower
                        // fixed-field access without calling generic array_get
                        // runtime helpers.
                        if let BasicValueEnum::PointerValue(arr_ptr) = obj_val {
                            // Try to infer a nominal class name from the object
                            let mut class_name_opt: Option<String> = None;
                            if let deno_ast::swc::ast::Expr::Ident(ident) = &*member.obj {
                                let ident_name = ident.sym.to_string();
                                // check locals for nominal annotation
                                if let Some((_, _ty, _init, _is_const, _is_weak, nominal)) =
                                    self.find_local(locals, &ident_name)
                                    && let Some(nom) = nominal
                                {
                                    class_name_opt = Some(nom);
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
                            } else if matches!(&*member.obj, deno_ast::swc::ast::Expr::This(_)) {
                                let fname = function.get_name().to_str().unwrap_or("");
                                if let Some(param_types) = self.fn_param_types.borrow().get(fname)
                                    && !param_types.is_empty()
                                    && let crate::types::OatsType::NominalStruct(n) =
                                        &param_types[0]
                                {
                                    class_name_opt = Some(n.clone());
                                }
                            }

                            // If we inferred a nominal type and have its fields registered,
                            // perform field load similar to dot-member lowering.
                            if let Some(class_name) = class_name_opt.clone()
                                && let Some(fields) = self.class_fields.borrow().get(&class_name)
                            {
                                // compute integer index
                                let idx_i64 = match idx_val {
                                    BasicValueEnum::IntValue(iv) => self
                                        .builder
                                        .build_int_cast(iv, self.i64_t, "idx_i64")
                                        .map_err(|_| Diagnostic::simple("LLVM builder error"))?,
                                    BasicValueEnum::FloatValue(fv) => self
                                        .builder
                                        .build_float_to_signed_int(fv, self.i64_t, "f2i")
                                        .map_err(|_| Diagnostic::simple("LLVM builder error"))?,
                                    _ => {
                                        return Err(Diagnostic::simple_with_span(
                                            "unsupported index type for computed member access",
                                            member.span.lo.0 as usize,
                                        ));
                                    }
                                };
                                // If index is a compile-time constant we can use it to lookup the field
                                if let Some(const_idx) = idx_i64.get_zero_extended_constant() {
                                    let idx_usize = const_idx as usize;
                                    if let Some((field_idx, (_fname, field_ty))) =
                                        fields.iter().enumerate().find(|(i, _)| *i == idx_usize)
                                    {
                                        // compute offset like in dot-member lowering
                                        let hdr_size = self
                                            .i64_t
                                            .const_int(std::mem::size_of::<u64>() as u64, false);
                                        let ptr_sz = self
                                            .i64_t
                                            .const_int(std::mem::size_of::<usize>() as u64, false);
                                        let idx_const =
                                            self.i64_t.const_int(field_idx as u64, false);
                                        let mul = self
                                            .builder
                                            .build_int_mul(idx_const, ptr_sz, "fld_off_mul")
                                            .map_err(|_| {
                                                Diagnostic::simple("LLVM builder error")
                                            })?;
                                        let meta_slot = self.i64_t.const_int(8u64, false);
                                        let tmp = self
                                            .builder
                                            .build_int_add(hdr_size, meta_slot, "hdr_plus_meta")
                                            .map_err(|_| {
                                                Diagnostic::simple("LLVM builder error")
                                            })?;
                                        let offset = self
                                            .builder
                                            .build_int_add(tmp, mul, "fld_off")
                                            .map_err(|_| {
                                                Diagnostic::simple("LLVM builder error")
                                            })?;
                                        let gep_ptr = self
                                            .i8_ptr_from_offset_i64(arr_ptr, offset, "field_i8ptr")
                                            .map_err(|_| Diagnostic::simple("operation failed"))?;
                                        match field_ty {
                                            crate::types::OatsType::Number => {
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
                                                let loaded = self
                                                    .builder
                                                    .build_load(
                                                        self.f64_t,
                                                        f64_ptr,
                                                        "field_f64_load",
                                                    )
                                                    .map_err(|_| {
                                                        Diagnostic::simple("operation failed")
                                                    })?;
                                                return Ok(loaded.as_basic_value_enum());
                                            }
                                            crate::types::OatsType::String
                                            | crate::types::OatsType::NominalStruct(_)
                                            | crate::types::OatsType::Array(_) => {
                                                let slot_ptr_ty =
                                                    self.context.ptr_type(AddressSpace::default());
                                                let slot_ptr = self
                                                    .builder
                                                    .build_pointer_cast(
                                                        gep_ptr,
                                                        slot_ptr_ty,
                                                        "slot_ptr_cast",
                                                    )
                                                    .map_err(|_| {
                                                        Diagnostic::simple("operation failed")
                                                    })?;
                                                let loaded = self
                                                    .builder
                                                    .build_load(
                                                        self.i8ptr_t,
                                                        slot_ptr,
                                                        "field_load",
                                                    )
                                                    .map_err(|_| {
                                                        Diagnostic::simple("operation failed")
                                                    })?;
                                                return Ok(loaded.as_basic_value_enum());
                                            }
                                            crate::types::OatsType::Union(_) => {
                                                let slot_ptr_ty =
                                                    self.context.ptr_type(AddressSpace::default());
                                                let slot_ptr = self
                                                    .builder
                                                    .build_pointer_cast(
                                                        gep_ptr,
                                                        slot_ptr_ty,
                                                        "slot_ptr_cast",
                                                    )
                                                    .map_err(|_| {
                                                        Diagnostic::simple("operation failed")
                                                    })?;
                                                let boxed = self
                                                    .builder
                                                    .build_load(
                                                        self.i8ptr_t,
                                                        slot_ptr,
                                                        "union_boxed_load",
                                                    )
                                                    .map_err(|_| {
                                                        Diagnostic::simple("operation failed")
                                                    })?;
                                                if let BasicValueEnum::PointerValue(boxed_ptr) =
                                                    boxed
                                                {
                                                    let unbox_f = self.get_union_unbox_f64();
                                                    let cs = self
                                                        .builder
                                                        .build_call(
                                                            unbox_f,
                                                            &[boxed_ptr.into()],
                                                            "union_unbox_f64_call",
                                                        )
                                                        .map_err(|_| {
                                                            Diagnostic::simple("operation failed")
                                                        })?;
                                                    if let inkwell::Either::Left(bv) =
                                                        cs.try_as_basic_value()
                                                    {
                                                        return Ok(bv);
                                                    }
                                                    let unbox_p = self.get_union_unbox_ptr();
                                                    let cs2 = self
                                                        .builder
                                                        .build_call(
                                                            unbox_p,
                                                            &[boxed_ptr.into()],
                                                            "union_unbox_ptr_call",
                                                        )
                                                        .map_err(|_| {
                                                            Diagnostic::simple("operation failed")
                                                        })?;
                                                    if let inkwell::Either::Left(bv2) =
                                                        cs2.try_as_basic_value()
                                                    {
                                                        return Ok(bv2);
                                                    }
                                                }
                                                return Err(Diagnostic::simple("operation failed"));
                                            }
                                            _ => {
                                                return Err(Diagnostic::simple(
                                                    "unsupported field type for tuple/nominal access",
                                                ));
                                            }
                                        }
                                    }
                                }
                            }
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
                                _ => {
                                    return Err(Diagnostic::simple_with_span(
                                        "unsupported index type for computed member access",
                                        member.span.lo.0 as usize,
                                    ));
                                }
                            };

                            // If index is numeric, try typed runtime helper array_get_f64 first
                            if matches!(
                                idx_val,
                                BasicValueEnum::IntValue(_) | BasicValueEnum::FloatValue(_)
                            ) {
                                let array_get = self.get_array_get_f64();
                                let cs = match self.builder.build_call(
                                    array_get,
                                    &[arr_ptr.into(), idx_i64.into()],
                                    "array_get_f64_call",
                                ) {
                                    Ok(cs) => cs,
                                    Err(_) => {
                                        return Err(Diagnostic::simple_with_span(
                                            "operation failed",
                                            member.span.lo.0 as usize,
                                        ));
                                    }
                                };
                                let either = cs.try_as_basic_value();
                                if let inkwell::Either::Left(bv) = either {
                                    return Ok(bv);
                                }
                            }

                            // Fallback: call runtime helper that returns a pointer and rc_inc's it
                            let array_get_ptr_fn = self.get_array_get_ptr();
                            let cs = match self.builder.build_call(
                                array_get_ptr_fn,
                                &[arr_ptr.into(), idx_i64.into()],
                                "array_get_ptr_call",
                            ) {
                                Ok(cs) => cs,
                                Err(_) => {
                                    return Err(Diagnostic::simple_with_span(
                                        "operation failed",
                                        member.span.lo.0 as usize,
                                    ));
                                }
                            };
                            let either = cs.try_as_basic_value();
                            if let inkwell::Either::Left(bv) = either {
                                return Ok(bv);
                            }
                        } else {
                            return Err(Diagnostic::simple_with_span(
                                "computed member access on non-pointer object",
                                member.span.lo.0 as usize,
                            ));
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
                                    // If it's not a parameter, check locals for a nominal annotation
                                    else if let Some((
                                        _,
                                        _ty,
                                        _init,
                                        _is_const,
                                        _is_weak,
                                        nominal,
                                    )) = self.find_local(locals, &ident_name)
                                        && let Some(nom) = nominal
                                    {
                                        class_name_opt = Some(nom);
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
                                        // Reserve an 8-byte metadata slot after the header so field
                                        // offsets are computed as: header_size + meta_slot + idx * ptr_size
                                        let meta_slot = self.i64_t.const_int(8u64, false);
                                        let tmp = self
                                            .builder
                                            .build_int_add(hdr_size, meta_slot, "hdr_plus_meta")
                                            .map_err(|_| {
                                                Diagnostic::simple("LLVM builder error")
                                            })?;
                                        let offset = self
                                            .builder
                                            .build_int_add(tmp, mul, "fld_off")
                                            .map_err(|_| {
                                                Diagnostic::simple("LLVM builder error")
                                            })?;
                                        // We need a i32 index sequence for gep on i8 pointer: cast offset to i64->i32 for index
                                        let offset_i64 = offset;
                                        let gep_ptr = self
                                            .i8_ptr_from_offset_i64(
                                                obj_ptr,
                                                offset_i64,
                                                "field_i8ptr",
                                            )
                                            .map_err(|_| Diagnostic::simple("operation failed"))?;
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
                                                // If the object expression had an origin marker,
                                                // check for a synthetic mapping for this object's field
                                                // and, if present, set last_expr_origin_local so
                                                // callers can find a statically-known closure ret type.
                                                if let Some(obj_orig) =
                                                    self.last_expr_origin_local.borrow().clone()
                                                {
                                                    let field_key =
                                                        format!("{}.{}", obj_orig, field_name);
                                                    if self
                                                        .closure_local_rettype
                                                        .borrow()
                                                        .contains_key(&field_key)
                                                    {
                                                        self.last_expr_origin_local
                                                            .borrow_mut()
                                                            .replace(field_key);
                                                    }
                                                }
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
                {
                    // Build helpful debug context about why member lowering failed
                    let keys: Vec<String> = self.class_fields.borrow().keys().cloned().collect();
                    let mut recv_info = String::from("<unknown>");
                    let mut recv_fields: Vec<String> = Vec::new();
                    if let deno_ast::swc::ast::Expr::Ident(ident) = &*member.obj {
                        let name = ident.sym.to_string();
                        if let Some((_, _ty, _init, _is_const, _is_weak, nominal)) =
                            self.find_local(locals, &name)
                        {
                            recv_info = format!("ident='{}' local_nominal={:?}", name, nominal);
                            if let Some(n) = nominal
                                && let Some(fields) = self.class_fields.borrow().get(&n)
                            {
                                recv_fields = fields.iter().map(|(n, _)| n.clone()).collect();
                            }
                        } else if let Some(idx) = param_map.get(&name) {
                            recv_info = format!("ident='{}' param_idx={}", name, idx);
                            if let Some(param_types) = self
                                .fn_param_types
                                .borrow()
                                .get(function.get_name().to_str().unwrap_or(""))
                            {
                                recv_info.push_str(&format!(" param_types={:?}", param_types));
                            }
                        } else {
                            recv_info = format!("ident='{}' not found in locals/params", name);
                        }
                    }

                    // Also include the lowered ABI kind of the receiver expression.
                    // If lowering the receiver produced a pointer and the prop is
                    // an identifier, attempt a last-resort lookup: scan all
                    // registered nominals for one that contains the requested
                    // field and perform the field load. This handles cases where
                    // earlier inference failed to pick the correct nominal but
                    // the field layout is nevertheless registered.
                    if let Ok(bv) = self.lower_expr(&member.obj, function, param_map, locals)
                        && let BasicValueEnum::PointerValue(obj_ptr) = bv
                    {
                        // If prop is an identifier, try to find a class that
                        // has this field and load it.
                        if let deno_ast::swc::ast::MemberProp::Ident(pi) = &member.prop {
                            let target_field = pi.sym.to_string();
                            for (_class_name, fields) in self.class_fields.borrow().iter() {
                                if let Some((field_idx, (_fname, field_ty))) = fields
                                    .iter()
                                    .enumerate()
                                    .find(|(_, (n, _))| n == &target_field)
                                {
                                    // compute byte offset = sizeof(u64) + meta_slot + idx * ptr_size
                                    let hdr_size = self
                                        .i64_t
                                        .const_int(std::mem::size_of::<u64>() as u64, false);
                                    let ptr_sz = self
                                        .i64_t
                                        .const_int(std::mem::size_of::<usize>() as u64, false);
                                    let idx_const = self.i64_t.const_int(field_idx as u64, false);
                                    let mul = match self.builder.build_int_mul(
                                        idx_const,
                                        ptr_sz,
                                        "fld_off_mul",
                                    ) {
                                        Ok(v) => v,
                                        Err(_) => continue,
                                    };
                                    let meta_slot = self.i64_t.const_int(8u64, false);
                                    let tmp = match self.builder.build_int_add(
                                        hdr_size,
                                        meta_slot,
                                        "hdr_plus_meta",
                                    ) {
                                        Ok(v) => v,
                                        Err(_) => continue,
                                    };
                                    let offset =
                                        match self.builder.build_int_add(tmp, mul, "fld_off") {
                                            Ok(v) => v,
                                            Err(_) => continue,
                                        };
                                    let gep_ptr = match self.i8_ptr_from_offset_i64(
                                        obj_ptr,
                                        offset,
                                        "field_i8ptr",
                                    ) {
                                        Ok(p) => p,
                                        Err(_) => continue,
                                    };
                                    match field_ty {
                                        crate::types::OatsType::Number => {
                                            let f64_ptr = match self.builder.build_pointer_cast(
                                                gep_ptr,
                                                self.context.ptr_type(AddressSpace::default()),
                                                "f64_ptr_cast",
                                            ) {
                                                Ok(p) => p,
                                                Err(_) => continue,
                                            };
                                            if let Ok(loaded) = self.builder.build_load(
                                                self.f64_t,
                                                f64_ptr,
                                                "field_f64_load",
                                            ) {
                                                return Ok(loaded.as_basic_value_enum());
                                            }
                                        }
                                        crate::types::OatsType::String
                                        | crate::types::OatsType::NominalStruct(_)
                                        | crate::types::OatsType::Array(_) => {
                                            let slot_ptr_ty =
                                                self.context.ptr_type(AddressSpace::default());
                                            let slot_ptr = match self.builder.build_pointer_cast(
                                                gep_ptr,
                                                slot_ptr_ty,
                                                "slot_ptr_cast",
                                            ) {
                                                Ok(p) => p,
                                                Err(_) => continue,
                                            };
                                            if let Ok(loaded) = self.builder.build_load(
                                                self.i8ptr_t,
                                                slot_ptr,
                                                "field_load",
                                            ) {
                                                return Ok(loaded.as_basic_value_enum());
                                            }
                                        }
                                        crate::types::OatsType::Union(_) => {
                                            let slot_ptr_ty =
                                                self.context.ptr_type(AddressSpace::default());
                                            let slot_ptr = match self.builder.build_pointer_cast(
                                                gep_ptr,
                                                slot_ptr_ty,
                                                "slot_ptr_cast",
                                            ) {
                                                Ok(p) => p,
                                                Err(_) => continue,
                                            };
                                            if let Ok(boxed) = self.builder.build_load(
                                                self.i8ptr_t,
                                                slot_ptr,
                                                "union_boxed_load",
                                            ) && let BasicValueEnum::PointerValue(boxed_ptr) =
                                                boxed
                                            {
                                                let unbox_f = self.get_union_unbox_f64();
                                                if let Ok(cs) = self.builder.build_call(
                                                    unbox_f,
                                                    &[boxed_ptr.into()],
                                                    "union_unbox_f64_call",
                                                ) && let inkwell::Either::Left(bv) =
                                                    cs.try_as_basic_value()
                                                {
                                                    return Ok(bv);
                                                }
                                                let unbox_p = self.get_union_unbox_ptr();
                                                if let Ok(cs2) = self.builder.build_call(
                                                    unbox_p,
                                                    &[boxed_ptr.into()],
                                                    "union_unbox_ptr_call",
                                                ) && let inkwell::Either::Left(bv2) =
                                                    cs2.try_as_basic_value()
                                                {
                                                    return Ok(bv2);
                                                }
                                            }
                                        }
                                        _ => {}
                                    }
                                }
                            }
                        }
                        // no matching field found; fall through to diagnostic below
                    }

                    let lowered_recv =
                        match self.lower_expr(&member.obj, function, param_map, locals) {
                            Ok(bv) => match bv.get_type() {
                                inkwell::types::BasicTypeEnum::PointerType(_) => {
                                    "pointer".to_string()
                                }
                                inkwell::types::BasicTypeEnum::FloatType(_) => "float".to_string(),
                                inkwell::types::BasicTypeEnum::IntType(_) => "int".to_string(),
                                _ => "other".to_string(),
                            },
                            Err(_) => "lower_failed".to_string(),
                        };

                    let msg = format!(
                        "unsupported member access expression; recv={}, lowered_recv={}, recv_fields={:?}, known_nominals={:?}",
                        recv_info, lowered_recv, recv_fields, keys
                    );
                    Err(Diagnostic::simple_with_span(msg, member.span.lo.0 as usize))
                }
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
                        // Ensure the number of arguments matches the constructor's
                        // declared parameter count. Truncate extra args or pad with
                        // nulls if the function expects more parameters. This fixes
                        // ABI mismatches where the constructor signature and callsite
                        // disagree.
                        let expected = fv.count_params() as usize;
                        let mut call_args = lowered_args.clone();
                        if call_args.len() > expected {
                            call_args.truncate(expected);
                        } else if call_args.len() < expected {
                            // pad with null pointers
                            while call_args.len() < expected {
                                call_args
                                    .push(self.i8ptr_t.const_null().as_basic_value_enum().into());
                            }
                        }

                        let cs = self
                            .builder
                            .build_call(fv, &call_args, "new_call")
                            .map_err(|_| Diagnostic::simple("LLVM builder error"))?;
                        let either = cs.try_as_basic_value();
                        match either {
                            inkwell::Either::Left(bv) => Ok(bv),
                            _ => Err(Diagnostic::simple("operation not supported")),
                        }
                    } else {
                        Err(Diagnostic::simple_with_span(
                            "unknown constructor or missing `<Name>_ctor` function",
                            new_expr.span.lo.0 as usize,
                        ))
                    }
                } else {
                    Err(Diagnostic::simple_with_span(
                        "unsupported `new` callee: only identifier constructors are supported",
                        new_expr.span.lo.0 as usize,
                    ))
                }
            }
            ast::Expr::Await(await_expr) => {
                // Phase 0+ improvement: call runtime `promise_poll_into(promise, out_ptr)`
                // and branch on the returned i32. If ready (1) load the resolved
                // value from out_ptr and return it. If pending (0) return the
                // original promise pointer (so callers still receive a pointer-like value).

                // If async lowering context is present, record that we've
                // emitted one await so the `gen_function_ir` wiring which
                // prepared resume/cont blocks and slot maps can correlate
                // to the lowered awaits. Full save/restore emission will
                // follow in later changes.
                if self.async_local_name_to_slot.borrow().is_some() {
                    let prev = self.async_await_counter.get();
                    self.async_await_counter.set(prev.wrapping_add(1));
                }

                let v = self.lower_expr(&await_expr.arg, function, param_map, locals)?;
                if let BasicValueEnum::PointerValue(pv) = v {
                    // allocate an i8* out slot
                    let out_alloca = match self
                        .builder
                        .build_alloca(self.i8ptr_t.as_basic_type_enum(), "await_out")
                    {
                        Ok(a) => a,
                        Err(_) => {
                            return Err(Diagnostic::simple("alloca failed for await out slot"));
                        }
                    };

                    // Call promise_poll_into(promise, out_slot)
                    let poll_fn = self.get_promise_poll_into();
                    let cs = match self.builder.build_call(
                        poll_fn,
                        &[pv.into(), out_alloca.into()],
                        "promise_poll",
                    ) {
                        Ok(c) => c,
                        Err(_) => return Err(Diagnostic::simple("promise_poll_into call failed")),
                    };

                    let either = cs.try_as_basic_value();
                    let ret_iv = either
                        .left()
                        .ok_or_else(|| Diagnostic::simple("promise_poll_into returned no value"))?
                        .into_int_value();

                    use inkwell::IntPredicate;
                    let one = self.i32_t.const_int(1, false);
                    let is_ready = match self.builder.build_int_compare(
                        IntPredicate::EQ,
                        ret_iv,
                        one,
                        "await_ready_cmp",
                    ) {
                        Ok(iv) => iv,
                        Err(_) => return Err(Diagnostic::simple("int_compare failed")),
                    };

                    let ready_bb = self.context.append_basic_block(function, "await_ready");
                    let pending_bb = self.context.append_basic_block(function, "await_pending");
                    let cont_bb = self.context.append_basic_block(function, "await_cont");

                    match self
                        .builder
                        .build_conditional_branch(is_ready, ready_bb, pending_bb)
                    {
                        Ok(_) => {}
                        Err(_) => {
                            return Err(Diagnostic::simple("failed to build branch for await"));
                        }
                    }

                    // ready: load out slot and branch to cont
                    self.builder.position_at_end(ready_bb);
                    let loaded =
                        match self
                            .builder
                            .build_load(self.i8ptr_t, out_alloca, "await_loaded")
                        {
                            Ok(l) => l,
                            Err(_) => return Err(Diagnostic::simple("await load failed")),
                        };
                    match self.builder.build_unconditional_branch(cont_bb) {
                        Ok(_) => (),
                        Err(_) => {
                            return Err(Diagnostic::simple("failed to branch to cont from ready"));
                        }
                    };

                    // pending: just use the original promise pointer
                    self.builder.position_at_end(pending_bb);
                    let pv_bv = pv.as_basic_value_enum();
                    match self.builder.build_unconditional_branch(cont_bb) {
                        Ok(_) => (),
                        Err(_) => {
                            return Err(Diagnostic::simple("failed to branch to cont from pending"));
                        }
                    };

                    // cont: create phi to select between loaded value and original promise
                    self.builder.position_at_end(cont_bb);
                    let phi_ty = self.i8ptr_t.as_basic_type_enum();
                    let phi = match self.builder.build_phi(phi_ty, "await_phi") {
                        Ok(p) => p,
                        Err(_) => return Err(Diagnostic::simple("failed to create phi for await")),
                    };
                    // incoming values: from ready_bb use loaded, from pending_bb use pv
                    phi.add_incoming(&[
                        (&loaded.as_basic_value_enum(), ready_bb),
                        (&pv_bv, pending_bb),
                    ]);
                    return Ok(phi.as_basic_value());
                }
                // Non-pointer awaited values are returned as-is (numbers/booleans)
                Ok(v)
            }
            ast::Expr::Arrow(arrow) => {
                // Detect captured variables in the arrow body by scanning for
                // identifier usages that are not parameters and that resolve to
                // names in the surrounding `locals` stack. We return a helpful
                // Diagnostic: capture listing is emitted here. Full closure
                // lowering will be implemented separately.
                // (boxed environments + trampolines) is planned in Phase 2.
                fn collect_idents_in_expr(
                    expr: &deno_ast::swc::ast::Expr,
                    out: &mut std::collections::HashSet<String>,
                ) {
                    use deno_ast::swc::ast;
                    match expr {
                        ast::Expr::Ident(id) => {
                            out.insert(id.sym.to_string());
                        }
                        ast::Expr::Bin(b) => {
                            collect_idents_in_expr(&b.left, out);
                            collect_idents_in_expr(&b.right, out);
                        }
                        ast::Expr::Unary(u) => {
                            collect_idents_in_expr(&u.arg, out);
                        }
                        ast::Expr::Call(c) => {
                            use deno_ast::swc::ast::Callee;
                            match &c.callee {
                                Callee::Expr(e) => collect_idents_in_expr(e, out),
                                Callee::Super(_) => {}
                                Callee::Import(_) => {}
                            }
                            for a in &c.args {
                                collect_idents_in_expr(&a.expr, out);
                            }
                        }
                        ast::Expr::Member(m) => {
                            collect_idents_in_expr(&m.obj, out);
                        }
                        ast::Expr::Arrow(a) => {
                            // nested arrow: scan body
                            if a.body.is_block_stmt() {
                                if let deno_ast::swc::ast::BlockStmtOrExpr::BlockStmt(b) = &*a.body
                                {
                                    for s in &b.stmts {
                                        use deno_ast::swc::ast::Stmt;
                                        if let Stmt::Expr(es) = s {
                                            collect_idents_in_expr(&es.expr, out);
                                        }
                                    }
                                }
                            } else if let deno_ast::swc::ast::BlockStmtOrExpr::Expr(e) = &*a.body {
                                collect_idents_in_expr(e, out);
                            }
                        }
                        ast::Expr::Object(o) => {
                            for prop in &o.props {
                                if let deno_ast::swc::ast::PropOrSpread::Prop(p) = prop
                                    && let deno_ast::swc::ast::Prop::KeyValue(kv) = &**p
                                {
                                    collect_idents_in_expr(&kv.value, out);
                                }
                            }
                        }
                        ast::Expr::Array(a) => {
                            for elem in a.elems.iter().flatten() {
                                collect_idents_in_expr(&elem.expr, out);
                            }
                        }
                        ast::Expr::Assign(asg) => {
                            collect_idents_in_expr(&asg.right, out);
                        }
                        ast::Expr::Cond(cnd) => {
                            collect_idents_in_expr(&cnd.test, out);
                            collect_idents_in_expr(&cnd.cons, out);
                            collect_idents_in_expr(&cnd.alt, out);
                        }
                        _ => {}
                    }
                }

                // Build a set of names visible in the surrounding locals stack
                let mut outer_names: std::collections::HashSet<String> =
                    std::collections::HashSet::new();
                for scope in locals.iter() {
                    for k in scope.keys() {
                        outer_names.insert(k.clone());
                    }
                }

                // Collect idents used in the arrow body
                let mut used: std::collections::HashSet<String> = std::collections::HashSet::new();
                if arrow.body.is_block_stmt() {
                    if let deno_ast::swc::ast::BlockStmtOrExpr::BlockStmt(block) = &*arrow.body {
                        for stmt in &block.stmts {
                            use deno_ast::swc::ast::Stmt;
                            if let Stmt::Expr(es) = stmt {
                                collect_idents_in_expr(&es.expr, &mut used);
                            }
                        }
                    }
                } else if let deno_ast::swc::ast::BlockStmtOrExpr::Expr(expr) = &*arrow.body {
                    collect_idents_in_expr(expr, &mut used);
                }

                // Remove arrow params from used
                for param in &arrow.params {
                    if let deno_ast::swc::ast::Pat::Ident(ident) = param {
                        used.remove(&ident.id.sym.to_string());
                    }
                }

                // Any remaining identifiers that exist in outer_names are captures
                let mut captures: Vec<String> = Vec::new();
                for name in used.into_iter() {
                    if outer_names.contains(&name) {
                        captures.push(name);
                    }
                }

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
                    // If no return type annotation is present, default to Number.
                    // TODO: Implement type inference and propagate inferred types.
                    crate::types::OatsType::Number
                };

                // Build LLVM function type. If captures are present, the first
                // parameter is the environment pointer (i8*).
                let mut llvm_param_types: Vec<BasicTypeEnum> = Vec::new();
                if !captures.is_empty() {
                    llvm_param_types.push(self.i8ptr_t.as_basic_type_enum());
                }
                llvm_param_types.extend(param_types.iter().map(|t| self.map_type_to_llvm(t)));
                let fn_type = self.build_llvm_fn_type(&llvm_param_types, &ret_type);

                // Create the arrow function (may accept env param as first arg)
                let arrow_fn = self.module.add_function(&arrow_fn_name, fn_type, None);

                // Record parameter types for the generated arrow function so
                // member-access lowering can consult `fn_param_types` to infer
                // nominal types for parameters (and `this` receiver when
                // applicable). This mirrors `gen_function_ir` behavior.
                self.fn_param_types
                    .borrow_mut()
                    .insert(arrow_fn_name.clone(), param_types.clone());

                // Save current insert block so we can return to it
                let current_block = self.builder.get_insert_block();

                // Build the function body
                let entry_bb = self.context.append_basic_block(arrow_fn, "entry");
                self.builder.position_at_end(entry_bb);

                // Create parameter map for arrow function. If env param exists,
                // shift user parameters by +1.
                let mut arrow_param_map = HashMap::new();
                for (idx, param) in arrow.params.iter().enumerate() {
                    if let ast::Pat::Ident(ident) = param {
                        let mapped_idx = if !captures.is_empty() {
                            (idx as u32) + 1
                        } else {
                            idx as u32
                        };
                        arrow_param_map.insert(ident.id.sym.to_string(), mapped_idx);
                    }
                }

                // Create locals stack for arrow function
                let mut arrow_locals = vec![HashMap::new()];

                // If the arrow accepts an env param (captures exist), bind env fields
                // to locals so body references to captured names resolve.
                if !captures.is_empty() {
                    // env is the first parameter of arrow_fn
                    let env_param = arrow_fn
                        .get_nth_param(0)
                        .ok_or_else(|| Diagnostic::simple("missing env parameter for arrow"))?;
                    let env_ptr = env_param.into_pointer_value();
                    // Convert env pointer to integer for offset math
                    let obj_ptr_int = self
                        .builder
                        .build_ptr_to_int(env_ptr, self.i64_t, "env_addr")
                        .map_err(|_| Diagnostic::simple("ptr_to_int failed"))?;
                    let header_size = 8u64;
                    let meta_slot = 8u64;

                    for (i, cname) in captures.iter().enumerate() {
                        let field_offset = header_size + meta_slot + (i as u64 * 8);
                        let off_const = self.i64_t.const_int(field_offset, false);
                        let field_addr = self
                            .builder
                            .build_int_add(obj_ptr_int, off_const, "cap_field_addr")
                            .map_err(|_| Diagnostic::simple("int_add failed"))?;
                        let field_ptr = self
                            .builder
                            .build_int_to_ptr(field_addr, self.i8ptr_t, "cap_field_ptr")
                            .map_err(|_| Diagnostic::simple("int_to_ptr failed"))?;
                        let loaded = self
                            .builder
                            .build_load(self.i8ptr_t, field_ptr, &format!("cap_load_{}", cname))
                            .map_err(|_| Diagnostic::simple("load failed"))?;

                        // Create an alloca for the captured local inside the arrow function and store the loaded value
                        let alloca = self
                            .builder
                            .build_alloca(self.i8ptr_t, &format!("cap_{}", cname))
                            .map_err(|_| Diagnostic::simple("alloca failed"))?;
                        let _ = self.builder.build_store(alloca, loaded);
                        // Insert into arrow_locals so later identifier lookups resolve to this alloca
                        if let Some(scope) = arrow_locals.last_mut() {
                            scope.insert(
                                cname.clone(),
                                (
                                    alloca,
                                    self.i8ptr_t.as_basic_type_enum(),
                                    true,
                                    true,
                                    false,
                                    None,
                                ),
                            );
                        }
                    }
                }

                // If there are captures, allocate environment in the outer function
                // and construct a boxed closure object to return here.
                //
                // Design note: the environment (`env`) is a heap-allocated
                // object following the standard object layout: [header][meta][fields...].
                // We emit a `*_env_field_map` global which lists which offsets
                // contain pointer-like fields so the runtime's cycle collector
                // can traverse them. The closure object itself stores two
                // pointer fields (fn_ptr and env_ptr) and may optionally carry
                // a `ret_tag` to allow specialized indirect-call lowering.
                //
                // The `closure_local_rettype` map stores a conservative static
                // return type for freshly-created closure temporaries (for
                // example `__closure_tmp`). When present, later indirect calls
                // that use this local can choose an optimized function type
                // (e.g., returning f64) instead of conservatively assuming
                // a pointer return. This is a pragmatic optimization to avoid
                // emitting a runtime ret_tag for all closures.
                if !captures.is_empty() {
                    // Collect current values of captured variables from outer scope
                    // Store tuples of (value, is_weak) so env allocation can use weak increments
                    let mut captured_vals: Vec<(inkwell::values::BasicValueEnum, bool)> =
                        Vec::new();
                    for cname in &captures {
                        // First check if it's a parameter in the outer function
                        if let Some(idx) = param_map.get(cname)
                            && let Some(pv) = function.get_nth_param(*idx)
                        {
                            // Determine whether this parameter was declared Weak<T>
                            let mut is_weak = false;
                            if let Some(param_types) = self
                                .fn_param_types
                                .borrow()
                                .get(function.get_name().to_str().unwrap_or(""))
                            {
                                let idx_usize = *idx as usize;
                                if idx_usize < param_types.len()
                                    && let crate::types::OatsType::Weak(_) = &param_types[idx_usize]
                                {
                                    is_weak = true;
                                }
                            }
                            // If parameter is pointer-like, use directly. If numeric, box it.
                            if pv.get_type().is_pointer_type() {
                                captured_vals.push((pv.as_basic_value_enum(), is_weak));
                            } else if pv.get_type().is_float_type() {
                                // box f64
                                let box_fn = self.get_union_box_f64();
                                let cs = self.builder.build_call(
                                    box_fn,
                                    &[pv.into()],
                                    "union_box_f64_ctor",
                                );
                                if let Ok(cs) = cs
                                    && let inkwell::Either::Left(bv) = cs.try_as_basic_value()
                                {
                                    let boxed_ptr = bv.into_pointer_value();
                                    captured_vals.push((boxed_ptr.as_basic_value_enum(), is_weak));
                                } else {
                                    return Err(Diagnostic::simple(
                                        "failed to box numeric capture",
                                    ));
                                }
                            } else if pv.get_type().is_int_type() {
                                // convert int->f64 then box
                                let iv = pv.into_int_value();
                                let fconv = self
                                    .builder
                                    .build_signed_int_to_float(iv, self.f64_t, "i_to_f")
                                    .map_err(|_| Diagnostic::simple("int->float cast failed"))?;
                                let box_fn = self.get_union_box_f64();
                                let cs = self.builder.build_call(
                                    box_fn,
                                    &[fconv.into()],
                                    "union_box_f64_ctor",
                                );
                                if let Ok(cs) = cs
                                    && let inkwell::Either::Left(bv) = cs.try_as_basic_value()
                                {
                                    let boxed_ptr = bv.into_pointer_value();
                                    captured_vals.push((boxed_ptr.as_basic_value_enum(), is_weak));
                                } else {
                                    return Err(Diagnostic::simple(
                                        "failed to box numeric capture",
                                    ));
                                }
                            } else {
                                return Err(Diagnostic::simple(
                                    "unsupported capture type: non-pointer parameter",
                                ));
                            }
                            continue;
                        }

                        // Otherwise look up local variable
                        if let Some((
                            alloca_ptr,
                            ty,
                            initialized,
                            _is_const,
                            is_weak_flag,
                            _nominal,
                        )) = self.find_local(locals, cname)
                        {
                            if !initialized {
                                return Err(Diagnostic::simple(
                                    "cannot capture uninitialized local",
                                ));
                            }
                            // load current value
                            let loaded = match self.builder.build_load(
                                ty,
                                alloca_ptr,
                                &format!("cap_load_{}", cname),
                            ) {
                                Ok(v) => v,
                                Err(_) => {
                                    return Err(Diagnostic::simple(
                                        "failed to load captured local",
                                    ));
                                }
                            };
                            // If pointer, use directly. If float/int, box into union object.
                            match loaded {
                                BasicValueEnum::PointerValue(_) => {
                                    captured_vals.push((loaded, is_weak_flag))
                                }
                                BasicValueEnum::FloatValue(fv) => {
                                    let box_fn = self.get_union_box_f64();
                                    let cs = self.builder.build_call(
                                        box_fn,
                                        &[fv.into()],
                                        "union_box_f64_ctor",
                                    );
                                    if let Ok(cs) = cs
                                        && let inkwell::Either::Left(bv) = cs.try_as_basic_value()
                                    {
                                        let boxed_ptr = bv.into_pointer_value();
                                        captured_vals
                                            .push((boxed_ptr.as_basic_value_enum(), is_weak_flag));
                                    } else {
                                        return Err(Diagnostic::simple(
                                            "failed to box numeric capture",
                                        ));
                                    }
                                }
                                BasicValueEnum::IntValue(iv) => {
                                    let fconv = self
                                        .builder
                                        .build_signed_int_to_float(iv, self.f64_t, "i_to_f")
                                        .map_err(|_| {
                                            Diagnostic::simple("int->float cast failed")
                                        })?;
                                    let box_fn = self.get_union_box_f64();
                                    let cs = self.builder.build_call(
                                        box_fn,
                                        &[fconv.into()],
                                        "union_box_f64_ctor",
                                    );
                                    if let Ok(cs) = cs
                                        && let inkwell::Either::Left(bv) = cs.try_as_basic_value()
                                    {
                                        let boxed_ptr = bv.into_pointer_value();
                                        captured_vals
                                            .push((boxed_ptr.as_basic_value_enum(), is_weak_flag));
                                    } else {
                                        return Err(Diagnostic::simple(
                                            "failed to box numeric capture",
                                        ));
                                    }
                                }
                                _ => {
                                    return Err(Diagnostic::simple(
                                        "unsupported capture type: non-pointer local",
                                    ));
                                }
                            }
                        } else {
                            return Err(Diagnostic::simple("capture not found in outer scope"));
                        }
                    }

                    // Allocate environment object (heap) storing captured pointer fields
                    // captured_vals is Vec<(BasicValueEnum, bool)> where bool indicates is_weak
                    let env_ptr = self
                        .heap_alloc_with_ptr_fields(captured_vals.as_slice())
                        .map_err(|_| Diagnostic::simple("failed to allocate env object"))?;

                    // Emit field_map global for env and store pointer into env.meta slot
                    let env_gv_name = format!("{}_env_field_map", arrow_fn_name);
                    // offsets are header + meta_slot + idx*8
                    let mut env_offsets: Vec<u64> = Vec::new();
                    let header_size = 8u64;
                    let meta_slot = 8u64;
                    for i in 0..captured_vals.len() {
                        env_offsets.push(header_size + meta_slot + (i as u64 * 8));
                    }
                    let env_gv_i8 = self
                        .emit_field_map_global(&env_gv_name, &env_offsets)
                        .map_err(|_| Diagnostic::simple("failed to emit env field_map"))?;
                    // store into env meta slot
                    let env_ptr_int = self
                        .builder
                        .build_ptr_to_int(env_ptr, self.i64_t, "env_addr_for_meta")
                        .map_err(|_| Diagnostic::simple("ptr_to_int failed"))?;
                    let off_const = self.i64_t.const_int(8, false);
                    let meta_addr = self
                        .builder
                        .build_int_add(env_ptr_int, off_const, "env_meta_addr")
                        .map_err(|_| Diagnostic::simple("int_add failed"))?;
                    let meta_ptr = self
                        .builder
                        .build_int_to_ptr(meta_addr, self.i8ptr_t, "env_meta_ptr")
                        .map_err(|_| Diagnostic::simple("int_to_ptr failed"))?;
                    let _ = self
                        .builder
                        .build_store(meta_ptr, env_gv_i8.as_basic_value_enum());

                    // Build closure object: store function pointer and env pointer into a 2-field heap object
                    let fn_ptr_bv = arrow_fn
                        .as_global_value()
                        .as_pointer_value()
                        .as_basic_value_enum();
                    let env_bv = env_ptr.as_basic_value_enum();
                    // We will store this closure into a tmp local `__closure_tmp` and
                    // record its return type in `closure_local_rettype`. For such
                    // statically-known closures we can avoid emitting the runtime
                    // ret_tag and allocate a compact 2-field object instead.
                    let use_static_layout = true; // conservative: current flow records mapping for __closure_tmp
                    let closure_obj = if use_static_layout {
                        self.heap_alloc_with_ptr_fields_simple(&[fn_ptr_bv, env_bv])
                            .map_err(|_| Diagnostic::simple("failed to allocate closure object"))?
                    } else {
                        // ret_tag: 0=void, 1=number (f64), 2=pointer (i8*)
                        let ret_tag_val: u64 = match ret_type {
                            crate::types::OatsType::Void => 0,
                            crate::types::OatsType::Number => 1,
                            _ => 2,
                        };
                        self.heap_alloc_closure_with_rettag(fn_ptr_bv, env_bv, ret_tag_val)
                            .map_err(|_| Diagnostic::simple("failed to allocate closure object"))?
                    };

                    // Emit field_map for closure object (two pointer fields at offsets 16 and 24)
                    let closure_gv_name = format!("{}_closure_field_map", arrow_fn_name);
                    // fields start after header + meta_slot; fn_ptr at idx 0, env_ptr at idx 1
                    let closure_offsets: Vec<u64> =
                        vec![header_size + meta_slot, header_size + meta_slot + 8];
                    let closure_gv_i8 = self
                        .emit_field_map_global(&closure_gv_name, &closure_offsets)
                        .map_err(|_| Diagnostic::simple("failed to emit closure field_map"))?;
                    // store into closure meta slot
                    let closure_ptr_int = self
                        .builder
                        .build_ptr_to_int(closure_obj, self.i64_t, "closure_addr_for_meta")
                        .map_err(|_| Diagnostic::simple("ptr_to_int failed"))?;
                    let closure_meta_addr = self
                        .builder
                        .build_int_add(closure_ptr_int, off_const, "closure_meta_addr")
                        .map_err(|_| Diagnostic::simple("int_add failed"))?;
                    let closure_meta_ptr = self
                        .builder
                        .build_int_to_ptr(closure_meta_addr, self.i8ptr_t, "closure_meta_ptr")
                        .map_err(|_| Diagnostic::simple("int_to_ptr failed"))?;
                    let _ = self
                        .builder
                        .build_store(closure_meta_ptr, closure_gv_i8.as_basic_value_enum());

                    // At this point, we'll return the closure object pointer (i8*) from the outer lowering
                    // after we finish constructing the arrow function. To keep the surrounding code path simple,
                    // create a pointer value representing the closure object and return it at the end of this arm.
                    // NOTE: we will still generate the arrow function body below which expects an env param.

                    // Insert a special local mapping in the outer function to indicate the closure value
                    // will be returned (handled below). We'll store the closure_obj into a temp alloca so
                    // it can be returned as BasicValueEnum.
                    let closure_ret = closure_obj.as_basic_value_enum();

                    // Lowering will continue to generate the arrow function body which expects an env param.
                    // After building the arrow function, we'll return `closure_ret` as the expression result.

                    // Save closure_ret in a temporary variable on the stack of the outer function so the
                    // code below (after function body generation) can return it. We'll use a simple trick:
                    // create an alloca, store the pointer, and later load it for the `Ok(...)` return.
                    let tmp_alloca = self
                        .builder
                        .build_alloca(self.i8ptr_t, "closure_tmp")
                        .map_err(|_| Diagnostic::simple("alloca failed"))?;
                    let _ = self.builder.build_store(tmp_alloca, closure_ret);
                    // remember where to load it later by adding an entry to locals stack
                    // mark initialized=true so emit_rc_dec_for_locals knows about it
                    if let Some(scope) = locals.last_mut() {
                        scope.insert(
                            "__closure_tmp".to_string(),
                            (
                                tmp_alloca,
                                self.i8ptr_t.as_basic_type_enum(),
                                true,
                                true,
                                false,
                                None,
                            ),
                        );
                    }
                    // Record the known return type for this closure temp so callers
                    // that load this local can emit statically-typed indirect calls.
                    self.closure_local_rettype
                        .borrow_mut()
                        .insert("__closure_tmp".to_string(), ret_type.clone());
                }

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
                        self.emit_rc_dec_for_locals(&arrow_locals);

                        let _ = self.builder.build_return(Some(&result));
                    } else {
                        return Err(Diagnostic::simple("Arrow body type mismatch"));
                    }
                }

                // Position back to original function
                if let Some(block) = current_block {
                    self.builder.position_at_end(block);
                }

                // If captures were present we previously stored a closure tmp in the outer locals
                if !captures.is_empty() {
                    // load tmp and return it
                    if let Some((tmp_ptr, _ty, init, _is_const, _is_weak, _nominal)) =
                        self.find_local(locals, "__closure_tmp")
                    {
                        if !init {
                            return Err(Diagnostic::simple("closure tmp uninitialized"));
                        }
                        let loaded =
                            match self
                                .builder
                                .build_load(self.i8ptr_t, tmp_ptr, "closure_load")
                            {
                                Ok(v) => v,
                                Err(_) => {
                                    return Err(Diagnostic::simple("failed to load closure tmp"));
                                }
                            };
                        return Ok(loaded);
                    } else {
                        return Err(Diagnostic::simple("closure tmp missing"));
                    }
                }

                // No captures: return the function pointer as a value
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
                                // Only identifier keys are supported currently.
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
                                } else if let Some((ptr, ty, _init, _is_const, _extra, _nominal)) =
                                    self.find_local(locals, &name)
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

                // Allocate object: layout = [header (8) | meta_slot (8) | fields...]
                // Reserve an 8-byte metadata slot after the header so collector
                // metadata can be stored at offset 8 when appropriate.
                let header_size = 8u64;
                let meta_slot = 8u64;
                let field_count = field_values.len();
                let total_size = header_size + meta_slot + (field_count as u64 * 8);

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

                // Store fields sequentially (fields start after header+meta_slot)
                for (idx, fv) in field_values.into_iter().enumerate() {
                    let offset = header_size + meta_slot + (idx as u64 * 8);
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

                    // If it's a float, store as raw f64 in the slot (no boxing).
                    if fv.get_type().is_float_type() {
                        // Cast slot pointer to f64* and store the float directly
                        let f64_slot_ptr = self
                            .builder
                            .build_pointer_cast(
                                field_ptr,
                                self.context.ptr_type(AddressSpace::default()),
                                "field_f64_slot",
                            )
                            .map_err(|_| Diagnostic::simple("pointer cast failed"))?;
                        // fv is a float value; store it directly
                        let _ = self.builder.build_store(f64_slot_ptr, fv);
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
                        // Treat integers as numbers (f64): convert to f64 and store inline
                        let intv = fv.into_int_value();
                        let fconv = self
                            .builder
                            .build_signed_int_to_float(intv, self.f64_t, "i_to_f")
                            .map_err(|_| Diagnostic::simple("int->float cast failed"))?;
                        let f64_slot_ptr = self
                            .builder
                            .build_pointer_cast(
                                field_ptr,
                                self.context.ptr_type(AddressSpace::default()),
                                "field_f64_slot",
                            )
                            .map_err(|_| Diagnostic::simple("pointer cast failed"))?;
                        let _ = self.builder.build_store(f64_slot_ptr, fconv);
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
                                .map_err(|_| {
                                    Diagnostic::simple_with_span(
                                        "failed to build call",
                                        tpl.span.lo.0 as usize,
                                    )
                                })?;
                            call_site
                                .try_as_basic_value()
                                .left()
                                .ok_or_else(|| {
                                    Diagnostic::simple_with_span(
                                        "num_to_str returned no value",
                                        tpl.span.lo.0 as usize,
                                    )
                                })?
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
                                .map_err(|_| {
                                    Diagnostic::simple_with_span(
                                        "failed to build select",
                                        tpl.span.lo.0 as usize,
                                    )
                                })?
                                .into_pointer_value()
                        } else {
                            return Err(Diagnostic::simple_with_span(
                                "unsupported value type in template literal",
                                tpl.span.lo.0 as usize,
                            ));
                        };

                        // Concatenate expression string with result
                        let left = result.ok_or_else(|| {
                            Diagnostic::simple_with_span(
                                "concat left operand missing",
                                tpl.span.lo.0 as usize,
                            )
                        })?;
                        let call_site = self
                            .builder
                            .build_call(
                                concat_fn,
                                &[left.into(), expr_str.into()],
                                "tpl_concat_expr",
                            )
                            .map_err(|_| {
                                Diagnostic::simple_with_span(
                                    "failed to build call",
                                    tpl.span.lo.0 as usize,
                                )
                            })?;
                        result = Some(
                            call_site
                                .try_as_basic_value()
                                .left()
                                .ok_or_else(|| {
                                    Diagnostic::simple_with_span(
                                        "concat call returned no value",
                                        tpl.span.lo.0 as usize,
                                    )
                                })?
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
                        let call_site =
                            self.builder
                                .build_call(disc_fn, &[pv.into()], "union_get_disc");
                        let cs = call_site.map_err(|_| {
                            Diagnostic::simple("failed to call union_get_discriminant")
                        })?;
                        if let inkwell::Either::Left(bv) = cs.try_as_basic_value() {
                            let disc = bv.into_int_value();
                            // Compare disc to constants: 0 -> number, 1 -> string, 2 -> boolean
                            let zero = self.i64_t.const_int(0, false);
                            let one = self.i64_t.const_int(1, false);
                            let two = self.i64_t.const_int(2, false);

                            let cmp_num = self
                                .builder
                                .build_int_compare(
                                    inkwell::IntPredicate::EQ,
                                    disc,
                                    zero,
                                    "disc_eq_num",
                                )
                                .map_err(|_| {
                                    Diagnostic::simple("failed to build int compare for typeof")
                                })?;
                            let cmp_str = self
                                .builder
                                .build_int_compare(
                                    inkwell::IntPredicate::EQ,
                                    disc,
                                    one,
                                    "disc_eq_str",
                                )
                                .map_err(|_| {
                                    Diagnostic::simple("failed to build int compare for typeof")
                                })?;
                            let cmp_bool = self
                                .builder
                                .build_int_compare(
                                    inkwell::IntPredicate::EQ,
                                    disc,
                                    two,
                                    "disc_eq_bool",
                                )
                                .map_err(|_| {
                                    Diagnostic::simple("failed to build int compare for typeof")
                                })?;

                            let s_num = self.intern_string_literal("number");
                            let s_str = self.intern_string_literal("string");
                            let s_bool = self.intern_string_literal("boolean");
                            let s_obj = self.intern_string_literal("object");

                            // Build nested selects: if num -> s_num else if str -> s_str else if bool -> s_bool else s_obj
                            let sel1 = self
                                .builder
                                .build_select(
                                    cmp_num,
                                    s_num.as_basic_value_enum(),
                                    s_str.as_basic_value_enum(),
                                    "sel_num_str",
                                )
                                .map_err(|_| {
                                    Diagnostic::simple("failed to build select for typeof")
                                })?;
                            let sel2 = self
                                .builder
                                .build_select(
                                    cmp_bool,
                                    s_bool.as_basic_value_enum(),
                                    s_obj.as_basic_value_enum(),
                                    "sel_bool_obj",
                                )
                                .map_err(|_| {
                                    Diagnostic::simple("failed to build select for typeof")
                                })?;
                            let final_sel = self
                                .builder
                                .build_select(cmp_str, sel1, sel2, "sel_final")
                                .map_err(|_| {
                                    Diagnostic::simple("failed to build select for typeof")
                                })?;
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

                // Only simple identifier updates are supported at present.
                if let ast::Expr::Ident(ident) = &*update.arg {
                    let name = ident.sym.to_string();

                    // Find the variable (parameter or local)
                    let var_entry = if param_map.contains_key(&name) {
                        // It's a parameter - we can't update parameters directly
                        // Need to create a local shadow
                        return Err(Diagnostic::simple(
                            "cannot update function parameter directly",
                        ));
                    } else if let Some((ptr, ty, initialized, is_const, _extra, _nominal)) =
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
