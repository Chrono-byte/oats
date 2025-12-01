//! Yield expression lowering
//!
//! This module handles yield expressions (`yield expr` and `yield* expr`).
//! Yield expressions are used in generator functions to produce values.

use crate::codegen::CodeGen;
use crate::diagnostics::Diagnostic;
use inkwell::values::{BasicValue, BasicValueEnum, FunctionValue};
use oats_ast::*;
use std::collections::HashMap;

type LocalEntry<'a> = (
    inkwell::values::PointerValue<'a>,
    inkwell::types::BasicTypeEnum<'a>,
    bool,
    bool,
    bool,
    Option<String>,
    Option<crate::types::OatsType>,
);
type LocalsStackLocal<'a> = Vec<std::collections::HashMap<String, LocalEntry<'a>>>;

impl<'a> CodeGen<'a> {
    /// Lowers a yield expression.
    ///
    /// Yield expressions are used in generator functions to produce values.
    /// `yield expr` yields a single value, while `yield* expr` delegates to another generator.
    ///
    /// Yield expressions suspend the generator, save live locals to state, and return
    /// control to the caller. When resumed, execution continues after the yield.
    #[allow(clippy::result_large_err)]
    pub(super) fn lower_yield_expr(
        &self,
        yield_expr: &YieldExpr,
        function: FunctionValue<'a>,
        param_map: &HashMap<String, u32>,
        locals: &mut LocalsStackLocal<'a>,
    ) -> crate::diagnostics::DiagnosticResult<BasicValueEnum<'a>> {
        use crate::diagnostics::Severity;

        // Require generator lowering context: yielding outside a generator
        // next implementation is an error. Compute a concrete resume index (1-based)
        // to store into the state.
        let yield_idx: u32 = if self.generator_next_function.borrow().is_some() {
            let prev = self.generator_yield_counter.get();
            self.generator_yield_counter.set(prev.wrapping_add(1));
            prev.wrapping_add(1)
        } else {
            return Err(Diagnostic::simple_with_span_boxed(
                Severity::Error,
                "`yield` used outside of generator function",
                yield_expr.span.start,
            ));
        };

        // Handle yield* (delegate to another generator)
        if yield_expr.delegate {
            return self.lower_yield_star_expr(yield_expr, function, param_map, locals, yield_idx);
        }

        // Lower the yielded expression
        let yielded_val = if let Some(arg) = &yield_expr.arg {
            self.lower_expr(arg, function, param_map, locals)?
        } else {
            // yield without argument yields undefined/null
            self.i8ptr_t.const_null().as_basic_value_enum()
        };

        // Get the out parameter (where to store the yielded value)
        let out_param = function.get_nth_param(1).ok_or_else(|| {
            Diagnostic::simple_with_span_boxed(
                Severity::Error,
                "generator next function missing out parameter",
                yield_expr.span.start,
            )
        })?;
        let out_ptr = out_param.into_pointer_value();

        // Store yielded value to out parameter
        // Box the value if needed (similar to async await)
        let boxed_val = match yielded_val {
            BasicValueEnum::FloatValue(fv) => {
                // Box f64 into union
                let box_fn = self.get_union_box_f64();
                let cs = self
                    .builder
                    .build_call(box_fn, &[fv.into()], "box_yield_f64")
                    .map_err(|_| Diagnostic::error("box call failed"))?;
                if let inkwell::Either::Left(bv) = cs.try_as_basic_value() {
                    bv
                } else {
                    return Err(Diagnostic::simple_boxed(
                        crate::diagnostics::Severity::Error,
                        "box returned void",
                    ));
                }
            }
            BasicValueEnum::PointerValue(pv) => {
                // Already a pointer, increment refcount
                let rc_inc = self.get_rc_inc();
                let _ = self
                    .builder
                    .build_call(rc_inc, &[pv.into()], "rc_inc_yield")
                    .map_err(|_| Diagnostic::error("rc_inc failed"))?;
                pv.as_basic_value_enum()
            }
            _ => {
                // Other types - box as pointer if needed
                yielded_val
            }
        };

        // Store to out parameter
        let out_ptr_ty = self.context.ptr_type(inkwell::AddressSpace::default());
        let out_slot = self
            .builder
            .build_pointer_cast(out_ptr, out_ptr_ty, "out_slot")
            .map_err(|_| Diagnostic::error("pointer cast failed"))?;
        let _ = self
            .builder
            .build_store(out_slot, boxed_val)
            .map_err(|_| Diagnostic::error("store failed"))?;

        // Get state parameter
        let state_param = function.get_nth_param(0).ok_or_else(|| {
            Diagnostic::simple_with_span_boxed(
                Severity::Error,
                "generator next function missing state parameter",
                yield_expr.span.start,
            )
        })?;
        let state_ptr = state_param.into_pointer_value();

        // Save live locals to state (similar to async await)
        if let Some(live_sets) = self.generator_yield_live_sets.borrow().as_ref()
            && (yield_idx as usize) <= live_sets.len() {
                let live_set = &live_sets[(yield_idx - 1) as usize];
                if let Some(local_name_to_slot) =
                    self.generator_local_name_to_slot.borrow().as_ref()
                {
                if let Some(local_name_to_slot) =
                    self.generator_local_name_to_slot.borrow().as_ref()
                {
                    for live_name in live_set {
                        if let Some(slot_idx) = local_name_to_slot.get(live_name) {
                            // Find the local in the locals stack
                            if let Some((
                                local_ptr,
                                local_ty,
                                _init,
                                _is_const,
                                _is_weak,
                                _nominal,
                                _oats_type,
                            )) = self.find_local(locals, live_name)
                            if let Some((
                                local_ptr,
                                local_ty,
                                _init,
                                _is_const,
                                _is_weak,
                                _nominal,
                                _oats_type,
                            )) = self.find_local(locals, live_name)
                            {
                                // Load local value
                                let local_val = self
                                    .builder
                                    .build_load(local_ty, local_ptr, &format!("live_{}", live_name))
                                    .map_err(|_| Diagnostic::error("load local failed"))?;

                                // Compute slot offset in state
                                let slot_offset = 16 + (*slot_idx * 8) as u64; // header(8) + state(4) + pad(4) = 16
                                let offset_const = self.i64_t.const_int(slot_offset, false);
                                let slot_i8ptr = unsafe {
                                    self.builder.build_gep(
                                        self.i8_t,
                                        state_ptr,
                                        &[offset_const],
                                        &format!("slot_{}_i8ptr", live_name),
                                    )
                                }
                                .map_err(|_| Diagnostic::error("gep failed"))?;

                                // Store to state slot
                                let slot_ptr_ty =
                                    self.context.ptr_type(inkwell::AddressSpace::default());
                                let slot_ptr_ty =
                                    self.context.ptr_type(inkwell::AddressSpace::default());
                                let slot_ptr = self
                                    .builder
                                    .build_pointer_cast(slot_i8ptr, slot_ptr_ty, "slot_ptr")
                                    .map_err(|_| Diagnostic::error("pointer cast failed"))?;

                                match local_ty {
                                    inkwell::types::BasicTypeEnum::FloatType(_ft) => {
                                        let f64_ptr = self
                                            .builder
                                            .build_pointer_cast(
                                                slot_ptr,
                                                self.context
                                                    .ptr_type(inkwell::AddressSpace::default()),
                                                self.context
                                                    .ptr_type(inkwell::AddressSpace::default()),
                                                "f64_ptr",
                                            )
                                            .map_err(|_| {
                                                Diagnostic::error("f64 ptr cast failed")
                                            })?;
                                            .map_err(|_| {
                                                Diagnostic::error("f64 ptr cast failed")
                                            })?;
                                        let _ = self
                                            .builder
                                            .build_store(f64_ptr, local_val)
                                            .map_err(|_| Diagnostic::error("store failed"))?;
                                    }
                                    _ => {
                                        // Pointer types
                                        let ptr_ptr = self
                                            .builder
                                            .build_pointer_cast(
                                                slot_ptr,
                                                self.context
                                                    .ptr_type(inkwell::AddressSpace::default()),
                                                self.context
                                                    .ptr_type(inkwell::AddressSpace::default()),
                                                "ptr_ptr",
                                            )
                                            .map_err(|_| Diagnostic::error("ptr cast failed"))?;
                                        let _ = self
                                            .builder
                                            .build_store(ptr_ptr, local_val)
                                            .map_err(|_| Diagnostic::error("store failed"))?;
                                    }
                                }
                            }
                        }
                    }
                }
            }

        // Store resume index to state field (offset 8)
        let state_addr_int = self
            .builder
            .build_ptr_to_int(state_ptr, self.i64_t, "state_addr")
            .map_err(|_| Diagnostic::error("ptr_to_int failed"))?;
        let state_off = self.i64_t.const_int(8, false);
        let state_field_int = self
            .builder
            .build_int_add(state_addr_int, state_off, "state_field_addr")
            .map_err(|_| Diagnostic::error("int_add failed"))?;
        let state_field_ptr = self
            .builder
            .build_int_to_ptr(
                state_field_int,
                self.context.ptr_type(inkwell::AddressSpace::default()),
                "state_field_ptr",
            )
            .map_err(|_| Diagnostic::error("int_to_ptr failed"))?;
        let resume_val = self.i32_t.const_int(yield_idx as u64, false);
        let _ = self
            .builder
            .build_store(state_field_ptr, resume_val)
            .map_err(|_| Diagnostic::error("store state failed"))?;

        // Store resume index to state field (offset 8) - this was already done above
        // but we need to ensure it happens after saving locals

        // Return 1 (yielded value available)
        // The generator is suspended here. When next() is called again,
        // it will resume at the resume block which restores state and
        // branches to the continuation block.
        let one = self.i32_t.const_int(1, false);
        let _ = self
            .builder
            .build_return(Some(&one.as_basic_value_enum()))
            .map_err(|_| Diagnostic::error("return failed"))?;

        // This point is unreachable (we returned above)
        // The continuation block will be used when the generator resumes
        // Return a dummy value for type checking
        Ok(self.i8ptr_t.const_null().as_basic_value_enum())
    }

    /// Lowers a yield* expression (delegating yield).
    ///
    /// `yield* gen()` delegates to another generator, forwarding all its yielded values.
    /// The delegated generator's state is stored in the current generator's state,
    /// and we loop calling its next function until it's done.
    #[allow(clippy::result_large_err)]
    fn lower_yield_star_expr(
        &self,
        yield_expr: &YieldExpr,
        function: FunctionValue<'a>,
        param_map: &HashMap<String, u32>,
        locals: &mut LocalsStackLocal<'a>,
        yield_idx: u32,
    ) -> crate::diagnostics::DiagnosticResult<BasicValueEnum<'a>> {
        use crate::diagnostics::Severity;

        // Lower the expression to get the delegated generator object (state pointer)
        let delegated_gen = if let Some(arg) = &yield_expr.arg {
            self.lower_expr(arg, function, param_map, locals)?
        } else {
            return Err(Diagnostic::simple_with_span_boxed(
                Severity::Error,
                "yield* requires a generator expression",
                yield_expr.span.start,
            ));
        };

        let delegated_gen_ptr = match delegated_gen {
            BasicValueEnum::PointerValue(pv) => pv,
            _ => {
                return Err(Diagnostic::simple_with_span_boxed(
                    Severity::Error,
                    "yield* requires a generator object (pointer type)",
                    yield_expr.span.start,
                ));
            }
        };

        // Get state parameter and out parameter
        let state_param = function.get_nth_param(0).ok_or_else(|| {
            Diagnostic::simple_with_span_boxed(
                Severity::Error,
                "generator next function missing state parameter",
                yield_expr.span.start,
            )
        })?;
        let state_ptr = state_param.into_pointer_value();

        let out_param = function.get_nth_param(1).ok_or_else(|| {
            Diagnostic::simple_with_span_boxed(
                Severity::Error,
                "generator next function missing out parameter",
                yield_expr.span.start,
            )
        })?;
        let out_ptr = out_param.into_pointer_value();

        // Extract the next function pointer from the delegated generator state (offset 0)
        let next_fn_ptr_ty = self.context.ptr_type(inkwell::AddressSpace::default());
        let next_fn_ptr = self
            .builder
            .build_pointer_cast(delegated_gen_ptr, next_fn_ptr_ty, "next_fn_ptr_cast")
            .map_err(|_| Diagnostic::error("pointer cast failed"))?;
        let next_fn_ptr_val = self
            .builder
            .build_load(self.i8ptr_t, next_fn_ptr, "load_next_fn")
            .map_err(|_| Diagnostic::error("load next fn failed"))?;

        // Cast the function pointer to the correct function type for indirect call
        // next function signature: fn(state: i8*, out: i8*) -> i32
        let next_fn_type = self
            .i32_t
            .fn_type(&[self.i8ptr_t.into(), self.i8ptr_t.into()], false);
        let fn_ptr_ty = self.context.ptr_type(inkwell::AddressSpace::default());
        let next_fn_ptr_cast = self
            .builder
            .build_pointer_cast(
                next_fn_ptr_val.into_pointer_value(),
                fn_ptr_ty,
                "next_fn_ptr_cast",
            )
            .map_err(|_| {
                Diagnostic::simple_boxed(crate::diagnostics::Severity::Error, "cast next fn failed")
            })?;
            .map_err(|_| {
                Diagnostic::simple_boxed(crate::diagnostics::Severity::Error, "cast next fn failed")
            })?;

        // Create a loop that calls the delegated generator's next function
        // Loop structure:
        //   loop_start:
        //     result = next_fn(delegated_gen_ptr, out_ptr)
        //     if result == 0: break (generator done)
        //     if result == 1: yield (save state and return)
        //     goto loop_start
        //   loop_end:
        //     continue execution

        let loop_start_bb = self
            .context
            .append_basic_block(function, "yield_star_loop_start");
        let loop_body_bb = self
            .context
            .append_basic_block(function, "yield_star_loop_body");
        let yield_bb = self
            .context
            .append_basic_block(function, "yield_star_yield");
        let loop_end_bb = self
            .context
            .append_basic_block(function, "yield_star_loop_end");

        // Branch to loop start
        let _ = self
            .builder
            .build_unconditional_branch(loop_start_bb)
            .map_err(|_| {
                Diagnostic::simple_boxed(crate::diagnostics::Severity::Error, "branch failed")
            })?;
            .map_err(|_| {
                Diagnostic::simple_boxed(crate::diagnostics::Severity::Error, "branch failed")
            })?;

        // Loop start: call delegated generator's next function (indirect call)
        self.builder.position_at_end(loop_start_bb);
        let call_result = self
            .builder
            .build_indirect_call(
                next_fn_type,
                next_fn_ptr_cast,
                &[delegated_gen_ptr.into(), out_ptr.into()],
                "delegated_next_call",
            )
            .map_err(|_| {
                Diagnostic::simple_boxed(
                    crate::diagnostics::Severity::Error,
                    "indirect call failed",
                )
            })?;
            .map_err(|_| {
                Diagnostic::simple_boxed(
                    crate::diagnostics::Severity::Error,
                    "indirect call failed",
                )
            })?;

        let result_val = if let inkwell::Either::Left(bv) = call_result.try_as_basic_value() {
            if let BasicValueEnum::IntValue(iv) = bv {
                iv
            } else {
                return Err(Diagnostic::simple_boxed(
                    crate::diagnostics::Severity::Error,
                    "next returned non-integer",
                ));
            }
        } else {
            return Err(Diagnostic::simple_boxed(
                crate::diagnostics::Severity::Error,
                "next returned void",
            ));
        };

        // Check result: 0 = done, 1 = yielded value
        let zero = self.i32_t.const_int(0, false);
        let one = self.i32_t.const_int(1, false);
        let is_done = self
            .builder
            .build_int_compare(inkwell::IntPredicate::EQ, result_val, zero, "is_done")
            .build_int_compare(inkwell::IntPredicate::EQ, result_val, zero, "is_done")
            .map_err(|_| Diagnostic::error("compare failed"))?;
        let is_yielded = self
            .builder
            .build_int_compare(inkwell::IntPredicate::EQ, result_val, one, "is_yielded")
            .build_int_compare(inkwell::IntPredicate::EQ, result_val, one, "is_yielded")
            .map_err(|_| Diagnostic::error("compare failed"))?;

        // Branch: if done, go to loop_end; if yielded, go to yield_bb; else continue loop
        let _ = self
            .builder
            .build_conditional_branch(is_done, loop_end_bb, loop_body_bb)
            .map_err(|_| Diagnostic::error("branch failed"))?;

        // Loop body: check if yielded
        self.builder.position_at_end(loop_body_bb);
        let _ = self
            .builder
            .build_conditional_branch(is_yielded, yield_bb, loop_start_bb)
            .map_err(|_| Diagnostic::error("branch failed"))?;

        // Yield block: save state and return
        self.builder.position_at_end(yield_bb);

        // Save live locals to state (same as regular yield)
        if let Some(live_sets) = self.generator_yield_live_sets.borrow().as_ref()
            && (yield_idx as usize) <= live_sets.len() {
                let live_set = &live_sets[(yield_idx - 1) as usize];
                if let Some(local_name_to_slot) =
                    self.generator_local_name_to_slot.borrow().as_ref()
                {
                if let Some(local_name_to_slot) =
                    self.generator_local_name_to_slot.borrow().as_ref()
                {
                    for live_name in live_set {
                        if let Some(slot_idx) = local_name_to_slot.get(live_name)
                            && let Some((
                                local_ptr,
                                local_ty,
                                _init,
                                _is_const,
                                _is_weak,
                                _nominal,
                                _oats_type,
                            )) = self.find_local(locals, live_name)
                            {
                                let local_val = self
                                    .builder
                                    .build_load(local_ty, local_ptr, &format!("live_{}", live_name))
                                    .map_err(|_| Diagnostic::error("load local failed"))?;

                                let slot_offset = 16 + (*slot_idx * 8) as u64;
                                let offset_const = self.i64_t.const_int(slot_offset, false);
                                let slot_i8ptr = unsafe {
                                    self.builder.build_gep(
                                        self.i8_t,
                                        state_ptr,
                                        &[offset_const],
                                        &format!("slot_{}_i8ptr", live_name),
                                    )
                                }
                                .map_err(|_| Diagnostic::error("gep failed"))?;

                                let slot_ptr_ty =
                                    self.context.ptr_type(inkwell::AddressSpace::default());
                                let slot_ptr_ty =
                                    self.context.ptr_type(inkwell::AddressSpace::default());
                                let slot_ptr = self
                                    .builder
                                    .build_pointer_cast(slot_i8ptr, slot_ptr_ty, "slot_ptr")
                                    .map_err(|_| Diagnostic::error("pointer cast failed"))?;

                                match local_ty {
                                    inkwell::types::BasicTypeEnum::FloatType(_ft) => {
                                        let f64_ptr = self
                                            .builder
                                            .build_pointer_cast(
                                                slot_ptr,
                                                self.context
                                                    .ptr_type(inkwell::AddressSpace::default()),
                                                self.context
                                                    .ptr_type(inkwell::AddressSpace::default()),
                                                "f64_ptr",
                                            )
                                            .map_err(|_| {
                                                Diagnostic::error("f64 ptr cast failed")
                                            })?;
                                            .map_err(|_| {
                                                Diagnostic::error("f64 ptr cast failed")
                                            })?;
                                        let _ = self
                                            .builder
                                            .build_store(f64_ptr, local_val)
                                            .map_err(|_| Diagnostic::error("store failed"))?;
                                    }
                                    _ => {
                                        let ptr_ptr = self
                                            .builder
                                            .build_pointer_cast(
                                                slot_ptr,
                                                self.context
                                                    .ptr_type(inkwell::AddressSpace::default()),
                                                self.context
                                                    .ptr_type(inkwell::AddressSpace::default()),
                                                "ptr_ptr",
                                            )
                                            .map_err(|_| Diagnostic::error("ptr cast failed"))?;
                                        let _ = self
                                            .builder
                                            .build_store(ptr_ptr, local_val)
                                            .map_err(|_| Diagnostic::error("store failed"))?;
                                    }
                                }
                            }
                    }
                }
            }

        // Store resume index to state field
        let state_addr_int = self
            .builder
            .build_ptr_to_int(state_ptr, self.i64_t, "state_addr")
            .map_err(|_| Diagnostic::error("ptr_to_int failed"))?;
        let state_off = self.i64_t.const_int(8, false);
        let state_field_int = self
            .builder
            .build_int_add(state_addr_int, state_off, "state_field_addr")
            .map_err(|_| Diagnostic::error("int_add failed"))?;
        let state_field_ptr = self
            .builder
            .build_int_to_ptr(
                state_field_int,
                self.context.ptr_type(inkwell::AddressSpace::default()),
                "state_field_ptr",
            )
            .map_err(|_| Diagnostic::error("int_to_ptr failed"))?;
        let resume_val = self.i32_t.const_int(yield_idx as u64, false);
        let _ = self
            .builder
            .build_store(state_field_ptr, resume_val)
            .map_err(|_| Diagnostic::error("store state failed"))?;

        // Return 1 (yielded value available in out parameter)
        let one = self.i32_t.const_int(1, false);
        let _ = self
            .builder
            .build_return(Some(&one.as_basic_value_enum()))
            .map_err(|_| Diagnostic::error("return failed"))?;

        // Loop end: delegated generator is done, continue execution
        self.builder.position_at_end(loop_end_bb);

        // Return a dummy value (execution will continue in continuation block)
        Ok(self.i8ptr_t.const_null().as_basic_value_enum())
    }
}
