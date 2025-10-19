use crate::diagnostics::Diagnostic;
use inkwell::values::BasicValueEnum;
use inkwell::values::FunctionValue;
use std::collections::HashMap;

use inkwell::types::{BasicType, BasicTypeEnum};
use inkwell::values::{BasicValue, PointerValue};

// LocalEntry now includes an Option<String> for an optional nominal type name
// LocalEntry now includes an Option<OatsType> for union tracking
type LocalEntry<'a> = (
    PointerValue<'a>,
    BasicTypeEnum<'a>,
    bool,
    bool,
    bool,
    Option<String>,
    Option<crate::types::OatsType>,
);
type LocalsStackLocal<'a> = Vec<HashMap<String, LocalEntry<'a>>>;

impl<'a> crate::codegen::CodeGen<'a> {
    #[allow(clippy::result_large_err)]
    pub(super) fn lower_await_expr(
        &self,
        await_expr: &deno_ast::swc::ast::AwaitExpr,
        function: FunctionValue<'a>,
        param_map: &HashMap<String, u32>,
        locals: &mut LocalsStackLocal<'a>,
    ) -> crate::diagnostics::DiagnosticResult<BasicValueEnum<'a>> {
        // The full Await expression lowering code goes here
        // Phase 0+ improvement: call runtime `promise_poll_into(promise, out_ptr)`
        // and branch on the returned i32. If ready (1) load the resolved
        // value from out_ptr and return it. If pending (0) return the
        // original promise pointer (so callers still receive a pointer-like value).

        // If async lowering context is present, record that we've
        // emitted one await so the `gen_function_ir` wiring which
        // prepared resume/cont blocks and slot maps can correlate
        // to the lowered awaits. We'll record a 1-based resume index
        // for this await so we can store it into the state when
        // suspending.
        // Require async lowering context: awaiting outside an async
        // poll implementation is an error for Phase-1 lowering. Compute
        // a concrete resume index (1-based) to store into the state.
        let resume_idx: u32 = if self.async_poll_function.borrow().is_some() {
            let prev = self.async_await_counter.get();
            self.async_await_counter.set(prev.wrapping_add(1));
            prev.wrapping_add(1)
        } else {
            return Err(Diagnostic::simple_with_span_boxed(
                "`await` used outside of async lowering context",
                await_expr.span.lo.0 as usize,
            ));
        };

        let v = self.lower_expr(&await_expr.arg, function, param_map, locals)?;
        if let BasicValueEnum::PointerValue(pv) = v {
            // allocate an i8* out slot
            let out_alloca = match self
                .builder
                .build_alloca(self.i8ptr_t.as_basic_type_enum(), "await_out")
            {
                Ok(a) => a,
                Err(_) => {
                    return Err(Diagnostic::simple_boxed("alloca failed for await out slot"));
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
                Err(_) => return Err(Diagnostic::simple_boxed("promise_poll_into call failed")),
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
                Err(_) => return Err(Diagnostic::simple_boxed("int_compare failed")),
            };

            let ready_bb = self.context.append_basic_block(function, "await_ready");
            let pending_bb = self.context.append_basic_block(function, "await_pending");

            // Use the pre-created cont block for this await (cont_0, cont_1, etc.)
            let cont_bb = if let Some(cont_blocks) = &*self.async_cont_blocks.borrow() {
                cont_blocks[(resume_idx - 1) as usize]
            } else {
                // Fallback: create local cont block if no async context
                self.context.append_basic_block(function, "await_cont")
            };

            match self
                .builder
                .build_conditional_branch(is_ready, ready_bb, pending_bb)
            {
                Ok(_) => {}
                Err(_) => {
                    return Err(Diagnostic::simple_boxed("failed to build branch for await"));
                }
            }

            // ready: load out slot and branch to cont
            self.builder.position_at_end(ready_bb);
            let loaded = match self
                .builder
                .build_load(self.i8ptr_t, out_alloca, "await_loaded")
            {
                Ok(l) => l,
                Err(_) => return Err(Diagnostic::simple_boxed("await load failed")),
            };
            match self.builder.build_unconditional_branch(cont_bb) {
                Ok(_) => (),
                Err(_) => {
                    return Err(Diagnostic::simple_boxed("failed to branch to cont from ready"));
                }
            };

            // pending: suspend if async lowering context exists by
            // saving live locals into the heap state and setting the
            // resume index; otherwise fall back to Phase-0 behaviour
            // and continue returning the original promise pointer.
            self.builder.position_at_end(pending_bb);
            // We MUST have a resume index here because async lowering
            // context was required above. Emit save-to-state and set
            // the resume index, then return Pending (0).
            {
                // The poll function's state param is the first param
                // of the poll function. We need to write live locals
                // into the state slots and set the state field.
                // Here `function` is the current LLVM function (likely
                // the *wrapper*); but the poll function is available
                // in CodeGen.async_poll_function. Retrieve its param.
                if let Some(poll_f) = *self.async_poll_function.borrow() {
                    let state_param = match poll_f.get_nth_param(0) {
                        Some(param) => param,
                        None => {
                            return Err(Diagnostic::simple_boxed(
                                "missing state parameter in async poll function",
                            ));
                        }
                    };
                    let state_ptr = state_param.into_pointer_value();

                    // load live set and local map
                    if let Some(live_sets) = &*self.async_await_live_sets.borrow()
                        && let Some(local_map) = &*self.async_local_name_to_slot.borrow()
                        && let Some(live_set) = live_sets.get((resume_idx - 1) as usize)
                    {
                        for name in live_set.iter() {
                            if let Some(slot_idx) = local_map.get(name)
                                && let Some((
                                    alloca_ptr,
                                    ty,
                                    _init,
                                    _is_const,
                                    _is_weak,
                                    _nominal,
                                    _oats_type,
                                )) = self.find_local(locals, name)
                            {
                                let loaded = match self.builder.build_load(
                                    ty,
                                    alloca_ptr,
                                    &format!("save_{}", name),
                                ) {
                                    Ok(v) => v,
                                    Err(_) => {
                                        return Err(Diagnostic::simple_boxed(
                                            "failed to load local for save",
                                        ));
                                    }
                                };

                                // compute slot address: state_base + (16 + slot_idx*8)
                                let base_int = match self.builder.build_ptr_to_int(
                                    state_ptr,
                                    self.i64_t,
                                    "state_addr",
                                ) {
                                    Ok(v) => v,
                                    Err(_) => {
                                        return Err(Diagnostic::simple_boxed(
                                            "ptr_to_int failed when saving locals",
                                        ));
                                    }
                                };
                                let off_const =
                                    self.i64_t.const_int(16 + (*slot_idx as u64 * 8), false);
                                let slot_addr_int = match self.builder.build_int_add(
                                    base_int,
                                    off_const,
                                    "slot_addr",
                                ) {
                                    Ok(v) => v,
                                    Err(_) => {
                                        return Err(Diagnostic::simple_boxed(
                                            "int_add failed when saving locals",
                                        ));
                                    }
                                };
                                let slot_ptr = match self.builder.build_int_to_ptr(
                                    slot_addr_int,
                                    self.i8ptr_t,
                                    "slot_ptr_save",
                                ) {
                                    Ok(p) => p,
                                    Err(_) => {
                                        return Err(Diagnostic::simple_boxed(
                                            "int_to_ptr failed when saving locals",
                                        ));
                                    }
                                };

                                match ty {
                                    inkwell::types::BasicTypeEnum::FloatType(_) => {
                                        let box_fn = self.get_union_box_f64();
                                        let cs = match self.builder.build_call(
                                            box_fn,
                                            &[loaded.into()],
                                            "box_save",
                                        ) {
                                            Ok(cs) => cs,
                                            Err(_) => {
                                                return Err(Diagnostic::simple_boxed(
                                                    "union_box_f64 call failed when saving locals",
                                                ));
                                            }
                                        };
                                        let boxed_ptr = cs
                                            .try_as_basic_value()
                                            .left()
                                            .ok_or_else(|| {
                                                Diagnostic::simple(
                                                    "boxing returned no value when saving locals",
                                                )
                                            })?
                                            .into_pointer_value();
                                        let _ = self
                                            .builder
                                            .build_store(slot_ptr, boxed_ptr.as_basic_value_enum());
                                    }
                                    _ => {
                                        let _ = self.builder.build_store(slot_ptr, loaded);
                                    }
                                }
                            }
                        }
                    }

                    // Save the awaited promise into its slot so we can re-poll on resume
                    let param_count = self.async_param_count.get() as u64;
                    let local_slot_count = self.async_local_slot_count.get() as u64;
                    let await_slot_offset = 16
                        + (param_count * 8)
                        + (local_slot_count * 8)
                        + ((resume_idx as u64 - 1) * 8);
                    let base_int = match self.builder.build_ptr_to_int(
                        state_ptr,
                        self.i64_t,
                        "await_promise_state_addr",
                    ) {
                        Ok(v) => v,
                        Err(_) => {
                            return Err(Diagnostic::simple_boxed(
                                "ptr_to_int failed when saving awaited promise",
                            ));
                        }
                    };
                    let off_const = self.i64_t.const_int(await_slot_offset, false);
                    let slot_addr_int = match self.builder.build_int_add(
                        base_int,
                        off_const,
                        "await_promise_slot_addr",
                    ) {
                        Ok(v) => v,
                        Err(_) => {
                            return Err(Diagnostic::simple_boxed(
                                "int_add failed when saving awaited promise",
                            ));
                        }
                    };
                    let slot_ptr = match self.builder.build_int_to_ptr(
                        slot_addr_int,
                        self.i8ptr_t,
                        "await_promise_slot_ptr",
                    ) {
                        Ok(p) => p,
                        Err(_) => {
                            return Err(Diagnostic::simple_boxed(
                                "int_to_ptr failed when saving awaited promise",
                            ));
                        }
                    };
                    // Store the promise pointer
                    let _ = self.builder.build_store(slot_ptr, pv.as_basic_value_enum());

                    // store resume index into state field at offset 8
                    let base_int = match self.builder.build_ptr_to_int(
                        state_ptr,
                        self.i64_t,
                        "state_addr_store",
                    ) {
                        Ok(v) => v,
                        Err(_) => {
                            return Err(Diagnostic::simple_boxed(
                                "ptr_to_int failed when storing state index",
                            ));
                        }
                    };
                    let state_off = self.i64_t.const_int(8, false);
                    let state_field_int = match self.builder.build_int_add(
                        base_int,
                        state_off,
                        "state_field_addr_store",
                    ) {
                        Ok(v) => v,
                        Err(_) => {
                            return Err(Diagnostic::simple_boxed(
                                "int_add failed when storing state index",
                            ));
                        }
                    };
                    let state_field_ptr = match self.builder.build_int_to_ptr(
                        state_field_int,
                        self.context.ptr_type(inkwell::AddressSpace::default()),
                        "state_field_ptr_store",
                    ) {
                        Ok(p) => p,
                        Err(_) => {
                            return Err(Diagnostic::simple_boxed(
                                "int_to_ptr failed when storing state index",
                            ));
                        }
                    };
                    let resume_val = self.i32_t.const_int(resume_idx as u64, false);
                    let _ = self.builder.build_store(state_field_ptr, resume_val);

                    // return 0 (pending)
                    let zero = self.i32_t.const_int(0, false);
                    let _ = self.builder.build_return(Some(&zero.as_basic_value_enum()));
                }
            }

            // cont: create phi to select between loaded value (from ready path)
            // and resumed value (which will be the original promise pointer)
            self.builder.position_at_end(cont_bb);
            let phi_ty = self.i8ptr_t.as_basic_type_enum();
            let phi = match self.builder.build_phi(phi_ty, "await_phi") {
                Ok(p) => p,
                Err(_) => return Err(Diagnostic::simple_boxed("failed to create phi for await")),
            };
            // Only add incoming from ready_bb - when we resume, the resume block
            // will provide the value by branching here with the promise pointer
            // Note: pending_bb returns early, so it's not a predecessor
            phi.add_incoming(&[(&loaded.as_basic_value_enum(), ready_bb)]);

            // If we're in async context, the resume block will also branch here
            // We need to add the promise pointer as an incoming value from resume path
            // The resume path will have the original promise available
            // For now, just return the phi - the resume block will handle providing the value
            return Ok(phi.as_basic_value());
        }
        // Non-pointer awaited values are returned as-is (numbers/booleans)
        Ok(v)
    }
}
