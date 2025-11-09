//! Host main function generation.
//!
//! This module handles emitting a C-compatible `main` function that calls
//! the generated `oats_main` symbol. This allows Oats programs to be
//! compiled as standalone executables.

use crate::codegen::CodeGen;
use crate::diagnostics::{Diagnostic, Severity};

impl<'a> CodeGen<'a> {
    /// Emit a simple C-compatible `main` function that calls the generated `oats_main` symbol.
    ///
    /// This covers the common case where user scripts export `function main(): number` with no params.
    /// Returns `true` to indicate we emitted a host main so the driver will skip linking an external `rt_main.o`.
    pub fn emit_host_main(
        &self,
        _params: &[crate::types::OatsType],
        _ret: &crate::types::OatsType,
    ) -> bool {
        // Only handle the simple case: no params, integer return or void.
        // Build: int main(int argc, char** argv) { int r = oats_main(); return r; }
        let i32_t = self.i32_t;

        // Build function type: i32 (int) with (i32, i8**) params
        let argv_type = self.context.ptr_type(inkwell::AddressSpace::default());
        let fn_type = i32_t.fn_type(&[i32_t.into(), argv_type.into()], false);
        let main_fn = self.module.add_function("main", fn_type, None);
        let entry = self.context.append_basic_block(main_fn, "entry");
        self.builder.position_at_end(entry);

        // Try to look up oats_main; it should be present if gen_function_ir emitted it.
        let oats_main_fn = match self
            .module
            .get_function(crate::runtime_functions::names::OATS_MAIN)
        {
            Some(f) => f,
            None => {
                // No oats_main available; emit an empty main that returns 1
                let const_one = i32_t.const_int(1, false);
                let _ = self.builder.build_return(Some(&const_one));
                return true;
            }
        };

        // Call oats_main(); we only support a no-arg oats_main here.
        let call_site = match self.builder.build_call(oats_main_fn, &[], "call_oats_main") {
            Ok(cs) => cs,
            Err(_) => {
                crate::diagnostics::emit_diagnostic(
                    &Diagnostic::simple_boxed(Severity::Error, "failed to build call to oats_main"),
                    Some(self.source),
                );
                let const_zero = i32_t.const_int(0, false);
                let _ = self.builder.build_return(Some(&const_zero));
                return true;
            }
        };

        // Run the async executor to process any enqueued promises
        if self.uses_async.get() {
            let executor_run_fn = self.get_executor_run();
            let _ = self
                .builder
                .build_call(executor_run_fn, &[], "call_executor");
        }

        // Interpret the result depending on its type. If the function returns an i64 or f64
        // we coerce/truncate to i32; if void, return 0.
        let either = call_site.try_as_basic_value();
        if let inkwell::Either::Left(bv) = either {
            let ret_val = self.coerce_return_value_to_i32(bv, i32_t);
            let exit_code = self.compute_exit_code(ret_val, i32_t);
            let _ = self.builder.build_return(Some(&exit_code));
        } else {
            // No basic return (void), return 0
            let const_zero = i32_t.const_int(0, false);
            let _ = self.builder.build_return(Some(&const_zero));
        }

        true
    }

    fn coerce_return_value_to_i32(
        &self,
        bv: inkwell::values::BasicValueEnum<'a>,
        i32_t: inkwell::types::IntType<'a>,
    ) -> inkwell::values::IntValue<'a> {
        if bv.get_type().is_int_type() {
            // Truncate or bitcast to i32 if needed
            let rv_int = bv.into_int_value();
            match self
                .builder
                .build_int_truncate_or_bit_cast(rv_int, i32_t, "ret_i32")
            {
                Ok(c) => c,
                Err(_) => {
                    crate::diagnostics::emit_diagnostic(
                        &Diagnostic::simple_boxed(
                            Severity::Error,
                            "int cast failed when building host main",
                        ),
                        Some(self.source),
                    );
                    i32_t.const_int(0, false)
                }
            }
        } else if bv.get_type().is_float_type() {
            // cast float to i32 via fptosi
            let fv = bv.into_float_value();
            match self.builder.build_float_to_signed_int(fv, i32_t, "f_to_i") {
                Ok(c) => c,
                Err(_) => {
                    crate::diagnostics::emit_diagnostic(
                        &Diagnostic::simple_boxed(
                            Severity::Error,
                            "float->int conversion failed in host main",
                        ),
                        Some(self.source),
                    );
                    i32_t.const_int(0, false)
                }
            }
        } else {
            // pointer return -> return 0
            i32_t.const_int(0, false)
        }
    }

    fn compute_exit_code(
        &self,
        ret_int_val: inkwell::values::IntValue<'a>,
        i32_t: inkwell::types::IntType<'a>,
    ) -> inkwell::values::IntValue<'a> {
        // Convert to proper exit code: 0 for success, 1 for failure
        let zero = i32_t.const_int(0, false);
        let one = i32_t.const_int(1, false);
        let is_success = match self.builder.build_int_compare(
            inkwell::IntPredicate::EQ,
            ret_int_val,
            zero,
            "is_success",
        ) {
            Ok(cmp) => cmp,
            Err(_) => {
                crate::diagnostics::emit_diagnostic(
                    &Diagnostic::simple_boxed(
                        Severity::Error,
                        "failed to build int compare for exit code",
                    ),
                    Some(self.source),
                );
                zero
            }
        };
        match self
            .builder
            .build_select(is_success, zero, one, "exit_code")
        {
            Ok(sel) => sel.into_int_value(),
            Err(_) => {
                crate::diagnostics::emit_diagnostic(
                    &Diagnostic::simple_boxed(
                        Severity::Error,
                        "failed to build select for exit code",
                    ),
                    Some(self.source),
                );
                zero
            }
        }
    }
}
