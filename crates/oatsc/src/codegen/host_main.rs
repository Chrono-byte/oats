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
        // Build function type: i32 with (i32, i8**) params
        let i32_t = self.context.i32_type();
        let argv_type = self.context.ptr_type(inkwell::AddressSpace::default());
        let main_fn = self.module.add_function(
            "main",
            i32_t.fn_type(&[i32_t.into(), argv_type.into()], false),
            None,
        );
        let entry = self.context.append_basic_block(main_fn, "entry");
        self.builder.position_at_end(entry);

        // Call oats_main (user defined or generated main function)
        let oats_main = match self.module.get_function("oats_main") {
            Some(f) => f,
            None => {
                crate::diagnostics::emit_diagnostic(
                    &Diagnostic::simple_boxed(
                        Severity::Error,
                        "oats_main symbol should be generated before host main",
                    ),
                    Some(self.source),
                );
                // Do not emit main if internal error
                return false;
            }
        };

        let call_result = self
            .builder
            .build_call(oats_main, &[], "call_oats_main")
            .inspect_err(|_e| {
                crate::diagnostics::emit_diagnostic(
                    &Diagnostic::simple_boxed(Severity::Error, "failed to build call to oats_main"),
                    Some(self.source),
                );
            });

        match call_result {
            Ok(call_ret) => {
                // Try to extract the return value (number/void)
                let either = call_ret.try_as_basic_value();
                if let inkwell::values::ValueKind::Basic(bv) = either {
                    // Always coerce to i32 for program exit code
                    let exit_code = self.coerce_return_value_to_i32(bv, i32_t);
                    let _ = self.builder.build_return(Some(&exit_code));
                } else {
                    // oats_main returns void? Return exit code 0
                    let _ = self.builder.build_return(Some(&i32_t.const_int(0, false)));
                }
                true
            }
            Err(_) => {
                // Already emitted a diagnostic above
                false
            }
        }
    }

    /// Coerce any return value from oats_main to i32 for use as exit code.
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
            // pointer or unknown return -> return 0
            i32_t.const_int(0, false)
        }
    }
}
