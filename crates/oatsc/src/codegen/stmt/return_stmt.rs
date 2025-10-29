use crate::diagnostics::Severity;
use inkwell::values::FunctionValue;
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
// A stack of per-scope local maps.
//
// Each entry in the vector is a HashMap representing a lexical scope's
// locals. The maps store `LocalEntry` tuples describing the alloca pointer,
// the ABI type of the slot, initialization flag, const flag, whether the
// local is a Weak<T> (affects RC semantics), an optional nominal type
// name used to guide member access lowering, and an optional OatsType
// for union tracking.
type LocalsStackLocal<'a> = Vec<HashMap<String, LocalEntry<'a>>>;

impl<'a> crate::codegen::CodeGen<'a> {
    pub(crate) fn lower_return_stmt(
        &self,
        ret: &deno_ast::swc::ast::ReturnStmt,
        function: FunctionValue<'a>,
        param_map: &HashMap<String, u32>,
        locals_stack: &mut LocalsStackLocal<'a>,
    ) -> crate::diagnostics::DiagnosticResult<bool> {
        // Lower return expression, emit rc_decs for locals then return.
        //
        // Important: we must run `emit_rc_dec_for_locals` before
        // emitting the `ret` instruction so that any pointer-valued
        // locals have their RC decremented and destructors (if any)
        // run before the caller resumes. This models deterministic
        // destruction and matches the runtime's RC expectations.
        if let Some(arg) = &ret.arg {
            match self.lower_expr(arg, function, param_map, locals_stack) {
                Ok(mut val) => {
                    // Check if function returns a union type and box if necessary
                    if let Some(return_type) = self.current_function_return_type.borrow().clone()
                        && matches!(return_type, crate::types::OatsType::Union(_))
                    {
                        // Box the value as a union
                        use inkwell::values::BasicValueEnum;
                        val = match val {
                            BasicValueEnum::FloatValue(fv) => {
                                let box_fn = self.get_union_box_f64();

                                self.builder
                                    .build_call(box_fn, &[fv.into()], "union_box")
                                    .map_err(|_| {
                                        crate::diagnostics::Diagnostic::simple_boxed(
                                            Severity::Error,
                                            "Failed to box f64 as union in return",
                                        )
                                    })?
                                    .try_as_basic_value()
                                    .left()
                                    .ok_or_else(|| {
                                        crate::diagnostics::Diagnostic::simple_boxed(
                                            Severity::Error,
                                            "union_box_f64 did not return value",
                                        )
                                    })?
                            }
                            BasicValueEnum::PointerValue(pv) => {
                                let box_fn = self.get_union_box_ptr();

                                self.builder
                                    .build_call(box_fn, &[pv.into()], "union_box")
                                    .map_err(|_| {
                                        crate::diagnostics::Diagnostic::simple_boxed(
                                            Severity::Error,
                                            "Failed to box ptr as union in return",
                                        )
                                    })?
                                    .try_as_basic_value()
                                    .left()
                                    .ok_or_else(|| {
                                        crate::diagnostics::Diagnostic::simple_boxed(
                                            Severity::Error,
                                            "union_box_ptr did not return value",
                                        )
                                    })?
                            }
                            BasicValueEnum::IntValue(iv) if iv.get_type().get_bit_width() == 1 => {
                                // Boolean -> convert to f64 and box
                                let as_f64 = self
                                    .builder
                                    .build_unsigned_int_to_float(iv, self.f64_t, "bool_to_f64")
                                    .map_err(|_| {
                                        crate::diagnostics::Diagnostic::simple_boxed(
                                            Severity::Error,
                                            "Failed to convert bool to f64 in union return",
                                        )
                                    })?;
                                let box_fn = self.get_union_box_f64();

                                self.builder
                                    .build_call(box_fn, &[as_f64.into()], "union_box")
                                    .map_err(|_| {
                                        crate::diagnostics::Diagnostic::simple_boxed(
                                            Severity::Error,
                                            "Failed to box bool as union in return",
                                        )
                                    })?
                                    .try_as_basic_value()
                                    .left()
                                    .ok_or_else(|| {
                                        crate::diagnostics::Diagnostic::simple_boxed(
                                            Severity::Error,
                                            "union_box_f64 did not return value",
                                        )
                                    })?
                            }
                            _ => val, // Already boxed or unsupported type
                        };
                    }
                    // emit rc decs for locals
                    self.emit_rc_dec_for_locals(locals_stack);
                    // build return with the lowered value
                    let _ = self.builder.build_return(Some(&val));
                    Ok(true)
                }
                Err(diag) => {
                    // Emit diagnostic so we know why the return expression failed to lower
                    crate::diagnostics::emit_diagnostic(&diag, Some(self.source));
                    Ok(false)
                }
            }
        } else {
            self.emit_rc_dec_for_locals(locals_stack);
            let _ = self.builder.build_return(None);
            Ok(true)
        }
    }
}
