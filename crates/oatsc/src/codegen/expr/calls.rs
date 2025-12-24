use crate::codegen::CodeGen;
use crate::diagnostics::{Diagnostic, Severity};
use crate::types::OatsType;
use inkwell::AddressSpace;
use inkwell::types::BasicTypeEnum;
use inkwell::values::{BasicValue, BasicValueEnum, PointerValue};
use oxc_ast::ast::{CallExpression, Expression, StaticMemberExpression, IdentifierReference, NewExpression};
use std::collections::HashMap;

type CallExpr<'a> = CallExpression<'a>;

#[allow(dead_code)]
type LocalEntry<'a> = (
    PointerValue<'a>,
    BasicTypeEnum<'a>,
    bool,
    bool,
    bool,
    Option<String>,
    Option<OatsType>,
);

impl<'a> CodeGen<'a> {
    /// Lower Call expression (function calls, method calls, super calls, closures).
    #[allow(clippy::result_large_err)]
    pub(super) fn lower_call_expr(
        &self,
        call: &CallExpr,
        function: inkwell::values::FunctionValue<'a>,
        param_map: &HashMap<String, u32>,
        locals: &mut Vec<HashMap<String, LocalEntry<'a>>>,
    ) -> crate::diagnostics::DiagnosticResult<inkwell::values::BasicValueEnum<'a>> {
        // Support simple identifier callees and member-callee method calls.
        //
        // We intentionally support a small set of call shapes in the
        // emitter: plain identifier calls (e.g., `foo()`), and
        // member method calls (e.g., `obj.method()`). More complex
        // call expressions (computed callees, dynamic property lookups)
        // can be added when needed.

        // Handle super(...) calls
        if let Expression::Super(_) = &call.callee {
            return self.lower_super_constructor_call(call, function, param_map, locals);
        }

        // Handle new Number(...) calls
        if let Expression::NewExpression(new_expr) = &call.callee
            && let Expression::Identifier(ident) = &new_expr.callee
            && ident.name == "Number"
        {
            return self.lower_number_constructor_call(new_expr, function, param_map, locals);
        }

        // Handle super.method() calls
        if let Expression::StaticMemberExpression(member) = &call.callee
            && let Expression::Super(_) = &member.object
        {
            // Extract property name and call super method directly
            let method_name = member.property.name.to_string();
            return self.lower_super_method_call_direct(
                &method_name,
                call,
                function,
                param_map,
                locals,
            );
        }

        // Handle static member expression calls (obj.method() or std functions)
        if let Expression::StaticMemberExpression(member) = &call.callee {
            return self.lower_static_member_call(member, call, function, param_map, locals);
        }

        // Handle plain identifier calls (foo())
        if let Expression::Identifier(ident) = &call.callee {
            return self.lower_identifier_call(ident, call, function, param_map, locals);
        }

        // Attempt to lower the callee as a general expression (closure calls)
        if let Ok(callee_val) = self.lower_expr(&call.callee, function, param_map, locals)
            && let BasicValueEnum::PointerValue(callee_ptr) = callee_val
        {
            return self.lower_closure_call(callee_ptr, call, function, param_map, locals);
        }

        Err(Diagnostic::simple_with_span_boxed(
            Severity::Error,
            "unsupported call expression",
            call.span.start as usize,
        ))
    }

    /// Lower super constructor calls: super(...)
    fn lower_super_constructor_call(
        &self,
        call: &CallExpr,
        function: inkwell::values::FunctionValue<'a>,
        param_map: &HashMap<String, u32>,
        locals: &mut Vec<HashMap<String, LocalEntry<'a>>>,
    ) -> crate::diagnostics::DiagnosticResult<inkwell::values::BasicValueEnum<'a>> {
        let parent_opt = self.current_class_parent.borrow().clone();
        let parent = if let Some(p) = parent_opt {
            p
        } else {
            return Err(Diagnostic::simple_with_span_boxed(
                Severity::Error,
                "super used outside of constructor or no parent",
                call.span.start as usize,
            ));
        };

        // Find `this` in locals
        if let Some((this_ptr, this_ty, _init, _is_const, _extra, _nominal, _oats_type)) =
            self.find_local(locals, "this")
        {
            let this_loaded = match self.builder.build_load(this_ty, this_ptr, "this_loaded") {
                Ok(v) => v,
                Err(_) => {
                    return Err(Diagnostic::simple_boxed(
                        Severity::Error,
                        "operation failed",
                    ));
                }
            };

            let mut args: Vec<inkwell::values::BasicMetadataValueEnum> = Vec::new();
            args.push(this_loaded.into());
            for a in &call.arguments {
                if let Some(expr) = a.as_expression() {
                    if let Ok(v) = self.lower_expr(expr, function, param_map, locals) {
                        args.push(v.into());
                    } else {
                        return Err(Diagnostic::simple_boxed(
                            Severity::Error,
                            "expression lowering failed",
                        ))?;
                    }
                } else {
                    return Err(Diagnostic::simple_boxed(
                        Severity::Error,
                        "unsupported argument type",
                    ))?;
                }
            }

            let init_name = format!("{parent}_init");
            if let Some(init_f) = self.module.get_function(&init_name) {
                let cs = match self.builder.build_call(init_f, &args, "call_super_init") {
                    Ok(cs) => cs,
                    Err(_) => {
                        return Err(Diagnostic::simple_boxed(
                            Severity::Error,
                            "operation failed",
                        ));
                    }
                };
                let either = cs.try_as_basic_value();
                if let inkwell::values::ValueKind::Basic(bv) = either {
                    Ok(bv)
                } else {
                    Err(Diagnostic::simple_boxed(
                        Severity::Error,
                        format!("super target '{init_name}' not found"),
                    ))
                }
            } else {
                Err(Diagnostic::simple_boxed(
                    Severity::Error,
                    format!("super target '{init_name}' not found"),
                ))
            }
        } else {
            Err(Diagnostic::simple_boxed(
                Severity::Error,
                "super used but `this` not found",
            ))
        }
    }

    /// Lower static member expression calls (`obj.method()` or std functions)
    fn lower_static_member_call(
        &self,
        member: &StaticMemberExpression<'_>,
        call: &CallExpr,
        function: inkwell::values::FunctionValue<'a>,
        param_map: &HashMap<String, u32>,
        locals: &mut Vec<HashMap<String, LocalEntry<'a>>>,
    ) -> crate::diagnostics::DiagnosticResult<inkwell::values::BasicValueEnum<'a>> {
        let prop_name = member.property.name.to_string();

        // Get object name if it's an identifier
        let obj_name = if let Expression::Identifier(obj_ident) = &member.object {
            obj_ident.name.to_string()
        } else {
            return Err(Diagnostic::simple_with_span_boxed(
                Severity::Error,
                "unsupported member expression object type",
                call.span.start as usize,
            ));
        };

        // If the identifier is a local variable, treat this as an instance method call
        if self.find_local(locals, &obj_name).is_some() {
            return self.lower_instance_method_call(
                (&obj_name, &prop_name),
                &member.object,
                call,
                function,
                param_map,
                locals,
            );
        }

        // Handle Promise.resolve()
        if obj_name == "Promise" && prop_name == "resolve" && call.arguments.len() == 1 {
            let arg_expr = call.arguments[0].as_expression().ok_or_else(|| {
                Diagnostic::simple_boxed(
                    Severity::Error,
                    "Promise.resolve requires an expression argument",
                )
            })?;
            let arg_val = match self.lower_expr(arg_expr, function, param_map, locals) {
                Ok(v) => v,
                Err(_) => {
                    return Err(Diagnostic::simple_boxed(
                        Severity::Error,
                        "expression lowering failed",
                    ))?;
                }
            };

            // Ensure we pass an `i8*` payload to the runtime
            let payload_ptr = match arg_val {
                BasicValueEnum::PointerValue(pv) => pv.as_basic_value_enum(),
                BasicValueEnum::FloatValue(fv) => {
                    let box_fn = self.get_union_box_f64();
                    let cs = match self.builder.build_call(box_fn, &[fv.into()], "box_f64") {
                        Ok(cs) => cs,
                        Err(_) => {
                            return Err(Diagnostic::simple_boxed(
                                Severity::Error,
                                "union_box_f64 call failed",
                            ))?;
                        }
                    };
                    match cs.try_as_basic_value() {
                        inkwell::values::ValueKind::Basic(bv) => bv,
                        _ => {
                            return Err(Diagnostic::simple_boxed(
                                Severity::Error,
                                "union_box_f64 returned non-value",
                            ))?;
                        }
                    }
                }
                _ => {
                    return Ok(arg_val);
                }
            };

            let f = self.get_promise_resolve();
            let cs = match self
                .builder
                .build_call(f, &[payload_ptr.into()], "promise_resolve_call")
            {
                Ok(cs) => cs,
                Err(_) => {
                    return Err(Diagnostic::simple_boxed(
                        Severity::Error,
                        "promise_resolve call failed",
                    ))?;
                }
            };
            match cs.try_as_basic_value() {
                inkwell::values::ValueKind::Basic(bv) => Ok(bv),
                _ => Err(Diagnostic::simple_boxed(
                    Severity::Error,
                    "promise_resolve returned non-value",
                )),
            }
        } else if obj_name == "Number" {
            // Handle Number.* static methods
            self.lower_number_static_method(&prop_name, call, function, param_map, locals)
        } else {
            // Handle std namespace functions (e.g., Math.max, console.log)
            // Special handling for console.log to dispatch based on argument type
            if obj_name == "console" && prop_name == "log" {
                return self.lower_console_log(call, function, param_map, locals);
            }

            let fname = format!("oats_std_{obj_name}_{prop_name}");

            let mut lowered_args: Vec<inkwell::values::BasicMetadataValueEnum> = Vec::new();
            for a in &call.arguments {
                if let Some(expr) = a.as_expression() {
                    if let Ok(val) = self.lower_expr(expr, function, param_map, locals) {
                        lowered_args.push(val.into());
                    } else {
                        return Err(Diagnostic::simple_boxed(
                            Severity::Error,
                            "expression lowering failed",
                        ))?;
                    }
                } else {
                    return Err(Diagnostic::simple_boxed(
                        Severity::Error,
                        "unsupported argument type",
                    ))?;
                }
            }

            if let Some(fv) = self.module.get_function(&fname) {
                let _cs = match self.builder.build_call(fv, &lowered_args, "std_call") {
                    Ok(cs) => cs,
                    Err(_) => {
                        return Err(Diagnostic::simple_boxed(
                            Severity::Error,
                            "operation failed",
                        ));
                    }
                };
                // console.log returns void, so we return a dummy value
                Ok(self.f64_t.const_float(0.0).as_basic_value_enum())
            } else {
                Err(Diagnostic::simple_with_span_boxed(
                    Severity::Error,
                    format!("std function '{fname}' not found"),
                    call.span.start as usize,
                ))
            }
        }
    }

    /// Lower console.log calls with type-based dispatch
    fn lower_console_log(
        &self,
        call: &CallExpr,
        function: inkwell::values::FunctionValue<'a>,
        param_map: &HashMap<String, u32>,
        locals: &mut Vec<HashMap<String, LocalEntry<'a>>>,
    ) -> crate::diagnostics::DiagnosticResult<inkwell::values::BasicValueEnum<'a>> {
        // Handle no arguments - print newline
        if call.arguments.is_empty() {
            if let Some(fv) = self.module.get_function("oats_std_console_log_newline") {
                let _ = self.builder.build_call(fv, &[], "console_log_newline");
                return Ok(self.f64_t.const_float(0.0).as_basic_value_enum());
            }
            return Err(Diagnostic::simple_with_span_boxed(
                Severity::Error,
                "oats_std_console_log_newline not found",
                call.span.start as usize,
            ));
        }

        // Handle single argument - dispatch based on type
        if call.arguments.len() == 1
            && let Some(expr) = call.arguments[0].as_expression()
        {
            // Get the type of the expression for union handling
            let expr_type = self.infer_type_from_expr(expr)?;
            let arg_val = self.lower_expr(expr, function, param_map, locals)?;

            match arg_val {
                BasicValueEnum::PointerValue(pv) => {
                    // Check if this is a union that needs unboxing
                    if matches!(expr_type, crate::types::OatsType::Union(_)) {
                        // This is a union - unbox it based on discriminant
                        let get_disc = self.get_union_get_discriminant();
                        let disc = match self.builder.build_call(
                            get_disc,
                            &[pv.into()],
                            "get_union_disc_console",
                        ) {
                            Ok(call) => call,
                            Err(_) => {
                                return Err(Diagnostic::simple_with_span_boxed(
                                    Severity::Error,
                                    "failed to get union discriminant for console.log",
                                    call.span.start as usize,
                                ));
                            }
                        };

                        let disc_val = match disc.try_as_basic_value() {
                            inkwell::values::ValueKind::Basic(bv) => bv.into_int_value(),
                            _ => {
                                return Err(Diagnostic::simple_with_span_boxed(
                                    Severity::Error,
                                    "union discriminant must be basic value",
                                    call.span.start as usize,
                                ));
                            }
                        };

                        // Check if it's a string (discriminant == 1 for pointer/string)
                        let is_string = match self.builder.build_int_compare(
                            inkwell::IntPredicate::EQ,
                            disc_val,
                            self.i64_t.const_int(1, false),
                            "is_union_string_console",
                        ) {
                            Ok(cmp) => cmp,
                            Err(_) => {
                                return Err(Diagnostic::simple_with_span_boxed(
                                    Severity::Error,
                                    "failed to compare union discriminant",
                                    call.span.start as usize,
                                ));
                            }
                        };

                        // Create blocks for string and number cases
                        let current_block = self.builder.get_insert_block().ok_or_else(|| {
                            Diagnostic::simple_with_span_boxed(
                                Severity::Error,
                                "no current insert block during union discriminant comparison",
                                call.span.start as usize,
                            )
                        })?;
                        let fn_val = current_block.get_parent().ok_or_else(|| {
                            Diagnostic::simple_with_span_boxed(
                                Severity::Error,
                                "current block has no parent function during union discriminant comparison",
                                call.span.start as usize,
                            )
                        })?;
                        let string_block = self
                            .context
                            .append_basic_block(fn_val, "union_string_console");
                        let number_block = self
                            .context
                            .append_basic_block(fn_val, "union_number_console");
                        let cont_block = self
                            .context
                            .append_basic_block(fn_val, "union_console_cont");

                        if self
                            .builder
                            .build_conditional_branch(is_string, string_block, number_block)
                            .is_err()
                        {
                            return Err(Diagnostic::simple_with_span_boxed(
                                Severity::Error,
                                "failed to build conditional branch",
                                call.span.start as usize,
                            ));
                        }

                        // String case
                        self.builder.position_at_end(string_block);
                        let unbox_str = self.get_union_unbox_ptr();
                        let str_result = match self.builder.build_call(
                            unbox_str,
                            &[pv.into()],
                            "unbox_union_str_console",
                        ) {
                            Ok(call) => call,
                            Err(_) => {
                                return Err(Diagnostic::simple_with_span_boxed(
                                    Severity::Error,
                                    "failed to unbox string from union",
                                    call.span.start as usize,
                                ));
                            }
                        };
                        let str_ptr = match str_result.try_as_basic_value() {
                            inkwell::values::ValueKind::Basic(bv) => bv.into_pointer_value(),
                            _ => {
                                return Err(Diagnostic::simple_with_span_boxed(
                                    Severity::Error,
                                    "unboxed string must be pointer",
                                    call.span.start as usize,
                                ));
                            }
                        };
                        if let Some(fv) = self
                            .module
                            .get_function(crate::runtime_functions::names::STD_CONSOLE_LOG)
                        {
                            let _ = self.builder.build_call(
                                fv,
                                &[str_ptr.into()],
                                "console_log_string",
                            );
                        } else {
                            return Err(Diagnostic::simple_with_span_boxed(
                                Severity::Error,
                                "oats_std_console_log not found",
                                call.span.start as usize,
                            ));
                        }
                        if self.builder.build_unconditional_branch(cont_block).is_err() {
                            return Err(Diagnostic::simple_with_span_boxed(
                                Severity::Error,
                                "failed to build branch",
                                call.span.start as usize,
                            ));
                        }

                        // Number case
                        self.builder.position_at_end(number_block);
                        let unbox_num = self.get_union_unbox_f64();
                        let num_result = match self.builder.build_call(
                            unbox_num,
                            &[pv.into()],
                            "unbox_union_num_console",
                        ) {
                            Ok(call) => call,
                            Err(_) => {
                                return Err(Diagnostic::simple_with_span_boxed(
                                    Severity::Error,
                                    "failed to unbox number from union",
                                    call.span.start as usize,
                                ));
                            }
                        };
                        let num_val = match num_result.try_as_basic_value() {
                            inkwell::values::ValueKind::Basic(bv) => bv.into_float_value(),
                            _ => {
                                return Err(Diagnostic::simple_with_span_boxed(
                                    Severity::Error,
                                    "unboxed number must be float",
                                    call.span.start as usize,
                                ));
                            }
                        };
                        if let Some(fv) = self.module.get_function("oats_std_console_log_number") {
                            let _ = self.builder.build_call(
                                fv,
                                &[num_val.into()],
                                "console_log_number",
                            );
                        } else {
                            return Err(Diagnostic::simple_with_span_boxed(
                                Severity::Error,
                                "oats_std_console_log_number not found",
                                call.span.start as usize,
                            ));
                        }
                        if self.builder.build_unconditional_branch(cont_block).is_err() {
                            return Err(Diagnostic::simple_with_span_boxed(
                                Severity::Error,
                                "failed to build branch",
                                call.span.start as usize,
                            ));
                        }

                        // Continue
                        self.builder.position_at_end(cont_block);
                        return Ok(self.f64_t.const_float(0.0).as_basic_value_enum());
                    }
                    // Regular string pointer
                    if let Some(fv) = self
                        .module
                        .get_function(crate::runtime_functions::names::STD_CONSOLE_LOG)
                    {
                        let _ = self.builder.build_call(fv, &[pv.into()], "console_log");
                        return Ok(self.f64_t.const_float(0.0).as_basic_value_enum());
                    }
                    return Err(Diagnostic::simple_with_span_boxed(
                        Severity::Error,
                        "oats_std_console_log not found",
                        call.span.start as usize,
                    ));
                }
                BasicValueEnum::FloatValue(fv_val) => {
                    // Number - use number version
                    if let Some(fv) = self.module.get_function("oats_std_console_log_number") {
                        let _ = self
                            .builder
                            .build_call(fv, &[fv_val.into()], "console_log_number");
                        return Ok(self.f64_t.const_float(0.0).as_basic_value_enum());
                    }
                    return Err(Diagnostic::simple_with_span_boxed(
                        Severity::Error,
                        "oats_std_console_log_number not found",
                        call.span.start as usize,
                    ));
                }
                BasicValueEnum::IntValue(iv_val) => {
                    // Check if it's i32 or i64
                    if iv_val.get_type().get_bit_width() == 32 {
                        // i32 - use int version
                        if let Some(fv) = self.module.get_function("oats_std_console_log_int") {
                            let _ =
                                self.builder
                                    .build_call(fv, &[iv_val.into()], "console_log_int");
                            return Ok(self.f64_t.const_float(0.0).as_basic_value_enum());
                        }
                        return Err(Diagnostic::simple_with_span_boxed(
                            Severity::Error,
                            "oats_std_console_log_int not found",
                            call.span.start as usize,
                        ));
                    } else {
                        // i64 or other - convert to f64 and use number version
                        let f64_val = self
                            .builder
                            .build_signed_int_to_float(iv_val, self.f64_t, "int_to_f64")
                            .map_err(|_| {
                                Diagnostic::simple_boxed(
                                    Severity::Error,
                                    "failed to convert int to float",
                                )
                            })?;
                        if let Some(fv) = self.module.get_function("oats_std_console_log_number") {
                            let _ = self.builder.build_call(
                                fv,
                                &[f64_val.into()],
                                "console_log_number",
                            );
                            return Ok(self.f64_t.const_float(0.0).as_basic_value_enum());
                        }
                        return Err(Diagnostic::simple_with_span_boxed(
                            Severity::Error,
                            "oats_std_console_log_number not found",
                            call.span.start as usize,
                        ));
                    }
                }
                _ => {
                    // Other types (arrays, structs, vectors) - convert to string
                    // For now, return error - can be extended later
                    return Err(Diagnostic::simple_with_span_boxed(
                        Severity::Error,
                        "console.log: unsupported argument type".to_string(),
                        call.span.start as usize,
                    ));
                }
            }
        }

        // Multiple arguments - convert all to strings and concatenate, or just use first
        // For now, handle first argument only (can be extended later)
        if let Some(expr) = call.arguments[0].as_expression() {
            let arg_val = self.lower_expr(expr, function, param_map, locals)?;
            // Convert to string pointer if needed
            let str_ptr = match arg_val {
                BasicValueEnum::PointerValue(pv) => pv,
                BasicValueEnum::FloatValue(fv) => {
                    // Convert number to string
                    let num_to_str = self.get_number_to_string();
                    let str_call = self
                        .builder
                        .build_call(num_to_str, &[fv.into()], "num_to_str")
                        .map_err(|_| {
                            Diagnostic::simple_boxed(
                                Severity::Error,
                                "failed to convert number to string",
                            )
                        })?;
                    match str_call.try_as_basic_value() {
                        inkwell::values::ValueKind::Basic(BasicValueEnum::PointerValue(p)) => p,
                        _ => {
                            return Err(Diagnostic::simple_boxed(
                                Severity::Error,
                                "number_to_string returned non-pointer",
                            ));
                        }
                    }
                }
                _ => {
                    return Err(Diagnostic::simple_with_span_boxed(
                        Severity::Error,
                        "unsupported console.log argument type",
                        call.span.start as usize,
                    ));
                }
            };

            // str_ptr is already a data pointer (offset +16 from object base)
            // String literals, str_concat, and number_to_string all return data pointers
            // oats_std_console_log expects a pointer to the actual string data
            if let Some(fv) = self
                .module
                .get_function(crate::runtime_functions::names::STD_CONSOLE_LOG)
            {
                let _ = self
                    .builder
                    .build_call(fv, &[str_ptr.into()], "console_log");
                Ok(self.f64_t.const_float(0.0).as_basic_value_enum())
            } else {
                Err(Diagnostic::simple_with_span_boxed(
                    Severity::Error,
                    "oats_std_console_log not found",
                    call.span.start as usize,
                ))
            }
        } else {
            Err(Diagnostic::simple_with_span_boxed(
                Severity::Error,
                "console.log requires expression arguments",
                call.span.start as usize,
            ))
        }
    }

    /// Lower instance method calls (`obj.method()`)
    fn lower_instance_method_call(
        &self,
        method_info: (&str, &str),
        obj_expr: &Expression<'_>,
        call: &CallExpr,
        function: inkwell::values::FunctionValue<'a>,
        param_map: &HashMap<String, u32>,
        locals: &mut Vec<HashMap<String, LocalEntry<'a>>>,
    ) -> crate::diagnostics::DiagnosticResult<inkwell::values::BasicValueEnum<'a>> {
        let (obj_name, method_name) = method_info;
        if let Ok(obj_val) = self.lower_expr(obj_expr, function, param_map, locals) {
            // Support weak reference helpers
            if method_name == "downgrade" {
                if !call.arguments.is_empty() {
                    return Err(Diagnostic::simple_boxed(
                        Severity::Error,
                        "downgrade() takes no arguments",
                    ))?;
                }
                if let BasicValueEnum::PointerValue(pv) = obj_val {
                    let f = self.get_rc_weak_inc();
                    let _ = self.builder.build_call(f, &[pv.into()], "rc_weak_inc_call");
                    return Ok(pv.as_basic_value_enum());
                }
                return Err(Diagnostic::simple_boxed(
                    Severity::Error,
                    "downgrade on non-pointer",
                ))?;
            }

            if method_name == "upgrade" {
                if !call.arguments.is_empty() {
                    return Err(Diagnostic::simple_boxed(
                        Severity::Error,
                        "upgrade() takes no arguments",
                    ))?;
                }
                if let BasicValueEnum::PointerValue(pv) = obj_val {
                    let f = self.get_rc_weak_upgrade();
                    let cs = self
                        .builder
                        .build_call(f, &[pv.into()], "rc_weak_upgrade_call");
                    if let Ok(cs) = cs
                        && let inkwell::values::ValueKind::Basic(bv) = cs.try_as_basic_value()
                    {
                        return Ok(bv);
                    }
                    return Err(Diagnostic::simple_boxed(Severity::Error, "upgrade failed"))?;
                }
                return Err(Diagnostic::simple_boxed(
                    Severity::Error,
                    "upgrade on non-pointer",
                ))?;
            }

            // Lower arguments
            let mut call_args: Vec<inkwell::values::BasicMetadataValueEnum> = Vec::new();
            call_args.push(obj_val.into());
            for a in &call.arguments {
                if let Some(expr) = a.as_expression() {
                    let val = match self.lower_expr(expr, function, param_map, locals) {
                        Ok(v) => v,
                        Err(d) => return Err(d)?,
                    };
                    call_args.push(val.into());
                } else {
                    return Err(Diagnostic::simple_boxed(
                        Severity::Error,
                        "unsupported argument type",
                    ))?;
                }
            }

            // Determine nominal type of the object
            let mut nominal_opt = None;
            if let Some((_, _, _, _, _, nom, _)) = self.find_local(locals, obj_name) {
                nominal_opt = nom;
            } else if let Some(param_idx) = param_map.get(obj_name) {
                let fname = function.get_name().to_str().unwrap_or_default();
                if let Some(param_types) = self.fn_param_types.borrow().get(fname) {
                    let idx = *param_idx as usize;
                    if idx < param_types.len()
                        && let crate::types::OatsType::NominalStruct(n) = &param_types[idx]
                    {
                        nominal_opt = Some(n.clone());
                    }
                }
            }

            if let Some(nom) = nominal_opt {
                let mut current_nom = nom.clone();
                let mut method_fn = None;

                loop {
                    let method_fn_name = if let Some(ref rta) = self.rta_results {
                        if let Some(devirt_target) = rta.can_devirtualize(&current_nom, method_name)
                        {
                            devirt_target
                        } else {
                            format!("{current_nom}_{method_name}")
                        }
                    } else {
                        format!("{current_nom}_{method_name}")
                    };

                    if let Some(f) = self.module.get_function(&method_fn_name) {
                        method_fn = Some(f);
                        break;
                    }

                    // Check parent class
                    let parents = self.class_parents.borrow();
                    if let Some(Some(parent_nom)) = parents.get(&current_nom) {
                        current_nom = parent_nom.clone();
                    } else {
                        break;
                    }
                }

                if let Some(method_fn) = method_fn {
                    let cs = match self.builder.build_call(
                        method_fn,
                        &call_args,
                        "class_method_call",
                    ) {
                        Ok(cs) => cs,
                        Err(_) => {
                            return Err(Box::new(
                                    Diagnostic::simple(Severity::Error, "class method call failed")
                                        .with_note(format!(
                                            "Failed to call method '{method_name}' on class '{nom}'"
                                        ))
                                        .with_help(format!(
                                            "Ensure the method '{method_name}' exists in class '{nom}' or its parents"
                                        )),
                                ));
                        }
                    };
                    let either = cs.try_as_basic_value();
                    if let inkwell::values::ValueKind::Basic(bv) = either {
                        return Ok(bv);
                    } else {
                        let zero = self.f64_t.const_float(0.0);
                        return Ok(zero.as_basic_value_enum());
                    }
                }
            }

            Err(Box::new(
                Diagnostic::simple(
                    Severity::Error,
                    format!(
                        "method '{method_name}' not found on object '{obj_name}'"
                    ),
                )
                .with_note(format!(
                    "The object '{obj_name}' does not have a method named '{method_name}'"
                ))
                .with_help(format!(
                    "Check that '{obj_name}' is a class instance with method '{method_name}'"
                )),
            ))
        } else {
            Err(Box::new(
                Diagnostic::simple(
                    Severity::Error,
                    format!(
                        "failed to lower object expression for method call '{method_name}'"
                    ),
                )
                .with_note(format!(
                    "The object expression '{obj_name}' could not be evaluated"
                ))
                .with_help(
                    "Ensure the object is properly initialized before calling methods on it",
                ),
            ))
        }
    }

    /// Lower plain identifier calls (`foo()`)
    fn lower_identifier_call(
        &self,
        ident: &IdentifierReference<'_>,
        call: &CallExpr,
        function: inkwell::values::FunctionValue<'a>,
        param_map: &HashMap<String, u32>,
        locals: &mut Vec<HashMap<String, LocalEntry<'a>>>,
    ) -> crate::diagnostics::DiagnosticResult<inkwell::values::BasicValueEnum<'a>> {
        let fn_name = ident.name.to_string();

        // Check symbol table for functions (including nested/mangled ones)
        let symbol_opt = self.symbol_table.borrow().get(&fn_name).cloned();
        if let Some(crate::types::Symbol::StdFunction { mangled_name, sig }) = symbol_opt
            && let Some(fv) = self.module.get_function(&mangled_name)
        {
            let mut call_args: Vec<inkwell::values::BasicMetadataValueEnum> = Vec::new();
            for (i, a) in call.arguments.iter().enumerate() {
                if let Some(expr) = a.as_expression() {
                    let mut val = match self.lower_expr(expr, function, param_map, locals) {
                        Ok(v) => v,
                        Err(d) => return Err(d)?,
                    };

                    // Check if we need to coerce the argument to match the parameter type
                    if let Some(param_type) = sig.params.get(i) {
                        val = self.coerce_argument_to_parameter_type(val, param_type)?;
                    }

                    call_args.push(val.into());
                } else {
                    return Err(Diagnostic::simple_boxed(
                        Severity::Error,
                        "unsupported argument type",
                    ))?;
                }
            }
            let cs = match self.builder.build_call(fv, &call_args, "function_call") {
                Ok(cs) => cs,
                Err(_) => {
                    return Err(Diagnostic::simple_boxed(
                        Severity::Error,
                        "function call failed",
                    ));
                }
            };
            let either = cs.try_as_basic_value();
            if let inkwell::values::ValueKind::Basic(bv) = either {
                return Ok(bv);
            } else {
                let zero = self.f64_t.const_float(0.0);
                return Ok(zero.as_basic_value_enum());
            }
        }

        // Check if it's a direct function call (fallback for global functions not in symbol table)
        if let Some(fv) = self.module.get_function(&fn_name) {
            let mut call_args: Vec<inkwell::values::BasicMetadataValueEnum> = Vec::new();
            for a in &call.arguments {
                if let Some(expr) = a.as_expression() {
                    let val = match self.lower_expr(expr, function, param_map, locals) {
                        Ok(v) => v,
                        Err(d) => return Err(d)?,
                    };
                    call_args.push(val.into());
                } else {
                    return Err(Diagnostic::simple_boxed(
                        Severity::Error,
                        "unsupported argument type",
                    ))?;
                }
            }
            let cs = match self.builder.build_call(fv, &call_args, "function_call") {
                Ok(cs) => cs,
                Err(_) => {
                    return Err(Diagnostic::simple_boxed(
                        Severity::Error,
                        "function call failed",
                    ));
                }
            };
            let either = cs.try_as_basic_value();
            if let inkwell::values::ValueKind::Basic(bv) = either {
                return Ok(bv);
            } else {
                let zero = self.f64_t.const_float(0.0);
                return Ok(zero.as_basic_value_enum());
            }
        }

        // Check for nested generic functions
        let has_nested = self.nested_generic_fns.borrow().contains_key(&fn_name);
        if has_nested
            && !call.arguments.is_empty()
            && let Some(first_arg_expr) = call.arguments[0].as_expression()
        {
            // Try to infer the type of the first argument to determine T
            let mut inferred_opt = None;
            if let Expression::Identifier(arg_ident) = first_arg_expr {
                let name = arg_ident.name.to_string();
                if let Some((_, _, _, _, _, _, oats_type)) = self.find_local(locals, &name) {
                    inferred_opt = oats_type;
                }
            } else {
                // Fallback to basic inference
                inferred_opt = Some(crate::types::infer_type(None, Some(first_arg_expr)));
            }

            if let Some(inferred) = inferred_opt
                && let crate::types::OatsType::Array(elem_ty) = inferred
                && let Ok(spec_name) = self.monomorphize_function(&fn_name, &[(*elem_ty).clone()])
            {
                if self.module.get_function(&spec_name).is_none() {
                    let fn_ty = self.i8ptr_t.fn_type(&[self.i8ptr_t.into()], false);
                    let _ = self.module.add_function(&spec_name, fn_ty, None);
                }
                let mut call_args: Vec<inkwell::values::BasicMetadataValueEnum> = Vec::new();
                for a in &call.arguments {
                    if let Some(expr) = a.as_expression() {
                        let val = match self.lower_expr(expr, function, param_map, locals) {
                            Ok(v) => v,
                            Err(d) => return Err(d)?,
                        };
                        call_args.push(val.into());
                    } else {
                        return Err(Diagnostic::simple_boxed(
                            Severity::Error,
                            "unsupported argument type",
                        ))?;
                    }
                }
                if let Some(spec_fv) = self.module.get_function(&spec_name) {
                    let cs = match self
                        .builder
                        .build_call(spec_fv, &call_args, "generic_mono_call")
                    {
                        Ok(cs) => cs,
                        Err(_) => {
                            return Err(Diagnostic::simple_boxed(
                                Severity::Error,
                                "generic monomorphized call failed",
                            ));
                        }
                    };
                    let either = cs.try_as_basic_value();
                    if let inkwell::values::ValueKind::Basic(bv) = either {
                        return Ok(bv);
                    } else {
                        let zero = self.f64_t.const_float(0.0);
                        return Ok(zero.as_basic_value_enum());
                    }
                }
            }
        }

        Err(Diagnostic::simple_with_span_boxed(
            Severity::Error,
            format!("function '{fn_name}' not found"),
            call.span.start as usize,
        ))
    }

    /// Lower new Number(...) constructor calls
    fn lower_number_constructor_call(
        &self,
        new_expr: &NewExpression,
        function: inkwell::values::FunctionValue<'a>,
        param_map: &HashMap<String, u32>,
        locals: &mut Vec<HashMap<String, LocalEntry<'a>>>,
    ) -> crate::diagnostics::DiagnosticResult<inkwell::values::BasicValueEnum<'a>> {
        // new Number() - no arguments, returns 0
        // new Number(value) - returns the value coerced to number
        let num_value = if new_expr.arguments.is_empty() {
            self.f64_t.const_float(0.0)
        } else if new_expr.arguments.len() == 1 {
            let arg_expr = new_expr.arguments[0].as_expression().ok_or_else(|| {
                Diagnostic::simple_with_span_boxed(
                    Severity::Error,
                    "new Number() argument must be an expression",
                    new_expr.span.start as usize,
                )
            })?;
            let arg_val = self.lower_expr(arg_expr, function, param_map, locals)?;
            self.coerce_to_f64(arg_val).ok_or_else(|| {
                Diagnostic::simple_with_span_boxed(
                    Severity::Error,
                    "new Number() argument must be coercible to number",
                    new_expr.span.start as usize,
                )
            })?
        } else {
            return Err(Diagnostic::simple_with_span_boxed(
                Severity::Error,
                "new Number() accepts at most one argument",
                new_expr.span.start as usize,
            ));
        };

        // Create Number object
        let new_fn = self.get_number_new();
        let cs = self
            .builder
            .build_call(new_fn, &[num_value.into()], "number_new_call")
            .map_err(|_| Diagnostic::simple_boxed(Severity::Error, "number_new call failed"))?;
        match cs.try_as_basic_value() {
            inkwell::values::ValueKind::Basic(bv) => Ok(bv),
            _ => Err(Diagnostic::simple_boxed(
                Severity::Error,
                "number_new returned non-value",
            )),
        }
    }

    /// Lower Number.* static method calls
    fn lower_number_static_method(
        &self,
        method_name: &str,
        call: &CallExpr,
        function: inkwell::values::FunctionValue<'a>,
        param_map: &HashMap<String, u32>,
        locals: &mut Vec<HashMap<String, LocalEntry<'a>>>,
    ) -> crate::diagnostics::DiagnosticResult<inkwell::values::BasicValueEnum<'a>> {
        match method_name {
            "isNaN" => {
                if call.arguments.len() != 1 {
                    return Err(Diagnostic::simple_with_span_boxed(
                        Severity::Error,
                        "Number.isNaN requires exactly one argument",
                        call.span.start as usize,
                    ));
                }
                let arg_expr = call.arguments[0].as_expression().ok_or_else(|| {
                    Diagnostic::simple_with_span_boxed(
                        Severity::Error,
                        "Number.isNaN requires an expression argument",
                        call.span.start as usize,
                    )
                })?;
                let arg_val = self.lower_expr(arg_expr, function, param_map, locals)?;
                let num_val = self.coerce_to_f64(arg_val).ok_or_else(|| {
                    Diagnostic::simple_with_span_boxed(
                        Severity::Error,
                        "Number.isNaN argument must be coercible to number",
                        call.span.start as usize,
                    )
                })?;

                let is_nan_fn = self.get_number_is_nan_static();
                let cs = self
                    .builder
                    .build_call(is_nan_fn, &[num_val.into()], "number_is_nan_call")
                    .map_err(|_| {
                        Diagnostic::simple_boxed(Severity::Error, "number_is_nan call failed")
                    })?;
                match cs.try_as_basic_value() {
                    inkwell::values::ValueKind::Basic(bv) => Ok(bv),
                    _ => Err(Diagnostic::simple_boxed(
                        Severity::Error,
                        "number_is_nan returned non-value",
                    )),
                }
            }
            "isFinite" => {
                if call.arguments.len() != 1 {
                    return Err(Diagnostic::simple_with_span_boxed(
                        Severity::Error,
                        "Number.isFinite requires exactly one argument",
                        call.span.start as usize,
                    ));
                }
                let arg_expr = call.arguments[0].as_expression().ok_or_else(|| {
                    Diagnostic::simple_with_span_boxed(
                        Severity::Error,
                        "Number.isFinite requires an expression argument",
                        call.span.start as usize,
                    )
                })?;
                let arg_val = self.lower_expr(arg_expr, function, param_map, locals)?;
                let num_val = self.coerce_to_f64(arg_val).ok_or_else(|| {
                    Diagnostic::simple_with_span_boxed(
                        Severity::Error,
                        "Number.isFinite argument must be coercible to number",
                        call.span.start as usize,
                    )
                })?;

                let is_finite_fn = self.get_number_is_finite_static();
                let cs = self
                    .builder
                    .build_call(is_finite_fn, &[num_val.into()], "number_is_finite_call")
                    .map_err(|_| {
                        Diagnostic::simple_boxed(Severity::Error, "number_is_finite call failed")
                    })?;
                match cs.try_as_basic_value() {
                    inkwell::values::ValueKind::Basic(bv) => Ok(bv),
                    _ => Err(Diagnostic::simple_boxed(
                        Severity::Error,
                        "number_is_finite returned non-value",
                    )),
                }
            }
            "isInteger" => {
                if call.arguments.len() != 1 {
                    return Err(Diagnostic::simple_with_span_boxed(
                        Severity::Error,
                        "Number.isInteger requires exactly one argument",
                        call.span.start as usize,
                    ));
                }
                let arg_expr = call.arguments[0].as_expression().ok_or_else(|| {
                    Diagnostic::simple_with_span_boxed(
                        Severity::Error,
                        "Number.isInteger requires an expression argument",
                        call.span.start as usize,
                    )
                })?;
                let arg_val = self.lower_expr(arg_expr, function, param_map, locals)?;
                let num_val = self.coerce_to_f64(arg_val).ok_or_else(|| {
                    Diagnostic::simple_with_span_boxed(
                        Severity::Error,
                        "Number.isInteger argument must be coercible to number",
                        call.span.start as usize,
                    )
                })?;

                let is_integer_fn = self.get_number_is_integer();
                let cs = self
                    .builder
                    .build_call(is_integer_fn, &[num_val.into()], "number_is_integer_call")
                    .map_err(|_| {
                        Diagnostic::simple_boxed(Severity::Error, "number_is_integer call failed")
                    })?;
                match cs.try_as_basic_value() {
                    inkwell::values::ValueKind::Basic(bv) => Ok(bv),
                    _ => Err(Diagnostic::simple_boxed(
                        Severity::Error,
                        "number_is_integer returned non-value",
                    )),
                }
            }
            "parseFloat" => {
                if call.arguments.len() != 1 {
                    return Err(Diagnostic::simple_with_span_boxed(
                        Severity::Error,
                        "Number.parseFloat requires exactly one argument",
                        call.span.start as usize,
                    ));
                }
                let arg_expr = call.arguments[0].as_expression().ok_or_else(|| {
                    Diagnostic::simple_with_span_boxed(
                        Severity::Error,
                        "Number.parseFloat requires an expression argument",
                        call.span.start as usize,
                    )
                })?;
                let arg_val = self.lower_expr(arg_expr, function, param_map, locals)?;

                // Convert argument to string pointer
                let str_ptr = match arg_val {
                    BasicValueEnum::PointerValue(pv) => pv,
                    BasicValueEnum::FloatValue(fv) => {
                        // Convert number to string first, then parse it back
                        let num_to_str_fn = self.get_number_to_string();
                        let str_cs = self
                            .builder
                            .build_call(num_to_str_fn, &[fv.into()], "num_to_str_for_parse")
                            .map_err(|_| {
                                Diagnostic::simple_boxed(
                                    Severity::Error,
                                    "number_to_string call failed",
                                )
                            })?;
                        match str_cs.try_as_basic_value() {
                            inkwell::values::ValueKind::Basic(BasicValueEnum::PointerValue(pv)) => {
                                pv
                            }
                            _ => {
                                return Err(Diagnostic::simple_boxed(
                                    Severity::Error,
                                    "number_to_string returned non-pointer",
                                ));
                            }
                        }
                    }
                    _ => {
                        return Err(Diagnostic::simple_with_span_boxed(
                            Severity::Error,
                            "Number.parseFloat argument must be string or number",
                            call.span.start as usize,
                        ));
                    }
                };

                let parse_float_fn = self.get_number_parse_float_static();
                let cs = self
                    .builder
                    .build_call(parse_float_fn, &[str_ptr.into()], "number_parse_float_call")
                    .map_err(|_| {
                        Diagnostic::simple_boxed(Severity::Error, "number_parse_float call failed")
                    })?;
                match cs.try_as_basic_value() {
                    inkwell::values::ValueKind::Basic(bv) => Ok(bv),
                    _ => Err(Diagnostic::simple_boxed(
                        Severity::Error,
                        "number_parse_float returned non-value",
                    )),
                }
            }
            "parseInt" => {
                if call.arguments.is_empty() || call.arguments.len() > 2 {
                    return Err(Diagnostic::simple_with_span_boxed(
                        Severity::Error,
                        "Number.parseInt requires 1 or 2 arguments",
                        call.span.start as usize,
                    ));
                }
                let str_expr = call.arguments[0].as_expression().ok_or_else(|| {
                    Diagnostic::simple_with_span_boxed(
                        Severity::Error,
                        "Number.parseInt first argument must be an expression",
                        call.span.start as usize,
                    )
                })?;
                let str_val = self.lower_expr(str_expr, function, param_map, locals)?;

                // Convert first argument to string pointer
                let str_ptr = match str_val {
                    BasicValueEnum::PointerValue(pv) => pv,
                    BasicValueEnum::FloatValue(fv) => {
                        // Convert number to string first
                        let num_to_str_fn = self.get_number_to_string();
                        let str_cs = self
                            .builder
                            .build_call(num_to_str_fn, &[fv.into()], "num_to_str_for_parse_int")
                            .map_err(|_| {
                                Diagnostic::simple_boxed(
                                    Severity::Error,
                                    "number_to_string call failed",
                                )
                            })?;
                        match str_cs.try_as_basic_value() {
                            inkwell::values::ValueKind::Basic(BasicValueEnum::PointerValue(pv)) => {
                                pv
                            }
                            _ => {
                                return Err(Diagnostic::simple_boxed(
                                    Severity::Error,
                                    "number_to_string returned non-pointer",
                                ));
                            }
                        }
                    }
                    _ => {
                        return Err(Diagnostic::simple_with_span_boxed(
                            Severity::Error,
                            "Number.parseInt first argument must be string or number",
                            call.span.start as usize,
                        ));
                    }
                };

                // Default radix is 10
                let radix_val = if call.arguments.len() >= 2 {
                    let radix_expr = call.arguments[1].as_expression().ok_or_else(|| {
                        Diagnostic::simple_with_span_boxed(
                            Severity::Error,
                            "Number.parseInt second argument must be an expression",
                            call.span.start as usize,
                        )
                    })?;
                    let radix_arg = self.lower_expr(radix_expr, function, param_map, locals)?;
                    self.coerce_to_f64(radix_arg).ok_or_else(|| {
                        Diagnostic::simple_with_span_boxed(
                            Severity::Error,
                            "Number.parseInt radix must be coercible to number",
                            call.span.start as usize,
                        )
                    })?
                } else {
                    self.f64_t.const_float(10.0)
                };

                let parse_int_fn = self.get_number_parse_int();
                let cs = self
                    .builder
                    .build_call(
                        parse_int_fn,
                        &[str_ptr.into(), radix_val.into()],
                        "number_parse_int_call",
                    )
                    .map_err(|_| {
                        Diagnostic::simple_boxed(Severity::Error, "number_parse_int call failed")
                    })?;
                match cs.try_as_basic_value() {
                    inkwell::values::ValueKind::Basic(bv) => Ok(bv),
                    _ => Err(Diagnostic::simple_boxed(
                        Severity::Error,
                        "number_parse_int returned non-value",
                    )),
                }
            }
            _ => Err(Diagnostic::simple_with_span_boxed(
                Severity::Error,
                format!("unsupported Number static method: {method_name}"),
                call.span.start as usize,
            )),
        }
    }

    /// Lower closure calls (calling closure objects)
    fn lower_closure_call(
        &self,
        callee_ptr: PointerValue<'a>,
        call: &CallExpr,
        function: inkwell::values::FunctionValue<'a>,
        param_map: &HashMap<String, u32>,
        locals: &mut Vec<HashMap<String, LocalEntry<'a>>>,
    ) -> crate::diagnostics::DiagnosticResult<inkwell::values::BasicValueEnum<'a>> {
        // Closure layout: [header][meta][fn_ptr][env_ptr]
        let header_size = self.i64_t.const_int(8u64, false);
        let meta_slot = self.i64_t.const_int(8u64, false);
        let ptr_sz = self.i64_t.const_int(8u64, false);
        let off_fn = self
            .builder
            .build_int_add(header_size, meta_slot, "hdr_plus_meta")
            .map_err(|_| Diagnostic::error("LLVM builder error"))?;
        let fn_ptr_i8 = self
            .i8_ptr_from_offset_i64(callee_ptr, off_fn, "closure_fn_i8")
            .map_err(|_| Diagnostic::error("operation failed"))?;
        let off_env = self
            .builder
            .build_int_add(off_fn, ptr_sz, "off_env")
            .map_err(|_| Diagnostic::error("LLVM builder error"))?;
        let env_ptr_i8 = self
            .i8_ptr_from_offset_i64(callee_ptr, off_env, "closure_env_i8")
            .map_err(|_| Diagnostic::error("operation failed"))?;

        let fn_ptr_slot_ty = self.context.ptr_type(AddressSpace::default());
        let fn_ptr_slot = self
            .builder
            .build_pointer_cast(fn_ptr_i8, fn_ptr_slot_ty, "fn_ptr_slot_cast")
            .map_err(|_| Diagnostic::error("LLVM builder error"))?;
        let fn_ptr_bv = match self
            .builder
            .build_load(self.i8ptr_t, fn_ptr_slot, "loaded_fn_ptr")
        {
            Ok(v) => v,
            Err(_) => {
                return Err(Diagnostic::simple_boxed(
                    Severity::Error,
                    "operation failed",
                ));
            }
        };

        let env_slot_ty = self.context.ptr_type(AddressSpace::default());
        let env_slot = self
            .builder
            .build_pointer_cast(env_ptr_i8, env_slot_ty, "env_slot_cast")
            .map_err(|_| Diagnostic::error("LLVM builder error"))?;
        let env_bv = match self
            .builder
            .build_load(self.i8ptr_t, env_slot, "loaded_env_ptr")
        {
            Ok(v) => v,
            Err(_) => {
                return Err(Diagnostic::simple_boxed(
                    Severity::Error,
                    "operation failed",
                ));
            }
        };

        let mut lowered_args: Vec<inkwell::values::BasicMetadataValueEnum> = Vec::new();
        for a in &call.arguments {
            if let Some(expr) = a.as_expression() {
                if let Ok(val) = self.lower_expr(expr, function, param_map, locals) {
                    lowered_args.push(val.into());
                } else {
                    return Err(Diagnostic::simple_boxed(
                        Severity::Error,
                        "expression lowering failed",
                    ))?;
                }
            } else {
                return Err(Diagnostic::simple_boxed(
                    Severity::Error,
                    "unsupported argument type",
                ))?;
            }
        }

        let static_ret = self
            .last_expr_origin_local
            .borrow()
            .clone()
            .and_then(|n| self.closure_local_rettype.borrow().get(&n).cloned());

        if let BasicValueEnum::PointerValue(fn_ptr_pv) = fn_ptr_bv {
            let mut param_types: Vec<inkwell::types::BasicMetadataTypeEnum> = Vec::new();
            param_types.push(self.i8ptr_t.into());
            for _ in &lowered_args {
                param_types.push(self.i8ptr_t.into());
            }

            let fn_ty = if let Some(rt) = static_ret {
                match rt {
                    crate::types::OatsType::Number => self.f64_t.fn_type(&param_types, false),
                    crate::types::OatsType::Void => {
                        self.context.void_type().fn_type(&param_types, false)
                    }
                    _ => self.i8ptr_t.fn_type(&param_types, false),
                }
            } else {
                let ptr_sz = self.i64_t.const_int(8u64, false);
                let off_ret = self
                    .builder
                    .build_int_add(off_env, ptr_sz, "off_ret")
                    .map_err(|_| Diagnostic::error("LLVM builder error"))?;
                let ret_i64_ptr = self
                    .i8_ptr_from_offset_i64(callee_ptr, off_ret, "closure_ret_i8")
                    .map_err(|_| Diagnostic::error("operation failed"))?;
                let ret_ptr_ty = self.context.ptr_type(inkwell::AddressSpace::default());
                let ret_ptr_cast = self
                    .builder
                    .build_pointer_cast(ret_i64_ptr, ret_ptr_ty, "ret_ptr_cast")
                    .map_err(|_| Diagnostic::error("LLVM builder error"))?;
                let ret_bv =
                    match self
                        .builder
                        .build_load(self.i64_t, ret_ptr_cast, "loaded_ret_tag")
                    {
                        Ok(v) => v,
                        Err(_) => {
                            return Err(Diagnostic::simple_boxed(
                                Severity::Error,
                                "operation failed",
                            ));
                        }
                    };
                if let BasicValueEnum::IntValue(ret_iv) = ret_bv {
                    let one = self.i64_t.const_int(1, false);
                    let two = self.i64_t.const_int(2, false);
                    let is_one = self
                        .builder
                        .build_int_compare(inkwell::IntPredicate::EQ, ret_iv, one, "ret_is_one")
                        .map_err(|_| Diagnostic::error("LLVM builder error"))?;
                    if is_one.is_const() {
                        self.f64_t.fn_type(&param_types, false)
                    } else {
                        let is_two = self
                            .builder
                            .build_int_compare(inkwell::IntPredicate::EQ, ret_iv, two, "ret_is_two")
                            .map_err(|_| Diagnostic::error("LLVM builder error"))?;
                        if is_two.is_const() {
                            self.i8ptr_t.fn_type(&param_types, false)
                        } else {
                            self.context.void_type().fn_type(&param_types, false)
                        }
                    }
                } else {
                    self.i8ptr_t.fn_type(&param_types, false)
                }
            };

            let fn_ptr_ty = self.context.ptr_type(inkwell::AddressSpace::default());
            let fn_ptr_cast = self
                .builder
                .build_pointer_cast(fn_ptr_pv, fn_ptr_ty, "fn_ptr_to_fnptr_cast")
                .map_err(|_| Diagnostic::error("LLVM builder error"))?;

            let mut call_args: Vec<inkwell::values::BasicMetadataValueEnum> = Vec::new();
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
                    return Err(Diagnostic::simple_boxed(
                        Severity::Error,
                        "operation failed",
                    ));
                }
            };
            let either = cs.try_as_basic_value();
            if let inkwell::values::ValueKind::Basic(bv) = either {
                self.last_expr_origin_local.borrow_mut().take();
                return Ok(bv);
            }
            let zero = self.f64_t.const_float(0.0);
            self.last_expr_origin_local.borrow_mut().take();
            return Ok(zero.as_basic_value_enum());
        }

        Err(Diagnostic::simple_with_span_boxed(
            Severity::Error,
            "unsupported closure call (indirect call lowering failed)",
            call.span.start as usize,
        ))
    }

    /// Lower super method calls (`super.method()`).
    ///
    /// This calls the parent class's method function instead of the current class's.
    fn lower_super_method_call_direct(
        &self,
        method_name: &str,
        call: &CallExpr,
        function: inkwell::values::FunctionValue<'a>,
        param_map: &HashMap<String, u32>,
        locals: &mut Vec<HashMap<String, LocalEntry<'a>>>,
    ) -> crate::diagnostics::DiagnosticResult<inkwell::values::BasicValueEnum<'a>> {
        let function_name = function.get_name().to_str().unwrap_or_default();
        let current_class = if let Some(underscore_pos) = function_name.rfind('_') {
            let prefix = &function_name[..underscore_pos];
            let suffix = &function_name[underscore_pos + 1..];
            if suffix == "ctor"
                || suffix == "init"
                || (suffix == "impl" && prefix.ends_with("_ctor"))
            {
                if suffix == "impl" {
                    if let Some(ctor_pos) = prefix.rfind('_') {
                        prefix[..ctor_pos].to_string()
                    } else {
                        prefix.to_string()
                    }
                } else {
                    prefix.to_string()
                }
            } else {
                prefix.to_string()
            }
        } else {
            if let Some(parent) = self.current_class_parent.borrow().as_ref() {
                let class_parents = self.class_parents.borrow();
                for (_class_name, class_parent) in class_parents.iter() {
                    if class_parent.as_ref() == Some(parent) {
                        return self.call_parent_method(
                            parent,
                            method_name,
                            call,
                            function,
                            param_map,
                            locals,
                        );
                    }
                }
            }
            return Err(Diagnostic::simple_with_span_boxed(
                Severity::Error,
                format!(
                    "cannot determine class from function name '{function_name}'"
                ),
                call.span.start as usize,
            ));
        };

        let class_parents = self.class_parents.borrow();
        let parent = if let Some(parent_opt) = class_parents.get(&current_class) {
            if let Some(p) = parent_opt {
                p.clone()
            } else {
                return Err(Diagnostic::simple_with_span_boxed(
                    Severity::Error,
                    format!("class '{current_class}' has no parent class"),
                    call.span.start as usize,
                ));
            }
        } else {
            return Err(Diagnostic::simple_with_span_boxed(
                Severity::Error,
                format!("class '{current_class}' not found in class hierarchy"),
                call.span.start as usize,
            ));
        };
        drop(class_parents);

        self.call_parent_method(&parent, method_name, call, function, param_map, locals)
    }

    /// Helper to call a parent class method
    fn call_parent_method(
        &self,
        parent: &str,
        method_name: &str,
        call: &CallExpr,
        function: inkwell::values::FunctionValue<'a>,
        param_map: &HashMap<String, u32>,
        locals: &mut Vec<HashMap<String, LocalEntry<'a>>>,
    ) -> crate::diagnostics::DiagnosticResult<inkwell::values::BasicValueEnum<'a>> {
        let this_ptr = if let Some(pv) = function.get_nth_param(0) {
            if let BasicValueEnum::PointerValue(ptr) = pv {
                ptr
            } else {
                return Err(Diagnostic::simple_with_span_boxed(
                    Severity::Error,
                    "super used but 'this' parameter is not a pointer",
                    call.span.start as usize,
                ));
            }
        } else {
            return Err(Diagnostic::simple_with_span_boxed(
                Severity::Error,
                "super used in function with no 'this' parameter",
                call.span.start as usize,
            ));
        };

        let mut call_args: Vec<inkwell::values::BasicMetadataValueEnum> = Vec::new();
        call_args.push(this_ptr.into());
        for a in &call.arguments {
            if let Some(expr) = a.as_expression() {
                let val = match self.lower_expr(expr, function, param_map, locals) {
                    Ok(v) => v,
                    Err(d) => return Err(d)?,
                };
                call_args.push(val.into());
            } else {
                return Err(Diagnostic::simple_boxed(
                    Severity::Error,
                    "unsupported argument type",
                ))?;
            }
        }

        let method_fn_name = format!("{parent}_{method_name}");
        if let Some(method_fn) = self.module.get_function(&method_fn_name) {
            let cs = match self
                .builder
                .build_call(method_fn, &call_args, "super_method_call")
            {
                Ok(cs) => cs,
                Err(_) => {
                    return Err(Diagnostic::simple_with_span_boxed(
                        Severity::Error,
                        format!("super method call '{method_fn_name}' failed"),
                        call.span.start as usize,
                    ));
                }
            };
            let either = cs.try_as_basic_value();
            if let inkwell::values::ValueKind::Basic(bv) = either {
                Ok(bv)
            } else {
                let zero = self.f64_t.const_float(0.0);
                Ok(zero.as_basic_value_enum())
            }
        } else {
            Err(Diagnostic::simple_with_span_boxed(
                Severity::Error,
                format!(
                    "super method '{method_name}' not found in parent class '{parent}'"
                ),
                call.span.start as usize,
            ))
        }
    }
}
