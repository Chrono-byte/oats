use crate::codegen::CodeGen;
use crate::diagnostics::{Diagnostic, Severity};
use crate::types::OatsType;
use inkwell::AddressSpace;
use inkwell::types::BasicTypeEnum;
use inkwell::values::{BasicValue, BasicValueEnum, PointerValue};
use oxc_ast::ast::*;
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

            let init_name = format!("{}_init", parent);
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
                        format!("super target '{}' not found", init_name),
                    ))
                }
            } else {
                Err(Diagnostic::simple_boxed(
                    Severity::Error,
                    format!("super target '{}' not found", init_name),
                ))
            }
        } else {
            Err(Diagnostic::simple_boxed(
                Severity::Error,
                "super used but `this` not found",
            ))
        }
    }

    /// Lower static member expression calls (obj.method() or std functions)
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
                &obj_name,
                &prop_name,
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
        } else {
            // Handle std namespace functions (e.g., Math.max, console.log)
            let fname = format!("oats_std_{}_{}", obj_name, prop_name);

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
                let cs = match self.builder.build_call(fv, &lowered_args, "std_call") {
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
                    let zero = self.f64_t.const_float(0.0);
                    Ok(zero.as_basic_value_enum())
                }
            } else {
                Err(Diagnostic::simple_with_span_boxed(
                    Severity::Error,
                    format!("std function '{}' not found", fname),
                    call.span.start as usize,
                ))
            }
        }
    }

    /// Lower instance method calls (obj.method())
    fn lower_instance_method_call(
        &self,
        obj_name: &str,
        method_name: &str,
        obj_expr: &Expression<'_>,
        call: &CallExpr,
        function: inkwell::values::FunctionValue<'a>,
        param_map: &HashMap<String, u32>,
        locals: &mut Vec<HashMap<String, LocalEntry<'a>>>,
    ) -> crate::diagnostics::DiagnosticResult<inkwell::values::BasicValueEnum<'a>> {
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
                } else {
                    return Err(Diagnostic::simple_boxed(
                        Severity::Error,
                        "downgrade on non-pointer",
                    ))?;
                }
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
                } else {
                    return Err(Diagnostic::simple_boxed(
                        Severity::Error,
                        "upgrade on non-pointer",
                    ))?;
                }
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

            // Check nominal on local to resolve class method name
            if let Some((_, _, _, _, _, nominal_opt, _)) = self.find_local(locals, obj_name)
                && let Some(nom) = nominal_opt
            {
                let method_fn_name = if let Some(ref rta) = self.rta_results {
                    if let Some(devirt_target) = rta.can_devirtualize(&nom, method_name) {
                        devirt_target
                    } else {
                        format!("{}_{}", nom, method_name)
                    }
                } else {
                    format!("{}_{}", nom, method_name)
                };

                if let Some(method_fn) = self.module.get_function(&method_fn_name) {
                    let cs =
                        match self
                            .builder
                            .build_call(method_fn, &call_args, "class_method_call")
                        {
                            Ok(cs) => cs,
                            Err(_) => {
                                return Err(Box::new(
                                    Diagnostic::simple(Severity::Error, "class method call failed")
                                        .with_note(format!(
                                            "Failed to call method '{}' on class '{}'",
                                            method_name, nom
                                        ))
                                        .with_help(format!(
                                            "Ensure the method '{}' exists in class '{}'",
                                            method_name, nom
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
                        "method '{}' not found on object '{}'",
                        method_name, obj_name
                    ),
                )
                .with_note(format!(
                    "The object '{}' does not have a method named '{}'",
                    obj_name, method_name
                ))
                .with_help(format!(
                    "Check that '{}' is a class instance with method '{}'",
                    obj_name, method_name
                )),
            ))
        } else {
            Err(Box::new(
                Diagnostic::simple(
                    Severity::Error,
                    format!(
                        "failed to lower object expression for method call '{}'",
                        method_name
                    ),
                )
                .with_note(format!(
                    "The object expression '{}' could not be evaluated",
                    obj_name
                ))
                .with_help(
                    "Ensure the object is properly initialized before calling methods on it",
                ),
            ))
        }
    }

    /// Lower plain identifier calls (foo())
    fn lower_identifier_call(
        &self,
        ident: &IdentifierReference<'_>,
        call: &CallExpr,
        function: inkwell::values::FunctionValue<'a>,
        param_map: &HashMap<String, u32>,
        locals: &mut Vec<HashMap<String, LocalEntry<'a>>>,
    ) -> crate::diagnostics::DiagnosticResult<inkwell::values::BasicValueEnum<'a>> {
        let fn_name = ident.name.to_string();

        // Check if it's a direct function call
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
            let inferred = crate::types::infer_type(None, Some(first_arg_expr));
            if let crate::types::OatsType::Array(elem_ty) = inferred
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
            format!("function '{}' not found", fn_name),
            call.span.start as usize,
        ))
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

    /// Lower super method calls (super.method()).
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
                    "cannot determine class from function name '{}'",
                    function_name
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
                    format!("class '{}' has no parent class", current_class),
                    call.span.start as usize,
                ));
            }
        } else {
            return Err(Diagnostic::simple_with_span_boxed(
                Severity::Error,
                format!("class '{}' not found in class hierarchy", current_class),
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

        let method_fn_name = format!("{}_{}", parent, method_name);
        if let Some(method_fn) = self.module.get_function(&method_fn_name) {
            let cs = match self
                .builder
                .build_call(method_fn, &call_args, "super_method_call")
            {
                Ok(cs) => cs,
                Err(_) => {
                    return Err(Diagnostic::simple_with_span_boxed(
                        Severity::Error,
                        format!("super method call '{}' failed", method_fn_name),
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
                    "super method '{}' not found in parent class '{}'",
                    method_name, parent
                ),
                call.span.start as usize,
            ))
        }
    }
}
