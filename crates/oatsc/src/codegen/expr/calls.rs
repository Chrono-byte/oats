use crate::codegen::CodeGen;
use crate::diagnostics::{Diagnostic, Severity};
use crate::types::OatsType;
use inkwell::AddressSpace;
use inkwell::types::BasicTypeEnum;
use inkwell::values::{BasicValue, BasicValueEnum, PointerValue};
use oats_ast::*;
use std::collections::HashMap;

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
        if let Callee::Super(_) = &call.callee {
            // Lower `super(...)` calls: call parent's `<Parent>_init(this, ...args)`
            let parent_opt = self.current_class_parent.borrow().clone();
            let parent = if let Some(p) = parent_opt {
                p
            } else {
                return Err(Diagnostic::simple_with_span_boxed(
                    Severity::Error,
                    "super used outside of constructor or no parent",
                    call.span.start,
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
                for a in &call.args {
                    if let Ok(v) = self.lower_expr(a, function, param_map, locals) {
                        args.push(v.into());
                    } else {
                        return Err(Diagnostic::simple_boxed(
                            Severity::Error,
                            "expression lowering failed",
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
                    if let inkwell::Either::Left(bv) = either {
                        Ok(bv)
                    } else {
                        let zero = self.f64_t.const_float(0.0);
                        Ok(zero.as_basic_value_enum())
                    }
                } else {
                    Err(Diagnostic::simple_with_span_boxed(
                        Severity::Error,
                        format!("super target '{}' not found", init_name),
                        call.span.start,
                    ))
                }
            } else {
                Err(Diagnostic::simple_with_span_boxed(
                    Severity::Error,
                    "super used but `this` not found",
                    call.span.start,
                ))
            }
        } else if let Callee::Expr(boxed_expr) = &call.callee {
            match &**boxed_expr {
                Expr::Member(member) => {
                    // Handle super.method() calls - call parent class method
                    if let Expr::Super(_) = &*member.obj {
                        return self
                            .lower_super_method_call(member, call, function, param_map, locals);
                    }
                    // Handle member calls like console.log() or Temporal.now()
                    if let Expr::Ident(obj_ident) = &*member.obj {
                        let obj_name = obj_ident.sym.to_string();

                        // If the identifier is a local variable, treat this as
                        // an instance method call (e.g. `p.sum()`), otherwise
                        // treat it as a potential std namespace (e.g. `Math.max`).
                        if self.find_local(locals, &obj_name).is_some() {
                            // Handle as complex member call on a local value.
                            use oats_ast::*;

                            // Support both direct property access (obj.prop()) and computed property access (obj[prop]())
                            let method_name = match &member.prop {
                                MemberProp::Ident(prop_ident) => prop_ident.sym.clone(),
                                MemberProp::PrivateName(_) => {
                                    return Err(Box::new(
                                        Diagnostic::simple_with_span(
                                            Severity::Error,
                                            "private name property calls not supported",
                                            member.span.start,
                                        )
                                        .with_note("Private name properties are not yet supported in method calls")
                                    ));
                                }
                                MemberProp::Computed(computed_expr) => {
                                    // Try to evaluate the computed property as a compile-time constant
                                    // Support string literals: obj["methodName"]()
                                    let span_start = member.span.start;
                                    match crate::codegen::const_eval::eval_const_expr(
                                        computed_expr,
                                        span_start,
                                        &std::collections::HashMap::new(),
                                    ) {
                                        Ok(crate::codegen::const_eval::ConstValue::Str(s)) => s,
                                        Ok(_) => {
                                            return Err(Box::new(
                                                Diagnostic::simple_with_span(
                                                    Severity::Error,
                                                    "computed property must be a string literal",
                                                    span_start,
                                                )
                                                .with_note("Only string literal computed properties are supported (e.g., obj[\"method\"]())")
                                                .with_help("Use a string literal: obj[\"methodName\"]() instead of obj[prop]()")
                                            ));
                                        }
                                        Err(_) => {
                                            return Err(Box::new(
                                                Diagnostic::simple_with_span(
                                                    Severity::Error,
                                                    "computed property must be a compile-time constant string",
                                                    span_start,
                                                )
                                                .with_note("Only string literal computed properties are supported (e.g., obj[\"method\"]())")
                                                .with_help("Use a string literal: obj[\"methodName\"]() instead of a variable")
                                            ));
                                        }
                                    }
                                }
                            };

                            if let Ok(obj_val) =
                                self.lower_expr(&member.obj, function, param_map, locals)
                            {
                                // Support weak reference helpers: downgrade()/upgrade()
                                if method_name == "downgrade" {
                                    if !call.args.is_empty() {
                                        return Err(Diagnostic::simple_boxed(
                                            Severity::Error,
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
                                        return Err(Diagnostic::simple_boxed(
                                            Severity::Error,
                                            "downgrade on non-pointer",
                                        ))?;
                                    }
                                }
                                if method_name == "upgrade" {
                                    if !call.args.is_empty() {
                                        return Err(Diagnostic::simple_boxed(
                                            Severity::Error,
                                            "expression lowering failed",
                                        ))?;
                                    }
                                    if let BasicValueEnum::PointerValue(pv) = obj_val {
                                        let f = self.get_rc_weak_upgrade();
                                        let cs = self.builder.build_call(
                                            f,
                                            &[pv.into()],
                                            "rc_weak_upgrade_call",
                                        );
                                        if let Ok(cs) = cs
                                            && let inkwell::Either::Left(bv) =
                                                cs.try_as_basic_value()
                                        {
                                            return Ok(bv);
                                        }
                                        return Err(Diagnostic::simple_boxed(
                                            Severity::Error,
                                            "upgrade failed",
                                        ))?;
                                    } else {
                                        return Err(Diagnostic::simple_boxed(
                                            Severity::Error,
                                            "upgrade on non-pointer",
                                        ))?;
                                    }
                                }

                                // Lower args and attempt to call a class method if present
                                let mut call_args: Vec<inkwell::values::BasicMetadataValueEnum> =
                                    Vec::new();
                                call_args.push(obj_val.into());
                                for a in &call.args {
                                    let val = match self.lower_expr(a, function, param_map, locals)
                                    {
                                        Ok(v) => v,
                                        Err(d) => return Err(d)?,
                                    };
                                    call_args.push(val.into());
                                }

                                // Check nominal on local to resolve class method name
                                if let Some((_, _, _, _, _, nominal_opt, _)) =
                                    self.find_local(locals, &obj_name)
                                    && let Some(nom) = nominal_opt
                                {
                                    // Try to devirtualize using RTA if available
                                    let method_fn_name = if let Some(ref rta) = self.rta_results {
                                        if let Some(devirt_target) =
                                            rta.can_devirtualize(&nom, &method_name)
                                        {
                                            devirt_target
                                        } else {
                                            format!("{}_{}", nom, method_name)
                                        }
                                    } else {
                                        format!("{}_{}", nom, method_name)
                                    };

                                    if let Some(method_fn) =
                                        self.module.get_function(&method_fn_name)
                                    {
                                        let cs = match self.builder.build_call(
                                            method_fn,
                                            &call_args,
                                            "class_method_call",
                                        ) {
                                            Ok(cs) => cs,
                                            Err(_) => {
                                                return Err(Box::new(
                                                Diagnostic::simple(
                                                    Severity::Error,
                                                    "class method call failed",
                                                )
                                                .with_note(format!("Failed to call method '{}' on class '{}'", method_name, nom))
                                                .with_help(format!("Ensure the method '{}' exists in class '{}'", method_name, nom))
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

                                // Fallback: fall through to error (no std handler for locals)
                                return Err(Box::new(
                                Diagnostic::simple(
                                    Severity::Error,
                                    format!("method '{}' not found on object '{}'", method_name, obj_name),
                                )
                                .with_note(format!("The object '{}' does not have a method named '{}'", obj_name, method_name))
                                .with_help(format!("Check that '{}' is a class instance with method '{}', or use optional chaining: {}.?{}()", obj_name, method_name, obj_name, method_name))
                            ));
                            } else {
                                return Err(Box::new(
                                    Diagnostic::simple(
                                        Severity::Error,
                                        format!("failed to lower object expression for method call '{}'", method_name),
                                    )
                                    .with_note(format!("The object expression '{}' could not be evaluated", obj_name))
                                    .with_help("Ensure the object is properly initialized before calling methods on it")
                                ));
                            }
                        } else {
                            // Non-local object - handled below in std namespace section
                        }

                        // Support both direct property access and computed property access (string literals)
                        // This handles std namespace calls like Math.max() or Math["max"]()
                        let prop_name = match &member.prop {
                            MemberProp::Ident(prop_ident) => prop_ident.sym.clone(),
                            MemberProp::PrivateName(_) => {
                                return Err(Diagnostic::simple_with_span_boxed(
                                    Severity::Error,
                                    "private name property calls not supported",
                                    member.span.start,
                                ));
                            }
                            MemberProp::Computed(computed_expr) => {
                                // Try to evaluate the computed property as a compile-time constant
                                // Support string literals: Math["max"]()
                                match crate::codegen::const_eval::eval_const_expr(
                                    computed_expr,
                                    member.span.start,
                                    &std::collections::HashMap::new(),
                                ) {
                                    Ok(crate::codegen::const_eval::ConstValue::Str(s)) => s,
                                    Ok(_) => {
                                        return Err(Diagnostic::simple_with_span_boxed(
                                            Severity::Error,
                                            "computed property must be a string literal",
                                            member.span.start,
                                        ));
                                    }
                                    Err(_) => {
                                        return Err(Diagnostic::simple_with_span_boxed(
                                            Severity::Error,
                                            "computed property must be a compile-time constant string",
                                            member.span.start,
                                        ));
                                    }
                                }
                            }
                        };

                        if obj_name == "Promise" && prop_name == "resolve" && call.args.len() == 1 {
                            // Lower the single argument
                            let arg_val =
                                match self.lower_expr(&call.args[0], function, param_map, locals) {
                                    Ok(v) => v,
                                    Err(_) => {
                                        return Err(Diagnostic::simple_boxed(
                                            Severity::Error,
                                            "expression lowering failed",
                                        ))?;
                                    }
                                };

                            // Ensure we pass an `i8*` payload to the runtime.
                            let payload_ptr = match arg_val {
                                BasicValueEnum::PointerValue(pv) => pv.as_basic_value_enum(),
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
                                            return Err(Diagnostic::simple_boxed(
                                                Severity::Error,
                                                "union_box_f64 call failed",
                                            ))?;
                                        }
                                    };
                                    match cs.try_as_basic_value() {
                                        inkwell::Either::Left(bv) => bv,
                                        _ => {
                                            return Err(Diagnostic::simple_boxed(
                                                Severity::Error,
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
                                    return Err(Diagnostic::simple_boxed(
                                        Severity::Error,
                                        "promise_resolve call failed",
                                    ))?;
                                }
                            };
                            match cs.try_as_basic_value() {
                                inkwell::Either::Left(bv) => Ok(bv),
                                _ => Err(Diagnostic::simple_boxed(
                                    Severity::Error,
                                    "promise_resolve returned non-value",
                                )),
                            }
                        } else {
                            let fname = format!("oats_std_{}_{}", obj_name, prop_name);

                            // Lower args
                            let mut lowered_args: Vec<inkwell::values::BasicMetadataValueEnum> =
                                Vec::new();
                            for a in &call.args {
                                if let Ok(val) = self.lower_expr(a, function, param_map, locals) {
                                    lowered_args.push(val.into());
                                } else {
                                    return Err(Diagnostic::simple_boxed(
                                        Severity::Error,
                                        "expression lowering failed",
                                    ))?;
                                }
                            }

                            if let Some(fv) = self.module.get_function(&fname) {
                                let cs =
                                    match self.builder.build_call(fv, &lowered_args, "std_call") {
                                        Ok(cs) => cs,
                                        Err(_) => {
                                            return Err(Diagnostic::simple_boxed(
                                                Severity::Error,
                                                "operation failed",
                                            ));
                                        }
                                    };
                                let either = cs.try_as_basic_value();
                                if let inkwell::Either::Left(bv) = either {
                                    Ok(bv)
                                } else {
                                    let zero = self.f64_t.const_float(0.0);
                                    Ok(zero.as_basic_value_enum())
                                }
                            } else {
                                Err(Diagnostic::simple_with_span_boxed(
                                    Severity::Error,
                                    format!("std function '{}' not found", fname),
                                    call.span.start,
                                ))
                            }
                        }
                    } else {
                        // Handle complex member calls (obj.method())
                        use oats_ast::*;
                        if let MemberProp::Ident(prop_ident) = &member.prop {
                            let method_name = prop_ident.sym.clone();

                            // Lower `Promise.resolve(x)` by calling the runtime
                            // helper `promise_resolve(i8*) -> i8*` so resolved
                            // promises are represented by runtime objects. If
                            // the argument is a numeric value we box it with
                            // `union_box_f64` so the runtime consistently
                            // receives an `i8*` payload.
                            if method_name == "resolve"
                                && call.args.len() == 1
                                && let Expr::Ident(ident) = &*member.obj
                                && ident.sym == "Promise"
                            {
                                // Lower the single argument
                                let arg_val = match self.lower_expr(
                                    &call.args[0],
                                    function,
                                    param_map,
                                    locals,
                                ) {
                                    Ok(v) => v,
                                    Err(_) => {
                                        return Err(Diagnostic::simple_boxed(
                                            Severity::Error,
                                            "expression lowering failed",
                                        ))?;
                                    }
                                };

                                // Ensure we pass an `i8*` payload to the runtime.
                                let payload_ptr = match arg_val {
                                    BasicValueEnum::PointerValue(pv) => pv.as_basic_value_enum(),
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
                                                return Err(Diagnostic::simple_boxed(
                                                    Severity::Error,
                                                    "union_box_f64 call failed",
                                                ))?;
                                            }
                                        };
                                        match cs.try_as_basic_value() {
                                            inkwell::Either::Left(bv) => bv,
                                            _ => {
                                                return Err(Diagnostic::simple_boxed(
                                                    Severity::Error,
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
                                        return Err(Diagnostic::simple_boxed(
                                            Severity::Error,
                                            "promise_resolve call failed",
                                        ))?;
                                    }
                                };
                                match cs.try_as_basic_value() {
                                    inkwell::Either::Left(bv) => return Ok(bv),
                                    _ => {
                                        return Err(Diagnostic::simple_boxed(
                                            Severity::Error,
                                            "promise_resolve returned non-value",
                                        ));
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
                                        return Err(Diagnostic::simple_boxed(
                                            Severity::Error,
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
                                        return Err(Diagnostic::simple_boxed(
                                            Severity::Error,
                                            "downgrade on non-pointer",
                                        ))?;
                                    }
                                }
                                if method_name == "upgrade" {
                                    // Expect no args
                                    if !call.args.is_empty() {
                                        return Err(Diagnostic::simple_boxed(
                                            Severity::Error,
                                            "expression lowering failed",
                                        ))?;
                                    }
                                    if let BasicValueEnum::PointerValue(pv) = obj_val {
                                        let f = self.get_rc_weak_upgrade();
                                        let cs = self.builder.build_call(
                                            f,
                                            &[pv.into()],
                                            "rc_weak_upgrade_call",
                                        );
                                        if let Ok(cs) = cs
                                            && let inkwell::Either::Left(bv) =
                                                cs.try_as_basic_value()
                                        {
                                            return Ok(bv);
                                        }
                                        return Err(Diagnostic::simple_boxed(
                                            Severity::Error,
                                            "upgrade failed",
                                        ))?;
                                    } else {
                                        return Err(Diagnostic::simple_boxed(
                                            Severity::Error,
                                            "upgrade on non-pointer",
                                        ))?;
                                    }
                                }

                                // General method call lowering: obj.method(args...)
                                // Infer types from argument expressions for type-directed lowering
                                let mut inferred_arg_types: Vec<crate::types::OatsType> =
                                    Vec::new();
                                // Infer type for 'this' (obj) - treat as pointer type
                                inferred_arg_types.push(crate::types::OatsType::String); // Default to pointer-like

                                for a in &call.args {
                                    // Infer type from expression before lowering
                                    let inferred = crate::types::infer_type_from_expr(a);
                                    inferred_arg_types
                                        .push(inferred.unwrap_or(crate::types::OatsType::Number));
                                }

                                // Lower all args
                                let mut call_args: Vec<inkwell::values::BasicMetadataValueEnum> =
                                    Vec::new();
                                call_args.push(obj_val.into()); // 'this' is first arg
                                for a in &call.args {
                                    let val = match self.lower_expr(a, function, param_map, locals)
                                    {
                                        Ok(v) => v,
                                        Err(d) => return Err(d)?,
                                    };
                                    call_args.push(val.into());
                                }

                                // Check if this is a class method call: obj is nominal type, method exists
                                let class_name = if let Expr::Ident(ident) = &*member.obj {
                                    let ident_name = ident.sym.to_string();
                                    if let Some((_, _, _, _, _, nominal, _)) =
                                        self.find_local(locals, &ident_name)
                                    {
                                        nominal.clone()
                                    } else {
                                        None
                                    }
                                } else {
                                    None
                                };
                                if let Some(class_name) = class_name {
                                    let method_fn_name = format!("{}_{}", class_name, method_name);
                                    if let Some(method_fn) =
                                        self.module.get_function(&method_fn_name)
                                    {
                                        // Call the class method
                                        let cs = match self.builder.build_call(
                                            method_fn,
                                            &call_args,
                                            "class_method_call",
                                        ) {
                                            Ok(cs) => cs,
                                            Err(_) => {
                                                return Err(Diagnostic::simple_boxed(
                                                    Severity::Error,
                                                    "class method call failed",
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

                                // Fallback: general method call lowering
                                // Use type-directed lowering based on inferred argument types
                                let ret_ty = self.f64_t; // Return type inference not yet implemented

                                // Convert inferred OatsTypes to LLVM parameter types
                                let param_tys: Vec<inkwell::types::BasicMetadataTypeEnum> =
                                    inferred_arg_types
                                        .iter()
                                        .map(|ty| {
                                            // Use map_type_to_llvm helper and convert to BasicMetadataTypeEnum
                                            let basic_ty = self.map_type_to_llvm(ty);
                                            basic_ty.into()
                                        })
                                        .collect();

                                let fn_ty = ret_ty.fn_type(&param_tys, false);

                                // Generate a unique name for this method call
                                let id = self.next_str_id.get();
                                self.next_str_id.set(id + 1);
                                let method_id = format!("method_{}_{}", method_name, id);
                                let method_fn = self.module.add_function(&method_id, fn_ty, None);

                                let cs =
                                    self.builder
                                        .build_call(method_fn, &call_args, "method_call");
                                if let Ok(cs) = cs {
                                    let either = cs.try_as_basic_value();
                                    if let inkwell::Either::Left(bv) = either {
                                        Ok(bv)
                                    } else {
                                        let zero = self.f64_t.const_float(0.0);
                                        Ok(zero.as_basic_value_enum())
                                    }
                                } else {
                                    Err(Diagnostic::simple_boxed(
                                        Severity::Error,
                                        "method call failed",
                                    ))
                                }
                            } else {
                                Err(Diagnostic::simple_boxed(
                                    Severity::Error,
                                    "object expression lowering failed",
                                ))
                            }
                        } else {
                            Err(Diagnostic::simple_with_span_boxed(
                                Severity::Error,
                                "computed property calls not supported",
                                call.span.start,
                            ))
                        }
                    }
                }
                // (super calls handled by Callee::Super branch below)
                _ => {
                    // Check if this is a plain function call like `print_f64(...)`
                    if let Expr::Ident(ident) = &**boxed_expr {
                        let fn_name = ident.sym.to_string();
                        if let Some(fv) = self.module.get_function(&fn_name) {
                            // Lower arguments
                            let mut call_args: Vec<inkwell::values::BasicMetadataValueEnum> =
                                Vec::new();
                            for a in &call.args {
                                let val = match self.lower_expr(a, function, param_map, locals) {
                                    Ok(v) => v,
                                    Err(d) => return Err(d)?,
                                };
                                call_args.push(val.into());
                            }
                            // Call the function
                            let cs = match self.builder.build_call(fv, &call_args, "function_call")
                            {
                                Ok(cs) => cs,
                                Err(_) => {
                                    return Err(Diagnostic::simple_boxed(
                                        Severity::Error,
                                        "function call failed",
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
                        // If the identifier corresponds to a nested generic function
                        // that we registered earlier, attempt call-site monomorphization
                        // to generate a specialized instance name and declare it so
                        // the IR contains the specialization name (tests look for it).
                        let has_nested = self.nested_generic_fns.borrow().contains_key(&fn_name);
                        if has_nested {
                            // Try to infer type arguments from the first argument if it's an array
                            if !call.args.is_empty() {
                                let inferred = crate::types::infer_type(None, Some(&call.args[0]));
                                if let crate::types::OatsType::Array(elem_ty) = inferred {
                                    // Monomorphize and get specialized name
                                    if let Ok(spec_name) =
                                        self.monomorphize_function(&fn_name, &[(*elem_ty).clone()])
                                    {
                                        // Declare a placeholder function so the name appears in IR
                                        if self.module.get_function(&spec_name).is_none() {
                                            let fn_ty =
                                                self.i8ptr_t.fn_type(&[self.i8ptr_t.into()], false);
                                            let _ =
                                                self.module.add_function(&spec_name, fn_ty, None);
                                        }
                                        // Now call the specialized function (pass lowered args)
                                        let mut call_args: Vec<
                                            inkwell::values::BasicMetadataValueEnum,
                                        > = Vec::new();
                                        for a in &call.args {
                                            let val = match self
                                                .lower_expr(a, function, param_map, locals)
                                            {
                                                Ok(v) => v,
                                                Err(d) => return Err(d)?,
                                            };
                                            call_args.push(val.into());
                                        }
                                        if let Some(spec_fv) = self.module.get_function(&spec_name)
                                        {
                                            let cs = match self.builder.build_call(
                                                spec_fv,
                                                &call_args,
                                                "generic_mono_call",
                                            ) {
                                                Ok(cs) => cs,
                                                Err(_) => {
                                                    return Err(Diagnostic::simple_boxed(
                                                        Severity::Error,
                                                        "generic monomorphized call failed",
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
                        }
                    }
                    // Attempt to lower the callee as a general expression.
                    // This covers calling closure objects (layout: [header][meta][fn_ptr][env_ptr]).
                    if let Ok(callee_val) = self.lower_expr(boxed_expr, function, param_map, locals)
                    {
                        if let BasicValueEnum::PointerValue(callee_ptr) = callee_val {
                            // compute offsets for fn_ptr (idx 0) and env_ptr (idx 1)
                            let header_size = self.i64_t.const_int(8u64, false);
                            let meta_slot = self.i64_t.const_int(8u64, false);
                            let ptr_sz = self.i64_t.const_int(8u64, false);
                            let off_fn = self
                                .builder
                                .build_int_add(header_size, meta_slot, "hdr_plus_meta")
                                .map_err(|_| Diagnostic::error("LLVM builder error"))?;
                            // fn_ptr offset = header + meta_slot + 0*8 == off_fn
                            let fn_ptr_i8 = self
                                .i8_ptr_from_offset_i64(callee_ptr, off_fn, "closure_fn_i8")
                                .map_err(|_| Diagnostic::error("operation failed"))?;
                            // env_ptr offset = off_fn + 8
                            let off_env = self
                                .builder
                                .build_int_add(off_fn, ptr_sz, "off_env")
                                .map_err(|_| Diagnostic::error("LLVM builder error"))?;
                            let env_ptr_i8 = self
                                .i8_ptr_from_offset_i64(callee_ptr, off_env, "closure_env_i8")
                                .map_err(|_| Diagnostic::error("operation failed"))?;

                            // bitcast fn_ptr_i8 to pointer-to-pointer (i8**), load the stored function pointer
                            let fn_ptr_slot_ty = self.context.ptr_type(AddressSpace::default());
                            let fn_ptr_slot = self
                                .builder
                                .build_pointer_cast(fn_ptr_i8, fn_ptr_slot_ty, "fn_ptr_slot_cast")
                                .map_err(|_| Diagnostic::error("LLVM builder error"))?;
                            let fn_ptr_bv = match self.builder.build_load(
                                self.i8ptr_t,
                                fn_ptr_slot,
                                "loaded_fn_ptr",
                            ) {
                                Ok(v) => v,
                                Err(_) => {
                                    return Err(Diagnostic::simple_boxed(
                                        Severity::Error,
                                        "operation failed",
                                    ));
                                }
                            };

                            // bitcast env_ptr_i8 similarly and load env pointer
                            let env_slot_ty = self.context.ptr_type(AddressSpace::default());
                            let env_slot = self
                                .builder
                                .build_pointer_cast(env_ptr_i8, env_slot_ty, "env_slot_cast")
                                .map_err(|_| Diagnostic::error("LLVM builder error"))?;
                            let env_bv = match self.builder.build_load(
                                self.i8ptr_t,
                                env_slot,
                                "loaded_env_ptr",
                            ) {
                                Ok(v) => v,
                                Err(_) => {
                                    return Err(Diagnostic::simple_boxed(
                                        Severity::Error,
                                        "operation failed",
                                    ));
                                }
                            };

                            // Prepare user args lowered
                            let mut lowered_args: Vec<inkwell::values::BasicMetadataValueEnum> =
                                Vec::new();
                            for a in &call.args {
                                if let Ok(val) = self.lower_expr(a, function, param_map, locals) {
                                    lowered_args.push(val.into());
                                } else {
                                    return Err(Diagnostic::simple_boxed(
                                        Severity::Error,
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
                                self.last_expr_origin_local.borrow().clone().and_then(|n| {
                                    self.closure_local_rettype.borrow().get(&n).cloned()
                                });

                            if let BasicValueEnum::PointerValue(fn_ptr_pv) = fn_ptr_bv {
                                // construct parameter list
                                let mut param_types: Vec<inkwell::types::BasicMetadataTypeEnum> =
                                    Vec::new();
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
                                        crate::types::OatsType::Void => {
                                            self.context.void_type().fn_type(&param_types, false)
                                        }
                                        _ => self.i8ptr_t.fn_type(&param_types, false),
                                    }
                                } else {
                                    // Fallback: read runtime tag from closure object (idx 2)
                                    let ptr_sz = self.i64_t.const_int(8u64, false);
                                    let off_ret = self
                                        .builder
                                        .build_int_add(off_env, ptr_sz, "off_ret")
                                        .map_err(|_| Diagnostic::error("LLVM builder error"))?;
                                    let ret_i64_ptr = self
                                        .i8_ptr_from_offset_i64(
                                            callee_ptr,
                                            off_ret,
                                            "closure_ret_i8",
                                        )
                                        .map_err(|_| Diagnostic::error("operation failed"))?;
                                    let ret_ptr_ty =
                                        self.context.ptr_type(inkwell::AddressSpace::default());
                                    let ret_ptr_cast = self
                                        .builder
                                        .build_pointer_cast(ret_i64_ptr, ret_ptr_ty, "ret_ptr_cast")
                                        .map_err(|_| Diagnostic::error("LLVM builder error"))?;
                                    let ret_bv = match self.builder.build_load(
                                        self.i64_t,
                                        ret_ptr_cast,
                                        "loaded_ret_tag",
                                    ) {
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
                                            .build_int_compare(
                                                inkwell::IntPredicate::EQ,
                                                ret_iv,
                                                one,
                                                "ret_is_one",
                                            )
                                            .map_err(|_| Diagnostic::error("LLVM builder error"))?;
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
                                                    Diagnostic::error("LLVM builder error")
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
                                    .map_err(|_| Diagnostic::error("LLVM builder error"))?;

                                let mut call_args: Vec<inkwell::values::BasicMetadataValueEnum> =
                                    Vec::new();
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
                            Err(Diagnostic::simple_with_span_boxed(
                                Severity::Error,
                                "unsupported closure call (indirect call lowering failed)",
                                call.span.start,
                            ))
                        } else {
                            Err(Diagnostic::simple_with_span_boxed(
                                Severity::Error,
                                "unsupported callee expression",
                                call.span.start,
                            ))
                        }
                    } else {
                        Err(Diagnostic::simple_with_span_boxed(
                            Severity::Error,
                            "expression lowering failed",
                            call.span.start,
                        ))
                    }
                }
            }
        } else {
            Err(Diagnostic::simple_with_span_boxed(
                Severity::Error,
                "unsupported call expression: callee form not supported",
                call.span.start,
            ))
        }
    }

    /// Lower super method calls (super.method()).
    ///
    /// This calls the parent class's method function instead of the current class's.
    fn lower_super_method_call(
        &self,
        member: &MemberExpr,
        call: &CallExpr,
        function: inkwell::values::FunctionValue<'a>,
        param_map: &HashMap<String, u32>,
        locals: &mut Vec<HashMap<String, LocalEntry<'a>>>,
    ) -> crate::diagnostics::DiagnosticResult<inkwell::values::BasicValueEnum<'a>> {
        // Determine which class this method belongs to
        // Method functions are named as {Class}_{method}
        // Constructor functions might be named as {Class}_ctor, {Class}_ctor_impl, or {Class}_init
        let function_name = function.get_name().to_str().unwrap_or("");
        let current_class = if let Some(underscore_pos) = function_name.rfind('_') {
            let prefix = &function_name[..underscore_pos];
            let suffix = &function_name[underscore_pos + 1..];
            // Handle constructor function names: Foo_ctor, Foo_ctor_impl, Foo_init
            if suffix == "ctor"
                || suffix == "init"
                || (suffix == "impl" && prefix.ends_with("_ctor"))
            {
                // For Foo_ctor or Foo_init, the class name is the prefix
                // For Foo_ctor_impl, we need to extract Foo from Foo_ctor
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
                // Regular method: {Class}_{method}
                prefix.to_string()
            }
        } else {
            // Fallback: try current_class_parent (for constructors)
            if let Some(parent) = self.current_class_parent.borrow().as_ref() {
                // If we're in a constructor, we need to find the class that has this parent
                let class_parents = self.class_parents.borrow();
                for (_class_name, class_parent) in class_parents.iter() {
                    if class_parent.as_ref() == Some(parent) {
                        // Found a class with this parent, use the parent directly
                        return self
                            .call_parent_method(parent, member, call, function, param_map, locals);
                    }
                }
            }
            return Err(Diagnostic::simple_with_span_boxed(
                Severity::Error,
                format!(
                    "cannot determine class from function name '{}'",
                    function_name
                ),
                call.span.start,
            ));
        };

        // Get parent class from class hierarchy
        let class_parents = self.class_parents.borrow();
        let parent = if let Some(parent_opt) = class_parents.get(&current_class) {
            if let Some(p) = parent_opt {
                p.clone()
            } else {
                return Err(Diagnostic::simple_with_span_boxed(
                    Severity::Error,
                    format!("class '{}' has no parent class", current_class),
                    call.span.start,
                ));
            }
        } else {
            return Err(Diagnostic::simple_with_span_boxed(
                Severity::Error,
                format!("class '{}' not found in class hierarchy", current_class),
                call.span.start,
            ));
        };
        drop(class_parents); // Release borrow before calling method

        self.call_parent_method(&parent, member, call, function, param_map, locals)
    }

    /// Helper to call a parent class method
    fn call_parent_method(
        &self,
        parent: &str,
        member: &MemberExpr,
        call: &CallExpr,
        function: inkwell::values::FunctionValue<'a>,
        param_map: &HashMap<String, u32>,
        locals: &mut Vec<HashMap<String, LocalEntry<'a>>>,
    ) -> crate::diagnostics::DiagnosticResult<inkwell::values::BasicValueEnum<'a>> {
        // Get method name
        let method_name = if let MemberProp::Ident(prop_ident) = &member.prop {
            prop_ident.sym.clone()
        } else {
            return Err(Diagnostic::simple_with_span_boxed(
                Severity::Error,
                "super method call requires identifier property",
                call.span.start,
            ));
        };

        // Get `this` pointer
        let this_ptr = if let Some(pv) = function.get_nth_param(0) {
            if let BasicValueEnum::PointerValue(ptr) = pv {
                ptr
            } else {
                return Err(Diagnostic::simple_with_span_boxed(
                    Severity::Error,
                    "super used but 'this' parameter is not a pointer",
                    call.span.start,
                ));
            }
        } else {
            return Err(Diagnostic::simple_with_span_boxed(
                Severity::Error,
                "super used in function with no 'this' parameter",
                call.span.start,
            ));
        };

        // Build call arguments: this + call.args
        let mut call_args: Vec<inkwell::values::BasicMetadataValueEnum> = Vec::new();
        call_args.push(this_ptr.into());
        for a in &call.args {
            let val = match self.lower_expr(a, function, param_map, locals) {
                Ok(v) => v,
                Err(d) => return Err(d)?,
            };
            call_args.push(val.into());
        }

        // Call parent class method: {Parent}_{method_name}
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
                        call.span.start,
                    ));
                }
            };
            let either = cs.try_as_basic_value();
            if let inkwell::Either::Left(bv) = either {
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
                call.span.start,
            ))
        }
    }
}
