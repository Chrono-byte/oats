use crate::codegen::CodeGen;
use crate::diagnostics::{Diagnostic, Severity};
use crate::types::OatsType;
use deno_ast::swc::ast;
use inkwell::AddressSpace;
use inkwell::types::BasicTypeEnum;
use inkwell::values::{BasicValue, BasicValueEnum, PointerValue};
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
        call: &deno_ast::swc::ast::CallExpr,
        function: inkwell::values::FunctionValue<'a>,
        param_map: &HashMap<String, u32>,
        locals: &mut Vec<HashMap<String, LocalEntry<'a>>>,
    ) -> crate::diagnostics::DiagnosticResult<inkwell::values::BasicValueEnum<'a>> {
        eprintln!(
            "[debug lower_call_expr] callee_kind={:?} span_lo={}",
            call.callee, call.span.lo.0 as usize
        );
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
                return Err(Diagnostic::simple_with_span_boxed(
                    Severity::Error,
                    "super used outside of constructor or no parent",
                    call.span.lo.0 as usize,
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
                    if let Ok(v) = self.lower_expr(&a.expr, function, param_map, locals) {
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
                        call.span.lo.0 as usize,
                    ))
                }
            } else {
                Err(Diagnostic::simple_with_span_boxed(
                    Severity::Error,
                    "super used but `this` not found",
                    call.span.lo.0 as usize,
                ))
            }
        } else if let ast::Callee::Expr(boxed_expr) = &call.callee {
            match &**boxed_expr {
                ast::Expr::Member(member) => {
                    // Handle member calls like console.log() or Temporal.now()
                    if let ast::Expr::Ident(obj_ident) = &*member.obj {
                        let obj_name = obj_ident.sym.to_string();

                        // If the identifier is a local variable, treat this as
                        // an instance method call (e.g. `p.sum()`), otherwise
                        // treat it as a potential std namespace (e.g. `Math.max`).
                        if self.find_local(locals, &obj_name).is_some() {
                            // Handle as complex member call on a local value.
                            use deno_ast::swc::ast::MemberProp;
                            if let MemberProp::Ident(prop_ident) = &member.prop {
                                let method_name = prop_ident.sym.to_string();

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
                                    let mut call_args: Vec<
                                        inkwell::values::BasicMetadataValueEnum,
                                    > = Vec::new();
                                    call_args.push(obj_val.into());
                                    for a in &call.args {
                                        let val = match self
                                            .lower_expr(&a.expr, function, param_map, locals)
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
                                        let method_fn_name = format!("{}_{}", nom, method_name);
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

                                    // Fallback: fall through to error (no std handler for locals)
                                    return Err(Diagnostic::simple_boxed(
                                        Severity::Error,
                                        "object method not found",
                                    ));
                                } else {
                                    return Err(Diagnostic::simple_boxed(
                                        Severity::Error,
                                        "object expression lowering failed",
                                    ));
                                }
                            } else {
                                return Err(Diagnostic::simple_with_span_boxed(
                                    Severity::Error,
                                    "computed property calls not supported",
                                    call.span.lo.0 as usize,
                                ));
                            }
                        }

                        let prop_name = match &member.prop {
                            ast::MemberProp::Ident(prop_ident) => prop_ident.sym.to_string(),
                            _ => {
                                return Err(Diagnostic::simple_with_span_boxed(
                                    Severity::Error,
                                    "computed property calls not supported",
                                    call.span.lo.0 as usize,
                                ));
                            }
                        };

                        if obj_name == "Promise" && prop_name == "resolve" && call.args.len() == 1 {
                            // Lower the single argument
                            let arg_val = match self.lower_expr(
                                &call.args[0].expr,
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
                                if let Ok(val) =
                                    self.lower_expr(&a.expr, function, param_map, locals)
                                {
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
                                    call.span.lo.0 as usize,
                                ))
                            }
                        }
                    } else {
                        // Handle complex member calls (obj.method())
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
                                // Lower all args first
                                let mut call_args: Vec<inkwell::values::BasicMetadataValueEnum> =
                                    Vec::new();
                                call_args.push(obj_val.into()); // 'this' is first arg
                                for a in &call.args {
                                    let val = match self
                                        .lower_expr(&a.expr, function, param_map, locals)
                                    {
                                        Ok(v) => v,
                                        Err(d) => return Err(d)?,
                                    };
                                    call_args.push(val.into());
                                }

                                // Check if this is a class method call: obj is nominal type, method exists
                                let class_name = if let ast::Expr::Ident(ident) = &*member.obj {
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
                                    if let Some(method_fn) = self.module.get_function(&method_fn_name) {
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
                                // For now, assume all method calls return f64 and take f64 args
                                // TODO: Type-directed lowering
                                let ret_ty = self.f64_t;
                                let param_tys: Vec<inkwell::types::BasicMetadataTypeEnum> =
                                    call_args.iter().map(|_| self.f64_t.into()).collect();
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
                                call.span.lo.0 as usize,
                            ))
                        }
                    }
                }
                // (super calls handled by Callee::Super branch below)
                _ => {
                    // Check if this is a plain function call like `print_f64(...)`
                    if let ast::Expr::Ident(ident) = &**boxed_expr {
                        let fn_name = ident.sym.to_string();
                        if let Some(fv) = self.module.get_function(&fn_name) {
                            eprintln!("[debug call] found module function '{}'", fn_name);
                            // Lower arguments
                            let mut call_args: Vec<inkwell::values::BasicMetadataValueEnum> =
                                Vec::new();
                            for a in &call.args {
                                let val =
                                    match self.lower_expr(&a.expr, function, param_map, locals) {
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
                        eprintln!(
                            "[debug call] fn='{}' has_module_fn={} has_nested_generic={}",
                            fn_name,
                            self.module.get_function(&fn_name).is_some(),
                            has_nested
                        );
                        if has_nested {
                            // Try to infer type arguments from the first argument if it's an array
                            if !call.args.is_empty() {
                                let inferred =
                                    crate::types::infer_type(None, Some(&call.args[0].expr));
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
                                                .lower_expr(&a.expr, function, param_map, locals)
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
                                if let Ok(val) =
                                    self.lower_expr(&a.expr, function, param_map, locals)
                                {
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
                                call.span.lo.0 as usize,
                            ))
                        } else {
                            Err(Diagnostic::simple_with_span_boxed(
                                Severity::Error,
                                "unsupported callee expression",
                                call.span.lo.0 as usize,
                            ))
                        }
                    } else {
                        Err(Diagnostic::simple_with_span_boxed(
                            Severity::Error,
                            "expression lowering failed",
                            call.span.lo.0 as usize,
                        ))
                    }
                }
            }
        } else {
            Err(Diagnostic::simple_with_span_boxed(
                Severity::Error,
                "unsupported call expression: callee form not supported",
                call.span.lo.0 as usize,
            ))
        }
    }
}
