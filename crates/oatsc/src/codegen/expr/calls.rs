use crate::codegen::CodeGen;
use crate::diagnostics::Diagnostic;
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
                    Err(_) => return Err(Diagnostic::simple_boxed("operation failed")),
                };

                let mut args: Vec<inkwell::values::BasicMetadataValueEnum> = Vec::new();
                args.push(this_loaded.into());
                for a in &call.args {
                    if let Ok(v) = self.lower_expr(&a.expr, function, param_map, locals) {
                        args.push(v.into());
                    } else {
                        return Err(Diagnostic::simple_boxed("expression lowering failed"))?;
                    }
                }

                let init_name = format!("{}_init", parent);
                if let Some(init_f) = self.module.get_function(&init_name) {
                    let cs = match self.builder.build_call(init_f, &args, "call_super_init") {
                        Ok(cs) => cs,
                        Err(_) => return Err(Diagnostic::simple_boxed("operation failed")),
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
                        format!("super target '{}' not found", init_name),
                        call.span.lo.0 as usize,
                    ))
                }
            } else {
                Err(Diagnostic::simple_with_span_boxed(
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
                            let val = match self.lower_expr(&a.expr, function, param_map, locals) {
                                Ok(v) => v,
                                Err(d) => return Err(d)?,
                            };

                            // Check if this is a union-typed variable that needs unboxing
                            let mut is_union = false;
                            if let deno_ast::swc::ast::Expr::Ident(ident) = &*a.expr {
                                let arg_name = ident.sym.to_string();
                                if let Some((_, _, _, _, _, _, oats_type)) =
                                    self.find_local(locals, &arg_name)
                                    && let Some(crate::types::OatsType::Union(_)) = oats_type
                                {
                                    is_union = true;
                                }
                            }

                            // If it's a union, unbox it and print the unboxed value
                            if is_union && matches!(val, BasicValueEnum::PointerValue(_)) {
                                let union_ptr = val.into_pointer_value();

                                // Get discriminant to determine type (0 = number, 1 = pointer/string)
                                let get_disc_fn = self.get_union_get_discriminant();
                                let disc_call = self
                                    .builder
                                    .build_call(get_disc_fn, &[union_ptr.into()], "union_get_disc")
                                    .map_err(|_| Diagnostic::simple("get discriminant failed"))?;

                                let disc = if let inkwell::Either::Left(bv) =
                                    disc_call.try_as_basic_value()
                                {
                                    bv.into_int_value()
                                } else {
                                    return Err(Diagnostic::simple_boxed(
                                        "discriminant call failed",
                                    ));
                                };

                                // Create blocks for number and string cases
                                let is_number_block =
                                    self.context.append_basic_block(function, "union_is_number");
                                let is_string_block =
                                    self.context.append_basic_block(function, "union_is_string");
                                let after_print_block = self
                                    .context
                                    .append_basic_block(function, "after_union_print");

                                // Compare discriminant with 0 (number)
                                let zero_i64 = self.i64_t.const_int(0, false);
                                let is_num = self
                                    .builder
                                    .build_int_compare(
                                        inkwell::IntPredicate::EQ,
                                        disc,
                                        zero_i64,
                                        "is_number",
                                    )
                                    .map_err(|_| Diagnostic::simple("compare failed"))?;

                                self.builder
                                    .build_conditional_branch(
                                        is_num,
                                        is_number_block,
                                        is_string_block,
                                    )
                                    .map_err(|_| Diagnostic::simple("branch failed"))?;

                                // Number case: unbox as f64 and print
                                self.builder.position_at_end(is_number_block);
                                let unbox_f64_fn = self.get_union_unbox_f64();
                                let unboxed_num_call = self
                                    .builder
                                    .build_call(unbox_f64_fn, &[union_ptr.into()], "unbox_f64")
                                    .map_err(|_| Diagnostic::simple("unbox f64 failed"))?;

                                if let inkwell::Either::Left(bv) =
                                    unboxed_num_call.try_as_basic_value()
                                {
                                    let num_val = bv.into_float_value();
                                    if let Some(print_fn) =
                                        self.module.get_function("print_f64_no_nl")
                                    {
                                        let _ = self
                                            .builder
                                            .build_call(
                                                print_fn,
                                                &[num_val.into()],
                                                "print_union_num",
                                            )
                                            .ok();
                                    }
                                }
                                self.builder
                                    .build_unconditional_branch(after_print_block)
                                    .map_err(|_| Diagnostic::simple("branch failed"))?;

                                // String case: unbox as pointer and print
                                self.builder.position_at_end(is_string_block);
                                let unbox_ptr_fn = self.get_union_unbox_ptr();
                                let unboxed_ptr_call = self
                                    .builder
                                    .build_call(unbox_ptr_fn, &[union_ptr.into()], "unbox_ptr")
                                    .map_err(|_| Diagnostic::simple("unbox ptr failed"))?;

                                if let inkwell::Either::Left(bv) =
                                    unboxed_ptr_call.try_as_basic_value()
                                {
                                    let str_val = bv.into_pointer_value();
                                    if let Some(print_fn) =
                                        self.module.get_function("print_str_no_nl")
                                    {
                                        let _ = self
                                            .builder
                                            .build_call(
                                                print_fn,
                                                &[str_val.into()],
                                                "print_union_str",
                                            )
                                            .ok();
                                    }
                                }
                                self.builder
                                    .build_unconditional_branch(after_print_block)
                                    .map_err(|_| Diagnostic::simple("branch failed"))?;

                                // Continue after printing union
                                self.builder.position_at_end(after_print_block);
                                continue;
                            }

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
                                    if let Some(orig) = self.last_expr_origin_local.borrow().clone()
                                    {
                                        if let Some((
                                            _ptr,
                                            _ty,
                                            _init,
                                            _is_const,
                                            _is_weak,
                                            nominal,
                                            _oats_type,
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
                                                if let Some(print_fn) =
                                                    self.module.get_function("print_str_no_nl")
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
                                            && self.class_fields.borrow().get(&orig).is_some()
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
                                                if let Some(print_fn) =
                                                    self.module.get_function("print_str_no_nl")
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
                                            if let deno_ast::swc::ast::Expr::Ident(ident) = &*a.expr
                                            {
                                                let arg_name = ident.sym.to_string();
                                                if let Some(idx) = param_map.get(&arg_name) {
                                                    // Look up param types for the current function
                                                    let fname = function
                                                        .get_name()
                                                        .to_str()
                                                        .unwrap_or("<fn>");
                                                    if let Some(param_types) =
                                                        self.fn_param_types.borrow().get(fname)
                                                        && let Some(pt) =
                                                            param_types.get(*idx as usize)
                                                        && matches!(
                                                            pt,
                                                            crate::types::OatsType::Array(_)
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
                                        // snapshot origin to decide ownership: if the argument
                                        // did not originate from a local (i.e., it's a temporary),
                                        // materialize into a temp alloca so we can rc_dec it after printing.
                                        let arg_origin =
                                            self.last_expr_origin_local.borrow().clone();
                                        if arg_origin.is_none() {
                                            // create a temp alloca to hold the temporary pointer
                                            let tmp_alloca = match self
                                                .builder
                                                .build_alloca(self.i8ptr_t, "tmp_str_alloc")
                                            {
                                                Ok(a) => a,
                                                Err(_) => {
                                                    return Err(Diagnostic::simple_boxed(
                                                        "operation failed",
                                                    ))?;
                                                }
                                            };
                                            let _ = self.builder.build_store(tmp_alloca, pv);
                                            // load for call
                                            let tmp_loaded = match self.builder.build_load(
                                                self.i8ptr_t,
                                                tmp_alloca,
                                                "tmp_loaded",
                                            ) {
                                                Ok(v) => v,
                                                Err(_) => {
                                                    return Err(Diagnostic::simple_boxed(
                                                        "operation failed",
                                                    ))?;
                                                }
                                            };
                                            let _ = self
                                                .builder
                                                .build_call(
                                                    print_fn,
                                                    &[tmp_loaded.into()],
                                                    "print_str_no_nl_call",
                                                )
                                                .ok();
                                            // rc_dec the temporary
                                            if let Some(rc_dec_fn) =
                                                self.module.get_function("rc_dec")
                                            {
                                                let _ = self
                                                    .builder
                                                    .build_call(
                                                        rc_dec_fn,
                                                        &[tmp_loaded.into()],
                                                        "rc_dec_tmp",
                                                    )
                                                    .ok();
                                            }
                                        } else {
                                            // argument is from a local; print directly
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
                                }
                                _ => {
                                    return Err(Diagnostic::simple_with_span_boxed(
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

                    // Monomorphize nested generic functions on demand.
                    if let Some((nested_fn, fsig)) =
                        self.nested_generic_fns.borrow().get(&fname).cloned()
                    {
                        use deno_ast::swc::ast;
                        // Build a substitution map from declared type-parameter
                        // identifiers to concrete OatsTypes. Extract declared
                        // type param names from the nested function AST if present.
                        let mut declared_names: Vec<String> = Vec::new();
                        if let Some(type_params) = &nested_fn.type_params {
                            for tp in &type_params.params {
                                let name = &tp.name;
                                declared_names.push(name.sym.to_string());
                            }
                        }

                        // First try to map explicit type arguments at the call-site
                        // (e.g., f::<Targs>(...)). SWC represents this as `call.type_args`.
                        let mut explicit_targs: Vec<crate::types::OatsType> = Vec::new();
                        if let Some(type_args) = &call.type_args {
                            for targ in &type_args.params {
                                // targ is Box<ast::TsType>
                                if let Some(mapped) = crate::types::map_ts_type(targ) {
                                    explicit_targs.push(mapped);
                                }
                            }
                        }

                        // Infer argument types from expressions as a fallback.
                        // If the argument is an identifier that refers to a
                        // local or a parameter of the current function, try
                        // to use the recorded `OatsType` (from `find_local`
                        // or `fn_param_types`) which is more accurate than
                        // expression-only inference for identifiers.
                        let mut inferred_params: Vec<crate::types::OatsType> = Vec::new();
                        for a in &call.args {
                            let mut inferred_opt: Option<crate::types::OatsType> = None;
                            // If the argument is a plain identifier, try to resolve its type
                            if let deno_ast::swc::ast::Expr::Ident(ident) = &*a.expr {
                                let arg_name = ident.sym.to_string();
                                // First, check lexical locals
                                if let Some((_, _, _, _, _, _, oats_type_opt)) =
                                    self.find_local(locals, &arg_name)
                                    && let Some(ot) = oats_type_opt
                                {
                                    inferred_opt = Some(ot.clone());
                                }
                                // Next, check if it's a parameter of the current function
                                if inferred_opt.is_none()
                                    && let Some(idx) = param_map.get(&arg_name)
                                {
                                    let caller_name = function.get_name().to_str().unwrap_or("");
                                    if let Some(param_types) =
                                        self.fn_param_types.borrow().get(caller_name)
                                    {
                                        let idx_us = *idx as usize;
                                        if idx_us < param_types.len() {
                                            inferred_opt = Some(param_types[idx_us].clone());
                                        }
                                    }
                                }
                            }

                            let inferred = if let Some(ot) = inferred_opt {
                                ot
                            } else {
                                crate::types::infer_type(None, Some(&a.expr))
                            };
                            inferred_params.push(inferred);
                        }

                        // Build substitution map keyed by declared type param names.
                        // Use explicit type args first (by position). Otherwise
                        // attempt name-based inference: find a function parameter
                        // whose annotated TS type references the type-parameter
                        // name and map it to the call-site inferred arg type.
                        let mut subst: std::collections::HashMap<String, crate::types::OatsType> =
                            std::collections::HashMap::new();

                        // Helper: recursively check whether a TsType contains a
                        // reference to a given type-parameter identifier.
                        fn ts_type_contains_name(
                            ty: &deno_ast::swc::ast::TsType,
                            name: &str,
                        ) -> bool {
                            use deno_ast::swc::ast;
                            match ty {
                                ast::TsType::TsTypeRef(type_ref) => {
                                    if let Some(ident) = type_ref.type_name.as_ident()
                                        && ident.sym.as_ref() == name
                                    {
                                        return true;
                                    }
                                    if let Some(tp) = &type_ref.type_params {
                                        for param in &tp.params {
                                            if ts_type_contains_name(param, name) {
                                                return true;
                                            }
                                        }
                                    }
                                    false
                                }
                                ast::TsType::TsArrayType(arr) => {
                                    ts_type_contains_name(&arr.elem_type, name)
                                }
                                ast::TsType::TsUnionOrIntersectionType(u) => {
                                    if let ast::TsUnionOrIntersectionType::TsUnionType(un) = u {
                                        for t in &un.types {
                                            if ts_type_contains_name(t, name) {
                                                return true;
                                            }
                                        }
                                    }
                                    false
                                }
                                ast::TsType::TsTupleType(tuple) => {
                                    for e in &tuple.elem_types {
                                        if ts_type_contains_name(&e.ty, name) {
                                            return true;
                                        }
                                    }
                                    false
                                }
                                ast::TsType::TsTypeLit(typelit) => {
                                    use deno_ast::swc::ast;
                                    for member in &typelit.members {
                                        if let ast::TsTypeElement::TsPropertySignature(prop) =
                                            member
                                            && let Some(type_ann) = &prop.type_ann
                                            && ts_type_contains_name(&type_ann.type_ann, name)
                                        {
                                            return true;
                                        }
                                    }
                                    false
                                }
                                _ => false,
                            }
                        }

                        for (i, name) in declared_names.iter().enumerate() {
                            // explicit type args override everything
                            if i < explicit_targs.len() {
                                subst.insert(name.clone(), explicit_targs[i].clone());
                                continue;
                            }

                            // Try name-based mapping: find a parameter that references this type param
                            let mut found = false;
                            for (pidx, param) in nested_fn.params.iter().enumerate() {
                                if let ast::Pat::Ident(ident) = &param.pat
                                    && let Some(type_ann) = &ident.type_ann
                                {
                                    let ts_ty = &*type_ann.type_ann;
                                    if ts_type_contains_name(ts_ty, name) {
                                        // use inferred type for this parameter position if available
                                        if pidx < inferred_params.len() {
                                            // Try to derive a more accurate mapping when the
                                            // parameter annotation is an array-like type.
                                            // For example, for `param: T[]` and an inferred
                                            // arg of `Array(Number)` we want to map `T -> Number`
                                            // instead of `T -> Array(Number)`.
                                            let inferred = inferred_params[pidx].clone();
                                            let mapped = match ts_ty {
                                                deno_ast::swc::ast::TsType::TsArrayType(_) => {
                                                    // If the call-site arg is an Array(inner), unwrap it
                                                    if let crate::types::OatsType::Array(inner) =
                                                        &inferred
                                                    {
                                                        // inner is &Box<OatsType>; clone the inner value
                                                        (**inner).clone()
                                                    } else {
                                                        inferred.clone()
                                                    }
                                                }
                                                deno_ast::swc::ast::TsType::TsTypeRef(type_ref) => {
                                                    // Handle Array<T> written as a generic type ref
                                                    if let Some(ident) =
                                                        type_ref.type_name.as_ident()
                                                        && ident.sym.as_ref() == "Array"
                                                    {
                                                        if let crate::types::OatsType::Array(
                                                            inner,
                                                        ) = &inferred
                                                        {
                                                            (**inner).clone()
                                                        } else {
                                                            inferred.clone()
                                                        }
                                                    } else {
                                                        inferred.clone()
                                                    }
                                                }
                                                _ => inferred.clone(),
                                            };
                                            subst.insert(name.clone(), mapped);
                                            found = true;
                                            break;
                                        }
                                    }
                                }
                            }
                            if found {
                                continue;
                            }

                            // Fallback: map by declared order to inferred arg if present
                            if i < inferred_params.len() {
                                subst.insert(name.clone(), inferred_params[i].clone());
                            }
                        }

                        // Use the substitution map to produce specialized param types
                        let mut spec_params: Vec<crate::types::OatsType> = Vec::new();
                        // Walk the original AST param list to re-map types that may
                        // reference named type parameters. The fsig.params is a
                        // convenience but may contain Generic placeholders without
                        // the original AST link; instead we inspect nested_fn.params
                        // to preserve mapping accuracy when types are named.
                        for (idx, param) in nested_fn.params.iter().enumerate() {
                            if let ast::Pat::Ident(ident) = &param.pat
                                && let Some(type_ann) = &ident.type_ann
                            {
                                let ts_ty = &*type_ann.type_ann;
                                if let Some(mapped) =
                                    crate::types::map_ts_type_with_subst(ts_ty, &subst)
                                {
                                    spec_params.push(mapped);
                                    continue;
                                }
                            }
                            // Fallback: use fsig.params if AST mapping fails
                            if let Some(p) = fsig.params.get(idx) {
                                spec_params.push(p.clone());
                            } else {
                                spec_params.push(crate::types::OatsType::Number);
                            }
                        }

                        // Specialize return type using the substitution map and AST
                        let mut spec_ret: crate::types::OatsType =
                            if let Some(rt) = &nested_fn.return_type {
                                let ts_ty = &*rt.type_ann;
                                crate::types::map_ts_type_with_subst(ts_ty, &subst)
                                    .unwrap_or(fsig.ret.clone())
                            } else {
                                fsig.ret.clone()
                            };

                        // If the specialization still contains unresolved generics
                        // (for example return type is `T | undefined` and `T` wasn't
                        // mapped), try an additional inference pass: look for a
                        // parameter annotated as `T[]` and use the call-site
                        // inferred argument type (e.g. Array(Number)) to map `T->Number`.
                        let needs_extra_infer = match &spec_ret {
                            crate::types::OatsType::Generic(_) => true,
                            crate::types::OatsType::Union(parts) => parts
                                .iter()
                                .any(|p| matches!(p, crate::types::OatsType::Generic(_))),
                            crate::types::OatsType::Option(inner) => {
                                matches!(**inner, crate::types::OatsType::Generic(_))
                            }
                            _ => false,
                        };
                        if needs_extra_infer {
                            // Look through declared_names and nested_fn.params to find an array param that references the generic name
                            for (pidx, param) in nested_fn.params.iter().enumerate() {
                                if let deno_ast::swc::ast::Pat::Ident(ident) = &param.pat
                                    && let Some(type_ann) = &ident.type_ann
                                {
                                    // If the param AST type mentions a declared type param
                                    // and that declared name is present in declared_names,
                                    // and the call-site inferred param is an Array(inner),
                                    // map the declared name -> inner.
                                    for decl_name in declared_names.iter() {
                                        // Only attempt if not already substituted
                                        if subst.contains_key(decl_name) {
                                            continue;
                                        }
                                        // If the parameter annotation contains the declared name,
                                        // and the inferred param at this position is Array(inner), use it.
                                        if ts_type_contains_name(&type_ann.type_ann, decl_name)
                                            && pidx < inferred_params.len()
                                        {
                                            let inferred = inferred_params[pidx].clone();
                                            if let crate::types::OatsType::Array(inner_box) =
                                                inferred
                                            {
                                                subst.insert(
                                                    decl_name.clone(),
                                                    (*inner_box).clone(),
                                                );
                                            } else {
                                                // If inferred param isn't an array but the annotation was T[],
                                                // fall back to using the inferred param itself.
                                                subst.insert(decl_name.clone(), inferred);
                                            }
                                        }
                                    }
                                }
                            }

                            // Recompute specialized params and return using the updated subst
                            spec_params.clear();
                            for (idx, param) in nested_fn.params.iter().enumerate() {
                                if let deno_ast::swc::ast::Pat::Ident(ident) = &param.pat
                                    && let Some(type_ann) = &ident.type_ann
                                {
                                    let ts_ty = &*type_ann.type_ann;
                                    if let Some(mapped) =
                                        crate::types::map_ts_type_with_subst(ts_ty, &subst)
                                    {
                                        spec_params.push(mapped);
                                        continue;
                                    }
                                }
                                if let Some(p) = fsig.params.get(idx) {
                                    spec_params.push(p.clone());
                                } else {
                                    spec_params.push(crate::types::OatsType::Number);
                                }
                            }
                            // recompute spec_ret
                            if let Some(rt) = &nested_fn.return_type {
                                let ts_ty = &*rt.type_ann;
                                spec_ret = crate::types::map_ts_type_with_subst(ts_ty, &subst)
                                    .unwrap_or(fsig.ret.clone());
                            } else {
                                spec_ret = fsig.ret.clone();
                            }
                        }

                        // Prefer using the call-site inferred argument types when generating
                        // the specialization key. This ties the specialization to what the
                        // caller actually passes (for example `Array(Number)` vs `Array(String)`)
                        // and avoids incorrectly reusing a specialization computed from
                        // AST-level substitution that may be incomplete.
                        let inferred_for_key = inferred_params.clone();
                        let key = format!("{}::<{:?}> -> {:?}", fname, inferred_for_key, spec_ret);

                        // Debug: print both inferred (call-site) and computed spec params
                        eprintln!(
                            "[mono-debug] callsite '{}' inferred={:?} spec_params={:?} key='{}' ret={:?}",
                            fname, inferred_for_key, spec_params, key, spec_ret
                        );

                        // Check if we've already created a specialization
                        let mut mono_map = self.monomorphized_map.borrow_mut();
                        let target_name = if let Some(name) = mono_map.get(&key) {
                            name.clone()
                        } else {
                            use std::collections::hash_map::DefaultHasher;
                            use std::hash::{Hash, Hasher};
                            let mut hasher = DefaultHasher::new();
                            key.hash(&mut hasher);
                            let h = hasher.finish();
                            let specialized = format!("{}_mono_{:x}", fname, h);
                            let prev_block = self.builder.get_insert_block();
                            // Attempt to generate the specialized function.
                            // Only record the specialization if generation succeeds.
                            let gen_result = self.gen_function_ir(
                                &specialized,
                                &nested_fn,
                                &spec_params,
                                &spec_ret,
                                None,
                            );
                            match gen_result {
                                Ok(_) => {
                                    eprintln!(
                                        "[mono] created specialization '{}' for key='{}' params={:?} ret={:?}",
                                        specialized, key, spec_params, spec_ret
                                    );
                                    if let Some(pb) = prev_block {
                                        self.builder.position_at_end(pb);
                                    }
                                    // Record the param types for the generated function so later lowering
                                    // can consult `fn_param_types` to resolve nominal info.
                                    self.fn_param_types
                                        .borrow_mut()
                                        .insert(specialized.clone(), spec_params.clone());
                                    mono_map.insert(key.clone(), specialized.clone());
                                    specialized
                                }
                                Err(diag) => {
                                    // On failure emit diagnostic and fall back to generic name
                                    crate::diagnostics::emit_diagnostic(&diag, Some(self.source));
                                    if let Some(pb) = prev_block {
                                        self.builder.position_at_end(pb);
                                    }
                                    fname.clone()
                                }
                            }
                        };

                        // Now lower args and call the specialized function
                        let mut call_args: Vec<inkwell::values::BasicMetadataValueEnum> =
                            Vec::new();
                        for a in &call.args {
                            let val = match self.lower_expr(&a.expr, function, param_map, locals) {
                                Ok(v) => v,
                                Err(d) => return Err(d)?,
                            };
                            call_args.push(val.into());
                        }
                        if let Some(fv) = self.module.get_function(&target_name) {
                            let cs = self.builder.build_call(fv, &call_args, "mono_call");
                            if let Ok(cs) = cs {
                                let either = cs.try_as_basic_value();
                                if let inkwell::Either::Left(bv) = either {
                                    return Ok(bv);
                                }
                            }
                        }
                        // Fall through to normal call lowering if something failed
                    }

                    if let Some(fv) = self.module.get_function(&fname) {
                        // Lower args
                        let mut lowered_args: Vec<inkwell::values::BasicMetadataValueEnum> =
                            Vec::new();
                        for a in &call.args {
                            if let Ok(val) = self.lower_expr(&a.expr, function, param_map, locals) {
                                lowered_args.push(val.into());
                            } else {
                                return Err(Diagnostic::simple_boxed(
                                    "expression lowering failed",
                                ))?;
                            }
                        }
                        let cs = match self.builder.build_call(fv, &lowered_args, "call_internal") {
                            Ok(cs) => cs,
                            Err(_) => return Err(Diagnostic::simple_boxed("operation failed")),
                        };
                        let either = cs.try_as_basic_value();
                        if let inkwell::Either::Left(bv) = either {
                            // Check if this function returns a union type
                            // If so, mark that the returned value is already boxed
                            if let Some(ret_type) =
                                self.current_function_return_type.borrow().clone()
                            {
                                // Only set flag if we're calling a function different from the current one
                                let current_fname = function.get_name().to_str().unwrap_or("");
                                if fname != current_fname
                                    && matches!(ret_type, crate::types::OatsType::Union(_))
                                {
                                    // This is a bit fragile - we're checking the current function's return type
                                    // We should actually check the *called* function's return type
                                    // For now, skip setting the flag here and handle it differently
                                }
                            }
                            // Reset the flag before returning - we'll set it properly in the caller if needed
                            self.last_expr_is_boxed_union.set(false);
                            // The call returned a value; propagate it.
                            Ok(bv)
                        } else {
                            // The call returned void. To make expression
                            // contexts uniformly expect a `BasicValueEnum`
                            // we return a harmless `f64` zero. This keeps
                            // expression-statement lowering simple: the
                            // caller can ignore the returned value.
                            self.last_expr_is_boxed_union.set(false);
                            let zero = self.f64_t.const_float(0.0);
                            Ok(zero.as_basic_value_enum())
                        }
                    } else {
                        Err(Diagnostic::simple_boxed(format!(
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
                                    return Err(Diagnostic::simple_boxed(
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
                                                "union_box_f64 call failed",
                                            ))?;
                                        }
                                    };
                                    match cs.try_as_basic_value() {
                                        inkwell::Either::Left(bv) => bv,
                                        _ => {
                                            return Err(Diagnostic::simple_boxed(
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
                                        "promise_resolve call failed",
                                    ))?;
                                }
                            };
                            match cs.try_as_basic_value() {
                                inkwell::Either::Left(bv) => return Ok(bv),
                                _ => {
                                    return Err(Diagnostic::simple_boxed(
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
                                let cs = match self.builder.build_call(f, &[], "math_random_call") {
                                    Ok(cs) => cs,
                                    Err(_) => {
                                        return Err(Diagnostic::simple_boxed("operation failed"));
                                    }
                                };
                                let either = cs.try_as_basic_value();
                                if let inkwell::Either::Left(bv) = either {
                                    return Ok(bv);
                                } else {
                                    return Err(Diagnostic::simple_boxed("operation failed"));
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
                                        "downgrade on non-pointer",
                                    ))?;
                                }
                            }
                            if method_name == "upgrade" {
                                // Expect no args
                                if !call.args.is_empty() {
                                    return Err(Diagnostic::simple_boxed(
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
                                            return Err(Diagnostic::simple_boxed(
                                                "operation failed",
                                            ));
                                        }
                                    };
                                    let either = cs.try_as_basic_value();
                                    if let inkwell::Either::Left(bv) = either {
                                        return Ok(bv);
                                    } else {
                                        return Err(Diagnostic::simple_boxed(
                                            "operation not supported",
                                        ));
                                    }
                                } else {
                                    return Err(Diagnostic::simple_boxed(
                                        "upgrade on non-pointer",
                                    ))?;
                                }
                            }
                            // Special-case: arr.push(x) / arr.pop()
                            if method_name == "push" {
                                // Expect one argument
                                if call.args.len() == 1 {
                                    let arg = &call.args[0];
                                    if let Ok(arg_val) =
                                        self.lower_expr(&arg.expr, function, param_map, locals)
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
                                                return Err(Diagnostic::simple_boxed(
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
                                            return Err(Diagnostic::simple_boxed(
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
                                            return Err(Diagnostic::simple_boxed(
                                                "operation failed",
                                            ));
                                        }
                                    };
                                    let either2 = cs2.try_as_basic_value();
                                    if let inkwell::Either::Left(bv2) = either2 {
                                        return Ok(bv2);
                                    }
                                    return Err(Diagnostic::simple_boxed("operation failed"));
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
                                    _oats_type,
                                )) = self.find_local(locals, &ident_name)
                                    && let Some(nom) = nominal
                                {
                                    class_name_opt = Some(nom);
                                }
                            } else if matches!(&*member.obj, deno_ast::swc::ast::Expr::This(_)) {
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
                                        let keys: Vec<String> =
                                            self.class_fields.borrow().keys().cloned().collect();
                                        eprintln!("[debug member_call] known_nominals={:?}", keys);
                                    }
                                    return Err(Diagnostic::simple_boxed(
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
                                let mut user_args: Vec<inkwell::values::BasicMetadataValueEnum> =
                                    Vec::new();
                                for a in &call.args {
                                    if let Ok(v) =
                                        self.lower_expr(&a.expr, function, param_map, locals)
                                    {
                                        user_args.push(v.into());
                                    } else {
                                        return Err(Diagnostic::simple_boxed(
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

                                let cs =
                                    match self.builder.build_call(method_f, &args, "call_method") {
                                        Ok(cs) => cs,
                                        Err(_) => {
                                            return Err(Diagnostic::simple_boxed(
                                                "operation failed",
                                            ));
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
                                let mut found: Option<(String, inkwell::values::FunctionValue)> =
                                    None;
                                for k in self.class_fields.borrow().keys() {
                                    let alt = format!("{}_{}", k, method_name);
                                    if let Some(fv) = self.module.get_function(&alt) {
                                        found = Some((k.clone(), fv));
                                        break;
                                    }
                                }
                                if let Some((_found_nom, method_f)) = found {
                                    #[cfg(debug_assertions)]
                                    eprintln!(
                                        "[debug member_call] falling back to nominal '{}' for method '{}'",
                                        _found_nom, method_name
                                    );
                                    // lower user args
                                    let mut user_args: Vec<
                                        inkwell::values::BasicMetadataValueEnum,
                                    > = Vec::new();
                                    for a in &call.args {
                                        if let Ok(v) =
                                            self.lower_expr(&a.expr, function, param_map, locals)
                                        {
                                            user_args.push(v.into());
                                        } else {
                                            return Err(Diagnostic::simple_boxed(
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
                                            return Err(Diagnostic::simple_boxed(
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
                                .i8_ptr_from_offset_i64(callee_ptr, off_env, "closure_env_i8")
                                .map_err(|_| Diagnostic::simple("operation failed"))?;

                            let fn_ptr_slot_ty = self.context.ptr_type(AddressSpace::default());
                            let fn_ptr_slot = self
                                .builder
                                .build_pointer_cast(fn_ptr_i8, fn_ptr_slot_ty, "fn_ptr_slot_cast")
                                .map_err(|_| Diagnostic::simple("LLVM builder error"))?;
                            let fn_ptr_bv = match self.builder.build_load(
                                self.i8ptr_t,
                                fn_ptr_slot,
                                "loaded_fn_ptr",
                            ) {
                                Ok(v) => v,
                                Err(_) => {
                                    return Err(Diagnostic::simple_boxed("operation failed"));
                                }
                            };

                            let env_slot_ty = self.context.ptr_type(AddressSpace::default());
                            let env_slot = self
                                .builder
                                .build_pointer_cast(env_ptr_i8, env_slot_ty, "env_slot_cast")
                                .map_err(|_| Diagnostic::simple("LLVM builder error"))?;
                            let env_bv = match self.builder.build_load(
                                self.i8ptr_t,
                                env_slot,
                                "loaded_env_ptr",
                            ) {
                                Ok(v) => v,
                                Err(_) => {
                                    return Err(Diagnostic::simple_boxed("operation failed"));
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
                                        "expression lowering failed",
                                    ))?;
                                }
                            }

                            // Indirect call via loaded function pointer
                            if let BasicValueEnum::PointerValue(fn_ptr_pv) = fn_ptr_bv {
                                let mut param_types: Vec<inkwell::types::BasicMetadataTypeEnum> =
                                    Vec::new();
                                param_types.push(self.i8ptr_t.into());
                                for _ in &lowered_args {
                                    param_types.push(self.i8ptr_t.into());
                                }
                                let fn_ty = self.i8ptr_t.fn_type(&param_types, false);

                                let mut call_args: Vec<inkwell::values::BasicMetadataValueEnum> =
                                    Vec::new();
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
                                        return Err(Diagnostic::simple_boxed("operation failed"));
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
                            Err(Box::new(d))
                        }
                    }
                }
                _ => {
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
                                .i8_ptr_from_offset_i64(callee_ptr, off_env, "closure_env_i8")
                                .map_err(|_| Diagnostic::simple("operation failed"))?;

                            // bitcast fn_ptr_i8 to pointer-to-pointer (i8**), load the stored function pointer
                            let fn_ptr_slot_ty = self.context.ptr_type(AddressSpace::default());
                            let fn_ptr_slot = self
                                .builder
                                .build_pointer_cast(fn_ptr_i8, fn_ptr_slot_ty, "fn_ptr_slot_cast")
                                .map_err(|_| Diagnostic::simple("LLVM builder error"))?;
                            let fn_ptr_bv = match self.builder.build_load(
                                self.i8ptr_t,
                                fn_ptr_slot,
                                "loaded_fn_ptr",
                            ) {
                                Ok(v) => v,
                                Err(_) => {
                                    return Err(Diagnostic::simple_boxed("operation failed"));
                                }
                            };

                            // bitcast env_ptr_i8 similarly and load env pointer
                            let env_slot_ty = self.context.ptr_type(AddressSpace::default());
                            let env_slot = self
                                .builder
                                .build_pointer_cast(env_ptr_i8, env_slot_ty, "env_slot_cast")
                                .map_err(|_| Diagnostic::simple("LLVM builder error"))?;
                            let env_bv = match self.builder.build_load(
                                self.i8ptr_t,
                                env_slot,
                                "loaded_env_ptr",
                            ) {
                                Ok(v) => v,
                                Err(_) => {
                                    return Err(Diagnostic::simple_boxed("operation failed"));
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
                                        .map_err(|_| Diagnostic::simple("LLVM builder error"))?;
                                    let ret_i64_ptr = self
                                        .i8_ptr_from_offset_i64(
                                            callee_ptr,
                                            off_ret,
                                            "closure_ret_i8",
                                        )
                                        .map_err(|_| Diagnostic::simple("operation failed"))?;
                                    let ret_ptr_ty =
                                        self.context.ptr_type(inkwell::AddressSpace::default());
                                    let ret_ptr_cast = self
                                        .builder
                                        .build_pointer_cast(ret_i64_ptr, ret_ptr_ty, "ret_ptr_cast")
                                        .map_err(|_| Diagnostic::simple("LLVM builder error"))?;
                                    let ret_bv = match self.builder.build_load(
                                        self.i64_t,
                                        ret_ptr_cast,
                                        "loaded_ret_tag",
                                    ) {
                                        Ok(v) => v,
                                        Err(_) => {
                                            return Err(Diagnostic::simple_boxed(
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
                                    .map_err(|_| Diagnostic::simple("LLVM builder error"))?;

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
                                        return Err(Diagnostic::simple_boxed("operation failed"));
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
                                "unsupported closure call (indirect call lowering failed)",
                                call.span.lo.0 as usize,
                            ))
                        } else {
                            Err(Diagnostic::simple_with_span_boxed(
                                "unsupported callee expression",
                                call.span.lo.0 as usize,
                            ))
                        }
                    } else {
                        Err(Diagnostic::simple_with_span_boxed(
                            "expression lowering failed",
                            call.span.lo.0 as usize,
                        ))
                    }
                }
            }
        } else {
            Err(Diagnostic::simple_with_span_boxed(
                "unsupported call expression: callee form not supported",
                call.span.lo.0 as usize,
            ))
        }
    }
}
