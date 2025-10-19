use crate::codegen::CodeGen;
use crate::diagnostics::{Diagnostic, Severity};
use crate::types::OatsType;
use deno_ast::swc::ast;
use inkwell::types::{BasicType, BasicTypeEnum};
use inkwell::values::{BasicValue, BasicValueEnum, FunctionValue};
use std::collections::HashMap;

// LocalEntry now includes an Option<String> for an optional nominal type name
// LocalEntry now includes an Option<OatsType> for union tracking
type LocalEntry<'a> = (
    inkwell::values::PointerValue<'a>,
    BasicTypeEnum<'a>,
    bool,
    bool,
    bool,
    Option<String>,
    Option<OatsType>,
);
type LocalsStackLocal<'a> = Vec<HashMap<String, LocalEntry<'a>>>;

impl<'a> CodeGen<'a> {
    #[allow(clippy::result_large_err)]
    pub(super) fn lower_arrow_expr(
        &self,
        arrow: &deno_ast::swc::ast::ArrowExpr,
        function: FunctionValue<'a>,
        param_map: &HashMap<String, u32>,
        locals: &mut LocalsStackLocal<'a>,
    ) -> crate::diagnostics::DiagnosticResult<BasicValueEnum<'a>> {
        // Detect captured variables in the arrow body by scanning for
        // identifier usages that are not parameters and that resolve to
        // names in the surrounding `locals` stack. We return a helpful
        // Diagnostic: capture listing is emitted here. Full closure
        // lowering will be implemented separately.
        // (boxed environments + trampolines) is planned in Phase 2.
        fn collect_idents_in_expr(
            expr: &deno_ast::swc::ast::Expr,
            out: &mut std::collections::HashSet<String>,
        ) {
            use deno_ast::swc::ast;
            match expr {
                ast::Expr::Ident(id) => {
                    out.insert(id.sym.to_string());
                }
                ast::Expr::Bin(b) => {
                    collect_idents_in_expr(&b.left, out);
                    collect_idents_in_expr(&b.right, out);
                }
                ast::Expr::Unary(u) => {
                    collect_idents_in_expr(&u.arg, out);
                }
                ast::Expr::Call(c) => {
                    use deno_ast::swc::ast::Callee;
                    match &c.callee {
                        Callee::Expr(e) => collect_idents_in_expr(e, out),
                        Callee::Super(_) => {}
                        Callee::Import(_) => {}
                    }
                    for a in &c.args {
                        collect_idents_in_expr(&a.expr, out);
                    }
                }
                ast::Expr::Member(m) => {
                    collect_idents_in_expr(&m.obj, out);
                }
                ast::Expr::Arrow(a) => {
                    // nested arrow: scan body
                    if a.body.is_block_stmt() {
                        if let deno_ast::swc::ast::BlockStmtOrExpr::BlockStmt(b) = &*a.body {
                            for s in &b.stmts {
                                use deno_ast::swc::ast::Stmt;
                                if let Stmt::Expr(es) = s {
                                    collect_idents_in_expr(&es.expr, out);
                                }
                            }
                        }
                    } else if let deno_ast::swc::ast::BlockStmtOrExpr::Expr(e) = &*a.body {
                        collect_idents_in_expr(e, out);
                    }
                }
                ast::Expr::Object(o) => {
                    for prop in &o.props {
                        if let deno_ast::swc::ast::PropOrSpread::Prop(p) = prop
                            && let deno_ast::swc::ast::Prop::KeyValue(kv) = &**p
                        {
                            collect_idents_in_expr(&kv.value, out);
                        }
                    }
                }
                ast::Expr::Array(a) => {
                    for elem in a.elems.iter().flatten() {
                        collect_idents_in_expr(&elem.expr, out);
                    }
                }
                ast::Expr::Assign(asg) => {
                    collect_idents_in_expr(&asg.right, out);
                }
                ast::Expr::Cond(cnd) => {
                    collect_idents_in_expr(&cnd.test, out);
                    collect_idents_in_expr(&cnd.cons, out);
                    collect_idents_in_expr(&cnd.alt, out);
                }
                _ => {}
            }
        }

        // Build a set of names visible in the surrounding locals stack
        let mut outer_names: std::collections::HashSet<String> = std::collections::HashSet::new();
        for scope in locals.iter() {
            for k in scope.keys() {
                outer_names.insert(k.clone());
            }
        }

        // Collect idents used in the arrow body
        let mut used: std::collections::HashSet<String> = std::collections::HashSet::new();
        if arrow.body.is_block_stmt() {
            if let deno_ast::swc::ast::BlockStmtOrExpr::BlockStmt(block) = &*arrow.body {
                for stmt in &block.stmts {
                    use deno_ast::swc::ast::Stmt;
                    if let Stmt::Expr(es) = stmt {
                        collect_idents_in_expr(&es.expr, &mut used);
                    }
                }
            }
        } else if let deno_ast::swc::ast::BlockStmtOrExpr::Expr(expr) = &*arrow.body {
            collect_idents_in_expr(expr, &mut used);
        }

        // Remove arrow params from used
        for param in &arrow.params {
            if let deno_ast::swc::ast::Pat::Ident(ident) = param {
                used.remove(&ident.id.sym.to_string());
            }
        }

        // Any remaining identifiers that exist in outer_names are captures
        let mut captures: Vec<String> = Vec::new();
        for name in used.into_iter() {
            if outer_names.contains(&name) {
                captures.push(name);
            }
        }

        // Generate a unique function name for this arrow
        let arrow_fn_name = format!("arrow_fn_{}", self.next_str_id.get());
        self.next_str_id.set(self.next_str_id.get() + 1);

        // Extract parameter types from arrow.params
        let mut param_types = Vec::new();
        for param in &arrow.params {
            match param {
                ast::Pat::Ident(ident) => {
                    if let Some(type_ann) = &ident.type_ann {
                        if let Some(mapped) = crate::types::map_ts_type(&type_ann.type_ann) {
                            param_types.push(mapped);
                        } else {
                            return Err(Diagnostic::simple_boxed(
                                Severity::Error,
                                "Arrow parameter has unsupported type annotation",
                            ));
                        }
                    } else {
                        return Err(Diagnostic::simple_boxed(
                            Severity::Error,
                            "Arrow parameter missing type annotation",
                        ));
                    }
                }
                _ => {
                    return Err(Diagnostic::simple_boxed(
                        Severity::Error,
                        "Arrow function parameter pattern not supported",
                    ));
                }
            }
        }

        // Determine return type from arrow.return_type or infer from body
        let ret_type = if let Some(return_type) = &arrow.return_type {
            if let Some(mapped) = crate::types::map_ts_type(&return_type.type_ann) {
                mapped
            } else {
                return Err(Diagnostic::simple_boxed(
                    Severity::Error,
                    "Arrow return type not supported",
                ));
            }
        } else {
            return Err(Diagnostic::simple_boxed(
                Severity::Error,
                "Arrow function return type annotation required - test",
            ));
        };

        // Build LLVM function type. If captures are present, the first
        // parameter is the environment pointer (i8*).
        let mut llvm_param_types: Vec<BasicTypeEnum> = Vec::new();
        if !captures.is_empty() {
            llvm_param_types.push(self.i8ptr_t.as_basic_type_enum());
        }
        llvm_param_types.extend(param_types.iter().map(|t| self.map_type_to_llvm(t)));
        let fn_type = self.build_llvm_fn_type(&llvm_param_types, &ret_type);

        // Create the arrow function (may accept env param as first arg)
        let arrow_fn = self.module.add_function(&arrow_fn_name, fn_type, None);

        // Record parameter types for the generated arrow function so
        // member-access lowering can consult `fn_param_types` to infer
        // nominal types for parameters (and `this` receiver when
        // applicable). This mirrors `gen_function_ir` behavior.
        self.fn_param_types
            .borrow_mut()
            .insert(arrow_fn_name.clone(), param_types.clone());

        // Save current insert block so we can return to it
        let current_block = self.builder.get_insert_block();

        // Build the function body
        let entry_bb = self.context.append_basic_block(arrow_fn, "entry");
        self.builder.position_at_end(entry_bb);

        // Create parameter map for arrow function. If env param exists,
        // shift user parameters by +1.
        let mut arrow_param_map = HashMap::new();
        for (idx, param) in arrow.params.iter().enumerate() {
            if let ast::Pat::Ident(ident) = param {
                let mapped_idx = if !captures.is_empty() {
                    (idx as u32) + 1
                } else {
                    idx as u32
                };
                arrow_param_map.insert(ident.id.sym.to_string(), mapped_idx);
            }
        }

        // Create locals stack for arrow function
        let mut arrow_locals = vec![HashMap::new()];

        // If the arrow accepts an env param (captures exist), bind env fields
        // to locals so body references to captured names resolve.
        if !captures.is_empty() {
            // env is the first parameter of arrow_fn
            let env_param = arrow_fn
                .get_nth_param(0)
                .ok_or_else(|| Diagnostic::error("missing env parameter for arrow"))?;
            let env_ptr = env_param.into_pointer_value();
            // Convert env pointer to integer for offset math
            let obj_ptr_int = self
                .builder
                .build_ptr_to_int(env_ptr, self.i64_t, "env_addr")
                .map_err(|_| Diagnostic::error("ptr_to_int failed"))?;
            let header_size = 8u64;
            let meta_slot = 8u64;

            for (i, cname) in captures.iter().enumerate() {
                let field_offset = header_size + meta_slot + (i as u64 * 8);
                let off_const = self.i64_t.const_int(field_offset, false);
                let field_addr = self
                    .builder
                    .build_int_add(obj_ptr_int, off_const, "cap_field_addr")
                    .map_err(|_| Diagnostic::error("int_add failed"))?;
                let field_ptr = self
                    .builder
                    .build_int_to_ptr(field_addr, self.i8ptr_t, "cap_field_ptr")
                    .map_err(|_| Diagnostic::error("int_to_ptr failed"))?;
                let loaded = self
                    .builder
                    .build_load(self.i8ptr_t, field_ptr, &format!("cap_load_{}", cname))
                    .map_err(|_| Diagnostic::error("load failed"))?;

                // Create an alloca for the captured local inside the arrow function and store the loaded value
                let alloca = self
                    .builder
                    .build_alloca(self.i8ptr_t, &format!("cap_{}", cname))
                    .map_err(|_| Diagnostic::error("alloca failed"))?;
                let _ = self.builder.build_store(alloca, loaded);
                // Insert into arrow_locals so later identifier lookups resolve to this alloca
                if let Some(scope) = arrow_locals.last_mut() {
                    scope.insert(
                        cname.clone(),
                        (
                            alloca,
                            self.i8ptr_t.as_basic_type_enum(),
                            true,
                            true,
                            false,
                            None,
                            None,
                        ),
                    );
                }
            }
        }

        // If there are captures, allocate environment in the outer function
        // and construct a boxed closure object to return here.
        //
        // Design note: the environment (`env`) is a heap-allocated
        // object following the standard object layout: [header][meta][fields...].
        // We emit a `*_env_field_map` global which lists which offsets
        // contain pointer-like fields so the runtime's cycle collector
        // can traverse them. The closure object itself stores two
        // pointer fields (fn_ptr and env_ptr) and may optionally carry
        // a `ret_tag` to allow specialized indirect-call lowering.
        //
        // The `closure_local_rettype` map stores a conservative static
        // return type for freshly-created closure temporaries (for
        // example `__closure_tmp`). When present, later indirect calls
        // that use this local can choose an optimized function type
        // (e.g., returning f64) instead of conservatively assuming
        // a pointer return. This is a pragmatic optimization to avoid
        // emitting a runtime ret_tag for all closures.
        if !captures.is_empty() {
            // Collect current values of captured variables from outer scope
            // Store tuples of (value, is_weak) so env allocation can use weak increments
            let mut captured_vals: Vec<(inkwell::values::BasicValueEnum, bool)> = Vec::new();
            for cname in &captures {
                // First check if it's a parameter in the outer function
                if let Some(idx) = param_map.get(cname)
                    && let Some(pv) = function.get_nth_param(*idx)
                {
                    // Determine whether this parameter was declared Weak<T>
                    let mut is_weak = false;
                    if let Some(param_types) = self
                        .fn_param_types
                        .borrow()
                        .get(function.get_name().to_str().unwrap_or(""))
                    {
                        let idx_usize = *idx as usize;
                        if idx_usize < param_types.len()
                            && let crate::types::OatsType::Weak(_) = &param_types[idx_usize]
                        {
                            is_weak = true;
                        }
                    }
                    // If parameter is pointer-like, use directly. If numeric, box it.
                    if pv.get_type().is_pointer_type() {
                        captured_vals.push((pv.as_basic_value_enum(), is_weak));
                    } else if pv.get_type().is_float_type() {
                        // box f64
                        let box_fn = self.get_union_box_f64();
                        let cs =
                            self.builder
                                .build_call(box_fn, &[pv.into()], "union_box_f64_ctor");
                        if let Ok(cs) = cs
                            && let inkwell::Either::Left(bv) = cs.try_as_basic_value()
                        {
                            let boxed_ptr = bv.into_pointer_value();
                            captured_vals.push((boxed_ptr.as_basic_value_enum(), is_weak));
                        } else {
                            return Err(Diagnostic::simple_boxed(
                                Severity::Error,
                                "failed to box numeric capture",
                            ));
                        }
                    } else if pv.get_type().is_int_type() {
                        // convert int->f64 then box
                        let iv = pv.into_int_value();
                        let fconv = self
                            .builder
                            .build_signed_int_to_float(iv, self.f64_t, "i_to_f")
                            .map_err(|_| Diagnostic::error("int->float cast failed"))?;
                        let box_fn = self.get_union_box_f64();
                        let cs =
                            self.builder
                                .build_call(box_fn, &[fconv.into()], "union_box_f64_ctor");
                        if let Ok(cs) = cs
                            && let inkwell::Either::Left(bv) = cs.try_as_basic_value()
                        {
                            let boxed_ptr = bv.into_pointer_value();
                            captured_vals.push((boxed_ptr.as_basic_value_enum(), is_weak));
                        } else {
                            return Err(Diagnostic::simple_boxed(
                                Severity::Error,
                                "failed to box numeric capture",
                            ));
                        }
                    } else {
                        return Err(Diagnostic::simple_boxed(
                            Severity::Error,
                            "unsupported capture type: non-pointer parameter",
                        ));
                    }
                    continue;
                }

                // Otherwise look up local variable
                if let Some((
                    alloca_ptr,
                    ty,
                    initialized,
                    _is_const,
                    is_weak_flag,
                    _nominal,
                    _oats_type,
                )) = self.find_local(locals, cname)
                {
                    if !initialized {
                        return Err(Diagnostic::simple_boxed(
                            Severity::Error,
                            "cannot capture uninitialized local",
                        ));
                    }
                    // load current value
                    let loaded = match self.builder.build_load(
                        ty,
                        alloca_ptr,
                        &format!("cap_load_{}", cname),
                    ) {
                        Ok(v) => v,
                        Err(_) => {
                            return Err(Diagnostic::simple_boxed(
                                Severity::Error,
                                "failed to load captured local",
                            ));
                        }
                    };
                    // If pointer, use directly. If float/int, box into union object.
                    match loaded {
                        BasicValueEnum::PointerValue(_) => {
                            captured_vals.push((loaded, is_weak_flag))
                        }
                        BasicValueEnum::FloatValue(fv) => {
                            let box_fn = self.get_union_box_f64();
                            let cs =
                                self.builder
                                    .build_call(box_fn, &[fv.into()], "union_box_f64_ctor");
                            if let Ok(cs) = cs
                                && let inkwell::Either::Left(bv) = cs.try_as_basic_value()
                            {
                                let boxed_ptr = bv.into_pointer_value();
                                captured_vals.push((boxed_ptr.as_basic_value_enum(), is_weak_flag));
                            } else {
                                return Err(Diagnostic::simple_boxed(
                                    Severity::Error,
                                    "failed to box numeric capture",
                                ));
                            }
                        }
                        BasicValueEnum::IntValue(iv) => {
                            let fconv = self
                                .builder
                                .build_signed_int_to_float(iv, self.f64_t, "i_to_f")
                                .map_err(|_| Diagnostic::error("int->float cast failed"))?;
                            let box_fn = self.get_union_box_f64();
                            let cs = self.builder.build_call(
                                box_fn,
                                &[fconv.into()],
                                "union_box_f64_ctor",
                            );
                            if let Ok(cs) = cs
                                && let inkwell::Either::Left(bv) = cs.try_as_basic_value()
                            {
                                let boxed_ptr = bv.into_pointer_value();
                                captured_vals.push((boxed_ptr.as_basic_value_enum(), is_weak_flag));
                            } else {
                                return Err(Diagnostic::simple_boxed(
                                    Severity::Error,
                                    "failed to box numeric capture",
                                ));
                            }
                        }
                        _ => {
                            return Err(Diagnostic::simple_boxed(
                                Severity::Error,
                                "unsupported capture type: non-pointer local",
                            ));
                        }
                    }
                } else {
                    return Err(Diagnostic::simple_boxed(
                        Severity::Error,
                        "capture not found in outer scope",
                    ));
                }
            }

            // Allocate environment object (heap) storing captured pointer fields
            // captured_vals is Vec<(BasicValueEnum, bool)> where bool indicates is_weak
            let env_ptr = self
                .heap_alloc_with_ptr_fields(captured_vals.as_slice())
                .map_err(|_| Diagnostic::error("failed to allocate env object"))?;

            // Emit field_map global for env and store pointer into env.meta slot
            let env_gv_name = format!("{}_env_field_map", arrow_fn_name);
            // offsets are header + meta_slot + idx*8
            let mut env_offsets: Vec<u64> = Vec::new();
            let header_size = 8u64;
            let meta_slot = 8u64;
            for i in 0..captured_vals.len() {
                env_offsets.push(header_size + meta_slot + (i as u64 * 8));
            }
            let env_gv_i8 = self
                .emit_field_map_global(&env_gv_name, &env_offsets)
                .map_err(|_| Diagnostic::error("failed to emit env field_map"))?;
            // store into env meta slot
            let env_ptr_int = self
                .builder
                .build_ptr_to_int(env_ptr, self.i64_t, "env_addr_for_meta")
                .map_err(|_| Diagnostic::error("ptr_to_int failed"))?;
            let off_const = self.i64_t.const_int(8, false);
            let meta_addr = self
                .builder
                .build_int_add(env_ptr_int, off_const, "env_meta_addr")
                .map_err(|_| Diagnostic::error("int_add failed"))?;
            let meta_ptr = self
                .builder
                .build_int_to_ptr(meta_addr, self.i8ptr_t, "env_meta_ptr")
                .map_err(|_| Diagnostic::error("int_to_ptr failed"))?;
            let _ = self
                .builder
                .build_store(meta_ptr, env_gv_i8.as_basic_value_enum());

            // Build closure object: store function pointer and env pointer into a 2-field heap object
            let fn_ptr_bv = arrow_fn
                .as_global_value()
                .as_pointer_value()
                .as_basic_value_enum();
            let env_bv = env_ptr.as_basic_value_enum();
            // We will store this closure into a tmp local `__closure_tmp` and
            // record its return type in `closure_local_rettype`. For such
            // statically-known closures we can avoid emitting the runtime
            // ret_tag and allocate a compact 2-field object instead.
            let use_static_layout = true; // conservative: current flow records mapping for __closure_tmp
            let closure_obj = if use_static_layout {
                self.heap_alloc_with_ptr_fields_simple(&[fn_ptr_bv, env_bv])
                    .map_err(|_| Diagnostic::error("failed to allocate closure object"))?
            } else {
                // ret_tag: 0=void, 1=number (f64), 2=pointer (i8*)
                let ret_tag_val: u64 = match ret_type {
                    crate::types::OatsType::Void => 0,
                    crate::types::OatsType::Number => 1,
                    _ => 2,
                };
                self.heap_alloc_closure_with_rettag(fn_ptr_bv, env_bv, ret_tag_val)
                    .map_err(|_| Diagnostic::error("failed to allocate closure object"))?
            };

            // Emit field_map for closure object (two pointer fields at offsets 16 and 24)
            let closure_gv_name = format!("{}_closure_field_map", arrow_fn_name);
            // fields start after header + meta_slot; fn_ptr at idx 0, env_ptr at idx 1
            let closure_offsets: Vec<u64> =
                vec![header_size + meta_slot, header_size + meta_slot + 8];
            let closure_gv_i8 = self
                .emit_field_map_global(&closure_gv_name, &closure_offsets)
                .map_err(|_| Diagnostic::error("failed to emit closure field_map"))?;
            // store into closure meta slot
            let closure_ptr_int = self
                .builder
                .build_ptr_to_int(closure_obj, self.i64_t, "closure_addr_for_meta")
                .map_err(|_| Diagnostic::error("ptr_to_int failed"))?;
            let closure_meta_addr = self
                .builder
                .build_int_add(closure_ptr_int, off_const, "closure_meta_addr")
                .map_err(|_| Diagnostic::error("int_add failed"))?;
            let closure_meta_ptr = self
                .builder
                .build_int_to_ptr(closure_meta_addr, self.i8ptr_t, "closure_meta_ptr")
                .map_err(|_| Diagnostic::error("int_to_ptr failed"))?;
            let _ = self
                .builder
                .build_store(closure_meta_ptr, closure_gv_i8.as_basic_value_enum());

            // At this point, we'll return the closure object pointer (i8*) from the outer lowering
            // after we finish constructing the arrow function. To keep the surrounding code path simple,
            // create a pointer value representing the closure object and return it at the end of this arm.
            // NOTE: we will still generate the arrow function body below which expects an env param.

            // Insert a special local mapping in the outer function to indicate the closure value
            // will be returned (handled below). We'll store the closure_obj into a temp alloca so
            // it can be returned as BasicValueEnum.
            let closure_ret = closure_obj.as_basic_value_enum();

            // Lowering will continue to generate the arrow function body which expects an env param.
            // After building the arrow function, we'll return `closure_ret` as the expression result.

            // Save closure_ret in a temporary variable on the stack of the outer function so the
            // code below (after function body generation) can return it. We'll use a simple trick:
            // create an alloca, store the pointer, and later load it for the `Ok(...)` return.
            let tmp_alloca = self
                .builder
                .build_alloca(self.i8ptr_t, "closure_tmp")
                .map_err(|_| Diagnostic::error("alloca failed"))?;
            let _ = self.builder.build_store(tmp_alloca, closure_ret);
            // remember where to load it later by adding an entry to locals stack
            // mark initialized=true so emit_rc_dec_for_locals knows about it
            if let Some(scope) = locals.last_mut() {
                scope.insert(
                    "__closure_tmp".to_string(),
                    (
                        tmp_alloca,
                        self.i8ptr_t.as_basic_type_enum(),
                        true,
                        true,
                        false,
                        None,
                        None,
                    ),
                );
            }
            // Record the known return type for this closure temp so callers
            // that load this local can emit statically-typed indirect calls.
            self.closure_local_rettype
                .borrow_mut()
                .insert("__closure_tmp".to_string(), ret_type.clone());
        }

        // Lower the body
        if arrow.body.is_block_stmt() {
            // Block body: { statements }
            if let ast::BlockStmtOrExpr::BlockStmt(block) = &*arrow.body {
                let terminated =
                    self.lower_stmts(&block.stmts, arrow_fn, &arrow_param_map, &mut arrow_locals)?;

                // If not terminated, add default return
                if !terminated {
                    match ret_type {
                        crate::types::OatsType::Void => {
                            let _ = self.builder.build_return(None);
                        }
                        crate::types::OatsType::Number => {
                            let zero = self.f64_t.const_float(0.0);
                            let _ = self.builder.build_return(Some(&zero));
                        }
                        _ => {
                            return Err(Diagnostic::simple_boxed(
                                Severity::Error,
                                "Cannot generate default return for arrow function",
                            ));
                        }
                    }
                }
            } else {
                return Err(Diagnostic::simple_boxed(
                    Severity::Error,
                    "Arrow body type mismatch",
                ));
            }
        } else {
            // Expression body: => expr (implicit return)
            if let ast::BlockStmtOrExpr::Expr(expr) = &*arrow.body {
                let result =
                    self.lower_expr(expr, arrow_fn, &arrow_param_map, &mut arrow_locals)?;

                // RC cleanup for locals before return
                self.emit_rc_dec_for_locals(&arrow_locals);

                let _ = self.builder.build_return(Some(&result));
            } else {
                return Err(Diagnostic::simple_boxed(
                    Severity::Error,
                    "Arrow body type mismatch",
                ));
            }
        }

        // Position back to original function
        if let Some(block) = current_block {
            self.builder.position_at_end(block);
        }

        // If captures were present we previously stored a closure tmp in the outer locals
        if !captures.is_empty() {
            // load tmp and return it
            if let Some((tmp_ptr, _ty, init, _is_const, _is_weak, _nominal, _oats_type)) =
                self.find_local(locals, "__closure_tmp")
            {
                if !init {
                    return Err(Diagnostic::simple_boxed(
                        Severity::Error,
                        "closure tmp uninitialized",
                    ));
                }
                let loaded = match self
                    .builder
                    .build_load(self.i8ptr_t, tmp_ptr, "closure_load")
                {
                    Ok(v) => v,
                    Err(_) => {
                        return Err(Diagnostic::simple_boxed(
                            Severity::Error,
                            "failed to load closure tmp",
                        ));
                    }
                };
                return Ok(loaded);
            } else {
                return Err(Diagnostic::simple_boxed(
                    Severity::Error,
                    "closure tmp missing",
                ));
            }
        }

        // No captures: return the function pointer as a value
        Ok(arrow_fn
            .as_global_value()
            .as_pointer_value()
            .as_basic_value_enum())
    }
}
