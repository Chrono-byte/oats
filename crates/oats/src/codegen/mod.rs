use inkwell::AddressSpace;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::BasicType;
use inkwell::types::BasicTypeEnum;
use inkwell::values::{BasicValue, BasicValueEnum, FunctionValue, PointerValue};
use std::cell::{Cell, RefCell};
use std::collections::HashMap;

pub mod helpers;
pub mod expr;
pub mod stmt;
/// Mapping from function name -> param OatsTypes vector. Stored so
/// lowering can inspect the declared nominal type of `this` parameters
/// (e.g. to resolve a `this.field` access to a concrete class name).
pub struct CodeGen<'a> {
    pub context: &'a Context,
    pub module: Module<'a>,
    pub builder: Builder<'a>,
    // Monotonic counter used to generate unique names for string literal
    // globals (e.g. `strlit.0`, `strlit.1`, ...). Using `Cell` lets us
    // mutate this counter from &self without requiring &mut.
    pub next_str_id: Cell<u32>,
    // Cache string literal contents to their emitted global values so
    // identical literals are emitted once and reused. Using `RefCell`
    // lets us mutate this from `&self`.
    pub string_literals: RefCell<HashMap<String, PointerValue<'a>>>,
    // Cache commonly used LLVM types to avoid repeated calls into Context
    pub f64_t: inkwell::types::FloatType<'a>,
    pub i64_t: inkwell::types::IntType<'a>,
    pub i32_t: inkwell::types::IntType<'a>,
    pub bool_t: inkwell::types::IntType<'a>,
    pub i8ptr_t: inkwell::types::PointerType<'a>,
    // Cache declared runtime helper functions once added
    pub fn_print_f64: RefCell<Option<FunctionValue<'a>>>,
    pub fn_print_str: RefCell<Option<FunctionValue<'a>>>,
    pub fn_strlen: RefCell<Option<FunctionValue<'a>>>,
    pub fn_malloc: RefCell<Option<FunctionValue<'a>>>,
    pub fn_memcpy: RefCell<Option<FunctionValue<'a>>>,
    pub fn_free: RefCell<Option<FunctionValue<'a>>>,
    pub fn_array_alloc: RefCell<Option<FunctionValue<'a>>>,
    pub fn_rc_inc: RefCell<Option<FunctionValue<'a>>>,
    pub fn_rc_dec: RefCell<Option<FunctionValue<'a>>>,
    /// Map of nominal struct name -> ordered list of (field name, field type)
    /// Populated by the frontend (main.rs) when scanning class declarations.
    pub class_fields: RefCell<HashMap<String, Vec<(String, crate::types::OatsType)>>>,
    /// Mapping from function name -> param OatsTypes vector. Stored so
    /// lowering can inspect the declared nominal type of `this` parameters
    /// (e.g. to resolve a `this.field` access to a concrete class name).
    pub fn_param_types: RefCell<HashMap<String, Vec<crate::types::OatsType>>>,
    /// Preprocessed source text used to map spans for diagnostics
    pub source: &'a str,
}

impl<'a> CodeGen<'a> {
    fn get_array_alloc(&self) -> FunctionValue<'a> {
        if let Some(f) = *self.fn_array_alloc.borrow() {
            return f;
        }
        // declare: i8* @array_alloc(i64, i32, i32)
        let i8ptr = self.i8ptr_t;
        let i64t = self.i64_t;
        let i32t = self.i32_t;
        let fn_type = i8ptr.fn_type(&[i64t.into(), i32t.into(), i32t.into()], false);
        let f = self.module.add_function("array_alloc", fn_type, None);
        *self.fn_array_alloc.borrow_mut() = Some(f);
        f
    }

    pub fn get_rc_inc(&self) -> FunctionValue<'a> {
        if let Some(f) = *self.fn_rc_inc.borrow() {
            return f;
        }
        let voidt = self.context.void_type();
        let i8ptr = self.i8ptr_t;
        let fn_type = voidt.fn_type(&[i8ptr.into()], false);
        let f = self.module.add_function("rc_inc", fn_type, None);
        *self.fn_rc_inc.borrow_mut() = Some(f);
        f
    }

    fn get_rc_dec(&self) -> FunctionValue<'a> {
        if let Some(f) = *self.fn_rc_dec.borrow() {
            return f;
        }
        let voidt = self.context.void_type();
        let i8ptr = self.i8ptr_t;
        let fn_type = voidt.fn_type(&[i8ptr.into()], false);
        let f = self.module.add_function("rc_dec", fn_type, None);
        *self.fn_rc_dec.borrow_mut() = Some(f);
        f
    }

    fn get_array_get_f64(&self) -> FunctionValue<'a> {
        if let Some(f) = self.module.get_function("array_get_f64") {
            return f;
        }
        let f64t = self.context.f64_type();
        let i8ptr = self.i8ptr_t;
        let i64t = self.i64_t;
        let fn_type = f64t.fn_type(&[i8ptr.into(), i64t.into()], false);

        self.module.add_function("array_get_f64", fn_type, None)
    }

    fn get_array_get_ptr(&self) -> FunctionValue<'a> {
        if let Some(f) = self.module.get_function("array_get_ptr") {
            return f;
        }
        let i8ptr = self.i8ptr_t;
        let i64t = self.i64_t;
        let fn_type = i8ptr.fn_type(&[i8ptr.into(), i64t.into()], false);

        self.module.add_function("array_get_ptr", fn_type, None)
    }

    // Borrowing variant: returns a borrowed pointer without incrementing
    // the refcount. Use only when you can guarantee the pointer's lifetime
    // doesn't outlive the array slot, or explicitly call `rc_inc` when
    // converting to an owned reference.
    #[allow(dead_code)]
    fn get_array_get_ptr_borrow(&self) -> FunctionValue<'a> {
        if let Some(f) = self.module.get_function("array_get_ptr_borrow") {
            return f;
        }
        let i8ptr = self.i8ptr_t;
        let i64t = self.i64_t;
        let fn_type = i8ptr.fn_type(&[i8ptr.into(), i64t.into()], false);

        self.module
            .add_function("array_get_ptr_borrow", fn_type, None)
    }

    /// Map of nominal struct name -> ordered list of field names.
    /// Populated by the frontend (main.rs) when scanning class declarations.
    pub fn _class_fields_placeholder(&self) {}

    fn get_array_set_ptr(&self) -> FunctionValue<'a> {
        if let Some(f) = self.module.get_function("array_set_ptr") {
            return f;
        }
        let voidt = self.context.void_type();
        let i8ptr = self.i8ptr_t;
        let i64t = self.i64_t;
        let fn_type = voidt.fn_type(&[i8ptr.into(), i64t.into(), i8ptr.into()], false);

        self.module.add_function("array_set_ptr", fn_type, None)
    }

    // `lower_expr_result` moved to `codegen/expr.rs` to begin modularization.

    
    pub fn gen_function_ir(
        &self,
        func_name: &str,
        func_decl: &deno_ast::swc::ast::Function,
        param_types: &[crate::types::OatsType],
        ret_type: &crate::types::OatsType,
        receiver_name: Option<&str>,
    ) -> FunctionValue<'a> {
        let llvm_param_types: Vec<inkwell::types::BasicTypeEnum> = param_types
            .iter()
            .map(|t| self.map_type_to_llvm(t))
            .collect();

        // Build function type, supporting Void return.
        let fn_type = match ret_type {
            crate::types::OatsType::Number => {
                let ft = self.context.f64_type();
                let args: Vec<inkwell::types::BasicTypeEnum> = llvm_param_types.clone();
                ft.fn_type(&args.iter().map(|a| (*a).into()).collect::<Vec<_>>(), false)
            }
            crate::types::OatsType::Boolean => {
                let it = self.bool_t;
                let args: Vec<inkwell::types::BasicTypeEnum> = llvm_param_types.clone();
                it.fn_type(&args.iter().map(|a| (*a).into()).collect::<Vec<_>>(), false)
            }
            crate::types::OatsType::String | crate::types::OatsType::NominalStruct(_) => {
                let pt = self.context.ptr_type(AddressSpace::default());
                let args: Vec<inkwell::types::BasicTypeEnum> = llvm_param_types.clone();
                pt.fn_type(&args.iter().map(|a| (*a).into()).collect::<Vec<_>>(), false)
            }
            crate::types::OatsType::Array(_) => {
                // Arrays are represented as i8* (opaque pointer to runtime array)
                let pt = self.context.ptr_type(AddressSpace::default());
                let args: Vec<inkwell::types::BasicTypeEnum> = llvm_param_types.clone();
                pt.fn_type(&args.iter().map(|a| (*a).into()).collect::<Vec<_>>(), false)
            }
            crate::types::OatsType::Void => {
                let vt = self.context.void_type();
                let args: Vec<inkwell::types::BasicTypeEnum> = llvm_param_types.clone();
                vt.fn_type(&args.iter().map(|a| (*a).into()).collect::<Vec<_>>(), false)
            }
        };

        // Ensure helper runtime functions (like str_concat) are emitted into the module
        self.gen_str_concat();

        let function = self.module.add_function(func_name, fn_type, None);

        // Record the declared parameter types for this function so lowering
        // (e.g. member access for `this`) can inspect the nominal type of
        // the receiver. We store the original OatsType vector under the
        // function name for later lookup.
        self.fn_param_types
            .borrow_mut()
            .insert(func_name.to_string(), param_types.to_vec());

        let entry = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(entry);

        // Build param name -> index map from function declaration params.
        // If a receiver_name is provided, map it to index 0 and shift other
        // param indices by +1 to account for the implicit `this` leading
        // parameter which is present in `param_types` but not in the AST.
        let mut param_map: HashMap<String, u32> = HashMap::new();
        if let Some(rname) = receiver_name {
            param_map.insert(rname.to_string(), 0u32);
        }
        for (i, p) in func_decl.params.iter().enumerate() {
            use deno_ast::swc::ast::Pat;
            if let Pat::Ident(ident) = &p.pat {
                let name = ident.id.sym.to_string();
                // index in LLVM params is offset by 1 if receiver present
                let idx = if receiver_name.is_some() {
                    (i + 1) as u32
                } else {
                    i as u32
                };
                param_map.insert(name, idx);
            }
        }

        // Allocate stack slots for function parameters and store incoming
        // parameter values into them so parameters behave like locals.
        // We create these allocas in the entry block before emitting body.
        let mut locals_stack: Vec<
            HashMap<
                String,
                (
                    inkwell::values::PointerValue<'a>,
                    BasicTypeEnum<'a>,
                    bool,
                    bool,
                ),
            >,
        > = Vec::new();
        locals_stack.push(HashMap::new());
        for (name, idx) in &param_map {
            let i = *idx as usize;
            if i >= llvm_param_types.len() {
                continue;
            }
            let param_ty = llvm_param_types[i];
            // Create an alloca for the parameter's LLVM type
            let alloca = match self.builder.build_alloca(param_ty, name) {
                Ok(a) => a,
                Err(_) => {
                    // If allocation fails at IR build time, insert unreachable and skip this param
                    let _ = self.builder.build_unreachable();
                    continue;
                }
            };
            // Store the incoming parameter into the alloca
            if let Some(pv) = function.get_nth_param(*idx) {
                let _ = self.builder.build_store(alloca, pv);
                // If this parameter is a pointer type, increment refcount for the stored value
                if param_ty == self.i8ptr_t.as_basic_type_enum() {
                    let rc_inc = self.get_rc_inc();
                    let _ = self
                        .builder
                        .build_call(rc_inc, &[pv.into()], "rc_inc_param")
                        .expect("build_call failed");
                }
            } else {
                // Missing param value: skip storing and continue
                continue;
            }
        }

        if let Some(body) = &func_decl.body {
            use deno_ast::swc::ast;
            let mut emitted_return = false;
            // locals_stack holds per-scope HashMaps for local variables
            // (pointer, type, initialized, is_const). The current scope is
            // already present (we pushed param slots earlier).
            for stmt in &body.stmts {
                match stmt {
                    ast::Stmt::Return(ret) => {
                        if let Some(arg) = &ret.arg {
                            // Use result-based lowering so we can emit diagnostics centrally
                            match self.lower_expr_result(
                                arg,
                                function,
                                &param_map,
                                &mut locals_stack,
                                Some(function.get_name().to_str().unwrap_or("")),
                            ) {
                                Ok(val) => {
                                    self.emit_rc_dec_for_locals(&locals_stack);
                                    let _ = self.builder.build_return(Some(&val));
                                }
                                Err(d) => {
                                    // Emit diagnostic and return void to avoid panics
                                    crate::diagnostics::emit_diagnostic(&d, None);
                                    self.emit_rc_dec_for_locals(&locals_stack);
                                    let _ = self.builder.build_return(None);
                                }
                            }
                        } else {
                            let _ = self.builder.build_return(None);
                        }
                        emitted_return = true;
                        break;
                    }
                    ast::Stmt::Expr(expr_stmt) => {
                        use deno_ast::swc::ast;
                        // If this is an assignment statement and the LHS is a `this.field`
                        // pattern, lower it specially so we can compute the field slot and
                        // perform refcount updates. Prefer an AST-based check rather than
                        // scanning the source text for `this.` which is brittle.
                        if let ast::Expr::Assign(assign) = &*expr_stmt.expr {
                            // Try AST-based LHS detection first: left.as_member() -> MemberExpr
                            if let deno_ast::swc::ast::AssignTarget::Simple(simple_target) =
                                &assign.left
                                && let deno_ast::swc::ast::SimpleAssignTarget::Member(member) =
                                    simple_target
                                && let deno_ast::swc::ast::MemberProp::Ident(prop_ident) =
                                    &member.prop
                            {
                                // Ensure object is an identifier named `this` (or a param that maps to a nominal)
                                if let deno_ast::swc::ast::Expr::Ident(obj_ident) = &*member.obj {
                                    let obj_name = obj_ident.sym.to_string();
                                    if obj_name == "this" {
                                        // Lower RHS value
                                        if let Some(val) = self
                                            .lower_expr_result(
                                                &assign.right,
                                                function,
                                                &param_map,
                                                &mut locals_stack,
                                                Some(function.get_name().to_str().unwrap_or("")),
                                            )
                                            .ok()
                                        {
                                            // If we have a `this` parameter, the function param map should
                                            // contain its index (usually 0). Use the incoming parameter
                                            // value rather than the alloca to obtain the object pointer.
                                            if let Some(this_idx) = param_map.get("this")
                                                && let Some(pv) = function.get_nth_param(*this_idx)
                                                && let BasicValueEnum::PointerValue(obj_ptr) = pv
                                            {
                                                // lookup class name via fn_param_types
                                                if let Some(param_types) = self
                                                    .fn_param_types
                                                    .borrow()
                                                    .get(function.get_name().to_str().unwrap_or(""))
                                                    && !param_types.is_empty()
                                                    && let crate::types::OatsType::NominalStruct(n) =
                                                        &param_types[0]
                                                {
                                                    let class_name = n.clone();
                                                    if let Some(fields) =
                                                        self.class_fields.borrow().get(&class_name)
                                                    {
                                                        let field_name = prop_ident.sym.to_string();
                                                        if let Some((
                                                            field_idx,
                                                            (_fname, _field_ty),
                                                        )) = fields
                                                            .iter()
                                                            .enumerate()
                                                            .find(|(_, (n, _))| n == &field_name)
                                                        {
                                                            // compute byte offset = hdr_size + idx * ptr_size
                                                            let hdr_size = self.i64_t.const_int(
                                                                std::mem::size_of::<u64>() as u64,
                                                                false,
                                                            );
                                                            let ptr_sz = self.i64_t.const_int(
                                                                std::mem::size_of::<usize>() as u64,
                                                                false,
                                                            );
                                                            let idx_const = self
                                                                .i64_t
                                                                .const_int(field_idx as u64, false);
                                                            let mul = self
                                                                .builder
                                                                .build_int_mul(
                                                                    idx_const,
                                                                    ptr_sz,
                                                                    "fld_off_mul",
                                                                )
                                                                .expect("mul failed");
                                                            let offset = self
                                                                .builder
                                                                .build_int_add(
                                                                    hdr_size, mul, "fld_off",
                                                                )
                                                                .expect("add failed");
                                                            let offset_i32 = self
                                                                .builder
                                                                .build_int_cast(
                                                                    offset,
                                                                    self.context.i32_type(),
                                                                    "off_i32",
                                                                )
                                                                .expect("cast off_i32");
                                                            // GEP from i8* by byte offset
                                                            let gep_res = unsafe {
                                                                self.builder.build_gep(
                                                                    self.context.i8_type(),
                                                                    obj_ptr,
                                                                    &[offset_i32],
                                                                    "field_i8ptr",
                                                                )
                                                            };
                                                            let gep_ptr = match gep_res {
                                                                Ok(p) => p,
                                                                Err(_) => {
                                                                    let _ = self
                                                                        .builder
                                                                        .build_unreachable();
                                                                    // fallback to normal lowering
                                                                    self.lower_expr(
                                                                        &expr_stmt.expr,
                                                                        function,
                                                                        &param_map,
                                                                        &mut locals_stack,
                                                                    );
                                                                    continue;
                                                                }
                                                            };
                                                            // Cast to pointer slot and store appropriately
                                                            let slot_ptr_ty = self
                                                                .context
                                                                .ptr_type(AddressSpace::default());
                                                            let slot_ptr = match self
                                                                .builder
                                                                .build_pointer_cast(
                                                                    gep_ptr,
                                                                    slot_ptr_ty,
                                                                    "slot_ptr_cast",
                                                                ) {
                                                                Ok(p) => p,
                                                                Err(_) => {
                                                                    let _ = self
                                                                        .builder
                                                                        .build_unreachable();
                                                                    // fallback to normal lowering
                                                                    self.lower_expr(
                                                                        &expr_stmt.expr,
                                                                        function,
                                                                        &param_map,
                                                                        &mut locals_stack,
                                                                    );
                                                                    continue;
                                                                }
                                                            };
                                                            match val {
                                                                BasicValueEnum::PointerValue(
                                                                    newpv,
                                                                ) => {
                                                                    // load old value
                                                                    let old = match self
                                                                        .builder
                                                                        .build_load(
                                                                            self.i8ptr_t,
                                                                            slot_ptr,
                                                                            "old_field",
                                                                        ) {
                                                                        Ok(v) => v,
                                                                        Err(_) => {
                                                                            let _ = self
                                                                                .builder
                                                                                .build_unreachable(
                                                                                );
                                                                            // fallback
                                                                            self.lower_expr(
                                                                                &expr_stmt.expr,
                                                                                function,
                                                                                &param_map,
                                                                                &mut locals_stack,
                                                                            );
                                                                            continue;
                                                                        }
                                                                    };
                                                                    // call rc_dec on old (runtime should handle null)
                                                                    let rc_dec = self.get_rc_dec();
                                                                    let _ = match self
                                                                        .builder
                                                                        .build_call(
                                                                            rc_dec,
                                                                            &[old.into()],
                                                                            "rc_dec_old_field",
                                                                        ) {
                                                                        Ok(_) => (),
                                                                        Err(_) => {
                                                                            let _ = self
                                                                                .builder
                                                                                .build_unreachable(
                                                                                );
                                                                            continue;
                                                                        }
                                                                    };
                                                                    // store new pointer
                                                                    let _ = self
                                                                        .builder
                                                                        .build_store(
                                                                        slot_ptr,
                                                                        newpv.as_basic_value_enum(),
                                                                    );
                                                                    // increment refcount of new value
                                                                    let rc_inc = self.get_rc_inc();
                                                                    let _ = match self
                                                                        .builder
                                                                        .build_call(
                                                                            rc_inc,
                                                                            &[newpv.into()],
                                                                            "rc_inc_field",
                                                                        ) {
                                                                        Ok(_) => (),
                                                                        Err(_) => {
                                                                            let _ = self
                                                                                .builder
                                                                                .build_unreachable(
                                                                                );
                                                                            continue;
                                                                        }
                                                                    };
                                                                }
                                                                BasicValueEnum::FloatValue(fv) => {
                                                                    // cast slot to opaque pointer and store as f64
                                                                    let elem_ptr = match self.builder.build_pointer_cast(
                                                                        gep_ptr,
                                                                        self.context.ptr_type(AddressSpace::default()),
                                                                        "elem_f64_ptr",
                                                                    ) {
                                                                        Ok(p) => p,
                                                                        Err(_) => {
                                                                            let _ = self.builder.build_unreachable();
                                                                            self.lower_expr(
                                                                                &expr_stmt.expr,
                                                                                function,
                                                                                &param_map,
                                                                                &mut locals_stack,
                                                                            );
                                                                            continue;
                                                                        }
                                                                    };
                                                                    let _ = self
                                                                        .builder
                                                                        .build_store(elem_ptr, fv);
                                                                }
                                                                _ => {
                                                                    // unsupported RHS type for member write: fallback to normal lowering
                                                                    let _ = self.lower_expr(
                                                                        &expr_stmt.expr,
                                                                        function,
                                                                        &param_map,
                                                                        &mut locals_stack,
                                                                    );
                                                                }
                                                            }
                                                            // we've handled the assignment; skip default lowering
                                                            continue;
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        } // Fallback: Lower expression for side-effects (e.g., println calls)
                        let _ = self.lower_expr(
                            &expr_stmt.expr,
                            function,
                            &param_map,
                            &mut locals_stack,
                        );
                    }
                    ast::Stmt::Decl(ast::Decl::Var(vdecl)) => {
                        // Evaluate simple variable initializers and create allocas
                        use deno_ast::swc::ast::Pat;
                        // Decide const-ness for the whole declaration.
                        // Use `let` as the mutable option; `const` is immutable.
                        // Parser enforces that `var` is disallowed; codegen no longer emits a diagnostic here.
                        for decl in &vdecl.decls {
                            if let Pat::Ident(ident) = &decl.name {
                                let name = ident.id.sym.to_string();
                                let is_const_decl =
                                    matches!(vdecl.kind, deno_ast::swc::ast::VarDeclKind::Const);
                                // Create an alloca for this local at function entry
                                // and insert it into the current scope as uninitialized
                                if let Some(init) = &decl.init {
                                    if let Some(val) = self
                                        .lower_expr_result(
                                            init,
                                            function,
                                            &param_map,
                                            &mut locals_stack,
                                            Some(function.get_name().to_str().unwrap_or("")),
                                        )
                                        .ok()
                                    {
                                        match val {
                                            BasicValueEnum::FloatValue(fv) => {
                                                let alloca = match self
                                                    .builder
                                                    .build_alloca(self.f64_t, &name)
                                                {
                                                    Ok(a) => a,
                                                    Err(_) => {
                                                        let _ = self.builder.build_unreachable();
                                                        continue;
                                                    }
                                                };
                                                // insert as uninitialized first to model TDZ
                                                self.insert_local_current_scope(
                                                    &mut locals_stack,
                                                    name.clone(),
                                                    alloca,
                                                    self.f64_t.as_basic_type_enum(),
                                                    false,
                                                    is_const_decl,
                                                );
                                                let _ = self.builder.build_store(alloca, fv);
                                                // mark initialized after storing initializer
                                                self.set_local_initialized(
                                                    &mut locals_stack,
                                                    &name,
                                                    true,
                                                );
                                            }
                                            BasicValueEnum::PointerValue(pv) => {
                                                let ptr_ty = self.i8ptr_t;
                                                let alloca = match self
                                                    .builder
                                                    .build_alloca(ptr_ty, &name)
                                                {
                                                    Ok(a) => a,
                                                    Err(_) => {
                                                        let _ = self.builder.build_unreachable();
                                                        continue;
                                                    }
                                                };
                                                self.insert_local_current_scope(
                                                    &mut locals_stack,
                                                    name.clone(),
                                                    alloca,
                                                    ptr_ty.as_basic_type_enum(),
                                                    false,
                                                    is_const_decl,
                                                );
                                                let _ = self.builder.build_store(alloca, pv);
                                                // increment refcount for the newly-stored pointer
                                                let rc_inc = self.get_rc_inc();
                                                let _ = self
                                                    .builder
                                                    .build_call(
                                                        rc_inc,
                                                        &[pv.into()],
                                                        "rc_inc_var_init",
                                                    )
                                                    .expect("build_call failed");
                                                self.set_local_initialized(
                                                    &mut locals_stack,
                                                    &name,
                                                    true,
                                                );
                                            }
                                            BasicValueEnum::IntValue(iv) => {
                                                let boolt = self.bool_t;
                                                let alloca =
                                                    match self.builder.build_alloca(boolt, &name) {
                                                        Ok(a) => a,
                                                        Err(_) => {
                                                            let _ =
                                                                self.builder.build_unreachable();
                                                            continue;
                                                        }
                                                    };
                                                self.insert_local_current_scope(
                                                    &mut locals_stack,
                                                    name.clone(),
                                                    alloca,
                                                    boolt.as_basic_type_enum(),
                                                    false,
                                                    is_const_decl,
                                                );
                                                let _ = self.builder.build_store(alloca, iv);
                                                self.set_local_initialized(
                                                    &mut locals_stack,
                                                    &name,
                                                    true,
                                                );
                                            }
                                            _ => {
                                                // Unsupported initializer type; still insert uninitialized local
                                                // so future references can produce TDZ errors.
                                                let ptr_ty =
                                                    self.context.ptr_type(AddressSpace::default());
                                                let alloca = self
                                                    .builder
                                                    .build_alloca(ptr_ty, &name)
                                                    .expect("build_alloca failed");
                                                self.insert_local_current_scope(
                                                    &mut locals_stack,
                                                    name.clone(),
                                                    alloca,
                                                    ptr_ty.as_basic_type_enum(),
                                                    false,
                                                    is_const_decl,
                                                );
                                            }
                                        }
                                    }
                                } else {
                                    // No initializer: for const this is a syntax error in JS/TS
                                    // For now, insert uninitialized const/local and let later checks catch usage.
                                    let ptr_ty = self.context.ptr_type(AddressSpace::default());
                                    let alloca = match self.builder.build_alloca(ptr_ty, &name) {
                                        Ok(a) => a,
                                        Err(_) => {
                                            let _ = self.builder.build_unreachable();
                                            continue;
                                        }
                                    };
                                    self.insert_local_current_scope(
                                        &mut locals_stack,
                                        name.clone(),
                                        alloca,
                                        ptr_ty.as_basic_type_enum(),
                                        false,
                                        is_const_decl,
                                    );
                                }
                            }
                        }
                    }
                    ast::Stmt::If(if_stmt) => {
                        // Lower an if statement: create then/else/merge blocks and
                        // lower the consequent/alternative as statements.
                        // Lower the test to i1
                        if let Some(test_val) = self
                            .lower_expr_result(
                                &if_stmt.test,
                                function,
                                &param_map,
                                &mut locals_stack,
                                Some(function.get_name().to_str().unwrap_or("")),
                            )
                            .ok()
                        {
                            let cond_i1 = match test_val {
                                BasicValueEnum::IntValue(iv) => iv,
                                BasicValueEnum::FloatValue(fv) => {
                                    let zero = self.context.f64_type().const_float(0.0);

                                    self.builder
                                        .build_float_compare(
                                            inkwell::FloatPredicate::ONE,
                                            fv,
                                            zero,
                                            "if_tcmp",
                                        )
                                        .expect("build_float_compare failed")
                                }
                                _ => {
                                    // unsupported test type -> skip lowering
                                    continue;
                                }
                            };

                            let then_bb = self.context.append_basic_block(function, "if.then");
                            let else_bb = self.context.append_basic_block(function, "if.else");
                            let merge_bb = self.context.append_basic_block(function, "if.merge");

                            self.builder
                                .build_conditional_branch(cond_i1, then_bb, else_bb)
                                .expect("build_conditional_branch failed");

                            // then
                            self.builder.position_at_end(then_bb);
                            match &*if_stmt.cons {
                                ast::Stmt::Block(block) => {
                                    for s in &block.stmts {
                                        match s {
                                            ast::Stmt::Return(ret) => {
                                                if let Some(arg) = &ret.arg {
                                                    if let Some(val) = self
                                                        .lower_expr_result(
                                                            arg,
                                                            function,
                                                            &param_map,
                                                            &mut locals_stack,
                                                            Some(
                                                                function
                                                                    .get_name()
                                                                    .to_str()
                                                                    .unwrap_or(""),
                                                            ),
                                                        )
                                                        .ok()
                                                    {
                                                        self.emit_rc_dec_for_locals(&locals_stack);
                                                        let _ =
                                                            self.builder.build_return(Some(&val));
                                                    } else {
                                                        self.emit_rc_dec_for_locals(&locals_stack);
                                                        let _ = self.builder.build_return(None);
                                                    }
                                                } else {
                                                    let _ = self.builder.build_return(None);
                                                }
                                                emitted_return = true;
                                                break;
                                            }
                                            ast::Stmt::Expr(expr_stmt) => {
                                                let _ = self.lower_expr(
                                                    &expr_stmt.expr,
                                                    function,
                                                    &param_map,
                                                    &mut locals_stack,
                                                );
                                            }
                                            ast::Stmt::Decl(ast::Decl::Var(vdecl)) => {
                                                use deno_ast::swc::ast::Pat;
                                                for decl in &vdecl.decls {
                                                    if let Pat::Ident(ident) = &decl.name {
                                                        let name = ident.id.sym.to_string();
                                                        let is_const_decl = matches!(
                                                            vdecl.kind,
                                                            deno_ast::swc::ast::VarDeclKind::Const
                                                        );
                                                        if let Some(init) = &decl.init
                                                            && let Some(val) = self.lower_expr(
                                                                init,
                                                                function,
                                                                &param_map,
                                                                &mut locals_stack,
                                                            )
                                                        {
                                                            match val {
                                                                BasicValueEnum::FloatValue(fv) => {
                                                                    let alloca = self
                                                                        .builder
                                                                        .build_alloca(
                                                                            self.context.f64_type(),
                                                                            &name,
                                                                        )
                                                                        .expect(
                                                                            "build_alloca failed",
                                                                        );
                                                                    let _ = self
                                                                        .builder
                                                                        .build_store(alloca, fv);
                                                                    self.insert_local_current_scope(&mut locals_stack, name.clone(), alloca, self.f64_t.as_basic_type_enum(), true, is_const_decl);
                                                                }
                                                                BasicValueEnum::PointerValue(
                                                                    pv,
                                                                ) => {
                                                                    let ptr_ty =
                                                                        self.context.ptr_type(
                                                                            AddressSpace::default(),
                                                                        );
                                                                    let alloca = self
                                                                        .builder
                                                                        .build_alloca(ptr_ty, &name)
                                                                        .expect(
                                                                            "build_alloca failed",
                                                                        );
                                                                    let _ = self
                                                                        .builder
                                                                        .build_store(alloca, pv);
                                                                    self.insert_local_current_scope(&mut locals_stack, name.clone(), alloca, ptr_ty.as_basic_type_enum(), true, is_const_decl);
                                                                }
                                                                BasicValueEnum::IntValue(iv) => {
                                                                    let boolt = self.bool_t;
                                                                    let alloca = self
                                                                        .builder
                                                                        .build_alloca(boolt, &name)
                                                                        .expect(
                                                                            "build_alloca failed",
                                                                        );
                                                                    let _ = self
                                                                        .builder
                                                                        .build_store(alloca, iv);
                                                                    self.insert_local_current_scope(&mut locals_stack, name.clone(), alloca, boolt.as_basic_type_enum(), true, is_const_decl);
                                                                }
                                                                _ => {}
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                            _ => {}
                                        }
                                    }
                                }
                                ast::Stmt::Return(ret) => {
                                    if let Some(arg) = &ret.arg {
                                        if let Some(val) = self
                                            .lower_expr_result(
                                                arg,
                                                function,
                                                &param_map,
                                                &mut locals_stack,
                                                Some(function.get_name().to_str().unwrap_or("")),
                                            )
                                            .ok()
                                        {
                                            self.emit_rc_dec_for_locals(&locals_stack);
                                            let _ = self.builder.build_return(Some(&val));
                                        } else {
                                            self.emit_rc_dec_for_locals(&locals_stack);
                                            let _ = self.builder.build_return(None);
                                        }
                                    } else {
                                        let _ = self.builder.build_return(None);
                                    }
                                    emitted_return = true;
                                }
                                ast::Stmt::Expr(expr_stmt) => {
                                    let _ = self.lower_expr(
                                        &expr_stmt.expr,
                                        function,
                                        &param_map,
                                        &mut locals_stack,
                                    );
                                }
                                ast::Stmt::Decl(ast::Decl::Var(vdecl)) => {
                                    use deno_ast::swc::ast::Pat;
                                    for decl in &vdecl.decls {
                                        if let Pat::Ident(ident) = &decl.name {
                                            let name = ident.id.sym.to_string();
                                            let _is_const_decl = matches!(
                                                vdecl.kind,
                                                deno_ast::swc::ast::VarDeclKind::Const
                                            );
                                            if let Some(init) = &decl.init
                                                && let Some(val) = self.lower_expr(
                                                    init,
                                                    function,
                                                    &param_map,
                                                    &mut locals_stack,
                                                )
                                            {
                                                match val {
                                                    BasicValueEnum::FloatValue(fv) => {
                                                        let alloca = self
                                                            .builder
                                                            .build_alloca(self.f64_t, &name)
                                                            .expect("build_alloca failed");
                                                        let _ =
                                                            self.builder.build_store(alloca, fv);
                                                        self.insert_local_current_scope(
                                                            &mut locals_stack,
                                                            name.clone(),
                                                            alloca,
                                                            self.f64_t.as_basic_type_enum(),
                                                            true,
                                                            false,
                                                        );
                                                    }
                                                    BasicValueEnum::PointerValue(pv) => {
                                                        let ptr_ty = self
                                                            .context
                                                            .ptr_type(AddressSpace::default());
                                                        let alloca = self
                                                            .builder
                                                            .build_alloca(ptr_ty, &name)
                                                            .expect("build_alloca failed");
                                                        let _ =
                                                            self.builder.build_store(alloca, pv);
                                                        self.insert_local_current_scope(
                                                            &mut locals_stack,
                                                            name.clone(),
                                                            alloca,
                                                            ptr_ty.as_basic_type_enum(),
                                                            true,
                                                            false,
                                                        );
                                                    }
                                                    BasicValueEnum::IntValue(iv) => {
                                                        let boolt = self.bool_t;
                                                        let alloca = self
                                                            .builder
                                                            .build_alloca(boolt, &name)
                                                            .expect("build_alloca failed");
                                                        let _ =
                                                            self.builder.build_store(alloca, iv);
                                                        self.insert_local_current_scope(
                                                            &mut locals_stack,
                                                            name.clone(),
                                                            alloca,
                                                            boolt.as_basic_type_enum(),
                                                            true,
                                                            false,
                                                        );
                                                    }
                                                    _ => {}
                                                }
                                            }
                                        }
                                    }
                                }
                                _ => {
                                    // unsupported consequent stmt type
                                }
                            }

                            if self.builder.get_insert_block().is_some() {
                                let _ = self
                                    .builder
                                    .build_unconditional_branch(merge_bb)
                                    .expect("build_unconditional_branch failed");
                            }

                            // else
                            self.builder.position_at_end(else_bb);
                            if let Some(alt) = &if_stmt.alt {
                                match &**alt {
                                    ast::Stmt::Block(block) => {
                                        for s in &block.stmts {
                                            match s {
                                                ast::Stmt::Return(ret) => {
                                                    if let Some(arg) = &ret.arg {
                                                        if let Some(val) = self.lower_expr_result(arg, function, &param_map, &mut locals_stack, Some(function.get_name().to_str().unwrap_or(""))).ok() {
                                                            let _ = self
                                                                .builder
                                                                .build_return(Some(&val));
                                                        } else {
                                                            let _ = self.builder.build_return(None);
                                                        }
                                                    } else {
                                                        let _ = self.builder.build_return(None);
                                                    }
                                                    emitted_return = true;
                                                    break;
                                                }
                                                ast::Stmt::Expr(expr_stmt) => {
                                                    let _ = self.lower_expr(
                                                        &expr_stmt.expr,
                                                        function,
                                                        &param_map,
                                                        &mut locals_stack,
                                                    );
                                                }
                                                ast::Stmt::Decl(ast::Decl::Var(vdecl)) => {
                                                    use deno_ast::swc::ast::Pat;
                                                    for decl in &vdecl.decls {
                                                        if let Pat::Ident(ident) = &decl.name {
                                                            let name = ident.id.sym.to_string();
                                                            let is_const_decl = matches!(vdecl.kind, deno_ast::swc::ast::VarDeclKind::Const);
                                                            if let Some(init) = &decl.init
                                                                && let Some(val) = self.lower_expr(
                                                                    init,
                                                                    function,
                                                                    &param_map,
                                                                    &mut locals_stack,
                                                                )
                                                            {
                                                                match val {
                                                                        BasicValueEnum::FloatValue(fv) => {
                                                                            let alloca = self.builder.build_alloca(self.f64_t, &name).expect("build_alloca failed");
                                                                            let _ = self.builder.build_store(alloca, fv);
                                                                            self.insert_local_current_scope(&mut locals_stack, name.clone(), alloca, self.f64_t.as_basic_type_enum(), true, is_const_decl);
                                                                        }
                                                                        BasicValueEnum::PointerValue(pv) => {
                                                                            let ptr_ty = self.context.ptr_type(AddressSpace::default());
                                                                            let alloca = self.builder.build_alloca(ptr_ty, &name).expect("build_alloca failed");
                                                                            let _ = self.builder.build_store(alloca, pv);
                                                                            self.insert_local_current_scope(&mut locals_stack, name.clone(), alloca, ptr_ty.as_basic_type_enum(), true, is_const_decl);
                                                                        }
                                                                        BasicValueEnum::IntValue(iv) => {
                                                                            let boolt = self.bool_t;
                                                                            let alloca = self.builder.build_alloca(boolt, &name).expect("build_alloca failed");
                                                                            let _ = self.builder.build_store(alloca, iv);
                                                                            self.insert_local_current_scope(&mut locals_stack, name.clone(), alloca, boolt.as_basic_type_enum(), true, is_const_decl);
                                                                        }
                                                                        _ => {}
                                                                    }
                                                            }
                                                        }
                                                    }
                                                }
                                                _ => {}
                                            }
                                        }
                                    }
                                    ast::Stmt::Return(ret) => {
                                        if let Some(arg) = &ret.arg {
                                            if let Some(val) = self.lower_expr_result(arg, function, &param_map, &mut locals_stack, Some(function.get_name().to_str().unwrap_or(""))).ok() {
                                                let _ = self.builder.build_return(Some(&val));
                                            } else {
                                                let _ = self.builder.build_return(None);
                                            }
                                        } else {
                                            let _ = self.builder.build_return(None);
                                        }
                                        emitted_return = true;
                                    }
                                    ast::Stmt::Expr(expr_stmt) => {
                                        let _ = self.lower_expr(
                                            &expr_stmt.expr,
                                            function,
                                            &param_map,
                                            &mut locals_stack,
                                        );
                                    }
                                    ast::Stmt::Decl(ast::Decl::Var(vdecl)) => {
                                        use deno_ast::swc::ast::Pat;
                                        for decl in &vdecl.decls {
                                            if let Pat::Ident(ident) = &decl.name {
                                                let name = ident.id.sym.to_string();
                                                let _is_const_decl = matches!(
                                                    vdecl.kind,
                                                    deno_ast::swc::ast::VarDeclKind::Const
                                                );
                                                if let Some(init) = &decl.init
                                                    && let Some(val) = self.lower_expr(
                                                        init,
                                                        function,
                                                        &param_map,
                                                        &mut locals_stack,
                                                    )
                                                {
                                                    match val {
                                                        BasicValueEnum::FloatValue(fv) => {
                                                            let alloca = self
                                                                .builder
                                                                .build_alloca(self.f64_t, &name)
                                                                .expect("build_alloca failed");
                                                            let _ = self
                                                                .builder
                                                                .build_store(alloca, fv);
                                                            self.insert_local_current_scope(
                                                                &mut locals_stack,
                                                                name.clone(),
                                                                alloca,
                                                                self.f64_t.as_basic_type_enum(),
                                                                true,
                                                                false,
                                                            );
                                                        }
                                                        BasicValueEnum::PointerValue(pv) => {
                                                            let ptr_ty = self
                                                                .context
                                                                .ptr_type(AddressSpace::default());
                                                            let alloca = self
                                                                .builder
                                                                .build_alloca(ptr_ty, &name)
                                                                .expect("build_alloca failed");
                                                            let _ = self
                                                                .builder
                                                                .build_store(alloca, pv);
                                                            self.insert_local_current_scope(
                                                                &mut locals_stack,
                                                                name.clone(),
                                                                alloca,
                                                                ptr_ty.as_basic_type_enum(),
                                                                true,
                                                                false,
                                                            );
                                                        }
                                                        BasicValueEnum::IntValue(iv) => {
                                                            let boolt = self.bool_t;
                                                            let alloca = self
                                                                .builder
                                                                .build_alloca(boolt, &name)
                                                                .expect("build_alloca failed");
                                                            let _ = self
                                                                .builder
                                                                .build_store(alloca, iv);
                                                            self.insert_local_current_scope(
                                                                &mut locals_stack,
                                                                name.clone(),
                                                                alloca,
                                                                boolt.as_basic_type_enum(),
                                                                true,
                                                                false,
                                                            );
                                                        }
                                                        _ => {}
                                                    }
                                                }
                                            }
                                        }
                                    }
                                    _ => {
                                        // unsupported alt stmt type
                                    }
                                }
                            }

                            if self.builder.get_insert_block().is_some() {
                                let _ = self
                                    .builder
                                    .build_unconditional_branch(merge_bb)
                                    .expect("build_unconditional_branch failed");
                            }

                            // continue at merge
                            self.builder.position_at_end(merge_bb);
                        }
                    }
                    ast::Stmt::For(forstmt) => {
                        // Build blocks for for-loop: cond -> body -> update -> end
                        let cond_bb = self.context.append_basic_block(function, "for.cond");
                        let body_bb = self.context.append_basic_block(function, "for.body");
                        let update_bb = self.context.append_basic_block(function, "for.update");
                        let end_bb = self.context.append_basic_block(function, "for.end");

                        // init
                        if let Some(init) = &forstmt.init {
                            match init {
                                ast::VarDeclOrExpr::VarDecl(vdecl) => {
                                    for decl in &vdecl.decls {
                                        use deno_ast::swc::ast::Pat;
                                        if let Pat::Ident(ident) = &decl.name {
                                            let name = ident.id.sym.to_string();
                                            let is_const_decl = matches!(
                                                vdecl.kind,
                                                deno_ast::swc::ast::VarDeclKind::Const
                                            );
                                            if let Some(init_expr) = &decl.init {
                                                if let Some(val) = self.lower_expr_result(init_expr, function, &param_map, &mut locals_stack, Some(function.get_name().to_str().unwrap_or(""))).ok() {
                                                    match val {
                                                        BasicValueEnum::FloatValue(fv) => {
                                                            let alloca = self
                                                                .builder
                                                                .build_alloca(self.f64_t, &name)
                                                                .expect("build_alloca failed");
                                                            self.insert_local_current_scope(
                                                                &mut locals_stack,
                                                                name.clone(),
                                                                alloca,
                                                                self.f64_t.as_basic_type_enum(),
                                                                false,
                                                                is_const_decl,
                                                            );
                                                            let _ = self
                                                                .builder
                                                                .build_store(alloca, fv);
                                                            self.set_local_initialized(
                                                                &mut locals_stack,
                                                                &name,
                                                                true,
                                                            );
                                                        }
                                                        BasicValueEnum::PointerValue(pv) => {
                                                            let ptr_ty = self.i8ptr_t;
                                                            let alloca = self
                                                                .builder
                                                                .build_alloca(ptr_ty, &name)
                                                                .expect("build_alloca failed");
                                                            self.insert_local_current_scope(
                                                                &mut locals_stack,
                                                                name.clone(),
                                                                alloca,
                                                                ptr_ty.as_basic_type_enum(),
                                                                false,
                                                                is_const_decl,
                                                            );
                                                            let _ = self
                                                                .builder
                                                                .build_store(alloca, pv);
                                                            let rc_inc = self.get_rc_inc();
                                                            let _ = self
                                                                .builder
                                                                .build_call(
                                                                    rc_inc,
                                                                    &[pv.into()],
                                                                    "rc_inc_var_init",
                                                                )
                                                                .expect("build_call failed");
                                                            self.set_local_initialized(
                                                                &mut locals_stack,
                                                                &name,
                                                                true,
                                                            );
                                                        }
                                                        BasicValueEnum::IntValue(iv) => {
                                                            let boolt = self.bool_t;
                                                            let alloca = self
                                                                .builder
                                                                .build_alloca(boolt, &name)
                                                                .expect("build_alloca failed");
                                                            self.insert_local_current_scope(
                                                                &mut locals_stack,
                                                                name.clone(),
                                                                alloca,
                                                                boolt.as_basic_type_enum(),
                                                                false,
                                                                is_const_decl,
                                                            );
                                                            let _ = self
                                                                .builder
                                                                .build_store(alloca, iv);
                                                            self.set_local_initialized(
                                                                &mut locals_stack,
                                                                &name,
                                                                true,
                                                            );
                                                        }
                                                        _ => {}
                                                    }
                                                }
                                            } else {
                                                let alloca = self
                                                    .builder
                                                    .build_alloca(self.f64_t, &name)
                                                    .expect("build_alloca failed");
                                                self.insert_local_current_scope(
                                                    &mut locals_stack,
                                                    name.clone(),
                                                    alloca,
                                                    self.f64_t.as_basic_type_enum(),
                                                    false,
                                                    is_const_decl,
                                                );
                                            }
                                        }
                                    }
                                }
                                ast::VarDeclOrExpr::Expr(e) => {
                                    let _ =
                                        self.lower_expr(e, function, &param_map, &mut locals_stack);
                                }
                            }
                        }

                        if self.builder.get_insert_block().is_some() {
                            let _ = self
                                .builder
                                .build_unconditional_branch(cond_bb)
                                .expect("build_unconditional_branch failed");
                        }

                        // cond
                        self.builder.position_at_end(cond_bb);
                        if let Some(test) = &forstmt.test {
                            if let Some(tv) =
                                self.lower_expr(test, function, &param_map, &mut locals_stack)
                            {
                                if let Some(cond) = self.to_condition_i1(tv) {
                                    self.builder
                                        .build_conditional_branch(cond, body_bb, end_bb)
                                        .expect("build_conditional_branch failed");
                                } else {
                                    self.builder
                                        .build_unconditional_branch(end_bb)
                                        .expect("build_unconditional_branch failed");
                                }
                            } else {
                                self.builder
                                    .build_unconditional_branch(end_bb)
                                    .expect("build_unconditional_branch failed");
                            }
                        } else {
                            self.builder
                                .build_unconditional_branch(body_bb)
                                .expect("build_unconditional_branch failed");
                        }

                        // body
                        self.builder.position_at_end(body_bb);
                        locals_stack.push(HashMap::new());
                        let mut body_returned = false;
                        match &*forstmt.body {
                            ast::Stmt::Block(block) => {
                                for s in &block.stmts {
                                    match s {
                                        ast::Stmt::Return(ret) => {
                                            if let Some(arg) = &ret.arg {
                                                if let Some(val) = self
                                                    .lower_expr_result(
                                                        arg,
                                                        function,
                                                        &param_map,
                                                        &mut locals_stack,
                                                        Some(
                                                            function
                                                                .get_name()
                                                                .to_str()
                                                                .unwrap_or(""),
                                                        ),
                                                    )
                                                    .ok()
                                                {
                                                    self.emit_rc_dec_for_locals(&locals_stack);
                                                    let _ = self.builder.build_return(Some(&val));
                                                } else {
                                                    self.emit_rc_dec_for_locals(&locals_stack);
                                                    let _ = self.builder.build_return(None);
                                                }
                                            } else {
                                                let _ = self.builder.build_return(None);
                                            }
                                            body_returned = true;
                                            break;
                                        }
                                        ast::Stmt::Expr(expr_stmt) => {
                                            let _ = self.lower_expr(
                                                &expr_stmt.expr,
                                                function,
                                                &param_map,
                                                &mut locals_stack,
                                            );
                                        }
                                        ast::Stmt::Decl(ast::Decl::Var(vdecl)) => {
                                            use deno_ast::swc::ast::Pat;
                                            for decl in &vdecl.decls {
                                                if let Pat::Ident(ident) = &decl.name {
                                                    let name = ident.id.sym.to_string();
                                                    let _is_const_decl = matches!(
                                                        vdecl.kind,
                                                        deno_ast::swc::ast::VarDeclKind::Const
                                                    );
                                                    if let Some(init) = &decl.init
                                                        && let Some(val) = self.lower_expr(
                                                            init,
                                                            function,
                                                            &param_map,
                                                            &mut locals_stack,
                                                        )
                                                    {
                                                        match val {
                                                            BasicValueEnum::FloatValue(fv) => {
                                                                let alloca = self
                                                                    .builder
                                                                    .build_alloca(self.f64_t, &name)
                                                                    .expect("build_alloca failed");
                                                                let _ = self
                                                                    .builder
                                                                    .build_store(alloca, fv);
                                                                self.insert_local_current_scope(
                                                                    &mut locals_stack,
                                                                    name.clone(),
                                                                    alloca,
                                                                    self.f64_t.as_basic_type_enum(),
                                                                    true,
                                                                    false,
                                                                );
                                                            }
                                                            BasicValueEnum::PointerValue(pv) => {
                                                                let ptr_ty = self.context.ptr_type(
                                                                    AddressSpace::default(),
                                                                );
                                                                let alloca = self
                                                                    .builder
                                                                    .build_alloca(ptr_ty, &name)
                                                                    .expect("build_alloca failed");
                                                                let _ = self
                                                                    .builder
                                                                    .build_store(alloca, pv);
                                                                self.insert_local_current_scope(
                                                                    &mut locals_stack,
                                                                    name.clone(),
                                                                    alloca,
                                                                    ptr_ty.as_basic_type_enum(),
                                                                    true,
                                                                    false,
                                                                );
                                                            }
                                                            BasicValueEnum::IntValue(iv) => {
                                                                let boolt = self.bool_t;
                                                                let alloca = self
                                                                    .builder
                                                                    .build_alloca(boolt, &name)
                                                                    .expect("build_alloca failed");
                                                                let _ = self
                                                                    .builder
                                                                    .build_store(alloca, iv);
                                                                self.insert_local_current_scope(
                                                                    &mut locals_stack,
                                                                    name.clone(),
                                                                    alloca,
                                                                    boolt.as_basic_type_enum(),
                                                                    true,
                                                                    false,
                                                                );
                                                            }
                                                            _ => {}
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                        _ => {}
                                    }
                                }
                            }
                            ast::Stmt::Return(ret) => {
                                if let Some(arg) = &ret.arg {
                                    if let Some(val) = self.lower_expr_result(arg, function, &param_map, &mut locals_stack, Some(function.get_name().to_str().unwrap_or(""))).ok() {
                                        self.emit_rc_dec_for_locals(&locals_stack);
                                        let _ = self.builder.build_return(Some(&val));
                                    } else {
                                        self.emit_rc_dec_for_locals(&locals_stack);
                                        let _ = self.builder.build_return(None);
                                    }
                                } else {
                                    let _ = self.builder.build_return(None);
                                }
                                body_returned = true;
                            }
                            ast::Stmt::Expr(expr_stmt) => {
                                let _ = self.lower_expr(
                                    &expr_stmt.expr,
                                    function,
                                    &param_map,
                                    &mut locals_stack,
                                );
                            }
                            _ => {}
                        }
                        if !body_returned && self.builder.get_insert_block().is_some() {
                            let _ = self
                                .builder
                                .build_unconditional_branch(update_bb)
                                .expect("build_unconditional_branch failed");
                        }
                        locals_stack.pop();

                        // update
                        self.builder.position_at_end(update_bb);
                        if let Some(update) = &forstmt.update {
                            let _ =
                                self.lower_expr(update, function, &param_map, &mut locals_stack);
                        }
                        if self.builder.get_insert_block().is_some() {
                            let _ = self
                                .builder
                                .build_unconditional_branch(cond_bb)
                                .expect("build_unconditional_branch failed");
                        }

                        // continue after loop
                        self.builder.position_at_end(end_bb);
                    }
                    ast::Stmt::ForOf(forof) => {
                        // for (let x of y) { body }
                        // Lower the RHS (iterable) and expect an array-like pointer
                        if let Some(arr_val) =
                            self.lower_expr(&forof.right, function, &param_map, &mut locals_stack)
                            && let BasicValueEnum::PointerValue(arr_ptr) = arr_val
                        {
                            // Setup blocks: cond -> body -> inc -> end
                            let cond_bb = self.context.append_basic_block(function, "forof.cond");
                            let body_bb = self.context.append_basic_block(function, "forof.body");
                            let inc_bb = self.context.append_basic_block(function, "forof.inc");
                            let end_bb = self.context.append_basic_block(function, "forof.end");

                            // create index alloca (i64) in entry of surrounding function
                            let idx_alloca = self
                                .builder
                                .build_alloca(self.i64_t, "forof_idx")
                                .expect("build_alloca failed");
                            let zero = self.i64_t.const_int(0, false);
                            let _ = self.builder.build_store(idx_alloca, zero);

                            // Create a local for the loop variable in the current scope (uninitialized)
                            let loop_var_name = match &forof.left {
                                ast::ForHead::VarDecl(boxed_vd) => {
                                    let vd = &**boxed_vd;
                                    // expect single decl with Ident pattern
                                    if let Some(decl) = vd.decls.first() {
                                        if let ast::Pat::Ident(ident) = &decl.name {
                                            Some(ident.id.sym.to_string())
                                        } else {
                                            None
                                        }
                                    } else {
                                        None
                                    }
                                }
                                ast::ForHead::Pat(boxed_p) => {
                                    if let ast::Pat::Ident(ident) = &**boxed_p {
                                        Some(ident.id.sym.to_string())
                                    } else {
                                        None
                                    }
                                }
                                _ => None,
                            };

                            // allocate placeholder alloca for the loop var (use ptr type to be generic)
                            let mut loop_var_alloca = None;
                            if let Some(name) = &loop_var_name {
                                let ptr_ty = self.i8ptr_t;
                                let alloca = self
                                    .builder
                                    .build_alloca(ptr_ty, name)
                                    .expect("build_alloca failed");
                                // Insert as uninitialized local
                                self.insert_local_current_scope(
                                    &mut locals_stack,
                                    name.clone(),
                                    alloca,
                                    ptr_ty.as_basic_type_enum(),
                                    false,
                                    false,
                                );
                                loop_var_alloca = Some((name.clone(), alloca));
                            }

                            // jump to cond
                            if self.builder.get_insert_block().is_some() {
                                let _ = self
                                    .builder
                                    .build_unconditional_branch(cond_bb)
                                    .expect("build_unconditional_branch failed");
                            }

                            // cond: load length from arr (second u64 at offset sizeof(u64)) and compare idx < len
                            self.builder.position_at_end(cond_bb);
                            // compute len: load u64 at arr_ptr + 8
                            // arr_ptr is i8* ; GEP by 8 to length location
                            let off_const = self
                                .i32_t
                                .const_int(std::mem::size_of::<u64>() as u64, false);
                            let len_i8_res = unsafe {
                                self.builder.build_gep(
                                    self.context.i8_type(),
                                    arr_ptr,
                                    &[off_const],
                                    "len_i8",
                                )
                            };
                            if let Ok(len_i8) = len_i8_res {
                                let len_ptr = self
                                    .builder
                                    .build_pointer_cast(
                                        len_i8,
                                        self.context.ptr_type(AddressSpace::default()),
                                        "len_ptr",
                                    )
                                    .expect("cast len_ptr failed");
                                let len_loaded = self
                                    .builder
                                    .build_load(self.i64_t, len_ptr, "len_load")
                                    .expect("load len failed");
                                let len_i64 = len_loaded.into_int_value();
                                let idx_loaded = self
                                    .builder
                                    .build_load(self.i64_t, idx_alloca, "idx_load")
                                    .expect("load idx failed")
                                    .into_int_value();
                                let cond = self
                                    .builder
                                    .build_int_compare(
                                        inkwell::IntPredicate::ULT,
                                        idx_loaded,
                                        len_i64,
                                        "forof_cond",
                                    )
                                    .expect("build_int_compare failed");
                                self.builder
                                    .build_conditional_branch(cond, body_bb, end_bb)
                                    .expect("build_conditional_branch failed");
                            } else {
                                // failed to compute length -> skip loop
                                self.builder
                                    .build_unconditional_branch(end_bb)
                                    .expect("build_unconditional_branch failed");
                            }

                            // body
                            self.builder.position_at_end(body_bb);
                            locals_stack.push(HashMap::new());

                            // load header to determine element-kind flag (elem_is_number stored in high 32 bits)
                            // header is at arr_ptr as u64
                            let header_ptr = match self.builder.build_pointer_cast(
                                arr_ptr,
                                self.context.ptr_type(AddressSpace::default()),
                                "header_ptr",
                            ) {
                                Ok(p) => p,
                                Err(_) => {
                                    // failed to inspect header -> skip loop body
                                    self.builder
                                        .build_unconditional_branch(end_bb)
                                        .expect("build_unconditional_branch failed");
                                    continue;
                                }
                            };
                            let header_loaded =
                                match self
                                    .builder
                                    .build_load(self.i64_t, header_ptr, "header_load")
                                {
                                    Ok(v) => v.into_int_value(),
                                    Err(_) => {
                                        self.builder
                                            .build_unconditional_branch(end_bb)
                                            .expect("build_unconditional_branch failed");
                                        continue;
                                    }
                                };
                            // shift right by 32
                            let shift_amt = self.i64_t.const_int(32, false);
                            let header_shr = self
                                .builder
                                .build_right_shift(header_loaded, shift_amt, false, "header_shr")
                                .expect("build_right_shift failed");
                            let one = self.i64_t.const_int(1, false);
                            let is_number = self
                                .builder
                                .build_int_compare(
                                    inkwell::IntPredicate::EQ,
                                    header_shr,
                                    one,
                                    "is_number",
                                )
                                .expect("build_int_compare failed");

                            // create blocks for number vs pointer element handling
                            let num_bb =
                                self.context.append_basic_block(function, "forof.elem_num");
                            let ptr_bb =
                                self.context.append_basic_block(function, "forof.elem_ptr");
                            let after_elem_bb = self
                                .context
                                .append_basic_block(function, "forof.after_elem");
                            self.builder
                                .build_conditional_branch(is_number, num_bb, ptr_bb)
                                .expect("build_conditional_branch failed");

                            // number path: call array_get_f64(arr, idx)
                            self.builder.position_at_end(num_bb);
                            let array_get_f64 = self.get_array_get_f64();
                            let idx_loaded = self
                                .builder
                                .build_load(self.i64_t, idx_alloca, "idx_load2")
                                .expect("load idx failed")
                                .into_int_value();
                            let cs = match self.builder.build_call(
                                array_get_f64,
                                &[arr_ptr.into(), idx_loaded.into()],
                                "array_get_f64_call",
                            ) {
                                Ok(cs) => cs,
                                Err(_) => {
                                    self.builder
                                        .build_unconditional_branch(after_elem_bb)
                                        .expect("build_unconditional_branch failed");
                                    continue;
                                }
                            };
                            if let inkwell::Either::Left(bv) = cs.try_as_basic_value() {
                                // store numeric value into loop var: bitcast alloca to f64* and store
                                if let Some((name, alloca)) = &loop_var_alloca {
                                    let elem_f64 = bv.into_float_value();
                                    let elem_ptr = match self.builder.build_pointer_cast(
                                        *alloca,
                                        self.context.ptr_type(AddressSpace::default()),
                                        "elem_f64_ptr",
                                    ) {
                                        Ok(p) => p,
                                        Err(_) => {
                                            self.builder
                                                .build_unconditional_branch(after_elem_bb)
                                                .expect("build_unconditional_branch failed");
                                            continue;
                                        }
                                    };
                                    let _ = self.builder.build_store(elem_ptr, elem_f64);
                                    // mark initialized
                                    self.set_local_initialized(&mut locals_stack, name, true);
                                }
                            }
                            if self.builder.get_insert_block().is_some() {
                                let _ = self
                                    .builder
                                    .build_unconditional_branch(after_elem_bb)
                                    .expect("build_unconditional_branch failed");
                            }

                            // pointer path: call array_get_ptr(arr, idx)
                            self.builder.position_at_end(ptr_bb);
                            let array_get_ptr = self.get_array_get_ptr();
                            let idx_loaded2 = self
                                .builder
                                .build_load(self.i64_t, idx_alloca, "idx_load3")
                                .expect("load idx failed")
                                .into_int_value();
                            let cs2 = match self.builder.build_call(
                                array_get_ptr,
                                &[arr_ptr.into(), idx_loaded2.into()],
                                "array_get_ptr_call",
                            ) {
                                Ok(cs) => cs,
                                Err(_) => {
                                    self.builder
                                        .build_unconditional_branch(after_elem_bb)
                                        .expect("build_unconditional_branch failed");
                                    continue;
                                }
                            };
                            if let inkwell::Either::Left(bv2) = cs2.try_as_basic_value()
                                && let Some((name, alloca)) = &loop_var_alloca
                            {
                                // If already initialized, decref old
                                if let Some((_ptr, _ty, init, _is_const)) =
                                    self.find_local(&locals_stack, name)
                                    && init
                                {
                                    // load old pointer value and call rc_dec
                                    if let Ok(old_loaded) = self.builder.build_load(
                                        self.i8ptr_t.as_basic_type_enum(),
                                        *alloca,
                                        "old_load",
                                    ) && let BasicValueEnum::PointerValue(oldpv) = old_loaded
                                    {
                                        let rc_dec = self.get_rc_dec();
                                        let _ = self
                                            .builder
                                            .build_call(rc_dec, &[oldpv.into()], "rc_dec_old")
                                            .expect("build_call failed");
                                    }
                                }
                                let pv = bv2.into_pointer_value();
                                let _ = self.builder.build_store(*alloca, pv.as_basic_value_enum());
                                // increment refcount for new value
                                let rc_inc = self.get_rc_inc();
                                let _ = self
                                    .builder
                                    .build_call(rc_inc, &[pv.into()], "rc_inc_forof")
                                    .expect("build_call failed");
                                self.set_local_initialized(&mut locals_stack, name, true);
                            }
                            if self.builder.get_insert_block().is_some() {
                                let _ = self
                                    .builder
                                    .build_unconditional_branch(after_elem_bb)
                                    .expect("build_unconditional_branch failed");
                            }

                            // after element retrieval: lower the loop body (user provided)
                            self.builder.position_at_end(after_elem_bb);
                            match &*forof.body {
                                ast::Stmt::Block(block) => {
                                    for s in &block.stmts {
                                        match s {
                                            ast::Stmt::Return(ret) => {
                                                if let Some(arg) = &ret.arg {
                                                    if let Some(val) = self.lower_expr(
                                                        arg,
                                                        function,
                                                        &param_map,
                                                        &mut locals_stack,
                                                    ) {
                                                        self.emit_rc_dec_for_locals(&locals_stack);
                                                        let _ =
                                                            self.builder.build_return(Some(&val));
                                                    } else {
                                                        self.emit_rc_dec_for_locals(&locals_stack);
                                                        let _ = self.builder.build_return(None);
                                                    }
                                                } else {
                                                    let _ = self.builder.build_return(None);
                                                }
                                                break;
                                            }
                                            ast::Stmt::Expr(expr_stmt) => {
                                                let _ = self.lower_expr(
                                                    &expr_stmt.expr,
                                                    function,
                                                    &param_map,
                                                    &mut locals_stack,
                                                );
                                            }
                                            ast::Stmt::Decl(ast::Decl::Var(vdecl)) => {
                                                use deno_ast::swc::ast::Pat;
                                                for decl in &vdecl.decls {
                                                    if let Pat::Ident(ident) = &decl.name {
                                                        let name = ident.id.sym.to_string();
                                                        let is_const_decl = matches!(
                                                            vdecl.kind,
                                                            deno_ast::swc::ast::VarDeclKind::Const
                                                        );
                                                        if let Some(init) = &decl.init
                                                            && let Some(val) = self.lower_expr(
                                                                init,
                                                                function,
                                                                &param_map,
                                                                &mut locals_stack,
                                                            )
                                                        {
                                                            match val {
                                                                BasicValueEnum::FloatValue(fv) => {
                                                                    let alloca = self
                                                                        .builder
                                                                        .build_alloca(
                                                                            self.f64_t, &name,
                                                                        )
                                                                        .expect(
                                                                            "build_alloca failed",
                                                                        );
                                                                    let _ = self
                                                                        .builder
                                                                        .build_store(alloca, fv);
                                                                    self.insert_local_current_scope(&mut locals_stack, name.clone(), alloca, self.f64_t.as_basic_type_enum(), true, is_const_decl);
                                                                }
                                                                BasicValueEnum::PointerValue(
                                                                    pv,
                                                                ) => {
                                                                    let ptr_ty =
                                                                        self.context.ptr_type(
                                                                            AddressSpace::default(),
                                                                        );
                                                                    let alloca = self
                                                                        .builder
                                                                        .build_alloca(ptr_ty, &name)
                                                                        .expect(
                                                                            "build_alloca failed",
                                                                        );
                                                                    let _ = self
                                                                        .builder
                                                                        .build_store(alloca, pv);
                                                                    self.insert_local_current_scope(&mut locals_stack, name.clone(), alloca, ptr_ty.as_basic_type_enum(), true, is_const_decl);
                                                                }
                                                                BasicValueEnum::IntValue(iv) => {
                                                                    let boolt = self.bool_t;
                                                                    let alloca = self
                                                                        .builder
                                                                        .build_alloca(boolt, &name)
                                                                        .expect(
                                                                            "build_alloca failed",
                                                                        );
                                                                    let _ = self
                                                                        .builder
                                                                        .build_store(alloca, iv);
                                                                    self.insert_local_current_scope(&mut locals_stack, name.clone(), alloca, boolt.as_basic_type_enum(), true, is_const_decl);
                                                                }
                                                                _ => {}
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                            _ => {}
                                        }
                                    }
                                }
                                ast::Stmt::Expr(expr_stmt) => {
                                    let _ = self.lower_expr(
                                        &expr_stmt.expr,
                                        function,
                                        &param_map,
                                        &mut locals_stack,
                                    );
                                }
                                _ => {}
                            }

                            // after body, branch to inc
                            if self.builder.get_insert_block().is_some() {
                                let _ = self
                                    .builder
                                    .build_unconditional_branch(inc_bb)
                                    .expect("build_unconditional_branch failed");
                            }

                            // pop inner scope
                            locals_stack.pop();

                            // inc: idx = idx + 1
                            self.builder.position_at_end(inc_bb);
                            let idx_now = self
                                .builder
                                .build_load(self.i64_t, idx_alloca, "idx_now")
                                .expect("load idx failed")
                                .into_int_value();
                            let one64 = self.i64_t.const_int(1, false);
                            let added = self
                                .builder
                                .build_int_add(idx_now, one64, "idx_inc")
                                .expect("build_int_add failed");
                            let _ = self
                                .builder
                                .build_store(idx_alloca, added.as_basic_value_enum());
                            if self.builder.get_insert_block().is_some() {
                                let _ = self
                                    .builder
                                    .build_unconditional_branch(cond_bb)
                                    .expect("build_unconditional_branch failed");
                            }

                            // end: nothing to do
                            self.builder.position_at_end(end_bb);
                        }
                    }
                    ast::Stmt::While(ws) => {
                        let cond_bb = self.context.append_basic_block(function, "while.cond");
                        let body_bb = self.context.append_basic_block(function, "while.body");
                        let end_bb = self.context.append_basic_block(function, "while.end");

                        if self.builder.get_insert_block().is_some() {
                            let _ = self
                                .builder
                                .build_unconditional_branch(cond_bb)
                                .expect("build_unconditional_branch failed");
                        }

                        self.builder.position_at_end(cond_bb);
                        if let Some(tv) =
                            self.lower_expr(&ws.test, function, &param_map, &mut locals_stack)
                        {
                            if let Some(cond) = self.to_condition_i1(tv) {
                                self.builder
                                    .build_conditional_branch(cond, body_bb, end_bb)
                                    .expect("build_conditional_branch failed");
                            } else {
                                self.builder
                                    .build_unconditional_branch(end_bb)
                                    .expect("build_unconditional_branch failed");
                            }
                        } else {
                            self.builder
                                .build_unconditional_branch(end_bb)
                                .expect("build_unconditional_branch failed");
                        }

                        self.builder.position_at_end(body_bb);
                        locals_stack.push(HashMap::new());
                        let mut returned = false;
                        match &*ws.body {
                            ast::Stmt::Block(block) => {
                                for s in &block.stmts {
                                    match s {
                                        ast::Stmt::Return(ret) => {
                                            if let Some(arg) = &ret.arg {
                                                if let Some(val) = self.lower_expr(
                                                    arg,
                                                    function,
                                                    &param_map,
                                                    &mut locals_stack,
                                                ) {
                                                    self.emit_rc_dec_for_locals(&locals_stack);
                                                    let _ = self.builder.build_return(Some(&val));
                                                } else {
                                                    self.emit_rc_dec_for_locals(&locals_stack);
                                                    let _ = self.builder.build_return(None);
                                                }
                                            } else {
                                                let _ = self.builder.build_return(None);
                                            }
                                            returned = true;
                                            break;
                                        }
                                        ast::Stmt::Expr(expr_stmt) => {
                                            let _ = self.lower_expr(
                                                &expr_stmt.expr,
                                                function,
                                                &param_map,
                                                &mut locals_stack,
                                            );
                                        }
                                        _ => {}
                                    }
                                }
                            }
                            ast::Stmt::Return(ret) => {
                                if let Some(arg) = &ret.arg {
                                    if let Some(val) = self.lower_expr(
                                        arg,
                                        function,
                                        &param_map,
                                        &mut locals_stack,
                                    ) {
                                        self.emit_rc_dec_for_locals(&locals_stack);
                                        let _ = self.builder.build_return(Some(&val));
                                    } else {
                                        self.emit_rc_dec_for_locals(&locals_stack);
                                        let _ = self.builder.build_return(None);
                                    }
                                } else {
                                    let _ = self.builder.build_return(None);
                                }
                                returned = true;
                            }
                            ast::Stmt::Expr(expr_stmt) => {
                                let _ = self.lower_expr(
                                    &expr_stmt.expr,
                                    function,
                                    &param_map,
                                    &mut locals_stack,
                                );
                            }
                            _ => {}
                        }
                        if !returned && self.builder.get_insert_block().is_some() {
                            let _ = self
                                .builder
                                .build_unconditional_branch(cond_bb)
                                .expect("build_unconditional_branch failed");
                        }
                        locals_stack.pop();
                        self.builder.position_at_end(end_bb);
                    }
                    ast::Stmt::DoWhile(dws) => {
                        let body_bb = self.context.append_basic_block(function, "dowhile.body");
                        let cond_bb = self.context.append_basic_block(function, "dowhile.cond");
                        let end_bb = self.context.append_basic_block(function, "dowhile.end");

                        if self.builder.get_insert_block().is_some() {
                            let _ = self
                                .builder
                                .build_unconditional_branch(body_bb)
                                .expect("build_unconditional_branch failed");
                        }

                        self.builder.position_at_end(body_bb);
                        locals_stack.push(HashMap::new());
                        let mut returned = false;
                        match &*dws.body {
                            ast::Stmt::Block(block) => {
                                for s in &block.stmts {
                                    match s {
                                        ast::Stmt::Return(ret) => {
                                            if let Some(arg) = &ret.arg {
                                                if let Some(val) = self.lower_expr(
                                                    arg,
                                                    function,
                                                    &param_map,
                                                    &mut locals_stack,
                                                ) {
                                                    self.emit_rc_dec_for_locals(&locals_stack);
                                                    let _ = self.builder.build_return(Some(&val));
                                                } else {
                                                    self.emit_rc_dec_for_locals(&locals_stack);
                                                    let _ = self.builder.build_return(None);
                                                }
                                            } else {
                                                let _ = self.builder.build_return(None);
                                            }
                                            returned = true;
                                            break;
                                        }
                                        ast::Stmt::Expr(expr_stmt) => {
                                            let _ = self.lower_expr(
                                                &expr_stmt.expr,
                                                function,
                                                &param_map,
                                                &mut locals_stack,
                                            );
                                        }
                                        _ => {}
                                    }
                                }
                            }
                            ast::Stmt::Return(ret) => {
                                if let Some(arg) = &ret.arg {
                                    if let Some(val) = self.lower_expr(
                                        arg,
                                        function,
                                        &param_map,
                                        &mut locals_stack,
                                    ) {
                                        self.emit_rc_dec_for_locals(&locals_stack);
                                        let _ = self.builder.build_return(Some(&val));
                                    } else {
                                        self.emit_rc_dec_for_locals(&locals_stack);
                                        let _ = self.builder.build_return(None);
                                    }
                                } else {
                                    let _ = self.builder.build_return(None);
                                }
                                returned = true;
                            }
                            ast::Stmt::Expr(expr_stmt) => {
                                let _ = self.lower_expr(
                                    &expr_stmt.expr,
                                    function,
                                    &param_map,
                                    &mut locals_stack,
                                );
                            }
                            _ => {}
                        }
                        locals_stack.pop();
                        if !returned && self.builder.get_insert_block().is_some() {
                            let _ = self
                                .builder
                                .build_unconditional_branch(cond_bb)
                                .expect("build_unconditional_branch failed");
                        }

                        self.builder.position_at_end(cond_bb);
                        if let Some(tv) =
                            self.lower_expr(&dws.test, function, &param_map, &mut locals_stack)
                        {
                            if let Some(cond) = self.to_condition_i1(tv) {
                                self.builder
                                    .build_conditional_branch(cond, body_bb, end_bb)
                                    .expect("build_conditional_branch failed");
                            } else {
                                self.builder
                                    .build_unconditional_branch(end_bb)
                                    .expect("build_unconditional_branch failed");
                            }
                        } else {
                            self.builder
                                .build_unconditional_branch(end_bb)
                                .expect("build_unconditional_branch failed");
                        }

                        self.builder.position_at_end(end_bb);
                    }
                    _ => {
                        // other statement types not supported in prototype
                    }
                }
            }
            if !emitted_return {
                let _ = self.builder.build_return(None);
            }
        } else {
            let _ = self.builder.build_return(None);
        }

        function
    }

    /// Emit a small C-compatible `main` function in the module.
    ///
    /// The emitted host `main` and `oats_entry` do NOT call into user code.
    /// They simply return 0 when emitted. This ensures building a host
    /// executable will not execute the user's `oats_main` as a side effect.
    ///
    /// Returns true if a host `main` was emitted (the module defines
    /// `oats_main`), false otherwise.
    pub fn emit_host_main(
        &self,
        _param_types: &[crate::types::OatsType],
        _ret_type: &crate::types::OatsType,
    ) -> bool {
        // If main already present, nothing to do
        if self.module.get_function("main").is_some() {
            return true;
        }

        // only emit a host main if the module defines `oats_main`; we don't
        // call into it here, but absence indicates there's nothing to host.
        if self.module.get_function("oats_main").is_none() {
            return false;
        }

        // Build main: int main() which returns an exit code
        let i32t = self.i32_t;
        let main_ty = i32t.fn_type(&[], false);
        let main_fn = self.module.add_function("main", main_ty, None);
        let entry = self.context.append_basic_block(main_fn, "entry");
        self.builder.position_at_end(entry);

        // The host `main` will invoke the user's `oats_main` when the
        // emitted `oats_main` exists and has a zero-argument signature. This
        // keeps the produced binary self-contained (no external rt_main
        // object required) while ensuring we only call the user function
        // when its signature is compatible with the simple invocation.
        if let Some(user_main) = self.module.get_function("oats_main")
            && user_main.count_params() == 0
        {
            // Call oats_main and ignore its return value.
            let _ = self
                .builder
                .build_call(user_main, &[], "call_oats_main_from_main");
        }

        let zero = i32t.const_int(0, false);
        let _ = self.builder.build_return(Some(&zero.as_basic_value_enum()));

        // Emit oats_entry that mirrors main but returns i32 for hosts that
        // call that symbol from a separate object file.
        if self.module.get_function("oats_entry").is_none() {
            let entry_ty = i32t.fn_type(&[], false);
            let entry_fn = self.module.add_function("oats_entry", entry_ty, None);
            let entry_bb = self.context.append_basic_block(entry_fn, "entry");
            self.builder.position_at_end(entry_bb);

            // Emit a simple oats_entry that calls into the user's `oats_main`
            // if it exists. This makes a linked host executable execute the
            // user's program when run. We call `oats_main` with no
            // arguments here and ignore its return value; this is the
            // pragmatic behavior for the current prototype (examples use
            // `oats_main()` with no args). If `oats_main` has a different
            // signature the call will be skipped and oats_entry will return
            // 0 instead.
            if let Some(user_main) = self.module.get_function("oats_main") {
                // Only call if the function type matches a no-arg callable
                // (defensive: check param count is zero).
                if user_main.count_params() == 0 {
                    let _ = self.builder.build_call(user_main, &[], "call_oats_main");
                }
            }

            let zero = i32t.const_int(0, false);
            let _ = self.builder.build_return(Some(&zero.as_basic_value_enum()));
        }

        true
    }
}
