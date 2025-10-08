use deno_ast::swc::ast;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::{BasicType, BasicTypeEnum};
use inkwell::values::BasicValueEnum;
use inkwell::values::{FunctionValue, PointerValue};
use std::cell::{Cell, RefCell};
use std::collections::HashMap;

pub mod expr;
pub mod helpers;
pub mod stmt;

// Locals are represented as a tuple (ptr, ty, initialized, is_const) in many
// helper modules. Use the same alias here so different files agree on the
// in-memory representation of the locals stack.
type LocalEntry<'a> = (PointerValue<'a>, BasicTypeEnum<'a>, bool, bool);
type LocalsStackLocal<'a> = Vec<std::collections::HashMap<String, LocalEntry<'a>>>;

/// The main code generation structure, holding the LLVM context, module,
/// builder, and various caches for types and functions.
pub struct CodeGen<'a> {
    pub context: &'a Context,
    pub module: Module<'a>,
    pub builder: Builder<'a>,
    pub next_str_id: Cell<u32>,
    pub string_literals: RefCell<HashMap<String, PointerValue<'a>>>,
    pub f64_t: inkwell::types::FloatType<'a>,
    pub i64_t: inkwell::types::IntType<'a>,
    pub i32_t: inkwell::types::IntType<'a>,
    pub bool_t: inkwell::types::IntType<'a>,
    pub i8ptr_t: inkwell::types::PointerType<'a>,
    pub fn_print_f64: RefCell<Option<FunctionValue<'a>>>,
    pub fn_print_str: RefCell<Option<FunctionValue<'a>>>,
    pub fn_strlen: RefCell<Option<FunctionValue<'a>>>,
    pub fn_malloc: RefCell<Option<FunctionValue<'a>>>,
    pub fn_memcpy: RefCell<Option<FunctionValue<'a>>>,
    pub fn_free: RefCell<Option<FunctionValue<'a>>>,
    pub fn_array_alloc: RefCell<Option<FunctionValue<'a>>>,
    pub fn_rc_inc: RefCell<Option<FunctionValue<'a>>>,
    pub fn_rc_dec: RefCell<Option<FunctionValue<'a>>>,
    pub class_fields: RefCell<HashMap<String, Vec<(String, crate::types::OatsType)>>>,
    pub fn_param_types: RefCell<HashMap<String, Vec<crate::types::OatsType>>>,
    pub source: &'a str,
}

impl<'a> CodeGen<'a> {
    // --- Runtime Helper Function Getters ---

    fn get_array_alloc(&self) -> FunctionValue<'a> {
        if let Some(f) = *self.fn_array_alloc.borrow() {
            return f;
        }
        let fn_type = self.i8ptr_t.fn_type(
            &[self.i64_t.into(), self.i32_t.into(), self.i32_t.into()],
            false,
        );
        let f = self.module.add_function("array_alloc", fn_type, None);
        *self.fn_array_alloc.borrow_mut() = Some(f);
        f
    }

    pub fn get_rc_inc(&self) -> FunctionValue<'a> {
        if let Some(f) = *self.fn_rc_inc.borrow() {
            return f;
        }
        let fn_type = self
            .context
            .void_type()
            .fn_type(&[self.i8ptr_t.into()], false);
        let f = self.module.add_function("rc_inc", fn_type, None);
        *self.fn_rc_inc.borrow_mut() = Some(f);
        f
    }

    fn get_rc_dec(&self) -> FunctionValue<'a> {
        if let Some(f) = *self.fn_rc_dec.borrow() {
            return f;
        }
        let fn_type = self
            .context
            .void_type()
            .fn_type(&[self.i8ptr_t.into()], false);
        let f = self.module.add_function("rc_dec", fn_type, None);
        *self.fn_rc_dec.borrow_mut() = Some(f);
        f
    }

    fn get_array_get_f64(&self) -> FunctionValue<'a> {
        self.module
            .get_function("array_get_f64")
            .unwrap_or_else(|| {
                let fn_type = self
                    .f64_t
                    .fn_type(&[self.i8ptr_t.into(), self.i64_t.into()], false);
                self.module.add_function("array_get_f64", fn_type, None)
            })
    }

    fn get_array_get_ptr(&self) -> FunctionValue<'a> {
        self.module
            .get_function("array_get_ptr")
            .unwrap_or_else(|| {
                let fn_type = self
                    .i8ptr_t
                    .fn_type(&[self.i8ptr_t.into(), self.i64_t.into()], false);
                self.module.add_function("array_get_ptr", fn_type, None)
            })
    }

    fn get_array_set_ptr(&self) -> FunctionValue<'a> {
        self.module
            .get_function("array_set_ptr")
            .unwrap_or_else(|| {
                // array_set_ptr(arr: i8*, idx: i64, p: i8*) -> void
                let fn_type = self.context.void_type().fn_type(
                    &[self.i8ptr_t.into(), self.i64_t.into(), self.i8ptr_t.into()],
                    false,
                );
                self.module.add_function("array_set_ptr", fn_type, None)
            })
    }

    // --- Main Function Generation ---

    /// Generates LLVM IR for a function declaration.
    /// This is the main entry point for function compilation.
    pub fn gen_function_ir(
        &self,
        func_name: &str,
        func_decl: &ast::Function,
        param_types: &[crate::types::OatsType],
        ret_type: &crate::types::OatsType,
        receiver_name: Option<&str>,
    ) -> FunctionValue<'a> {
        // 1. Build the LLVM function type.
        let llvm_param_types: Vec<_> = param_types
            .iter()
            .map(|t| self.map_type_to_llvm(t))
            .collect();
        let fn_type = self.build_llvm_fn_type(&llvm_param_types, ret_type);

        // 2. Add the function to the module and set up the entry block.
        self.gen_str_concat(); // Ensure runtime helpers are available.
        let function = self.module.add_function(func_name, fn_type, None);
        let entry = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(entry);

        // 3. Store metadata and create stack allocations for parameters.
        self.fn_param_types
            .borrow_mut()
            .insert(func_name.to_string(), param_types.to_vec());
        let (param_map, mut locals_stack) =
            self.create_param_allocas(function, func_decl, &llvm_param_types, receiver_name);

        // 4. Lower the function body statements into IR.
        let mut emitted_terminator = false;
        if let Some(body) = &func_decl.body {
            emitted_terminator =
                self.lower_stmts(&body.stmts, function, &param_map, &mut locals_stack);
        }

        // 5. Add an implicit `return void` if the function hasn't already returned.
        if !emitted_terminator
            && self
                .builder
                .get_insert_block()
                .is_none_or(|b| b.get_terminator().is_none())
        {
            // Helpers live in helpers.rs; emit rc decs for locals before returning
            self.emit_rc_dec_for_locals(&locals_stack);
            self.builder
                .build_return(None)
                .expect("Failed to build implicit return");
        }

        function
    }

    // --- Function Generation Helpers ---

    /// Helper to build the LLVM function type from parameter and return types.
    fn build_llvm_fn_type(
        &self,
        llvm_param_types: &[BasicTypeEnum<'a>],
        ret_type: &crate::types::OatsType,
    ) -> inkwell::types::FunctionType<'a> {
        let args: Vec<_> = llvm_param_types.iter().map(|&t| t.into()).collect();
        match ret_type {
            crate::types::OatsType::Void => self.context.void_type().fn_type(&args, false),
            _ => self.map_type_to_llvm(ret_type).fn_type(&args, false),
        }
    }

    /// Creates stack allocations (`alloca`) for all function parameters, making them
    /// accessible like local variables and handling initial reference counting.
    fn create_param_allocas(
        &self,
        function: FunctionValue<'a>,
        func_decl: &ast::Function,
        llvm_param_types: &[BasicTypeEnum<'a>],
        receiver_name: Option<&str>,
    ) -> (HashMap<String, u32>, LocalsStackLocal<'a>) {
        let mut param_map = HashMap::new();
        if let Some(rname) = receiver_name {
            param_map.insert(rname.to_string(), 0u32);
        }
        for (i, p) in func_decl.params.iter().enumerate() {
            if let ast::Pat::Ident(ident) = &p.pat {
                let name = ident.id.sym.to_string();
                let idx = (i + receiver_name.map_or(0, |_| 1)) as u32;
                param_map.insert(name, idx);
            }
        }

        // Initialize an empty locals stack. Allocas for `let` and `const` will be added later.
        let locals_stack: LocalsStackLocal<'a> = vec![HashMap::new()];

        // We no longer create allocas for parameters. Still, increment RC for
        // pointer-typed parameters so ownership is consistent with previous behavior.
        for &idx in param_map.values() {
            if let Some(param_ty) = llvm_param_types.get(idx as usize)
                && param_ty.is_pointer_type()
                    && let Some(pv) = function.get_nth_param(idx) {
                        let rc_inc = self.get_rc_inc();
                        self.builder
                            .build_call(rc_inc, &[pv.into()], "rc_inc_param")
                            .unwrap();
                    }
        }
        (param_map, locals_stack)
    }

    /// Lowers a slice of AST statements into the current basic block.
    /// Returns `true` if a terminator instruction (e.g., return) was emitted.
    fn lower_stmts(
        &self,
        stmts: &[ast::Stmt],
        function: FunctionValue<'a>,
        param_map: &HashMap<String, u32>,
        locals_stack: &mut LocalsStackLocal<'a>,
    ) -> bool {
        for stmt in stmts {
            if self.lower_stmt(stmt, function, param_map, locals_stack) {
                return true; // Terminator found, stop processing.
            }
        }
        false // No terminator found.
    }

    // ... Additional helper functions (`lower_stmt`, `lower_if_stmt`, etc.) would go here ...
    // ... They are defined in the `stmt.rs` module as per the file structure. ...

    /// Minimal placeholder for `lower_stmt` so the top-level lowering entry
    /// point compiles while statement lowering lives in `stmt.rs`.
    fn lower_stmt(
        &self,
        _stmt: &ast::Stmt,
        _function: FunctionValue<'a>,
        _param_map: &HashMap<String, u32>,
        _locals_stack: &mut LocalsStackLocal<'a>,
    ) -> bool {
        // A small statement lowering implementation that covers the test
        // cases: variable declarations with initializers, expression
        // statements, return statements and blocks. This is intentionally
        // minimal: more statements can be added into `stmt.rs` later.
        match _stmt {
            ast::Stmt::Decl(d) => {
                if let ast::Decl::Var(var_decl) = d {
                    for decl in &var_decl.decls {
                        // Only handle simple identifier patterns for now
                        if let ast::Pat::Ident(ident) = &decl.name {
                            let name = ident.id.sym.to_string();

                            // If there is an initializer, lower it and allocate a
                            // matching alloca for its type.
                            if let Some(init) = &decl.init {
                                // `init` is an Option<Box<Expr>> (deno_ast wrapper); use `.as_ref()`
                                if let Ok(val) =
                                    self.lower_expr(init, _function, _param_map, _locals_stack)
                                {
                                    let ty = val.get_type().as_basic_type_enum();
                                    let alloca = self
                                        .builder
                                        .build_alloca(ty, &name)
                                        .expect("Failed to alloca var");
                                    // store lowered value
                                    let _ = self.builder.build_store(alloca, val);
                                    // If pointer type, increment RC for stored pointer
                                    if let inkwell::types::BasicTypeEnum::PointerType(_) = ty
                                        && let BasicValueEnum::PointerValue(pv) = val {
                                            let rc_inc = self.get_rc_inc();
                                            let _ = self.builder.build_call(
                                                rc_inc,
                                                &[pv.into()],
                                                "rc_inc_local",
                                            );
                                        }
                                    // mark initialized in locals; is_const=false for now
                                    self.insert_local_current_scope(
                                        _locals_stack,
                                        name,
                                        alloca,
                                        ty,
                                        true,
                                        false,
                                    );
                                }
                            } else {
                                // No initializer: create an uninitialized slot with i64 as default
                                let ty = self.i64_t.as_basic_type_enum();
                                let alloca = self
                                    .builder
                                    .build_alloca(ty, &name)
                                    .expect("Failed to alloca var");
                                self.insert_local_current_scope(
                                    _locals_stack,
                                    name,
                                    alloca,
                                    ty,
                                    false,
                                    false,
                                );
                            }
                        }
                    }
                }
                false
            }
            ast::Stmt::Expr(expr_stmt) => {
                // Evaluate expression for side-effects; ignore result
                let _ = self.lower_expr(&expr_stmt.expr, _function, _param_map, _locals_stack);
                false
            }
            ast::Stmt::Return(ret) => {
                // Lower return expression, emit rc_decs for locals then return
                if let Some(arg) = &ret.arg {
                    if let Ok(val) = self.lower_expr(arg, _function, _param_map, _locals_stack)
                    {
                        // emit rc decs for locals
                        self.emit_rc_dec_for_locals(_locals_stack);
                        // build return with the lowered value
                        let _ = self.builder.build_return(Some(&val));
                        return true;
                    }
                } else {
                    self.emit_rc_dec_for_locals(_locals_stack);
                    let _ = self.builder.build_return(None);
                    return true;
                }
                false
            }
            ast::Stmt::Block(block) => {
                // new scope
                _locals_stack.push(HashMap::new());
                let terminated =
                    self.lower_stmts(&block.stmts, _function, _param_map, _locals_stack);
                // pop scope
                _locals_stack.pop();
                terminated
            }
            // Minimal ForOf lowering: handle `for (let v of iterable) { body }`
            ast::Stmt::ForOf(forof) => {
                // Only handle left as a var decl: `for (let v of rhs)`
                // forof.left can be either a VarDecl or a Pat; we match on VarDecl
                if let ast::ForHead::VarDecl(var_decl) = &forof.left
                    && var_decl.decls.len() == 1 {
                        let decl = &var_decl.decls[0];
                        if let ast::Pat::Ident(ident) = &decl.name {
                            let loop_var_name = ident.id.sym.to_string();
                            // Lower RHS (iterable)
                            if let Ok(iter_val) =
                                self.lower_expr(&forof.right, _function, _param_map, _locals_stack)
                                && let BasicValueEnum::PointerValue(arr_ptr) = iter_val {
                                    // create index
                                    let idx_alloca = self
                                        .builder
                                        .build_alloca(self.i64_t, "for_idx")
                                        .expect("alloca idx");
                                    let zero = self.i64_t.const_int(0, false);
                                    let _ = self.builder.build_store(idx_alloca, zero);

                                    // create blocks
                                    let loop_cond_bb =
                                        self.context.append_basic_block(_function, "for.cond");
                                    let loop_body_bb =
                                        self.context.append_basic_block(_function, "for.body");
                                    let loop_after_bb =
                                        self.context.append_basic_block(_function, "for.after");

                                    let _ = self.builder.build_unconditional_branch(loop_cond_bb);

                                    self.builder.position_at_end(loop_cond_bb);
                                    // call strlen to get length
                                    if let Some(strlen_fn) = self.module.get_function("strlen") {
                                        let cs = match self.builder.build_call(
                                            strlen_fn,
                                            &[arr_ptr.into()],
                                            "strlen_call",
                                        ) {
                                            Ok(cs) => cs,
                                            Err(_) => return false,
                                        };
                                        if let inkwell::Either::Left(bv) = cs.try_as_basic_value() {
                                            let len = bv.into_int_value();
                                            let cur_idx = match self
                                                .builder
                                                .build_load(self.i64_t, idx_alloca, "idx_load")
                                            {
                                                Ok(v) => v.into_int_value(),
                                                Err(_) => return false,
                                            };
                                            let cmp = match self.builder.build_int_compare(
                                                inkwell::IntPredicate::ULT,
                                                cur_idx,
                                                len,
                                                "cmp_idx",
                                            ) {
                                                Ok(v) => v,
                                                Err(_) => return false,
                                            };
                                            if self.builder.build_conditional_branch(
                                                cmp,
                                                loop_body_bb,
                                                loop_after_bb,
                                            ).is_err() {
                                                return false;
                                            }
                                        } else if self
                                            .builder
                                            .build_unconditional_branch(loop_after_bb).is_err()
                                        {
                                            return false;
                                        }
                                    } else if self.builder.build_unconditional_branch(loop_after_bb).is_err()
                                    {
                                        return false;
                                    }

                                    // body
                                    self.builder.position_at_end(loop_body_bb);
                                    let cur_idx = match self.builder.build_load(
                                        self.i64_t,
                                        idx_alloca,
                                        "idx_load2",
                                    ) {
                                        Ok(v) => v.into_int_value(),
                                        Err(_) => return false,
                                    };
                                    if let Some(array_get_f64_fn) =
                                        self.module.get_function("array_get_f64")
                                    {
                                        let cs = match self.builder.build_call(
                                            array_get_f64_fn,
                                            &[arr_ptr.into(), cur_idx.into()],
                                            "array_get_f64_call",
                                        ) {
                                            Ok(cs) => cs,
                                            Err(_) => return false,
                                        };
                                        if let inkwell::Either::Left(bv) = cs.try_as_basic_value() {
                                            let ty = bv.get_type().as_basic_type_enum();
                                            let alloca = self
                                                .builder
                                                .build_alloca(ty, &loop_var_name)
                                                .expect("alloca loop var");
                                            let _ = self.builder.build_store(alloca, bv);
                                            self.insert_local_current_scope(
                                                _locals_stack,
                                                loop_var_name.clone(),
                                                alloca,
                                                ty,
                                                true,
                                                false,
                                            );
                                        }
                                    } else if let Some(array_get_ptr_fn) =
                                        self.module.get_function("array_get_ptr")
                                    {
                                        let cs = match self.builder.build_call(
                                            array_get_ptr_fn,
                                            &[arr_ptr.into(), cur_idx.into()],
                                            "array_get_ptr_call",
                                        ) {
                                            Ok(cs) => cs,
                                            Err(_) => return false,
                                        };
                                        if let inkwell::Either::Left(bv) = cs.try_as_basic_value() {
                                            let pv = bv.into_pointer_value();
                                            let ty = pv.get_type().as_basic_type_enum();
                                            let alloca = self
                                                .builder
                                                .build_alloca(ty, &loop_var_name)
                                                .expect("alloca loop var ptr");
                                            let _ = self.builder.build_store(alloca, bv);
                                            let rc_inc = self.get_rc_inc();
                                            let _ = self.builder.build_call(
                                                rc_inc,
                                                &[pv.into()],
                                                "rc_inc_loop_var",
                                            );
                                            self.insert_local_current_scope(
                                                _locals_stack,
                                                loop_var_name.clone(),
                                                alloca,
                                                ty,
                                                true,
                                                false,
                                            );
                                        }
                                    }

                                    // Lower the loop body: handle a Block or single statement
                                    let terminated = match &*forof.body {
                                        ast::Stmt::Block(block) => self.lower_stmts(
                                            &block.stmts,
                                            _function,
                                            _param_map,
                                            _locals_stack,
                                        ),
                                        other => self.lower_stmt(
                                            other,
                                            _function,
                                            _param_map,
                                            _locals_stack,
                                        ),
                                    };
                                    let cur_idx2 = match self.builder.build_load(
                                        self.i64_t,
                                        idx_alloca,
                                        "idx_load3",
                                    ) {
                                        Ok(v) => v.into_int_value(),
                                        Err(_) => return false,
                                    };
                                    let one = self.i64_t.const_int(1, false);
                                    let next_idx =
                                        match self.builder.build_int_add(cur_idx2, one, "idx_next")
                                        {
                                            Ok(v) => v,
                                            Err(_) => return false,
                                        };
                                    let _ = self.builder.build_store(idx_alloca, next_idx);
                                    if !terminated
                                        && self.builder.build_unconditional_branch(loop_cond_bb).is_err()
                                        {
                                            return false;
                                        }
                                    self.builder.position_at_end(loop_after_bb);
                                    return terminated;
                                }
                        }
                    }
                false
            }
            _ => false,
        }
    }

    // --- Host Main Generation ---

    pub fn emit_host_main(
        &self,
        _params: &[crate::types::OatsType],
        _ret: &crate::types::OatsType,
    ) -> bool {
        // Emit a simple C-compatible `main` function that calls the
        // generated `oats_main` symbol. This covers the common case where
        // user scripts export `function main(): number` with no params.
        // Return `true` to indicate we emitted a host main so the driver
        // will skip linking an external `rt_main.o`.

        // Only handle the simple case: no params, integer return or void.
        // Build: int main(int argc, char** argv) { int r = oats_main(); return r; }
        let i32_t = self.i32_t;
        let i8ptr_t = self.i8ptr_t;

        // Build function type: i32 (int) with (i32, i8**) params
        let fn_type = i32_t.fn_type(&[i32_t.into(), i8ptr_t.as_basic_type_enum().into()], false);
        let main_fn = self.module.add_function("main", fn_type, None);
        let entry = self.context.append_basic_block(main_fn, "entry");
        self.builder.position_at_end(entry);

        // Try to look up oats_main; it should be present if gen_function_ir emitted it.
        let oats_main_fn = match self.module.get_function("oats_main") {
            Some(f) => f,
            None => {
                // No oats_main available; emit an empty main that returns 1
                let const_one = i32_t.const_int(1, false);
                let _ = self.builder.build_return(Some(&const_one));
                return true;
            }
        };

        // Call oats_main(); we only support a no-arg oats_main here.
        let call_site = self
            .builder
            .build_call(oats_main_fn, &[], "call_oats_main")
            .expect("build_call failed");
        // Interpret the result depending on its type. If the function returns an i64 or f64
        // we coerce/truncate to i32; if void, return 0.
        let either = call_site.try_as_basic_value();
        if let inkwell::Either::Left(bv) = either {
            let ret_val = if bv.get_type().is_int_type() {
                // Truncate or bitcast to i32 if needed
                let rv_int = bv.into_int_value();
                let cast = self
                    .builder
                    .build_int_truncate_or_bit_cast(rv_int, i32_t, "ret_i32")
                    .expect("int cast failed");
                inkwell::values::BasicValueEnum::IntValue(cast)
            } else if bv.get_type().is_float_type() {
                // cast float to i32 via fptosi
                let fv = bv.into_float_value();
                let conv = self
                    .builder
                    .build_float_to_signed_int(fv, i32_t, "f_to_i")
                    .expect("float->int failed");
                inkwell::values::BasicValueEnum::IntValue(conv)
            } else if bv.get_type().is_pointer_type() {
                // pointer return -> return 0
                inkwell::values::BasicValueEnum::IntValue(i32_t.const_int(0, false))
            } else {
                inkwell::values::BasicValueEnum::IntValue(i32_t.const_int(0, false))
            };
            let _ = self.builder.build_return(Some(&ret_val));
        } else {
            // No basic return (void), return 0
            let const_zero = i32_t.const_int(0, false);
            let _ = self.builder.build_return(Some(&const_zero));
        }

        true
    }
}
