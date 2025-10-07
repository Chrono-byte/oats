use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::BasicType;
use inkwell::types::BasicTypeEnum;
use inkwell::values::{BasicValue, BasicValueEnum, CallSiteValue, FunctionValue, PointerValue};
use inkwell::AddressSpace;
use std::cell::{Cell, RefCell};
use std::collections::HashMap;
// ...existing code...

pub mod helpers;

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
}

impl<'a> CodeGen<'a> {

    fn lower_expr(
        &self,
        expr: &deno_ast::swc::ast::Expr,
        function: FunctionValue<'a>,
        param_map: &HashMap<String, u32>,
        locals: &mut Vec<HashMap<String, (inkwell::values::PointerValue<'a>, BasicTypeEnum<'a>, bool, bool)>>,
    ) -> Option<BasicValueEnum<'a>> {
        use deno_ast::swc::ast;

        match expr {
            ast::Expr::Bin(bin) => {
                use deno_ast::swc::ast::BinaryOp;
                use inkwell::FloatPredicate;

                let l = self.lower_expr(&bin.left, function, param_map, locals)?;
                let r = self.lower_expr(&bin.right, function, param_map, locals)?;

                // coercion now handled by self.coerce_to_f64

                // Helper to handle float arithmetic
                let float_bin = |builder: &Builder<'a>, lf: inkwell::values::FloatValue<'a>, rf: inkwell::values::FloatValue<'a>, op: &BinaryOp| -> Option<BasicValueEnum<'a>> {
                    match op {
                        BinaryOp::Add => {
                            let v = builder.build_float_add(lf, rf, "sum").expect("build_float_add failed");
                            Some(v.as_basic_value_enum())
                        }
                        BinaryOp::Sub => {
                            let v = builder.build_float_sub(lf, rf, "sub").expect("build_float_sub failed");
                            Some(v.as_basic_value_enum())
                        }
                        BinaryOp::Mul => {
                            let v = builder.build_float_mul(lf, rf, "mul").expect("build_float_mul failed");
                            Some(v.as_basic_value_enum())
                        }
                        BinaryOp::Div => {
                            let v = builder.build_float_div(lf, rf, "div").expect("build_float_div failed");
                            Some(v.as_basic_value_enum())
                        }
                        _ => None,
                    }
                };

                // Helper to handle float comparisons -> i1
                let float_cmp = |builder: &Builder<'a>, lf: inkwell::values::FloatValue<'a>, rf: inkwell::values::FloatValue<'a>, op: &BinaryOp| -> Option<BasicValueEnum<'a>> {
                    let pred = match op {
                        BinaryOp::Lt => FloatPredicate::OLT,
                        BinaryOp::LtEq => FloatPredicate::OLE,
                        BinaryOp::Gt => FloatPredicate::OGT,
                        BinaryOp::GtEq => FloatPredicate::OGE,
                        BinaryOp::EqEq => FloatPredicate::OEQ,
                        BinaryOp::NotEq => FloatPredicate::ONE,
                        _ => return None,
                    };
                    let iv = builder.build_float_compare(pred, lf, rf, "cmp").expect("build_float_compare failed");
                    Some(iv.as_basic_value_enum())
                };

                // Try float path by coercing ints/bools to float as needed
                if let (Some(lf), Some(rf)) = (self.coerce_to_f64(l), self.coerce_to_f64(r)) {
                    if let Some(v) = float_bin(&self.builder, lf, rf, &bin.op) {
                        return Some(v);
                    }
                    if let Some(v) = float_cmp(&self.builder, lf, rf, &bin.op) {
                        return Some(v);
                    }
                }

                // Logical operators: short-circuiting
                if let deno_ast::swc::ast::BinaryOp::LogicalAnd = bin.op {
                    // a && b -> if a truthy then b else a
                    let left_val = l;
                    let cond = self.to_condition_i1(left_val.clone())?;
                    let then_bb = self.context.append_basic_block(function, "and.then");
                    let else_bb = self.context.append_basic_block(function, "and.else");
                    let merge_bb = self.context.append_basic_block(function, "and.merge");
                    self.builder.build_conditional_branch(cond, then_bb, else_bb).expect("build_conditional_branch failed");
                    // then: evaluate right
                    self.builder.position_at_end(then_bb);
                    let rv = self.lower_expr(&bin.right, function, param_map, locals);
                    if self.builder.get_insert_block().is_some() {
                        let _ = self.builder.build_unconditional_branch(merge_bb).expect("build_unconditional_branch failed");
                    }
                    // else: keep left
                    self.builder.position_at_end(else_bb);
                    if self.builder.get_insert_block().is_some() {
                        let _ = self.builder.build_unconditional_branch(merge_bb).expect("build_unconditional_branch failed");
                    }
                    self.builder.position_at_end(merge_bb);
                    if let Some(rval) = rv {
                        if let Some(phi) = self.build_phi_merge(then_bb, else_bb, rval, left_val) {
                            return Some(phi);
                        }
                    }
                    return None;
                }
                if let deno_ast::swc::ast::BinaryOp::LogicalOr = bin.op {
                    // a || b -> if a truthy then a else b
                    let left_val = l;
                    let cond = self.to_condition_i1(left_val.clone())?;
                    let then_bb = self.context.append_basic_block(function, "or.then");
                    let else_bb = self.context.append_basic_block(function, "or.else");
                    let merge_bb = self.context.append_basic_block(function, "or.merge");
                    self.builder.build_conditional_branch(cond, then_bb, else_bb).expect("build_conditional_branch failed");
                    // then: keep left
                    self.builder.position_at_end(then_bb);
                    if self.builder.get_insert_block().is_some() {
                        let _ = self.builder.build_unconditional_branch(merge_bb).expect("build_unconditional_branch failed");
                    }
                    // else: evaluate right
                    self.builder.position_at_end(else_bb);
                    let rv = self.lower_expr(&bin.right, function, param_map, locals);
                    if self.builder.get_insert_block().is_some() {
                        let _ = self.builder.build_unconditional_branch(merge_bb).expect("build_unconditional_branch failed");
                    }
                    self.builder.position_at_end(merge_bb);
                    if let Some(rval) = rv {
                        if let Some(phi) = self.build_phi_merge(then_bb, else_bb, left_val, rval) {
                            return Some(phi);
                        }
                    }
                    return None;
                }

                // Otherwise, handle pointer concat for Add and pointer equality
                match (l, r) {
                    (BasicValueEnum::PointerValue(lp), BasicValueEnum::PointerValue(rp)) => {
                        // Keep string concat behavior for Add
                        if let BinaryOp::Add = bin.op {
                            if let Some(strcat) = self.module.get_function("str_concat") {
                                let call_site = self.builder.build_call(
                                    strcat,
                                    &[lp.into(), rp.into()],
                                    "concat",
                                );
                                let call_site: CallSiteValue = call_site.expect("build_call failed");
                                let either = call_site.try_as_basic_value();
                                match either {
                                    inkwell::Either::Left(bv) => Some(bv),
                                    _ => None,
                                }
                            } else {
                                None
                            }
                        } else {
                            None
                        }
                    }
                    _ => None,
                }
            }
            ast::Expr::Ident(id) => {
                let name = id.sym.to_string();
                // First check locals (alloca slots) by searching scope stack
                if let Some((ptr, ty, initialized, _is_const)) = self.find_local(locals, &name) {
                    // If not initialized -> TDZ: generate a trap (unreachable)
                    if !initialized {
                        // Emit a call to unreachable to trap at runtime
                        let _ = self.builder.build_unreachable();
                        return None;
                    }
                    // build_load signature depends on LLVM version; for newer
                    // inkwell it expects (pointee_ty, ptr, name).
                    let loaded = self.builder.build_load(ty, ptr, &name).expect("build_load failed");
                    return Some(loaded);
                }
                // Fallback to function parameter (should be rare since params are
                // typically allocated into locals by gen_function_ir)
                if let Some(idx) = param_map.get(&name) {
                    return Some(function.get_nth_param(*idx).unwrap().into());
                }
                None
            }
            ast::Expr::Call(call) => {
                // Support simple identifier callees (calls to nested or module-level functions)
                if let ast::Callee::Expr(boxed_expr) = &call.callee {
                    if let ast::Expr::Ident(ident) = &**boxed_expr {
                        let fname = ident.sym.to_string();

                        // Special-case: println(x) -> call runtime print helpers
                        if fname == "println" {
                            // expect exactly one arg
                            if call.args.len() != 1 {
                                return None;
                            }
                            let a = &call.args[0];
                            if let Some(val) = self.lower_expr(&a.expr, function, param_map, locals) {
                                match val {
                                    BasicValueEnum::FloatValue(fv) => {
                                        let print_fn = self.module.get_function("print_f64").unwrap();
                                        let _ = self.builder.build_call(print_fn, &[fv.into()], "print_f64_call").expect("build_call failed");
                                        return None;
                                    }
                                    BasicValueEnum::PointerValue(pv) => {
                                        let print_fn = self.module.get_function("print_str").unwrap();
                                        let _ = self.builder.build_call(print_fn, &[pv.into()], "print_str_call").expect("build_call failed");
                                        return None;
                                    }
                                    _ => return None,
                                }
                            } else {
                                return None;
                            }
                        }

                        if let Some(fv) = self.module.get_function(&fname) {
                            // Lower args
                            let mut lowered_args: Vec<inkwell::values::BasicMetadataValueEnum> = Vec::new();
                            for a in &call.args {
                                if let Some(val) = self.lower_expr(&a.expr, function, param_map, locals) {
                                    lowered_args.push(val.into());
                                } else {
                                    // unsupported arg lowering
                                    return None;
                                }
                            }
                            let cs = self.builder.build_call(fv, &lowered_args, "call_internal").expect("build_call failed");
                            let either = cs.try_as_basic_value();
                            match either {
                                inkwell::Either::Left(bv) => Some(bv),
                                _ => None,
                            }
                        } else {
                            None
                        }
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            ast::Expr::Assign(assign) => {
                // support simple assignments `ident = expr` where the left side is an identifier
                if let Some(bid) = assign.left.as_ident() {
                    let name = bid.id.sym.to_string();
                        if let Some((ptr, _ty, _init, is_const)) = self.find_local(locals, &name) {
                            // Disallow assigning to const locals
                            if is_const {
                                // For now, emit unreachable to trap at runtime; in a future
                                // stage we could produce a compile-time diagnostic instead.
                                let _ = self.builder.build_unreachable();
                                return None;
                            }
                            if let Some(val) = self.lower_expr(&assign.right, function, param_map, locals) {
                                let _ = self.builder.build_store(ptr, val);
                                // mark initialized after assignment
                                self.set_local_initialized(locals, &name, true);
                                return Some(val);
                            }
                        }
                }
                None
            }
            ast::Expr::Cond(cond) => {
                // Ternary expression: test ? cons : alt
                // Lower test to an i1
                let test_val = self.lower_expr(&cond.test, function, param_map, locals)?;
                let cond_i1 = match test_val {
                    BasicValueEnum::IntValue(iv) => iv.as_basic_value_enum(),
                    BasicValueEnum::FloatValue(fv) => {
                        // JS-like truthiness for numbers: false for +0, -0, and NaN
                        let zero = self.context.f64_type().const_float(0.0);
                        let is_not_zero = self.builder.build_float_compare(inkwell::FloatPredicate::ONE, fv, zero, "neq0").expect("build_float_compare failed");
                        // check not NaN: fv == fv
                        let is_not_nan = self.builder.build_float_compare(inkwell::FloatPredicate::OEQ, fv, fv, "not_nan").expect("build_float_compare failed");
                        let cond = self.builder.build_and(is_not_zero, is_not_nan, "num_truth").expect("build_and failed");
                        cond.as_basic_value_enum()
                    }
                    BasicValueEnum::PointerValue(pv) => {
                        // pointer truthiness: non-null and non-empty string are truthy
                        let is_null = self.builder.build_is_null(pv, "is_null").expect("build_is_null failed");
                        let is_not_null = self.builder.build_not(is_null, "not_null").expect("build_not failed");
                        // call strlen(ptr) and check != 0
                        if let Some(strlen_fn) = self.module.get_function("strlen") {
                            let cs = self.builder.build_call(strlen_fn, &[pv.into()], "strlen_call").expect("build_call strlen failed");
                            let either = cs.try_as_basic_value();
                            if let inkwell::Either::Left(bv) = either {
                                let len = bv.into_int_value();
                                let zero64 = self.context.i64_type().const_int(0, false);
                                let len_nonzero = self.builder.build_int_compare(inkwell::IntPredicate::NE, len, zero64, "len_nonzero").expect("build_int_compare failed");
                                let cond = self.builder.build_and(is_not_null, len_nonzero, "ptr_truth").expect("build_and failed");
                                return Some(cond.as_basic_value_enum());
                            }
                        }
                        // fallback: non-null
                        is_not_null.as_basic_value_enum()
                    }
                    _ => return None,
                };

                // Create basic blocks
                let then_bb = self.context.append_basic_block(function, "then");
                let else_bb = self.context.append_basic_block(function, "else");
                let merge_bb = self.context.append_basic_block(function, "merge");

                // branch based on cond_i1
                let cond_val = self.to_condition_i1(cond_i1)?;
                self.builder.build_conditional_branch(cond_val, then_bb, else_bb).expect("build_conditional_branch failed");

                // then
                self.builder.position_at_end(then_bb);
                let then_val = self.lower_expr(&cond.cons, function, param_map, locals);
                if self.builder.get_insert_block().is_some() {
                    let _ = self.builder.build_unconditional_branch(merge_bb).expect("build_unconditional_branch failed");
                }

                // else
                self.builder.position_at_end(else_bb);
                let else_val = self.lower_expr(&cond.alt, function, param_map, locals);
                if self.builder.get_insert_block().is_some() {
                    let _ = self.builder.build_unconditional_branch(merge_bb).expect("build_unconditional_branch failed");
                }

                // merge
                self.builder.position_at_end(merge_bb);
                // If both sides produced values, create a phi via helper
                if let (Some(tv), Some(ev)) = (then_val, else_val) {
                    if let Some(phi) = self.build_phi_merge(then_bb, else_bb, tv, ev) {
                        return Some(phi);
                    }
                }

                None
            }
            ast::Expr::Lit(lit) => {
                use deno_ast::swc::ast::Lit;
                match &*lit {
                    Lit::Num(n) => {
                        let fv = self.context.f64_type().const_float(n.value);
                        Some(fv.as_basic_value_enum())
                    }
                    Lit::Bool(b) => {
                        let iv = self.context.bool_type().const_int(if b.value { 1 } else { 0 }, false);
                        Some(iv.as_basic_value_enum())
                    }
                    Lit::Str(s) => {
                        let bytes = s.value.as_bytes();
                        let key = String::from_utf8_lossy(bytes).into_owned();
                        // Check cache first (cache stores the computed pointer)
                        if let Some(ptr_val) = self.string_literals.borrow().get(&key) {
                            return Some(ptr_val.as_basic_value_enum());
                        }

                        let array_ty = self.context.i8_type().array_type((bytes.len() + 1) as u32);
                        // generate a unique global name for this string literal
                        let id = self.next_str_id.get();
                        let name = format!("strlit.{}", id);
                        self.next_str_id.set(id.wrapping_add(1));
                        let gv = self.module.add_global(array_ty, None, &name);
                        let const_array = self.context.const_string(bytes, true);
                        gv.set_initializer(&const_array);

                        let zero = self.context.i32_type().const_int(0, false);
                        let indices = &[zero, zero];
                        let gep = unsafe { self.builder.build_gep(array_ty, gv.as_pointer_value(), indices, "strptr") };
                        if let Ok(ptr) = gep {
                            // store pointer in cache for future reuse
                            self.string_literals.borrow_mut().insert(key, ptr);
                            return Some(ptr.as_basic_value_enum());
                        }
                        None
                    }
                    _ => None,
                }
            }
            _ => None,
        }
    }

    pub fn gen_function_ir(
        &self,
        func_name: &str,
        func_decl: &deno_ast::swc::ast::Function,
        param_types: &[crate::types::OatsType],
        ret_type: &crate::types::OatsType,
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
                let it = self.context.bool_type();
                let args: Vec<inkwell::types::BasicTypeEnum> = llvm_param_types.clone();
                it.fn_type(&args.iter().map(|a| (*a).into()).collect::<Vec<_>>(), false)
            }
            crate::types::OatsType::String | crate::types::OatsType::NominalStruct(_) => {
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

        let entry = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(entry);

        // Build param name -> index map from function declaration params
        let mut param_map: HashMap<String, u32> = HashMap::new();
        for (i, p) in func_decl.params.iter().enumerate() {
            use deno_ast::swc::ast::Pat;
            if let Pat::Ident(ident) = &p.pat {
                let name = ident.id.sym.to_string();
                param_map.insert(name, i as u32);
            }
        }

        // Allocate stack slots for function parameters and store incoming
        // parameter values into them so parameters behave like locals.
        // We create these allocas in the entry block before emitting body.
        let mut locals_stack: Vec<HashMap<String, (inkwell::values::PointerValue<'a>, BasicTypeEnum<'a>, bool, bool)>> = Vec::new();
        locals_stack.push(HashMap::new());
        for (name, idx) in &param_map {
            let i = *idx as usize;
            if i >= llvm_param_types.len() {
                continue;
            }
            let param_ty = llvm_param_types[i];
            // Create an alloca for the parameter's LLVM type
            let alloca = self.builder.build_alloca(param_ty, name).expect("build_alloca failed");
            // Store the incoming parameter into the alloca
            let pv = function.get_nth_param(*idx).unwrap();
            let _ = self.builder.build_store(alloca, pv);
            // Insert into current scope: initialized=true, is_const=false
            self.insert_local_current_scope(&mut locals_stack, name.clone(), alloca, param_ty, true, false);
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
                            if let Some(val) = self.lower_expr(arg, function, &param_map, &mut locals_stack) {
                                let _ = self.builder.build_return(Some(&val));
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
                        // Lower expression for side-effects (e.g., println calls)
                        let _ = self.lower_expr(&expr_stmt.expr, function, &param_map, &mut locals_stack);
                    }
                    ast::Stmt::Decl(ast::Decl::Var(vdecl)) => {
                        // Evaluate simple variable initializers and create allocas
                        use deno_ast::swc::ast::Pat;
                        // Decide const-ness for the whole declaration
                        let is_const_decl = matches!(vdecl.kind, deno_ast::swc::ast::VarDeclKind::Const);
                        for decl in &vdecl.decls {
                            if let Pat::Ident(ident) = &decl.name {
                                let name = ident.id.sym.to_string();
                                // Create an alloca for this local at function entry
                                // and insert it into the current scope as uninitialized
                                if let Some(init) = &decl.init {
                                    if let Some(val) = self.lower_expr(&init, function, &param_map, &mut locals_stack) {
                                        match val {
                                            BasicValueEnum::FloatValue(fv) => {
                                                let alloca = self.builder.build_alloca(self.context.f64_type(), &name).expect("build_alloca failed");
                                                // insert as uninitialized first to model TDZ
                                                self.insert_local_current_scope(&mut locals_stack, name.clone(), alloca, self.context.f64_type().as_basic_type_enum(), false, is_const_decl);
                                                let _ = self.builder.build_store(alloca, fv);
                                                // mark initialized after storing initializer
                                                self.set_local_initialized(&mut locals_stack, &name, true);
                                            }
                                            BasicValueEnum::PointerValue(pv) => {
                                                let ptr_ty = self.context.ptr_type(AddressSpace::default());
                                                let alloca = self.builder.build_alloca(ptr_ty, &name).expect("build_alloca failed");
                                                self.insert_local_current_scope(&mut locals_stack, name.clone(), alloca, ptr_ty.as_basic_type_enum(), false, is_const_decl);
                                                let _ = self.builder.build_store(alloca, pv);
                                                self.set_local_initialized(&mut locals_stack, &name, true);
                                            }
                                            BasicValueEnum::IntValue(iv) => {
                                                let boolt = self.context.bool_type();
                                                let alloca = self.builder.build_alloca(boolt, &name).expect("build_alloca failed");
                                                self.insert_local_current_scope(&mut locals_stack, name.clone(), alloca, boolt.as_basic_type_enum(), false, is_const_decl);
                                                let _ = self.builder.build_store(alloca, iv);
                                                self.set_local_initialized(&mut locals_stack, &name, true);
                                            }
                                            _ => {
                                                // Unsupported initializer type; still insert uninitialized local
                                                // so future references can produce TDZ errors.
                                                let ptr_ty = self.context.ptr_type(AddressSpace::default());
                                                let alloca = self.builder.build_alloca(ptr_ty, &name).expect("build_alloca failed");
                                                self.insert_local_current_scope(&mut locals_stack, name.clone(), alloca, ptr_ty.as_basic_type_enum(), false, is_const_decl);
                                            }
                                        }
                                    }
                                } else {
                                    // No initializer: for const this is a syntax error in JS/TS
                                    // For now, insert uninitialized const/local and let later checks catch usage.
                                    let ptr_ty = self.context.ptr_type(AddressSpace::default());
                                    let alloca = self.builder.build_alloca(ptr_ty, &name).expect("build_alloca failed");
                                    self.insert_local_current_scope(&mut locals_stack, name.clone(), alloca, ptr_ty.as_basic_type_enum(), false, is_const_decl);
                                }
                            }
                        }
                    }
                    ast::Stmt::If(if_stmt) => {
                        // Lower an if statement: create then/else/merge blocks and
                        // lower the consequent/alternative as statements.
                        // Lower the test to i1
                        if let Some(test_val) = self.lower_expr(&if_stmt.test, function, &param_map, &mut locals_stack) {
                            let cond_i1 = match test_val {
                                BasicValueEnum::IntValue(iv) => iv,
                                BasicValueEnum::FloatValue(fv) => {
                                    let zero = self.context.f64_type().const_float(0.0);
                                    let cmp = self.builder.build_float_compare(inkwell::FloatPredicate::ONE, fv, zero, "if_tcmp").expect("build_float_compare failed");
                                    cmp
                                }
                                _ => {
                                    // unsupported test type -> skip lowering
                                    continue;
                                }
                            };

                            let then_bb = self.context.append_basic_block(function, "if.then");
                            let else_bb = self.context.append_basic_block(function, "if.else");
                            let merge_bb = self.context.append_basic_block(function, "if.merge");

                            self.builder.build_conditional_branch(cond_i1, then_bb, else_bb).expect("build_conditional_branch failed");

                            // then
                            self.builder.position_at_end(then_bb);
                            match &*if_stmt.cons {
                                ast::Stmt::Block(block) => {
                                    for s in &block.stmts {
                                        match s {
                                            ast::Stmt::Return(ret) => {
                                                if let Some(arg) = &ret.arg {
                                                    if let Some(val) = self.lower_expr(arg, function, &param_map, &mut locals_stack) {
                                                        let _ = self.builder.build_return(Some(&val));
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
                                                let _ = self.lower_expr(&expr_stmt.expr, function, &param_map, &mut locals_stack);
                                            }
                                            ast::Stmt::Decl(ast::Decl::Var(vdecl)) => {
                                                use deno_ast::swc::ast::Pat;
                                                for decl in &vdecl.decls {
                                                    if let Pat::Ident(ident) = &decl.name {
                                                        let name = ident.id.sym.to_string();
                                                        if let Some(init) = &decl.init {
                                                            if let Some(val) = self.lower_expr(&init, function, &param_map, &mut locals_stack) {
                                                                match val {
                                                                    BasicValueEnum::FloatValue(fv) => {
                                                                        let alloca = self.builder.build_alloca(self.context.f64_type(), &name).expect("build_alloca failed");
                                                                        let _ = self.builder.build_store(alloca, fv);
                                                                        self.insert_local_current_scope(&mut locals_stack, name.clone(), alloca, self.context.f64_type().as_basic_type_enum(), true, false);
                                                                    }
                                                                    BasicValueEnum::PointerValue(pv) => {
                                                                        let ptr_ty = self.context.ptr_type(AddressSpace::default());
                                                                        let alloca = self.builder.build_alloca(ptr_ty, &name).expect("build_alloca failed");
                                                                        let _ = self.builder.build_store(alloca, pv);
                                                                        self.insert_local_current_scope(&mut locals_stack, name.clone(), alloca, ptr_ty.as_basic_type_enum(), true, false);
                                                                    }
                                                                    BasicValueEnum::IntValue(iv) => {
                                                                        let boolt = self.context.bool_type();
                                                                        let alloca = self.builder.build_alloca(boolt, &name).expect("build_alloca failed");
                                                                        let _ = self.builder.build_store(alloca, iv);
                                                                        self.insert_local_current_scope(&mut locals_stack, name.clone(), alloca, boolt.as_basic_type_enum(), true, false);
                                                                    }
                                                                    _ => {}
                                                                }
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
                                        if let Some(val) = self.lower_expr(arg, function, &param_map, &mut locals_stack) {
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
                                    let _ = self.lower_expr(&expr_stmt.expr, function, &param_map, &mut locals_stack);
                                }
                                ast::Stmt::Decl(ast::Decl::Var(vdecl)) => {
                                    use deno_ast::swc::ast::Pat;
                                    for decl in &vdecl.decls {
                                        if let Pat::Ident(ident) = &decl.name {
                                            let name = ident.id.sym.to_string();
                                            if let Some(init) = &decl.init {
                                                if let Some(val) = self.lower_expr(&init, function, &param_map, &mut locals_stack) {
                                                    match val {
                                                        BasicValueEnum::FloatValue(fv) => {
                                                            let alloca = self.builder.build_alloca(self.context.f64_type(), &name).expect("build_alloca failed");
                                                            let _ = self.builder.build_store(alloca, fv);
                                                            self.insert_local_current_scope(&mut locals_stack, name.clone(), alloca, self.context.f64_type().as_basic_type_enum(), true, false);
                                                        }
                                                        BasicValueEnum::PointerValue(pv) => {
                                                            let ptr_ty = self.context.ptr_type(AddressSpace::default());
                                                            let alloca = self.builder.build_alloca(ptr_ty, &name).expect("build_alloca failed");
                                                            let _ = self.builder.build_store(alloca, pv);
                                                            self.insert_local_current_scope(&mut locals_stack, name.clone(), alloca, ptr_ty.as_basic_type_enum(), true, false);
                                                        }
                                                        BasicValueEnum::IntValue(iv) => {
                                                            let boolt = self.context.bool_type();
                                                            let alloca = self.builder.build_alloca(boolt, &name).expect("build_alloca failed");
                                                            let _ = self.builder.build_store(alloca, iv);
                                                            self.insert_local_current_scope(&mut locals_stack, name.clone(), alloca, boolt.as_basic_type_enum(), true, false);
                                                        }
                                                        _ => {}
                                                    }
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
                                let _ = self.builder.build_unconditional_branch(merge_bb).expect("build_unconditional_branch failed");
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
                                                        if let Some(val) = self.lower_expr(arg, function, &param_map, &mut locals_stack) {
                                                            let _ = self.builder.build_return(Some(&val));
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
                                                    let _ = self.lower_expr(&expr_stmt.expr, function, &param_map, &mut locals_stack);
                                                }
                                                ast::Stmt::Decl(ast::Decl::Var(vdecl)) => {
                                                    use deno_ast::swc::ast::Pat;
                                                    for decl in &vdecl.decls {
                                                        if let Pat::Ident(ident) = &decl.name {
                                                            let name = ident.id.sym.to_string();
                                                            if let Some(init) = &decl.init {
                                                                if let Some(val) = self.lower_expr(&init, function, &param_map, &mut locals_stack) {
                                                                    match val {
                                                                        BasicValueEnum::FloatValue(fv) => {
                                                                            let alloca = self.builder.build_alloca(self.context.f64_type(), &name).expect("build_alloca failed");
                                                                            let _ = self.builder.build_store(alloca, fv);
                                                                            self.insert_local_current_scope(&mut locals_stack, name.clone(), alloca, self.context.f64_type().as_basic_type_enum(), true, false);
                                                                        }
                                                                        BasicValueEnum::PointerValue(pv) => {
                                                                            let ptr_ty = self.context.ptr_type(AddressSpace::default());
                                                                            let alloca = self.builder.build_alloca(ptr_ty, &name).expect("build_alloca failed");
                                                                            let _ = self.builder.build_store(alloca, pv);
                                                                            self.insert_local_current_scope(&mut locals_stack, name.clone(), alloca, ptr_ty.as_basic_type_enum(), true, false);
                                                                        }
                                                                        BasicValueEnum::IntValue(iv) => {
                                                                            let boolt = self.context.bool_type();
                                                                            let alloca = self.builder.build_alloca(boolt, &name).expect("build_alloca failed");
                                                                            let _ = self.builder.build_store(alloca, iv);
                                                                            self.insert_local_current_scope(&mut locals_stack, name.clone(), alloca, boolt.as_basic_type_enum(), true, false);
                                                                        }
                                                                        _ => {}
                                                                    }
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
                                            if let Some(val) = self.lower_expr(arg, function, &param_map, &mut locals_stack) {
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
                                        let _ = self.lower_expr(&expr_stmt.expr, function, &param_map, &mut locals_stack);
                                    }
                                    ast::Stmt::Decl(ast::Decl::Var(vdecl)) => {
                                        use deno_ast::swc::ast::Pat;
                                        for decl in &vdecl.decls {
                                            if let Pat::Ident(ident) = &decl.name {
                                                let name = ident.id.sym.to_string();
                                                if let Some(init) = &decl.init {
                                                    if let Some(val) = self.lower_expr(&init, function, &param_map, &mut locals_stack) {
                                                        match val {
                                                            BasicValueEnum::FloatValue(fv) => {
                                                                let alloca = self.builder.build_alloca(self.context.f64_type(), &name).expect("build_alloca failed");
                                                                let _ = self.builder.build_store(alloca, fv);
                                                                self.insert_local_current_scope(&mut locals_stack, name.clone(), alloca, self.context.f64_type().as_basic_type_enum(), true, false);
                                                            }
                                                            BasicValueEnum::PointerValue(pv) => {
                                                                let ptr_ty = self.context.ptr_type(AddressSpace::default());
                                                                let alloca = self.builder.build_alloca(ptr_ty, &name).expect("build_alloca failed");
                                                                let _ = self.builder.build_store(alloca, pv);
                                                                self.insert_local_current_scope(&mut locals_stack, name.clone(), alloca, ptr_ty.as_basic_type_enum(), true, false);
                                                            }
                                                            BasicValueEnum::IntValue(iv) => {
                                                                let boolt = self.context.bool_type();
                                                                let alloca = self.builder.build_alloca(boolt, &name).expect("build_alloca failed");
                                                                let _ = self.builder.build_store(alloca, iv);
                                                                self.insert_local_current_scope(&mut locals_stack, name.clone(), alloca, boolt.as_basic_type_enum(), true, false);
                                                            }
                                                            _ => {}
                                                        }
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
                                let _ = self.builder.build_unconditional_branch(merge_bb).expect("build_unconditional_branch failed");
                            }

                            // continue at merge
                            self.builder.position_at_end(merge_bb);
                        }
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

    /// Emit a small C-compatible `main` function in the module that calls the
    /// generated `oats_main`. This avoids needing a separate host shim object.
    ///
    /// Supported script signatures:
    /// - `main()` -> calls `oats_main()` and ignores return value
    /// - `main(a: number, b: number)` -> calls `oats_main(1.5, 2.25)` and prints the
    ///   returned double via `print_f64` (provided by the runtime staticlib).
    /// Returns true if a host `main` was emitted, false if the signature is
    /// unsupported.
    pub fn emit_host_main(
        &self,
        param_types: &[crate::types::OatsType],
        ret_type: &crate::types::OatsType,
    ) -> bool {
        // If main already present, nothing to do
        if self.module.get_function("main").is_some() {
            return true;
        }

        let oats_fn = match self.module.get_function("oats_main") {
            Some(f) => f,
            None => return false,
        };

        // Build main: int main() which returns an exit code
        let i32t = self.context.i32_type();
        let main_ty = i32t.fn_type(&[], false);
        let main_fn = self.module.add_function("main", main_ty, None);
        let entry = self.context.append_basic_block(main_fn, "entry");
        self.builder.position_at_end(entry);

        // Call oats_main with simple prepared args based on signature
        let call_site = match param_types.len() {
            0 => {
                self.builder
                    .build_call(oats_fn, &[], "call_oats_main")
                    .expect("build_call failed")
            }
            2 => {
                use crate::types::OatsType;
                if param_types[0] != OatsType::Number || param_types[1] != OatsType::Number {
                    let one = i32t.const_int(1, false);
                    let _ = self.builder.build_return(Some(&one.as_basic_value_enum()));
                    return false;
                }
                let a = self.context.f64_type().const_float(1.5);
                let b = self.context.f64_type().const_float(2.25);
                self.builder
                    .build_call(oats_fn, &[a.into(), b.into()], "call_oats_main")
                    .expect("build_call failed")
            }
            _ => {
                let one = i32t.const_int(1, false);
                let _ = self.builder.build_return(Some(&one.as_basic_value_enum()));
                return false;
            }
        };

        // If the script returns a Number, convert f64->i32 and return that.
        if let crate::types::OatsType::Number = ret_type {
            if let inkwell::Either::Left(bv) = call_site.try_as_basic_value() {
                let dbl = bv.into_float_value();
                let code = self.builder.build_float_to_signed_int(dbl, i32t, "ret_to_i32").expect("fp->si failed");
                let _ = self.builder.build_return(Some(&code.as_basic_value_enum()));
            } else {
                let zero = i32t.const_int(0, false);
                let _ = self.builder.build_return(Some(&zero.as_basic_value_enum()));
            }
        } else {
            let zero = i32t.const_int(0, false);
            let _ = self.builder.build_return(Some(&zero.as_basic_value_enum()));
        }

        // Emit oats_entry that mirrors main but returns i32 for hosts that
        // call that symbol from a separate object file.
        if self.module.get_function("oats_entry").is_none() {
            let entry_ty = i32t.fn_type(&[], false);
            let entry_fn = self.module.add_function("oats_entry", entry_ty, None);
            let entry_bb = self.context.append_basic_block(entry_fn, "entry");
            self.builder.position_at_end(entry_bb);

            let call_site = match param_types.len() {
                0 => self
                    .builder
                    .build_call(oats_fn, &[], "call_oats_main")
                    .expect("build_call failed"),
                2 => {
                    use crate::types::OatsType;
                    if param_types[0] == OatsType::Number && param_types[1] == OatsType::Number {
                        let a = self.context.f64_type().const_float(1.5);
                        let b = self.context.f64_type().const_float(2.25);
                        self.builder
                            .build_call(oats_fn, &[a.into(), b.into()], "call_oats_main")
                            .expect("build_call failed")
                    } else {
                        let zero = i32t.const_int(0, false);
                        let _ = self.builder.build_return(Some(&zero.as_basic_value_enum()));
                        return true;
                    }
                }
                _ => {
                    let zero = i32t.const_int(0, false);
                    let _ = self.builder.build_return(Some(&zero.as_basic_value_enum()));
                    return true;
                }
            };

            if let inkwell::Either::Left(bv) = call_site.try_as_basic_value() {
                let dbl = bv.into_float_value();
                let code = self.builder.build_float_to_signed_int(dbl, i32t, "ret_to_i32").expect("fp->si failed");
                let _ = self.builder.build_return(Some(&code.as_basic_value_enum()));
            } else {
                let zero = i32t.const_int(0, false);
                let _ = self.builder.build_return(Some(&zero.as_basic_value_enum()));
            }
        }

        true
    }
}
