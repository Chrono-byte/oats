//! Code generation for top-level items.
//!
//! This module implements lowering of high-level declarations (functions,
//! constructors, etc.) into LLVM IR using the shared `CodeGen` helpers.
//!
//! Notes on design and ABI decisions:
//! - Functions use a simple, handwritten ABI mapping defined by `OatsType` ->
//!   LLVM primitive types. Numbers map to `f64`, booleans to `i1`/i8 as
//!   appropriate, and any pointer-like types map to `i8*` so the runtime can
//!   uniformly operate on object pointers. Unions are mapped to either `f64`
//!   or `i8*` depending on whether any branch is pointer-like; this keeps
//!   math-heavy unions efficient while still supporting heap objects.
//! - Constructors return `i8*` (object base pointer). The runtime expects the
//!   object layout described below and the codegen relies on the runtime's
//!   helpers (rc_inc/rc_dec, box/unbox) to manage lifetimes.

use deno_ast::swc::ast;
use inkwell::values::BasicValue;
use inkwell::values::FunctionValue;
use std::collections::HashMap;

// Type alias for the `locals_stack` used during statement lowering.
//
// This is a stack of scopes, where each scope maps local variable names to
// a tuple containing:
// - PointerValue: the alloca or pointer to the variable's storage.
// - BasicTypeEnum: the LLVM type of the variable.
// - bool: is this variable mutable (declared with `let` vs `const`)
// - bool: is this variable a function parameter
// - bool: is this variable declared as Weak<T> (for reference counting)
// - Option<String>: if the variable is of a NominalStruct type, the name of
//   that nominal type; otherwise None. This helps member lowering resolve
//   fields without fallback heuristics.
type LocalsStackLocal<'a> = Vec<
    HashMap<
        String,
        (
            inkwell::values::PointerValue<'a>,
            inkwell::types::BasicTypeEnum<'a>,
            bool,
            bool,
            bool,
            Option<String>,
        ),
    >,
>;

impl<'a> crate::codegen::CodeGen<'a> {
    /// Compute a conservative live-set for each `await` in `func`.
    ///
    /// Strategy (conservative but tighter than "all locals"):
    /// - Walk the function body in a deterministic, left-to-right order
    ///   collecting encountered `Expr` nodes into a flat vector.
    /// - Record the indices of `Await` expressions in that vector.
    /// - For each await index, collect all identifier names that appear in
    ///   any expression after that index. The result is a Vec of HashSets
    ///   where each entry corresponds to the nth await in lexical order.
    ///
    /// Notes: we intentionally do NOT traverse into nested function/arrow
    /// bodies so identifiers captured only by inner functions don't count
    /// as live for the outer function. This is a conservative pass that
    fn compute_await_live_sets(
        &self,
        func: &ast::Function,
    ) -> Vec<std::collections::HashSet<String>> {
        use std::collections::HashSet;

        enum Node {
            Ident(String),
            AwaitMarker,
        }

        fn flatten_expr(e: &ast::Expr, out: &mut Vec<Node>) {
            use deno_ast::swc::ast::*;
            match e {
                Expr::Ident(id) => out.push(Node::Ident(id.sym.to_string())),
                Expr::Call(c) => {
                    if let Callee::Expr(ec) = &c.callee {
                        flatten_expr(ec, out);
                    }
                    for a in &c.args {
                        flatten_expr(&a.expr, out);
                    }
                }
                Expr::Member(m) => {
                    flatten_expr(&m.obj, out);
                    if let MemberProp::Computed(cmp) = &m.prop {
                        flatten_expr(&cmp.expr, out);
                    }
                }
                Expr::Bin(b) => {
                    flatten_expr(&b.left, out);
                    flatten_expr(&b.right, out);
                }
                Expr::Unary(u) => flatten_expr(&u.arg, out),
                Expr::Await(a) => {
                    // Evaluate the awaited expression first, then mark the
                    // suspension point so idents inside the arg are treated
                    // as occurring before the await (not after).
                    flatten_expr(&a.arg, out);
                    out.push(Node::AwaitMarker);
                }
                Expr::Paren(p) => flatten_expr(&p.expr, out),
                Expr::Array(arr) => {
                    for el in arr.elems.iter().flatten() {
                        flatten_expr(&el.expr, out);
                    }
                }
                Expr::New(n) => {
                    flatten_expr(&n.callee, out);
                    if let Some(args) = &n.args {
                        for a in args {
                            flatten_expr(&a.expr, out);
                        }
                    }
                }
                Expr::Assign(asg) => {
                    // Conservatively flatten assignment
                    // Note: asg.left is an AssignTarget, not Expr, so we only flatten the RHS
                    flatten_expr(&asg.right, out);
                }
                Expr::Cond(c) => {
                    flatten_expr(&c.test, out);
                    flatten_expr(&c.cons, out);
                    flatten_expr(&c.alt, out);
                }
                Expr::Tpl(_) | Expr::Lit(_) => {
                    // literals: no idents to record
                }
                Expr::Arrow(a) => {
                    // don't recurse into nested function/arrow bodies
                    let _ = a;
                }
                _ => {
                    // conservative: don't traverse into patterns we don't care about
                }
            }
        }

        fn flatten_stmt(s: &ast::Stmt, out: &mut Vec<Node>) {
            use deno_ast::swc::ast::*;
            match s {
                Stmt::Expr(es) => flatten_expr(&es.expr, out),
                Stmt::Return(r) => {
                    if let Some(arg) = &r.arg {
                        flatten_expr(arg, out);
                    }
                }
                Stmt::Decl(ast::Decl::Var(vd)) => {
                    for decl in &vd.decls {
                        if let Some(init) = &decl.init {
                            flatten_expr(init, out);
                        }
                    }
                }
                Stmt::If(ifst) => {
                    flatten_expr(&ifst.test, out);
                    // shallow: if cons/alt are simple expr statements, flatten
                    match &*ifst.cons {
                        Stmt::Expr(e) => flatten_expr(&e.expr, out),
                        Stmt::Block(b) => {
                            for st in &b.stmts {
                                flatten_stmt(st, out);
                            }
                        }
                        _ => {}
                    }
                    if let Some(alt) = &ifst.alt {
                        match &**alt {
                            Stmt::Expr(e) => flatten_expr(&e.expr, out),
                            Stmt::Block(b) => {
                                for st in &b.stmts {
                                    flatten_stmt(st, out);
                                }
                            }
                            _ => {}
                        }
                    }
                }
                Stmt::Block(b) => {
                    for st in &b.stmts {
                        flatten_stmt(st, out);
                    }
                }
                Stmt::While(w) => {
                    flatten_expr(&w.test, out);
                    flatten_stmt(&w.body, out);
                }
                Stmt::For(f) => {
                    if let Some(init) = &f.init {
                        match init {
                            deno_ast::swc::ast::VarDeclOrExpr::Expr(e) => flatten_expr(e, out),
                            deno_ast::swc::ast::VarDeclOrExpr::VarDecl(vd) => {
                                for decl in &vd.decls {
                                    if let Some(init) = &decl.init {
                                        flatten_expr(init, out);
                                    }
                                }
                            }
                        }
                    }
                    if let Some(test) = &f.test {
                        flatten_expr(test, out);
                    }
                    if let Some(update) = &f.update {
                        flatten_expr(update, out);
                    }
                    flatten_stmt(&f.body, out);
                }
                _ => {}
            }
        }

        // Build a single ordered sequence of Nodes for the whole function body
        let mut nodes: Vec<Node> = Vec::new();
        if let Some(body) = &func.body {
            for s in &body.stmts {
                flatten_stmt(s, &mut nodes);
            }
        }

        // Find indexes of await markers
        let mut await_positions: Vec<usize> = Vec::new();
        for (i, n) in nodes.iter().enumerate() {
            if let Node::AwaitMarker = n {
                await_positions.push(i);
            }
        }

        // For each await position, collect all identifier names that occur after it
        let mut res: Vec<HashSet<String>> = Vec::new();
        for &pos in &await_positions {
            let mut set = HashSet::new();
            for n in nodes.iter().skip(pos + 1) {
                if let Node::Ident(name) = n {
                    set.insert(name.clone());
                }
            }
            res.push(set);
        }

        res
    }
    /// Generates LLVM IR for a function declaration.
    ///
    /// This is the main entry point for compiling a user function into LLVM
    /// IR. Responsibilities include:
    /// - Mapping Oats parameter and return types to LLVM ABI types.
    /// - Creating the LLVM function and entry basic block.
    /// - Registering any anonymous struct-typed parameters as generated
    ///   nominal structs so member lowering can resolve fields.
    /// - Allocating stack slots for parameters and wiring the initial
    ///   `locals_stack` used by statement lowering.
    /// - Lowering the function body via `lower_stmts` and emitting an
    ///   implicit `return` and `rc_dec` cleanup if the body doesn't
    ///   explicitly terminate.
    ///
    /// # Arguments
    /// * `func_name` - exported name to give the generated LLVM function.
    /// * `func_decl` - the AST `Function` node describing parameters and body.
    /// * `param_types` - the list of resolved `OatsType` for parameters.
    /// * `ret_type` - the declared return `OatsType` for ABI mapping.
    /// * `receiver_name` - optional `this` receiver name for methods.
    ///
    /// # Returns
    /// Returns the created `FunctionValue` on success or a `Diagnostic` on
    /// failure. The function emits calls to runtime helpers (for example
    /// for union boxing) and expects the runtime to provide those symbols
    /// during linking.
    pub fn gen_function_ir(
        &self,
        func_name: &str,
        func_decl: &ast::Function,
        param_types: &[crate::types::OatsType],
        ret_type: &crate::types::OatsType,
        receiver_name: Option<&str>,
    ) -> Result<FunctionValue<'a>, crate::diagnostics::Diagnostic> {
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

        // If this is an `async` function, emit a synchronous implementation
        // function `<name>_async_impl` that returns the inner promise type, and
        // make the exported function a thin wrapper that calls the impl and
        // returns a resolved promise via `promise_resolve`.
        if func_decl.is_async {
            use crate::types::OatsType;

            // Expect declared return to be `Promise(inner)`
            let inner_ret = if let OatsType::Promise(inner) = ret_type {
                inner.clone()
            } else {
                return Err(crate::diagnostics::Diagnostic::simple(
                    "async function must declare a Promise<T> return type",
                ));
            };

            // Create a non-async clone of the function AST for the impl
            let mut impl_decl = func_decl.clone();
            impl_decl.is_async = false;
            let impl_name = format!("{}_async_impl", func_name);

            // Generate the synchronous implementation function which returns T
            let impl_fn = self.gen_function_ir(
                &impl_name,
                &impl_decl,
                param_types,
                &inner_ret,
                receiver_name,
            )?;

            // Emit a poll function: `fn <name>_poll(state: i8*, out: i8*) -> i32`
            let poll_name = format!("{}_poll", func_name);
            let poll_ft = self
                .i32_t
                .fn_type(&[self.i8ptr_t.into(), self.i8ptr_t.into()], false);
            let poll_f = self.module.add_function(&poll_name, poll_ft, None);
            let poll_entry = self.context.append_basic_block(poll_f, "entry");
            self.builder.position_at_end(poll_entry);
            // Compute per-await live-sets early so we can reserve state
            // layout indices and prepare resume/cont basic blocks before
            // emitting the poll body. This lets the poll function set a
            // correct "done" sentinel value that does not collide with
            // resume indices.
            let param_count = llvm_param_types.len();
            let await_live_sets = self.compute_await_live_sets(&impl_decl);
            let mut all_live_names: std::collections::HashSet<String> =
                std::collections::HashSet::new();
            for s in &await_live_sets {
                for n in s.iter() {
                    all_live_names.insert(n.clone());
                }
            }
            // Reserve one 8-byte slot per live local name (after param slots)
            let local_slot_count = all_live_names.len();
            let _total_slots = param_count + local_slot_count;

            // Prepare resume/cont block vectors sized to number of awaits
            let mut resume_blocks: Vec<inkwell::basic_block::BasicBlock<'a>> = Vec::new();
            let mut cont_blocks: Vec<inkwell::basic_block::BasicBlock<'a>> = Vec::new();
            for i in 0..await_live_sets.len() {
                resume_blocks.push(
                    self.context
                        .append_basic_block(poll_f, &format!("resume_{}", i)),
                );
                cont_blocks.push(
                    self.context
                        .append_basic_block(poll_f, &format!("cont_{}", i)),
                );
            }

            // Now emit the poll body entry: read the state field (u32 at offset +8)
            // and dispatch based on it. Layout: [poll_fn_ptr (8)] [state_u32 (4) + pad(4)] [param slots ...]
            // If state == 0 => call impl (first run). If state matches a resume
            // index (1-based) we will jump into the corresponding resume block.
            // Otherwise the promise is already completed and we return ready.
            let state_param = poll_f.get_nth_param(0).unwrap();
            let state_ptr = state_param.into_pointer_value();

            // Read state u32 at offset 8
            let state_addr_int =
                match self
                    .builder
                    .build_ptr_to_int(state_ptr, self.i64_t, "state_addr")
                {
                    Ok(v) => v,
                    Err(_) => {
                        return Err(crate::diagnostics::Diagnostic::simple(
                            "ptr_to_int failed in poll",
                        ));
                    }
                };
            let state_off = self.i64_t.const_int(8, false);
            let state_field_int =
                match self
                    .builder
                    .build_int_add(state_addr_int, state_off, "state_field_addr")
                {
                    Ok(v) => v,
                    Err(_) => {
                        return Err(crate::diagnostics::Diagnostic::simple(
                            "int_add failed in poll",
                        ));
                    }
                };
            let state_field_ptr = match self.builder.build_int_to_ptr(
                state_field_int,
                self.context.ptr_type(inkwell::AddressSpace::default()),
                "state_field_ptr",
            ) {
                Ok(p) => p,
                Err(_) => {
                    return Err(crate::diagnostics::Diagnostic::simple(
                        "int_to_ptr failed in poll",
                    ));
                }
            };
            let state_loaded =
                match self
                    .builder
                    .build_load(self.i32_t, state_field_ptr, "state_ld")
                {
                    Ok(v) => v.into_int_value(),
                    Err(_) => {
                        return Err(crate::diagnostics::Diagnostic::simple(
                            "failed to load state field in poll",
                        ));
                    }
                };

            // Compare state == 0
            let zero = self.i32_t.const_int(0, false);
            let is_zero = match self.builder.build_int_compare(
                inkwell::IntPredicate::EQ,
                state_loaded,
                zero,
                "state_eq_zero",
            ) {
                Ok(iv) => iv,
                Err(_) => {
                    return Err(crate::diagnostics::Diagnostic::simple(
                        "int_compare failed in poll",
                    ));
                }
            };
            // Create blocks: run_impl, already_done
            let run_impl_bb = self.context.append_basic_block(poll_f, "run_impl");
            let done_bb = self.context.append_basic_block(poll_f, "already_done");
            match self
                .builder
                .build_conditional_branch(is_zero, run_impl_bb, done_bb)
            {
                Ok(_) => {}
                Err(_) => {
                    return Err(crate::diagnostics::Diagnostic::simple(
                        "failed to build conditional branch in poll",
                    ));
                }
            }

            // run_impl: lower the original function body here
            self.builder.position_at_end(run_impl_bb);
            // Create param allocas & locals mapping for poll_f so lowering
            // operates on the poll function and can access the state param.
            let (param_map, mut locals_stack) =
                self.create_param_allocas(poll_f, func_decl, &llvm_param_types, receiver_name)?;
            // Keep a copy of the poll function's locals stack so resume blocks
            // can restore saved values into the same allocas later.
            *self.async_poll_locals.borrow_mut() = Some(locals_stack.clone());

            // Lower the body statements into the poll function's run_impl block.
            let mut emitted_terminator = false;
            if let Some(body) = &func_decl.body {
                emitted_terminator =
                    self.lower_stmts(&body.stmts, poll_f, &param_map, &mut locals_stack)?;
            }

            if !emitted_terminator
                && self
                    .builder
                    .get_insert_block()
                    .is_none_or(|b| b.get_terminator().is_none())
            {
                self.emit_rc_dec_for_locals(&locals_stack);
                match ret_type {
                    crate::types::OatsType::Void => {
                        self.builder.build_return(None).map_err(|_| {
                            crate::diagnostics::Diagnostic::simple(
                                "Failed to build implicit return in poll",
                            )
                        })?;
                    }
                    crate::types::OatsType::Number => {
                        let zero = self.f64_t.const_float(0.0);
                        self.builder
                            .build_return(Some(&zero.as_basic_value_enum()))
                            .map_err(|_| {
                                crate::diagnostics::Diagnostic::simple(
                                    "Failed to build implicit return (number) in poll",
                                )
                            })?;
                    }
                    _ => {
                        let nullp = self.i8ptr_t.const_null();
                        self.builder
                            .build_return(Some(&nullp.as_basic_value_enum()))
                            .map_err(|_| {
                                crate::diagnostics::Diagnostic::simple(
                                    "Failed to build implicit return (ptr) in poll",
                                )
                            })?;
                    }
                }
            }
            // We reconstruct values with the expected LLVM ABI types and pass
            // them to the impl function here.
            // (state_ptr already holds the state base pointer)

            // Build args by loading each saved slot from the heap state.
            let mut impl_args: Vec<inkwell::values::BasicMetadataValueEnum> = Vec::new();
            for (i, pty) in llvm_param_types.iter().enumerate() {
                // compute slot address: state_base + (16 + i*8)
                let base_int =
                    match self
                        .builder
                        .build_ptr_to_int(state_ptr, self.i64_t, "state_addr")
                    {
                        Ok(v) => v,
                        Err(_) => {
                            return Err(crate::diagnostics::Diagnostic::simple(
                                "ptr_to_int failed in poll",
                            ));
                        }
                    };
                let off = self.i64_t.const_int(16 + (i as u64 * 8), false);
                let slot_addr_int = match self.builder.build_int_add(base_int, off, "slot_addr") {
                    Ok(v) => v,
                    Err(_) => {
                        return Err(crate::diagnostics::Diagnostic::simple(
                            "int_add failed in poll",
                        ));
                    }
                };

                // We'll treat state slots as i8* pointers. Numbers are boxed
                // into union objects when stored, so here we load an i8* and
                // unbox for Number parameters; pointer-like params are passed
                // through directly.
                let slot_ptr =
                    match self
                        .builder
                        .build_int_to_ptr(slot_addr_int, self.i8ptr_t, "slot_ptr")
                    {
                        Ok(p) => p,
                        Err(_) => {
                            return Err(crate::diagnostics::Diagnostic::simple(
                                "int_to_ptr failed in poll",
                            ));
                        }
                    };

                let loaded_ptr = match self.builder.build_load(self.i8ptr_t, slot_ptr, "arg_load") {
                    Ok(v) => v,
                    Err(_) => {
                        return Err(crate::diagnostics::Diagnostic::simple(
                            "failed to load captured arg in poll",
                        ));
                    }
                };

                // If the original param ABI was a float (Number), unbox it.
                match pty {
                    inkwell::types::BasicTypeEnum::FloatType(_) => {
                        let unbox_fn = self.get_union_unbox_f64();
                        let cs = match self.builder.build_call(
                            unbox_fn,
                            &[loaded_ptr.into()],
                            "unbox_f64_poll",
                        ) {
                            Ok(cs) => cs,
                            Err(_) => {
                                return Err(crate::diagnostics::Diagnostic::simple(
                                    "union_unbox_f64 call failed in poll",
                                ));
                            }
                        };
                        let fv = cs
                            .try_as_basic_value()
                            .left()
                            .ok_or_else(|| {
                                crate::diagnostics::Diagnostic::simple("unbox returned no value")
                            })?
                            .into_float_value();
                        impl_args.push(fv.into());
                    }
                    _ => {
                        impl_args.push(loaded_ptr.into());
                    }
                }
            }

            let call_site = match self
                .builder
                .build_call(impl_fn, &impl_args, "call_impl_in_poll")
            {
                Ok(cs) => cs,
                Err(_) => {
                    return Err(crate::diagnostics::Diagnostic::simple(
                        "failed to call impl in poll",
                    ));
                }
            };
            let either = call_site.try_as_basic_value();
            let payload_bv = if matches!(*inner_ret, OatsType::Void) {
                // write null into out_ptr
                let nullp = self.i8ptr_t.const_null();
                nullp.as_basic_value_enum()
            } else {
                let bv = either.left().ok_or_else(|| {
                    crate::diagnostics::Diagnostic::simple("async impl did not return value")
                })?;
                match *inner_ret {
                    OatsType::Number => {
                        // box f64 -> i8*
                        let fv = bv.into_float_value();
                        let box_fn = self.get_union_box_f64();
                        let cs = match self
                            .builder
                            .build_call(box_fn, &[fv.into()], "box_f64_poll")
                        {
                            Ok(cs) => cs,
                            Err(_) => {
                                return Err(crate::diagnostics::Diagnostic::simple(
                                    "union_box_f64 call failed",
                                ));
                            }
                        };
                        cs.try_as_basic_value().left().ok_or_else(|| {
                            crate::diagnostics::Diagnostic::simple("boxing failed")
                        })?
                    }
                    _ => bv,
                }
            };

            // store payload into out_ptr (assume out_ptr points to i8* slot)
            let out_param = poll_f.get_nth_param(1).unwrap();
            let out_ptr_cast = self
                .builder
                .build_pointer_cast(out_param.into_pointer_value(), self.i8ptr_t, "out_cast")
                .map_err(|_| crate::diagnostics::Diagnostic::simple("pointer cast failed"))?;
            let _ = self.builder.build_store(out_ptr_cast, payload_bv);

            // set state = 1
            let one_i32 = self.i32_t.const_int(1, false);
            let _ = self.builder.build_store(state_field_ptr, one_i32);

            // return 1 (ready)
            let one = self.i32_t.const_int(1, false);
            let _ = self.builder.build_return(Some(&one.as_basic_value_enum()));

            // We will fill in `done_bb` (dispatch to resumes or return ready)
            // after we compute the resume/cont vectors and the done sentinel
            // value. For now leave `done_bb` empty and continue lowering the
            // impl into `run_impl_bb`.

            // Move builder back to the wrapper function entry so subsequent
            // allocations and stores are emitted into the exported wrapper
            // rather than the poll function.
            self.builder.position_at_end(entry);

            // Now create a heap-allocated state object and store the poll fn ptr at offset 0
            // State layout: [poll_fn_ptr (8)] [reserved 8 bytes] [param slots 8 bytes each ...]
            let malloc_fn = self.get_malloc();
            // Compute per-await live-sets so we can reserve state slots for
            // locals that must be preserved across suspension points. This
            // only computes the layout here; actual save/restore emission
            // will be implemented in the next step.
            let param_count = llvm_param_types.len();
            let await_live_sets = self.compute_await_live_sets(&impl_decl);
            let mut all_live_names: std::collections::HashSet<String> =
                std::collections::HashSet::new();
            for s in &await_live_sets {
                for n in s.iter() {
                    all_live_names.insert(n.clone());
                }
            }
            // Reserve one 8-byte slot per live local name (after param slots)
            let local_slot_count = all_live_names.len();
            let total_slots = param_count + local_slot_count;
            let state_size_bytes = 16u64 + (total_slots as u64 * 8u64);
            let state_size = self.i64_t.const_int(state_size_bytes, false);
            let cs = match self
                .builder
                .build_call(malloc_fn, &[state_size.into()], "malloc_state")
            {
                Ok(cs) => cs,
                Err(_) => {
                    return Err(crate::diagnostics::Diagnostic::simple(
                        "malloc failed for state",
                    ));
                }
            };
            let state_ptr = cs
                .try_as_basic_value()
                .left()
                .ok_or_else(|| crate::diagnostics::Diagnostic::simple("malloc failed for state"))?
                .into_pointer_value();

            // store poll function pointer at offset 0 (first word of state)
            // cast poll_f to i8* and store at state_ptr
            let poll_ptr = poll_f.as_global_value().as_pointer_value();
            let poll_ptr_i8 =
                match self
                    .builder
                    .build_pointer_cast(poll_ptr, self.i8ptr_t, "poll_i8ptr")
                {
                    Ok(p) => p,
                    Err(_) => {
                        return Err(crate::diagnostics::Diagnostic::simple(
                            "pointer cast failed",
                        ));
                    }
                };
            let _ = self
                .builder
                .build_store(state_ptr, poll_ptr_i8.as_basic_value_enum());

            // Store each incoming parameter into the state slots starting at offset 16
            for (i, pty) in llvm_param_types.iter().enumerate().take(param_count) {
                if let Some(arg_val) = function.get_nth_param(i as u32) {
                    let slot_offset = 16 + (i as u64 * 8);
                    let obj_ptr_int = match self.builder.build_ptr_to_int(
                        state_ptr,
                        self.i64_t,
                        &format!("state_addr_store_{}", i),
                    ) {
                        Ok(v) => v,
                        Err(_) => {
                            return Err(crate::diagnostics::Diagnostic::simple(
                                "ptr_to_int failed when storing captures",
                            ));
                        }
                    };
                    let off_const = self.i64_t.const_int(slot_offset, false);
                    let field_addr = match self.builder.build_int_add(
                        obj_ptr_int,
                        off_const,
                        &format!("slot_addr_store_{}", i),
                    ) {
                        Ok(v) => v,
                        Err(_) => {
                            return Err(crate::diagnostics::Diagnostic::simple(
                                "int_add failed when storing captures",
                            ));
                        }
                    };

                    // Use i8* slots for stored captures. If the parameter ABI is
                    // a float (Number), box it into a union object before
                    // storing; otherwise store the pointer/value directly.
                    let slot_ptr = match self.builder.build_int_to_ptr(
                        field_addr,
                        self.i8ptr_t,
                        &format!("slot_ptr_store_{}", i),
                    ) {
                        Ok(p) => p,
                        Err(_) => {
                            return Err(crate::diagnostics::Diagnostic::simple(
                                "int_to_ptr failed when storing captures",
                            ));
                        }
                    };

                    // If the function parameter ABI is a float, box it.
                    match pty {
                        inkwell::types::BasicTypeEnum::FloatType(_) => {
                            let fv = arg_val.into_float_value();
                            let box_fn = self.get_union_box_f64();
                            let cs = match self.builder.build_call(
                                box_fn,
                                &[fv.into()],
                                &format!("box_f64_store_{}", i),
                            ) {
                                Ok(cs) => cs,
                                Err(_) => {
                                    return Err(crate::diagnostics::Diagnostic::simple(
                                        "union_box_f64 call failed when storing captures",
                                    ));
                                }
                            };
                            let boxed_ptr = cs
                                .try_as_basic_value()
                                .left()
                                .ok_or_else(|| {
                                    crate::diagnostics::Diagnostic::simple(
                                        "boxing returned no value",
                                    )
                                })?
                                .into_pointer_value();
                            let _ = self
                                .builder
                                .build_store(slot_ptr, boxed_ptr.as_basic_value_enum());
                        }
                        _ => {
                            // store pointer-like or integer param directly into i8* slot
                            let _ = self.builder.build_store(slot_ptr, arg_val);
                        }
                    }
                }
            }

            // (Optional) Build a name -> slot index map for locals. Locals occupy
            // slots after the parameter slots. We'll attach this mapping to a
            // local variable to be used later when emitting save/restore.
            // Deterministically assign slot indices to local names by
            // sorting the name list. This prevents non-deterministic builds
            // when `HashSet` iteration order varies across runs.
            let mut local_name_to_slot: std::collections::HashMap<String, usize> =
                std::collections::HashMap::new();
            let mut names: Vec<String> = all_live_names.into_iter().collect();
            names.sort();
            let mut idx = param_count;
            for name in names.into_iter() {
                local_name_to_slot.insert(name, idx);
                idx += 1;
            }

            // Store async lowering context in CodeGen so expression lowering
            // (await) can consult live-sets and the name->slot map.
            self.async_await_live_sets
                .borrow_mut()
                .replace(await_live_sets.clone());
            self.async_local_name_to_slot
                .borrow_mut()
                .replace(local_name_to_slot.clone());
            // Prepare empty resume/cont block vectors sized to number of awaits
            let mut resume_blocks: Vec<inkwell::basic_block::BasicBlock<'a>> = Vec::new();
            let mut cont_blocks: Vec<inkwell::basic_block::BasicBlock<'a>> = Vec::new();
            for i in 0..await_live_sets.len() {
                resume_blocks.push(
                    self.context
                        .append_basic_block(poll_f, &format!("resume_{}", i)),
                );
                cont_blocks.push(
                    self.context
                        .append_basic_block(poll_f, &format!("cont_{}", i)),
                );
            }
            self.async_resume_blocks.borrow_mut().replace(resume_blocks);
            self.async_cont_blocks.borrow_mut().replace(cont_blocks);
            self.async_poll_function.borrow_mut().replace(poll_f);
            self.async_await_counter.set(0);
            self.async_param_count.set(param_count as u32);

            // Now populate the `already_done` block to either dispatch to
            // resume_i blocks (if state contains a resume index) or return
            // ready immediately when the promise is already completed.
            // We consider resume indices to be in the range [1..=await_count].
            let _await_count = await_live_sets.len();
            self.builder.position_at_end(done_bb);
            // if state_loaded == 0 -> unreachable here; compare against 1..N
            // We'll build a switch: default -> return ready; cases 1..N -> branch to resume_i
            let default_ret = self.i32_t.const_int(1, false);
            // Build the switch instruction on state_loaded and add cases for
            // each resume index (1-based -> resume_0..resume_{N-1}).
            // Build a switch on the state_loaded value. The builder API
            // expects a SwitchValue with an expected number of cases.
            // Build an explicit cases slice for the switch instruction.
            let mut cases: Vec<(
                inkwell::values::IntValue<'a>,
                inkwell::basic_block::BasicBlock<'a>,
            )> = Vec::new();
            if let Some(resume_vec) = &*self.async_resume_blocks.borrow() {
                for (i, rb) in resume_vec.iter().enumerate() {
                    let case_val = self.i32_t.const_int(i as u64 + 1, false);
                    cases.push((case_val, *rb));
                }
            }
            let _switch_inst = self
                .builder
                .build_switch(state_loaded, done_bb, cases.as_slice())
                .map_err(|_| {
                    crate::diagnostics::Diagnostic::simple("failed to build switch in poll")
                })?;

            // Emit resume handlers: each resume block reloads live locals
            // from the state slots into the poll function's allocas and then
            // jumps to the corresponding cont block.
            if let (Some(local_map), Some(live_sets), Some(poll_locals)) = (
                &*self.async_local_name_to_slot.borrow(),
                &*self.async_await_live_sets.borrow(),
                &*self.async_poll_locals.borrow(),
            )
                && let (Some(resume_vec), Some(cont_vec)) = (
                    &*self.async_resume_blocks.borrow(),
                    &*self.async_cont_blocks.borrow(),
                ) {
                    for (i, rb) in resume_vec.iter().enumerate() {
                        let cont_bb = cont_vec[i];
                        self.builder.position_at_end(*rb);
                        if let Some(live_set) = live_sets.get(i) {
                            for name in live_set.iter() {
                                if let Some(slot_idx) = local_map.get(name) {
                                    // locate the alloca for the local in poll_locals
                                    let mut target_alloca: Option<(
                                        inkwell::values::PointerValue<'a>,
                                        inkwell::types::BasicTypeEnum<'a>,
                                    )> = None;
                                    for scope in poll_locals.iter().rev() {
                                        if let Some(entry) = scope.get(name) {
                                            target_alloca = Some((entry.0, entry.1));
                                            break;
                                        }
                                    }
                                    if let Some((alloca_ptr, ty)) = target_alloca {
                                        // compute slot addr and load i8*
                                        // Use the poll function's incoming state pointer
                                        // (state_ptr was defined earlier) to compute the
                                        // resume slot address.
                                        let base_int = match self.builder.build_ptr_to_int(
                                            state_ptr,
                                            self.i64_t,
                                            "resume_state_addr",
                                        ) {
                                            Ok(v) => v,
                                            Err(_) => {
                                                return Err(
                                                    crate::diagnostics::Diagnostic::simple(
                                                        "ptr_to_int failed when resuming locals",
                                                    ),
                                                );
                                            }
                                        };
                                        let off_const = self
                                            .i64_t
                                            .const_int(16 + (*slot_idx as u64 * 8), false);
                                        let slot_addr_int = match self.builder.build_int_add(
                                            base_int,
                                            off_const,
                                            "slot_addr_resume",
                                        ) {
                                            Ok(v) => v,
                                            Err(_) => {
                                                return Err(
                                                    crate::diagnostics::Diagnostic::simple(
                                                        "int_add failed when resuming locals",
                                                    ),
                                                );
                                            }
                                        };
                                        let slot_ptr = match self.builder.build_int_to_ptr(
                                            slot_addr_int,
                                            self.i8ptr_t,
                                            "slot_ptr_resume",
                                        ) {
                                            Ok(p) => p,
                                            Err(_) => {
                                                return Err(
                                                    crate::diagnostics::Diagnostic::simple(
                                                        "int_to_ptr failed when resuming locals",
                                                    ),
                                                );
                                            }
                                        };
                                        let loaded_slot = match self.builder.build_load(
                                            self.i8ptr_t,
                                            slot_ptr,
                                            &format!("slot_load_{}", name),
                                        ) {
                                            Ok(v) => v,
                                            Err(_) => {
                                                return Err(
                                                    crate::diagnostics::Diagnostic::simple(
                                                        "failed to load from slot when resuming",
                                                    ),
                                                );
                                            }
                                        };
                                        // store into alloca (unbox for floats)
                                        match ty {
                                            inkwell::types::BasicTypeEnum::FloatType(_) => {
                                                let unbox_fn = self.get_union_unbox_f64();
                                                let cs = match self.builder.build_call(
                                                    unbox_fn,
                                                    &[loaded_slot.into()],
                                                    "unbox_resume",
                                                ) {
                                                    Ok(c) => c,
                                                    Err(_) => {
                                                        return Err(
                                                            crate::diagnostics::Diagnostic::simple(
                                                                "union_unbox_f64 call failed when resuming locals",
                                                            ),
                                                        );
                                                    }
                                                };
                                                let fv = cs
                                                            .try_as_basic_value()
                                                            .left()
                                                            .ok_or_else(|| {
                                                                crate::diagnostics::Diagnostic::simple(
                                                                    "unbox returned no value when resuming locals",
                                                                )
                                                            })?
                                                            .into_float_value();
                                                let _ = self.builder.build_store(
                                                    alloca_ptr,
                                                    fv.as_basic_value_enum(),
                                                );
                                            }
                                            _ => {
                                                let slot_ptr_val = loaded_slot.into_pointer_value();
                                                let casted = match self.builder.build_pointer_cast(
                                                    slot_ptr_val,
                                                    alloca_ptr.get_type(),
                                                    "cast_resume",
                                                ) {
                                                    Ok(c) => c,
                                                    Err(_) => {
                                                        return Err(
                                                            crate::diagnostics::Diagnostic::simple(
                                                                "pointer cast failed when resuming locals",
                                                            ),
                                                        );
                                                    }
                                                };
                                                let _ = self.builder.build_store(
                                                    alloca_ptr,
                                                    casted.as_basic_value_enum(),
                                                );
                                            }
                                        }
                                    }
                                }
                            }
                        }
                        let _ = self.builder.build_unconditional_branch(cont_bb);
                    }
                }

            // Default: return ready (1)
            let _ = self
                .builder
                .build_return(Some(&default_ret.as_basic_value_enum()));

            // Call promise_new_from_state(state_ptr)
            let p_new = self.get_promise_new_from_state();
            let pres_cs = self
                .builder
                .build_call(
                    p_new,
                    &[state_ptr.as_basic_value_enum().into()],
                    "promise_new_from_state",
                )
                .map_err(|_| {
                    crate::diagnostics::Diagnostic::simple("promise_new_from_state call failed")
                })?;
            let pres_val = pres_cs.try_as_basic_value().left().ok_or_else(|| {
                crate::diagnostics::Diagnostic::simple("promise_new_from_state returned no value")
            })?;

            // Enqueue the promise (no-op for MVP) and return it
            let enq = self.get_executor_enqueue();
            let _ = self
                .builder
                .build_call(enq, &[pres_val.into()], "executor_enqueue");
            let _ = self.builder.build_return(Some(&pres_val));

            // Clear async lowering context stored in CodeGen so subsequent
            // function compilations don't accidentally reuse state from
            // this async wrapper. We intentionally `take()` the Option so
            // the previous values are dropped here.
            let _ = self.async_await_live_sets.borrow_mut().take();
            let _ = self.async_local_name_to_slot.borrow_mut().take();
            let _ = self.async_resume_blocks.borrow_mut().take();
            let _ = self.async_cont_blocks.borrow_mut().take();
            let _ = self.async_poll_function.borrow_mut().take();
            let _ = self.async_poll_locals.borrow_mut().take();
            self.async_await_counter.set(0);
            self.async_param_count.set(0);

            return Ok(function);
        }

        // 3. If any parameter types are anonymous struct literals, register
        // them as nominal structs in `class_fields` under a generated name so
        // downstream lowering (which expects nominal names) can resolve
        // member accesses. Then store param types in `fn_param_types`.
        let mut param_types_owned = param_types.to_vec();
        for (i, p) in param_types_owned.iter_mut().enumerate() {
            if let crate::types::OatsType::StructLiteral(fields) = p {
                // Create a generated nominal name like <func_name>_param_struct_<i>
                let gen_name = format!("{}_param_struct_{}", func_name, i);
                // Insert into class_fields for lowering
                self.class_fields
                    .borrow_mut()
                    .insert(gen_name.clone(), fields.clone());
                // Replace the param type with NominalStruct so existing lowering uses it
                *p = crate::types::OatsType::NominalStruct(gen_name);
            }
        }
        self.fn_param_types
            .borrow_mut()
            .insert(func_name.to_string(), param_types_owned.clone());
        let (param_map, mut locals_stack) =
            self.create_param_allocas(function, func_decl, &llvm_param_types, receiver_name)?;

        // 4. Lower the function body statements into IR.
        let mut emitted_terminator = false;
        if let Some(body) = &func_decl.body {
            emitted_terminator =
                self.lower_stmts(&body.stmts, function, &param_map, &mut locals_stack)?;
        }

        // 5. Add an implicit `return void` if the function hasn't already returned.
        if !emitted_terminator
            && self
                .builder
                .get_insert_block()
                .is_none_or(|b| b.get_terminator().is_none())
        {
            self.emit_rc_dec_for_locals(&locals_stack);
            // If the declared return type is Void, emit a bare `ret void`.
            // Otherwise, return a sensible zero/null value matching the ABI
            // (0.0 for numbers, null ptr for pointer-like returns).
            match ret_type {
                crate::types::OatsType::Void => {
                    self.builder.build_return(None).map_err(|_| {
                        crate::diagnostics::Diagnostic::simple("Failed to build implicit return")
                    })?;
                }
                crate::types::OatsType::Number => {
                    let zero = self.f64_t.const_float(0.0);
                    self.builder
                        .build_return(Some(&zero.as_basic_value_enum()))
                        .map_err(|_| {
                            crate::diagnostics::Diagnostic::simple(
                                "Failed to build implicit return (number)",
                            )
                        })?;
                }
                _ => {
                    // Pointer-like or other -> return null i8*.
                    let nullp = self.i8ptr_t.const_null();
                    self.builder
                        .build_return(Some(&nullp.as_basic_value_enum()))
                        .map_err(|_| {
                            crate::diagnostics::Diagnostic::simple(
                                "Failed to build implicit return (ptr)",
                            )
                        })?;
                }
            }
        }

        Ok(function)
    }

    /// Generate a complete constructor function for a class.
    ///
    /// The constructor created here follows the runtime object layout used
    /// throughout Oats:
    ///
    /// Layout (byte offsets):
    /// - 0: header (8 bytes) — stores reference count and flags. See the
    ///   runtime for the bit layout constants.
    /// - 8: metadata pointer (8 bytes) — points to a `*_field_map` global
    ///   used by the cycle collector/runtime to locate pointer fields.
    /// - 16+: field slots (each 8 bytes)
    ///
    /// The constructor allocates the object via the runtime `malloc` helper,
    /// sets up the header with an initial RC of 1, stores the `field_map`
    /// pointer at offset 8, zero-initializes field slots, and stores any
    /// constructor parameters into fields, performing union-boxing and
    /// reference-count increments as required.
    ///
    /// # Arguments
    /// * `class_name` - nominal class name used to name the generated ctor
    ///   and the `*_field_map` global.
    /// * `ctor` - AST node describing constructor parameters and body.
    /// * `fields` - list of class fields with their `OatsType`s.
    ///
    /// # Returns
    /// Returns `Ok(())` on success or a `Diagnostic` on error.
    pub fn gen_constructor_ir(
        &self,
        class_name: &str,
        ctor: &deno_ast::swc::ast::Constructor,
        fields: &[(String, crate::types::OatsType)],
    ) -> Result<(), crate::diagnostics::Diagnostic> {
        use crate::types::OatsType;

        let fname = format!("{}_ctor", class_name);
        let init_name = format!("{}_init", class_name);

        let mut param_types_vec: Vec<crate::types::OatsType> = Vec::new();
        let mut param_names: Vec<String> = Vec::new();

        for param in &ctor.params {
            use deno_ast::swc::ast::{ParamOrTsParamProp, TsParamPropParam};
            match param {
                ParamOrTsParamProp::TsParamProp(ts_param) => {
                    if let TsParamPropParam::Ident(binding_ident) = &ts_param.param {
                        let pname = binding_ident.id.sym.to_string();
                        let pty = binding_ident
                            .type_ann
                            .as_ref()
                            .and_then(|ann| crate::types::map_ts_type(&ann.type_ann))
                            .unwrap_or(OatsType::Number);
                        param_types_vec.push(pty);
                        param_names.push(pname);
                    }
                }
                ParamOrTsParamProp::Param(p) => {
                    if let deno_ast::swc::ast::Pat::Ident(bind_ident) = &p.pat {
                        let pname = bind_ident.id.sym.to_string();
                        let pty = bind_ident
                            .type_ann
                            .as_ref()
                            .and_then(|ann| crate::types::map_ts_type(&ann.type_ann))
                            .unwrap_or(OatsType::Number);
                        param_types_vec.push(pty);
                        param_names.push(pname);
                    }
                }
            }
        }

        self.fn_param_types
            .borrow_mut()
            .insert(fname.clone(), param_types_vec.clone());

        let mut llvm_param_types: Vec<inkwell::types::BasicMetadataTypeEnum> = Vec::new();
        for pty in &param_types_vec {
            let llvm_ty = match pty {
                OatsType::Number => self.f64_t.into(),
                OatsType::String
                | OatsType::NominalStruct(_)
                | OatsType::StructLiteral(_)
                | OatsType::Array(_)
                | OatsType::Tuple(_)
                | OatsType::Promise(_)
                | OatsType::Weak(_)
                | OatsType::Option(_) => self.i8ptr_t.into(),
                OatsType::Boolean => self.bool_t.into(),
                OatsType::Union(parts) => {
                    let any_ptr = parts.iter().any(|p| {
                        matches!(
                            p,
                            OatsType::String
                                | OatsType::NominalStruct(_)
                                | OatsType::Array(_)
                                | OatsType::Promise(_)
                                | OatsType::Weak(_)
                                | OatsType::Option(_)
                        )
                    });
                    if any_ptr {
                        self.i8ptr_t.into()
                    } else {
                        self.f64_t.into()
                    }
                }
                OatsType::Void => continue,
            };
            llvm_param_types.push(llvm_ty);
        }

        // We'll emit two functions:
        // - `<Class>_init(this, ...params)` which assumes `this` is already
        //    allocated and stores parameters into fields (used by `super(...)`)
        // - `<Class>_ctor(...params)` which allocates the object and then
        //    calls `<Class>_init` to perform initialization and finally
        //    returns the allocated object.

        let init_param_types = std::iter::once(self.i8ptr_t.into())
            .chain(llvm_param_types.iter().cloned())
            .collect::<Vec<_>>();

        let init_fn_ty = self.i8ptr_t.fn_type(&init_param_types, false);
        let init_f = self.module.add_function(&init_name, init_fn_ty, None);

        let ctor_fn_ty = self.i8ptr_t.fn_type(&llvm_param_types, false);
        let f = self.module.add_function(&fname, ctor_fn_ty, None);

        // Emit init function body first
        let init_entry = self.context.append_basic_block(init_f, "entry");
        self.builder.position_at_end(init_entry);

        // Merge constructor parameter properties (TS shorthand `constructor(public x: T)`) into
        // the effective field list so they become true object fields even if not
        // listed in the declared `fields` slice passed in. This ensures the
        // constructor will allocate storage and initialize those fields.
        let mut combined_fields: Vec<(String, crate::types::OatsType)> = fields.to_vec();
        // Detect parameter properties by comparing the ctor param list. We
        // previously collected `param_names` and `param_types_vec`; reconstruct
        // which params were declared as `TsParamProp` by checking the AST again
        // (ctor.params). For simplicity, if a parameter name matches an
        // existing field we leave it alone; otherwise append it.
        for (i, pname) in param_names.iter().enumerate() {
            // If the pname is already a declared field, skip
            if combined_fields.iter().any(|(n, _)| n == pname) {
                continue;
            }
            // Otherwise, this parameter becomes a field (TS param-property)
            // and we use the resolved parameter type as its field type.
            combined_fields.push((pname.clone(), param_types_vec[i].clone()));
        }

        // Layout: [ header (8) | metadata ptr (8) | field0 (8) | field1 (8) | ... ]
        // Reserve an 8-byte metadata slot immediately after the header so the
        // runtime can store a pointer to the class field_map at offset 8.
        let header_size = 8u64;
        let meta_slot = 8u64; // metadata pointer word after header
        let field_count = combined_fields.len();
        let total_size = header_size + meta_slot + (field_count as u64 * 8);

        let malloc_fn = self.get_malloc();
        let size_const = self.i64_t.const_int(total_size, false);
        let call_site = self
            .builder
            .build_call(malloc_fn, &[size_const.into()], "call_malloc")
            .map_err(|_| crate::diagnostics::Diagnostic::simple("build_call failed"))?;
        let malloc_ret = call_site
            .try_as_basic_value()
            .left()
            .ok_or_else(|| {
                crate::diagnostics::Diagnostic::simple("malloc call did not return a value")
            })?
            .into_pointer_value();

        let header_ptr = self
            .builder
            .build_pointer_cast(malloc_ret, self.i8ptr_t, "hdr_ptr")
            .map_err(|_| crate::diagnostics::Diagnostic::simple("pointer cast failed"))?;
        // Set header: rc=1 and type_tag=2 (class with field_map)
        // The header is a packed 64-bit value. Here we set the strong refcount
        // to 1 in the low bits and set a type tag in the high bits so the
        // runtime can recognize class objects quickly. The magic bit layout
        // is defined in the runtime; we mirror its expectations here.
        let type_tag_val = 2u64 << 49;
        let header_val = self.i64_t.const_int(type_tag_val | 1u64, false);
        // Store the header as an i64 at the object base. We intentionally cast
        // the malloc return to an i8* then store the i64 header at offset 0.
        let _ = self.builder.build_store(header_ptr, header_val);

        // For the init function, `this` is the first parameter; set up locals and store params
        let mut locals: LocalsStackLocal = vec![];
        let mut scope = HashMap::new();

        let this_param = init_f.get_nth_param(0).ok_or_else(|| {
            crate::diagnostics::Diagnostic::simple("missing `this` parameter for init")
        })?;
        let this_alloca = self
            .builder
            .build_alloca(self.i8ptr_t, "this")
            .map_err(|_| crate::diagnostics::Diagnostic::simple("alloca failed for this"))?;
        let _ = self.builder.build_store(this_alloca, this_param);
        scope.insert(
            "this".to_string(),
            (
                this_alloca,
                self.i8ptr_t.into(),
                true,
                true,
                false,
                Some(class_name.to_string()),
            ),
        );

        // Zero-initialize all field slots to null so subsequent loads aren't garbage.
        for field_idx in 0..field_count {
            let field_offset = header_size + meta_slot + (field_idx as u64 * 8);
            let obj_ptr_int = self
                .builder
                .build_ptr_to_int(malloc_ret, self.i64_t, "obj_addr_for_zero")
                .map_err(|_| crate::diagnostics::Diagnostic::simple("ptr_to_int failed"))?;
            let off_const = self.i64_t.const_int(field_offset, false);
            let field_addr = self
                .builder
                .build_int_add(obj_ptr_int, off_const, "field_addr_zero")
                .map_err(|_| crate::diagnostics::Diagnostic::simple("int_add failed"))?;
            let field_ptr_cast = self
                .builder
                .build_int_to_ptr(field_addr, self.i8ptr_t, "field_ptr_zero")
                .map_err(|_| crate::diagnostics::Diagnostic::simple("int_to_ptr failed"))?;
            let null_ptr = self.i8ptr_t.const_null();
            // We store a null i8* into the field slot. For non-pointer fields
            // (e.g. numbers) the representation in object fields is still an
            // i8* when the field is pointer-like; numeric fields are stored
            // directly in registers during computation and only boxed when
            // necessary (by unions). Zero-init prevents uninitialised reads
            // during early phase of constructor execution.
            let _ = self
                .builder
                .build_store(field_ptr_cast, null_ptr.as_basic_value_enum());
        }

        let mut param_map: HashMap<String, u32> = HashMap::new();

        for (i, pname) in param_names.iter().enumerate() {
            // In init function, parameter indices shift by +1 due to `this`
            let param_val = init_f.get_nth_param((i + 1) as u32).ok_or_else(|| {
                crate::diagnostics::Diagnostic::simple(format!(
                    "missing parameter {} for constructor {}",
                    pname, class_name
                ))
            })?;
            let param_ty = param_val.get_type();
            let alloca = self
                .builder
                .build_alloca(param_ty, &format!("param_{}", pname))
                .map_err(|_| {
                    crate::diagnostics::Diagnostic::simple(format!(
                        "alloca failed for param {} in constructor {}",
                        pname, class_name
                    ))
                })?;
            let _ = self.builder.build_store(alloca, param_val);
            // Mark parameter as weak if its declared OatsType is Weak(_)
            let is_param_weak = matches!(param_types_vec.get(i), Some(OatsType::Weak(_)));
            // If the declared param type is a NominalStruct, record its name so
            // member lowering can infer fields without fallback heuristics.
            let nominal = match param_types_vec.get(i) {
                Some(crate::types::OatsType::NominalStruct(n)) => Some(n.clone()),
                _ => None,
            };
            scope.insert(
                pname.clone(),
                (alloca, param_ty, true, true, is_param_weak, nominal),
            );
            param_map.insert(pname.clone(), i as u32);
        }

        locals.push(scope);

        for (field_idx, (field_name, _field_type)) in combined_fields.iter().enumerate() {
            if let Some(param_idx) = param_names.iter().position(|pn| pn == field_name) {
                let param_val = init_f
                    .get_nth_param((param_idx + 1) as u32)
                    .ok_or_else(|| {
                        crate::diagnostics::Diagnostic::simple(format!(
                            "missing parameter {} for constructor {}",
                            field_name, class_name
                        ))
                    })?;
                let field_offset = header_size + meta_slot + (field_idx as u64 * 8);
                let field_ptr_int = self
                    .builder
                    .build_ptr_to_int(malloc_ret, self.i64_t, "obj_addr")
                    .map_err(|_| crate::diagnostics::Diagnostic::simple("ptr_to_int failed"))?;
                let offset_const = self.i64_t.const_int(field_offset, false);
                let field_addr = self
                    .builder
                    .build_int_add(field_ptr_int, offset_const, "field_addr")
                    .map_err(|_| crate::diagnostics::Diagnostic::simple("int_add failed"))?;
                let field_ptr_cast = self
                    .builder
                    .build_int_to_ptr(field_addr, self.i8ptr_t, "field_ptr")
                    .map_err(|_| crate::diagnostics::Diagnostic::simple("int_to_ptr failed"))?;
                // If the field type is a union, we expect to store a boxed union object.
                match _field_type {
                    OatsType::Union(_) => {
                        // If the incoming param is a float, box it; if it's already a pointer, box via union_box_ptr.
                        // We do a small ABI-aware decision here: the constructor
                        // ABI may pass unions as either a float or pointer. If
                        // the parameter's LLVM type is a float then we must
                        // allocate a union container for the f64 and return a
                        // pointer to it (`union_box_f64`). Otherwise, if the
                        // ABI passed an i8* we call `union_box_ptr` which will
                        // wrap the existing pointer in a union tag. The
                        // runtime's union_box_* helpers return an owned pointer
                        // (with RC=1) which we then store into the object and
                        // immediately `rc_inc` to reflect the object's strong
                        // reference to it.
                        if param_val.get_type().is_float_type() {
                            let unboxed_f = param_val.into_float_value();
                            let box_fn = self.get_union_box_f64();
                            let cs = self.builder.build_call(
                                box_fn,
                                &[unboxed_f.into()],
                                "union_box_f64_ctor",
                            );
                            if let Ok(cs) = cs
                                && let inkwell::Either::Left(bv) = cs.try_as_basic_value()
                            {
                                let boxed_ptr = bv.into_pointer_value();
                                let boxed_bv =
                                    inkwell::values::BasicValueEnum::PointerValue(boxed_ptr);
                                let _ = self.builder.build_store(field_ptr_cast, boxed_bv);
                                // For union-boxed pointers we treat the stored pointer as strong by default.
                                // If the declared field type within the union is Weak, more refined handling
                                // The weak/strong distinction will be added later.
                                // Use the strong `rc_inc` path for current allocations.
                                let rc_inc_fn = self.get_rc_inc();
                                let _ = self.builder.build_call(
                                    rc_inc_fn,
                                    &[boxed_ptr.into()],
                                    "rc_inc_field",
                                );
                            }
                        } else if param_val.get_type().is_pointer_type() {
                            // box the pointer payload
                            let box_fn = self.get_union_box_ptr();
                            let cs = self.builder.build_call(
                                box_fn,
                                &[param_val.into()],
                                "union_box_ptr_ctor",
                            );
                            if let Ok(cs) = cs
                                && let inkwell::Either::Left(bv) = cs.try_as_basic_value()
                            {
                                let boxed_ptr = bv.into_pointer_value();
                                let boxed_bv =
                                    inkwell::values::BasicValueEnum::PointerValue(boxed_ptr);
                                let _ = self.builder.build_store(field_ptr_cast, boxed_bv);
                                let rc_inc_fn = self.get_rc_inc();
                                let _ = self.builder.build_call(
                                    rc_inc_fn,
                                    &[boxed_ptr.into()],
                                    "rc_inc_field",
                                );
                            }
                        } else {
                            // other param ABI not expected for unions
                            let _ = self.builder.build_store(field_ptr_cast, param_val);
                        }
                    }
                    _ => {
                        let _ = self.builder.build_store(field_ptr_cast, param_val);

                        if param_val.get_type().is_pointer_type() {
                            // If the declared field type is Weak<T>, use rc_weak_inc; otherwise rc_inc
                            match _field_type {
                                OatsType::Weak(_) => {
                                    let rc_weak_inc = self.get_rc_weak_inc();
                                    let _ = self.builder.build_call(
                                        rc_weak_inc,
                                        &[param_val.into()],
                                        "rc_weak_inc_field",
                                    );
                                }
                                _ => {
                                    let rc_inc_fn = self.get_rc_inc();
                                    let _ = self.builder.build_call(
                                        rc_inc_fn,
                                        &[param_val.into()],
                                        "rc_inc_field",
                                    );
                                }
                            }
                        }
                    }
                }
            }
        }

        // Emit metadata: list of byte offsets for pointer-like fields so the runtime
        // can traverse object pointer fields. This is a simple array of i64 offsets
        // named `<class_name>_field_map` and will be used by the cycle collector.
        {
            let mut ptr_field_offsets: Vec<inkwell::values::BasicValueEnum> = Vec::new();
            for (field_idx, (_field_name, field_type)) in combined_fields.iter().enumerate() {
                // Determine if this field is pointer-like
                let is_ptr = matches!(
                    field_type,
                    crate::types::OatsType::String
                        | crate::types::OatsType::NominalStruct(_)
                        | crate::types::OatsType::Array(_)
                        | crate::types::OatsType::Promise(_)
                        | crate::types::OatsType::Weak(_)
                        | crate::types::OatsType::Option(_)
                        | crate::types::OatsType::Union(_)
                );
                if is_ptr {
                    let offset = header_size + meta_slot + (field_idx as u64 * 8);
                    let const_off = self.i64_t.const_int(offset, false);
                    ptr_field_offsets.push(const_off.as_basic_value_enum());
                }
            }

            // Create global struct constant containing length + offsets if there are any pointer fields
            if !ptr_field_offsets.is_empty() {
                let len = ptr_field_offsets.len();
                let _arr_ty = self.i64_t.array_type(len as u32);
                // Convert BasicValueEnum::IntValue items into IntValue vector
                let int_vals: Vec<inkwell::values::IntValue> = ptr_field_offsets
                    .into_iter()
                    .map(|bv| bv.into_int_value())
                    .collect();

                // Build a struct type: { i64 meta0, [N x i32] }
                // meta0 packs magic/version in high 32 bits and len in low 32 bits.
                // The array contains i32 offsets (relative to object base) for
                // each pointer-like field. The runtime will use this table to
                // walk object fields during cycle collection and serialization.
                let arr_i32_ty = self.context.i32_type().array_type(len as u32);
                let struct_ty = self
                    .context
                    .struct_type(&[self.i64_t.into(), arr_i32_ty.into()], false);

                let magic_u64 = 0x4F415453u64; // 'OATS'
                let meta0_val = (magic_u64 << 32) | (len as u64 & 0xffffffffu64);
                let meta0_const = self.i64_t.const_int(meta0_val, false);

                // Convert offsets to i32 constants
                let int32_vals: Vec<inkwell::values::IntValue> = int_vals
                    .into_iter()
                    .map(|iv| {
                        // Try to extract a constant; fall back to zero if not available.
                        let v = iv.get_zero_extended_constant().unwrap_or(0);
                        self.i32_t.const_int(v & 0xffffffffu64, false)
                    })
                    .collect();
                let array_const = self.i32_t.const_array(&int32_vals);
                let initializer = self
                    .context
                    .const_struct(&[meta0_const.into(), array_const.into()], false);

                let gv_name = format!("{}_field_map", class_name);
                let gv = self.module.add_global(struct_ty, None, &gv_name);
                gv.set_initializer(&initializer);
                gv.set_constant(true);

                // Store pointer to the field_map global into the object's second word
                // Compute i8* pointer to the global and store at offset +8 from object base
                if let Some(gv_global) = self.module.get_global(&gv_name) {
                    let gv_ptr = gv_global.as_pointer_value();
                    // cast global pointer to i8*
                    if let Ok(gv_i8ptr) =
                        self.builder
                            .build_pointer_cast(gv_ptr, self.i8ptr_t, "field_map_i8ptr")
                    {
                        // compute field slot address: ptr_to_int(malloc_ret) + 8 -> int_to_ptr(i8*)
                        let obj_ptr_int = self
                            .builder
                            .build_ptr_to_int(malloc_ret, self.i64_t, "obj_addr_for_meta")
                            .map_err(|_| {
                                crate::diagnostics::Diagnostic::simple("ptr_to_int failed")
                            })?;
                        let off_const = self.i64_t.const_int(8, false);
                        let field_addr = self
                            .builder
                            .build_int_add(obj_ptr_int, off_const, "meta_field_addr")
                            .map_err(|_| {
                                crate::diagnostics::Diagnostic::simple("int_add failed")
                            })?;
                        let field_ptr = self
                            .builder
                            .build_int_to_ptr(field_addr, self.i8ptr_t, "meta_field_ptr")
                            .map_err(|_| {
                                crate::diagnostics::Diagnostic::simple("int_to_ptr failed")
                            })?;
                        // store pointer value
                        let _ = self
                            .builder
                            .build_store(field_ptr, gv_i8ptr.as_basic_value_enum());
                    }
                }
            }
        }

        // Lower ctor body inside init function context (so `super(...)` can call parent init)
        if let Some(body) = &ctor.body {
            for stmt in &body.stmts {
                let _ = self.lower_stmt(stmt, init_f, &param_map, &mut locals);
            }
        }

        // init returns void (performing stores into this), but we keep signature as i8* to
        // match callers. Return `this` at end of init.
        let this_loaded = match self
            .builder
            .build_load(self.i8ptr_t, this_alloca, "this_ld")
        {
            Ok(v) => v,
            Err(_) => {
                return Err(crate::diagnostics::Diagnostic::simple(
                    "failed to load this",
                ));
            }
        };
        let _ = self.builder.build_return(Some(&this_loaded));

        // Now emit ctor function: allocate object then call init
        let ctor_entry = self.context.append_basic_block(f, "entry");
        self.builder.position_at_end(ctor_entry);
        // allocate object
        let malloc_fn = self.get_malloc();
        let size_const = self.i64_t.const_int(total_size, false);
        let call_site2 = self
            .builder
            .build_call(malloc_fn, &[size_const.into()], "call_malloc")
            .map_err(|_| crate::diagnostics::Diagnostic::simple("build_call failed"))?;
        let malloc_ret2 = call_site2
            .try_as_basic_value()
            .left()
            .ok_or_else(|| {
                crate::diagnostics::Diagnostic::simple("malloc call did not return a value")
            })?
            .into_pointer_value();

        // store header
        let header_ptr = self
            .builder
            .build_pointer_cast(malloc_ret2, self.i8ptr_t, "hdr_ptr")
            .map_err(|_| crate::diagnostics::Diagnostic::simple("pointer cast failed"))?;
        let header_val = self.i64_t.const_int(2u64 << 49 | 1u64, false);
        let _ = self.builder.build_store(header_ptr, header_val);

        // call init(this, ...) - prepare args
        let mut init_args: Vec<inkwell::values::BasicMetadataValueEnum> = Vec::new();
        init_args.push(malloc_ret2.as_basic_value_enum().into());
        for i in 0..llvm_param_types.len() {
            if let Some(p) = f.get_nth_param(i as u32) {
                init_args.push(p.into());
            } else {
                // pad with null
                init_args.push(self.i8ptr_t.const_null().as_basic_value_enum().into());
            }
        }
        let _ = self
            .builder
            .build_call(init_f, &init_args, "call_init")
            .map_err(|_| crate::diagnostics::Diagnostic::simple("init call failed"))?;

        let _ = self
            .builder
            .build_return(Some(&malloc_ret2.as_basic_value_enum()));
        Ok(())
    }
}
