//! Async function handling for code generation.
//!
//! This module contains code generation logic for async functions, including
//! live-set computation for await points and async function lowering.

use crate::diagnostics::Severity;
use inkwell::values::{BasicValue, FunctionValue};
use oats_ast::*;
use std::collections::{HashMap, HashSet};

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
    pub fn compute_await_live_sets(&self, func: &Function) -> Vec<HashSet<String>> {
        enum Node {
            Ident(String),
            AwaitMarker,
        }

        fn flatten_expr(e: &Expr, out: &mut Vec<Node>) {
            match e {
                Expr::Ident(id) => out.push(Node::Ident(id.sym.clone())),
                Expr::Call(c) => {
                    if let Callee::Expr(ec) = &c.callee {
                        flatten_expr(ec, out);
                    }
                    for a in &c.args {
                        flatten_expr(a, out);
                    }
                }
                Expr::Member(m) => {
                    flatten_expr(&m.obj, out);
                    if let MemberProp::Computed(cmp) = &m.prop {
                        flatten_expr(cmp, out);
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
                        flatten_expr(el, out);
                    }
                }
                Expr::New(n) => {
                    flatten_expr(&n.callee, out);
                    for a in &n.args {
                        flatten_expr(a, out);
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

        fn flatten_stmt(s: &Stmt, out: &mut Vec<Node>) {
            match s {
                Stmt::ExprStmt(es) => flatten_expr(&es.expr, out),
                Stmt::Return(r) => {
                    if let Some(arg) = &r.arg {
                        flatten_expr(arg, out);
                    }
                }
                Stmt::VarDecl(vd) => {
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
                        Stmt::ExprStmt(e) => flatten_expr(&e.expr, out),
                        Stmt::Block(b) => {
                            for st in &b.stmts {
                                flatten_stmt(st, out);
                            }
                        }
                        _ => {}
                    }
                    if let Some(alt) = &ifst.alt {
                        match alt.as_ref() {
                            Stmt::ExprStmt(e) => flatten_expr(&e.expr, out),
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
                            ForInit::Expr(e) => flatten_expr(e, out),
                            ForInit::VarDecl(vd) => {
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

    /// Generate async function wrapper and poll function.
    ///
    /// This function handles the async-specific code generation for functions
    /// marked as async. It creates a poll function and a wrapper that returns
    /// a Promise.
    #[allow(clippy::result_large_err)]
    pub fn gen_async_function_ir(
        &self,
        func_name: &str,
        func_decl: &Function,
        llvm_param_types: &[inkwell::types::BasicMetadataTypeEnum<'a>],
        ret_type: &crate::types::OatsType,
        function: FunctionValue<'a>,
        entry: inkwell::basic_block::BasicBlock<'a>,
    ) -> crate::diagnostics::DiagnosticResult<FunctionValue<'a>> {
        use crate::types::OatsType;

        // Track that this compilation uses async features
        self.uses_async.set(true);

        // Expect declared return to be `Promise(inner)`
        let inner_ret = if let OatsType::Promise(inner) = ret_type {
            inner.clone()
        } else {
            return Err(crate::diagnostics::Diagnostic::simple_boxed(
                Severity::Error,
                "async function must declare a Promise<T> return type",
            ));
        };

        // Create a non-async clone of the function AST for processing
        let mut impl_decl = func_decl.clone();
        impl_decl.is_async = false;

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
        // emitting the poll body.
        let param_count = llvm_param_types.len();
        let await_live_sets = self.compute_await_live_sets(&impl_decl);
        let mut all_live_names: HashSet<String> = HashSet::new();
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

        // CRITICAL: Set up async lowering context BEFORE generating poll function body
        // Build a name -> slot index map for locals. Locals occupy
        // slots after the parameter slots.
        let mut local_name_to_slot: HashMap<String, usize> = HashMap::new();
        let mut names: Vec<String> = all_live_names.clone().into_iter().collect();
        names.sort();
        let mut idx = param_count;
        for name in names.into_iter() {
            local_name_to_slot.insert(name, idx);
            idx += 1;
        }

        // Store async lowering context in CodeGen BEFORE generating poll body
        self.async_await_live_sets
            .borrow_mut()
            .replace(await_live_sets.clone());
        self.async_local_name_to_slot
            .borrow_mut()
            .replace(local_name_to_slot.clone());
        self.async_resume_blocks
            .borrow_mut()
            .replace(resume_blocks.clone());
        self.async_cont_blocks
            .borrow_mut()
            .replace(cont_blocks.clone());
        self.async_poll_function.borrow_mut().replace(poll_f);
        self.async_await_counter.set(0);
        self.async_param_count.set(param_count as u32);

        // Now emit the poll body entry: read the state field (u32 at offset +8)
        // and dispatch based on it. Layout: [poll_fn_ptr (8)] [state_u32 (4) + pad(4)] [param slots ...]
        // If state == 0 => call impl (first run). If state matches a resume
        // index (1-based) we will jump into the corresponding resume block.
        // Otherwise the promise is already completed and we return ready.
        let state_param = poll_f.get_nth_param(0).ok_or_else(|| {
            crate::diagnostics::Diagnostic::simple_boxed(
                Severity::Error,
                "poll function missing state parameter",
            )
        })?;
        let state_ptr = state_param.into_pointer_value();

        // Read state u32 at offset 8
        let state_addr_int =
            match self
                .builder
                .build_ptr_to_int(state_ptr, self.i64_t, "state_addr")
            {
                Ok(v) => v,
                Err(_) => {
                    return Err(crate::diagnostics::Diagnostic::simple_boxed(
                        Severity::Error,
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
                    return Err(crate::diagnostics::Diagnostic::simple_boxed(
                        Severity::Error,
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
                return Err(crate::diagnostics::Diagnostic::simple_boxed(
                    Severity::Error,
                    "int_to_ptr failed in poll",
                ));
            }
        };
        let state_loaded = match self
            .builder
            .build_load(self.i32_t, state_field_ptr, "state_ld")
        {
            Ok(v) => v.into_int_value(),
            Err(_) => {
                return Err(crate::diagnostics::Diagnostic::simple_boxed(
                    Severity::Error,
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
                return Err(crate::diagnostics::Diagnostic::simple_boxed(
                    Severity::Error,
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
                return Err(crate::diagnostics::Diagnostic::simple_boxed(
                    Severity::Error,
                    "failed to build conditional branch in poll",
                ));
            }
        }

        // run_impl: Lower the function body with async context active.
        // This allows await expressions to properly save/restore state.
        self.builder.position_at_end(run_impl_bb);

        // Create param allocas for poll_f and load parameters from state slots
        // We need to build a param_map with u32 indices for lower_stmts
        let mut param_map: HashMap<String, u32> = HashMap::new();

        type LocalEntry<'a> = (
            inkwell::values::PointerValue<'a>,
            inkwell::types::BasicTypeEnum<'a>,
            bool,                           // is_mutable
            bool,                           // is_param
            bool,                           // is_weak
            Option<String>,                 // nominal type name
            Option<crate::types::OatsType>, // oats_type for unions
        );
        let mut locals_stack: Vec<HashMap<String, LocalEntry<'a>>> = vec![HashMap::new()];

        // Load each parameter from state and create allocas
        for (i, param) in func_decl.params.iter().enumerate() {
            let param_name = match &param.pat {
                Pat::Ident(id) => id.sym.clone(),
                _ => {
                    // Destructuring parameters not yet supported in codegen
                    format!("_param_{}", i)
                }
            };

            let llvm_ty_meta = llvm_param_types[i];
            let llvm_ty: inkwell::types::BasicTypeEnum = match llvm_ty_meta {
                inkwell::types::BasicMetadataTypeEnum::FloatType(ft) => ft.into(),
                inkwell::types::BasicMetadataTypeEnum::IntType(it) => it.into(),
                inkwell::types::BasicMetadataTypeEnum::PointerType(pt) => pt.into(),
                inkwell::types::BasicMetadataTypeEnum::ArrayType(at) => at.into(),
                inkwell::types::BasicMetadataTypeEnum::VectorType(vt) => vt.into(),
                inkwell::types::BasicMetadataTypeEnum::StructType(st) => st.into(),
                inkwell::types::BasicMetadataTypeEnum::ScalableVectorType(svt) => svt.into(),
                inkwell::types::BasicMetadataTypeEnum::MetadataType(_) => {
                    // Metadata types can't be used for allocas, default to i8*
                    self.i8ptr_t.into()
                }
            };

            // Create alloca for this parameter
            let alloca = match self.builder.build_alloca(llvm_ty, &param_name) {
                Ok(a) => a,
                Err(_) => {
                    return Err(crate::diagnostics::Diagnostic::simple_boxed(
                        Severity::Error,
                        "failed to create parameter alloca in poll",
                    ));
                }
            };

            // Load value from state slot
            let base_int = match self
                .builder
                .build_ptr_to_int(state_ptr, self.i64_t, "state_addr")
            {
                Ok(v) => v,
                Err(_) => {
                    return Err(crate::diagnostics::Diagnostic::simple_boxed(
                        Severity::Error,
                        "ptr_to_int failed loading param",
                    ));
                }
            };
            let off = self.i64_t.const_int(16 + (i as u64 * 8), false);
            let slot_addr_int = match self.builder.build_int_add(base_int, off, "slot_addr") {
                Ok(v) => v,
                Err(_) => {
                    return Err(crate::diagnostics::Diagnostic::simple_boxed(
                        Severity::Error,
                        "int_add failed loading param",
                    ));
                }
            };
            let slot_ptr =
                match self
                    .builder
                    .build_int_to_ptr(slot_addr_int, self.i8ptr_t, "slot_ptr")
                {
                    Ok(p) => p,
                    Err(_) => {
                        return Err(crate::diagnostics::Diagnostic::simple_boxed(
                            Severity::Error,
                            "int_to_ptr failed loading param",
                        ));
                    }
                };

            // Load and unbox if needed
            let loaded_ptr = match self
                .builder
                .build_load(self.i8ptr_t, slot_ptr, "param_load")
            {
                Ok(v) => v,
                Err(_) => {
                    return Err(crate::diagnostics::Diagnostic::simple_boxed(
                        Severity::Error,
                        "failed to load param from state",
                    ));
                }
            };

            match llvm_ty {
                inkwell::types::BasicTypeEnum::FloatType(_) => {
                    let unbox_fn = self.get_union_unbox_f64();
                    let cs =
                        match self
                            .builder
                            .build_call(unbox_fn, &[loaded_ptr.into()], "unbox_param")
                        {
                            Ok(cs) => cs,
                            Err(_) => {
                                return Err(crate::diagnostics::Diagnostic::simple_boxed(
                                    Severity::Error,
                                    "union_unbox_f64 call failed",
                                ));
                            }
                        };
                    let fv = cs
                        .try_as_basic_value()
                        .left()
                        .ok_or_else(|| {
                            crate::diagnostics::Diagnostic::simple_boxed(
                                Severity::Error,
                                "unbox returned no value",
                            )
                        })?
                        .into_float_value();
                    let _ = self.builder.build_store(alloca, fv.as_basic_value_enum());
                }
                _ => {
                    let slot_ptr_val = loaded_ptr.into_pointer_value();
                    let casted = match self.builder.build_pointer_cast(
                        slot_ptr_val,
                        alloca.get_type(),
                        "cast_param",
                    ) {
                        Ok(c) => c,
                        Err(_) => {
                            return Err(crate::diagnostics::Diagnostic::simple_boxed(
                                Severity::Error,
                                "pointer cast failed loading param",
                            ));
                        }
                    };
                    let loaded_val = match self.builder.build_load(
                        llvm_ty,
                        casted,
                        &format!("{}_val", param_name),
                    ) {
                        Ok(v) => v,
                        Err(_) => {
                            return Err(crate::diagnostics::Diagnostic::simple_boxed(
                                Severity::Error,
                                "failed to load casted param",
                            ));
                        }
                    };
                    let _ = self.builder.build_store(alloca, loaded_val);
                }
            }

            param_map.insert(param_name.clone(), i as u32);
            locals_stack[0].insert(
                param_name,
                (alloca, llvm_ty, false, true, false, None, None),
            );
        }

        // Keep a copy of the poll function's locals stack so resume blocks
        // can restore saved values into the same allocas later.
        *self.async_poll_locals.borrow_mut() = Some(locals_stack.clone());

        // Lower the body statements with async context active
        let mut emitted_terminator = false;
        if let Some(body) = &func_decl.body {
            emitted_terminator =
                self.lower_stmts(&body.stmts, poll_f, &param_map, &mut locals_stack)?;
        }

        // If no explicit return, write result and return Ready (1)
        if !emitted_terminator
            && self
                .builder
                .get_insert_block()
                .is_none_or(|b| b.get_terminator().is_none())
        {
            self.emit_rc_dec_for_locals(&locals_stack);

            // Get the out_ptr parameter
            let out_param = poll_f.get_nth_param(1).ok_or_else(|| {
                crate::diagnostics::Diagnostic::simple_boxed(
                    Severity::Error,
                    "poll function missing out parameter",
                )
            })?;
            let out_ptr_cast = self
                .builder
                .build_pointer_cast(out_param.into_pointer_value(), self.i8ptr_t, "out_cast")
                .map_err(|_| {
                    crate::diagnostics::Diagnostic::simple_boxed(
                        Severity::Error,
                        "pointer cast failed",
                    )
                })?;

            // Store result based on return type
            let payload_bv = if matches!(*inner_ret, OatsType::Void) {
                let nullp = self.i8ptr_t.const_null();
                nullp.as_basic_value_enum()
            } else {
                // For implicit return, use default value
                match *inner_ret {
                    OatsType::Number => {
                        let zero = self.f64_t.const_float(0.0);
                        let box_fn = self.get_union_box_f64();
                        let cs = self
                            .builder
                            .build_call(box_fn, &[zero.into()], "box_default")
                            .map_err(|_| {
                                crate::diagnostics::Diagnostic::simple_boxed(
                                    Severity::Error,
                                    "boxing failed",
                                )
                            })?;
                        cs.try_as_basic_value().left().ok_or_else(|| {
                            crate::diagnostics::Diagnostic::simple_boxed(
                                Severity::Error,
                                "boxing returned no value",
                            )
                        })?
                    }
                    _ => {
                        let nullp = self.i8ptr_t.const_null();
                        nullp.as_basic_value_enum()
                    }
                }
            };

            let _ = self.builder.build_store(out_ptr_cast, payload_bv);

            // Set state to done (1)
            let one_i32 = self.i32_t.const_int(1, false);
            let _ = self.builder.build_store(state_field_ptr, one_i32);

            // Return Ready (1)
            let one = self.i32_t.const_int(1, false);
            self.builder
                .build_return(Some(&one.as_basic_value_enum()))
                .map_err(|_| {
                    crate::diagnostics::Diagnostic::simple_boxed(
                        Severity::Error,
                        "Failed to build return",
                    )
                })?;
        }

        // Move builder back to the wrapper function entry so subsequent
        // allocations and stores are emitted into the exported wrapper
        // rather than the poll function.
        self.builder.position_at_end(entry);

        // Now create a heap-allocated state object and store the poll fn ptr at offset 0
        // State layout: [poll_fn_ptr (8)] [reserved 8 bytes] [param slots 8 bytes each ...]
        let malloc_fn = self.get_malloc();
        // Compute per-await live-sets so we can reserve state slots for
        // locals that must be preserved across suspension points.
        let param_count = llvm_param_types.len();
        let await_live_sets = self.compute_await_live_sets(&impl_decl);
        let mut all_live_names: HashSet<String> = HashSet::new();
        for s in &await_live_sets {
            for n in s.iter() {
                all_live_names.insert(n.clone());
            }
        }
        // Reserve one 8-byte slot per live local name (after param slots)
        let local_slot_count = all_live_names.len();
        // Store for later use in expr.rs
        self.async_local_slot_count.set(local_slot_count);
        // Also reserve one slot per await point to store the awaited promise
        let await_promise_slot_count = await_live_sets.len();
        let total_slots = param_count + local_slot_count + await_promise_slot_count;
        let state_size_bytes = 16u64 + (total_slots as u64 * 8u64);
        let state_size = self.i64_t.const_int(state_size_bytes, false);
        let cs = match self
            .builder
            .build_call(malloc_fn, &[state_size.into()], "malloc_state")
        {
            Ok(cs) => cs,
            Err(_) => {
                return Err(crate::diagnostics::Diagnostic::simple_boxed(
                    Severity::Error,
                    "malloc failed for state",
                ));
            }
        };
        let state_ptr_wrapper = cs
            .try_as_basic_value()
            .left()
            .ok_or_else(|| {
                crate::diagnostics::Diagnostic::simple_boxed(
                    Severity::Error,
                    "malloc failed for state",
                )
            })?
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
                    return Err(crate::diagnostics::Diagnostic::simple_boxed(
                        Severity::Error,
                        "pointer cast failed",
                    ));
                }
            };
        let _ = self
            .builder
            .build_store(state_ptr_wrapper, poll_ptr_i8.as_basic_value_enum());

        // Store each incoming parameter into the state slots starting at offset 16
        for (i, pty) in llvm_param_types.iter().enumerate().take(param_count) {
            if let Some(arg_val) = function.get_nth_param(i as u32) {
                let slot_offset = 16 + (i as u64 * 8);
                let obj_ptr_int = match self.builder.build_ptr_to_int(
                    state_ptr_wrapper,
                    self.i64_t,
                    &format!("state_addr_store_{}", i),
                ) {
                    Ok(v) => v,
                    Err(_) => {
                        return Err(crate::diagnostics::Diagnostic::simple_boxed(
                            Severity::Error,
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
                        return Err(crate::diagnostics::Diagnostic::simple_boxed(
                            Severity::Error,
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
                        return Err(crate::diagnostics::Diagnostic::simple_boxed(
                            Severity::Error,
                            "int_to_ptr failed when storing captures",
                        ));
                    }
                };

                // If the function parameter ABI is a float, box it.
                let pty_basic: inkwell::types::BasicTypeEnum = match *pty {
                    inkwell::types::BasicMetadataTypeEnum::FloatType(ft) => ft.into(),
                    inkwell::types::BasicMetadataTypeEnum::IntType(it) => it.into(),
                    inkwell::types::BasicMetadataTypeEnum::PointerType(pt) => pt.into(),
                    inkwell::types::BasicMetadataTypeEnum::ArrayType(at) => at.into(),
                    inkwell::types::BasicMetadataTypeEnum::VectorType(vt) => vt.into(),
                    inkwell::types::BasicMetadataTypeEnum::StructType(st) => st.into(),
                    inkwell::types::BasicMetadataTypeEnum::ScalableVectorType(svt) => svt.into(),
                    inkwell::types::BasicMetadataTypeEnum::MetadataType(_) => {
                        // Metadata types can't be used here, default to i8*
                        self.i8ptr_t.into()
                    }
                };
                match pty_basic {
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
                                return Err(crate::diagnostics::Diagnostic::simple_boxed(
                                    Severity::Error,
                                    "union_box_f64 call failed when storing captures",
                                ));
                            }
                        };
                        let boxed_ptr = cs
                            .try_as_basic_value()
                            .left()
                            .ok_or_else(|| {
                                crate::diagnostics::Diagnostic::simple_boxed(
                                    Severity::Error,
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

        // CRITICAL: Create promise from state and return it from wrapper function
        // The builder is currently positioned in the wrapper function's entry block
        let p_new = self.get_promise_new_from_state();
        let pres_cs = self
            .builder
            .build_call(
                p_new,
                &[state_ptr_wrapper.as_basic_value_enum().into()],
                "promise_new_from_state",
            )
            .map_err(|_| {
                crate::diagnostics::Diagnostic::simple_boxed(
                    Severity::Error,
                    "promise_new_from_state call failed in wrapper",
                )
            })?;
        let pres_val = pres_cs.try_as_basic_value().left().ok_or_else(|| {
            crate::diagnostics::Diagnostic::simple_boxed(
                Severity::Error,
                "promise_new_from_state returned no value in wrapper",
            )
        })?;

        // Enqueue the promise (executor will schedule it)
        let enq = self.get_executor_enqueue();
        let _ = self
            .builder
            .build_call(enq, &[pres_val.into()], "executor_enqueue");

        // Return the promise from the wrapper function
        let _ = self.builder.build_return(Some(&pres_val));

        // Now populate the `already_done` block to either dispatch to
        // resume_i blocks (if state contains a resume index) or return
        // ready immediately when the promise is already completed.
        let _await_count = await_live_sets.len();
        self.builder.position_at_end(done_bb);
        // if state_loaded == 0 -> unreachable here; compare against 1..N
        // We'll build a switch: default -> return ready; cases 1..N -> branch to resume_i
        let default_ret = self.i32_t.const_int(1, false);
        // Create a block for the default case (already completed, return ready)
        let ready_return_bb = self.context.append_basic_block(poll_f, "ready_return");
        // Build the switch instruction on state_loaded and add cases for
        // each resume index (1-based -> resume_0..resume_{N-1}).
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
            .build_switch(state_loaded, ready_return_bb, cases.as_slice())
            .map_err(|_| {
                crate::diagnostics::Diagnostic::simple_boxed(
                    Severity::Error,
                    "failed to build switch in poll",
                )
            })?;

        // Emit resume handlers: each resume block reloads live locals
        // from the state slots into the poll function's allocas and then
        // jumps to the corresponding cont block.
        if let (Some(local_map), Some(live_sets), Some(poll_locals)) = (
            &*self.async_local_name_to_slot.borrow(),
            &*self.async_await_live_sets.borrow(),
            &*self.async_poll_locals.borrow(),
        ) && let (Some(resume_vec), Some(cont_vec)) = (
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
                                let base_int = match self.builder.build_ptr_to_int(
                                    state_ptr,
                                    self.i64_t,
                                    "resume_state_addr",
                                ) {
                                    Ok(v) => v,
                                    Err(_) => {
                                        return Err(crate::diagnostics::Diagnostic::simple_boxed(
                                            Severity::Error,
                                            "ptr_to_int failed when resuming locals",
                                        ));
                                    }
                                };
                                let off_const =
                                    self.i64_t.const_int(16 + (*slot_idx as u64 * 8), false);
                                let slot_addr_int = match self.builder.build_int_add(
                                    base_int,
                                    off_const,
                                    "slot_addr_resume",
                                ) {
                                    Ok(v) => v,
                                    Err(_) => {
                                        return Err(crate::diagnostics::Diagnostic::simple_boxed(
                                            Severity::Error,
                                            "int_add failed when resuming locals",
                                        ));
                                    }
                                };
                                let slot_ptr = match self.builder.build_int_to_ptr(
                                    slot_addr_int,
                                    self.i8ptr_t,
                                    "slot_ptr_resume",
                                ) {
                                    Ok(p) => p,
                                    Err(_) => {
                                        return Err(crate::diagnostics::Diagnostic::simple_boxed(
                                            Severity::Error,
                                            "int_to_ptr failed when resuming locals",
                                        ));
                                    }
                                };
                                let loaded_slot = match self.builder.build_load(
                                    self.i8ptr_t,
                                    slot_ptr,
                                    &format!("slot_load_{}", name),
                                ) {
                                    Ok(v) => v,
                                    Err(_) => {
                                        return Err(crate::diagnostics::Diagnostic::simple_boxed(
                                            Severity::Error,
                                            "failed to load from slot when resuming",
                                        ));
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
                                                    crate::diagnostics::Diagnostic::simple_boxed(
                                                        Severity::Error,
                                                        "union_unbox_f64 call failed when resuming locals",
                                                    ),
                                                );
                                            }
                                        };
                                        let fv = cs
                                            .try_as_basic_value()
                                            .left()
                                            .ok_or_else(|| {
                                                crate::diagnostics::Diagnostic::simple_boxed(
                                                    Severity::Error,
                                                    "unbox returned no value when resuming locals",
                                                )
                                            })?
                                            .into_float_value();
                                        let _ = self
                                            .builder
                                            .build_store(alloca_ptr, fv.as_basic_value_enum());
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
                                                    crate::diagnostics::Diagnostic::simple_boxed(
                                                        Severity::Error,
                                                        "pointer cast failed when resuming locals",
                                                    ),
                                                );
                                            }
                                        };
                                        let _ = self
                                            .builder
                                            .build_store(alloca_ptr, casted.as_basic_value_enum());
                                    }
                                }
                            }
                        }
                    }
                }

                // Before branching to cont, re-poll the awaited promise to get the result
                let poll_f_val = *self.async_poll_function.borrow();
                let poll_f_resume = poll_f_val.ok_or_else(|| {
                    crate::diagnostics::Diagnostic::simple_boxed(
                        Severity::Error,
                        "poll function not set",
                    )
                })?;
                let poll_state_param = poll_f_resume.get_nth_param(0).ok_or_else(|| {
                    crate::diagnostics::Diagnostic::simple_boxed(
                        Severity::Error,
                        "poll function missing state parameter",
                    )
                })?;
                let poll_state_ptr = poll_state_param.into_pointer_value();

                let param_count = llvm_param_types.len();
                let local_slot_count = all_live_names.len();
                let await_slot_offset = 16 + (param_count * 8) + (local_slot_count * 8) + (i * 8);
                let base_int = match self.builder.build_ptr_to_int(
                    poll_state_ptr,
                    self.i64_t,
                    "resume_state_addr_promise",
                ) {
                    Ok(v) => v,
                    Err(_) => {
                        return Err(crate::diagnostics::Diagnostic::simple_boxed(
                            Severity::Error,
                            "ptr_to_int failed when loading awaited promise",
                        ));
                    }
                };
                let off_const = self.i64_t.const_int(await_slot_offset as u64, false);
                let slot_addr_int = match self.builder.build_int_add(
                    base_int,
                    off_const,
                    "promise_slot_addr_resume",
                ) {
                    Ok(v) => v,
                    Err(_) => {
                        return Err(crate::diagnostics::Diagnostic::simple_boxed(
                            Severity::Error,
                            "int_add failed when loading awaited promise",
                        ));
                    }
                };
                let slot_ptr = match self.builder.build_int_to_ptr(
                    slot_addr_int,
                    self.i8ptr_t,
                    "promise_slot_ptr_resume",
                ) {
                    Ok(p) => p,
                    Err(_) => {
                        return Err(crate::diagnostics::Diagnostic::simple_boxed(
                            Severity::Error,
                            "int_to_ptr failed when loading awaited promise",
                        ));
                    }
                };
                // Load the promise pointer
                let promise_ptr =
                    match self
                        .builder
                        .build_load(self.i8ptr_t, slot_ptr, "resume_promise_load")
                    {
                        Ok(v) => v,
                        Err(_) => {
                            return Err(crate::diagnostics::Diagnostic::simple_boxed(
                                Severity::Error,
                                "load failed when loading awaited promise",
                            ));
                        }
                    };

                // Allocate output slot and re-poll the promise
                let out_alloca = match self.builder.build_alloca(self.i8ptr_t, "resume_await_out") {
                    Ok(a) => a,
                    Err(_) => {
                        return Err(crate::diagnostics::Diagnostic::simple_boxed(
                            Severity::Error,
                            "alloca failed for resume await out slot",
                        ));
                    }
                };
                let poll_fn = self.get_promise_poll_into();
                let _cs = match self.builder.build_call(
                    poll_fn,
                    &[promise_ptr.into(), out_alloca.into()],
                    "resume_promise_poll",
                ) {
                    Ok(cs) => cs,
                    Err(_) => {
                        return Err(crate::diagnostics::Diagnostic::simple_boxed(
                            Severity::Error,
                            "promise_poll_into call failed on resume",
                        ));
                    }
                };
                // Load the result (assuming it's ready now - executor wouldn't wake us if not)
                let resumed_value =
                    match self
                        .builder
                        .build_load(self.i8ptr_t, out_alloca, "resume_await_loaded")
                    {
                        Ok(v) => v,
                        Err(_) => {
                            return Err(crate::diagnostics::Diagnostic::simple_boxed(
                                Severity::Error,
                                "load failed when loading resumed value",
                            ));
                        }
                    };

                let _ = self.builder.build_unconditional_branch(cont_bb);

                // Fix up phi nodes in cont_bb: add incoming edge from this resume block
                let first_inst = cont_bb.get_first_instruction();
                if let Some(inst) = first_inst
                    && inst.get_opcode() == inkwell::values::InstructionOpcode::Phi
                {
                    unsafe {
                        use inkwell::values::AsValueRef;
                        let phi_value = inkwell::values::PhiValue::new(inst.as_value_ref());
                        phi_value.add_incoming(&[(&resumed_value.as_basic_value_enum(), *rb)]);
                    }
                }
            }
        }

        // Populate the ready_return block (promise already completed)
        self.builder.position_at_end(ready_return_bb);
        let _ = self
            .builder
            .build_return(Some(&default_ret.as_basic_value_enum()));

        // Clear async lowering context stored in CodeGen so subsequent
        // function compilations don't accidentally reuse state from
        // this async wrapper.
        let _ = self.async_await_live_sets.borrow_mut().take();
        let _ = self.async_local_name_to_slot.borrow_mut().take();
        let _ = self.async_resume_blocks.borrow_mut().take();
        let _ = self.async_cont_blocks.borrow_mut().take();
        let _ = self.async_poll_function.borrow_mut().take();
        let _ = self.async_poll_locals.borrow_mut().take();
        self.async_await_counter.set(0);
        self.async_param_count.set(0);

        // Clear escape info for this function before returning
        let _ = self.current_escape_info.borrow_mut().take();
        Ok(function)
    }
}
