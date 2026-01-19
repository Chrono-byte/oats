//! Generator function handling for code generation.
//!
//! This module contains code generation logic for generator functions, including
//! live-set computation for yield points and generator function lowering.

use crate::codegen::helpers::extract_basic_value_from_valuekind;
use crate::diagnostics::Severity;
use inkwell::values::{BasicValue, FunctionValue};
use oxc_ast::ast::Function;
use std::collections::{HashMap, HashSet};

impl<'a> crate::codegen::CodeGen<'a> {
    /// Find the positions (statement indices) of all yield expressions in a statement list.
    ///
    /// This walks through statements and identifies which statements contain yield expressions.
    /// Returns a vector of statement indices where yields occur.
    fn find_yield_positions_in_stmts(&self, stmts: &[oxc_ast::ast::Statement]) -> Vec<usize> {
        fn contains_yield_expr(expr: &oxc_ast::ast::Expression) -> bool {
            match expr {
                oxc_ast::ast::Expression::YieldExpression(_) => true,
                oxc_ast::ast::Expression::BinaryExpression(b) => {
                    contains_yield_expr(&b.left) || contains_yield_expr(&b.right)
                }
                oxc_ast::ast::Expression::UnaryExpression(u) => contains_yield_expr(&u.argument),
                oxc_ast::ast::Expression::CallExpression(c) => {
                    // Handle callee - it's an Expression, so recurse
                    if contains_yield_expr(&c.callee) {
                        return true;
                    }
                    c.arguments.iter().any(|arg| {
                        if let Some(expr) = arg.as_expression() {
                            contains_yield_expr(expr)
                        } else {
                            false
                        }
                    })
                }
                oxc_ast::ast::Expression::StaticMemberExpression(m) => {
                    contains_yield_expr(&m.object)
                }
                oxc_ast::ast::Expression::ComputedMemberExpression(m) => {
                    contains_yield_expr(&m.object) || contains_yield_expr(&m.expression)
                }
                oxc_ast::ast::Expression::ConditionalExpression(c) => {
                    contains_yield_expr(&c.test)
                        || contains_yield_expr(&c.consequent)
                        || contains_yield_expr(&c.alternate)
                }
                oxc_ast::ast::Expression::AssignmentExpression(a) => contains_yield_expr(&a.right),
                oxc_ast::ast::Expression::ParenthesizedExpression(p) => {
                    contains_yield_expr(&p.expression)
                }
                oxc_ast::ast::Expression::ArrayExpression(arr) => arr.elements.iter().any(|el| {
                    if let Some(expr) = el.as_expression() {
                        contains_yield_expr(expr)
                    } else {
                        false
                    }
                }),
                oxc_ast::ast::Expression::NewExpression(n) => {
                    contains_yield_expr(&n.callee)
                        || n.arguments.iter().any(|arg| {
                            if let Some(expr) = arg.as_expression() {
                                contains_yield_expr(expr)
                            } else {
                                false
                            }
                        })
                }
                oxc_ast::ast::Expression::SequenceExpression(s) => {
                    s.expressions.iter().any(contains_yield_expr)
                }
                _ => false,
            }
        }

        fn contains_yield_stmt(stmt: &oxc_ast::ast::Statement) -> bool {
            match stmt {
                oxc_ast::ast::Statement::ExpressionStatement(es) => {
                    contains_yield_expr(&es.expression)
                }
                oxc_ast::ast::Statement::ReturnStatement(r) => r
                    .argument
                    .as_ref()
                    .is_some_and(|arg| contains_yield_expr(arg)),
                oxc_ast::ast::Statement::VariableDeclaration(vd) => {
                    vd.declarations.iter().any(|d| {
                        d.init
                            .as_ref()
                            .is_some_and(|init| contains_yield_expr(init))
                    })
                }
                oxc_ast::ast::Statement::IfStatement(if_stmt) => {
                    contains_yield_expr(&if_stmt.test)
                        || contains_yield_stmt(&if_stmt.consequent)
                        || if_stmt
                            .alternate
                            .as_ref()
                            .is_some_and(|a| contains_yield_stmt(a))
                }
                oxc_ast::ast::Statement::WhileStatement(w) => {
                    contains_yield_expr(&w.test) || contains_yield_stmt(&w.body)
                }
                oxc_ast::ast::Statement::ForStatement(f) => {
                    (f.init.as_ref().is_some_and(|i| match i {
                        oxc_ast::ast::ForStatementInit::VariableDeclaration(vd) => {
                            vd.declarations.iter().any(|d| {
                                d.init
                                    .as_ref()
                                    .is_some_and(|init| contains_yield_expr(init))
                            })
                        }
                        _ => false,
                    })) || f
                        .test
                        .as_ref()
                        .is_some_and(|test| contains_yield_expr(test))
                        || f.update
                            .as_ref()
                            .is_some_and(|update| contains_yield_expr(update))
                        || contains_yield_stmt(&f.body)
                }
                oxc_ast::ast::Statement::BlockStatement(b) => {
                    b.body.iter().any(contains_yield_stmt)
                }
                _ => false,
            }
        }

        let mut positions = Vec::new();
        for (idx, stmt) in stmts.iter().enumerate() {
            if contains_yield_stmt(stmt) {
                positions.push(idx);
            }
        }
        positions
    }

    /// Compute a conservative live-set for each `yield` in `func`.
    ///
    /// Strategy (similar to async await live sets):
    /// - Walk the function body in a deterministic, left-to-right order
    ///   collecting encountered `Expr` nodes into a flat vector.
    /// - Record the indices of `Yield` expressions in that vector.
    /// - For each yield index, collect all identifier names that appear in
    ///   any expression after that index. The result is a Vec of `HashSets`
    ///   where each entry corresponds to the nth yield in lexical order.
    pub fn compute_yield_live_sets(&self, func: &Function) -> Vec<HashSet<String>> {
        enum Node {
            Ident(String),
            YieldMarker,
        }

        fn flatten_expr(e: &oxc_ast::ast::Expression, out: &mut Vec<Node>) {
            match e {
                oxc_ast::ast::Expression::Identifier(id) => {
                    out.push(Node::Ident(id.name.to_string()));
                }
                oxc_ast::ast::Expression::CallExpression(c) => {
                    // Handle callee - it's an Expression, so recurse
                    flatten_expr(&c.callee, out);
                    for a in &c.arguments {
                        if let Some(expr) = a.as_expression() {
                            flatten_expr(expr, out);
                        } else if let oxc_ast::ast::Argument::SpreadElement(spread) = a {
                            flatten_expr(&spread.argument, out);
                        }
                    }
                }
                oxc_ast::ast::Expression::StaticMemberExpression(m) => {
                    flatten_expr(&m.object, out);
                }
                oxc_ast::ast::Expression::ComputedMemberExpression(m) => {
                    flatten_expr(&m.object, out);
                    flatten_expr(&m.expression, out);
                }
                oxc_ast::ast::Expression::BinaryExpression(b) => {
                    flatten_expr(&b.left, out);
                    flatten_expr(&b.right, out);
                }
                oxc_ast::ast::Expression::UnaryExpression(u) => flatten_expr(&u.argument, out),
                oxc_ast::ast::Expression::YieldExpression(y) => {
                    // Evaluate the yielded expression first, then mark the
                    // suspension point so idents inside the arg are treated
                    // as occurring before the yield (not after).
                    if let Some(arg) = &y.argument {
                        flatten_expr(arg, out);
                    }
                    out.push(Node::YieldMarker);
                }
                oxc_ast::ast::Expression::ParenthesizedExpression(p) => {
                    flatten_expr(&p.expression, out);
                }
                oxc_ast::ast::Expression::ArrayExpression(arr) => {
                    for el in &arr.elements {
                        if let Some(expr) = el.as_expression() {
                            flatten_expr(expr, out);
                        }
                    }
                }
                oxc_ast::ast::Expression::NewExpression(n) => {
                    flatten_expr(&n.callee, out);
                    for a in &n.arguments {
                        if let Some(expr) = a.as_expression() {
                            flatten_expr(expr, out);
                        } else if let oxc_ast::ast::Argument::SpreadElement(spread) = a {
                            flatten_expr(&spread.argument, out);
                        }
                    }
                }
                oxc_ast::ast::Expression::AssignmentExpression(asg) => {
                    flatten_expr(&asg.right, out);
                }
                oxc_ast::ast::Expression::ConditionalExpression(c) => {
                    flatten_expr(&c.test, out);
                    flatten_expr(&c.consequent, out);
                    flatten_expr(&c.alternate, out);
                }
                oxc_ast::ast::Expression::TemplateLiteral(_)
                | oxc_ast::ast::Expression::NumericLiteral(_)
                | oxc_ast::ast::Expression::StringLiteral(_)
                | oxc_ast::ast::Expression::BooleanLiteral(_)
                | oxc_ast::ast::Expression::NullLiteral(_) => {
                    // literals: no idents to record
                }
                oxc_ast::ast::Expression::ArrowFunctionExpression(_a) => {
                    // don't recurse into nested function/arrow bodies
                }
                _ => {
                    // conservative: don't traverse into patterns we don't care about
                }
            }
        }

        fn flatten_stmt(s: &oxc_ast::ast::Statement, out: &mut Vec<Node>) {
            match s {
                oxc_ast::ast::Statement::ExpressionStatement(es) => {
                    flatten_expr(&es.expression, out);
                }
                oxc_ast::ast::Statement::ReturnStatement(r) => {
                    if let Some(arg) = &r.argument {
                        flatten_expr(arg, out);
                    }
                }
                oxc_ast::ast::Statement::VariableDeclaration(vd) => {
                    for decl in &vd.declarations {
                        if let Some(init) = &decl.init {
                            flatten_expr(init, out);
                        }
                    }
                }
                oxc_ast::ast::Statement::IfStatement(if_stmt) => {
                    flatten_expr(&if_stmt.test, out);
                    flatten_stmt(&if_stmt.consequent, out);
                    if let Some(alt) = &if_stmt.alternate {
                        flatten_stmt(alt, out);
                    }
                }
                oxc_ast::ast::Statement::WhileStatement(w) => {
                    flatten_expr(&w.test, out);
                    flatten_stmt(&w.body, out);
                }
                oxc_ast::ast::Statement::ForStatement(f) => {
                    if let Some(init) = &f.init
                        && let oxc_ast::ast::ForStatementInit::VariableDeclaration(vd) = init
                    {
                        for decl in &vd.declarations {
                            if let Some(init_expr) = &decl.init {
                                flatten_expr(init_expr, out);
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
                oxc_ast::ast::Statement::BlockStatement(b) => {
                    for s in &b.body {
                        flatten_stmt(s, out);
                    }
                }
                _ => {
                    // conservative: don't traverse into other statement types
                }
            }
        }

        let mut nodes: Vec<Node> = Vec::new();
        if let Some(body) = &func.body {
            for s in &body.statements {
                flatten_stmt(s, &mut nodes);
            }
        }

        // Find yield indices
        let yield_indices: Vec<usize> = nodes
            .iter()
            .enumerate()
            .filter_map(|(i, n)| match n {
                Node::YieldMarker => Some(i),
                _ => None,
            })
            .collect();

        // For each yield, collect all idents that appear after it
        let mut live_sets: Vec<HashSet<String>> = Vec::new();
        for &yield_idx in &yield_indices {
            let mut live: HashSet<String> = HashSet::new();
            for node in nodes.iter().skip(yield_idx + 1) {
                if let Node::Ident(name) = node {
                    live.insert(name.clone());
                }
            }
            live_sets.push(live);
        }

        live_sets
    }

    /// Generate generator function wrapper and next function.
    ///
    /// This function handles the generator-specific code generation for functions
    /// marked as generator. It creates a next function and a wrapper that returns
    /// a generator object.
    pub fn gen_generator_function_ir(
        &self,
        func_name: &str,
        func_decl: &Function,
        llvm_param_types: &[inkwell::types::BasicMetadataTypeEnum<'a>],
        _ret_type: &crate::types::OatsType,
        function: FunctionValue<'a>,
        entry: inkwell::basic_block::BasicBlock<'a>,
    ) -> crate::diagnostics::DiagnosticResult<FunctionValue<'a>> {
        // Generators return a generator object (i8* pointer to generator state)
        // The generator state contains the function's state machine

        // Emit a next function: `fn <name>_next(state: i8*, out: i8*) -> i32`
        // Returns: 0 = done, 1 = yielded value available in out
        let next_name = format!("{func_name}_next");
        let next_ft = self
            .i32_t
            .fn_type(&[self.i8ptr_t.into(), self.i8ptr_t.into()], false);
        let next_f = self.module.add_function(&next_name, next_ft, None);
        let next_entry = self.context.append_basic_block(next_f, "entry");
        self.builder.position_at_end(next_entry);

        // Compute per-yield live-sets early so we can reserve state
        // layout indices and prepare resume/cont basic blocks before
        // emitting the next body.
        let param_count = llvm_param_types.len();
        let yield_live_sets = self.compute_yield_live_sets(func_decl);
        let mut all_live_names: HashSet<String> = HashSet::new();
        for s in &yield_live_sets {
            for n in s {
                all_live_names.insert(n.clone());
            }
        }

        // Prepare resume/cont block vectors sized to number of yields
        let mut resume_blocks: Vec<inkwell::basic_block::BasicBlock<'a>> = Vec::new();
        let mut cont_blocks: Vec<inkwell::basic_block::BasicBlock<'a>> = Vec::new();
        for i in 0..yield_live_sets.len() {
            resume_blocks.push(
                self.context
                    .append_basic_block(next_f, &format!("resume_{i}")),
            );
            cont_blocks.push(
                self.context
                    .append_basic_block(next_f, &format!("cont_{i}")),
            );
        }

        // Build a name -> slot index map for locals
        let mut local_name_to_slot: HashMap<String, usize> = HashMap::new();
        let mut names: Vec<String> = all_live_names.clone().into_iter().collect();
        names.sort();
        let mut idx = param_count;
        for name in names {
            local_name_to_slot.insert(name, idx);
            idx += 1;
        }

        // Store generator lowering context in CodeGen BEFORE generating next body
        self.generator_yield_live_sets
            .borrow_mut()
            .replace(yield_live_sets);
        self.generator_local_name_to_slot
            .borrow_mut()
            .replace(local_name_to_slot.clone());
        self.generator_resume_blocks
            .borrow_mut()
            .replace(resume_blocks.clone());
        self.generator_cont_blocks
            .borrow_mut()
            .replace(cont_blocks.clone());
        self.generator_next_function.borrow_mut().replace(next_f);
        self.generator_yield_counter.set(0);
        self.generator_param_count.set(param_count as u32);
        self.generator_local_slot_count.set(all_live_names.len());

        // Now emit the next body entry: read the state field (u32 at offset +8)
        // and dispatch based on it. Layout: [next_fn_ptr (8)] [state_u32 (4) + pad(4)] [param slots ...]
        // If state == 0 => call impl (first run). If state matches a resume
        // index (1-based) we will jump into the corresponding resume block.
        // Otherwise the generator is done and we return 0.
        let state_param = next_f.get_nth_param(0).ok_or_else(|| {
            crate::diagnostics::Diagnostic::simple_boxed(
                Severity::Error,
                "next function missing state parameter",
            )
        })?;
        let state_ptr = state_param.into_pointer_value();

        // Read state u32 at offset 8
        let state_off = self.i64_t.const_int(8, false);
        // Use GEP instead of ptrtoint/add/inttoptr to preserve pointer provenance
        let state_field_ptr = unsafe {
            self.builder
                .build_gep(self.i8_t, state_ptr, &[state_off], "state_field_ptr")
                .map_err(|_| {
                    crate::diagnostics::Diagnostic::simple_boxed(
                        Severity::Error,
                        "GEP failed in next",
                    )
                })?
        };
        let state_val = match self
            .builder
            .build_load(self.i32_t, state_field_ptr, "state_load")
        {
            Ok(v) => v.into_int_value(),
            Err(_) => {
                return Err(crate::diagnostics::Diagnostic::simple_boxed(
                    Severity::Error,
                    "state load failed in next",
                ));
            }
        };

        // Create dispatch: if state == 0, go to run_impl, else check resume blocks
        let zero_state = self.i32_t.const_int(0, false);
        let run_impl_bb = self.context.append_basic_block(next_f, "run_impl");
        let done_bb = self.context.append_basic_block(next_f, "done");

        let is_zero = self
            .builder
            .build_int_compare(inkwell::IntPredicate::EQ, state_val, zero_state, "is_zero")
            .map_err(|_| {
                crate::diagnostics::Diagnostic::simple_boxed(
                    Severity::Error,
                    "int compare failed in next",
                )
            })?;

        // Build dispatch switch for resume blocks
        let dispatch_bb = self.context.append_basic_block(next_f, "dispatch");
        match self
            .builder
            .build_conditional_branch(is_zero, run_impl_bb, dispatch_bb)
        {
            Ok(_) => {}
            Err(_) => {
                return Err(crate::diagnostics::Diagnostic::simple_boxed(
                    Severity::Error,
                    "branch failed in next",
                ));
            }
        }

        // Dispatch block: switch on state to resume blocks
        self.builder.position_at_end(dispatch_bb);

        // Build cases vector for switch
        let mut cases: Vec<(
            inkwell::values::IntValue<'a>,
            inkwell::basic_block::BasicBlock<'a>,
        )> = Vec::new();
        for (i, resume_bb) in resume_blocks.iter().enumerate() {
            let case_val = self.i32_t.const_int((i + 1) as u64, false);
            cases.push((case_val, *resume_bb));
        }

        self.builder
            .build_switch(state_val, done_bb, cases.as_slice())
            .map_err(|_| {
                crate::diagnostics::Diagnostic::simple_boxed(
                    Severity::Error,
                    "switch failed in next",
                )
            })?;

        // Done block: return 0 (generator finished)
        self.builder.position_at_end(done_bb);
        let zero = self.i32_t.const_int(0, false);
        match self.builder.build_return(Some(&zero.as_basic_value_enum())) {
            Ok(_) => {}
            Err(_) => {
                return Err(crate::diagnostics::Diagnostic::simple_boxed(
                    Severity::Error,
                    "return failed in done block",
                ));
            }
        }

        // run_impl: Lower the function body with generator context active.
        // This allows yield expressions to properly save/restore state.
        self.builder.position_at_end(run_impl_bb);

        // Create param allocas for next_f and load parameters from state slots
        let mut param_map: HashMap<String, u32> = HashMap::new();

        use crate::codegen::common_types::LocalEntry;
        let mut locals_stack: Vec<HashMap<String, LocalEntry<'a>>> = vec![HashMap::new()];

        // Load each parameter from state and create allocas
        for (i, param) in func_decl.params.items.iter().enumerate() {
            let param_name = match &param.pattern.kind {
                oxc_ast::ast::BindingPatternKind::BindingIdentifier(id) => id.name.to_string(),
                _ => {
                    // Destructuring parameters not yet supported in codegen
                    format!("_param_{i}")
                }
            };
            param_map.insert(param_name.clone(), i as u32);

            // Compute slot offset: after header (8) + state (4) + padding (4) = 16, then param slots
            let slot_offset = 16 + (i * 8) as u64;
            let param_i8ptr = self
                .offset_i8_ptr(state_ptr, slot_offset, &format!("param_{i}_i8ptr"))
                .map_err(|_| {
                    crate::diagnostics::Diagnostic::simple_boxed(
                        Severity::Error,
                        "gep failed for param",
                    )
                })?;

            let llvm_ty_meta = *llvm_param_types.get(i).unwrap();
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

            let param_alloca = self
                .builder
                .build_alloca(llvm_ty, &format!("param_{i}"))
                .map_err(|_| {
                    crate::diagnostics::Diagnostic::simple_boxed(
                        Severity::Error,
                        "alloca failed for param",
                    )
                })?;

            // Load from state slot
            let slot_ptr_ty = self.context.ptr_type(inkwell::AddressSpace::default());
            let slot_ptr = self
                .builder
                .build_pointer_cast(param_i8ptr, slot_ptr_ty, "slot_ptr")
                .map_err(|_| {
                    crate::diagnostics::Diagnostic::simple_boxed(
                        Severity::Error,
                        "pointer cast failed",
                    )
                })?;

            let loaded = if let inkwell::types::BasicMetadataTypeEnum::FloatType(ft) = llvm_ty_meta
            {
                // Cast slot_ptr to generic pointer type for loading
                let f64_ptr = self
                    .builder
                    .build_pointer_cast(
                        slot_ptr,
                        self.context.ptr_type(inkwell::AddressSpace::default()),
                        "f64_ptr",
                    )
                    .map_err(|_| {
                        crate::diagnostics::Diagnostic::simple_boxed(
                            Severity::Error,
                            "f64 ptr cast failed",
                        )
                    })?;
                // Load the float value - ft is used here to specify the load type
                self.builder
                    .build_load(ft, f64_ptr, &format!("param_{i}_load"))
                    .map_err(|_| {
                        crate::diagnostics::Diagnostic::simple_boxed(
                            Severity::Error,
                            "param load failed",
                        )
                    })?
                    .as_basic_value_enum()
            } else {
                // Pointer types
                let ptr_ptr = self
                    .builder
                    .build_pointer_cast(
                        slot_ptr,
                        self.context.ptr_type(inkwell::AddressSpace::default()),
                        "ptr_ptr",
                    )
                    .map_err(|_| {
                        crate::diagnostics::Diagnostic::simple_boxed(
                            Severity::Error,
                            "ptr cast failed",
                        )
                    })?;
                let ptr_ptr_typed = self
                    .builder
                    .build_pointer_cast(
                        ptr_ptr,
                        self.context.ptr_type(inkwell::AddressSpace::default()),
                        "ptr_ptr_typed",
                    )
                    .map_err(|_| {
                        crate::diagnostics::Diagnostic::simple_boxed(
                            Severity::Error,
                            "ptr typed cast failed",
                        )
                    })?;
                self.builder
                    .build_load(self.i8ptr_t, ptr_ptr_typed, &format!("param_{i}_load"))
                    .map_err(|_| {
                        crate::diagnostics::Diagnostic::simple_boxed(
                            Severity::Error,
                            "param load failed",
                        )
                    })?
                    .as_basic_value_enum()
            };

            let _ = self.builder.build_store(param_alloca, loaded);
            locals_stack.get_mut(0).unwrap().insert(
                param_name,
                (param_alloca, llvm_ty, true, false, false, None, None),
            );
        }

        // Store locals stack for resume blocks
        *self.generator_next_locals.borrow_mut() = Some(locals_stack.clone());

        // Find all yield points in the body (needed for continuation blocks)
        let yield_positions: Vec<usize> = if let Some(body) = &func_decl.body {
            self.find_yield_positions_in_stmts(&body.statements)
        } else {
            Vec::new()
        };

        // Split function body at yield points and lower each segment
        if let Some(body) = &func_decl.body {
            // Lower body segments: from start to first yield, between yields, and after last yield
            let mut stmt_idx = 0;
            let yield_idx = 0;

            // Lower initial segment (before first yield)
            while stmt_idx < body.statements.len() {
                if yield_idx < yield_positions.len()
                    && stmt_idx == *yield_positions.get(yield_idx).unwrap()
                {
                    // We've reached a yield point - lower up to (but not including) this statement
                    // The yield will be handled by lower_expr when we lower the statement
                    break;
                }

                // Lower this statement
                if self.lower_stmt(
                    body.statements.get(stmt_idx).unwrap(),
                    next_f,
                    &param_map,
                    &mut locals_stack,
                )? {
                    // Terminator found (return, break, etc.)
                    break;
                }
                stmt_idx += 1;
            }

            // If we hit a yield, handle it
            if yield_idx < yield_positions.len()
                && stmt_idx == *yield_positions.get(yield_idx).unwrap()
            {
                // Lower the statement containing the yield
                // The yield expression will save state and return
                let _ = self.lower_stmt(
                    body.statements.get(stmt_idx).unwrap(),
                    next_f,
                    &param_map,
                    &mut locals_stack,
                );
            }
        }

        // If we reach here without yielding, generator is done
        // This happens when the function completes normally
        let _ = self.builder.build_unconditional_branch(done_bb);

        // Implement resume blocks: restore state and continue after yield
        for (resume_idx, resume_bb) in resume_blocks.iter().enumerate() {
            self.builder.position_at_end(*resume_bb);

            // Restore live locals from state
            if let Some(live_sets) = self.generator_yield_live_sets.borrow().as_ref()
                && resume_idx < live_sets.len()
            {
                let live_set = live_sets.get(resume_idx).unwrap();
                if let Some(local_name_to_slot) =
                    self.generator_local_name_to_slot.borrow().as_ref()
                {
                    for live_name in live_set {
                        if let Some(slot_idx) = local_name_to_slot.get(live_name) {
                            // Find the local in the locals stack
                            if let Some((
                                local_ptr,
                                local_ty,
                                _init,
                                _is_const,
                                _is_weak,
                                _nominal,
                                _oats_type,
                            )) = self.find_local(&locals_stack, live_name)
                            {
                                // Compute slot offset in state
                                let slot_offset = 16 + (*slot_idx * 8) as u64;
                                let slot_i8ptr = self
                                    .offset_i8_ptr(
                                        state_ptr,
                                        slot_offset,
                                        &format!("restore_{live_name}_i8ptr"),
                                    )
                                    .map_err(|_| {
                                        crate::diagnostics::Diagnostic::simple_boxed(
                                            Severity::Error,
                                            "gep failed for restore",
                                        )
                                    })?;

                                // Load from state slot
                                let slot_ptr_ty =
                                    self.context.ptr_type(inkwell::AddressSpace::default());
                                let slot_ptr = self
                                    .builder
                                    .build_pointer_cast(slot_i8ptr, slot_ptr_ty, "slot_ptr")
                                    .map_err(|_| {
                                        crate::diagnostics::Diagnostic::simple_boxed(
                                            Severity::Error,
                                            "pointer cast failed",
                                        )
                                    })?;

                                let restored_val = if let inkwell::types::BasicTypeEnum::FloatType(
                                    ft,
                                ) = local_ty
                                {
                                    // Cast slot_ptr to generic pointer type for loading
                                    let f64_ptr = self
                                        .builder
                                        .build_pointer_cast(
                                            slot_ptr,
                                            self.context.ptr_type(inkwell::AddressSpace::default()),
                                            "f64_ptr",
                                        )
                                        .map_err(|_| {
                                            crate::diagnostics::Diagnostic::simple_boxed(
                                                Severity::Error,
                                                "f64 ptr cast failed",
                                            )
                                        })?;
                                    // Load the float value - ft is used here to specify the load type
                                    self.builder
                                        .build_load(ft, f64_ptr, &format!("restore_{live_name}"))
                                        .map_err(|_| {
                                            crate::diagnostics::Diagnostic::simple_boxed(
                                                Severity::Error,
                                                "load failed",
                                            )
                                        })?
                                } else {
                                    // Pointer types
                                    let ptr_ptr = self
                                        .builder
                                        .build_pointer_cast(
                                            slot_ptr,
                                            self.context.ptr_type(inkwell::AddressSpace::default()),
                                            "ptr_ptr",
                                        )
                                        .map_err(|_| {
                                            crate::diagnostics::Diagnostic::simple_boxed(
                                                Severity::Error,
                                                "ptr cast failed",
                                            )
                                        })?;
                                    self.builder
                                        .build_load(
                                            self.i8ptr_t,
                                            ptr_ptr,
                                            &format!("restore_{live_name}"),
                                        )
                                        .map_err(|_| {
                                            crate::diagnostics::Diagnostic::simple_boxed(
                                                Severity::Error,
                                                "load failed",
                                            )
                                        })?
                                };

                                // Store to local alloca
                                let _ = self.builder.build_store(local_ptr, restored_val).map_err(
                                    |_| {
                                        crate::diagnostics::Diagnostic::simple_boxed(
                                            Severity::Error,
                                            "store failed",
                                        )
                                    },
                                )?;
                            }
                        }
                    }
                }
            }

            // Branch to continuation block
            let cont_bb = *cont_blocks.get(resume_idx).unwrap();
            let _ = self
                .builder
                .build_unconditional_branch(cont_bb)
                .map_err(|_| {
                    crate::diagnostics::Diagnostic::simple_boxed(
                        Severity::Error,
                        "branch to cont failed",
                    )
                })?;

            // Continuation block: continue execution after yield
            self.builder.position_at_end(cont_bb);

            // Restore locals_stack from stored value (needed for continuation blocks)
            if let Some(stored_locals) = self.generator_next_locals.borrow().as_ref() {
                // Use the stored locals_stack for continuation
                let mut cont_locals_stack = stored_locals.clone();

                // Lower remaining statements after this yield point
                if let Some(body) = &func_decl.body {
                    // Find the next yield position (if any)
                    let next_yield_pos = if resume_idx + 1 < yield_positions.len() {
                        Some(*yield_positions.get(resume_idx + 1).unwrap())
                    } else {
                        None
                    };

                    // Find where to start (after the yield statement)
                    let start_pos = *yield_positions.get(resume_idx).unwrap() + 1;
                    let end_pos = match next_yield_pos {
                        Some(pos) => pos,
                        None => body.statements.len(),
                    };

                    // Lower statements in this segment
                    let mut hit_terminator = false;
                    for i in start_pos..end_pos {
                        if self.lower_stmt(
                            body.statements.get(i).unwrap(),
                            next_f,
                            &param_map,
                            &mut cont_locals_stack,
                        )? {
                            // Terminator found (return, break, etc.)
                            hit_terminator = true;
                            break;
                        }
                    }

                    // If we didn't hit a terminator and there's no next yield, branch to done
                    if !hit_terminator {
                        let _ = self.builder.build_unconditional_branch(done_bb);
                    }
                    // If there's a next yield, it will be handled when that yield is reached
                    // (the yield will return, and the next continuation block will handle it)
                } else {
                    // No body - branch to done
                    let _ = self.builder.build_unconditional_branch(done_bb);
                }
            } else {
                // No stored locals - branch to done
                let _ = self.builder.build_unconditional_branch(done_bb);
            }
        }

        // Move builder back to the wrapper function entry so subsequent
        // allocations and stores are emitted into the exported wrapper
        // rather than the next function.
        self.builder.position_at_end(entry);

        // Now create a heap-allocated state object and store the next fn ptr at offset 0
        // State layout: [next_fn_ptr (8)] [state_u32 (4) + pad(4)] [param slots 8 bytes each ...] [local slots ...]
        let malloc_fn = self.get_malloc();

        // Compute state size: header (8) + state field (4) + padding (4) = 16, then param and local slots
        let local_slot_count = all_live_names.len();
        let total_slots = param_count + local_slot_count;
        let state_size_bytes = 16u64 + (total_slots as u64 * 8u64);
        let state_size = self.i64_t.const_int(state_size_bytes, false);

        let cs = match self
            .builder
            .build_call(malloc_fn, &[state_size.into()], "gen_state_malloc")
        {
            Ok(cs) => cs,
            Err(_) => {
                return Err(crate::diagnostics::Diagnostic::simple_boxed(
                    Severity::Error,
                    "malloc failed for generator state",
                ));
            }
        };

        let state_ptr_wrapper = match extract_basic_value_from_valuekind(cs.try_as_basic_value()) {
            Some(bv) => bv.into_pointer_value(),
            None => {
                return Err(crate::diagnostics::Diagnostic::simple_boxed(
                    Severity::Error,
                    "malloc returned no value",
                ));
            }
        };

        // Store next function pointer at offset 0
        let next_fn_ptr_ty = self.context.ptr_type(inkwell::AddressSpace::default());
        let next_fn_ptr = self
            .builder
            .build_pointer_cast(state_ptr_wrapper, next_fn_ptr_ty, "next_fn_ptr_cast")
            .map_err(|_| {
                crate::diagnostics::Diagnostic::simple_boxed(
                    Severity::Error,
                    "pointer cast failed for next fn ptr",
                )
            })?;
        let next_fn_ptr_val = self
            .builder
            .build_pointer_cast(
                next_f.as_global_value().as_pointer_value(),
                self.i8ptr_t,
                "next_fn_as_i8ptr",
            )
            .map_err(|_| {
                crate::diagnostics::Diagnostic::simple_boxed(
                    Severity::Error,
                    "next fn pointer cast failed",
                )
            })?;
        let store = self
            .builder
            .build_store(next_fn_ptr, next_fn_ptr_val.as_basic_value_enum());
        if let Ok(st) = store {
            let _ = st.set_alignment(8);
        }

        // Store initial state (0) at offset 8
        let state_off = self.i64_t.const_int(8, false);
        // Use GEP instead of ptrtoint/add/inttoptr to preserve pointer provenance
        let state_field_ptr = unsafe {
            self.builder
                .build_gep(
                    self.i8_t,
                    state_ptr_wrapper,
                    &[state_off],
                    "state_field_ptr_wrapper",
                )
                .map_err(|_| {
                    crate::diagnostics::Diagnostic::simple_boxed(
                        Severity::Error,
                        "GEP failed in wrapper",
                    )
                })?
        };
        let zero_state = self.i32_t.const_int(0, false);
        let store = self
            .builder
            .build_store(state_field_ptr, zero_state.as_basic_value_enum());
        if let Ok(st) = store {
            let _ = st.set_alignment(8);
        }

        // Store parameters in state slots (starting at offset 16)
        for (i, _param) in func_decl.params.items.iter().enumerate() {
            let param_val = function.get_nth_param(i as u32).ok_or_else(|| {
                crate::diagnostics::Diagnostic::simple_boxed(
                    Severity::Error,
                    "parameter missing in wrapper",
                )
            })?;

            // Compute slot offset: after header (8) + state (4) + padding (4) = 16, then param slots
            let slot_offset = 16 + (i * 8) as u64;
            let slot_i8ptr = self
                .offset_i8_ptr(
                    state_ptr_wrapper,
                    slot_offset,
                    &format!("param_{i}_slot_i8ptr"),
                )
                .map_err(|_| {
                    crate::diagnostics::Diagnostic::simple_boxed(
                        Severity::Error,
                        "gep failed for param slot",
                    )
                })?;

            // Store parameter value in state slot
            let slot_ptr_ty = self.context.ptr_type(inkwell::AddressSpace::default());
            let slot_ptr = self
                .builder
                .build_pointer_cast(slot_i8ptr, slot_ptr_ty, "slot_ptr")
                .map_err(|_| {
                    crate::diagnostics::Diagnostic::simple_boxed(
                        Severity::Error,
                        "pointer cast failed for param slot",
                    )
                })?;

            let llvm_ty_meta = *llvm_param_types.get(i).unwrap();
            if let inkwell::types::BasicMetadataTypeEnum::FloatType(_) = llvm_ty_meta {
                // Box f64 into union
                let fv = param_val.into_float_value();
                let box_fn = self.get_union_box_f64();
                let cs = self
                    .builder
                    .build_call(box_fn, &[fv.into()], &format!("box_param_{i}"))
                    .map_err(|_| {
                        crate::diagnostics::Diagnostic::simple_boxed(
                            Severity::Error,
                            "union_box_f64 call failed",
                        )
                    })?;
                let boxed_ptr = match extract_basic_value_from_valuekind(cs.try_as_basic_value()) {
                    Some(bv) => bv.into_pointer_value(),
                    None => {
                        return Err(crate::diagnostics::Diagnostic::simple_boxed(
                            Severity::Error,
                            "boxing returned no value",
                        ));
                    }
                };
                let f64_ptr = self
                    .builder
                    .build_pointer_cast(
                        slot_ptr,
                        self.context.ptr_type(inkwell::AddressSpace::default()),
                        "f64_ptr",
                    )
                    .map_err(|_| {
                        crate::diagnostics::Diagnostic::simple_boxed(
                            Severity::Error,
                            "f64 ptr cast failed",
                        )
                    })?;
                let store = self
                    .builder
                    .build_store(f64_ptr, boxed_ptr.as_basic_value_enum());
                if let Ok(st) = store {
                    let _ = st.set_alignment(8);
                }
            } else {
                // Pointer types - store directly
                let ptr_ptr = self
                    .builder
                    .build_pointer_cast(
                        slot_ptr,
                        self.context.ptr_type(inkwell::AddressSpace::default()),
                        "ptr_ptr",
                    )
                    .map_err(|_| {
                        crate::diagnostics::Diagnostic::simple_boxed(
                            Severity::Error,
                            "ptr cast failed",
                        )
                    })?;
                let param_ptr = param_val.into_pointer_value();
                let param_i8ptr = self
                    .builder
                    .build_pointer_cast(param_ptr, self.i8ptr_t, "param_i8ptr")
                    .map_err(|_| {
                        crate::diagnostics::Diagnostic::simple_boxed(
                            Severity::Error,
                            "param pointer cast failed",
                        )
                    })?;
                let store = self
                    .builder
                    .build_store(ptr_ptr, param_i8ptr.as_basic_value_enum());
                if let Ok(st) = store {
                    let _ = st.set_alignment(8);
                }
            }
        }

        // Return the generator state pointer from the wrapper function
        let _ = self
            .builder
            .build_return(Some(&state_ptr_wrapper.as_basic_value_enum()));

        // Clear generator lowering context stored in CodeGen so subsequent
        // function compilations don't accidentally reuse state from
        // this generator wrapper.
        let _ = self.generator_yield_live_sets.borrow_mut().take();
        let _ = self.generator_local_name_to_slot.borrow_mut().take();
        let _ = self.generator_resume_blocks.borrow_mut().take();
        let _ = self.generator_cont_blocks.borrow_mut().take();
        let _ = self.generator_next_function.borrow_mut().take();
        let _ = self.generator_next_locals.borrow_mut().take();
        self.generator_yield_counter.set(0);
        self.generator_param_count.set(0);
        self.generator_local_slot_count.set(0);

        // Clear escape info for this function before returning
        let _ = self.current_escape_info.borrow_mut().take();
        Ok(function)
    }
}
