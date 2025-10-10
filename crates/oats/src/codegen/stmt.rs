//! Statement lowering helpers
//!
//! This module contains the lowering logic for AST statement nodes into
//! LLVM IR using the `CodeGen` helpers. It implements a minimal but
//! well-documented subset of statement lowering used by the test-suite and
//! examples: variable declarations, expression statements, control-flow
//! constructs (if/for/while/do-while/for-of), returns and block scoping.
//!
//! The file documents non-obvious decisions such as union boxing and
//! reference-counting management so future contributors understand the
//! rationale behind emitted IR.

use inkwell::AddressSpace;
use inkwell::types::BasicType;
use inkwell::values::{BasicValue, BasicValueEnum, FunctionValue};
use std::collections::HashMap;

type LocalEntry<'a> = (
    inkwell::values::PointerValue<'a>,
    inkwell::types::BasicTypeEnum<'a>,
    bool,
    bool,
    bool,
    Option<String>,
);
// A stack of per-scope local maps.
//
// Each entry in the vector is a HashMap representing a lexical scope's
// locals. The maps store `LocalEntry` tuples describing the alloca pointer,
// the ABI type of the slot, initialization flag, const flag, whether the
// local is a Weak<T> (affects RC semantics), and an optional nominal type
// name used to guide member access lowering.
type LocalsStackLocal<'a> = Vec<HashMap<String, LocalEntry<'a>>>;

impl<'a> crate::codegen::CodeGen<'a> {
    /// Lower a sequence of statements into `function`.
    ///
    /// Returns `Ok(true)` if lowering emitted a terminator instruction
    /// (for example `return` or an unconditional branch that ends the
    /// current block). This signals callers to stop emitting fall-through
    /// code for the current basic block.
    pub(crate) fn lower_stmts(
        &self,
        stmts: &[deno_ast::swc::ast::Stmt],
        function: inkwell::values::FunctionValue<'a>,
        param_map: &HashMap<String, u32>,
        locals_stack: &mut LocalsStackLocal<'a>,
    ) -> Result<bool, crate::diagnostics::Diagnostic> {
        for stmt in stmts {
            if self.lower_stmt(stmt, function, param_map, locals_stack)? {
                return Ok(true); // Terminator found, stop processing.
            }
        }
        Ok(false)
    }

    pub(crate) fn lower_stmt(
        &self,
        _stmt: &deno_ast::swc::ast::Stmt,
        _function: FunctionValue<'a>,
        _param_map: &HashMap<String, u32>,
        _locals_stack: &mut LocalsStackLocal<'a>,
    ) -> Result<bool, crate::diagnostics::Diagnostic> {
        // A small statement lowering implementation that covers the test
        // cases: variable declarations with initializers, expression
        // statements, return statements and blocks. This is intentionally
        // minimal: more statements can be added into `stmt.rs` later.
        match _stmt {
            deno_ast::swc::ast::Stmt::Decl(d) => {
                if let deno_ast::swc::ast::Decl::Var(var_decl) = d {
                    for decl in &var_decl.decls {
                        // Only simple identifier patterns are handled at present.
                        if let deno_ast::swc::ast::Pat::Ident(ident) = &decl.name {
                            let name = ident.id.sym.to_string();

                            // Determine if the identifier has an explicit type annotation
                            // (do this regardless of whether there's an initializer so we
                            // know whether the local should be treated as Weak<T> even
                            // for uninitialized locals). Also capture a nominal struct
                            // name if the annotation maps to a NominalStruct so member
                            // lowering can use it.
                            let mut declared_mapped: Option<crate::types::OatsType> = None;
                            let mut declared_union: Option<crate::types::OatsType> = None;
                            let mut declared_is_weak = false;
                            let mut declared_nominal: Option<String> = None;
                            if let Some(type_ann) = &ident.type_ann
                                && let Some(mapped) = crate::types::map_ts_type(&type_ann.type_ann)
                            {
                                declared_mapped = Some(mapped.clone());
                                if let crate::types::OatsType::Union(_) = &mapped {
                                    declared_union = Some(mapped.clone());
                                }
                                if let crate::types::OatsType::Weak(_) = &mapped {
                                    declared_is_weak = true;
                                }
                                if let crate::types::OatsType::NominalStruct(n) = &mapped {
                                    declared_nominal = Some(n.clone());
                                }
                                // If the declared type is an Array, tag the local with
                                // a sentinel nominal so lowering sites (like println)
                                // can detect arrays at compile time.
                                if let crate::types::OatsType::Array(_) = &mapped {
                                    declared_nominal = Some("__oats_array".to_string());
                                }
                            }

                            // If there is an initializer, lower it and allocate a
                            // matching alloca for its type. If the declared identifier
                            // has a TypeScript type annotation that maps to a union,
                            // allocate the ABI slot accordingly and box numeric
                            // payloads into union objects.
                            if let Some(init) = &decl.init {
                                // If the initializer is an object literal and the
                                // declared local carries a nominal struct name
                                // (for example `const user: User = { ... }`),
                                // register the object's property list under
                                // `self.class_fields` keyed by the nominal name.
                                // This helps member-access lowering (e.g., `user.x`)
                                // infer field offsets for locals typed with
                                // interface/type-alias names declared in-scope.
                                if let deno_ast::swc::ast::Expr::Object(obj_lit) = &**init
                                    && declared_nominal.is_some()
                                {
                                    // declared_nominal was checked with `is_some()` above;
                                    // be defensive and avoid panics by returning a Diagnostic
                                    // if the value is somehow missing.
                                    let nominal_name = if let Some(n) = declared_nominal.clone() {
                                        n
                                    } else {
                                        return Err(crate::diagnostics::Diagnostic::simple(
                                            "internal error: nominal name missing",
                                        ));
                                    };
                                    let mut fields: Vec<(String, crate::types::OatsType)> =
                                        Vec::new();
                                    use deno_ast::swc::ast;
                                    for prop in &obj_lit.props {
                                        if let ast::PropOrSpread::Prop(prop_box) = prop
                                            && let ast::Prop::KeyValue(kv) = &**prop_box
                                            && let ast::PropName::Ident(id) = &kv.key
                                        {
                                            let fname = id.sym.to_string();
                                            // Try to infer the field type from the initializer expression.
                                            let fty =
                                                crate::types::infer_type(None, Some(&kv.value));
                                            fields.push((fname, fty));
                                        }
                                    }
                                    if !fields.is_empty() {
                                        self.class_fields.borrow_mut().insert(nominal_name, fields);
                                    }
                                }
                                // If initializer is `new ClassName(...)`, record the declared nominal
                                // so member call lowering can infer the class for locals without
                                // an explicit type annotation.
                                if let deno_ast::swc::ast::Expr::New(new_expr) = &**init
                                    && let deno_ast::swc::ast::Expr::Ident(ident) =
                                        &*new_expr.callee
                                {
                                    declared_nominal = Some(ident.sym.to_string());
                                }
                                // `init` is an Option<Box<Expr>> (deno_ast wrapper); use `.as_ref()`
                                // Special-case: if the declared type is a Tuple and the
                                // initializer is an array literal, allocate a native
                                // tuple object here and register its field types so
                                // member/index lowering can treat it as a nominal.
                                if let Some(crate::types::OatsType::Tuple(elem_types)) =
                                    &declared_mapped
                                    && let deno_ast::swc::ast::Expr::Array(arr_lit) = &**init
                                {
                                    // number of elements
                                    let elem_count = arr_lit.elems.len();
                                    // Generate a nominal name for this tuple shape
                                    let fname = _function.get_name().to_str().unwrap_or("<fn>");
                                    let gen_name = format!("tuple_{}_{}", fname, name);
                                    // Register field list under class_fields as ("0", type0), ...
                                    let mut fields: Vec<(String, crate::types::OatsType)> =
                                        Vec::new();
                                    for (i, et) in elem_types.iter().enumerate() {
                                        fields.push((format!("{}", i), et.clone()));
                                    }
                                    self.class_fields
                                        .borrow_mut()
                                        .insert(gen_name.clone(), fields);

                                    // Allocate object: header + meta_slot + elem_count * 8
                                    let header_size = 8u64;
                                    let meta_slot = 8u64;
                                    let total_size =
                                        header_size + meta_slot + (elem_count as u64 * 8);
                                    let malloc_fn = self.get_malloc();
                                    let size_const = self.i64_t.const_int(total_size, false);
                                    let cs = self.builder.build_call(
                                        malloc_fn,
                                        &[size_const.into()],
                                        "tuple_malloc",
                                    );
                                    if let Ok(cs) = cs
                                        && let inkwell::Either::Left(bv) = cs.try_as_basic_value()
                                    {
                                        let malloc_ret = bv.into_pointer_value();
                                        // store header
                                        let header_ptr = self
                                            .builder
                                            .build_pointer_cast(malloc_ret, self.i8ptr_t, "hdr_ptr")
                                            .map_err(|_| {
                                                crate::diagnostics::Diagnostic::simple(
                                                    "pointer cast failed",
                                                )
                                            })?;
                                        let header_val = self.i64_t.const_int(1u64, false);
                                        let _ = self.builder.build_store(header_ptr, header_val);

                                        // For each element in the array literal, lower and store
                                        for (i, opt) in arr_lit.elems.iter().enumerate() {
                                            if let Some(expr_or_spread) = opt {
                                                let ev = self.lower_expr(
                                                    &expr_or_spread.expr,
                                                    _function,
                                                    _param_map,
                                                    _locals_stack,
                                                )?;
                                                let offset =
                                                    header_size + meta_slot + (i as u64 * 8);
                                                let obj_addr = self
                                                    .builder
                                                    .build_ptr_to_int(
                                                        malloc_ret, self.i64_t, "obj_addr",
                                                    )
                                                    .map_err(|_| {
                                                        crate::diagnostics::Diagnostic::simple(
                                                            "ptr_to_int failed",
                                                        )
                                                    })?;
                                                let offset_const =
                                                    self.i64_t.const_int(offset, false);
                                                let field_addr = self
                                                    .builder
                                                    .build_int_add(
                                                        obj_addr,
                                                        offset_const,
                                                        "field_addr",
                                                    )
                                                    .map_err(|_| {
                                                        crate::diagnostics::Diagnostic::simple(
                                                            "int_add failed",
                                                        )
                                                    })?;
                                                let field_ptr = self
                                                    .builder
                                                    .build_int_to_ptr(
                                                        field_addr,
                                                        self.i8ptr_t,
                                                        "field_ptr",
                                                    )
                                                    .map_err(|_| {
                                                        crate::diagnostics::Diagnostic::simple(
                                                            "int_to_ptr failed",
                                                        )
                                                    })?;
                                                // Store element into slot using same logic as object literal
                                                // Use the declared element type to choose storage
                                                let field_ty = match elem_types.get(i) {
                                                    Some(ft) => ft,
                                                    None => {
                                                        return Err(
                                                            crate::diagnostics::Diagnostic::simple(
                                                                "tuple element type missing",
                                                            ),
                                                        );
                                                    }
                                                };
                                                match field_ty {
                                                            crate::types::OatsType::Number => {
                                                                // Coerce to f64 then store into an f64* slot
                                                                let fv = if ev.get_type().is_float_type() {
                                                                    ev.into_float_value()
                                                                } else if ev.get_type().is_int_type() {
                                                                    let iv = ev.into_int_value();
                                                                    self.builder.build_signed_int_to_float(iv, self.f64_t, "i_to_f").map_err(|_| crate::diagnostics::Diagnostic::simple("int->float cast failed"))?
                                                                } else if let Some(fv) = self.coerce_to_f64(ev) {
                                                                    fv
                                                                } else {
                                                                    return Err(crate::diagnostics::Diagnostic::simple("expected numeric value for tuple number element"));
                                                                };
                                                                let f64_ptr_ty = self.context.ptr_type(AddressSpace::default());
                                                                let elem_f64_ptr = self.builder.build_pointer_cast(field_ptr, f64_ptr_ty, "tuple_elem_f64_ptr").map_err(|_| crate::diagnostics::Diagnostic::simple("pointer cast failed"))?;
                                                                let _ = self.builder.build_store(elem_f64_ptr, fv.as_basic_value_enum());
                                                            }
                                                            crate::types::OatsType::Union(_) => {
                                                                // Box union payloads into a heap object and store pointer
                                                                // If ev is float -> union_box_f64, if pointer -> union_box_ptr
                                                                if ev.get_type().is_float_type() {
                                                                    let box_fn = self.get_union_box_f64();
                                                                    let cs2 = self.builder.build_call(box_fn, &[ev.into()], "union_box_f64_ctor");
                                                                    if let Ok(cs2) = cs2 && let inkwell::Either::Left(bv2) = cs2.try_as_basic_value() {
                                                                        let boxed_ptr = bv2.into_pointer_value();
                                                                        let boxed_bv = inkwell::values::BasicValueEnum::PointerValue(boxed_ptr);
                                                                        let _ = self.builder.build_store(field_ptr, boxed_bv);
                                                                        let rc_inc = self.get_rc_inc();
                                                                        let _ = self.builder.build_call(rc_inc, &[boxed_ptr.into()], "rc_inc_field");
                                                                    }
                                                                } else if ev.get_type().is_pointer_type() {
                                                                    let box_fn = self.get_union_box_ptr();
                                                                    let cs2 = self.builder.build_call(box_fn, &[ev.into()], "union_box_ptr_ctor");
                                                                    if let Ok(cs2) = cs2 && let inkwell::Either::Left(bv2) = cs2.try_as_basic_value() {
                                                                        let boxed_ptr = bv2.into_pointer_value();
                                                                        let boxed_bv = inkwell::values::BasicValueEnum::PointerValue(boxed_ptr);
                                                                        let _ = self.builder.build_store(field_ptr, boxed_bv);
                                                                        let rc_inc = self.get_rc_inc();
                                                                        let _ = self.builder.build_call(rc_inc, &[boxed_ptr.into()], "rc_inc_field");
                                                                    }
                                                                } else {
                                                                    return Err(crate::diagnostics::Diagnostic::simple("unsupported tuple union element type at init"));
                                                                }
                                                            }
                                                            // Pointer-like types: store pointer and rc_inc
                                                            crate::types::OatsType::String
                                                            | crate::types::OatsType::NominalStruct(_)
                                                            | crate::types::OatsType::Array(_)
                                                            | crate::types::OatsType::Option(_)
                                                            | crate::types::OatsType::Weak(_)
                                                            | crate::types::OatsType::Promise(_) => {
                                                                if let inkwell::values::BasicValueEnum::PointerValue(p) = ev {
                                                                    let _ = self.builder.build_store(field_ptr, ev);
                                                                    let rc_inc = self.get_rc_inc();
                                                                    let _ = self.builder.build_call(rc_inc, &[p.into()], "rc_inc_field");
                                                                } else {
                                                                    return Err(crate::diagnostics::Diagnostic::simple("expected pointer for tuple reference element"));
                                                                }
                                                            }
                                                            _ => {
                                                                // Fallback: store as is (covers booleans/int where appropriate)
                                                                let _ = self.builder.build_store(field_ptr, ev);
                                                            }
                                                        }
                                            } else {
                                                return Err(
                                                    crate::diagnostics::Diagnostic::simple(
                                                        "elided tuple element not supported",
                                                    ),
                                                );
                                            }
                                        }

                                        // Create an alloca for the local and store the tuple pointer
                                        let allocated_ty = self.i8ptr_t.as_basic_type_enum();
                                        let alloca =
                                            match self.builder.build_alloca(allocated_ty, &name) {
                                                Ok(a) => a,
                                                Err(_) => {
                                                    crate::diagnostics::emit_diagnostic(
                                                        &crate::diagnostics::Diagnostic::simple(
                                                            "alloca failed for local variable",
                                                        ),
                                                        Some(self.source),
                                                    );
                                                    return Ok(false);
                                                }
                                            };
                                        let _ = self
                                            .builder
                                            .build_store(alloca, malloc_ret.as_basic_value_enum());
                                        // increment rc for stored pointer
                                        let rc_inc = self.get_rc_inc();
                                        let _ = self.builder.build_call(
                                            rc_inc,
                                            &[malloc_ret.into()],
                                            "rc_inc_local",
                                        );

                                        // mark initialized and insert local with nominal pointing to tuple generated name
                                        self.insert_local_current_scope(
                                            _locals_stack,
                                            name,
                                            alloca,
                                            allocated_ty,
                                            true,
                                            false,
                                            declared_is_weak,
                                            Some(gen_name),
                                        );
                                        // done handling tuple init
                                        continue;
                                    } else {
                                        crate::diagnostics::emit_diagnostic(
                                            &crate::diagnostics::Diagnostic::simple(
                                                "malloc failed for tuple allocation",
                                            ),
                                            Some(self.source),
                                        );
                                        return Ok(false);
                                    }
                                }

                                if let Ok(mut val) =
                                    self.lower_expr(init, _function, _param_map, _locals_stack)
                                {
                                    // If declared as a Union that includes pointer-like
                                    // parts we must pick a stable ABI slot. The
                                    // strategy here is:
                                    // - If any union arm is pointer-like (string,
                                    //   array, nominal struct, promise), represent the
                                    //   union in the ABI as an `i8*` pointer.
                                    // - Numeric-only unions are represented as `f64`.
                                    //
                                    // When a pointer-like union receives a numeric
                                    // initializer, we "box" it into a heap object
                                    // using runtime helpers (`union_box_f64` /
                                    // `union_box_ptr`) so the stored slot is a
                                    // pointer. Boxing keeps the runtime layout
                                    // uniform for pointer-like unions and simplifies
                                    // RC handling.
                                    let mut allocated_ty = val.get_type().as_basic_type_enum();
                                    if let Some(crate::types::OatsType::Union(parts)) =
                                        &declared_union
                                    {
                                        let any_ptr = parts.iter().any(|p| {
                                            matches!(
                                                p,
                                                crate::types::OatsType::String
                                                    | crate::types::OatsType::NominalStruct(_)
                                                    | crate::types::OatsType::Array(_)
                                                    | crate::types::OatsType::Promise(_)
                                            )
                                        });
                                        if any_ptr {
                                            // Ensure val is boxed pointer (union_box_f64 or union_box_ptr)
                                            if val.get_type().is_float_type() {
                                                let box_fn = self.get_union_box_f64();
                                                let cs = self.builder.build_call(
                                                    box_fn,
                                                    &[val.into()],
                                                    "union_box_f64_ctor",
                                                );
                                                if let Ok(cs) = cs
                                                    && let inkwell::Either::Left(bv) =
                                                        cs.try_as_basic_value()
                                                {
                                                    val = bv;
                                                }
                                            } else if let BasicValueEnum::PointerValue(pv) = val {
                                                let box_fn = self.get_union_box_ptr();
                                                let cs = self.builder.build_call(
                                                    box_fn,
                                                    &[pv.into()],
                                                    "union_box_ptr_ctor",
                                                );
                                                if let Ok(cs) = cs
                                                    && let inkwell::Either::Left(bv) =
                                                        cs.try_as_basic_value()
                                                {
                                                    val = bv;
                                                }
                                            }
                                            allocated_ty = self.i8ptr_t.as_basic_type_enum();
                                        } else {
                                            // union of only numbers -> use f64 slot
                                            allocated_ty = self.f64_t.as_basic_type_enum();
                                            if val.get_type().is_int_type() {
                                                // coerce int to float
                                                if let BasicValueEnum::IntValue(iv) = val
                                                    && let Ok(fv_val) =
                                                        self.builder.build_signed_int_to_float(
                                                            iv, self.f64_t, "i2f",
                                                        )
                                                {
                                                    val =
                                                        inkwell::values::BasicValueEnum::FloatValue(
                                                            fv_val,
                                                        );
                                                }
                                            }
                                        }
                                    }

                                    let alloca =
                                        match self.builder.build_alloca(allocated_ty, &name) {
                                            Ok(a) => a,
                                            Err(_) => {
                                                crate::diagnostics::emit_diagnostic(
                                                    &crate::diagnostics::Diagnostic::simple(
                                                        "alloca failed for local variable",
                                                    ),
                                                    Some(self.source),
                                                );
                                                return Ok(false);
                                            }
                                        };
                                    // store lowered (possibly boxed) value
                                    let _ = self.builder.build_store(alloca, val);
                                    // If this slot is a pointer-like ABI slot then we
                                    // must increment the reference count for the
                                    // stored value. The runtime's `rc_inc` helper
                                    // resolves string-data vs object-base pointers and
                                    // performs an atomic increment on the low-32-bit
                                    // RC field. This keeps local ownership
                                    // semantics consistent for pointers.
                                    if let inkwell::types::BasicTypeEnum::PointerType(_) =
                                        allocated_ty
                                        && let BasicValueEnum::PointerValue(pv) = val
                                    {
                                        let rc_inc = self.get_rc_inc();
                                        let _ = self.builder.build_call(
                                            rc_inc,
                                            &[pv.into()],
                                            "rc_inc_local",
                                        );
                                    }
                                    // mark initialized in locals; is_const=false by default
                                    self.insert_local_current_scope(
                                        _locals_stack,
                                        name,
                                        alloca,
                                        allocated_ty,
                                        true,
                                        false,
                                        declared_is_weak,
                                        declared_nominal.clone(),
                                    );
                                }
                            } else {
                                // No initializer: create an uninitialized slot
                                // using `i64` as a conservative ABI choice. The
                                // slot will be marked uninitialized so further
                                // lowering can detect reads of uninitialized
                                // locals and emit diagnostics if necessary.
                                let ty = self.i64_t.as_basic_type_enum();
                                let alloca = match self.builder.build_alloca(ty, &name) {
                                    Ok(a) => a,
                                    Err(_) => {
                                        crate::diagnostics::emit_diagnostic(
                                            &crate::diagnostics::Diagnostic::simple(
                                                "alloca failed for uninitialized var",
                                            ),
                                            Some(self.source),
                                        );
                                        return Ok(false);
                                    }
                                };
                                self.insert_local_current_scope(
                                    _locals_stack,
                                    name,
                                    alloca,
                                    ty,
                                    false,
                                    false,
                                    declared_is_weak,
                                    declared_nominal.clone(),
                                );
                            }
                        }
                    }
                }
                Ok(false)
            }
            deno_ast::swc::ast::Stmt::Expr(expr_stmt) => {
                // Evaluate expression for side-effects. If lowering fails,
                // emit a diagnostic and continue rather than aborting the
                // whole compile. This avoids silent empty functions while
                // keeping compilation resilient for constructs like
                // `this.x = ...` during constructor emission.
                match self.lower_expr(&expr_stmt.expr, _function, _param_map, _locals_stack) {
                    Ok(_val) => { /* success, continue */ }
                    Err(d) => {
                        crate::diagnostics::emit_diagnostic(&d, Some(self.source));
                        // don't propagate error further; continue lowering
                    }
                }
                Ok(false)
            }
            deno_ast::swc::ast::Stmt::Return(ret) => {
                // Lower return expression, emit rc_decs for locals then return.
                //
                // Important: we must run `emit_rc_dec_for_locals` before
                // emitting the `ret` instruction so that any pointer-valued
                // locals have their RC decremented and destructors (if any)
                // run before the caller resumes. This models deterministic
                // destruction and matches the runtime's RC expectations.
                if let Some(arg) = &ret.arg {
                    if let Ok(val) = self.lower_expr(arg, _function, _param_map, _locals_stack) {
                        // emit rc decs for locals
                        self.emit_rc_dec_for_locals(_locals_stack);
                        // build return with the lowered value
                        let _ = self.builder.build_return(Some(&val));
                        return Ok(true);
                    }
                } else {
                    self.emit_rc_dec_for_locals(_locals_stack);
                    let _ = self.builder.build_return(None);
                    return Ok(true);
                }
                Ok(false)
            }
            deno_ast::swc::ast::Stmt::Break(_break_stmt) => {
                // Break statement: jump to the loop's break (exit) block.
                //
                // We must emit RC decrements for locals before branching out
                // of the current lexical scope because `break` logically
                // exits the scope and any pointer-valued locals need their
                // reference counts adjusted. `emit_rc_dec_for_locals` is the
                // helper that lowers these decrements.
                if let Some(loop_ctx) = self.loop_context_stack.borrow().last().copied() {
                    // TODO: Support labeled breaks by resolving the label to the
                    // target loop context. Currently only unlabeled `break`
                    // statements are implemented. See issue #TODO (add issue)
                    // for full labeled-break semantics and tests.

                    // Emit RC decrements for locals owned by the current loop
                    // (only scopes starting at `locals_start`). This avoids
                    // decreffing outer scopes that remain live after the break.
                    self._emit_rc_dec_for_locals_from(_locals_stack, loop_ctx.locals_start);

                    // Branch to break block
                    let _ = self
                        .builder
                        .build_unconditional_branch(loop_ctx.break_block);

                    Ok(true) // break terminates the current block
                } else {
                    // Break outside of a loop is a semantic error. Current
                    // behavior is to ignore it; a diagnostic should be emitted
                    // instead. See issue #TODO (add issue) to track error
                    // reporting for invalid control-flow statements.
                    Ok(false)
                }
            }
            deno_ast::swc::ast::Stmt::Continue(_continue_stmt) => {
                // Continue statement: jump to the loop's continue block.
                //
                // Like `break`, a `continue` may exit the current lexical
                // scope. Emit RC decrements for locals to preserve correct
                // lifetime semantics before transferring control.
                if let Some(loop_ctx) = self.loop_context_stack.borrow().last().copied() {
                    // TODO: Support labeled continues by resolving the label to
                    // the target loop context. Currently only unlabeled
                    // `continue` statements are implemented. See issue #TODO
                    // (add issue) for full labeled-continue semantics and tests.

                    // Emit RC decrements for locals owned by the current loop
                    // (only scopes starting at `locals_start`). This avoids
                    // decreffing outer scopes that remain live after the continue.
                    self._emit_rc_dec_for_locals_from(_locals_stack, loop_ctx.locals_start);

                    // Branch to continue block
                    let _ = self
                        .builder
                        .build_unconditional_branch(loop_ctx.continue_block);

                    Ok(true) // continue terminates the current block
                } else {
                    // Continue outside of loop is a semantic error. Currently the
                    // implementation ignores it; it should instead emit a
                    // diagnostic. See issue #TODO (add issue) to track reporting
                    // of control-flow errors.
                    Ok(false)
                }
            }
            deno_ast::swc::ast::Stmt::Block(block) => {
                // new scope
                _locals_stack.push(HashMap::new());
                let terminated =
                    self.lower_stmts(&block.stmts, _function, _param_map, _locals_stack)?;
                // pop scope
                _locals_stack.pop();
                Ok(terminated)
            }
            // Handle if statements: `if (cond) { ... } else { ... }`
            deno_ast::swc::ast::Stmt::If(ifstmt) => {
                // Lower condition
                if let Ok(cond_val) =
                    self.lower_expr(&ifstmt.test, _function, _param_map, _locals_stack)
                {
                    if let Some(cond_bool) = self.to_condition_i1(cond_val) {
                        let then_bb = self.context.append_basic_block(_function, "if.then");
                        let else_bb = self.context.append_basic_block(_function, "if.else");
                        let merge_bb = self.context.append_basic_block(_function, "if.merge");

                        // Conditional branch
                        let _ = self
                            .builder
                            .build_conditional_branch(cond_bool, then_bb, else_bb);

                        // Build then block
                        self.builder.position_at_end(then_bb);
                        let then_terminated: bool = match &*ifstmt.cons {
                            deno_ast::swc::ast::Stmt::Block(block) => self.lower_stmts(
                                &block.stmts,
                                _function,
                                _param_map,
                                _locals_stack,
                            )?,
                            _ => {
                                self.lower_stmt(&ifstmt.cons, _function, _param_map, _locals_stack)?
                            }
                        };
                        if !then_terminated {
                            let _ = self.builder.build_unconditional_branch(merge_bb);
                        }

                        // Build else block
                        self.builder.position_at_end(else_bb);
                        let else_terminated: bool = if let Some(alt) = &ifstmt.alt {
                            match &**alt {
                                deno_ast::swc::ast::Stmt::Block(block) => self.lower_stmts(
                                    &block.stmts,
                                    _function,
                                    _param_map,
                                    _locals_stack,
                                )?,
                                _ => self.lower_stmt(alt, _function, _param_map, _locals_stack)?,
                            }
                        } else {
                            false // no else branch
                        };
                        if !else_terminated {
                            let _ = self.builder.build_unconditional_branch(merge_bb);
                        }

                        // Position at merge
                        self.builder.position_at_end(merge_bb);

                        // If both branches terminated, the if statement terminates
                        Ok(then_terminated && else_terminated)
                    } else {
                        Ok(false)
                    }
                } else {
                    Ok(false)
                }
            }
            // Minimal ForOf lowering: handle `for (let v of iterable) { body }`.
            deno_ast::swc::ast::Stmt::ForOf(forof) => {
                // Only handle the case where the left-hand side is a var
                // declaration (e.g., `for (let v of rhs)`).
                // forof.left can be either a VarDecl or a Pat; we match on VarDecl
                if let deno_ast::swc::ast::ForHead::VarDecl(var_decl) = &forof.left
                    && var_decl.decls.len() == 1
                {
                    let decl = &var_decl.decls[0];
                    if let deno_ast::swc::ast::Pat::Ident(ident) = &decl.name {
                        let loop_var_name = ident.id.sym.to_string();
                        // capture nominal type for the loop variable if annotated
                        let mut declared_nominal: Option<String> = None;
                        if let Some(type_ann) = &ident.type_ann
                            && let Some(mapped) = crate::types::map_ts_type(&type_ann.type_ann)
                            && let crate::types::OatsType::NominalStruct(n) = &mapped
                        {
                            declared_nominal = Some(n.clone());
                        }
                        // Lower RHS (iterable)
                        if let Ok(iter_val) =
                            self.lower_expr(&forof.right, _function, _param_map, _locals_stack)
                            && let BasicValueEnum::PointerValue(arr_ptr) = iter_val
                        {
                            // create index
                            let idx_alloca = match self.builder.build_alloca(self.i64_t, "for_idx")
                            {
                                Ok(a) => a,
                                Err(_) => {
                                    crate::diagnostics::emit_diagnostic(
                                        &crate::diagnostics::Diagnostic::simple(
                                            "alloca failed for for-loop index",
                                        ),
                                        Some(self.source),
                                    );
                                    return Ok(false);
                                }
                            };
                            let zero = self.i64_t.const_int(0, false);
                            let _ = self.builder.build_store(idx_alloca, zero);

                            // create blocks
                            let loop_cond_bb =
                                self.context.append_basic_block(_function, "for.cond");
                            let loop_body_bb =
                                self.context.append_basic_block(_function, "for.body");
                            let loop_after_bb =
                                self.context.append_basic_block(_function, "for.after");

                            // Push loop context (for-of: continue jumps to condition, break to after)
                            self.loop_context_stack.borrow_mut().push(
                                crate::codegen::LoopContext {
                                    continue_block: loop_cond_bb,
                                    break_block: loop_after_bb,
                                    locals_start: _locals_stack.len(),
                                },
                            );

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
                                    Err(_) => return Ok(false),
                                };
                                if let inkwell::Either::Left(bv) = cs.try_as_basic_value() {
                                    let len = bv.into_int_value();
                                    let cur_idx = match self
                                        .builder
                                        .build_load(self.i64_t, idx_alloca, "idx_load")
                                    {
                                        Ok(v) => v.into_int_value(),
                                        Err(_) => return Ok(false),
                                    };
                                    let cmp = match self.builder.build_int_compare(
                                        inkwell::IntPredicate::ULT,
                                        cur_idx,
                                        len,
                                        "cmp_idx",
                                    ) {
                                        Ok(v) => v,
                                        Err(_) => return Ok(false),
                                    };
                                    if self
                                        .builder
                                        .build_conditional_branch(cmp, loop_body_bb, loop_after_bb)
                                        .is_err()
                                    {
                                        return Ok(false);
                                    }
                                } else if self
                                    .builder
                                    .build_unconditional_branch(loop_after_bb)
                                    .is_err()
                                {
                                    return Ok(false);
                                }
                            } else if self
                                .builder
                                .build_unconditional_branch(loop_after_bb)
                                .is_err()
                            {
                                return Ok(false);
                            }

                            // body
                            self.builder.position_at_end(loop_body_bb);
                            let cur_idx = match self.builder.build_load(
                                self.i64_t,
                                idx_alloca,
                                "idx_load2",
                            ) {
                                Ok(v) => v.into_int_value(),
                                Err(_) => return Ok(false),
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
                                    Err(_) => return Ok(false),
                                };
                                if let inkwell::Either::Left(bv) = cs.try_as_basic_value() {
                                    let ty = bv.get_type().as_basic_type_enum();
                                    let alloca = match self.builder.build_alloca(ty, &loop_var_name)
                                    {
                                        Ok(a) => a,
                                        Err(_) => {
                                            crate::diagnostics::emit_diagnostic(
                                                &crate::diagnostics::Diagnostic::simple(
                                                    "alloca failed for loop variable",
                                                ),
                                                Some(self.source),
                                            );
                                            return Ok(false);
                                        }
                                    };
                                    let _ = self.builder.build_store(alloca, bv);
                                    self.insert_local_current_scope(
                                        _locals_stack,
                                        loop_var_name.clone(),
                                        alloca,
                                        ty,
                                        true,
                                        false,
                                        false,
                                        declared_nominal.clone(),
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
                                    Err(_) => return Ok(false),
                                };
                                if let inkwell::Either::Left(bv) = cs.try_as_basic_value() {
                                    let pv = bv.into_pointer_value();
                                    let ty = pv.get_type().as_basic_type_enum();
                                    let alloca = match self.builder.build_alloca(ty, &loop_var_name)
                                    {
                                        Ok(a) => a,
                                        Err(_) => {
                                            crate::diagnostics::emit_diagnostic(
                                                &crate::diagnostics::Diagnostic::simple(
                                                    "alloca failed for loop variable (ptr)",
                                                ),
                                                Some(self.source),
                                            );
                                            return Ok(false);
                                        }
                                    };
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
                                        false,
                                        declared_nominal.clone(),
                                    );
                                }
                            }

                            // Lower the loop body: handle a Block or single statement
                            let terminated = match &*forof.body {
                                deno_ast::swc::ast::Stmt::Block(block) => self.lower_stmts(
                                    &block.stmts,
                                    _function,
                                    _param_map,
                                    _locals_stack,
                                )?,
                                other => {
                                    self.lower_stmt(other, _function, _param_map, _locals_stack)?
                                }
                            };
                            let cur_idx2 = match self.builder.build_load(
                                self.i64_t,
                                idx_alloca,
                                "idx_load3",
                            ) {
                                Ok(v) => v.into_int_value(),
                                Err(_) => return Ok(false),
                            };
                            let one = self.i64_t.const_int(1, false);
                            let next_idx =
                                match self.builder.build_int_add(cur_idx2, one, "idx_next") {
                                    Ok(v) => v,
                                    Err(_) => return Ok(false),
                                };
                            let _ = self.builder.build_store(idx_alloca, next_idx);
                            if !terminated
                                && self
                                    .builder
                                    .build_unconditional_branch(loop_cond_bb)
                                    .is_err()
                            {
                                self.loop_context_stack.borrow_mut().pop();
                                return Ok(false);
                            }
                            self.builder.position_at_end(loop_after_bb);

                            // Pop loop context
                            self.loop_context_stack.borrow_mut().pop();

                            return Ok(terminated);
                        }
                    }
                }
                Ok(false)
            }
            // Handle regular for loops: `for (init; test; update) { body }`
            deno_ast::swc::ast::Stmt::For(forstmt) => {
                // Push new scope for the loop (init vars live in this scope)
                _locals_stack.push(HashMap::new());

                // Lower init (can be VarDecl or Expr)
                if let Some(init) = &forstmt.init {
                    match init {
                        deno_ast::swc::ast::VarDeclOrExpr::VarDecl(var_decl) => {
                            // Handle var declaration (e.g., let i = 0)
                            let _ = self.lower_stmt(
                                &deno_ast::swc::ast::Stmt::Decl(deno_ast::swc::ast::Decl::Var(
                                    Box::new((**var_decl).clone()),
                                )),
                                _function,
                                _param_map,
                                _locals_stack,
                            );
                        }
                        deno_ast::swc::ast::VarDeclOrExpr::Expr(expr) => {
                            // Handle expression (e.g., i = 0)
                            let _ = self.lower_expr(expr, _function, _param_map, _locals_stack);
                        }
                    }
                }

                // Create basic blocks
                let loop_cond_bb = self.context.append_basic_block(_function, "for.cond");
                let loop_body_bb = self.context.append_basic_block(_function, "for.body");
                let loop_incr_bb = self.context.append_basic_block(_function, "for.incr");
                let loop_after_bb = self.context.append_basic_block(_function, "for.after");

                // Push loop context (continue jumps to increment, break jumps to after)
                self.loop_context_stack
                    .borrow_mut()
                    .push(crate::codegen::LoopContext {
                        continue_block: loop_incr_bb,
                        break_block: loop_after_bb,
                        locals_start: _locals_stack.len(),
                    });

                // Branch to condition
                let _ = self.builder.build_unconditional_branch(loop_cond_bb);

                // Build condition block
                self.builder.position_at_end(loop_cond_bb);
                if let Some(test) = &forstmt.test {
                    if let Ok(cond_val) =
                        self.lower_expr(test, _function, _param_map, _locals_stack)
                    {
                        // Coerce to i1 boolean
                        if let Some(cond_bool) = self.to_condition_i1(cond_val) {
                            let _ = self.builder.build_conditional_branch(
                                cond_bool,
                                loop_body_bb,
                                loop_after_bb,
                            );
                        } else {
                            // Failed to coerce, bail out
                            _locals_stack.pop();
                            return Ok(false);
                        }
                    } else {
                        // Failed to lower test, bail out
                        _locals_stack.pop();
                        return Ok(false);
                    }
                } else {
                    // No test means infinite loop: always branch to body
                    let _ = self.builder.build_unconditional_branch(loop_body_bb);
                }

                // Build body block
                self.builder.position_at_end(loop_body_bb);
                let body_terminated: bool = match &*forstmt.body {
                    deno_ast::swc::ast::Stmt::Block(block) => {
                        self.lower_stmts(&block.stmts, _function, _param_map, _locals_stack)?
                    }
                    _ => self.lower_stmt(&forstmt.body, _function, _param_map, _locals_stack)?,
                };

                // If body didn't terminate, branch to increment
                if !body_terminated {
                    let _ = self.builder.build_unconditional_branch(loop_incr_bb);
                }

                // Build increment block
                self.builder.position_at_end(loop_incr_bb);
                if let Some(update) = &forstmt.update {
                    let _ = self.lower_expr(update, _function, _param_map, _locals_stack);
                }
                let _ = self.builder.build_unconditional_branch(loop_cond_bb);

                // Position after loop
                self.builder.position_at_end(loop_after_bb);

                // Pop loop context
                self.loop_context_stack.borrow_mut().pop();

                // Pop loop scope
                _locals_stack.pop();

                Ok(false) // loops don't terminate unless body always returns
            }
            // Handle while loops: `while (test) { body }`
            deno_ast::swc::ast::Stmt::While(while_stmt) => {
                // Create basic blocks
                let loop_cond_bb = self.context.append_basic_block(_function, "while.cond");
                let loop_body_bb = self.context.append_basic_block(_function, "while.body");
                let loop_after_bb = self.context.append_basic_block(_function, "while.after");

                // Push loop context (continue jumps to condition, break jumps to after)
                self.loop_context_stack
                    .borrow_mut()
                    .push(crate::codegen::LoopContext {
                        continue_block: loop_cond_bb,
                        break_block: loop_after_bb,
                        locals_start: _locals_stack.len(),
                    });

                // Branch to condition
                let _ = self.builder.build_unconditional_branch(loop_cond_bb);

                // Build condition block
                self.builder.position_at_end(loop_cond_bb);
                if let Ok(cond_val) =
                    self.lower_expr(&while_stmt.test, _function, _param_map, _locals_stack)
                {
                    // Coerce to i1 boolean
                    if let Some(cond_bool) = self.to_condition_i1(cond_val) {
                        let _ = self.builder.build_conditional_branch(
                            cond_bool,
                            loop_body_bb,
                            loop_after_bb,
                        );
                    } else {
                        self.loop_context_stack.borrow_mut().pop();
                        return Ok(false);
                    }
                } else {
                    self.loop_context_stack.borrow_mut().pop();
                    return Ok(false);
                }

                // Build body block
                self.builder.position_at_end(loop_body_bb);
                let body_terminated: bool = match &*while_stmt.body {
                    deno_ast::swc::ast::Stmt::Block(block) => {
                        self.lower_stmts(&block.stmts, _function, _param_map, _locals_stack)?
                    }
                    _ => self.lower_stmt(&while_stmt.body, _function, _param_map, _locals_stack)?,
                };

                // If body didn't terminate, branch back to condition
                if !body_terminated {
                    let _ = self.builder.build_unconditional_branch(loop_cond_bb);
                }

                // Pop loop context
                self.loop_context_stack.borrow_mut().pop();

                // Position after loop
                self.builder.position_at_end(loop_after_bb);

                Ok(false) // while loops don't terminate unless body always returns
            }
            // Handle do-while loops: `do { body } while (test)`
            deno_ast::swc::ast::Stmt::DoWhile(dowhile_stmt) => {
                // Create basic blocks
                let loop_body_bb = self.context.append_basic_block(_function, "dowhile.body");
                let loop_cond_bb = self.context.append_basic_block(_function, "dowhile.cond");
                let loop_after_bb = self.context.append_basic_block(_function, "dowhile.after");

                // Push loop context (continue jumps to condition, break jumps to after)
                self.loop_context_stack
                    .borrow_mut()
                    .push(crate::codegen::LoopContext {
                        continue_block: loop_cond_bb,
                        break_block: loop_after_bb,
                        locals_start: _locals_stack.len(),
                    });

                // Branch directly to body (execute at least once)
                let _ = self.builder.build_unconditional_branch(loop_body_bb);

                // Build body block
                self.builder.position_at_end(loop_body_bb);
                let body_terminated: bool = match &*dowhile_stmt.body {
                    deno_ast::swc::ast::Stmt::Block(block) => {
                        self.lower_stmts(&block.stmts, _function, _param_map, _locals_stack)?
                    }
                    _ => {
                        self.lower_stmt(&dowhile_stmt.body, _function, _param_map, _locals_stack)?
                    }
                };

                // If body didn't terminate, branch to condition check
                if !body_terminated {
                    let _ = self.builder.build_unconditional_branch(loop_cond_bb);
                }

                // Build condition block
                self.builder.position_at_end(loop_cond_bb);
                if let Ok(cond_val) =
                    self.lower_expr(&dowhile_stmt.test, _function, _param_map, _locals_stack)
                {
                    // Coerce to i1 boolean
                    if let Some(cond_bool) = self.to_condition_i1(cond_val) {
                        let _ = self.builder.build_conditional_branch(
                            cond_bool,
                            loop_body_bb, // Loop back to body
                            loop_after_bb,
                        );
                    } else {
                        self.loop_context_stack.borrow_mut().pop();
                        return Ok(false);
                    }
                } else {
                    self.loop_context_stack.borrow_mut().pop();
                    return Ok(false);
                }

                // Pop loop context
                self.loop_context_stack.borrow_mut().pop();

                // Position after loop
                self.builder.position_at_end(loop_after_bb);

                Ok(false) // do-while loops don't terminate unless body always returns
            }
            _ => Ok(false),
        }
    }
}
