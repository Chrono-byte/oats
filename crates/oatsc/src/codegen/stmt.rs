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
    Option<crate::types::OatsType>,
);
// A stack of per-scope local maps.
//
// Each entry in the vector is a HashMap representing a lexical scope's
// locals. The maps store `LocalEntry` tuples describing the alloca pointer,
// the ABI type of the slot, initialization flag, const flag, whether the
// local is a Weak<T> (affects RC semantics), an optional nominal type
// name used to guide member access lowering, and an optional OatsType
// for union tracking.
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

                            // Language Spec v2: `let` is immutable by default. Support
                            // `let mut` by consulting the parser-computed set of
                            // var declarations that contained a `mut` token. This
                            // avoids scanning the source text at lowering time and
                            // is resilient to `mut` inside strings/comments.
                            let is_mut_decl =
                                if matches!(var_decl.kind, deno_ast::swc::ast::VarDeclKind::Let) {
                                    let start = var_decl.span.lo.0 as usize;
                                    let end = var_decl.span.hi.0 as usize;
                                    // Check parser-computed set first, then fall back to the
                                    // original source-scan heuristic if necessary. This helps
                                    // cover edge-cases where spans or byte offsets differ.
                                    if self.mut_var_decls.contains(&start) {
                                        true
                                    } else if end > start && end <= self.source.len() {
                                        let slice = &self.source[start..end];
                                        slice.contains("mut ")
                                            || slice.contains("mut\t")
                                            || slice.contains("mut\n")
                                    } else {
                                        false
                                    }
                                } else {
                                    false
                                };

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
                            eprintln!(
                                "[debug] Processing var decl: {} kind={:?}",
                                name, var_decl.kind
                            );
                            if let Some(init) = &decl.init {
                                // If this is a `const` declaration, attempt compile-time evaluation
                                if matches!(var_decl.kind, deno_ast::swc::ast::VarDeclKind::Const) {
                                    let span_start = var_decl.span.lo.0 as usize;
                                    // Borrow const_items immutably for evaluation
                                    let const_map = self.const_items.borrow();
                                    match crate::codegen::const_eval::eval_const_expr(
                                        init, span_start, &const_map,
                                    ) {
                                        Ok(cv) => {
                                            // Only treat primitives as const; arrays and objects need runtime allocation
                                            if matches!(
                                                cv,
                                                crate::codegen::const_eval::ConstValue::Number(_)
                                                    | crate::codegen::const_eval::ConstValue::Bool(
                                                        _
                                                    )
                                                    | crate::codegen::const_eval::ConstValue::Str(
                                                        _
                                                    )
                                            ) {
                                                drop(const_map);
                                                // Insert into compile-time const map keyed by name
                                                self.const_items
                                                    .borrow_mut()
                                                    .insert(name.clone(), cv);
                                                // For consts we do not lower the initializer further; the
                                                // lowered uses will be replaced by LLVM constants later.
                                                continue;
                                            } else {
                                                drop(const_map);
                                                // For arrays/objects, fall through to normal lowering
                                            }
                                        }
                                        Err(_diag) => {
                                            // If const evaluation fails, fall through to normal lowering
                                            drop(const_map);
                                        }
                                    }
                                }
                                // Infer a local OatsType from the initializer expression
                                // so we can record it on the local entry. This helps
                                // call-site monomorphization to see concrete types for
                                // locals (e.g., `Array(String)`) when no explicit
                                // annotation was provided.
                                let init_inferred = crate::types::infer_type(None, Some(init));
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
                                // an explicit type annotation. Also pre-allocate a local
                                // slot for the declared variable before lowering the
                                // initializer so subsequent statements in the same
                                // function can resolve the local even if initializer
                                // lowering emits diagnostics or performs nested
                                // emissions that temporarily disrupt symbol lookup.
                                if let deno_ast::swc::ast::Expr::New(new_expr) = &**init
                                    && let deno_ast::swc::ast::Expr::Ident(ident) =
                                        &*new_expr.callee
                                {
                                    declared_nominal = Some(ident.sym.to_string());

                                    // Pre-allocate an i8* slot and insert the local into
                                    // the current scope marked as uninitialized. This
                                    // ensures `find_local` can discover the nominal
                                    // type for member-call lowering that appears
                                    // after this declaration.
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
                                    // Insert local early as uninitialized; we'll store to it below
                                    // For `var` declarations, insert into function scope (hoisted)
                                    // For `let`/`const` declarations, insert into current scope (block-scoped)
                                    if matches!(var_decl.kind, deno_ast::swc::ast::VarDeclKind::Var)
                                    {
                                        self.insert_local_function_scope(
                                            _locals_stack,
                                            crate::codegen::helpers::LocalVarInfo {
                                                name: name.clone(),
                                                ptr: alloca,
                                                ty: allocated_ty,
                                                initialized: false,
                                                // var declarations are mutable
                                                is_const: false,
                                                is_weak: declared_is_weak,
                                                nominal: declared_nominal.clone(),
                                                oats_type: declared_union.clone(),
                                            },
                                        );
                                    } else {
                                        self.insert_local_current_scope(
                                            _locals_stack,
                                            crate::codegen::helpers::LocalVarInfo {
                                                name: name.clone(),
                                                ptr: alloca,
                                                ty: allocated_ty,
                                                initialized: false,
                                                is_const: matches!(
                                                    var_decl.kind,
                                                    deno_ast::swc::ast::VarDeclKind::Const
                                                ) || !is_mut_decl,
                                                is_weak: declared_is_weak,
                                                nominal: declared_nominal.clone(),
                                                oats_type: declared_union.clone(),
                                            },
                                        );
                                    }
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
                                                                        let rc_inc = self.get_rc_inc();
                                                                        let _ = self.builder.build_call(rc_inc, &[boxed_ptr.into()], "rc_inc_field");
                                                                        let boxed_bv = inkwell::values::BasicValueEnum::PointerValue(boxed_ptr);
                                                                        let _ = self.builder.build_store(field_ptr, boxed_bv);
                                                                        // Note: no rc_dec here as the store takes ownership of the newly boxed object
                                                                    }
                                                                } else if ev.get_type().is_pointer_type() {
                                                                    let box_fn = self.get_union_box_ptr();
                                                                    let cs2 = self.builder.build_call(box_fn, &[ev.into()], "union_box_ptr_ctor");
                                                                    if let Ok(cs2) = cs2 && let inkwell::Either::Left(bv2) = cs2.try_as_basic_value() {
                                                                        let boxed_ptr = bv2.into_pointer_value();
                                                                        let rc_inc = self.get_rc_inc();
                                                                        let _ = self.builder.build_call(rc_inc, &[boxed_ptr.into()], "rc_inc_field");
                                                                        let boxed_bv = inkwell::values::BasicValueEnum::PointerValue(boxed_ptr);
                                                                        let _ = self.builder.build_store(field_ptr, boxed_bv);
                                                                        // Note: no rc_dec here as the store takes ownership of the newly boxed object
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
                                                                    let rc_inc = self.get_rc_inc();
                                                                    let _ = self.builder.build_call(rc_inc, &[p.into()], "rc_inc_field");
                                                                    let _ = self.builder.build_store(field_ptr, ev);
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
                                        // increment rc for stored pointer (unless elided)
                                        if !self.should_elide_rc_for_local(&name) {
                                            let rc_inc = self.get_rc_inc();
                                            let _ = self.builder.build_call(
                                                rc_inc,
                                                &[malloc_ret.into()],
                                                "rc_inc_local",
                                            );
                                        }
                                        let _ = self
                                            .builder
                                            .build_store(alloca, malloc_ret.as_basic_value_enum());
                                        // mark initialized and insert local with nominal pointing to tuple generated name
                                        self.insert_local_current_scope(
                                            _locals_stack,
                                            crate::codegen::helpers::LocalVarInfo {
                                                name: name.clone(),
                                                ptr: alloca,
                                                ty: allocated_ty,
                                                initialized: true,
                                                // If this was a `let` without `mut`, treat as const/immutable
                                                is_const: matches!(
                                                    var_decl.kind,
                                                    deno_ast::swc::ast::VarDeclKind::Const
                                                ) || !is_mut_decl,
                                                is_weak: declared_is_weak,
                                                nominal: Some(gen_name),
                                                oats_type: declared_union.clone(),
                                            },
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

                                // Special case: object literal without explicit type annotation
                                // Generate a nominal name, register fields, and handle like tuple
                                if declared_nominal.is_none()
                                    && let deno_ast::swc::ast::Expr::Object(obj_lit) = &**init
                                {
                                    use deno_ast::swc::ast;
                                    // Generate nominal name for this anonymous object literal
                                    let fname = _function.get_name().to_str().unwrap_or("<fn>");
                                    let gen_name = format!("objlit_{}_{}", fname, name);

                                    // Infer field types from the object literal properties
                                    let mut fields: Vec<(String, crate::types::OatsType)> =
                                        Vec::new();
                                    for prop in &obj_lit.props {
                                        if let ast::PropOrSpread::Prop(prop_box) = prop {
                                            match &**prop_box {
                                                ast::Prop::KeyValue(kv) => {
                                                    if let ast::PropName::Ident(id) = &kv.key {
                                                        let field_name = id.sym.to_string();

                                                        // Try to infer type more intelligently
                                                        let field_ty = if let ast::Expr::Ident(
                                                            val_ident,
                                                        ) = &*kv.value
                                                        {
                                                            // Value is an identifier - look it up in locals
                                                            let val_name =
                                                                val_ident.sym.to_string();
                                                            if let Some((
                                                                _,
                                                                ty,
                                                                _,
                                                                _,
                                                                _,
                                                                nominal,
                                                                _oats_type,
                                                            )) = self.find_local(
                                                                _locals_stack,
                                                                &val_name,
                                                            ) {
                                                                // If it has a nominal type, use that
                                                                if let Some(nom_name) = nominal {
                                                                    crate::types::OatsType::NominalStruct(nom_name)
                                                                } else if ty.is_float_type() {
                                                                    crate::types::OatsType::Number
                                                                } else if ty.is_pointer_type() {
                                                                    crate::types::OatsType::String
                                                                } else {
                                                                    crate::types::OatsType::Number
                                                                }
                                                            } else {
                                                                // Fallback to expression-based inference
                                                                crate::types::infer_type(
                                                                    None,
                                                                    Some(&kv.value),
                                                                )
                                                            }
                                                        } else if let ast::Expr::Object(
                                                            nested_obj,
                                                        ) = &*kv.value
                                                        {
                                                            // Value is an inline object literal - generate a nominal name for it
                                                            let nested_name = format!(
                                                                "objlit_{}_{}_{}",
                                                                fname, name, field_name
                                                            );

                                                            // Infer its fields recursively
                                                            let mut nested_fields: Vec<(
                                                                String,
                                                                crate::types::OatsType,
                                                            )> = Vec::new();
                                                            for nested_prop in &nested_obj.props {
                                                                if let ast::PropOrSpread::Prop(
                                                                    nested_prop_box,
                                                                ) = nested_prop
                                                                    && let ast::Prop::KeyValue(
                                                                        nested_kv,
                                                                    ) = &**nested_prop_box
                                                                    && let ast::PropName::Ident(
                                                                        nested_id,
                                                                    ) = &nested_kv.key
                                                                {
                                                                    let nested_field_name =
                                                                        nested_id.sym.to_string();
                                                                    let nested_field_ty =
                                                                        crate::types::infer_type(
                                                                            None,
                                                                            Some(&nested_kv.value),
                                                                        );
                                                                    nested_fields.push((
                                                                        nested_field_name,
                                                                        nested_field_ty,
                                                                    ));
                                                                }
                                                            }

                                                            // Register the nested object's fields
                                                            if !nested_fields.is_empty() {
                                                                self.class_fields
                                                                    .borrow_mut()
                                                                    .insert(
                                                                        nested_name.clone(),
                                                                        nested_fields,
                                                                    );
                                                            }

                                                            crate::types::OatsType::NominalStruct(
                                                                nested_name,
                                                            )
                                                        } else {
                                                            // Not an identifier or object, use expression-based inference
                                                            crate::types::infer_type(
                                                                None,
                                                                Some(&kv.value),
                                                            )
                                                        };

                                                        fields.push((field_name.clone(), field_ty));
                                                    }
                                                }
                                                ast::Prop::Assign(assign) => {
                                                    // Shorthand property { x } - infer from identifier
                                                    let field_name = assign.key.sym.to_string();
                                                    // Try to infer type from param or local
                                                    let field_ty = if _param_map
                                                        .contains_key(&field_name)
                                                    {
                                                        crate::types::OatsType::Number // default for params
                                                    } else if let Some((
                                                        _,
                                                        ty,
                                                        _,
                                                        _,
                                                        _,
                                                        nominal,
                                                        _oats_type,
                                                    )) =
                                                        self.find_local(_locals_stack, &field_name)
                                                    {
                                                        // Check if this local has a nominal type annotation
                                                        if let Some(nom_name) = nominal {
                                                            crate::types::OatsType::NominalStruct(
                                                                nom_name,
                                                            )
                                                        }
                                                        // Try to map LLVM type back to OatsType
                                                        else if ty.is_float_type() {
                                                            crate::types::OatsType::Number
                                                        } else if ty.is_pointer_type() {
                                                            crate::types::OatsType::String // assume string/pointer
                                                        } else {
                                                            crate::types::OatsType::Number
                                                        }
                                                    } else {
                                                        crate::types::OatsType::Number
                                                    };
                                                    fields.push((field_name.clone(), field_ty));
                                                }
                                                ast::Prop::Shorthand(ident) => {
                                                    // Shorthand property { x } in deno_ast
                                                    let field_name = ident.sym.to_string();
                                                    // Try to infer type from param or local
                                                    let field_ty = if _param_map
                                                        .contains_key(&field_name)
                                                    {
                                                        crate::types::OatsType::Number // default for params
                                                    } else if let Some((
                                                        _,
                                                        ty,
                                                        _,
                                                        _,
                                                        _,
                                                        nominal,
                                                        _oats_type,
                                                    )) =
                                                        self.find_local(_locals_stack, &field_name)
                                                    {
                                                        // Check if this local has a nominal type annotation
                                                        if let Some(nom_name) = nominal {
                                                            crate::types::OatsType::NominalStruct(
                                                                nom_name,
                                                            )
                                                        }
                                                        // Try to map LLVM type back to OatsType
                                                        else if ty.is_float_type() {
                                                            crate::types::OatsType::Number
                                                        } else if ty.is_pointer_type() {
                                                            crate::types::OatsType::String // assume string/pointer
                                                        } else {
                                                            crate::types::OatsType::Number
                                                        }
                                                    } else {
                                                        crate::types::OatsType::Number
                                                    };
                                                    fields.push((field_name.clone(), field_ty));
                                                }
                                                _ => {}
                                            }
                                        }
                                    }

                                    // Register the fields under the generated nominal name
                                    if !fields.is_empty() {
                                        self.class_fields
                                            .borrow_mut()
                                            .insert(gen_name.clone(), fields);
                                    }

                                    // Now set declared_nominal so the rest of the logic uses it
                                    declared_nominal = Some(gen_name);
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
                                    //
                                    // NOTE: We don't box pointer values here because
                                    // they might already be boxed unions from function
                                    // calls. This prevents double-boxing. Only scalar
                                    // values (f64, bool) are boxed.
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
                                            // Only box scalar values (f64, i1)
                                            // Pointers are assumed to be either already boxed
                                            // or need to be treated as direct pointer values
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
                                                allocated_ty = self.i8ptr_t.as_basic_type_enum();
                                            } else if val.get_type().is_int_type() {
                                                // Boolean -> convert to f64, then box
                                                if let BasicValueEnum::IntValue(iv) = val {
                                                    let as_f64 =
                                                        self.builder.build_unsigned_int_to_float(
                                                            iv,
                                                            self.f64_t,
                                                            "bool_to_f64",
                                                        );
                                                    if let Ok(fv) = as_f64 {
                                                        let box_fn = self.get_union_box_f64();
                                                        let cs = self.builder.build_call(
                                                            box_fn,
                                                            &[fv.into()],
                                                            "union_box_f64_ctor",
                                                        );
                                                        if let Ok(cs) = cs
                                                            && let inkwell::Either::Left(bv) =
                                                                cs.try_as_basic_value()
                                                        {
                                                            val = bv;
                                                        }
                                                    }
                                                }
                                                allocated_ty = self.i8ptr_t.as_basic_type_enum();
                                            } else if val.get_type().is_pointer_type() {
                                                // Pointer value - don't box it, might already be boxed
                                                // Just use it directly
                                                allocated_ty = self.i8ptr_t.as_basic_type_enum();
                                            }
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

                                    // If a local with this name was pre-inserted (for
                                    // example when we pre-allocated a slot for `new`
                                    // or `await` initializers), reuse its alloca so we
                                    // don't create duplicate slots. Otherwise allocate
                                    // a fresh alloca for the local.
                                    let maybe_existing = self.find_local(_locals_stack, &name);
                                    let alloca = if let Some((
                                        existing_ptr,
                                        _existing_ty,
                                        _existing_init,
                                        _existing_is_const,
                                        _existing_is_weak,
                                        _existing_nominal,
                                        _existing_oats_type,
                                    )) = &maybe_existing
                                    {
                                        *existing_ptr
                                    } else {
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
                                        }
                                    };
                                    // Always store the initializer into the slot and
                                    // perform RC increment for pointer-like values.
                                    if val.get_type().is_pointer_type()
                                        && let BasicValueEnum::PointerValue(pv) = val
                                        && !self.should_elide_rc_for_local(&name)
                                    {
                                        let rc_inc = self.get_rc_inc();
                                        let _ = self.builder.build_call(
                                            rc_inc,
                                            &[pv.into()],
                                            "rc_inc_local",
                                        );
                                    }
                                    let _ = self.builder.build_store(alloca, val);
                                    // If we reused an existing local, mark it initialized.
                                    if maybe_existing.is_some() {
                                        self.set_local_initialized(_locals_stack, &name, true);
                                    } else {
                                        // mark initialized in locals; is_const=false by default
                                        // For `var` declarations, insert into function scope (hoisted)
                                        // For `let`/`const` declarations, insert into current scope (block-scoped)
                                        if matches!(
                                            var_decl.kind,
                                            deno_ast::swc::ast::VarDeclKind::Var
                                        ) {
                                            self.insert_local_function_scope(
                                                _locals_stack,
                                                crate::codegen::helpers::LocalVarInfo {
                                                    name: name.clone(),
                                                    ptr: alloca,
                                                    ty: allocated_ty,
                                                    initialized: true,
                                                    // var declarations are mutable
                                                    is_const: false,
                                                    is_weak: declared_is_weak,
                                                    nominal: declared_nominal.clone(),
                                                    oats_type: declared_mapped
                                                        .clone()
                                                        .or(Some(init_inferred.clone())),
                                                },
                                            );
                                        } else {
                                            self.insert_local_current_scope(
                                                _locals_stack,
                                                crate::codegen::helpers::LocalVarInfo {
                                                    name: name.clone(),
                                                    ptr: alloca,
                                                    ty: allocated_ty,
                                                    initialized: true,
                                                    // If this was a `let` without `mut`, treat as const/immutable
                                                    is_const: matches!(
                                                        var_decl.kind,
                                                        deno_ast::swc::ast::VarDeclKind::Const
                                                    ) || !is_mut_decl,
                                                    is_weak: declared_is_weak,
                                                    nominal: declared_nominal.clone(),
                                                    oats_type: declared_mapped
                                                        .clone()
                                                        .or(Some(init_inferred.clone())),
                                                },
                                            );
                                        }
                                    }
                                } else {
                                    // No initializer: create an uninitialized slot
                                    // using `i64` as a conservative ABI choice. The
                                    // slot will be marked uninitialized so further
                                    // lowering can detect reads of uninitialized
                                    // locals and emit diagnostics if necessary.
                                    // For union types, use i8ptr since unions are boxed.
                                    let ty = if declared_union.is_some() {
                                        self.i8ptr_t.as_basic_type_enum()
                                    } else {
                                        self.i64_t.as_basic_type_enum()
                                    };
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
                                        crate::codegen::helpers::LocalVarInfo {
                                            name,
                                            ptr: alloca,
                                            ty,
                                            initialized: false,
                                            is_const: matches!(
                                                var_decl.kind,
                                                deno_ast::swc::ast::VarDeclKind::Const
                                            ) || !is_mut_decl,
                                            is_weak: declared_is_weak,
                                            nominal: declared_nominal.clone(),
                                            oats_type: declared_union.clone(),
                                        },
                                    );
                                }
                            }
                        }
                    }
                }

                // Handle nested function declarations inside a block/function
                // (e.g. `function inner() {}` defined within another function).
                // Emit them as top-level module functions so later call sites
                // that reference the identifier can resolve via
                // `module.get_function(name)` during expression lowering.
                if let deno_ast::swc::ast::Decl::Fn(fdecl) = d {
                    // For nested functions, require/check parameter and return
                    // annotations using the same strictness check used by the
                    // top-level builder. If strictness checking fails (missing
                    // or invalid annotations) we emit a diagnostic and skip
                    // emitting the nested function rather than silently
                    // defaulting its return type to `Void` which produces
                    // incorrect codegen (see getFirstElement lowering bug).
                    let fname = fdecl.ident.sym.to_string();
                    // call the strictness checker which validates annotations
                    // and returns a concrete FunctionSig (params + return)
                    let mut symbols = crate::types::SymbolTable::new();
                    match crate::types::check_function_strictness(&fdecl.function, &mut symbols) {
                        Ok(fsig) => {
                            // If this nested function is generic (has type params)
                            // register it for monomorphization instead of emitting
                            // a single generic instance now. We'll create
                            // specialized versions at call-sites when concrete
                            // type arguments are inferred.
                            if fdecl.function.type_params.is_some() {
                                self.nested_generic_fns.borrow_mut().insert(
                                    fname.clone(),
                                    ((*fdecl.function).clone(), fsig.clone()),
                                );
                            } else {
                                let param_types_vec = fsig.params.clone();
                                let ret_type = fsig.ret.clone();

                                // Preserve current insertion block so we can restore it after
                                // emitting the nested function. gen_function_ir mutates the
                                // builder position (it emits into the new function), which
                                // would otherwise leave us positioned inside the nested
                                // function and cause later code to be emitted into it.
                                let prev_block = self.builder.get_insert_block();
                                if let Err(diag) = self.gen_function_ir(
                                    &fname,
                                    &fdecl.function,
                                    &param_types_vec,
                                    &ret_type,
                                    None,
                                ) {
                                    crate::diagnostics::emit_diagnostic(&diag, Some(self.source));
                                }
                                if let Some(pb) = prev_block {
                                    // Restore to the previous block so lowering resumes
                                    // at the correct location.
                                    self.builder.position_at_end(pb);
                                }
                            }
                        }
                        Err(e) => {
                            // Emit a diagnostic so the user knows to add annotations
                            // rather than silently generating an incorrect void
                            // returning function.
                            let msg = format!(
                                "skipping nested function '{}' due to missing/invalid type annotations: {}",
                                fname, e
                            );
                            crate::diagnostics::emit_diagnostic(
                                &crate::diagnostics::Diagnostic::simple(&msg),
                                Some(self.source),
                            );
                        }
                    }
                }
                // Handle nested class declarations inside functions/blocks.
                // Emit their methods and constructors as top-level module
                // functions so callsites can resolve <Class>_<method> symbols
                // during expression lowering. We do not attempt fallbacks;
                // emit diagnostics when function strictness checking fails.
                if let deno_ast::swc::ast::Decl::Class(class_decl) = d {
                    use deno_ast::swc::ast;
                    let class_name = class_decl.ident.sym.to_string();

                    // Collect class fields (from ClassProp and constructor params)
                    let mut fields: Vec<(String, crate::types::OatsType)> = Vec::new();
                    for member in &class_decl.class.body {
                        if let ast::ClassMember::ClassProp(prop) = member
                            && let ast::PropName::Ident(id) = &prop.key
                        {
                            let fname = id.sym.to_string();
                            if fields.iter().all(|(n, _)| n != &fname) {
                                let ftype = if let Some(type_ann) = &prop.type_ann {
                                    if let Some(mt) = crate::types::map_ts_type(&type_ann.type_ann)
                                    {
                                        mt
                                    } else {
                                        crate::types::OatsType::Number
                                    }
                                } else {
                                    crate::types::OatsType::Number
                                };
                                fields.push((fname, ftype));
                            }
                        }
                    }

                    // Also collect parameter properties from the constructor
                    // (TypeScript shorthand `constructor(public x: T)`) so methods
                    // can see these fields during lowering. We scan ctor.params
                    // for `ParamOrTsParamProp::TsParamProp` entries and add them.
                    use deno_ast::swc::ast::{ParamOrTsParamProp, TsParamPropParam};
                    for member in &class_decl.class.body {
                        if let ast::ClassMember::Constructor(cons) = member {
                            for p in &cons.params {
                                if let ParamOrTsParamProp::TsParamProp(ts_param) = p
                                    && let TsParamPropParam::Ident(binding_ident) = &ts_param.param
                                {
                                    let fname = binding_ident.id.sym.to_string();
                                    if fields.iter().all(|(n, _)| n != &fname) {
                                        let ty = crate::types::infer_type(
                                            binding_ident
                                                .type_ann
                                                .as_ref()
                                                .map(|ann| &*ann.type_ann),
                                            None,
                                        );
                                        fields.push((fname, ty));
                                    }
                                }
                            }
                        }
                    }

                    // Determine parent name (if any) so `super(...)` lowering can work
                    let parent_name_opt = if let Some(sc) = &class_decl.class.super_class {
                        if let deno_ast::swc::ast::Expr::Ident(id) = &**sc {
                            Some(id.sym.to_string())
                        } else {
                            None
                        }
                    } else {
                        None
                    };
                    // Set current class parent while emitting members
                    *self.current_class_parent.borrow_mut() = parent_name_opt.clone();

                    // Emit methods and constructor bodies as module functions.
                    for member in &class_decl.class.body {
                        use deno_ast::swc::ast::ClassMember;
                        match member {
                            ClassMember::Method(m) => {
                                let mname = match &m.key {
                                    ast::PropName::Ident(id) => id.sym.to_string(),
                                    ast::PropName::Str(s) => s.value.to_string(),
                                    _ => continue,
                                };
                                // Strictness check to obtain parameter/return types. If the
                                // strict check fails (for example when the method uses
                                // default params or non-ident patterns), fall back to a
                                // permissive extraction that maps simple idents and
                                // defaults unknown types to Number. This keeps nested
                                // class emission resilient while still producing usable IR.
                                let mut method_symbols = crate::types::SymbolTable::new();
                                let mut params: Vec<crate::types::OatsType> = Vec::new();
                                params.push(crate::types::OatsType::NominalStruct(
                                    class_name.clone(),
                                ));
                                let ret: crate::types::OatsType;
                                if let Ok(sig) = crate::types::check_function_strictness(
                                    &m.function,
                                    &mut method_symbols,
                                ) {
                                    params.extend(sig.params.into_iter());
                                    ret = sig.ret;
                                } else {
                                    // Fallback: extract parameter types permissively
                                    for p in &m.function.params {
                                        match &p.pat {
                                            deno_ast::swc::ast::Pat::Ident(ident) => {
                                                if let Some(type_ann) = &ident.type_ann
                                                    && let Some(mt) = crate::types::map_ts_type(
                                                        &type_ann.type_ann,
                                                    )
                                                {
                                                    params.push(mt);
                                                    continue;
                                                }
                                                params.push(crate::types::OatsType::Number);
                                            }
                                            _ => {
                                                params.push(crate::types::OatsType::Number);
                                            }
                                        }
                                    }
                                    // Fallback return type: use annotated return type if present
                                    if let Some(rt) = &m.function.return_type {
                                        if let Some(mapped) =
                                            crate::types::map_ts_type(&rt.type_ann)
                                        {
                                            ret = mapped;
                                        } else {
                                            ret = crate::types::OatsType::Void;
                                        }
                                    } else {
                                        ret = crate::types::OatsType::Void;
                                    }
                                }

                                let fname = format!("{}_{}", class_name, mname);
                                let prev_block = self.builder.get_insert_block();
                                if let Err(d) = self.gen_function_ir(
                                    &fname,
                                    &m.function,
                                    &params,
                                    &ret,
                                    Some("this"),
                                ) {
                                    crate::diagnostics::emit_diagnostic(&d, Some(self.source));
                                }
                                if let Some(pb) = prev_block {
                                    self.builder.position_at_end(pb);
                                }
                            }
                            ClassMember::Constructor(ctor) => {
                                // gather constructor param/prop-inferred fields
                                use deno_ast::swc::ast::{Expr, MemberProp, Stmt};
                                if let Some(body) = &ctor.body {
                                    for stmt in &body.stmts {
                                        if let Stmt::Expr(expr_stmt) = stmt
                                            && let Expr::Assign(assign) = &*expr_stmt.expr
                                            && let deno_ast::swc::ast::AssignTarget::Simple(
                                                simple_target,
                                            ) = &assign.left
                                            && let deno_ast::swc::ast::SimpleAssignTarget::Member(
                                                mem,
                                            ) = simple_target
                                            && matches!(&*mem.obj, Expr::This(_))
                                            && let MemberProp::Ident(ident) = &mem.prop
                                        {
                                            let name = ident.sym.to_string();
                                            let inferred =
                                                crate::types::infer_type(None, Some(&assign.right));
                                            if fields.iter().all(|(n, _)| n != &name) {
                                                fields.push((name, inferred));
                                            }
                                        }
                                    }
                                }

                                // Register class fields for lowering
                                self.class_fields
                                    .borrow_mut()
                                    .insert(class_name.clone(), fields.clone());

                                // Emit constructor IR. Ensure current_class_parent is set so `super(...)` works.
                                let prev_block = self.builder.get_insert_block();
                                // Collect simple identifier decorators (MVP): only support
                                // decorator expressions that are plain identifiers.
                                let mut deco_names: Vec<String> = Vec::new();
                                for d in &class_decl.class.decorators {
                                    if let deno_ast::swc::ast::Expr::Ident(id) = &*d.expr {
                                        deco_names.push(id.sym.to_string());
                                    } else {
                                        // Non-ident decorators are currently unsupported in the MVP.
                                    }
                                }
                                if let Err(diag) = self.gen_constructor_ir(
                                    &class_name,
                                    ctor,
                                    &fields,
                                    if deco_names.is_empty() {
                                        None
                                    } else {
                                        Some(deco_names)
                                    },
                                ) {
                                    crate::diagnostics::emit_diagnostic(&diag, Some(self.source));
                                }
                                if let Some(pb) = prev_block {
                                    self.builder.position_at_end(pb);
                                }
                            }
                            _ => {}
                        }
                    }
                }
                // Clear current class parent after emitting members
                self.current_class_parent.borrow_mut().take();
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
                    match self.lower_expr(arg, _function, _param_map, _locals_stack) {
                        Ok(mut val) => {
                            // Check if function returns a union type and box if necessary
                            if let Some(return_type) =
                                self.current_function_return_type.borrow().clone()
                                && matches!(return_type, crate::types::OatsType::Union(_))
                            {
                                // Box the value as a union
                                use inkwell::values::BasicValueEnum;
                                val = match val {
                                    BasicValueEnum::FloatValue(fv) => {
                                        let box_fn = self.get_union_box_f64();

                                        self.builder
                                            .build_call(box_fn, &[fv.into()], "union_box")
                                            .map_err(|_| {
                                                crate::diagnostics::Diagnostic::simple(
                                                    "Failed to box f64 as union in return",
                                                )
                                            })?
                                            .try_as_basic_value()
                                            .left()
                                            .ok_or_else(|| {
                                                crate::diagnostics::Diagnostic::simple(
                                                    "union_box_f64 did not return value",
                                                )
                                            })?
                                    }
                                    BasicValueEnum::PointerValue(pv) => {
                                        let box_fn = self.get_union_box_ptr();

                                        self.builder
                                            .build_call(box_fn, &[pv.into()], "union_box")
                                            .map_err(|_| {
                                                crate::diagnostics::Diagnostic::simple(
                                                    "Failed to box ptr as union in return",
                                                )
                                            })?
                                            .try_as_basic_value()
                                            .left()
                                            .ok_or_else(|| {
                                                crate::diagnostics::Diagnostic::simple(
                                                    "union_box_ptr did not return value",
                                                )
                                            })?
                                    }
                                    BasicValueEnum::IntValue(iv)
                                        if iv.get_type().get_bit_width() == 1 =>
                                    {
                                        // Boolean -> convert to f64 and box
                                        let as_f64 = self
                                            .builder
                                            .build_unsigned_int_to_float(
                                                iv,
                                                self.f64_t,
                                                "bool_to_f64",
                                            )
                                            .map_err(|_| {
                                                crate::diagnostics::Diagnostic::simple(
                                                    "Failed to convert bool to f64 in union return",
                                                )
                                            })?;
                                        let box_fn = self.get_union_box_f64();

                                        self.builder
                                            .build_call(box_fn, &[as_f64.into()], "union_box")
                                            .map_err(|_| {
                                                crate::diagnostics::Diagnostic::simple(
                                                    "Failed to box bool as union in return",
                                                )
                                            })?
                                            .try_as_basic_value()
                                            .left()
                                            .ok_or_else(|| {
                                                crate::diagnostics::Diagnostic::simple(
                                                    "union_box_f64 did not return value",
                                                )
                                            })?
                                    }
                                    _ => val, // Already boxed or unsupported type
                                };
                            }
                            // emit rc decs for locals
                            self.emit_rc_dec_for_locals(_locals_stack);
                            // build return with the lowered value
                            let _ = self.builder.build_return(Some(&val));
                            Ok(true)
                        }
                        Err(diag) => {
                            // Emit diagnostic so we know why the return expression failed to lower
                            crate::diagnostics::emit_diagnostic(&diag, Some(self.source));
                            Ok(false)
                        }
                    }
                } else {
                    self.emit_rc_dec_for_locals(_locals_stack);
                    let _ = self.builder.build_return(None);
                    Ok(true)
                }
            }
            deno_ast::swc::ast::Stmt::Break(_break_stmt) => {
                // Find the target loop context
                let target_ctx = if let Some(label) = _break_stmt.label.as_ref() {
                    // Labeled break: find the loop with matching label
                    let label_str = label.sym.as_str();
                    self.loop_context_stack
                        .borrow()
                        .iter()
                        .rev()
                        .find(|ctx| ctx.label.as_deref() == Some(label_str))
                        .cloned()
                } else {
                    // Unlabeled break: use innermost loop
                    self.loop_context_stack.borrow().last().cloned()
                };

                if let Some(loop_ctx) = target_ctx {
                    // Emit RC decrements for locals owned by the target loop
                    self._emit_rc_dec_for_locals_from(_locals_stack, loop_ctx.locals_start);

                    // Branch to break block
                    let _ = self
                        .builder
                        .build_unconditional_branch(loop_ctx.break_block);
                } else {
                    return Err(crate::diagnostics::Diagnostic::simple(
                        "break statement outside of loop",
                    ));
                }
                Ok(true)
            }
            deno_ast::swc::ast::Stmt::Continue(_continue_stmt) => {
                // Find the target loop context
                let target_ctx = if let Some(label) = _continue_stmt.label.as_ref() {
                    // Labeled continue: find the loop with matching label
                    let label_str = label.sym.as_str();
                    self.loop_context_stack
                        .borrow()
                        .iter()
                        .rev()
                        .find(|ctx| ctx.label.as_deref() == Some(label_str))
                        .cloned()
                } else {
                    // Unlabeled continue: use innermost loop
                    self.loop_context_stack.borrow().last().cloned()
                };

                if let Some(loop_ctx) = target_ctx {
                    // Emit RC decrements for locals owned by the target loop
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

                        // If both branches terminated, the merge block is unreachable.
                        // Add an unreachable instruction to make LLVM happy.
                        if then_terminated && else_terminated {
                            let _ = self.builder.build_unreachable();
                        }

                        // If both branches terminated, the if statement terminates
                        Ok(then_terminated && else_terminated)
                    } else {
                        Ok(false)
                    }
                } else {
                    Ok(false)
                }
            }
            deno_ast::swc::ast::Stmt::Labeled(labeled) => {
                // Set the current label for labeled statements
                let label_str = labeled.label.sym.to_string();
                *self.current_label.borrow_mut() = Some(label_str);
                let result = self.lower_stmt(&labeled.body, _function, _param_map, _locals_stack);
                *self.current_label.borrow_mut() = None;
                result
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
                                    label: self.current_label.borrow().clone(),
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
                                        crate::codegen::helpers::LocalVarInfo {
                                            name: loop_var_name.clone(),
                                            ptr: alloca,
                                            ty,
                                            initialized: true,
                                            is_const: false,
                                            is_weak: false,
                                            nominal: declared_nominal.clone(),
                                            oats_type: None,
                                        },
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
                                    if !self.should_elide_rc_for_local(&loop_var_name) {
                                        let rc_inc = self.get_rc_inc();
                                        let _ = self.builder.build_call(
                                            rc_inc,
                                            &[pv.into()],
                                            "rc_inc_loop_var",
                                        );
                                    }
                                    let _ = self.builder.build_store(alloca, bv);
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
                        label: self.current_label.borrow().clone(),
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
                        let _ = self.builder.build_unconditional_branch(loop_after_bb);
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
                        label: self.current_label.borrow().clone(),
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
                    let _ = self.builder.build_unconditional_branch(loop_after_bb);
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
                        label: self.current_label.borrow().clone(),
                    });

                // Branch directly to body (execute at least once)
                let _ = self.builder.build_unconditional_branch(loop_body_bb);

                // Build body block
                self.builder.position_at_end(loop_body_bb);
                let _body_terminated: bool = match &*dowhile_stmt.body {
                    deno_ast::swc::ast::Stmt::Block(block) => {
                        self.lower_stmts(&block.stmts, _function, _param_map, _locals_stack)?
                    }
                    _ => {
                        self.lower_stmt(&dowhile_stmt.body, _function, _param_map, _locals_stack)?
                    }
                };

                // If body didn't terminate, branch to condition
                if !_body_terminated {
                    let _ = self.builder.build_unconditional_branch(loop_cond_bb);
                }

                // Build condition block (executed after body)
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
                    let _ = self.builder.build_unconditional_branch(loop_after_bb);
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
