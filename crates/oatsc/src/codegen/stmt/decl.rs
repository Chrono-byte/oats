use crate::diagnostics::{Diagnostic, Severity};
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

/// Type information extracted from a variable declaration
struct DeclTypeInfo {
    declared_mapped: Option<crate::types::OatsType>,
    declared_union: Option<crate::types::OatsType>,
    declared_is_weak: bool,
    declared_nominal: Option<String>,
    init_inferred: Option<crate::types::OatsType>,
    // effective_type is computed and used locally but not stored/accessed elsewhere
    // Keeping it for potential future use or debugging
    #[allow(dead_code)]
    effective_type: crate::types::OatsType,
}

/// Context information for variable declaration processing
struct VarDeclContext<'b> {
    name: String,
    var_decl: &'b oats_ast::VarDecl,
    is_mut_decl: bool,
    declared_is_weak: bool,
    declared_union: Option<crate::types::OatsType>,
    declared_nominal: Option<String>,
}

impl<'a> crate::codegen::CodeGen<'a> {
    /// Resolve type information from a variable declaration.
    ///
    /// Extracts declared type annotations, infers types from initializers,
    /// and determines the effective type and related flags (union, weak, nominal).
    fn resolve_decl_type(
        &self,
        decl: &oats_ast::VarDeclarator,
    ) -> crate::diagnostics::DiagnosticResult<DeclTypeInfo> {
        let mut declared_mapped: Option<crate::types::OatsType> = None;
        let mut declared_union: Option<crate::types::OatsType> = None;
        let mut declared_is_weak = false;
        let mut declared_nominal: Option<String> = None;

        // Determine if the identifier has an explicit type annotation
        if let Some(ty) = &decl.ty
            && let Some(mapped) =
                crate::types::map_ts_type_with_aliases(ty, Some(&*self.type_aliases.borrow()))
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

        // Compute inferred type from initializer if present
        let init_inferred = decl
            .init
            .as_ref()
            .map(|init| crate::types::infer_type(None, Some(init)));

        // Require either explicit type annotation or initializer for inference
        if declared_mapped.is_none() && init_inferred.is_none() {
            return Err(crate::diagnostics::Diagnostic::simple_boxed(
                Severity::Error,
                "Variable declaration requires explicit type annotation or initializer",
            ));
        }

        // Determine effective type: declared takes precedence, then inferred
        let effective_type = declared_mapped
            .as_ref()
            .or(init_inferred.as_ref())
            .ok_or_else(|| {
                crate::diagnostics::Diagnostic::simple_boxed(
                    Severity::Error,
                    "Internal error: variable type resolution failed (this should not happen)",
                )
            })?
            .clone();

        // If no explicit type annotation, set flags from inferred type
        if declared_mapped.is_none() {
            if let crate::types::OatsType::Union(_) = &effective_type {
                declared_union = Some(effective_type.clone());
            }
            if let crate::types::OatsType::Weak(_) = &effective_type {
                declared_is_weak = true;
            }
            if let crate::types::OatsType::NominalStruct(n) = &effective_type {
                declared_nominal = Some(n.clone());
            }
            // If the inferred type is an Array, tag the local with
            // a sentinel nominal so lowering sites (like println)
            // can detect arrays at compile time.
            if let crate::types::OatsType::Array(_) = &effective_type {
                declared_nominal = Some("__oats_array".to_string());
            }
        }

        Ok(DeclTypeInfo {
            declared_mapped,
            declared_union,
            declared_is_weak,
            declared_nominal,
            init_inferred,
            effective_type,
        })
    }

    /// Handle compile-time constant evaluation for const declarations.
    ///
    /// Returns `Ok(true)` if the const was successfully evaluated and stored,
    /// meaning the caller should skip further lowering. Returns `Ok(false)` if
    /// the const should fall through to normal lowering (e.g., arrays/objects).
    fn handle_const_decl(
        &self,
        init: &oats_ast::Expr,
        var_decl: &oats_ast::VarDecl,
        name: &str,
    ) -> crate::diagnostics::DiagnosticResult<bool> {
        if !matches!(var_decl.kind, oats_ast::VarDeclKind::Const) {
            return Ok(false);
        }

        let span_start = var_decl.span.start;
        let const_map = self.const_items.borrow();
        match crate::codegen::const_eval::eval_const_expr(init, span_start, &const_map) {
            Ok(cv) => {
                // Only treat primitives as const; arrays and objects need runtime allocation
                if matches!(
                    cv,
                    crate::codegen::const_eval::ConstValue::Number(_)
                        | crate::codegen::const_eval::ConstValue::Bool(_)
                        | crate::codegen::const_eval::ConstValue::Str(_)
                ) {
                    drop(const_map);
                    // Insert into compile-time const map keyed by name
                    self.const_items.borrow_mut().insert(name.to_string(), cv);
                    // For consts we do not lower the initializer further; the
                    // lowered uses will be replaced by LLVM constants later.
                    return Ok(true);
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
        Ok(false)
    }

    /// Handle object literal initialization with nominal type registration.
    ///
    /// If the initializer is an object literal and the declared local carries
    /// a nominal struct name, register the object's property list under
    /// `self.class_fields` keyed by the nominal name.
    fn handle_object_literal_init(
        &self,
        init: &oats_ast::Expr,
        declared_nominal: &Option<String>,
    ) -> crate::diagnostics::DiagnosticResult<()> {
        if let oats_ast::Expr::Object(obj_lit) = init
            && let Some(nominal_name) = declared_nominal.clone()
        {
            let mut fields: Vec<(String, crate::types::OatsType)> = Vec::new();
            for prop in &obj_lit.props {
                if let oats_ast::PropOrSpread::Prop(prop) = prop
                    && let oats_ast::Prop::KeyValue(kv) = prop
                    && let oats_ast::PropName::Ident(id) = &kv.key
                {
                    let fname = id.sym.clone();
                    // Try to infer the field type from the initializer expression.
                    let fty = crate::types::infer_type(None, Some(&kv.value));
                    fields.push((fname, fty));
                }
            }
            if !fields.is_empty() {
                self.class_fields.borrow_mut().insert(nominal_name, fields);
            }
        }
        Ok(())
    }

    /// Handle `new ClassName(...)` initializers.
    ///
    /// Pre-allocates a local slot for the declared variable before lowering
    /// the initializer so subsequent statements can resolve the local even
    /// if initializer lowering emits diagnostics.
    fn handle_new_expr_init(
        &self,
        init: &oats_ast::Expr,
        name: &str,
        declared_is_weak: bool,
        declared_nominal: &mut Option<String>,
        declared_union: &Option<crate::types::OatsType>,
        locals_stack: &mut LocalsStackLocal<'a>,
    ) -> crate::diagnostics::DiagnosticResult<bool> {
        if let oats_ast::Expr::New(new_expr) = init
            && let oats_ast::Expr::Ident(ident) = &*new_expr.callee
        {
            *declared_nominal = Some(ident.sym.clone());

            // Pre-allocate an i8* slot and insert the local into
            // the current scope marked as uninitialized. This
            // ensures `find_local` can discover the nominal
            // type for member-call lowering that appears
            // after this declaration.
            let allocated_ty = self.i8ptr_t.as_basic_type_enum();
            let alloca = match self.builder.build_alloca(allocated_ty, name) {
                Ok(a) => a,
                Err(_) => {
                    crate::diagnostics::emit_diagnostic(
                        &crate::diagnostics::Diagnostic::simple_boxed(
                            Severity::Error,
                            "alloca failed for local variable",
                        ),
                        Some(self.source),
                    );
                    return Ok(false);
                }
            };
            // Insert local early as uninitialized; we'll store to it below
            self.insert_local_current_scope(
                locals_stack,
                crate::codegen::helpers::LocalVarInfo {
                    name: name.to_string(),
                    ptr: alloca,
                    ty: allocated_ty,
                    initialized: false,
                    is_const: false,
                    is_weak: declared_is_weak,
                    nominal: declared_nominal.clone(),
                    oats_type: declared_union.clone(),
                },
            );
            return Ok(true);
        }
        Ok(false)
    }

    /// Handle tuple initialization from array literals.
    ///
    /// If the declared type is a Tuple and the initializer is an array literal,
    /// allocate a native tuple object and register its field types.
    /// Returns `Ok(true)` if tuple initialization was handled, `Ok(false)` otherwise.
    fn handle_tuple_init<'b>(
        &self,
        init: &oats_ast::Expr,
        declared_mapped: &Option<crate::types::OatsType>,
        var_ctx: &VarDeclContext<'b>,
        function: FunctionValue<'a>,
        param_map: &HashMap<String, u32>,
        locals_stack: &mut LocalsStackLocal<'a>,
    ) -> crate::diagnostics::DiagnosticResult<bool> {
        let Some(crate::types::OatsType::Tuple(elem_types)) = declared_mapped else {
            return Ok(false);
        };
        let oats_ast::Expr::Array(arr_lit) = init else {
            return Ok(false);
        };

        // number of elements
        let elem_count = arr_lit.elems.len();
        // Generate a nominal name for this tuple shape
        let fname = function.get_name().to_str().unwrap_or("<fn>");
        let gen_name = format!("tuple_{}_{}", fname, var_ctx.name);
        // Register field list under class_fields as ("0", type0), ...
        let mut fields: Vec<(String, crate::types::OatsType)> = Vec::new();
        for (i, et) in elem_types.iter().enumerate() {
            fields.push((format!("{}", i), et.clone()));
        }
        self.class_fields
            .borrow_mut()
            .insert(gen_name.clone(), fields);

        // Allocate object: header + meta_slot + elem_count * 8
        let header_size = 8u64;
        let meta_slot = 8u64;
        let total_size = header_size + meta_slot + (elem_count as u64 * 8);
        let malloc_fn = self.get_malloc();
        let size_const = self.i64_t.const_int(total_size, false);
        let cs = self
            .builder
            .build_call(malloc_fn, &[size_const.into()], "tuple_malloc");
        let malloc_ret = if let Ok(cs) = cs
            && let inkwell::Either::Left(bv) = cs.try_as_basic_value()
        {
            bv.into_pointer_value()
        } else {
            crate::diagnostics::emit_diagnostic(
                &crate::diagnostics::Diagnostic::simple_boxed(
                    Severity::Error,
                    "malloc failed for tuple allocation",
                ),
                Some(self.source),
            );
            return Ok(false);
        };

        // store header
        let header_ptr = self
            .builder
            .build_pointer_cast(malloc_ret, self.i8ptr_t, "hdr_ptr")
            .map_err(|_| {
                crate::diagnostics::Diagnostic::simple_boxed(Severity::Error, "pointer cast failed")
            })?;
        let header_val = self.i64_t.const_int(1u64, false);
        let _ = self.builder.build_store(header_ptr, header_val);

        // For each element in the array literal, lower and store
        for (i, opt) in arr_lit.elems.iter().enumerate() {
            let Some(expr) = opt else {
                return Err(crate::diagnostics::Diagnostic::simple_boxed(
                    Severity::Error,
                    "elided tuple element not supported",
                ));
            };
            let ev = self.lower_expr(expr, function, param_map, locals_stack)?;
            let offset = header_size + meta_slot + (i as u64 * 8);
            let obj_addr = self
                .builder
                .build_ptr_to_int(malloc_ret, self.i64_t, "obj_addr")
                .map_err(|_| {
                    crate::diagnostics::Diagnostic::simple_boxed(
                        Severity::Error,
                        "ptr_to_int failed",
                    )
                })?;
            let offset_const = self.i64_t.const_int(offset, false);
            let field_addr = self
                .builder
                .build_int_add(obj_addr, offset_const, "field_addr")
                .map_err(|_| {
                    crate::diagnostics::Diagnostic::simple_boxed(Severity::Error, "int_add failed")
                })?;
            let field_ptr = self
                .builder
                .build_int_to_ptr(field_addr, self.i8ptr_t, "field_ptr")
                .map_err(|_| {
                    crate::diagnostics::Diagnostic::simple_boxed(
                        Severity::Error,
                        "int_to_ptr failed",
                    )
                })?;
            // Store element into slot using same logic as object literal
            // Use the declared element type to choose storage
            let field_ty = elem_types.get(i).ok_or_else(|| {
                crate::diagnostics::Diagnostic::simple_boxed(
                    Severity::Error,
                    "tuple element type missing",
                )
            })?;
            self.store_tuple_element(field_ptr, ev, field_ty)?;
        }

        // Create an alloca for the local and store the tuple pointer
        let allocated_ty = self.i8ptr_t.as_basic_type_enum();
        let alloca = match self.builder.build_alloca(allocated_ty, &var_ctx.name) {
            Ok(a) => a,
            Err(_) => {
                crate::diagnostics::emit_diagnostic(
                    &crate::diagnostics::Diagnostic::simple_boxed(
                        Severity::Error,
                        "alloca failed for local variable",
                    ),
                    Some(self.source),
                );
                return Ok(false);
            }
        };
        // increment rc for stored pointer (unless elided)
        if !self.should_elide_rc_for_local(&var_ctx.name) {
            let rc_inc = self.get_rc_inc();
            let _ = self
                .builder
                .build_call(rc_inc, &[malloc_ret.into()], "rc_inc_local");
        }
        let _ = self
            .builder
            .build_store(alloca, malloc_ret.as_basic_value_enum());
        // mark initialized and insert local with nominal pointing to tuple generated name
        self.insert_local_current_scope(
            locals_stack,
            crate::codegen::helpers::LocalVarInfo {
                name: var_ctx.name.clone(),
                ptr: alloca,
                ty: allocated_ty,
                initialized: true,
                // If this was a `let` without `mut`, treat as const/immutable
                is_const: matches!(var_ctx.var_decl.kind, oats_ast::VarDeclKind::Const)
                    || !var_ctx.is_mut_decl,
                is_weak: var_ctx.declared_is_weak,
                nominal: Some(gen_name),
                oats_type: var_ctx.declared_union.clone(),
            },
        );
        Ok(true)
    }

    /// Store a tuple element into a field pointer based on its type.
    fn store_tuple_element(
        &self,
        field_ptr: inkwell::values::PointerValue<'a>,
        ev: BasicValueEnum<'a>,
        field_ty: &crate::types::OatsType,
    ) -> crate::diagnostics::DiagnosticResult<()> {
        match field_ty {
            crate::types::OatsType::Number => {
                // Coerce to f64 then store into an f64* slot
                let fv = if ev.get_type().is_float_type() {
                    ev.into_float_value()
                } else if ev.get_type().is_int_type() {
                    let iv = ev.into_int_value();
                    self.builder
                        .build_signed_int_to_float(iv, self.f64_t, "i_to_f")
                        .map_err(|_| {
                            crate::diagnostics::Diagnostic::simple_boxed(
                                Severity::Error,
                                "int->float cast failed",
                            )
                        })?
                } else if let Some(fv) = self.coerce_to_f64(ev) {
                    fv
                } else {
                    return Err(crate::diagnostics::Diagnostic::simple_boxed(
                        Severity::Error,
                        "expected numeric value for tuple number element",
                    ));
                };
                let f64_ptr_ty = self.context.ptr_type(AddressSpace::default());
                let elem_f64_ptr = self
                    .builder
                    .build_pointer_cast(field_ptr, f64_ptr_ty, "tuple_elem_f64_ptr")
                    .map_err(|_| {
                        crate::diagnostics::Diagnostic::simple_boxed(
                            Severity::Error,
                            "pointer cast failed",
                        )
                    })?;
                let _ = self
                    .builder
                    .build_store(elem_f64_ptr, fv.as_basic_value_enum());
            }
            crate::types::OatsType::Union(_) => {
                // Box union payloads into a heap object and store pointer
                // If ev is float -> union_box_f64, if pointer -> union_box_ptr
                if ev.get_type().is_float_type() {
                    let box_fn = self.get_union_box_f64();
                    let cs2 = self
                        .builder
                        .build_call(box_fn, &[ev.into()], "union_box_f64_ctor");
                    if let Ok(cs2) = cs2
                        && let inkwell::Either::Left(bv2) = cs2.try_as_basic_value()
                    {
                        let boxed_ptr = bv2.into_pointer_value();
                        let rc_inc = self.get_rc_inc();
                        let _ =
                            self.builder
                                .build_call(rc_inc, &[boxed_ptr.into()], "rc_inc_field");
                        let boxed_bv = inkwell::values::BasicValueEnum::PointerValue(boxed_ptr);
                        let _ = self.builder.build_store(field_ptr, boxed_bv);
                    }
                } else if ev.get_type().is_pointer_type() {
                    let box_fn = self.get_union_box_ptr();
                    let cs2 = self
                        .builder
                        .build_call(box_fn, &[ev.into()], "union_box_ptr_ctor");
                    if let Ok(cs2) = cs2
                        && let inkwell::Either::Left(bv2) = cs2.try_as_basic_value()
                    {
                        let boxed_ptr = bv2.into_pointer_value();
                        let rc_inc = self.get_rc_inc();
                        let _ =
                            self.builder
                                .build_call(rc_inc, &[boxed_ptr.into()], "rc_inc_field");
                        let boxed_bv = inkwell::values::BasicValueEnum::PointerValue(boxed_ptr);
                        let _ = self.builder.build_store(field_ptr, boxed_bv);
                    }
                } else {
                    return Err(crate::diagnostics::Diagnostic::simple_boxed(
                        Severity::Error,
                        "unsupported tuple union element type at init",
                    ));
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
                    return Err(crate::diagnostics::Diagnostic::simple_boxed(
                        Severity::Error,
                        "expected pointer for tuple reference element",
                    ));
                }
            }
            _ => {
                // Fallback: store as is (covers booleans/int where appropriate)
                let _ = self.builder.build_store(field_ptr, ev);
            }
        }
        Ok(())
    }

    /// Apply union boxing to a value if needed.
    ///
    /// Returns the (possibly boxed) value and the allocated type.
    fn handle_union_boxing(
        &self,
        mut val: BasicValueEnum<'a>,
        declared_union: &Option<crate::types::OatsType>,
    ) -> crate::diagnostics::DiagnosticResult<(BasicValueEnum<'a>, inkwell::types::BasicTypeEnum<'a>)>
    {
        let mut allocated_ty = val.get_type().as_basic_type_enum();
        if let Some(crate::types::OatsType::Union(parts)) = declared_union {
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
                    let cs = self
                        .builder
                        .build_call(box_fn, &[val.into()], "union_box_f64_ctor");
                    if let Ok(cs) = cs
                        && let inkwell::Either::Left(bv) = cs.try_as_basic_value()
                    {
                        val = bv;
                    }
                    allocated_ty = self.i8ptr_t.as_basic_type_enum();
                } else if val.get_type().is_int_type() {
                    // Boolean -> convert to f64, then box
                    if let BasicValueEnum::IntValue(iv) = val {
                        let as_f64 =
                            self.builder
                                .build_unsigned_int_to_float(iv, self.f64_t, "bool_to_f64");
                        if let Ok(fv) = as_f64 {
                            let box_fn = self.get_union_box_f64();
                            let cs =
                                self.builder
                                    .build_call(box_fn, &[fv.into()], "union_box_f64_ctor");
                            if let Ok(cs) = cs
                                && let inkwell::Either::Left(bv) = cs.try_as_basic_value()
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
                        && let Ok(fv_val) = self
                            .builder
                            .build_signed_int_to_float(iv, self.f64_t, "i2f")
                    {
                        val = inkwell::values::BasicValueEnum::FloatValue(fv_val);
                    }
                }
            }
        }
        Ok((val, allocated_ty))
    }

    /// Lower array destructuring: let [a, b, c] = arr
    fn lower_array_destructuring(
        &self,
        arr_pat: &oats_ast::ArrayPat,
        init: &oats_ast::Expr,
        var_decl: &oats_ast::VarDecl,
        function: FunctionValue<'a>,
        param_map: &HashMap<String, u32>,
        locals_stack: &mut LocalsStackLocal<'a>,
    ) -> crate::diagnostics::DiagnosticResult<()> {
        use oats_ast::*;

        // Lower the initializer (should be an array)
        let arr_val = self.lower_expr(init, function, param_map, locals_stack)?;
        let arr_ptr = if let BasicValueEnum::PointerValue(pv) = arr_val {
            pv
        } else {
            return Err(crate::diagnostics::Diagnostic::simple_boxed(
                Severity::Error,
                "Array destructuring requires an array initializer",
            ));
        };

        // Get array length helper
        let array_get_length = self.get_array_get_length();
        let length_call = self
            .builder
            .build_call(array_get_length, &[arr_ptr.into()], "arr_len")
            .map_err(|_| Diagnostic::simple_boxed(Severity::Error, "failed to get array length"))?;
        // Note: length is computed but not currently used for bounds checking
        // This could be used in the future to validate array destructuring bounds
        let length = if let inkwell::Either::Left(bv) = length_call.try_as_basic_value() {
            bv.into_int_value()
        } else {
            return Err(Diagnostic::simple_boxed(
                Severity::Error,
                "array length call failed",
            ));
        };

        let is_mut_decl = matches!(var_decl.kind, VarDeclKind::Let { mutable: true });

        // Find rest pattern position (if any)
        let rest_pos = arr_pat.elems.iter().position(|opt| {
            if let Some(Pat::Rest(_)) = opt {
                true
            } else {
                false
            }
        });

        // Extract each element
        for (idx, opt_elem_pat) in arr_pat.elems.iter().enumerate() {
            // Skip elements after rest pattern
            if let Some(rest_pos) = rest_pos
                && idx > rest_pos {
                    break;
                }

            if let Some(elem_pat) = opt_elem_pat {
                // Get array element at index
                let idx_const = self.i64_t.const_int(idx as u64, false);
                let array_get_ptr = self.get_array_get_ptr();
                let elem_call = self
                    .builder
                    .build_call(
                        array_get_ptr,
                        &[arr_ptr.into(), idx_const.into()],
                        "arr_elem",
                    )
                    .map_err(|_| {
                        Diagnostic::simple_boxed(Severity::Error, "failed to get array element")
                    })?;
                let elem_val = if let inkwell::Either::Left(bv) = elem_call.try_as_basic_value() {
                    bv
                } else {
                    return Err(Diagnostic::simple_boxed(
                        Severity::Error,
                        "array element call failed",
                    ));
                };

                // Handle nested patterns recursively
                match elem_pat {
                    Pat::Ident(ident) => {
                        let name = ident.sym.clone();
                        let alloca = self
                            .builder
                            .build_alloca(self.i8ptr_t.as_basic_type_enum(), &name)
                            .map_err(|_| {
                                Diagnostic::simple_boxed(Severity::Error, "alloca failed")
                            })?;

                        // RC increment for the element
                        if let BasicValueEnum::PointerValue(elem_pv) = elem_val {
                            let rc_inc = self.get_rc_inc();
                            let _ = self
                                .builder
                                .build_call(rc_inc, &[elem_pv.into()], "rc_inc_elem")
                                .map_err(|_| {
                                    Diagnostic::simple_boxed(Severity::Error, "rc_inc failed")
                                })?;
                        }

                        let _ = self.builder.build_store(alloca, elem_val);
                        self.insert_local_current_scope(
                            locals_stack,
                            crate::codegen::helpers::LocalVarInfo {
                                name,
                                ptr: alloca,
                                ty: self.i8ptr_t.as_basic_type_enum(),
                                initialized: true,
                                is_const: matches!(var_decl.kind, VarDeclKind::Const)
                                    || !is_mut_decl,
                                is_weak: false,
                                nominal: None,
                                oats_type: None,
                            },
                        );
                    }
                    Pat::Array(nested_arr_pat) => {
                        // Nested array destructuring: let [[a, b], c] = arr
                        // The loaded value is the nested array element, recursively destructure it
                        if let BasicValueEnum::PointerValue(nested_arr_ptr) = elem_val {
                            // Create a temporary variable to hold the nested array
                            let temp_name = format!("__nested_arr_elem_{}", idx);
                            let temp_alloca = self
                                .builder
                                .build_alloca(self.i8ptr_t.as_basic_type_enum(), &temp_name)
                                .map_err(|_| {
                                    Diagnostic::simple_boxed(
                                        Severity::Error,
                                        format!(
                                            "alloca failed for nested array element at index {}",
                                            idx
                                        ),
                                    )
                                })?;

                            // Increment RC for the nested array
                            let rc_inc = self.get_rc_inc();
                            let _ = self.builder.build_call(
                                rc_inc,
                                &[nested_arr_ptr.into()],
                                &format!("rc_inc_nested_arr_elem_{}", idx),
                            );

                            // Store the loaded nested array
                            let _ = self.builder.build_store(temp_alloca, elem_val);

                            // Insert temporary into locals
                            self.insert_local_current_scope(
                                locals_stack,
                                crate::codegen::helpers::LocalVarInfo {
                                    name: temp_name.clone(),
                                    ptr: temp_alloca,
                                    ty: self.i8ptr_t.as_basic_type_enum(),
                                    initialized: true,
                                    is_const: true,
                                    is_weak: false,
                                    nominal: None,
                                    oats_type: None,
                                },
                            );

                            // Create a synthetic identifier expression for the temporary
                            let temp_ident = oats_ast::Expr::Ident(oats_ast::Ident {
                                sym: temp_name.clone(),
                                span: nested_arr_pat.span.clone(),
                            });

                            // Recursively destructure the nested array
                            self.lower_array_destructuring(
                                nested_arr_pat,
                                &temp_ident,
                                var_decl,
                                function,
                                param_map,
                                locals_stack,
                            )?;
                        } else {
                            return Err(Diagnostic::simple_boxed(
                                Severity::Error,
                                format!(
                                    "Nested array destructuring requires array type for element at index {}",
                                    idx
                                ),
                            ));
                        }
                    }
                    Pat::Object(nested_obj_pat) => {
                        // Nested object destructuring: let [{a, b}, c] = arr
                        // The loaded value is the nested object element, recursively destructure it
                        if let BasicValueEnum::PointerValue(nested_obj_ptr) = elem_val {
                            // Create a temporary variable to hold the nested object
                            let temp_name = format!("__nested_obj_elem_{}", idx);
                            let temp_alloca = self
                                .builder
                                .build_alloca(self.i8ptr_t.as_basic_type_enum(), &temp_name)
                                .map_err(|_| {
                                    Diagnostic::simple_boxed(
                                        Severity::Error,
                                        format!(
                                            "alloca failed for nested object element at index {}",
                                            idx
                                        ),
                                    )
                                })?;

                            // Increment RC for the nested object
                            let rc_inc = self.get_rc_inc();
                            let _ = self.builder.build_call(
                                rc_inc,
                                &[nested_obj_ptr.into()],
                                &format!("rc_inc_nested_obj_elem_{}", idx),
                            );

                            // Store the loaded nested object
                            let _ = self.builder.build_store(temp_alloca, elem_val);

                            // Insert temporary into locals
                            self.insert_local_current_scope(
                                locals_stack,
                                crate::codegen::helpers::LocalVarInfo {
                                    name: temp_name.clone(),
                                    ptr: temp_alloca,
                                    ty: self.i8ptr_t.as_basic_type_enum(),
                                    initialized: true,
                                    is_const: true,
                                    is_weak: false,
                                    nominal: None,
                                    oats_type: None,
                                },
                            );

                            // Create a synthetic identifier expression for the temporary
                            let temp_ident = oats_ast::Expr::Ident(oats_ast::Ident {
                                sym: temp_name.clone(),
                                span: nested_obj_pat.span.clone(),
                            });

                            // Recursively destructure the nested object
                            self.lower_object_destructuring(
                                nested_obj_pat,
                                &temp_ident,
                                var_decl,
                                function,
                                param_map,
                                locals_stack,
                            )?;
                        } else {
                            return Err(Diagnostic::simple_boxed(
                                Severity::Error,
                                format!(
                                    "Nested object destructuring requires object type for element at index {}",
                                    idx
                                ),
                            ));
                        }
                    }
                    Pat::Rest(rest_pat) => {
                        // Rest pattern: let [a, b, ...rest] = arr
                        // Create a new array with remaining elements from index idx onwards

                        // Calculate rest array length: length - idx
                        let idx_i64 = self.i64_t.const_int(idx as u64, false);
                        let length_i64 = self
                            .builder
                            .build_int_cast(length, self.i64_t, "length_i64")
                            .map_err(|_| {
                                Diagnostic::simple_boxed(Severity::Error, "int cast failed")
                            })?;
                        let rest_len = self
                            .builder
                            .build_int_sub(length_i64, idx_i64, "rest_len")
                            .map_err(|_| {
                                Diagnostic::simple_boxed(Severity::Error, "int sub failed")
                            })?;

                        // Get the element type from the source array (assume pointer elements for now)
                        // TODO: Could infer element type from array metadata if available
                        let elem_size = self.i32_t.const_int(8, false); // pointer size
                        let is_number = self.i32_t.const_int(0, false); // pointer elements

                        // Allocate new array for rest elements
                        let array_alloc_fn = self.get_array_alloc();
                        let rest_arr_call = self
                            .builder
                            .build_call(
                                array_alloc_fn,
                                &[rest_len.into(), elem_size.into(), is_number.into()],
                                "rest_arr_alloc",
                            )
                            .map_err(|_| {
                                Diagnostic::simple_boxed(Severity::Error, "array alloc failed")
                            })?;

                        let rest_arr_ptr =
                            if let inkwell::Either::Left(bv) = rest_arr_call.try_as_basic_value() {
                                bv.into_pointer_value()
                            } else {
                                return Err(Diagnostic::simple_boxed(
                                    Severity::Error,
                                    "rest array allocation failed",
                                ));
                            };

                        // Copy elements from source array to rest array
                        // Loop from idx to length, copying each element
                        let array_get_ptr = self.get_array_get_ptr();
                        let array_set_ptr = self.get_array_set_ptr();

                        // Create a loop to copy elements
                        let loop_start_bb =
                            self.context.append_basic_block(function, "rest_loop_start");
                        let loop_body_bb =
                            self.context.append_basic_block(function, "rest_loop_body");
                        let loop_end_bb =
                            self.context.append_basic_block(function, "rest_loop_end");

                        // Initialize loop counter (i = idx)
                        let i_alloca =
                            self.builder
                                .build_alloca(self.i64_t, "rest_i")
                                .map_err(|_| {
                                    Diagnostic::simple_boxed(Severity::Error, "alloca failed")
                                })?;
                        let _ = self.builder.build_store(i_alloca, idx_i64);

                        // Branch to loop start
                        let _ = self
                            .builder
                            .build_unconditional_branch(loop_start_bb)
                            .map_err(|_| {
                                Diagnostic::simple_boxed(Severity::Error, "branch failed")
                            })?;

                        // Loop start: check if i < length
                        self.builder.position_at_end(loop_start_bb);
                        let i_val = self
                            .builder
                            .build_load(self.i64_t, i_alloca, "load_i")
                            .map_err(|_| {
                                Diagnostic::simple_boxed(Severity::Error, "load failed")
                            })?;
                        let i_val_int = if let BasicValueEnum::IntValue(iv) = i_val {
                            iv
                        } else {
                            return Err(Diagnostic::simple_boxed(
                                Severity::Error,
                                "loop counter is not an integer",
                            ));
                        };
                        let cond = self
                            .builder
                            .build_int_compare(
                                inkwell::IntPredicate::ULT,
                                i_val_int,
                                length_i64,
                                "i_lt_len",
                            )
                            .map_err(|_| {
                                Diagnostic::simple_boxed(Severity::Error, "compare failed")
                            })?;
                        let _ = self
                            .builder
                            .build_conditional_branch(cond, loop_body_bb, loop_end_bb)
                            .map_err(|_| {
                                Diagnostic::simple_boxed(Severity::Error, "branch failed")
                            })?;

                        // Loop body: copy element from source[i] to rest[i - idx]
                        self.builder.position_at_end(loop_body_bb);

                        // Get source element
                        let src_elem_call = self
                            .builder
                            .build_call(
                                array_get_ptr,
                                &[arr_ptr.into(), i_val_int.into()],
                                "src_elem",
                            )
                            .map_err(|_| {
                                Diagnostic::simple_boxed(Severity::Error, "get elem failed")
                            })?;
                        let src_elem =
                            if let inkwell::Either::Left(bv) = src_elem_call.try_as_basic_value() {
                                bv
                            } else {
                                return Err(Diagnostic::simple_boxed(
                                    Severity::Error,
                                    "get element failed",
                                ));
                            };

                        // Calculate rest index: i - idx
                        let rest_idx = self
                            .builder
                            .build_int_sub(i_val_int, idx_i64, "rest_idx")
                            .map_err(|_| {
                            Diagnostic::simple_boxed(Severity::Error, "int sub failed")
                        })?;

                        // Store in rest array
                        // Create alloca for rest_arr_ptr (array_set_ptr needs i8**)
                        let rest_arr_ptr_alloca = self
                            .builder
                            .build_alloca(self.i8ptr_t, "rest_arr_ptr_alloca")
                            .map_err(|_| {
                                Diagnostic::simple_boxed(Severity::Error, "alloca failed")
                            })?;
                        let _ = self.builder.build_store(rest_arr_ptr_alloca, rest_arr_ptr);

                        let _ = self
                            .builder
                            .build_call(
                                array_set_ptr,
                                &[rest_arr_ptr_alloca.into(), rest_idx.into(), src_elem.into()],
                                "set_rest_elem",
                            )
                            .map_err(|_| {
                                Diagnostic::simple_boxed(Severity::Error, "set elem failed")
                            })?;

                        // Increment i
                        let one = self.i64_t.const_int(1, false);
                        let i_next = self
                            .builder
                            .build_int_add(i_val_int, one, "i_next")
                            .map_err(|_| {
                                Diagnostic::simple_boxed(Severity::Error, "int add failed")
                            })?;
                        let _ = self.builder.build_store(i_alloca, i_next);

                        // Branch back to loop start
                        let _ = self
                            .builder
                            .build_unconditional_branch(loop_start_bb)
                            .map_err(|_| {
                                Diagnostic::simple_boxed(Severity::Error, "branch failed")
                            })?;

                        // Loop end: store rest array in binding
                        self.builder.position_at_end(loop_end_bb);

                        // Extract binding name from rest pattern
                        let rest_name = match &*rest_pat.arg {
                            Pat::Ident(ident) => ident.sym.clone(),
                            _ => {
                                return Err(Diagnostic::simple_boxed(
                                    Severity::Error,
                                    "Rest pattern must bind to an identifier",
                                ));
                            }
                        };

                        // Create alloca for rest binding
                        let rest_alloca = self
                            .builder
                            .build_alloca(self.i8ptr_t.as_basic_type_enum(), &rest_name)
                            .map_err(|_| {
                                Diagnostic::simple_boxed(Severity::Error, "alloca failed")
                            })?;

                        // Load the final rest array pointer (in case reallocation happened)
                        let final_rest_arr_ptr = self
                            .builder
                            .build_load(self.i8ptr_t, rest_arr_ptr_alloca, "final_rest_arr")
                            .map_err(|_| {
                                Diagnostic::simple_boxed(Severity::Error, "load failed")
                            })?;

                        // Increment RC for the rest array
                        if let BasicValueEnum::PointerValue(rest_pv) = final_rest_arr_ptr {
                            let rc_inc = self.get_rc_inc();
                            let _ = self
                                .builder
                                .build_call(rc_inc, &[rest_pv.into()], "rc_inc_rest")
                                .map_err(|_| {
                                    Diagnostic::simple_boxed(Severity::Error, "rc_inc failed")
                                })?;
                        }

                        // Store rest array
                        let _ = self.builder.build_store(rest_alloca, final_rest_arr_ptr);

                        // Insert into locals
                        self.insert_local_current_scope(
                            locals_stack,
                            crate::codegen::helpers::LocalVarInfo {
                                name: rest_name,
                                ptr: rest_alloca,
                                ty: self.i8ptr_t.as_basic_type_enum(),
                                initialized: true,
                                is_const: matches!(var_decl.kind, VarDeclKind::Const)
                                    || !is_mut_decl,
                                is_weak: false,
                                nominal: None,
                                oats_type: None,
                            },
                        );
                    }
                }
            }
        }

        Ok(())
    }

    /// Lower object destructuring: let {a, b: c} = obj
    fn lower_object_destructuring(
        &self,
        obj_pat: &oats_ast::ObjectPat,
        init: &oats_ast::Expr,
        var_decl: &oats_ast::VarDecl,
        function: FunctionValue<'a>,
        param_map: &HashMap<String, u32>,
        locals_stack: &mut LocalsStackLocal<'a>,
    ) -> crate::diagnostics::DiagnosticResult<()> {
        use oats_ast::*;

        // Lower the initializer (should be an object)
        let obj_val = self.lower_expr(init, function, param_map, locals_stack)?;
        let obj_ptr = if let BasicValueEnum::PointerValue(pv) = obj_val {
            pv
        } else {
            return Err(Diagnostic::simple_boxed(
                Severity::Error,
                "Object destructuring requires an object initializer",
            ));
        };

        let is_mut_decl = matches!(var_decl.kind, VarDeclKind::Let { mutable: true });

        // Determine the nominal type of the object being destructured
        let mut class_name_opt: Option<String> = None;

        // Check if init is an identifier (local or parameter)
        if let Expr::Ident(ident) = init {
            let ident_name = ident.sym.clone();
            // Check if it's a parameter
            if let Some(param_idx) = param_map.get(&ident_name)
                && let Some(param_types) = self
                    .fn_param_types
                    .borrow()
                    .get(function.get_name().to_str().unwrap_or(""))
            {
                let idx = *param_idx as usize;
                if idx < param_types.len()
                    && let crate::types::OatsType::NominalStruct(n) = &param_types[idx]
                {
                    class_name_opt = Some(n.clone());
                }
            }
            // Check if it's a local with nominal type
            if class_name_opt.is_none()
                && let Some((_, _ty, _init, _is_const, _is_weak, nominal, _oats_type)) =
                    self.find_local(locals_stack, &ident_name)
                && let Some(nom) = nominal
            {
                class_name_opt = Some(nom);
            }
        } else if let Expr::New(new_expr) = init
            && let Expr::Ident(ident) = &*new_expr.callee
        {
            // new ClassName() - get class name from callee
            class_name_opt = Some(ident.sym.clone());
        }

        let class_name = class_name_opt.ok_or_else(|| {
            Diagnostic::simple_boxed(
                Severity::Error,
                "Object destructuring requires a typed object (class instance)".to_string(),
            )
        })?;

        // Get class fields
        let fields = self.class_fields.borrow();
        let field_list = fields.get(&class_name).ok_or_else(|| {
            Diagnostic::simple_boxed(
                Severity::Error,
                format!("Class '{}' has no registered fields", class_name),
            )
        })?;

        // Track which properties have been destructured (for rest pattern)
        let mut destructured_fields: std::collections::HashSet<String> =
            std::collections::HashSet::new();

        // First pass: identify rest pattern and track destructured fields
        for prop in &obj_pat.props {
            match prop {
                ObjectPatProp::KeyValue { key, .. } => {
                    let prop_name = match key {
                        PropName::Ident(ident) => ident.sym.clone(),
                        PropName::Str(s) => s.clone(),
                        PropName::Num(n) => n.to_string(),
                    };
                    destructured_fields.insert(prop_name);
                }
                ObjectPatProp::Rest { .. } => {
                    // Rest pattern detected but not currently used
                }
            }
        }

        // Extract each property from the destructuring pattern
        for prop in &obj_pat.props {
            match prop {
                ObjectPatProp::KeyValue { key, value, .. } => {
                    // Get property name from key
                    let prop_name = match key {
                        PropName::Ident(ident) => ident.sym.clone(),
                        PropName::Str(s) => s.clone(),
                        PropName::Num(n) => n.to_string(),
                    };

                    // Find the field in the class
                    let (field_idx, (_fname, _field_ty)) = field_list
                        .iter()
                        .enumerate()
                        .find(|(_, (n, _))| n == &prop_name)
                        .ok_or_else(|| {
                            Diagnostic::simple_boxed(
                                Severity::Error,
                                format!("Class '{}' has no field '{}'", class_name, prop_name),
                            )
                        })?;

                    // Compute field offset: header (8) + meta (8) + field_idx * 8
                    let header_size = 8u64;
                    let meta_size = 8u64;
                    let field_offset = header_size + meta_size + (field_idx as u64 * 8);

                    // Get field pointer using GEP
                    let offset_const = self.i64_t.const_int(field_offset, false);
                    let field_i8ptr = unsafe {
                        self.builder.build_gep(
                            self.i8_t,
                            obj_ptr,
                            &[offset_const],
                            &format!("field_{}_i8ptr", prop_name),
                        )
                    }
                    .map_err(|_| {
                        Diagnostic::simple_boxed(
                            Severity::Error,
                            format!("failed to compute field pointer for '{}'", prop_name),
                        )
                    })?;

                    // Cast to i8ptr and load field value
                    let field_ptr = self
                        .builder
                        .build_pointer_cast(
                            field_i8ptr,
                            self.i8ptr_t,
                            &format!("field_{}_ptr", prop_name),
                        )
                        .map_err(|_| {
                            Diagnostic::simple_boxed(
                                Severity::Error,
                                format!("failed to cast field pointer for '{}'", prop_name),
                            )
                        })?;

                    let loaded = self
                        .builder
                        .build_load(
                            self.i8ptr_t,
                            field_ptr,
                            &format!("field_{}_load", prop_name),
                        )
                        .map_err(|_| {
                            Diagnostic::simple_boxed(
                                Severity::Error,
                                format!("failed to load field '{}'", prop_name),
                            )
                        })?;

                    // Handle the pattern value (binding name)
                    match value {
                        Pat::Ident(binding_ident) => {
                            let binding_name = binding_ident.sym.clone();

                            // Increment RC for the loaded pointer
                            if let BasicValueEnum::PointerValue(loaded_ptr) = loaded {
                                let rc_inc = self.get_rc_inc();
                                let _ = self.builder.build_call(
                                    rc_inc,
                                    &[loaded_ptr.into()],
                                    &format!("rc_inc_{}", binding_name),
                                );
                            }

                            // Create alloca for the binding
                            let alloca = self
                                .builder
                                .build_alloca(self.i8ptr_t.as_basic_type_enum(), &binding_name)
                                .map_err(|_| {
                                    Diagnostic::simple_boxed(
                                        Severity::Error,
                                        format!("alloca failed for '{}'", binding_name),
                                    )
                                })?;

                            // Store the loaded value
                            let _ = self.builder.build_store(alloca, loaded);

                            // Insert into locals
                            self.insert_local_current_scope(
                                locals_stack,
                                crate::codegen::helpers::LocalVarInfo {
                                    name: binding_name,
                                    ptr: alloca,
                                    ty: self.i8ptr_t.as_basic_type_enum(),
                                    initialized: true,
                                    is_const: matches!(var_decl.kind, VarDeclKind::Const)
                                        || !is_mut_decl,
                                    is_weak: false,
                                    nominal: None,
                                    oats_type: None,
                                },
                            );
                        }
                        Pat::Object(nested_obj_pat) => {
                            // Nested object destructuring: let {a: {b, c}} = obj
                            // The loaded value is the nested object, recursively destructure it
                            if let BasicValueEnum::PointerValue(nested_obj_ptr) = loaded {
                                // Create a temporary variable to hold the nested object
                                // This allows us to reuse the existing destructuring logic
                                let temp_name = format!("__nested_obj_{}", prop_name);
                                let temp_alloca = self
                                    .builder
                                    .build_alloca(self.i8ptr_t.as_basic_type_enum(), &temp_name)
                                    .map_err(|_| {
                                        Diagnostic::simple_boxed(
                                            Severity::Error,
                                            format!(
                                                "alloca failed for nested object '{}'",
                                                prop_name
                                            ),
                                        )
                                    })?;

                                // Increment RC for the nested object
                                let rc_inc = self.get_rc_inc();
                                let _ = self.builder.build_call(
                                    rc_inc,
                                    &[nested_obj_ptr.into()],
                                    &format!("rc_inc_nested_{}", prop_name),
                                );

                                // Store the loaded nested object
                                let _ = self.builder.build_store(temp_alloca, loaded);

                                // Insert temporary into locals
                                self.insert_local_current_scope(
                                    locals_stack,
                                    crate::codegen::helpers::LocalVarInfo {
                                        name: temp_name.clone(),
                                        ptr: temp_alloca,
                                        ty: self.i8ptr_t.as_basic_type_enum(),
                                        initialized: true,
                                        is_const: true, // Temporary, immutable
                                        is_weak: false,
                                        nominal: None, // TODO: Could infer from field type
                                        oats_type: None,
                                    },
                                );

                                // Create a synthetic identifier expression for the temporary
                                // Use the nested pattern's span
                                let temp_ident = oats_ast::Expr::Ident(oats_ast::Ident {
                                    sym: temp_name.clone(),
                                    span: nested_obj_pat.span.clone(),
                                });

                                // Recursively destructure the nested object
                                self.lower_object_destructuring(
                                    nested_obj_pat,
                                    &temp_ident,
                                    var_decl,
                                    function,
                                    param_map,
                                    locals_stack,
                                )?;
                            } else {
                                return Err(Diagnostic::simple_boxed(
                                    Severity::Error,
                                    format!(
                                        "Nested object destructuring requires object type for field '{}'",
                                        prop_name
                                    ),
                                ));
                            }
                        }
                        Pat::Array(nested_arr_pat) => {
                            // Nested array destructuring: let {a: [b, c]} = obj
                            // The loaded value is the nested array, recursively destructure it
                            if let BasicValueEnum::PointerValue(nested_arr_ptr) = loaded {
                                // Create a temporary variable to hold the nested array
                                let temp_name = format!("__nested_arr_{}", prop_name);
                                let temp_alloca = self
                                    .builder
                                    .build_alloca(self.i8ptr_t.as_basic_type_enum(), &temp_name)
                                    .map_err(|_| {
                                        Diagnostic::simple_boxed(
                                            Severity::Error,
                                            format!(
                                                "alloca failed for nested array '{}'",
                                                prop_name
                                            ),
                                        )
                                    })?;

                                // Increment RC for the nested array
                                let rc_inc = self.get_rc_inc();
                                let _ = self.builder.build_call(
                                    rc_inc,
                                    &[nested_arr_ptr.into()],
                                    &format!("rc_inc_nested_arr_{}", prop_name),
                                );

                                // Store the loaded nested array
                                let _ = self.builder.build_store(temp_alloca, loaded);

                                // Insert temporary into locals
                                self.insert_local_current_scope(
                                    locals_stack,
                                    crate::codegen::helpers::LocalVarInfo {
                                        name: temp_name.clone(),
                                        ptr: temp_alloca,
                                        ty: self.i8ptr_t.as_basic_type_enum(),
                                        initialized: true,
                                        is_const: true,
                                        is_weak: false,
                                        nominal: None,
                                        oats_type: None,
                                    },
                                );

                                // Create a synthetic identifier expression for the temporary
                                // Use the nested pattern's span
                                let temp_ident = oats_ast::Expr::Ident(oats_ast::Ident {
                                    sym: temp_name.clone(),
                                    span: nested_arr_pat.span.clone(),
                                });

                                // Recursively destructure the nested array
                                self.lower_array_destructuring(
                                    nested_arr_pat,
                                    &temp_ident,
                                    var_decl,
                                    function,
                                    param_map,
                                    locals_stack,
                                )?;
                            } else {
                                return Err(Diagnostic::simple_boxed(
                                    Severity::Error,
                                    format!(
                                        "Nested array destructuring requires array type for field '{}'",
                                        prop_name
                                    ),
                                ));
                            }
                        }
                        Pat::Rest(_) => {
                            return Err(Diagnostic::simple_boxed(
                                Severity::Error,
                                "Rest patterns in object destructuring not yet supported",
                            ));
                        }
                    }
                }
                ObjectPatProp::Rest { arg, .. } => {
                    // Rest pattern: let {a, b, ...rest} = obj
                    // Create a new object with all remaining properties

                    // Extract binding name from rest pattern
                    let rest_name = arg.sym.clone();

                    // Calculate how many fields remain (not destructured)
                    let remaining_fields: Vec<(usize, (String, crate::types::OatsType))> =
                        field_list
                            .iter()
                            .enumerate()
                            .filter(|(_, (name, _))| !destructured_fields.contains(name))
                            .map(|(idx, (name, ty))| (idx, (name.clone(), ty.clone())))
                            .collect();

                    let rest_field_count = remaining_fields.len();

                    // Allocate memory for rest object: header (8) + meta (8) + fields (8 each)
                    let header_size = 8u64;
                    let meta_size = 8u64;
                    let field_size = 8u64;
                    let total_size =
                        header_size + meta_size + (rest_field_count as u64 * field_size);

                    let malloc_fn = self.get_malloc();
                    let size_const = self.i64_t.const_int(total_size, false);
                    let malloc_call = self
                        .builder
                        .build_call(malloc_fn, &[size_const.into()], "rest_obj_malloc")
                        .map_err(|_| Diagnostic::simple_boxed(Severity::Error, "malloc failed"))?;

                    let rest_obj_ptr =
                        if let inkwell::Either::Left(bv) = malloc_call.try_as_basic_value() {
                            bv.into_pointer_value()
                        } else {
                            return Err(Diagnostic::simple_boxed(
                                Severity::Error,
                                "malloc returned no value",
                            ));
                        };

                    // Initialize header (static bit set)
                    let header_offset = self.i64_t.const_int(0, false);
                    let header_ptr = unsafe {
                        self.builder.build_gep(
                            self.i64_t,
                            rest_obj_ptr,
                            &[header_offset],
                            "rest_header_ptr",
                        )
                    }
                    .map_err(|_| Diagnostic::simple_boxed(Severity::Error, "gep failed"))?;
                    let static_header = self.i64_t.const_int(1u64 << 32, false);
                    let _ = self.builder.build_store(header_ptr, static_header);

                    // Initialize meta slot (field count)
                    let meta_offset = self.i64_t.const_int(1, false);
                    let meta_ptr = unsafe {
                        self.builder.build_gep(
                            self.i64_t,
                            rest_obj_ptr,
                            &[meta_offset],
                            "rest_meta_ptr",
                        )
                    }
                    .map_err(|_| Diagnostic::simple_boxed(Severity::Error, "gep failed"))?;
                    let field_count_val = self.i64_t.const_int(rest_field_count as u64, false);
                    let _ = self.builder.build_store(meta_ptr, field_count_val);

                    // Copy each remaining field from source to rest object
                    for (rest_idx, (field_idx, (field_name, _field_ty))) in
                        remaining_fields.iter().enumerate()
                    {
                        // Get source field value
                        let src_field_offset =
                            header_size + meta_size + (*field_idx as u64 * field_size);
                        let src_offset_const = self.i64_t.const_int(src_field_offset, false);
                        let src_field_i8ptr = unsafe {
                            self.builder.build_gep(
                                self.i8_t,
                                obj_ptr,
                                &[src_offset_const],
                                &format!("src_field_{}_i8ptr", field_name),
                            )
                        }
                        .map_err(|_| Diagnostic::simple_boxed(Severity::Error, "gep failed"))?;

                        let src_field_ptr = self
                            .builder
                            .build_pointer_cast(
                                src_field_i8ptr,
                                self.i8ptr_t,
                                &format!("src_field_{}_ptr", field_name),
                            )
                            .map_err(|_| {
                                Diagnostic::simple_boxed(Severity::Error, "cast failed")
                            })?;

                        let src_field_val = self
                            .builder
                            .build_load(
                                self.i8ptr_t,
                                src_field_ptr,
                                &format!("src_field_{}_load", field_name),
                            )
                            .map_err(|_| {
                                Diagnostic::simple_boxed(Severity::Error, "load failed")
                            })?;

                        // Increment RC for copied field
                        if let BasicValueEnum::PointerValue(src_pv) = src_field_val {
                            let rc_inc = self.get_rc_inc();
                            let _ = self
                                .builder
                                .build_call(
                                    rc_inc,
                                    &[src_pv.into()],
                                    &format!("rc_inc_rest_{}", field_name),
                                )
                                .map_err(|_| {
                                    Diagnostic::simple_boxed(Severity::Error, "rc_inc failed")
                                })?;
                        }

                        // Store in rest object
                        let rest_field_offset =
                            header_size + meta_size + (rest_idx as u64 * field_size);
                        let rest_offset_const = self.i64_t.const_int(rest_field_offset, false);
                        let rest_field_i8ptr = unsafe {
                            self.builder.build_gep(
                                self.i8_t,
                                rest_obj_ptr,
                                &[rest_offset_const],
                                &format!("rest_field_{}_i8ptr", field_name),
                            )
                        }
                        .map_err(|_| Diagnostic::simple_boxed(Severity::Error, "gep failed"))?;

                        let rest_field_ptr = self
                            .builder
                            .build_pointer_cast(
                                rest_field_i8ptr,
                                self.i8ptr_t,
                                &format!("rest_field_{}_ptr", field_name),
                            )
                            .map_err(|_| {
                                Diagnostic::simple_boxed(Severity::Error, "cast failed")
                            })?;

                        let _ = self.builder.build_store(rest_field_ptr, src_field_val);
                    }

                    // Create alloca for rest binding
                    let rest_alloca = self
                        .builder
                        .build_alloca(self.i8ptr_t.as_basic_type_enum(), &rest_name)
                        .map_err(|_| Diagnostic::simple_boxed(Severity::Error, "alloca failed"))?;

                    // Increment RC for the rest object
                    let rc_inc = self.get_rc_inc();
                    let _ = self
                        .builder
                        .build_call(rc_inc, &[rest_obj_ptr.into()], "rc_inc_rest_obj")
                        .map_err(|_| Diagnostic::simple_boxed(Severity::Error, "rc_inc failed"))?;

                    // Store rest object
                    let _ = self
                        .builder
                        .build_store(rest_alloca, rest_obj_ptr.as_basic_value_enum());

                    // Insert into locals
                    self.insert_local_current_scope(
                        locals_stack,
                        crate::codegen::helpers::LocalVarInfo {
                            name: rest_name,
                            ptr: rest_alloca,
                            ty: self.i8ptr_t.as_basic_type_enum(),
                            initialized: true,
                            is_const: matches!(var_decl.kind, VarDeclKind::Const) || !is_mut_decl,
                            is_weak: false,
                            nominal: Some(class_name.clone()), // Rest object has same type as source
                            oats_type: None,
                        },
                    );
                }
            }
        }

        Ok(())
    }

    pub(crate) fn lower_decl_stmt(
        &self,
        var_decl: &oats_ast::VarDecl,
        function: FunctionValue<'a>,
        param_map: &HashMap<String, u32>,
        locals_stack: &mut LocalsStackLocal<'a>,
    ) -> crate::diagnostics::DiagnosticResult<bool> {
        for decl in &var_decl.decls {
            // Handle destructuring patterns
            match &decl.name {
                oats_ast::Pat::Array(arr_pat) => {
                    // Array destructuring: let [a, b, c] = arr
                    if let Some(init) = &decl.init {
                        self.lower_array_destructuring(
                            arr_pat,
                            init,
                            var_decl,
                            function,
                            param_map,
                            locals_stack,
                        )?;
                    } else {
                        return Err(crate::diagnostics::Diagnostic::error(
                            "Array destructuring requires an initializer",
                        )
                        .with_code("E1005")
                        .with_label(crate::diagnostics::Label {
                            span: crate::diagnostics::Span {
                                start: decl.span.start,
                                end: decl.span.end,
                            },
                            message: "Array destructuring must have an initializer".into(),
                        })
                        .into());
                    }
                    continue;
                }
                oats_ast::Pat::Object(obj_pat) => {
                    // Object destructuring: let {a, b: c} = obj
                    if let Some(init) = &decl.init {
                        self.lower_object_destructuring(
                            obj_pat,
                            init,
                            var_decl,
                            function,
                            param_map,
                            locals_stack,
                        )?;
                    } else {
                        return Err(crate::diagnostics::Diagnostic::error(
                            "Object destructuring requires an initializer",
                        )
                        .with_code("E1006")
                        .with_label(crate::diagnostics::Label {
                            span: crate::diagnostics::Span {
                                start: decl.span.start,
                                end: decl.span.end,
                            },
                            message: "Object destructuring must have an initializer".into(),
                        })
                        .into());
                    }
                    continue;
                }
                oats_ast::Pat::Rest(_) => {
                    return Err(crate::diagnostics::Diagnostic::error(
                        "Rest patterns in variable declarations not yet supported",
                    )
                    .with_code("E1007")
                    .with_label(crate::diagnostics::Label {
                        span: crate::diagnostics::Span {
                            start: decl.span.start,
                            end: decl.span.end,
                        },
                        message: "Rest patterns (...rest) not yet supported".into(),
                    })
                    .into());
                }
                _ => {}
            }

            // Simple identifier pattern
            let name = match &decl.name {
                oats_ast::Pat::Ident(ident) => ident.sym.clone(),
                _ => {
                    return Err(crate::diagnostics::Diagnostic::error(
                        "Unsupported pattern in variable declaration",
                    )
                    .with_code("E1004")
                    .with_label(crate::diagnostics::Label {
                        span: crate::diagnostics::Span {
                            start: decl.span.start,
                            end: decl.span.end,
                        },
                        message: "Unsupported pattern type".into(),
                    })
                    .into());
                }
            };
            {
                // Language Spec v2: `let` is immutable by default. Support
                // `let mut` by checking the mutable flag in VarDeclKind::Let.
                let is_mut_decl =
                    matches!(var_decl.kind, oats_ast::VarDeclKind::Let { mutable: true });

                // Resolve type information
                let type_info = self.resolve_decl_type(decl)?;
                let declared_mapped = type_info.declared_mapped;
                let declared_union = type_info.declared_union;
                let declared_is_weak = type_info.declared_is_weak;
                let mut declared_nominal = type_info.declared_nominal;
                let init_inferred = type_info.init_inferred;

                // Processing var decl: {} kind={:?}

                if let Some(init) = &decl.init {
                    // Handle const evaluation
                    if self.handle_const_decl(init, var_decl, &name)? {
                        continue;
                    }

                    // Infer a local OatsType from the initializer expression
                    let init_inferred = init_inferred
                        .as_ref()
                        .ok_or_else(|| crate::diagnostics::Diagnostic::simple_boxed(
                            Severity::Error,
                            "Internal error: failed to infer type from initializer (this should not happen)",
                        ))?
                        .clone();

                    // Handle object literal initialization
                    self.handle_object_literal_init(init, &declared_nominal)?;

                    // Handle new expression initialization
                    if self.handle_new_expr_init(
                        init,
                        &name,
                        declared_is_weak,
                        &mut declared_nominal,
                        &declared_union,
                        locals_stack,
                    )? {
                        // Continue to lower the new expression normally
                    }

                    // Handle tuple initialization
                    let var_ctx = VarDeclContext {
                        name: name.clone(),
                        var_decl,
                        is_mut_decl,
                        declared_is_weak,
                        declared_union: declared_union.clone(),
                        declared_nominal: declared_nominal.clone(),
                    };
                    if self.handle_tuple_init(
                        init,
                        &declared_mapped,
                        &var_ctx,
                        function,
                        param_map,
                        locals_stack,
                    )? {
                        continue;
                    }

                    // Lower the initializer expression
                    if let Ok(val) = self.lower_expr(init, function, param_map, locals_stack) {
                        // Apply union boxing if needed
                        let (val, allocated_ty) = self.handle_union_boxing(val, &declared_union)?;

                        // If a local with this name was pre-inserted (for
                        // example when we pre-allocated a slot for `new`
                        // or `await` initializers), reuse its alloca so we
                        // don't create duplicate slots. Otherwise allocate
                        // a fresh alloca for the local.
                        let maybe_existing = self.find_local(locals_stack, &name);
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
                                        &crate::diagnostics::Diagnostic::simple_boxed(
                                            Severity::Error,
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
                            let _ = self
                                .builder
                                .build_call(rc_inc, &[pv.into()], "rc_inc_local");
                        }
                        let _ = self.builder.build_store(alloca, val);
                        // If we reused an existing local, mark it initialized.
                        if maybe_existing.is_some() {
                            self.set_local_initialized(locals_stack, &name, true);
                        } else {
                            // mark initialized in locals; is_const=false by default
                            // For `let`/`const` declarations, insert into current scope (block-scoped)
                            self.insert_local_current_scope(
                                locals_stack,
                                crate::codegen::helpers::LocalVarInfo {
                                    name: name.clone(),
                                    ptr: alloca,
                                    ty: allocated_ty,
                                    initialized: true,
                                    // If this was a `let` without `mut`, treat as const/immutable
                                    is_const: matches!(var_decl.kind, oats_ast::VarDeclKind::Const)
                                        || !is_mut_decl,
                                    is_weak: declared_is_weak,
                                    nominal: declared_nominal.clone(),
                                    oats_type: declared_mapped
                                        .clone()
                                        .or(Some(init_inferred.clone())),
                                },
                            );
                        }
                    } else {
                        // Lowering failed, handle uninitialized variable
                        let var_ctx = VarDeclContext {
                            name: name.clone(),
                            var_decl,
                            is_mut_decl,
                            declared_is_weak,
                            declared_union: declared_union.clone(),
                            declared_nominal: declared_nominal.clone(),
                        };
                        self.handle_uninitialized_var(&var_ctx, locals_stack)?;
                    }
                } else {
                    // No initializer: create an uninitialized slot
                    let var_ctx = VarDeclContext {
                        name: name.clone(),
                        var_decl,
                        is_mut_decl,
                        declared_is_weak,
                        declared_union: declared_union.clone(),
                        declared_nominal: declared_nominal.clone(),
                    };
                    self.handle_uninitialized_var(&var_ctx, locals_stack)?;
                }
            }
        }

        Ok(false)
    }

    /// Handle uninitialized variable declarations.
    ///
    /// Creates an uninitialized slot using `i64` as a conservative ABI choice
    /// (or `i8ptr` for unions). The slot will be marked uninitialized so further
    /// lowering can detect reads of uninitialized locals and emit diagnostics.
    fn handle_uninitialized_var<'b>(
        &self,
        var_ctx: &VarDeclContext<'b>,
        locals_stack: &mut LocalsStackLocal<'a>,
    ) -> crate::diagnostics::DiagnosticResult<()> {
        // For union types, use i8ptr since unions are boxed.
        let ty = if var_ctx.declared_union.is_some() {
            self.i8ptr_t.as_basic_type_enum()
        } else {
            self.i64_t.as_basic_type_enum()
        };
        let alloca = match self.builder.build_alloca(ty, &var_ctx.name) {
            Ok(a) => a,
            Err(_) => {
                crate::diagnostics::emit_diagnostic(
                    &crate::diagnostics::Diagnostic::simple_boxed(
                        Severity::Error,
                        "alloca failed for uninitialized var",
                    ),
                    Some(self.source),
                );
                return Ok(());
            }
        };
        self.insert_local_current_scope(
            locals_stack,
            crate::codegen::helpers::LocalVarInfo {
                name: var_ctx.name.clone(),
                ptr: alloca,
                ty,
                initialized: false,
                is_const: matches!(var_ctx.var_decl.kind, oats_ast::VarDeclKind::Const)
                    || !var_ctx.is_mut_decl,
                is_weak: var_ctx.declared_is_weak,
                nominal: var_ctx.declared_nominal.clone(),
                oats_type: var_ctx.declared_union.clone(),
            },
        );
        Ok(())
    }
}
