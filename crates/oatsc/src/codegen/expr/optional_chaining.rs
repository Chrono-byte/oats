//! Code generation for optional chaining expressions (`?.`).
//!
//! Optional chaining (`obj?.prop`, `arr?.[0]`) safely accesses properties/methods
//! when the object might be null or undefined. If the object is null/undefined,
//! the entire expression evaluates to `undefined` (represented as a null pointer).

use crate::codegen::CodeGen;
use crate::diagnostics::{Diagnostic, Severity};
use inkwell::types::BasicType;
use inkwell::values::{BasicValue, BasicValueEnum, FunctionValue};
use oxc_ast::ast::Expression;
use std::collections::HashMap;

#[allow(dead_code)]
type LocalEntry<'a> = (
    inkwell::values::PointerValue<'a>,
    inkwell::types::BasicTypeEnum<'a>,
    bool,
    bool,
    bool,
    Option<String>,
    Option<crate::types::OatsType>,
);
type LocalsStackLocal<'a> = Vec<std::collections::HashMap<String, LocalEntry<'a>>>;

impl<'a> CodeGen<'a> {
    /// Lowers an optional member expression (`obj?.prop`, `arr?.[0]`, or `func?.()`).
    ///
    /// Generates code that checks for null/undefined before accessing the member or calling.
    /// If the object is null/undefined, returns null pointer (undefined).
    #[allow(clippy::result_large_err)]
    pub(super) fn lower_optional_member_expr(
        &self,
        chain: &oxc_ast::ast::ChainExpression<'_>,
        function: FunctionValue<'a>,
        param_map: &HashMap<String, u32>,
        locals: &mut LocalsStackLocal<'a>,
    ) -> crate::diagnostics::DiagnosticResult<BasicValueEnum<'a>> {
        use oxc_ast::ast::ChainElement;

        // Extract the object from the chain element
        let obj_expr = match &chain.expression {
            ChainElement::StaticMemberExpression(member) => &member.object,
            ChainElement::ComputedMemberExpression(member) => &member.object,
            ChainElement::CallExpression(call) => {
                // For call expressions, the object is the callee
                &call.callee
            }
            _ => {
                return Err(Diagnostic::simple_with_span_boxed(
                    Severity::Error,
                    "unsupported chain element type in optional chaining",
                    chain.span.start as usize,
                ));
            }
        };

        // Lower the object expression
        let obj_val = self.lower_expr(obj_expr, function, param_map, locals)?;

        // Check if object is null/undefined (null pointer)
        let obj_ptr = match obj_val {
            BasicValueEnum::PointerValue(pv) => pv,
            _ => {
                // For non-pointer values, optional chaining doesn't apply - return value directly
                return Ok(obj_val);
            }
        };

        // Allocate storage for result (will hold null or the access result)
        let result_alloca = self
            .builder
            .build_alloca(self.i8ptr_t.as_basic_type_enum(), "opt_chain_result")
            .map_err(|_| {
                Diagnostic::simple_with_span_boxed(
                    Severity::Error,
                    "failed to allocate result for optional chaining",
                    chain.span.start as usize,
                )
            })?;

        // Initialize with null (for the null case)
        let null_ptr = self.i8ptr_t.const_null();
        let _ = self
            .builder
            .build_store(result_alloca, null_ptr.as_basic_value_enum());

        // Create basic blocks
        let access_bb = self
            .context
            .append_basic_block(function, "opt_chain_access");
        let merge_bb = self.context.append_basic_block(function, "opt_chain_merge");

        // Check if pointer is null
        let is_null = self
            .builder
            .build_is_null(obj_ptr, "opt_chain_is_null")
            .map_err(|_| {
                Diagnostic::simple_with_span_boxed(
                    Severity::Error,
                    "failed to check null for optional chaining",
                    chain.span.start as usize,
                )
            })?;

        // Branch: if null, go to merge (already has null stored), otherwise go to access
        let _ = self
            .builder
            .build_conditional_branch(is_null, merge_bb, access_bb)
            .map_err(|_| {
                Diagnostic::simple_with_span_boxed(
                    Severity::Error,
                    "failed to branch for optional chaining",
                    chain.span.start as usize,
                )
            })?;

        // Access block: perform the actual member access or call
        self.builder.position_at_end(access_bb);

        // Perform the member access or call based on chain element type
        let access_result = match &chain.expression {
            ChainElement::StaticMemberExpression(member) => self.lower_optional_static_member(
                obj_ptr,
                member.as_ref(),
                function,
                param_map,
                locals,
            ),
            ChainElement::ComputedMemberExpression(member) => {
                let idx_val = self.lower_expr(&member.expression, function, param_map, locals)?;
                self.lower_optional_computed_member(obj_ptr, idx_val, function, param_map, locals)
            }
            ChainElement::CallExpression(call) => {
                self.lower_optional_call(obj_ptr, call.as_ref(), function, param_map, locals)
            }
            _ => {
                return Err(Diagnostic::simple_with_span_boxed(
                    Severity::Error,
                    "unsupported chain element type",
                    chain.span.start as usize,
                ));
            }
        }?;

        // Box the result if it's numeric (to have a consistent i8* return type)
        let boxed_result = match access_result {
            BasicValueEnum::FloatValue(fv) => {
                let box_fn = self.get_union_box_f64();
                let cs = self
                    .builder
                    .build_call(box_fn, &[fv.into()], "box_opt_result")
                    .map_err(|_| Diagnostic::error("box call failed"))?;
                match cs.try_as_basic_value() {
                    inkwell::values::ValueKind::Basic(bv) => bv,
                    _ => {
                        return Err(Diagnostic::simple_boxed(
                            Severity::Error,
                            "box returned void",
                        ));
                    }
                }
            }
            BasicValueEnum::IntValue(iv) => {
                let fv = self
                    .builder
                    .build_signed_int_to_float(iv, self.f64_t, "i2f")
                    .map_err(|_| Diagnostic::error("int to float failed"))?;
                let box_fn = self.get_union_box_f64();
                let cs = self
                    .builder
                    .build_call(box_fn, &[fv.into()], "box_opt_result")
                    .map_err(|_| Diagnostic::error("box call failed"))?;
                match cs.try_as_basic_value() {
                    inkwell::values::ValueKind::Basic(bv) => bv,
                    _ => {
                        return Err(Diagnostic::simple_boxed(
                            Severity::Error,
                            "box returned void",
                        ));
                    }
                }
            }
            BasicValueEnum::PointerValue(pv) => {
                // Already a pointer, increment refcount
                let rc_inc = self.get_rc_inc();
                let _ = self
                    .builder
                    .build_call(rc_inc, &[pv.into()], "rc_inc_opt")
                    .map_err(|_| Diagnostic::error("rc_inc failed"))?;
                pv.as_basic_value_enum()
            }
            _ => {
                // Other types (ArrayValue, StructValue, VectorValue) are not expected
                // in optional chaining results, but handle gracefully
                return Err(Diagnostic::simple_with_span_boxed(
                    Severity::Error,
                    "unexpected value type in optional chaining result",
                    chain.span.start as usize,
                ));
            }
        };

        // Store the boxed result (overwrites the null we stored earlier)
        let _ = self.builder.build_store(result_alloca, boxed_result);

        // Branch to merge block
        let _ = self
            .builder
            .build_unconditional_branch(merge_bb)
            .map_err(|_| {
                Diagnostic::simple_with_span_boxed(
                    Severity::Error,
                    "failed to branch to merge",
                    chain.span.start as usize,
                )
            })?;

        // Merge block: load and return the result
        self.builder.position_at_end(merge_bb);
        let result = self
            .builder
            .build_load(
                self.i8ptr_t.as_basic_type_enum(),
                result_alloca,
                "opt_result",
            )
            .map_err(|_| {
                Diagnostic::simple_with_span_boxed(
                    Severity::Error,
                    "failed to load optional chaining result",
                    chain.span.start as usize,
                )
            })?;

        Ok(result.as_basic_value_enum())
    }

    /// Lower optional static member access (obj?.prop)
    fn lower_optional_static_member(
        &self,
        obj_ptr: inkwell::values::PointerValue<'a>,
        member: &oxc_ast::ast::StaticMemberExpression<'_>,
        function: FunctionValue<'a>,
        param_map: &HashMap<String, u32>,
        locals: &mut LocalsStackLocal<'a>,
    ) -> crate::diagnostics::DiagnosticResult<BasicValueEnum<'a>> {
        let field_name = member.property.name.to_string();

        // Try to get the class name from the object expression
        let mut class_name_opt: Option<String> = None;
        if let Expression::Identifier(ident) = &member.object {
            let ident_name = ident.name.to_string();
            if ident_name == "this" {
                let fname = function.get_name().to_str().unwrap_or_default();
                if let Some(param_types) = self.fn_param_types.borrow().get(fname)
                    && !param_types.is_empty()
                    && let crate::types::OatsType::NominalStruct(n) = &param_types[0]
                {
                    class_name_opt = Some(n.clone());
                }
            } else if let Some(param_idx) = param_map.get(&ident_name) {
                let fname = function.get_name().to_str().unwrap_or_default();
                if let Some(param_types) = self.fn_param_types.borrow().get(fname) {
                    let idx = *param_idx as usize;
                    if idx < param_types.len()
                        && let crate::types::OatsType::NominalStruct(n) = &param_types[idx]
                    {
                        class_name_opt = Some(n.clone());
                    }
                }
            } else if let Some((_, _, _, _, _, nominal, _)) = self.find_local(locals, &ident_name) {
                class_name_opt = nominal;
            }
        }

        // If we have a class name, get the field offset
        if let Some(class_name) = class_name_opt
            && let Some(fields) = self.class_fields.borrow().get(&class_name)
        {
            let mut field_offset = 16u64; // Start after header (8) + meta_ptr (8)
            for (fname, _ftype) in fields {
                if fname == &field_name {
                    // Found the field - compute offset and load it
                    let offset = self.i64_t.const_int(field_offset, false);
                    let field_i8ptr = self
                        .i8_ptr_from_offset_i64(obj_ptr, offset, "field_i8ptr")
                        .map_err(|_| Diagnostic::error("failed to compute field pointer"))?;

                    let field_ptr_ty = self.context.ptr_type(inkwell::AddressSpace::default());
                    let field_ptr = self
                        .builder
                        .build_pointer_cast(field_i8ptr, field_ptr_ty, "field_ptr")
                        .map_err(|_| Diagnostic::error("failed to cast field pointer"))?;

                    let loaded = self
                        .builder
                        .build_load(self.i8ptr_t, field_ptr, "field_load")
                        .map_err(|_| Diagnostic::error("failed to load field"))?;

                    return Ok(loaded.as_basic_value_enum());
                }
                field_offset += 8; // Increment by pointer size
            }
        }

        // Fallback: if we can't determine the field offset, return an error
        Err(Diagnostic::simple_with_span_boxed(
            Severity::Error,
            format!("could not determine field offset for '{field_name}' in optional chaining"),
            member.span.start as usize,
        ))
    }

    /// Lower optional computed member access (arr?.[0])
    fn lower_optional_computed_member(
        &self,
        obj_ptr: inkwell::values::PointerValue<'a>,
        idx_val: BasicValueEnum<'a>,
        _function: FunctionValue<'a>,
        _param_map: &HashMap<String, u32>,
        _locals: &mut LocalsStackLocal<'a>,
    ) -> crate::diagnostics::DiagnosticResult<BasicValueEnum<'a>> {
        // Use array_get_ptr for pointer arrays
        let array_get_ptr_fn = self.get_array_get_ptr();
        let idx_i64 = match idx_val {
            BasicValueEnum::IntValue(iv) => self
                .builder
                .build_int_cast(iv, self.i64_t, "idx_i64")
                .map_err(|_| Diagnostic::error("LLVM builder error"))?,
            BasicValueEnum::FloatValue(fv) => self
                .builder
                .build_float_to_signed_int(fv, self.i64_t, "f2i")
                .map_err(|_| Diagnostic::error("LLVM builder error"))?,
            _ => {
                return Err(Diagnostic::simple_boxed(
                    Severity::Error,
                    "unsupported index type for optional computed member access",
                ));
            }
        };

        let cs = self
            .builder
            .build_call(
                array_get_ptr_fn,
                &[obj_ptr.into(), idx_i64.into()],
                "opt_array_get",
            )
            .map_err(|_| Diagnostic::error("array_get_ptr call failed"))?;

        match cs.try_as_basic_value() {
            inkwell::values::ValueKind::Basic(bv) => Ok(bv),
            _ => Err(Diagnostic::simple_boxed(
                Severity::Error,
                "array_get_ptr returned void",
            )),
        }
    }

    /// Lower optional call (func?.())
    fn lower_optional_call(
        &self,
        callee_ptr: inkwell::values::PointerValue<'a>,
        call: &oxc_ast::ast::CallExpression<'_>,
        function: FunctionValue<'a>,
        param_map: &HashMap<String, u32>,
        locals: &mut LocalsStackLocal<'a>,
    ) -> crate::diagnostics::DiagnosticResult<BasicValueEnum<'a>> {
        // Allocate storage for result (will hold null or the call result)
        let result_alloca = self
            .builder
            .build_alloca(self.i8ptr_t.as_basic_type_enum(), "opt_call_result")
            .map_err(|_| {
                Diagnostic::simple_with_span_boxed(
                    Severity::Error,
                    "failed to allocate result for optional call",
                    call.span.start as usize,
                )
            })?;

        // Initialize with null (for the null case)
        let null_ptr = self.i8ptr_t.const_null();
        let _ = self
            .builder
            .build_store(result_alloca, null_ptr.as_basic_value_enum());

        // Create basic blocks
        let call_bb = self.context.append_basic_block(function, "opt_call_exec");
        let merge_bb = self.context.append_basic_block(function, "opt_call_merge");

        // Check if callee is null
        let is_null = self
            .builder
            .build_is_null(callee_ptr, "opt_call_is_null")
            .map_err(|_| {
                Diagnostic::simple_with_span_boxed(
                    Severity::Error,
                    "failed to check null for optional call",
                    call.span.start as usize,
                )
            })?;

        // Branch: if null, go to merge (result is already null), else call
        let _ = self
            .builder
            .build_conditional_branch(is_null, merge_bb, call_bb);

        // Call block: perform the function call
        self.builder.position_at_end(call_bb);

        // Lower arguments
        let mut lowered_args: Vec<inkwell::values::BasicMetadataValueEnum> = Vec::new();
        for arg in &call.arguments {
            if let Some(expr) = arg.as_expression() {
                if let Ok(val) = self.lower_expr(expr, function, param_map, locals) {
                    lowered_args.push(val.into());
                } else {
                    return Err(Diagnostic::simple_boxed(
                        Severity::Error,
                        "expression lowering failed in optional call",
                    ));
                }
            } else {
                return Err(Diagnostic::simple_boxed(
                    Severity::Error,
                    "spread arguments not supported in optional calls",
                ));
            }
        }

        // Perform indirect call through the pointer
        // Create function type based on arguments (simplified: assume returns i8*)
        let mut param_types: Vec<inkwell::types::BasicMetadataTypeEnum> = Vec::new();
        for _ in &lowered_args {
            param_types.push(self.i8ptr_t.into()); // Assume all args are pointers
        }
        let fn_type = self.i8ptr_t.fn_type(&param_types, false);

        let call_result = self
            .builder
            .build_indirect_call(fn_type, callee_ptr, &lowered_args, "opt_call_result")
            .map_err(|_| {
                Diagnostic::simple_with_span_boxed(
                    Severity::Error,
                    "failed to build indirect call for optional call",
                    call.span.start as usize,
                )
            })?;

        // Store the result
        match call_result.try_as_basic_value() {
            inkwell::values::ValueKind::Basic(bv) => {
                let _ = self.builder.build_store(result_alloca, bv);
            }
            _ => {
                // Void return, store null
                let _ = self
                    .builder
                    .build_store(result_alloca, null_ptr.as_basic_value_enum());
            }
        }

        // Branch to merge
        let _ = self.builder.build_unconditional_branch(merge_bb);

        // Merge block: return the result
        self.builder.position_at_end(merge_bb);
        let final_result = self
            .builder
            .build_load(self.i8ptr_t, result_alloca, "opt_call_final")
            .map_err(|_| {
                Diagnostic::simple_with_span_boxed(
                    Severity::Error,
                    "failed to load optional call result",
                    call.span.start as usize,
                )
            })?;

        Ok(final_result.as_basic_value_enum())
    }
}
