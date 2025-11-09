//! Code generation for optional chaining expressions (`?.`).
//!
//! Optional chaining (`obj?.prop`, `arr?.[0]`) safely accesses properties/methods
//! when the object might be null or undefined. If the object is null/undefined,
//! the entire expression evaluates to `undefined` (represented as a null pointer).

use crate::codegen::CodeGen;
use crate::diagnostics::{Diagnostic, Severity};
use inkwell::values::{BasicValue, BasicValueEnum, FunctionValue};
use oats_ast::*;
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
type LocalsStackLocal<'a> = Vec<std::collections::HashMap<String, LocalEntry<'a>>>;

impl<'a> CodeGen<'a> {
    /// Lowers an optional member expression (`obj?.prop` or `arr?.[0]`).
    ///
    /// Generates code that checks for null/undefined before accessing the member.
    /// If the object is null/undefined, returns null pointer (undefined).
    #[allow(clippy::result_large_err)]
    pub(super) fn lower_optional_member_expr(
        &self,
        optional: &OptionalMemberExpr,
        function: FunctionValue<'a>,
        param_map: &HashMap<String, u32>,
        locals: &mut LocalsStackLocal<'a>,
    ) -> crate::diagnostics::DiagnosticResult<BasicValueEnum<'a>> {
        // Lower the object expression first
        let obj_val = self.lower_expr(&optional.obj, function, param_map, locals)?;

        // Get the current basic block
        let current_block = self
            .builder
            .get_insert_block()
            .ok_or_else(|| Diagnostic::simple_boxed(Severity::Error, "no current basic block"))?;

        // For non-pointer types, they can't be null, so just do regular member access
        let ptr = match obj_val {
            BasicValueEnum::PointerValue(p) => p,
            _ => {
                // Non-pointer types can't be null, so create a MemberExpr and use regular access
                let member_expr = MemberExpr {
                    obj: optional.obj.clone(),
                    prop: optional.prop.clone(),
                    span: optional.span.clone(),
                };
                return self.lower_member_expr(&member_expr, function, param_map, locals);
            }
        };

        // Create blocks for null check and member access
        let access_block = self.context.append_basic_block(function, "opt_access");
        let merge_block = self.context.append_basic_block(function, "opt_merge");

        // Check if object is null
        let is_null = self
            .builder
            .build_is_null(ptr, "is_null")
            .map_err(|_| Diagnostic::simple_boxed(Severity::Error, "failed to build null check"))?;

        // Branch: if null, go to merge (return null), else access
        self.builder
            .build_conditional_branch(is_null, merge_block, access_block)
            .map_err(|_| {
                Diagnostic::simple_boxed(Severity::Error, "failed to build conditional branch")
            })?;

        // In access_block: perform member access
        self.builder.position_at_end(access_block);

        // Create a MemberExpr to reuse existing member access logic
        let member_expr = MemberExpr {
            obj: optional.obj.clone(),
            prop: optional.prop.clone(),
            span: optional.span.clone(),
        };

        let access_result = self.lower_member_expr(&member_expr, function, param_map, locals)?;

        // Ensure access result is a pointer for the phi node
        let access_ptr = match access_result {
            BasicValueEnum::PointerValue(p) => p,
            _ => {
                // For non-pointer results, we need to box them or return null
                // For now, return null as a conservative approach
                self.builder
                    .build_unconditional_branch(merge_block)
                    .map_err(|_| {
                        Diagnostic::simple_boxed(Severity::Error, "failed to build branch")
                    })?;
                self.builder.position_at_end(merge_block);
                return Ok(self.i8ptr_t.const_null().as_basic_value_enum());
            }
        };

        self.builder
            .build_unconditional_branch(merge_block)
            .map_err(|_| Diagnostic::simple_boxed(Severity::Error, "failed to build branch"))?;

        // In merge_block: create phi node
        self.builder.position_at_end(merge_block);
        let null_ptr = self.i8ptr_t.const_null();
        let phi = self
            .builder
            .build_phi(self.i8ptr_t, "opt_result")
            .map_err(|_| Diagnostic::simple_boxed(Severity::Error, "failed to create phi node"))?;

        // Add incoming values: null from current_block, access result from access_block
        phi.add_incoming(&[(&null_ptr, current_block), (&access_ptr, access_block)]);
        Ok(phi.as_basic_value().as_basic_value_enum())
    }
}
