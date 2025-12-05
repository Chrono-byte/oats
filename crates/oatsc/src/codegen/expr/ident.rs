use crate::diagnostics::{Diagnostic, Severity};
use inkwell::values::BasicValueEnum;
use inkwell::values::FunctionValue;
use std::collections::HashMap;

use crate::types::OatsType;
use inkwell::types::BasicTypeEnum;
use inkwell::values::{BasicValue, PointerValue};

// LocalEntry now includes an Option<String> for an optional nominal type name
// LocalEntry now includes an Option<OatsType> for union tracking
type LocalEntry<'a> = (
    PointerValue<'a>,
    BasicTypeEnum<'a>,
    bool,
    bool,
    bool,
    Option<String>,
    Option<crate::types::OatsType>,
);
type LocalsStackLocal<'a> = Vec<HashMap<String, LocalEntry<'a>>>;

impl<'a> crate::codegen::CodeGen<'a> {
    #[allow(clippy::result_large_err)]
    pub(super) fn lower_ident_expr(
        &self,
        id: &oxc_ast::ast::IdentifierReference<'_>,
        function: FunctionValue<'a>,
        param_map: &HashMap<String, u32>,
        locals: &mut LocalsStackLocal<'a>,
    ) -> crate::diagnostics::DiagnosticResult<BasicValueEnum<'a>> {
        let name = id.name.to_string();

        // First, check if the identifier is a function parameter.
        if let Some(idx) = param_map.get(&name)
            && let Some(pv) = function.get_nth_param(*idx)
        {
            // Record that this expression originated from a parameter
            // so later lowering (e.g., array access) can consult
            // `fn_param_types` for more accurate element type info.
            self.last_expr_origin_local
                .borrow_mut()
                .replace(name.clone());
            return Ok(pv);
        }

        // Check if the identifier refers to an enum type (enums themselves are not values)
        if let Some(OatsType::Enum(_, _)) = self.symbol_table.borrow().get_type(&name) {
            return Err(Diagnostic::simple_boxed(
                Severity::Error,
                format!(
                    "'{}' is an enum type and cannot be used as a value. Use 'enum.member' syntax instead.",
                    name
                ),
            ));
        }

        // const_eval removed - const items are no longer handled specially

        // If not a parameter, then it must be a local variable (`let` or `const`).
        if let Some((ptr, ty, initialized, _is_const, _extra, _nominal, _oats_type)) =
            self.find_local(locals, &name)
        {
            // If not initialized -> TDZ: generate a trap (unreachable).
            //
            // Emitting `unreachable` here is a simple way to guard
            // against invalid reads in generated code. In future we
            // could lower a runtime diagnostic or insert a proper
            // throw.
            if !initialized {
                // Emit a call to unreachable to trap at runtime
                let _ = self.builder.build_unreachable();
                return Err(Diagnostic::simple_boxed(
                    Severity::Error,
                    "expression lowering failed",
                ))?;
            }
            let loaded = match self.builder.build_load(ty, ptr, &name) {
                Ok(v) => v,
                Err(_) => {
                    return Err(Diagnostic::simple_boxed(
                        Severity::Error,
                        "operation failed",
                    ));
                }
            };
            // Record that this expression originated from local `name`.
            self.last_expr_origin_local
                .borrow_mut()
                .replace(name.clone());
            return Ok(loaded);
        }

        Err(Diagnostic::simple_boxed(
            Severity::Error,
            format!("undefined identifier '{}'", name),
        ))
    }
}
