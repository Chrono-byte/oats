use inkwell::values::FunctionValue;
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
    pub(crate) fn lower_block_stmt(
        &self,
        block: &deno_ast::swc::ast::BlockStmt,
        function: FunctionValue<'a>,
        param_map: &HashMap<String, u32>,
        locals_stack: &mut LocalsStackLocal<'a>,
    ) -> crate::diagnostics::DiagnosticResult<bool> {
        // new scope
        locals_stack.push(HashMap::new());
        let terminated = self.lower_stmts(&block.stmts, function, param_map, locals_stack)?;
        // pop scope
        locals_stack.pop();
        Ok(terminated)
    }
}
