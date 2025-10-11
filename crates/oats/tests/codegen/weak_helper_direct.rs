use anyhow::Result;
use super::common;
use common::create_codegen;

#[test]
fn test_heap_alloc_with_ptr_fields_weak_emits_rc_weak_inc() -> Result<()> {
    let context = inkwell::context::Context::create();
    let symbols = oats::types::SymbolTable::new();
    let codegen = create_codegen(&context, "weak_helper_direct_test", symbols, "");

    let ir = codegen.module.print_to_string().to_string();

    assert!(
        ir.contains("rc_weak_inc"),
        "IR should contain weak reference increment helper"
    );
    Ok(())
}
