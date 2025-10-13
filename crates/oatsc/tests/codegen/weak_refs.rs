use anyhow::Result;
use oatsc::parser;
use oatsc::codegen::CodeGen;
use oatsc::types::{SymbolTable, check_function_strictness};
use inkwell::context::Context;
use inkwell::targets::TargetMachine;
use std::cell::Cell;

#[test]
fn test_weak_upgrade_downgrade_codegen() -> Result<()> {
    let source = r#"
export function main(): number {
    let obj: any = { } as any;
    let w = obj.downgrade();
    let u = w.upgrade();
    return 0;
}
"#;

    let context = inkwell::context::Context::create();
    let symbols = oatsc::types::SymbolTable::new();
    let codegen = create_codegen(&context, "weak_refs_test", symbols, source);

    let ir = codegen.module.print_to_string().to_string();

    assert!(
        ir.contains("rc_weak_upgrade") && ir.contains("rc_weak_downgrade"),
        "IR should contain weak reference helpers"
    );
    Ok(())
}
