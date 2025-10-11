use anyhow::Result;
use super::common;
use common::create_codegen;
use oats::types::OatsType;

#[test]
fn test_heap_alloc_with_ptr_fields_weak_emits_rc_weak_inc() -> Result<()> {
    let context = inkwell::context::Context::create();
    let mut symbols = oats::types::SymbolTable::new();

    // Add a weak field to the symbol table
    symbols.insert("weak_field".to_string(), OatsType::Weak(Box::new(OatsType::Number)));

    for (key, value) in symbols.all_symbols() {
        println!("Symbol: {} -> {:?}", key, value);
    }

    let source = r#"
        export class TestClass {
            weak_field: Weak<number>;

            constructor() {
                this.weak_field = new Weak(42);
            }
        }
    "#;

    let codegen = create_codegen(&context, "weak_helper_direct_test", symbols, source);

    // Generate IR
    let ir = codegen.module.print_to_string().to_string();

    // Assert that rc_weak_inc is present in the IR
    assert!(
        ir.contains("rc_weak_inc"),
        "IR should contain weak reference increment helper"
    );

    println!("Generated IR:\n{}", ir);

    Ok(())
}
