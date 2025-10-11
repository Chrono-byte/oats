use anyhow::Result;

// common helper provides IR generation for tests
use super::common;
use common::create_codegen;

#[test]
fn class_simple_emits_ctor_and_method() -> Result<()> {
    let src = std::fs::read_to_string("../../examples/class_simple.oats")?;

    let context = inkwell::context::Context::create();
    let symbols = oats::types::SymbolTable::new();
    let codegen = create_codegen(&context, "class_lowering_test", symbols, &src);

    let ir = codegen.module.print_to_string().to_string();

    assert!(
        ir.contains("Foo_ctor"),
        "expected generated IR to contain `Foo_ctor`: {}",
        ir
    );
    assert!(
        ir.contains("Foo_bar"),
        "expected generated IR to contain `Foo_bar`: {}",
        ir
    );

    Ok(())
}
