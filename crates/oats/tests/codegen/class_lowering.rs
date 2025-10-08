use anyhow::Result;

// common helper provides IR generation for tests
#[path = "../common/mod.rs"]
mod common;
use common::gen_ir_for_source;

#[test]
fn class_simple_emits_ctor_and_method() -> Result<()> {
    let src = std::fs::read_to_string("../../examples/class_simple.oats")?;

    let ir = gen_ir_for_source(&src)?;

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
