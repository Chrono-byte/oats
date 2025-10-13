use anyhow::Result;

// common helper provides IR generation for tests
use super::common;
use common::gen_ir_for_source;

#[test]
fn class_field_emits_ctor_and_method() -> Result<()> {
    let src = std::fs::read_to_string("../../examples/class_field.oats")?;

    let ir = gen_ir_for_source(&src)?;

    assert!(
        ir.contains("Point_ctor"),
        "expected generated IR to contain `Foo_ctor`: {}",
        ir
    );
    assert!(
        ir.contains("Point_sum"),
        "expected generated IR to contain `Foo_bar`: {}",
        ir
    );

    Ok(())
}
