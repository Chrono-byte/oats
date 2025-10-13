use anyhow::Result;

use super::common;
use common::gen_ir_for_source;

#[test]
fn class_fields_lowering_emits_field_access() -> Result<()> {
    let src = std::fs::read_to_string("../../examples/class_field.oats")?;

    let ir = gen_ir_for_source(&src)?;

    // Expect constructor and method present
    assert!(
        ir.contains("Point_ctor"),
        "expected generated IR to contain `Point_ctor`: {}",
        ir
    );
    assert!(
        ir.contains("Point_sum"),
        "expected generated IR to contain `Point_sum`: {}",
        ir
    );

    // Ensure the method call used in `main` was actually lowered/used.
    // If the lowering rejected the call expression (returned None) it may
    // result in the call not being present in the emitted IR. Detect that
    // by asserting we actually call the generated `Point_sum` from `oats_main`.
    assert!(
        ir.contains("call double @Point_sum"),
        "expected generated IR to call `Point_sum` from `oats_main`, got IR: {}",
        ir
    );

    Ok(())
}
