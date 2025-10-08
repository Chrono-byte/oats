use anyhow::Result;
// Use shared test helper
#[path = "../common/mod.rs"]
mod common;
use common::gen_ir_for_source;

#[test]
fn constructor_with_params_allocates_and_initializes() -> Result<()> {
    let src = std::fs::read_to_string("../../examples/class_constructor.oats")?;

    let ir = gen_ir_for_source(&src)?;

    // Verify constructor was generated with correct signature
    assert!(
        ir.contains("define ptr @Counter_ctor(double"),
        "Constructor should accept number parameter"
    );
    assert!(
        ir.contains("call ptr @malloc"),
        "Constructor should allocate memory"
    );

    // Verify methods were generated
    assert!(
        ir.contains("define double @Counter_increment(ptr"),
        "increment method should be generated"
    );
    assert!(
        ir.contains("define double @Counter_getValue(ptr"),
        "getValue method should be generated"
    );

    // Verify main calls constructor
    assert!(
        ir.contains("call ptr @Counter_ctor(double"),
        "main should call constructor with parameter"
    );

    Ok(())
}
