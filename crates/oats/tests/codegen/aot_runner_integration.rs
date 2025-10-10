use super::common;
use anyhow::Result;
use common::gen_ir_for_source;

#[test]
fn aot_run_generates_oats_main_symbol() -> Result<()> {
    let src = std::fs::read_to_string("../../examples/add.oats")?;
    let ir = gen_ir_for_source(&src)?;
    assert!(
        ir.contains("oats_main"),
        "expected generated IR to contain `oats_main`"
    );
    Ok(())
}
