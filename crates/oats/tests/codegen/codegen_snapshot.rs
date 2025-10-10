use anyhow::Result;
use super::common;
use common::gen_ir_for_source;

#[test]
fn snapshot_add_example_ir() -> Result<()> {
    let src = std::fs::read_to_string("../../examples/add.oats")?;
    let ir = gen_ir_for_source(&src)?;
    insta::assert_snapshot!(ir);
    Ok(())
}
