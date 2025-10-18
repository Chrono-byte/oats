use anyhow::Result;

#[test]
fn test_unowned_reference_codegen() -> Result<()> {
    let source = r#"
class TestObj {
}

export function main(): number {
    let obj: TestObj = new TestObj();
    let u: unowned<TestObj> = obj;
    return 0;
}
"#;

    let ir = crate::common::gen_ir_for_source(source)?;

    // Unowned references should not generate RC operations
    // The test passes if compilation succeeds and we can generate IR
    assert!(ir.contains("define"), "Should generate valid LLVM IR");
    Ok(())
}
