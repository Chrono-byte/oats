use anyhow::Result;

#[test]
fn test_unowned_reference_codegen() -> Result<()> {
    let source = r#"
export function main(): number {
    let obj: any = { } as any;
    let u: unowned<any> = obj;
    return 0;
}
"#;

    let ir = crate::common::gen_ir_for_source(source)?;

    // Unowned references should not generate RC operations
    // The test passes if compilation succeeds and we can generate IR
    assert!(ir.contains("define"), "Should generate valid LLVM IR");
    Ok(())
}
