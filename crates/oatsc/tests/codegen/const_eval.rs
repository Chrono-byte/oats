use anyhow::Result;
use super::common::gen_ir_for_source;

#[test]
fn numeric_const_folding_inlines_constants() -> Result<()> {
    let src = r#"
export function main(): number {
    const A = 1 + 2 * 3;
    return A + 0.0;
}
"#;
    let ir = gen_ir_for_source(src)?;

    // We expect the numeric computation to be folded; generated IR should
    // contain the numeric constant 7.0 (or an operation that uses a constant).
    assert!(ir.contains("7") || ir.contains("7.0"), "expected folded numeric constant in IR\nIR:\n{}", ir);

    Ok(())
}

#[test]
fn const_ref_chaining_folds_values() -> Result<()> {
    let src = r#"
export function main(): number {
    const B = 10;
    const C = B + 5;
    return C;
}
"#;
    let ir = gen_ir_for_source(src)?;

    // Expect 15 to appear as a folded numeric constant in IR
    assert!(ir.contains("15") || ir.contains("15.0"), "expected folded const reference in IR\nIR:\n{}", ir);

    Ok(())
}
