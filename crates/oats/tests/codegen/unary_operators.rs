use super::common::gen_ir_for_source;
use anyhow::Result;

#[test]
fn test_binary_operators() -> Result<()> {
    let source = r#"
        export function main(): number {
            let x = 5;
            let y = -x;
            let z = !true;
            return y + (z ? 1 : 0);
        }
    "#;

    let ir = gen_ir_for_source(source)?;

    // Check for negation operator
    assert!(
        ir.contains("fneg"),
        "Expected IR to contain floating-point negation (fneg) operator"
    );

    // Check for logical NOT operator
    assert!(
        ir.contains("xor i1"),
        "Expected IR to contain logical NOT operator implemented as XOR"
    );

    Ok(())
}
