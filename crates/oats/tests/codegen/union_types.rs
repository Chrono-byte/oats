use anyhow::Result;

#[test]
fn union_local_and_param_boxing() -> Result<()> {
    let src = r#"
        export function main(): number {
            let u: number | string = 1;
            let s: number | string = "hi";
            return 0;
        }
    "#;

    let ir = crate::tests::common::gen_ir_for_source(src)?;
    // Should include boxing of the numeric literal into union_box_f64
    assert!(ir.contains("union_box_f64"), "IR should box numbers into union_box_f64");
    // Strings may be left as pointers and boxed when assigned to union slots
    Ok(())
}

#[test]
fn union_param_boxing() -> Result<()> {
    let src = r#"
        export function main(): number {
            function f(x: number | string): number { return 0; }
            f(3);
            return 0;
        }
    "#;

    let ir = crate::tests::common::gen_ir_for_source(src)?;
    // Param boxing should emit union_box_f64 when numbers are passed
    assert!(ir.contains("union_box_f64") || ir.contains("union_box_ptr"));
    Ok(())
}

#[test]
fn typeof_guard_uses_discriminant() -> Result<()> {
    let src = r#"
        export function main(): number {
            let u: number | string = 1;
            if (typeof u === "number") {
                return 1;
            }
            return 0;
        }
    "#;

    let ir = crate::tests::common::gen_ir_for_source(src)?;
    // The lowered IR should test the union discriminant or unbox number - check for union_unbox_f64 or a discriminant helper
    assert!(ir.contains("union_unbox_f64") || ir.contains("union_get_discriminant"));
    Ok(())
}
