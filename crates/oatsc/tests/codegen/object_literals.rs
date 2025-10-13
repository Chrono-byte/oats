use anyhow::Result;

#[test]
fn object_literal_basic() -> Result<()> {
    let src = r#"
        let point = { x: 1, y: 2 };
        export function main(): number {
            let p = point;
            return 0;
        }
    "#;

    let parsed = oatsc::parser::parse_oats_module(src, None)?;
    // ensure it parsed
    assert!(parsed.parsed.program_ref().body().count() > 0);
    Ok(())
}
