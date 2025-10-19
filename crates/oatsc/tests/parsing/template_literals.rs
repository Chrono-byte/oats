// Test template literal parsing and lowering
use anyhow::Result;

#[test]
fn parse_simple_template_literal() -> Result<()> {
    let src = r#"
        export function main(): number {
            let name = "World";
            let greeting = `Hello ${name}!`;
            print_str(greeting);
            return 0;
        }
    "#;

    let (parsed_mod_opt, _) = oatsc::parser::parse_oats_module(src, None)?;
    let parsed_mod = parsed_mod_opt.ok_or_else(|| anyhow::anyhow!("Failed to parse source"))?;
    let parsed = &parsed_mod.parsed;

    // Verify it parses without errors
    let body_count = parsed.program_ref().body().count();
    assert!(body_count > 0);

    // println!("Template literal parsed successfully");
    Ok(())
}

#[test]
fn parse_template_with_multiple_expressions() -> Result<()> {
    let src = r#"
        export function main(): number {
            let x = 5;
            let y = 10;
            let msg = `${x} + ${y} = ${x + y}`;
            print_str(msg);
            return 0;
        }
    "#;

    let (parsed_mod_opt, _) = oatsc::parser::parse_oats_module(src, None)?;
    let parsed_mod = parsed_mod_opt.ok_or_else(|| anyhow::anyhow!("Failed to parse source"))?;
    let parsed = &parsed_mod.parsed;

    let body_count = parsed.program_ref().body().count();
    assert!(body_count > 0);
    Ok(())
}

#[test]
fn parse_template_with_number() -> Result<()> {
    let src = r#"
        export function main(): number {
            let count = 42;
            let msg = `Count: ${count}`;
            print_str(msg);
            return 0;
        }
    "#;

    let (parsed_mod_opt, _) = oatsc::parser::parse_oats_module(src, None)?;
    let parsed_mod = parsed_mod_opt.ok_or_else(|| anyhow::anyhow!("Failed to parse source"))?;
    let parsed = &parsed_mod.parsed;

    let body_count = parsed.program_ref().body().count();
    assert!(body_count > 0);
    Ok(())
}

#[test]
fn parse_multiline_template() -> Result<()> {
    let src = r#"
        export function main(): number {
            let msg = `Line 1
Line 2
Line 3`;
            print_str(msg);
            return 0;
        }
    "#;

    let (parsed_mod_opt, _) = oatsc::parser::parse_oats_module(src, None)?;
    let parsed_mod = parsed_mod_opt.ok_or_else(|| anyhow::anyhow!("Failed to parse source"))?;
    let parsed = &parsed_mod.parsed;

    let body_count = parsed.program_ref().body().count();
    assert!(body_count > 0);
    Ok(())
}
