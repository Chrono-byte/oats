use anyhow::Result;
use oatsc::parser;

#[test]
fn semicolon_after_block_comment_is_ok() -> Result<()> {
    let source = r#"export function main(): number { return 1 /* comment */; }"#;
    let (parsed_mod_opt, _) = parser::parse_oats_module(source, None)?;
    let parsed_mod = parsed_mod_opt.ok_or_else(|| anyhow::anyhow!("Failed to parse source"))?;
    let parsed = &parsed_mod.parsed;
    assert!(parsed.program_ref().body().next().is_some());
    Ok(())
}

#[test]
fn semicolon_on_next_line_is_ok() -> Result<()> {
    let source = "export function main(): number { return 1\n; }";
    let (parsed_mod_opt, _) = parser::parse_oats_module(source, None)?;
    let parsed_mod = parsed_mod_opt.ok_or_else(|| anyhow::anyhow!("Failed to parse source"))?;
    let parsed = &parsed_mod.parsed;
    assert!(parsed.program_ref().body().next().is_some());
    Ok(())
}

#[test]
fn comment_with_semicolon_inside_does_not_count() {
    let source = r#"export function main(): number { return 1 // ;\n }"#;
    let parsed_mod = parser::parse_oats_module_with_options(source, None, true); // enforce_semicolons = true
    let parsed = parsed_mod
        .as_ref()
        .ok()
        .and_then(|(m, _)| m.as_ref())
        .map(|pm| &pm.parsed);
    assert!(parsed.is_none());
}

#[test]
fn block_comment_with_semicolon_inside_counts_only_if_followed_by_semicolon() -> Result<()> {
    // if semicolon appears inside block comment but not after, it's still missing
    let source = r#"export function main(): number { return 1 /* ; */ }"#;
    let parsed_mod = parser::parse_oats_module_with_options(source, None, true); // enforce_semicolons = true
    let parsed = parsed_mod
        .as_ref()
        .ok()
        .and_then(|(m, _)| m.as_ref())
        .map(|pm| &pm.parsed);
    assert!(parsed.is_none());
    Ok(())
}
