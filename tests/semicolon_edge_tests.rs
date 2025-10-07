use anyhow::Result;
use oats::parser;

#[test]
fn semicolon_after_block_comment_is_ok() -> Result<()> {
    let source = r#"export function main(): number { return 1 /* comment */; }"#;
    let parsed = parser::parse_oats_module(source, None)?;
    assert!(parsed.program_ref().body().next().is_some());
    Ok(())
}

#[test]
fn semicolon_on_next_line_is_ok() -> Result<()> {
    let source = "export function main(): number { return 1\n; }";
    let parsed = parser::parse_oats_module(source, None)?;
    assert!(parsed.program_ref().body().next().is_some());
    Ok(())
}

#[test]
fn comment_with_semicolon_inside_does_not_count() {
    let source = r#"export function main(): number { return 1 // ;\n }"#;
    let parsed = parser::parse_oats_module(source, None);
    assert!(parsed.is_err());
}

#[test]
fn block_comment_with_semicolon_inside_counts_only_if_followed_by_semicolon() -> Result<()> {
    // if semicolon appears inside block comment but not after, it's still missing
    let source = r#"export function main(): number { return 1 /* ; */ }"#;
    let parsed = parser::parse_oats_module(source, None);
    assert!(parsed.is_err());
    Ok(())
}
