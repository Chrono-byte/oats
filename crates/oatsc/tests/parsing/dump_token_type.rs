use anyhow::Result;
use oatsc::parser;

#[test]
fn dump_token_type() -> Result<()> {
    let source = r#"export function main(): number { return 1; }"#;
    let (parsed_mod_opt, _) = parser::parse_oats_module(source, None)?;
    let parsed_mod = parsed_mod_opt.ok_or_else(|| anyhow::anyhow!("Failed to parse source"))?;
    let _parsed = &parsed_mod.parsed;
    // Note: oats_ast doesn't expose tokens - this test is disabled
    // Tokens are not part of the AST structure in oats_ast
    eprintln!("Note: Token access not available in oats_ast");
    Ok(())
}
