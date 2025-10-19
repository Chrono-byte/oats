use anyhow::Result;
use oatsc::parser;
use std::any::type_name;

fn type_of<T>(_: &T) -> &str {
    type_name::<T>()
}

#[test]
fn dump_token_type() -> Result<()> {
    let source = r#"export function main(): number { return 1; }"#;
    let (parsed_mod_opt, _) = parser::parse_oats_module(source, None)?;
    let parsed_mod = parsed_mod_opt.ok_or_else(|| anyhow::anyhow!("Failed to parse source"))?;
    let parsed = &parsed_mod.parsed;
    let toks = parsed.tokens();
    if let Some(t0) = toks.first() {
        eprintln!("token type: {}", type_of(&t0.token));
    }
    Ok(())
}
