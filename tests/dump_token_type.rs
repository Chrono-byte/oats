use anyhow::Result;
use oats::parser;
use std::any::type_name;

fn type_of<T>(_: &T) -> &str { type_name::<T>() }

#[test]
fn dump_token_type() -> Result<()> {
    let source = r#"export function main(): number { return 1; }"#;
    let parsed = parser::parse_oats_module(source, None)?;
    let toks = parsed.tokens();
    if let Some(t0) = toks.first() {
        eprintln!("token type: {}", type_of(&t0.token));
    }
    Ok(())
}
