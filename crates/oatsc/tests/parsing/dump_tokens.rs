use anyhow::Result;
use oatsc::parser;

#[test]
fn dump_tokens() -> Result<()> {
    let source = r#"export function main(): number { return 1; }"#;
    let parsed_mod = parser::parse_oats_module(source, None)?;
    let parsed = &parsed_mod.parsed;
    let toks = parsed.tokens();
    eprintln!("tokens count: {}", toks.len());
    for t in toks.iter().take(200) {
        eprintln!(
            "tok: span=({}-{}), dbg={:?}",
            t.span.lo.0, t.span.hi.0, t.token
        );
    }
    Ok(())
}
