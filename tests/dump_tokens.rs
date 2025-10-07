use anyhow::Result;
use oats::parser;

#[test]
fn dump_tokens() -> Result<()> {
    let source = r#"export function main(): number { return 1; }"#;
    let parsed = parser::parse_oats_module(source, None)?;
    let toks = parsed.tokens();
    eprintln!("tokens count: {}", toks.len());
    for t in toks.iter().take(200) {
        eprintln!("tok: span=({}-{}), dbg={:?}", t.span.lo.0, t.span.hi.0, t.token);
    }
    Ok(())
}
