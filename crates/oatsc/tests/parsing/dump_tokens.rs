use anyhow::Result;
use oatsc::parser;

#[test]
fn dump_tokens() -> Result<()> {
    let source = r#"export function main(): number { return 1; }"#;
    let (parsed_mod_opt, _) = parser::parse_oats_module(source, None)?;
    let parsed_mod = parsed_mod_opt.ok_or_else(|| anyhow::anyhow!("Failed to parse source"))?;
    let parsed = &parsed_mod.parsed;

    // Tokens are now available in ParsedModule
    eprintln!("Module has {} statements", parsed.body.len());
    eprintln!("Found {} tokens", parsed_mod.tokens.len());

    // Filter out whitespace and comments for cleaner output
    let meaningful_tokens: Vec<_> = parsed_mod
        .tokens
        .iter()
        .filter(|t| {
            !matches!(
                t.kind,
                oats_parser::tokenizer::TokenKind::Whitespace
                    | oats_parser::tokenizer::TokenKind::LineComment(_)
                    | oats_parser::tokenizer::TokenKind::BlockComment(_)
            )
        })
        .collect();

    eprintln!(
        "Meaningful tokens (excluding whitespace/comments): {}",
        meaningful_tokens.len()
    );
    for token in meaningful_tokens.iter().take(20) {
        eprintln!("  {:?} at {:?}: {:?}", token.kind, token.span, token.text);
    }

    Ok(())
}
