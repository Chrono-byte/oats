use anyhow::Result;
use oatsc::parser;

#[test]
fn dump_token_type() -> Result<()> {
    let source = r#"export function main(): number { return 1; }"#;
    let (parsed_mod_opt, _) = parser::parse_oats_module(source, None)?;
    let parsed_mod = parsed_mod_opt.ok_or_else(|| anyhow::anyhow!("Failed to parse source"))?;
    let _parsed = &parsed_mod.parsed;

    // Tokens are now available in ParsedModule
    eprintln!("Found {} tokens", parsed_mod.tokens.len());

    // Filter out whitespace and comments, then show token types
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

    eprintln!("Token types found:");
    for token in meaningful_tokens.iter().take(20) {
        eprintln!("  {:?}", token.kind);
    }

    Ok(())
}
