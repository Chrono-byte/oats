use anyhow::Result;
use deno_ast::{parse_module, MediaType, ParsedSource, SourceTextInfo, ParseParams};
use std::sync::Arc;
use url::Url;

pub fn parse_oats_module(source_code: &str) -> Result<ParsedSource> {
    let sti = SourceTextInfo::from_string(source_code.to_string());
    let params = ParseParams {
        specifier: Url::parse("file:///file.ts")?,
        text: Arc::from(sti.text().clone()),
        media_type: MediaType::TypeScript,
        capture_tokens: false,
        scope_analysis: false,
        maybe_syntax: None,
    };

    let parsed = parse_module(params)?;
    Ok(parsed)
}
