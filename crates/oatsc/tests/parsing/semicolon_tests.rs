use anyhow::Result;

use oatsc::parser;

#[test]
fn missing_semicolon_is_rejected() -> Result<()> {
    let source = r#"export function main(): number { return 1 }"#;
    let parsed_mod = parser::parse_oats_module(source, None);
    let parsed = parsed_mod.as_ref().map(|m| &m.parsed);
    assert!(parsed.is_err(), "expected missing-semicolon to be an error");
    Ok(())
}

#[test]
fn semicolon_present_is_ok() -> Result<()> {
    let source = r#"export function main(): number { return 1; }"#;
    let parsed_mod = parser::parse_oats_module(source, None)?;
    let parsed = &parsed_mod.parsed;
    // sanity: ensure exported function present
    let mut found = false;
    for item in parsed.program_ref().body() {
        if let deno_ast::ModuleItemRef::ModuleDecl(md) = item
            && let deno_ast::swc::ast::ModuleDecl::ExportDecl(decl) = md
            && let deno_ast::swc::ast::Decl::Fn(_f) = &decl.decl
        {
            found = true;
        }
    }
    assert!(found, "expected exported function to be present");
    Ok(())
}
