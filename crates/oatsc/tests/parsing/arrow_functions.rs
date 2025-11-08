// Test arrow function parsing and lowering
use anyhow::Result;

#[test]
fn parse_simple_arrow_function() -> Result<()> {
    let src = r#"
        let add = (x: number, y: number): number => x + y;

        export function main(): number {
            let result = add(2, 3);
            return result;
        }
    "#;

    let (parsed_mod_opt, _) = oatsc::parser::parse_oats_module(src, None)?;
    let parsed_mod = parsed_mod_opt.ok_or_else(|| anyhow::anyhow!("Failed to parse source"))?;
    let parsed = &parsed_mod.parsed;

    // Verify it parses without errors
    use oats_ast::*;
    let body_count = parsed.body.len();
    assert!(body_count > 0);

    // Find the const declaration with arrow function
    let mut found_arrow = false;
    for stmt in &parsed.body {
        if let Stmt::VarDecl(var_decl) = stmt {
            for decl in &var_decl.decls {
                if let Some(init) = &decl.init {
                    // Check if init is an arrow function
                    if let Expr::Arrow(_arrow) = init {
                        found_arrow = true;
                        // println!("Found arrow function!");
                        // println!("  Params: {:?}", arrow.params.len());
                        // println!("  Body: {:?}", arrow.body);
                        break;
                    }
                }
            }
        }
    }

    assert!(found_arrow, "Should have found arrow function in AST");
    Ok(())
}

#[test]
fn parse_arrow_with_block_body() -> Result<()> {
    let src = r#"
        let compute = (x: number): number => {
            let doubled = x * 2;
            return doubled + 1;
        };
    "#;

    let (parsed_mod_opt, _) = oatsc::parser::parse_oats_module(src, None)?;
    let parsed_mod = parsed_mod_opt.ok_or_else(|| anyhow::anyhow!("Failed to parse source"))?;
    let parsed = &parsed_mod.parsed;

    // Should parse successfully
    let body_count = parsed.body.len();
    assert!(body_count > 0);
    Ok(())
}

#[test]
fn parse_arrow_as_callback() -> Result<()> {
    let src = r#"
        function forEach(arr: number[], callback: (x: number) => void): void {
            for (let i = 0; i < arr.length; i = i + 1) {
                callback(arr[i]);
            }
        }

        export function main(): number {
            let nums = [1, 2, 3];
            forEach(nums, (x) => print_f64(x));
            return 0;
        }
    "#;

    let (parsed_mod_opt, _) = oatsc::parser::parse_oats_module(src, None)?;
    let parsed_mod = parsed_mod_opt.ok_or_else(|| anyhow::anyhow!("Failed to parse source"))?;
    let parsed = &parsed_mod.parsed;

    // Should parse successfully
    let body_count = parsed.body.len();
    assert!(body_count > 0);
    Ok(())
}
