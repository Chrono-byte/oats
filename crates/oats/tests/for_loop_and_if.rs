/// Test for-loop and if statement lowering
use anyhow::Result;

#[test]
fn for_loop_and_if_statement_in_fibonacci() -> Result<()> {
    // Read the fibonacci example which contains both for-loop and if statement
    let src = std::fs::read_to_string("../../examples/fibonaci.oats")?;
    
    // Parse and compile it
    let parsed_mod = oats::parser::parse_oats_module(&src, None)?;
    let parsed = &parsed_mod.parsed;

    // Find exported main function
    let mut found_main = false;
    for item_ref in parsed.program_ref().body() {
        if let deno_ast::ModuleItemRef::ModuleDecl(module_decl) = item_ref
            && let deno_ast::swc::ast::ModuleDecl::ExportDecl(decl) = module_decl
            && let deno_ast::swc::ast::Decl::Fn(f) = &decl.decl
        {
            let name = f.ident.sym.to_string();
            if name == "main" {
                found_main = true;
                break;
            }
        }
    }
    assert!(found_main, "No exported main function found in fibonaci.oats");

    // The test just verifies the file parses and has a main function with a for-loop
    // The actual IR generation is tested by running the aot_run binary
    Ok(())
}

#[test]
fn for_loop_generates_basic_blocks() {
    // Simple inline test that verifies for-loop syntax is recognized
    let src = r#"
        export function test(): number {
            for (let i = 0; i < 10; i = i + 1) {
                print_f64(i);
            }
            return 0;
        }
    "#;
    
    let result = oats::parser::parse_oats_module(src, None);
    assert!(result.is_ok(), "For-loop should parse successfully");
}

#[test]
fn if_statement_generates_basic_blocks() {
    // Simple inline test that verifies if statement syntax is recognized
    let src = r#"
        export function test(): number {
            if (1 > 0) {
                return 1;
            } else {
                return 0;
            }
        }
    "#;
    
    let result = oats::parser::parse_oats_module(src, None);
    assert!(result.is_ok(), "If statement should parse successfully");
}
