// use super::common::gen_ir_for_source;
// use anyhow::Result;
// use oatsc::parser;

// #[test]
// fn test_closure_weak_capture_codegen() -> Result<()> {
//     let source = r#"
// class TestObj {
//     field: number;

//     constructor() {
//         this.field = 0;
//     }

//     downgrade(): Weak<TestObj> {
//         // dummy implementation
//         return null as Weak<TestObj>;
//     }
// }

// export function main(): number {
//     let obj: TestObj = new TestObj();
//     let w: Weak<TestObj> | null;
//     let f: () => Weak<TestObj> | null = () => w;
//     return 0;
// }
// "#;

//     let parsed_mod = parser::parse_oats_module(source, None)?;
//     let parsed = &parsed_mod.parsed;

//     let mut func_decl_opt: Option<(String, deno_ast::swc::ast::Function)> = None;
//     for item_ref in parsed.program_ref().body() {
//         if let deno_ast::ModuleItemRef::ModuleDecl(module_decl) = item_ref
//             && let deno_ast::swc::ast::ModuleDecl::ExportDecl(decl) = module_decl
//             && let deno_ast::swc::ast::Decl::Fn(f) = &decl.decl
//         {
//             func_decl_opt = Some((f.ident.sym.to_string(), (*f.function).clone()));
//             break;
//         }
//     }

//     let (_, _) =
//         func_decl_opt.ok_or_else(|| anyhow::anyhow!("No exported function found"))?;

//     let ir = gen_ir_for_source(source)?;

//     assert!(
//         ir.contains("rc_weak_inc") || ir.contains("rc_weak_upgrade") || ir.contains("rc_weak_dec"),
//         "IR should reference rc_weak helper functions"
//     );

//     Ok(())
// }
