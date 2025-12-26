use oatsc::builder;
use oatsc::CompileOptions;
use std::collections::HashMap;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = std::env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <file.ts>", args[0]);
        std::process::exit(1);
    }
    
    let src_file = &args[1];
    let mut options = CompileOptions::new(src_file.clone());
    options.emit_ir = true;
    options.out_dir = Some("/tmp".to_string());
    
    let result = builder::compile_file(src_file, Some(options))?;
    
    // The IR file should be written to /tmp/{filename}.ll
    let filename = std::path::Path::new(src_file)
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("out");
    let ir_path = format!("/tmp/{}.ll", filename);
    
    if let Ok(ir) = std::fs::read_to_string(&ir_path) {
        println!("{}", ir);
    } else {
        eprintln!("Could not read IR file: {}", ir_path);
    }
    
    Ok(())
}


