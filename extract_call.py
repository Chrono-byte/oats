#!/usr/bin/env python3
"""
Extract Call expression handler from mod.rs to calls.rs module.
Automates the modularization of the 1,939-line Call match arm.
"""

import subprocess
import sys
import re
from pathlib import Path

# File paths
MOD_RS = Path("/home/chrono/Dev/oats/crates/oatsc/src/codegen/expr/mod.rs")
CALLS_RS = Path("/home/chrono/Dev/oats/crates/oatsc/src/codegen/expr/calls.rs")
WORK_DIR = Path("/home/chrono/Dev/oats")

def run_command(cmd, description):
    """Run a shell command and report status."""
    print(f"\n[*] {description}")
    print(f"    Command: {' '.join(cmd)}")
    result = subprocess.run(cmd, cwd=WORK_DIR, capture_output=True, text=True)
    if result.returncode != 0:
        print(f"    ❌ Failed with exit code {result.returncode}")
        if result.stderr:
            print(f"    stderr: {result.stderr[:500]}")
        return False
    print(f"    ✓ Success")
    return True

def extract_call_handler():
    """Extract the Call handler code (without the match arm wrapper)."""
    print("\n[*] Reading Call handler from mod.rs (lines 111-2049)")
    with open(MOD_RS, 'r') as f:
        lines = f.readlines()
    
    # Extract lines 111-2049 (1-indexed, so 110-2048 in 0-indexed)
    # Line 111 is: "            ast::Expr::Call(call) => {"
    # Line 2049 is: "            }"
    call_lines = lines[110:2049]
    
    # First line should be the match arm opening
    first = call_lines[0].strip()
    print(f"    First line: {first[:60]}...")
    print(f"    Total lines: {len(call_lines)}")
    
    # Skip the first line (the match arm opening) and last line (closing brace)
    # Then dedent all content by 16 spaces (match arm body indentation)
    handler_lines = call_lines[1:-1]
    
    handler_code = ''.join(handler_lines)
    # Dedent by 16 spaces (match arm body indentation)
    handler_code = re.sub(r'^                ', '', handler_code, flags=re.MULTILINE)
    
    print(f"    ✓ Extracted {len(handler_lines)} lines of handler code")
    return handler_code.rstrip() + '\n'

def create_calls_module(handler_code):
    """Create the calls.rs module file."""
    print("\n[*] Creating calls.rs module")
    
    # Build the module with proper structure
    module_code = f'''use crate::diagnostics::Diagnostic;
use crate::codegen::expr::LocalsStackLocal;
use deno_ast::swc::ast;
use inkwell::values::{{BasicValueEnum, FunctionValue}};
use std::collections::HashMap;

/// Lower Call expression (function calls, method calls, super calls, closures).
pub(super) fn lower_call_expr(
    codegen: &crate::codegen::CodeGen,
    call: &ast::CallExpr,
    function: FunctionValue,
    param_map: &HashMap<String, u32>,
    locals: &mut LocalsStackLocal,
) -> Result<BasicValueEnum, Diagnostic> {{
{handler_code}
}}
'''
    
    with open(CALLS_RS, 'w') as f:
        f.write(module_code)
    
    line_count = module_code.count('\n')
    print(f"    ✓ Created calls.rs ({len(module_code)} bytes, {line_count} lines)")
    return True

def wire_module():
    """Add module declaration and function call to mod.rs."""
    print("\n[*] Wiring calls module into mod.rs")
    
    with open(MOD_RS, 'r') as f:
        content = f.read()
    
    # Step 1: Add pub mod calls; declaration (if not present)
    if 'pub mod calls;' not in content:
        # Find a good place to add it (after other pub mod declarations)
        pub_mod_pattern = r'(pub mod paren;)'
        if re.search(pub_mod_pattern, content):
            content = re.sub(
                pub_mod_pattern,
                r'\1\n    pub mod calls;',
                content,
                count=1
            )
            print("    ✓ Added pub mod calls; declaration")
        else:
            print("    ⚠ Could not find insertion point for pub mod calls;")
    
    # Step 2: Replace the Call match arm with function call
    # Match: "            ast::Expr::Call(call) => { ... }" (entire block from line 111-2049)
    call_pattern = r'(\s+)ast::Expr::Call\(call\) => \{(?:(?!^            \}).)*?^            \}'
    replacement = r'\1ast::Expr::Call(call) => self.lower_call_expr(call, function, param_map, locals),'
    
    new_content = re.sub(call_pattern, replacement, content, flags=re.DOTALL | re.MULTILINE)
    
    if new_content != content:
        with open(MOD_RS, 'w') as f:
            f.write(new_content)
        print("    ✓ Replaced Call match arm with function call")
        return True
    else:
        print("    ❌ Could not find Call match arm to replace")
        return False

def main():
    """Main extraction workflow."""
    print("=" * 70)
    print("Call Expression Handler Extraction")
    print("=" * 70)
    
    # Step 1: Extract the Call handler code
    handler = extract_call_handler()
    
    # Step 2: Create the calls.rs module
    if not create_calls_module(handler):
        print("❌ Failed to create calls.rs")
        sys.exit(1)
    
    # Step 3: Wire the module into mod.rs
    if not wire_module():
        print("❌ Failed to wire module")
        sys.exit(1)
    
    # Step 4: Build the project
    if not run_command(['cargo', 'build', '--workspace'], "Building project"):
        print("❌ Build failed - attempting to show errors...")
        subprocess.run(['cargo', 'build', '--workspace'], cwd=WORK_DIR)
        sys.exit(1)
    
    # Step 5: Run tests
    if not run_command(['cargo', 'test', '--test', 'codegen'], "Running codegen tests"):
        print("❌ Tests failed")
        sys.exit(1)
    
    # Step 6: Commit
    print("\n[*] Creating git commit")
    subprocess.run(['git', 'add', '-A'], cwd=WORK_DIR)
    commit_msg = 'refactor(codegen/expr): wire calls module for ast::Expr::Call handling'
    result = subprocess.run(
        ['git', '-c', 'commit.gpgsign=false', 'commit', '-m', commit_msg],
        cwd=WORK_DIR,
        capture_output=True,
        text=True
    )
    
    if result.returncode == 0:
        print(f"    ✓ Committed: {commit_msg}")
    else:
        print(f"    ⚠ Commit message: {result.stdout.strip()}")
    
    # Summary
    print("\n" + "=" * 70)
    print("Extraction Complete!")
    print("=" * 70)
    
    # Get file stats
    with open(MOD_RS, 'r') as f:
        mod_lines = len(f.readlines())
    with open(CALLS_RS, 'r') as f:
        calls_lines = len(f.readlines())
    
    print(f"mod.rs lines: {mod_lines}")
    print(f"calls.rs lines: {calls_lines}")
    print("\n✓ All steps completed successfully!")

if __name__ == '__main__':
    main()
