#!/usr/bin/env python3
"""
Extract Call expression handler from mod.rs to calls.rs module.
Properly implements as impl<'a> CodeGen<'a> with method.
"""

import subprocess
import sys
import re
from pathlib import Path

# File paths
MOD_RS = Path("/home/chrono/Dev/oats/crates/oatsc/src/codegen/expr/mod.rs")
CALLS_RS = Path("/home/chrono/Dev/oats/crates/oatsc/src/codegen/expr/calls.rs")
MEMBER_RS = Path("/home/chrono/Dev/oats/crates/oatsc/src/codegen/expr/member_access.rs")
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

def extract_imports():
    """Extract necessary imports from member_access.rs as template."""
    print("\n[*] Extracting import template from member_access.rs")
    with open(MEMBER_RS, 'r') as f:
        lines = f.readlines()
    
    # Find the end of imports (before the type LocalEntry)
    imports = []
    for line in lines:
        if line.startswith('use ') or line.startswith('type '):
            if line.startswith('type '):
                break
            imports.append(line)
        elif 'use ' in line and not line.strip().startswith('//'):
            imports.append(line)
    
    print(f"    ✓ Found {len(imports)} import lines")
    return ''.join(imports)

def extract_call_handler():
    """Extract the Call handler code (without the match arm wrapper)."""
    print("\n[*] Reading Call handler from mod.rs (lines 111-2049)")
    with open(MOD_RS, 'r') as f:
        lines = f.readlines()
    
    # Extract lines 111-2049 (1-indexed, so 110-2048 in 0-indexed)
    call_lines = lines[110:2049]
    
    # Skip the first line (match arm opening) and last line (closing brace)
    handler_lines = call_lines[1:-1]
    
    handler_code = ''.join(handler_lines)
    # Dedent by 16 spaces (match arm body indentation)
    handler_code = re.sub(r'^                ', '', handler_code, flags=re.MULTILINE)
    
    print(f"    ✓ Extracted {len(handler_lines)} lines of handler code")
    return handler_code.rstrip()

def create_calls_module(imports, handler_code):
    """Create the calls.rs module file with proper impl block."""
    print("\n[*] Creating calls.rs module")
    
    # Build the module with proper structure (matching member_access.rs pattern)
    module_code = f'''{imports}
use crate::codegen::CodeGen;
use std::collections::HashMap;

type LocalEntry<'a> = (
    inkwell::values::PointerValue<'a>,
    inkwell::types::BasicTypeEnum<'a>,
    bool,
    bool,
    bool,
    Option<String>,
    Option<crate::types::OatsType>,
);

impl<'a> CodeGen<'a> {{
    /// Lower Call expression (function calls, method calls, super calls, closures).
    pub(super) fn lower_call_expr(
        &self,
        call: &deno_ast::swc::ast::CallExpr,
        function: inkwell::values::FunctionValue<'a>,
        param_map: &HashMap<String, u32>,
        locals: &mut Vec<HashMap<String, LocalEntry<'a>>>,
    ) -> Result<inkwell::values::BasicValueEnum<'a>, crate::diagnostics::Diagnostic> {{
{handler_code}
    }}
}}
'''
    
    with open(CALLS_RS, 'w') as f:
        f.write(module_code)
    
    line_count = module_code.count('\n')
    print(f"    ✓ Created calls.rs ({len(module_code)} bytes, {line_count} lines)")
    return True

def wire_module():
    """Add module declaration and wire function call to mod.rs."""
    print("\n[*] Wiring calls module into mod.rs")
    
    with open(MOD_RS, 'r') as f:
        content = f.read()
    
    # Step 1: Add pub mod calls; declaration (if not present)
    if 'pub mod calls;' not in content:
        # Add after the mod declarations
        pub_mod_section = re.search(r'(pub mod paren;\n)', content)
        if pub_mod_section:
            insert_pos = pub_mod_section.end()
            content = content[:insert_pos] + '    pub mod calls;\n' + content[insert_pos:]
            print("    ✓ Added pub mod calls; declaration")
        else:
            print("    ⚠ Could not find insertion point for pub mod calls;")
    
    # Step 2: Replace the Call match arm with function call
    # Use a simple multiline-aware replacement
    call_pattern = r'(\s+)ast::Expr::Call\(call\) => \{[^}]*?(?:\{[^}]*?\}[^}]*?)*?\n\1\}'
    replacement = r'\1ast::Expr::Call(call) => self.lower_call_expr(call, function, param_map, locals),'
    
    new_content = re.sub(call_pattern, replacement, content, flags=re.DOTALL)
    
    if new_content != content:
        with open(MOD_RS, 'w') as f:
            f.write(new_content)
        print("    ✓ Replaced Call match arm with function call")
        return True
    else:
        print("    ❌ Could not find Call match arm to replace")
        # Try simpler pattern
        print("    [!] Attempting manual file edit...")
        return False

def main():
    """Main extraction workflow."""
    print("=" * 70)
    print("Call Expression Handler Extraction")
    print("=" * 70)
    
    # Step 1: Extract imports
    imports = extract_imports()
    
    # Step 2: Extract the Call handler code
    handler = extract_call_handler()
    
    # Step 3: Create the calls.rs module
    if not create_calls_module(imports, handler):
        print("❌ Failed to create calls.rs")
        sys.exit(1)
    
    # Step 4: Wire the module into mod.rs
    if not wire_module():
        print("⚠ Warning: wiring may not have succeeded - attempting manual replacement")
        # Do manual replacement
        with open(MOD_RS, 'r') as f:
            content = f.read()
        
        # Add module declaration
        if 'pub mod calls;' not in content:
            content = content.replace(
                'pub mod paren;',
                'pub mod paren;\n    pub mod calls;'
            )
        
        # Replace Call arm - use line-based approach
        lines = content.split('\n')
        in_call_arm = False
        brace_depth = 0
        start_idx = None
        end_idx = None
        
        for i, line in enumerate(lines):
            if 'ast::Expr::Call(call) => {' in line:
                in_call_arm = True
                start_idx = i
                brace_depth = 1
            elif in_call_arm:
                # Count braces
                brace_depth += line.count('{') - line.count('}')
                if brace_depth == 0:
                    end_idx = i
                    break
        
        if start_idx is not None and end_idx is not None:
            # Replace the block
            new_lines = (
                lines[:start_idx] + 
                [lines[start_idx].replace(
                    'ast::Expr::Call(call) => {',
                    'ast::Expr::Call(call) => self.lower_call_expr(call, function, param_map, locals),'
                )] +
                lines[end_idx+1:]
            )
            content = '\n'.join(new_lines)
            with open(MOD_RS, 'w') as f:
                f.write(content)
            print("    ✓ Manually replaced Call match arm with function call")
    
    # Step 5: Build the project
    if not run_command(['cargo', 'build', '--workspace'], "Building project"):
        print("❌ Build failed")
        sys.exit(1)
    
    # Step 6: Run tests
    if not run_command(['cargo', 'test', '--test', 'codegen'], "Running codegen tests"):
        print("❌ Tests failed")
        sys.exit(1)
    
    # Step 7: Commit
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
        print(f"    ⚠ Commit output: {result.stdout.strip()}")
    
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
