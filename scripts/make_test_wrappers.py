#!/usr/bin/env python3
"""
Generate top-level test wrapper files under crates/oats/tests/ to include tests placed in subdirectories.
This makes Cargo discover the integration tests while keeping the files organized in subfolders.

It will create one wrapper per immediate subdirectory (parsing, codegen, end_to_end) named
`crates/oats/tests/<subdir>.rs` which contains `#[path = "<subdir>/<file>"] mod <file_stem>;` entries.
"""
from pathlib import Path
ROOT = Path(__file__).resolve().parents[1]
TESTS_DIR = ROOT / 'crates' / 'oats' / 'tests'

subdirs = [p for p in TESTS_DIR.iterdir() if p.is_dir()]

for sd in subdirs:
    files = sorted([f for f in sd.iterdir() if f.suffix == '.rs' and f.name != 'mod.rs'])
    if not files:
        continue
    wrapper = TESTS_DIR / f"{sd.name}.rs"
    lines = [f"// Auto-generated wrapper for tests in {sd.name}\n"]
    for f in files:
        stem = f.stem
        rel = f"{sd.name}/{f.name}"
        # Ensure the module name is a valid identifier
        lines.append(f'#[path = "{rel}"] mod {stem};')
    wrapper.write_text('\n'.join(lines) + '\n')
    print(f'Wrote {wrapper}')
print('Done')
