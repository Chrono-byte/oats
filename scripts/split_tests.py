#!/usr/bin/env python3
"""
Split integration tests under crates/oats/tests/ into categorized subdirectories and
adjust references to the shared test utility at tests/common/mod.rs.

Usage: python3 scripts/split_tests.py
"""
import re
from pathlib import Path
import shutil

ROOT = Path(__file__).resolve().parents[1]
TESTS_DIR = ROOT / 'crates' / 'oats' / 'tests'
COMMON_REL = '../common/mod.rs'  # path used from a subdirectory

PARSING_KEYWORDS = [
    'dump', 'token', 'semicolon', 'arrow', 'parse', 'template'
]
END_TO_END = ['end_to_end.rs']

def categorize(name: str) -> str:
    if name in END_TO_END:
        return 'end_to_end'
    lname = name.lower()
    for k in PARSING_KEYWORDS:
        if k in lname:
            return 'parsing'
    # otherwise go into codegen
    return 'codegen'

def ensure_common_mod_in_file(text: str) -> str:
    # If file already has a path-mod for common, leave alone
    if re.search(r"#\[path\s*=\s*\".*common/mod.rs\"\]\s*mod\s+common\s*;", text):
        return text
    # If file declares `mod common;` replace with path version
    if re.search(r"^\s*mod\s+common\s*;", text, flags=re.M):
        text = re.sub(r"^\s*mod\s+common\s*;", f'#[path = "{COMMON_REL}"] mod common;', text, flags=re.M)
        return text
    # If file uses `use common::...` but doesn't declare mod, prepend the path-mod
    if re.search(r"use\s+common::", text):
        return f'#[path = "{COMMON_REL}"] mod common;\n' + text
    return text


def main():
    if not TESTS_DIR.exists():
        print('tests dir not found:', TESTS_DIR)
        return
    moved = []
    for p in list(TESTS_DIR.glob('*.rs')):
        if p.name == 'mod.rs':
            continue
        if p.name == 'common.rs':
            continue
        if p.name == 'lib.rs':
            continue
        # skip snapshots dir and any existing directories
        if p.name == 'end_to_end.rs' or p.suffix == '.rs':
            cat = categorize(p.name)
            dest_dir = TESTS_DIR / cat
            dest_dir.mkdir(parents=True, exist_ok=True)
            dest = dest_dir / p.name
            text = p.read_text()
            new_text = ensure_common_mod_in_file(text)
            # remove original file and write new one
            p.unlink()
            dest.write_text(new_text)
            moved.append((p.name, cat))
            print(f'Moved {p.name} -> {dest.relative_to(ROOT)}')
    # Also move any codegen snapshot file if present at top
    # snapshots dir should remain in tests/ (do not move)
    print('\nSummary: moved {} files'.format(len(moved)))
    for name, cat in moved:
        print(f' - {name}: {cat}')

if __name__ == '__main__':
    main()
