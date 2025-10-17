#!/bin/bash
# Vendor TypeScript conformance tests for parser validation
# This script downloads the TypeScript conformance test suite and extracts
# the test cases for use in Oats parser testing.

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
VENDOR_DIR="$PROJECT_ROOT/third_party/typescript_conformance_tests"

echo "Vendoring TypeScript conformance tests to $VENDOR_DIR"

# Clean existing directory
rm -rf "$VENDOR_DIR"
mkdir -p "$VENDOR_DIR"

# Clone TypeScript repo (shallow clone for speed)
echo "Cloning TypeScript repository..."
git clone --depth 1 https://github.com/microsoft/TypeScript.git "$VENDOR_DIR/repo"

# Copy conformance tests
echo "Copying conformance test files..."
cp -r "$VENDOR_DIR/repo/tests/cases/conformance" "$VENDOR_DIR/"

# Clean up
rm -rf "$VENDOR_DIR/repo"

# Create README
cat > "$VENDOR_DIR/README.md" << 'EOF'
# TypeScript Conformance Tests

This directory contains vendored TypeScript conformance test cases from
https://github.com/microsoft/TypeScript

These tests are used to validate that the Oats parser can successfully parse
all valid TypeScript syntax.

## Update Instructions

Run `scripts/vendor_typescript_conformance_tests.sh` to update to the latest
version from the TypeScript main branch.

## License

These files are licensed under the Apache License 2.0, as per the TypeScript
project.
EOF

echo "TypeScript conformance tests vendored successfully."
echo "Run 'cargo test typescript_conformance' to validate parser compatibility."