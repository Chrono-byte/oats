#!/bin/bash
# Test script to verify standalone toasty functionality

set -e

echo "=== Testing Standalone Toasty ==="

# Create a test directory
TEST_DIR=$(mktemp -d)
echo "Test directory: $TEST_DIR"

# Create a simple test program
cat > "$TEST_DIR/test.oats" << 'EOF'
export function main(): number {
    const x = 10;
    const y = 20;
    return x + y;
}
EOF

echo ""
echo "1. Testing with local runtime build (OATS_NO_REMOTE_RUNTIME=1)"
export OATS_NO_REMOTE_RUNTIME=1
cargo run -p toasty --quiet -- build "$TEST_DIR/test.oats" --out-dir "$TEST_DIR" --quiet

if [ -x "$TEST_DIR/test" ]; then
    RESULT=$("$TEST_DIR/test")
    if [ "$RESULT" = "30" ]; then
        echo "✓ Local build test passed (output: $RESULT)"
    else
        echo "✗ Local build test failed (expected 30, got $RESULT)"
        exit 1
    fi
else
    echo "✗ Executable not created"
    exit 1
fi

# Clean up test artifacts
rm -f "$TEST_DIR/test" "$TEST_DIR/test.o" "$TEST_DIR/test.ll"

echo ""
echo "2. Testing with remote runtime (if available)"
unset OATS_NO_REMOTE_RUNTIME
cargo run -p toasty --quiet -- build "$TEST_DIR/test.oats" --out-dir "$TEST_DIR" --quiet || {
    echo "⚠ Remote runtime fetch failed (expected if no release exists yet)"
}

if [ -x "$TEST_DIR/test" ]; then
    RESULT=$("$TEST_DIR/test")
    if [ "$RESULT" = "30" ]; then
        echo "✓ Remote build test passed (output: $RESULT)"
    else
        echo "✗ Remote build test failed (expected 30, got $RESULT)"
        exit 1
    fi
fi

# Cleanup
rm -rf "$TEST_DIR"

echo ""
echo "=== All tests passed! ==="
