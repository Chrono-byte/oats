#!/usr/bin/env bash
set -euo pipefail
SCRIPT_DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
ROOT_DIR=$(cd "$SCRIPT_DIR/.." && pwd)

# Build and run the cycle_test example using the existing helper.
EXAMPLE="$ROOT_DIR/examples/cycle_test.oats"

# Reuse the run_aot_tempdir.sh script but point to our example
cd "$ROOT_DIR"
# Ensure env script is sourced like the helper expects
if [ -f "$ROOT_DIR/scripts/setup_env.sh" ]; then
    . "$ROOT_DIR/scripts/setup_env.sh"
fi

OATS_OUT_DIR="$ROOT_DIR/aot_out" \
OATS_SRC_FILE="$EXAMPLE" \
    cargo run -p oats --bin aot_run -- "$EXAMPLE"
