#!/usr/bin/env bash
# Compile every .oats file in examples/proper_tests/
set -uo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
cd "$REPO_ROOT" || exit 1

# Load LLVM/env helpers if present
if [ -f ./scripts/setup_env.sh ]; then
    # shellcheck source=/dev/null
    source ./scripts/setup_env.sh
fi

export OATS_OUT_DIR="${REPO_ROOT}/aot_out"
files=(examples/proper_tests/*.oats)

if [ ${#files[@]} -eq 0 ]; then
    echo "No .oats files found in examples/proper_tests/"
    exit 0
fi

failures=()
for file in "${files[@]}"; do
    echo "------------------------------------------------------------"
    echo "Compiling: $file"
    if cargo run -p oats --bin aot_run -- "$file"; then
        echo "Compiled: $file"
    else
        echo "FAILED: $file"
        failures+=("$file")
    fi
done

echo "------------------------------------------------------------"
if [ ${#failures[@]} -ne 0 ]; then
    echo "Compilation failed for ${#failures[@]} file(s):"
    printf ' - %s\n' "${failures[@]}"
    exit 1
else
    echo "All files compiled successfully."
    exit 0
fi