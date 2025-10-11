#!/usr/bin/env bash
# Compile every .oats file in examples/proper_tests/
set -uo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
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
    # Capture both stdout and stderr so we can scan for compiler errors even
    # when cargo exits with 0. We save output to a temp file per invocation.
    tmp_out=$(mktemp)
    if cargo run -p oats --bin aot_run -- "$file" > "$tmp_out" 2>&1; then
        # Strip common ANSI color sequences from the captured output so
        # grepping for diagnostics is robust even when colored output is
        # produced. Use perl which handles the escape sequence reliably.
        cleaned_out=$(mktemp)
        perl -pe 's/\e\[[0-9;]*[A-Za-z]//g' "$tmp_out" > "$cleaned_out"

        # Look for rustc-style diagnostics such as 'error:' or 'error[...]'.
        if grep -E '(^|\s)error:|^error\[' "$cleaned_out" >/dev/null; then
            echo "FAILED (errors in output): $file"
            echo "---- begin output ----"
            sed -n '1,200p' "$tmp_out"
            echo "---- end output ----"
            failures+=("$file")
        else
            echo "Compiled: $file"
        fi
        rm -f "$cleaned_out"
    else
        echo "FAILED: $file"
        echo "---- begin output ----"
        sed -n '1,200p' "$tmp_out"
        echo "---- end output ----"
        failures+=("$file")
    fi
    rm -f "$tmp_out"
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