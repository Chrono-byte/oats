#!/usr/bin/env bash
# Compile every .oats file in examples/proper_tests/

# Exit immediately if a command exits with a non-zero status (-e),
# treat unset variables as an error (-u), and ensure pipelines fail
# on the first command that fails (-o pipefail).
set -euo pipefail

# Enable nullglob so that if no files match the glob pattern, the result is
# an empty array instead of the pattern string itself.
shopt -s nullglob

# --- Setup Paths ---
# Get the directory of the script itself, resolving symlinks.
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# Assume the repo root is one level up from the script's directory.
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
# Change to the repository root directory for consistent pathing.
cd "$REPO_ROOT" || exit 1

# --- Environment Setup ---
# Load LLVM/env helpers if present.
# if [ -f ./scripts/setup_env.sh ]; then
    # shellcheck source=/dev/null
    # source ./scripts/setup_env.sh
# fi

# Set the output directory for the compiler artifacts.
export OATS_OUT_DIR="${REPO_ROOT}/aot_out" 
export OATS_RUNTIME_PATH="/home/chrono/Dev/oats/target/debug/libruntime.a" 
export OATS_STD_PATH="/home/chrono/Dev/oats/target/debug/liboats_std.a"

# --- Find Files ---
# Find all .oats files to compile. Thanks to nullglob, this will be an
# empty array if no files are found.
files=(examples/proper_tests/*.oats)

if [ ${#files[@]} -eq 0 ]; then
    echo "No .oats files found in examples/proper_tests/"
    exit 0
fi

# --- Compilation ---
failures=()

# Determine which compiler binary to use (always use 'toasty')
compiler_bin="toasty"

for file in "${files[@]}"; do
    echo "------------------------------------------------------------"
    echo "Compiling: $file"
    
    # Create a temporary file to capture all compiler output (stdout & stderr).
    tmp_out=$(mktemp)

    # Run the selected compiler binary and check its exit code.
    if cargo run -p toasty --bin "$compiler_bin" -- build "$file" &> "$tmp_out"; then
        # Command succeeded (exit code 0), but we still need to scan the output
        # for diagnostics like type errors that don't cause a non-zero exit.
        
        # Strip ANSI color codes from the output for reliable grepping.
        cleaned_out=$(mktemp)
        perl -pe 's/\e\[[0-9;]*[A-Za-z]//g' "$tmp_out" > "$cleaned_out"

        # Look for rustc-style diagnostics like 'error:' or 'error[...]'.
        if grep -E '(^|\s)error:|^error\[' "$cleaned_out" >/dev/null; then
            echo "FAILED (errors found in output): $file"
            echo "---- begin output ----"
            sed -n '1,200p' "$tmp_out" # Print first 200 lines of raw output
            echo "---- end output ----"
            failures+=("$file")
        else
            echo "Compiled successfully: $file"
        fi
        rm -f "$cleaned_out"
    else
        # Command failed (non-zero exit code), indicating a crash or fatal error.
        echo "FAILED (non-zero exit code): $file"
        echo "---- begin output ----"
        sed -n '1,50p' "$tmp_out" # Print first 200 lines of raw output
        echo "---- end output ----"
        failures+=("$file")
    fi
    # Clean up the temporary output file.
    rm -f "$tmp_out"
done

# --- Summary ---
echo "------------------------------------------------------------"
if [ ${#failures[@]} -ne 0 ]; then
    echo "Compilation failed for ${#failures[@]} file(s):"
    # Use printf for safely printing each failed file on a new line.
    printf ' - %s\n' "${failures[@]}"
    exit 1
else
    echo "All files compiled successfully."
    exit 0
fi