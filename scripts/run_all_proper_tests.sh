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

# --- Build Local Binaries ---
# Build toasty and oatsc binaries to ensure we use local versions
echo "Building local compiler binaries..."
cargo build -p toasty -p oatsc --bin toasty --bin oatsc || {
    echo "Failed to build compiler binaries"
    exit 1
}

# Determine which build profile to use (debug or release)
# Check if binaries exist in debug first, then release
if [ -f "${REPO_ROOT}/target/debug/toasty" ]; then
    BUILD_PROFILE="debug"
elif [ -f "${REPO_ROOT}/target/release/toasty" ]; then
    BUILD_PROFILE="release"
else
    echo "Error: Could not find built toasty binary"
    exit 1
fi

# Set paths to local binaries
TOASTY_BIN="${REPO_ROOT}/target/${BUILD_PROFILE}/toasty"
OATSC_BIN="${REPO_ROOT}/target/${BUILD_PROFILE}/oatsc"

# Verify binaries exist
if [ ! -f "$TOASTY_BIN" ]; then
    echo "Error: toasty binary not found at $TOASTY_BIN"
    exit 1
fi

if [ ! -f "$OATSC_BIN" ]; then
    echo "Error: oatsc binary not found at $OATSC_BIN"
    exit 1
fi

# Set the output directory for the compiler artifacts.
export OATS_OUT_DIR="${REPO_ROOT}/aot_out"
export OATS_RUNTIME_PATH="${REPO_ROOT}/target/${BUILD_PROFILE}/libruntime.a"
export OATS_STD_PATH="${REPO_ROOT}/target/${BUILD_PROFILE}/liboats_std.a"

# Set OATS_OATSC_PATH to use the local oatsc binary
export OATS_OATSC_PATH="$OATSC_BIN"

# Create a temporary directory within the repo for script temp files
TMP_DIR="${REPO_ROOT}/.test_tmp"
mkdir -p "$TMP_DIR"
# Clean up function to remove temp directory on exit (only if successful)
# Keep temp files on failure for debugging
cleanup() {
    if [ ${#failures[@]} -eq 0 ]; then
        rm -rf "$TMP_DIR"
    else
        echo ""
        echo "Note: Temp files preserved in $TMP_DIR for debugging"
    fi
}
trap cleanup EXIT

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

for file in "${files[@]}"; do
    echo "------------------------------------------------------------"
    echo "Compiling: $file"

    # Create a temporary file to capture all compiler output (stdout & stderr).
    # Use a temp file within the repo
    tmp_out="${TMP_DIR}/$(basename "$file" .oats)_output.txt"

    # Run the local toasty binary and check its exit code.
    # Capture both stdout and stderr, and also preserve exit code
    set +e  # Temporarily disable exit on error to capture exit code
    "$TOASTY_BIN" build "$file" &> "$tmp_out"
    exit_code=$?
    set -e  # Re-enable exit on error
    
    if [ $exit_code -eq 0 ]; then
        # Command succeeded (exit code 0), but we still need to scan the output
        # for diagnostics like type errors that don't cause a non-zero exit.

        # Strip ANSI color codes from the output for reliable grepping.
        cleaned_out="${TMP_DIR}/$(basename "$file" .oats)_cleaned.txt"
        perl -pe 's/\e\[[0-9;]*[A-Za-z]//g' "$tmp_out" > "$cleaned_out"

        # Look for rustc-style diagnostics like 'error:' or 'error[...]'.
        if grep -E '(^|\s)error:|^error\[' "$cleaned_out" >/dev/null; then
            echo "FAILED (errors found in output): $file"
            echo "---- begin output ----"
            # Check if file exists and has content
            if [ ! -s "$tmp_out" ]; then
                echo "(no output captured)"
            else
                # Show full output, or up to 500 lines if very long
                line_count=$(wc -l < "$tmp_out" 2>/dev/null || echo "0")
                if [ "$line_count" -gt 500 ]; then
                    head -n 500 "$tmp_out"
                    echo "... (output truncated at 500 lines, see $tmp_out for full output)"
                else
                    cat "$tmp_out"
                fi
            fi
            echo "---- end output ----"
            echo "Full output saved to: $tmp_out"
            failures+=("$file")
        else
            echo "Compiled successfully: $file"
        fi
    else
        # Command failed (non-zero exit code), indicating a crash or fatal error.
        echo "FAILED (non-zero exit code: $exit_code): $file"
        echo "---- begin output ----"
        # Check if file exists and has content
        if [ ! -s "$tmp_out" ]; then
            echo "(no output captured)"
        else
            # Show full output, or up to 500 lines if very long
            line_count=$(wc -l < "$tmp_out" 2>/dev/null || echo "0")
            if [ "$line_count" -gt 500 ]; then
                head -n 500 "$tmp_out"
                echo "... (output truncated at 500 lines, see $tmp_out for full output)"
            else
                cat "$tmp_out"
            fi
        fi
        echo "---- end output ----"
        echo "Full output saved to: $tmp_out"
        failures+=("$file")
    fi
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
