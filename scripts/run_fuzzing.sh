#!/bin/bash
# Fuzzing runner script for Oats compiler
# This script runs fuzzing targets with appropriate timeouts and crash detection

set -e

# Color output
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
NC='\033[0m' # No Color

# Configuration
FUZZ_TIME="${FUZZ_TIME:-60}"  # Default: 60 seconds per target
FUZZ_JOBS="${FUZZ_JOBS:-$(nproc)}"  # Use all CPU cores by default

echo -e "${GREEN}Oats Fuzzing Suite${NC}"
echo "Fuzzing time per target: ${FUZZ_TIME}s"
echo "Parallel jobs: ${FUZZ_JOBS}"
echo ""

# Ensure we're in the right directory
cd "$(dirname "$0")/.."

# Check if nightly toolchain is installed
if ! rustup toolchain list | grep -q nightly; then
    echo -e "${YELLOW}Installing nightly toolchain for fuzzing...${NC}"
    rustup toolchain install nightly
fi

# Check if cargo-fuzz is installed
if ! command -v cargo-fuzz &> /dev/null; then
    echo -e "${RED}Error: cargo-fuzz not found${NC}"
    echo "Install with: cargo install cargo-fuzz"
    exit 1
fi

# Array of fuzz targets
TARGETS=("fuzz_parser" "fuzz_compiler")

# Function to run a single fuzz target
run_fuzz_target() {
    local target=$1
    echo -e "${YELLOW}Running fuzzer: ${target}${NC}"
    
    # Run fuzzer with timeout
    # -j ${FUZZ_JOBS}: parallel fuzzing jobs
    # -max_total_time=${FUZZ_TIME}: time limit in seconds
    # -- -rss_limit_mb=4096: limit memory to 4GB per job
    if cargo +nightly fuzz run "${target}" \
        -j "${FUZZ_JOBS}" \
        -- \
        -max_total_time="${FUZZ_TIME}" \
        -rss_limit_mb=4096 \
        -print_final_stats=1; then
        echo -e "${GREEN}✓ ${target} completed without crashes${NC}"
        return 0
    else
        echo -e "${RED}✗ ${target} found crashes or errors${NC}"
        return 1
    fi
}

# Track results
FAILED_TARGETS=()

# Run all fuzz targets
for target in "${TARGETS[@]}"; do
    echo ""
    if ! run_fuzz_target "${target}"; then
        FAILED_TARGETS+=("${target}")
    fi
    echo ""
done

# Summary
echo "=========================================="
echo -e "${GREEN}Fuzzing Summary${NC}"
echo "=========================================="

if [ ${#FAILED_TARGETS[@]} -eq 0 ]; then
    echo -e "${GREEN}All fuzzing targets passed!${NC}"
    exit 0
else
    echo -e "${RED}Failed targets:${NC}"
    for target in "${FAILED_TARGETS[@]}"; do
        echo "  - ${target}"
    done
    echo ""
    echo "Check fuzz/artifacts/${target}/ for crash inputs"
    exit 1
fi
