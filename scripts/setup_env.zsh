#!/usr/bin/env zsh
# Source this file to set environment variables needed to build and run the project.
# Example: source ./scripts/setup_env.zsh

# Source the POSIX setup for shared detection logic
if [ -f "$(dirname -- "$0")/setup_env.sh" ]; then
  # shellcheck source=/dev/null
  . "$(dirname -- "$0")/setup_env.sh"
else
  echo "[oats] missing setup_env.sh; please ensure scripts/setup_env.sh exists" >&2
  return 1
fi

# Convenience helpers (available in your shell after sourcing this file)
oats_build() {
  echo "[oats] cargo build (debug)"
  cargo build
}

oats_run_aot() {
  echo "[oats] running AOT runner (builds runtime if necessary)"
  cargo run --bin aot_run
}

oats_clean() {
  echo "[oats] cargo clean"
  cargo clean
}

echo "[oats] helper functions: oats_build, oats_run_aot, oats_clean"
