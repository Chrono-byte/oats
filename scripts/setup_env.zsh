oats_build() {
oats_run_aot() {
oats_clean() {
echo "[oats] helper functions: oats_build, oats_run_aot, oats_clean"
#!/usr/bin/env zsh
# Convenience zsh wrapper that sources the POSIX setup and adds shell helpers.
# Usage: source ./scripts/setup_env.zsh

SCRIPT_DIR=$(cd "$(dirname -- "$0")" && pwd)
POSIX_SETUP="$SCRIPT_DIR/setup_env.sh"

if [ -f "$POSIX_SETUP" ]; then
  # shellcheck source=/dev/null
  . "$POSIX_SETUP"
else
  echo "[oats] missing setup_env.sh; please ensure scripts/setup_env.sh exists" >&2
  return 1
fi

# zsh-friendly convenience helpers
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
