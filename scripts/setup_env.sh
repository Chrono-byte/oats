#!/usr/bin/env sh
# POSIX-compatible script to set LLVM env vars for building the project.
# Source this file (sh-compatible shell):
# . ./scripts/setup_env.sh


# If the user already set LLVM_SYS_181_PREFIX, trust it
if [ -n "${LLVM_SYS_181_PREFIX}" ]; then
  _found="${LLVM_SYS_181_PREFIX}"
else
  # Candidate prefixes to search for LLVM 18
  _candidates="/usr/lib64/llvm18 /usr/lib/llvm18 /usr/lib/llvm-18 /usr/lib64/llvm-18 /usr/local/opt/llvm@18 /opt/llvm18"

  _found=""
  for _p in $_candidates; do
    if [ -d "$_p" ]; then
      _found="$_p"
      break
    fi
  done

  if [ -z "$_found" ]; then
    echo "[oats] Could not auto-detect LLVM 18 installation." 1>&2
    echo "[oats] Please set LLVM_SYS_181_PREFIX to your LLVM 18 prefix, e.g.:" 1>&2
    echo "    export LLVM_SYS_181_PREFIX=/usr/lib64/llvm18" 1>&2
    return 1 2>/dev/null || exit 1
  fi

  export LLVM_SYS_181_PREFIX="$_found"
fi

if [ -n "${LD_LIBRARY_PATH}" ]; then
  export LD_LIBRARY_PATH="${LLVM_SYS_181_PREFIX}/lib:${LD_LIBRARY_PATH}"
else
  export LD_LIBRARY_PATH="${LLVM_SYS_181_PREFIX}/lib"
fi

echo "[oats] LLVM_SYS_181_PREFIX set to: ${LLVM_SYS_181_PREFIX}"
echo "[oats] LD_LIBRARY_PATH updated to include: ${LLVM_SYS_181_PREFIX}/lib"
