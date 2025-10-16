# Oats Runtime Fetcher

This module enables toasty to work standalone by automatically fetching pre-built runtime libraries from GitHub releases.

## How It Works

1. When building an Oats program, the builder first attempts to fetch a pre-built runtime library for your platform
2. The runtime is cached locally in `~/.cache/oats/runtime/` (or `$OATS_RUNTIME_CACHE` if set)
3. If fetching fails or the platform is unsupported, it falls back to building the runtime locally using cargo

## Environment Variables

- `OATS_NO_REMOTE_RUNTIME`: Set to disable fetching and always build locally
- `OATS_RUNTIME_CACHE`: Override the default cache directory

## Supported Platforms

- Linux x86_64
- Linux aarch64 (ARM64)
- macOS x86_64 (Intel)
- macOS aarch64 (Apple Silicon)

## GitHub Actions Workflow

The `.github/workflows/build-runtime.yml` workflow automatically:
- Builds the runtime for all supported platforms
- Creates a GitHub release tagged as `runtime-<commit-sha>`
- Uploads the runtime libraries as release assets

This runs on every push to main that modifies the runtime crate.
