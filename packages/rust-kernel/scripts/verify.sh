#!/usr/bin/env bash
set -euo pipefail

crate_dir="$(cd "$(dirname "$0")/.." && pwd)"
repository_dir="$(cd "$crate_dir/../.." && pwd)"
generated_dir="$repository_dir/packages/core/src/rust-kernel/generated"
temporary_dir="$(mktemp -d)"
trap 'rm -rf "$temporary_dir"' EXIT

cd "$crate_dir"
cargo fmt --check
cargo clippy --locked --all-targets --all-features -- -D warnings
cargo test --locked --all-targets --all-features

CARGO_TARGET_DIR="$temporary_dir/target" \
ICHIRAN_RUST_WASM_OUTPUT_DIR="$temporary_dir/generated" \
  bash "$crate_dir/scripts/build-wasm.sh"

for file in \
  ichiran_kernel.js \
  ichiran_kernel.d.ts \
  ichiran_kernel_bg.wasm \
  ichiran_kernel_bg.wasm.d.ts
do
  if ! cmp "$generated_dir/$file" "$temporary_dir/generated/$file"; then
    echo "Generated Rust WASM artifact is stale: $file" >&2
    echo "Regenerate it with: bun run build:rust-wasm" >&2
    exit 1
  fi
done

echo "Rust checks and fresh generated WASM comparison passed"
