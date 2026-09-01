#!/usr/bin/env bash
set -euo pipefail

crate_dir="$(cd "$(dirname "$0")/.." && pwd)"
repository_dir="$(cd "$crate_dir/../.." && pwd)"
output_dir="$repository_dir/packages/core/src/rust-kernel/generated"
temporary_dir="$(mktemp -d)"
trap 'rm -rf "$temporary_dir"' EXIT

cargo build \
  --manifest-path "$crate_dir/Cargo.toml" \
  --profile wasm-release \
  --target wasm32-unknown-unknown \
  --no-default-features \
  --features wasm

wasm-bindgen \
  --target web \
  --out-dir "$temporary_dir" \
  --out-name ichiran_kernel \
  "$crate_dir/target/wasm32-unknown-unknown/wasm-release/ichiran_kernel.wasm"

bun "$repository_dir/node_modules/binaryen/bin/wasm-opt" \
  --enable-bulk-memory \
  --enable-nontrapping-float-to-int \
  -O2 \
  "$temporary_dir/ichiran_kernel_bg.wasm" \
  -o "$temporary_dir/ichiran_kernel_bg.optimized.wasm"

mkdir -p "$output_dir"
cp "$temporary_dir/ichiran_kernel.js" "$output_dir/ichiran_kernel.js"
cp "$temporary_dir/ichiran_kernel.d.ts" "$output_dir/ichiran_kernel.d.ts"
cp "$temporary_dir/ichiran_kernel_bg.wasm.d.ts" "$output_dir/ichiran_kernel_bg.wasm.d.ts"
cp "$temporary_dir/ichiran_kernel_bg.optimized.wasm" "$output_dir/ichiran_kernel_bg.wasm"

wc -c "$output_dir/ichiran_kernel_bg.wasm"
