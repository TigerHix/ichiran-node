#!/usr/bin/env bash
set -euo pipefail

crate_dir="$(cd "$(dirname "$0")/.." && pwd)"
repository_dir="$(cd "$crate_dir/../.." && pwd)"
output_dir="${ICHIRAN_RUST_WASM_OUTPUT_DIR:-$repository_dir/packages/core/src/rust-kernel/generated}"
cargo_target_dir="${CARGO_TARGET_DIR:-$crate_dir/target}"
temporary_dir="$(mktemp -d)"
trap 'rm -rf "$temporary_dir"' EXIT

required_wasm_bindgen="wasm-bindgen 0.2.127"
actual_wasm_bindgen="$(wasm-bindgen --version 2>/dev/null || true)"
if [[ "$actual_wasm_bindgen" != "$required_wasm_bindgen" ]]; then
  echo "Rust WASM build requires $required_wasm_bindgen; found ${actual_wasm_bindgen:-none}." >&2
  echo "Install it with: cargo install wasm-bindgen-cli --version 0.2.127 --locked" >&2
  exit 1
fi

if [[ "$cargo_target_dir" != /* ]]; then
  cargo_target_dir="$(pwd)/$cargo_target_dir"
fi
if [[ "$output_dir" != /* ]]; then
  output_dir="$(pwd)/$output_dir"
fi

cd "$crate_dir"
required_rustc="rustc 1.92.0"
actual_rustc="$(rustc --version)"
if [[ "$actual_rustc" != "$required_rustc "* ]]; then
  echo "Rust WASM build requires $required_rustc; found $actual_rustc." >&2
  exit 1
fi

cargo build \
  --locked \
  --manifest-path "$crate_dir/Cargo.toml" \
  --profile wasm-release \
  --target wasm32-unknown-unknown \
  --no-default-features \
  --features wasm

wasm-bindgen \
  --target web \
  --out-dir "$temporary_dir" \
  --out-name ichiran_kernel \
  "$cargo_target_dir/wasm32-unknown-unknown/wasm-release/ichiran_kernel.wasm"

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
