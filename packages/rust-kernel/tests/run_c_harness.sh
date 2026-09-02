#!/usr/bin/env bash
set -euo pipefail

crate_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
repository="$(cd "$crate_dir/../.." && pwd)"
release_dir="${1:-$repository/browser-alpha/release}"

cd "$crate_dir"
cargo build --release --locked
case "$(uname -s)" in
  Linux) link_args=(-ldl -lpthread -lm) ;;
  Darwin) link_args=(-lpthread -lm) ;;
  *)
    echo "unsupported C harness host: $(uname -s)" >&2
    exit 2
    ;;
esac
cc -std=c11 -Wall -Wextra -Werror -Iinclude tests/c_harness.c \
  target/release/libichiran_kernel.a "${link_args[@]}" \
  -o target/c_harness
cc -std=c11 -Wall -Wextra -Werror -Iinclude tests/c_product_harness.c \
  target/release/libichiran_kernel.a "${link_args[@]}" \
  -o target/c_product_harness
bun tests/c_parity_corpus.ts "$release_dir" \
  | target/c_harness "$release_dir/hot.bin"
bun tests/c_product_corpus.ts "$release_dir" \
  | target/c_product_harness "$release_dir/hot.bin" "$release_dir/details.bin"
