#!/usr/bin/env bash
set -euo pipefail

crate_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
repository="$(cd "$crate_dir/../.." && pwd)"
mode=immutable
if [ "${1:-}" = --same-pack ]; then
  mode=same-pack
  shift
fi
release_dir="${1:-$repository/browser-alpha/release}"
shift || true
generator_args=("$release_dir")
if [ "$mode" = same-pack ]; then
  generator_args=(--same-pack "$release_dir")
  if [ "${1:-}" = --source-lock ] && [ -n "${2:-}" ]; then
    generator_args+=(--source-lock "$2")
    shift 2
  fi
fi
[ "$#" -eq 0 ] || { echo "unknown C harness argument: $1" >&2; exit 2; }

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
bun tests/c_parity_corpus.ts "${generator_args[@]}" \
  | target/c_harness "$release_dir/hot.bin"
target/c_product_harness \
  "$release_dir/hot.bin" \
  "$release_dir/lexicon.bin" \
  "$release_dir/gloss.en.bin" \
  "$release_dir/gloss.zh-Hans.bin"
