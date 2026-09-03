#!/bin/sh
set -eu

if [ "$#" -lt 1 ] || [ "$#" -gt 2 ]; then
  echo 'Usage: sh scripts/rust-kernel-source-release-differential.sh <source-release> [wasm-file]' >&2
  exit 2
fi

repository=$(CDPATH= cd -- "$(dirname "$0")/.." && pwd)
release=$(CDPATH= cd -- "$1" && pwd -P)
wasm=${2-}
installed=$(mktemp -d)
trap 'rm -rf -- "$installed"' EXIT HUP INT TERM

cd "$repository"
test -z "$(git status --porcelain=v1 --untracked-files=all)" || {
  echo 'Rust same-pack qualification requires a clean checkout' >&2
  exit 1
}
bun packages/browser-demo/scripts/verify-release.ts "$release"
cp "$release/manifest.json" "$installed/manifest.json"
gzip -dc "$release/hot.bin.gz" > "$installed/hot.bin"
gzip -dc "$release/details.bin.gz" > "$installed/details.bin"

if [ -n "$wasm" ]; then
  bun packages/core/tools/rust-kernel-wasm-differential.ts --same-pack "$installed" "$wasm"
else
  bun packages/core/tools/rust-kernel-wasm-differential.ts --same-pack "$installed"
fi
test -z "$(git status --porcelain=v1 --untracked-files=all)" || {
  echo 'Rust same-pack qualification changed the checkout' >&2
  exit 1
}
