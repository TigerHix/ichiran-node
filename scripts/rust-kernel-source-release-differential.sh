#!/bin/sh
set -eu

if [ "$#" -lt 1 ]; then
  echo 'Usage: sh scripts/rust-kernel-source-release-differential.sh <source-release> [wasm-file] [--source-lock <file>]' >&2
  exit 2
fi

repository=$(CDPATH= cd -- "$(dirname "$0")/.." && pwd)
release=$(CDPATH= cd -- "$1" && pwd -P)
shift
wasm=
source_lock=
while [ "$#" -gt 0 ]; do
  case "$1" in
    --source-lock)
      [ "$#" -ge 2 ] || { echo '--source-lock requires a file' >&2; exit 2; }
      source_lock=$2
      shift 2
      ;;
    *)
      [ -z "$wasm" ] || { echo "Unknown qualification argument: $1" >&2; exit 2; }
      wasm=$1
      shift
      ;;
  esac
done
installed=$(mktemp -d)
trap 'rm -rf -- "$installed"' EXIT HUP INT TERM

cd "$repository"
qualification_commit=$(git rev-parse HEAD)
test -z "$(git status --porcelain=v1 --untracked-files=all)" || {
  echo 'Rust same-pack qualification requires a clean checkout' >&2
  exit 1
}
if [ -n "$source_lock" ]; then
  bun packages/browser-demo/scripts/verify-release.ts "$release" --source-lock "$source_lock"
else
  bun packages/browser-demo/scripts/verify-release.ts "$release"
fi
cp "$release/manifest.json" "$installed/manifest.json"
gzip -dc "$release/hot.bin.gz" > "$installed/hot.bin"
gzip -dc "$release/details.bin.gz" > "$installed/details.bin"

if [ -n "$wasm" ]; then
  bun packages/core/tools/rust-kernel-wasm-differential.ts --same-pack "$installed" "$wasm"
else
  bun packages/core/tools/rust-kernel-wasm-differential.ts --same-pack "$installed"
fi
test "$(git rev-parse HEAD)" = "$qualification_commit" || {
  echo 'Rust same-pack qualification source commit changed during the run' >&2
  exit 1
}
test -z "$(git status --porcelain=v1 --untracked-files=all)" || {
  echo 'Rust same-pack qualification changed the checkout' >&2
  exit 1
}
