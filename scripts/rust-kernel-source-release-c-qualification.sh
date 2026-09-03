#!/bin/sh
set -eu

if [ "$#" -ne 1 ]; then
  echo 'Usage: sh scripts/rust-kernel-source-release-c-qualification.sh <source-release>' >&2
  exit 2
fi

repository=$(CDPATH= cd -- "$(dirname "$0")/.." && pwd)
release=$(CDPATH= cd -- "$1" && pwd -P)
installed=$(mktemp -d)
trap 'rm -rf -- "$installed"' EXIT HUP INT TERM

cd "$repository"
test -z "$(git status --porcelain=v1 --untracked-files=all)" || {
  echo 'Native same-pack qualification requires a clean checkout' >&2
  exit 1
}
bun packages/browser-demo/scripts/verify-release.ts "$release"
cp "$release/manifest.json" "$installed/manifest.json"
gzip -dc "$release/hot.bin.gz" > "$installed/hot.bin"
gzip -dc "$release/details.bin.gz" > "$installed/details.bin"
bash packages/rust-kernel/tests/run_c_harness.sh --same-pack "$installed"
test -z "$(git status --porcelain=v1 --untracked-files=all)" || {
  echo 'Native same-pack qualification changed the checkout' >&2
  exit 1
}
