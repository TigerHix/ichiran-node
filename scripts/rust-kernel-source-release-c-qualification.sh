#!/bin/sh
set -eu

if [ "$#" -ne 1 ] && [ "$#" -ne 3 ]; then
  echo 'Usage: sh scripts/rust-kernel-source-release-c-qualification.sh <source-release> [--source-lock <file>]' >&2
  exit 2
fi

repository=$(CDPATH= cd -- "$(dirname "$0")/.." && pwd)
release=$(CDPATH= cd -- "$1" && pwd -P)
source_lock=
if [ "$#" -eq 3 ]; then
  [ "$2" = '--source-lock' ] || { echo "Unknown qualification argument: $2" >&2; exit 2; }
  source_lock=$3
fi
installed=$(mktemp -d)
trap 'rm -rf -- "$installed"' EXIT HUP INT TERM

cd "$repository"
qualification_commit=$(git rev-parse HEAD)
test -z "$(git status --porcelain=v1 --untracked-files=all)" || {
  echo 'Native same-pack qualification requires a clean checkout' >&2
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
if [ -n "$source_lock" ]; then
  bash packages/rust-kernel/tests/run_c_harness.sh --same-pack "$installed" \
    --source-lock "$source_lock"
else
  bash packages/rust-kernel/tests/run_c_harness.sh --same-pack "$installed"
fi
test "$(git rev-parse HEAD)" = "$qualification_commit" || {
  echo 'Native same-pack qualification source commit changed during the run' >&2
  exit 1
}
test -z "$(git status --porcelain=v1 --untracked-files=all)" || {
  echo 'Native same-pack qualification changed the checkout' >&2
  exit 1
}
