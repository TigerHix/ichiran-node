#!/bin/sh
set -eu

if [ "$#" -ne 1 ] && [ "$#" -ne 3 ]; then
  echo 'Usage: sh scripts/source-release-host-qualification.sh <source-release> [--source-lock <file>]' >&2
  exit 2
fi

repository=$(CDPATH= cd -- "$(dirname "$0")/.." && pwd)
release=$(CDPATH= cd -- "$1" && pwd -P)
source_lock=
if [ "$#" -eq 3 ]; then
  [ "$2" = '--source-lock' ] || { echo "Unknown qualification argument: $2" >&2; exit 2; }
  source_lock=$3
fi

cd "$repository"
qualification_commit=$(git rev-parse HEAD)
test -z "$(git status --porcelain=v1 --untracked-files=all)" || {
  echo 'Source host qualification requires a clean checkout' >&2
  exit 1
}
if [ -n "$source_lock" ]; then
  bun packages/browser-demo/scripts/verify-release.ts "$release" --source-lock "$source_lock"
else
  bun packages/browser-demo/scripts/verify-release.ts "$release"
fi
bun run build
RUN_PARITY_TESTS=true ICHIRAN_PACK_DIR="$release" bun test \
  packages/node/tests/runtime-release.test.ts \
  packages/cli/tests/source-release.test.ts \
  packages/cli/tests/upstream-260118-parity.test.ts \
  packages/api/tests/analyzer-api.test.ts \
  packages/api/tests/input-validation.test.ts
test "$(git rev-parse HEAD)" = "$qualification_commit" || {
  echo 'Source host qualification source commit changed during the run' >&2
  exit 1
}
test -z "$(git status --porcelain=v1 --untracked-files=all)" || {
  echo 'Source host qualification changed the checkout' >&2
  exit 1
}
