#!/bin/sh
set -eu

if [ "$#" -ne 1 ]; then
  echo 'Usage: sh scripts/source-release-host-qualification.sh <source-release>' >&2
  exit 2
fi

repository=$(CDPATH= cd -- "$(dirname "$0")/.." && pwd)
release=$(CDPATH= cd -- "$1" && pwd)

cd "$repository"
bun packages/browser-demo/scripts/verify-release.ts "$release"
bun run build
ICHIRAN_PACK_DIR="$release" bun test \
  packages/node/tests/runtime-release.test.ts \
  packages/cli/tests/source-release.test.ts \
  packages/api/tests/analyzer-api.test.ts \
  packages/api/tests/input-validation.test.ts
