#!/bin/sh
set -eu

release=dist/browser-alpha
shell=packages/browser-demo/dist
if [ ! -L "$release" ]; then
  echo "$release must be the generation symlink produced by alpha:release:build." >&2
  exit 1
fi

commit=$(git rev-parse HEAD)
link_target=$(readlink "$release")
generation=$(basename "$link_target")
case "$generation" in
  *[!0-9a-f]*|'')
    echo "Invalid analyzer release generation: $generation" >&2
    exit 1
    ;;
esac
if [ "${#generation}" -ne 64 ]; then
  echo "Invalid analyzer release generation length: $generation" >&2
  exit 1
fi
if [ "$link_target" != "browser-alpha.generations/$generation" ]; then
  echo "Unexpected analyzer release link target: $link_target" >&2
  echo "Expected browser-alpha.generations/$generation" >&2
  exit 1
fi

bun run alpha:release:verify -- \
  --out "$release" \
  --shell-dir "$shell"

fly deploy "$@" \
  --build-arg "ICHIRAN_SOURCE_COMMIT=$commit" \
  --build-arg "ICHIRAN_RELEASE_GENERATION=$generation"
