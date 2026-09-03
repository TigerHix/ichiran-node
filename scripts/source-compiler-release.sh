#!/bin/sh
set -eu

required_bun_version=1.3.5
actual_bun_version=$(bun --version)
test "$actual_bun_version" = "$required_bun_version" || {
  echo "Source compiler release requires Bun $required_bun_version; found $actual_bun_version" >&2
  exit 1
}
command -v cargo >/dev/null 2>&1 || {
  echo 'Source compiler release requires cargo from rustup with toolchain 1.92.0' >&2
  exit 1
}
cargo +1.92.0 --version >/dev/null 2>&1 || {
  echo 'Source compiler release requires rustup toolchain 1.92.0 (cargo +1.92.0)' >&2
  exit 1
}

repository=$(git -C "$(dirname "$0")" rev-parse --show-toplevel)
cd "$repository"
qualified_head=$(git rev-parse HEAD)

assert_source_checkout() {
  test "$(git rev-parse HEAD)" = "$qualified_head" || {
    echo 'Source compiler checkout moved during build or release' >&2
    exit 1
  }
  test -z "$(git status --porcelain=v1 --untracked-files=all)" || {
    echo 'Source compiler release requires a clean checkout' >&2
    exit 1
  }
}

assert_source_checkout
bun run build:source-compiler
assert_source_checkout
ICHIRAN_SOURCE_COMPILER_COMMIT="$qualified_head" \
  bun --smol packages/data/dist/source-compiler/cli.js "$@"
assert_source_checkout
