#!/bin/sh
set -eu

repository=$(CDPATH= cd -- "$(dirname "$0")/.." && pwd -P)
audit_root=$(mktemp -d /tmp/ichiran-data-package.XXXXXX)
trap 'rm -rf -- "$audit_root"' EXIT HUP INT TERM
archive="$audit_root/archive"
mkdir -p "$archive"

git -C "$repository" archive HEAD | tar -x -C "$archive"
(
  cd "$archive"
  bun install --production --frozen-lockfile --filter @ichiran/data
  test ! -e node_modules/postgres
  test ! -e node_modules/@ichiran/reference-postgres
  bun install --frozen-lockfile
  bun test packages/data/tests/source-compiler-oracle-boundary.test.ts \
    --test-name-pattern 'checked-in lock'

  mkdir -p packages/core/dist/obsolete packages/data/dist/obsolete
  printf '%s\n' "import postgres from 'postgres';" > packages/core/dist/obsolete/conn.js
  printf '%s\n' "import postgres from 'postgres';" > packages/data/dist/obsolete/conn.js
  bun run build:source-compiler

  test ! -e packages/core/dist/obsolete/conn.js
  test ! -e packages/data/dist/obsolete/conn.js
  test -f packages/core/dist/index.js
  test -f packages/core/dist/runtime.js
  test -f packages/core/dist/rust-kernel/generated/ichiran_kernel_bg.wasm
  test -f packages/data/dist/index.js
  test -f packages/data/dist/source-compiler/cli.js
  find packages/core/dist -type f -name '*.js' -print \
    | while IFS= read -r output; do
        module=${output#packages/core/dist/}
        module=${module%.js}
        test -f "packages/core/src/$module.ts" \
          || test -f "packages/core/src/$module.js" || {
          echo "built core module has no source owner: $module.js" >&2
          exit 1
        }
      done
  find packages/data/dist -type f -name '*.js' -print \
    | while IFS= read -r output; do
        module=${output#packages/data/dist/}
        module=${module%.js}
        test -f "packages/data/src/$module.ts" || {
          echo "built data module has no source owner: $module.js" >&2
          exit 1
        }
      done
  if find packages/data/dist -type f \
      | grep -Eq '/(migration-cli|db/|[^/]*-oracle\.|release-orchestration\.)'; then
    echo 'source compiler build contains migration-oracle code' >&2
    exit 1
  fi
  if find packages/data/dist/data -type f ! -name 'conj-rules.*' | grep -q .; then
    echo 'source compiler build contains a legacy data-loader module' >&2
    exit 1
  fi
  bun packages/data/dist/source-compiler/cli.js --help | grep -q 'ichiran-data baseline'
  bun -e "import('@ichiran/core').then(value => { if (typeof value.Analyzer !== 'function') process.exit(1) })"
  bun -e "import('./packages/data/dist/index.js').then(value => { if (typeof value.runSourceCompilerRelease !== 'function') process.exit(1) })"

  for runtime_package in node cli api; do
    mkdir -p "packages/$runtime_package/dist/obsolete"
    printf '%s\n' "import 'lru-cache';" \
      > "packages/$runtime_package/dist/obsolete/stale.js"
    bun run --cwd "packages/$runtime_package" build
    test ! -e "packages/$runtime_package/dist/obsolete/stale.js"
    find "packages/$runtime_package/dist" -type f -print \
      | while IFS= read -r output; do
          module=${output#packages/$runtime_package/dist/}
          case "$module" in
            *.d.ts.map) module=${module%.d.ts.map} ;;
            *.js.map) module=${module%.js.map} ;;
            *.d.ts) module=${module%.d.ts} ;;
            *.js) module=${module%.js} ;;
            *) echo "unexpected runtime package artifact: $output" >&2; exit 1 ;;
          esac
          test -f "packages/$runtime_package/src/$module.ts" || {
            echo "built runtime artifact has no source owner: $output" >&2
            exit 1
          }
        done
  done
  if grep -R -E \
      "['\"](@ichiran/reference-postgres|@ichiran/grammar|postgres)([^'\"]*)?['\"]" \
      packages/core/dist packages/data/dist packages/node/dist packages/cli/dist packages/api/dist; then
    echo 'product package output imports a PostgreSQL oracle or grammar dependency' >&2
    exit 1
  fi
)

printf '%s\n' 'Package audit passed: clean archive source compiler and runtime inventories'
