#!/usr/bin/env bash
set -euo pipefail

repository="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
[ "$#" -ge 1 ] || {
  echo "usage: $0 RELEASE_DIR [--same-pack --source-lock LOCK_FILE]" >&2
  exit 2
}
release="$(cd "$1" && pwd)"
shift
mode_args=()
if [ "${1:-}" = --same-pack ]; then
  [ "${2:-}" = --source-lock ] && [ -n "${3:-}" ] || {
    echo "--same-pack requires --source-lock LOCK_FILE" >&2
    exit 2
  }
  source_lock_absolute="$(cd "$(dirname "$3")" && pwd)/$(basename "$3")"
  case "$source_lock_absolute" in
    "$repository"/*) source_lock="${source_lock_absolute#"$repository"/}" ;;
    *) echo "source lock must be inside the repository: $source_lock_absolute" >&2; exit 2 ;;
  esac
  mode_args=(--same-pack)
  shift 3
fi
[ "$#" -eq 0 ] || { echo "unknown argument: $1" >&2; exit 2; }

for command in bun gzip plutil; do
  command -v "$command" >/dev/null || { echo "missing required tool: $command" >&2; exit 1; }
done
[ -f "$release/manifest.json" ] || { echo "missing $release/manifest.json" >&2; exit 1; }

prepared="$repository/work/apple/prepared-release"
app_pack="$repository/apple/ValidationApp/Resources/Pack"
generated="$repository/apple/ValidationAppTests/Resources/Generated"
rm -rf "$prepared" "$app_pack" "$generated"
mkdir -p "$prepared" "$app_pack" "$generated"
cp "$release/manifest.json" "$prepared/manifest.json"

for name in hot details; do
  file="$(plutil -extract "$name.file" raw "$release/manifest.json")"
  encoding="$(plutil -extract "$name.encoding" raw "$release/manifest.json")"
  [ -f "$release/$file" ] || { echo "missing $release/$file" >&2; exit 1; }
  cp "$release/$file" "$prepared/$file"
  case "$encoding" in
    gzip) gzip -dc "$release/$file" > "$prepared/$name.bin" ;;
    identity) cp "$release/$file" "$prepared/$name.bin" ;;
    *) echo "unsupported release encoding: $encoding" >&2; exit 1 ;;
  esac
done

cp "$prepared/manifest.json" "$app_pack/manifest.json"
hot_file="$(plutil -extract hot.file raw "$prepared/manifest.json")"
details_file="$(plutil -extract details.file raw "$prepared/manifest.json")"
cp "$prepared/$hot_file" "$app_pack/$hot_file"
cp "$prepared/$details_file" "$app_pack/$details_file"

if [ "${mode_args[0]:-}" = --same-pack ]; then
  bun "$repository/packages/rust-kernel/tests/c_parity_corpus.ts" \
    --same-pack "$prepared" --source-lock "$source_lock" > "$generated/clean-corpus.tsv"
  bun "$repository/packages/rust-kernel/tests/c_product_corpus.ts" \
    --same-pack "$prepared" --source-lock "$source_lock" > "$generated/product-corpus.tsv"
else
  bun "$repository/packages/rust-kernel/tests/c_parity_corpus.ts" \
    "$prepared" > "$generated/clean-corpus.tsv"
  bun "$repository/packages/rust-kernel/tests/c_product_corpus.ts" \
    "$prepared" > "$generated/product-corpus.tsv"
fi

echo "$prepared"
