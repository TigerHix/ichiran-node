#!/bin/sh
set -eu

release=dist/browser-alpha
for file in manifest.json hot.bin.gz details.bin.gz stats.json; do
  if [ ! -f "$release/$file" ]; then
    echo "Missing $release/$file; build and verify the analyzer release first." >&2
    exit 1
  fi
done

fly deploy "$@"
