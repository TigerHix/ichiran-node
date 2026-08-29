# Route-aware surface index artifact

The production surface index is a deterministic minimal acyclic byte automaton.
It stores only accepted endpoints, direct/morphology terminal bits, and a direct
subtree count per state. The browser reader derives a dense direct-root rank
while walking a surface and can recover a surface from that rank; it stores no
generated morphology postings.

## Logical set and runtime route

The oracle's familiar counts describe the normalized logical union before the
analyzer's table route is applied:

| Set | Direct | Morphology | Both | Union |
|---|---:|---:|---:|---:|
| normalized, all routes | 448,323 | 7,850,940 | 9,395 | 8,289,868 |
| runtime-active route | 432,664 | 7,849,804 | 9,394 | 8,273,074 |
| filtered by the route | 15,659 | 1,136 | 1 | 16,794 |

The route is exactly core's `testWord(surface, 'kana')`: a non-empty string in
`[ァ-ヺヽヾーぁ-ゔゝゞ]` uses `kana_text`; every other string uses
`kanji_text`. The 16,794 omitted logical keys are therefore the already-measured
16,622 wrong-table non-kana keys plus 172 wrong-table all-kana keys. They have
never been reachable through current exact lookup.

The physical text-table union also contains 8,289,868 distinct strings, but it
is not the same set. It has 55 stale generated strings (`じゃないで` and 54
`...ものは...` forms), while the normalized `conj_source_reading` relation has
55 replacements (`為り` and the corrected 54 `...のは...` forms). The COPY
projection intentionally exports the normalized relation: this is the reviewed
alpha correction policy, not an accidental reduction of the oracle union.

## Reproduction

The checked-in COPY projection is `SURFACE_INDEX_COPY_QUERY` in
`packages/data/src/browser-pack/surface-index.ts`. It emits one UTF-8 bytewise
sorted TSV row per normalized logical surface and its four table flags.

```bash
cargo build --release \
  --manifest-path packages/data/tools/surface-index/Cargo.toml

surface_query=$(bun -e \
  "import { SURFACE_INDEX_COPY_QUERY } from './packages/data/src/browser-pack/surface-index.ts'; process.stdout.write(SURFACE_INDEX_COPY_QUERY)")

/usr/bin/time -v bash -o pipefail -c \
  'psql "$1" -X -qAt -v ON_ERROR_STOP=1 -c "SET work_mem='"'"'512MB'"'"'" -c "$2" | "$3" --output "$4"' \
  bash \
  'postgresql:///ichiran_test?host=/var/run/postgresql' \
  "$surface_query" \
  packages/data/tools/surface-index/target/release/ichiran-surface-index \
  /tmp/ichiran-surface-index.bin

gzip -9 -c /tmp/ichiran-surface-index.bin \
  > /tmp/ichiran-surface-index.bin.gz
sha256sum /tmp/ichiran-surface-index.bin
```

## Measured artifact

Measurements are from the frozen local `ichiran_test` oracle on 2026-08-28.

| Measure | Result |
|---|---:|
| input logical surfaces | 8,289,868 |
| runtime-active surfaces | 8,273,074 |
| minimal states | 578,318 |
| edges | 952,316 |
| raw bytes | 8,435,880 (8.05 MiB) |
| gzip-9 bytes | 3,986,769 (3.80 MiB) |
| SQL export plus streaming build | 107.8 s wall |
| build pipeline peak RSS | 37,132 KiB |
| strict reader open/validation, desktop Node | 34.3 ms |

SHA-256:

```text
f880f8d9f6873cd495596084330ea408d3216b13fb4c6652bd7ec4625f91386a
```

Two complete SQL exports and builds produced identical bytes. Combined with the
measured root payload, the surface-plus-root subtotal is approximately 16.51 MiB. The
current complete qualification hot image, after morphology, analyzer support, and
annotations are included, is 24,422,280 bytes (23.29 MiB), leaving 743,544 bytes
(0.71 MiB) under the 24 MiB gate.

## Verification coverage

- Fixture tests exhaustively compare compiler output with brute-force routing,
  lookup flags, scan endpoints, direct ranks, and rank-to-surface selection.
- Tests reject unsorted input and malformed headers, state sentinels, edge
  ordering/targets, subtree counts, and inconsistent language totals.
- A second independent SQL stream checked all 8,289,868 oracle rows against the
  TypeScript reader. Every active route/flag matched, all 16,794 inactive rows
  were absent, and all 432,664 direct ranks round-tripped to their exact surface.
- The independent exhaustive pass completed in 95.5 seconds wall. All portable
  tests and both portable/data TypeScript checks passed afterward.
