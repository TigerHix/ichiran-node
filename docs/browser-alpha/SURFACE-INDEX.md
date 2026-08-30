# Route-aware surface index artifact

The format and algorithm remain current. Counts and digests below are locked to
upstream Ichiran `ea9583368e67cad22d94abae8dbcc8df96d99bcd` and data release
`ichiran-260118`. The generated release's `dist/browser-alpha/stats.json`, checked
against `browser-alpha/sources.lock.json`, is the source of truth for measurements.

The production surface index is a deterministic minimal acyclic byte automaton.
It stores only accepted endpoints, direct/morphology terminal bits, and a direct
subtree count per state. The browser reader derives a dense direct-root rank
while walking a surface and can recover a surface from that rank; it stores no
generated morphology postings.

## Logical set and runtime route

The compiler first forms the normalized logical union, then applies the analyzer's
table route. The locked release records:

| Measure | Count |
|---|---:|
| normalized input surfaces | 8,411,378 |
| runtime-active surfaces | 8,393,704 |
| active direct surfaces | 443,275 |
| active morphology surfaces | 7,959,940 |
| active direct/morphology overlap | 9,511 |
| omitted by the runtime route | 17,674 |

The route is exactly core's `testWord(surface, 'kana')`: a non-empty string in
`[ァ-ヺヽヾーぁ-ゔゝゞ]` uses `kana_text`; every other string uses
`kanji_text`. The COPY projection exports the normalized relation and its table
flags; the route filter excludes keys that exact runtime lookup cannot reach. The
lock deliberately records the aggregate omitted count rather than carrying the old
snapshot's wrong-table taxonomy into the runtime contract.

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
  'postgresql:///ichiran_oracle_ea958336?host=/var/run/postgresql' \
  "$surface_query" \
  packages/data/tools/surface-index/target/release/ichiran-surface-index \
  /tmp/ichiran-surface-index.bin

gzip -9 -c /tmp/ichiran-surface-index.bin \
  > /tmp/ichiran-surface-index.bin.gz
sha256sum /tmp/ichiran-surface-index.bin
```

## Locked `ichiran-260118` artifact

These are generated-release values, not standalone microbenchmarks:

| Measure | Result |
|---|---:|
| input logical surfaces | 8,411,378 |
| runtime-active surfaces | 8,393,704 |
| minimal states | 589,125 |
| edges | 971,845 |
| raw section bytes | 8,600,452 (8.20 MiB) |

SHA-256:

```text
427f62904262f698774172cdbde5ce30532511111b5565b2e4bd40fddb1164df
```

The root payload is 9,088,056 bytes, so the two raw sections total 17,688,508
bytes (16.87 MiB). The complete installed hot image, after morphology, analyzer
support, annotations, and the pack header are included, is 24,857,288 bytes
(23.71 MiB), 308,536 bytes below the 24 MiB gate. Wire and persisted totals belong
to the complete release and are recorded in its generated `stats.json`.

## Verification coverage

- Fixture tests exhaustively compare compiler output with brute-force routing,
  lookup flags, scan endpoints, direct ranks, and rank-to-surface selection.
- Tests reject unsorted input and malformed headers, state sentinels, edge
  ordering/targets, subtree counts, and inconsistent language totals.
- The release compiler accounts for all 8,411,378 normalized input surfaces:
  8,393,704 are present and 17,674 are route-omitted. The 443,275 direct
  endpoints define the dense rank space.
- Release verification reopens the emitted section, checks its locked byte length
  and digest, and rejects any mismatch between compiler counts, lock, and
  `stats.json`.
