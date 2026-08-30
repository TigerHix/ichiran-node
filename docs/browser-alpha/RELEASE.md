# Browser alpha release compiler

The release compiler is the single production path from the frozen PostgreSQL
snapshot and pinned raw inputs to the two browser data assets. PostgreSQL is a
build-time input only. The emitted release has no database client, SQL, or server
dependency.

## Build

Build the production PWA shell first and measure it with the package's checked-in
measurement command. It excludes the staged `analyzer/` directory so the data files
are not counted twice:

```bash
bun run alpha:demo:build
bun run --cwd packages/browser-demo measure:shell
```

Pass that exact integer as `--shell-bytes`, then run from the repository root:

```bash
bun run alpha:release:build -- \
  --database 'postgresql:///ichiran_oracle_ea958336?host=%2Fvar%2Frun%2Fpostgresql' \
  --out dist/browser-alpha \
  --pack-version ichiran-260118 \
  --shell-bytes <measured-production-shell-bytes>
```

The compiler normally requires a completely clean checkout, including no
untracked files. `--allow-dirty` exists only for development builds and does not
alter the recorded `sourceCommit`.

Use the package commands above rather than invoking the TypeScript file directly.
Both supported build and verify commands first compile `packages/reference-postgres`; the
support freezer deliberately imports that fresh `dist` tree and temporarily
routes legacy cache reads through the supplied read-only snapshot transaction.

The database connection is placed in a repeatable-read, read-only transaction.
The compiler checks database name, PostgreSQL version, encoding, and collation;
checks every raw file byte count and SHA-256 from
`browser-alpha/sources.lock.json`; and checks Bun, Node, and all binary format
versions. It then checks exact locked logical counts for every section.
It also requires the exact uncompressed byte length and SHA-256 of all six
components, so a database with equal row counts but different values cannot
produce a release.

The large generated-member relation uses a transaction-local 256 MiB `work_mem`.
PostgreSQL 16 severely underestimates the materialized derived sets and otherwise
chooses a quadratic nested loop, so nested loops are disabled only while freezing
analyzer support and restored before the transaction continues. The affected joins
are explicit hashable equalities and the final `ORDER BY` owns artifact order; these
planner settings change compiler cost, not projected values. A future compiler can
replace this bounded workaround with temporary tables plus `ANALYZE`.

The surface COPY export is compiled twice by the Rust minimal-automaton builder.
Every TypeScript binary encoder, the enclosing hot pack, deterministic gzip
transport, and manifest are also rebuilt and compared byte for byte. No timestamp
or machine path is written to an artifact. Before packaging, the release build also
streams the exhaustive legacy-versus-portable morphology relation through the exact
compiled morphology bytes in the same read-only transaction. Publication requires
the locked relation totals, zero alpha-only or duplicate candidates, and the exact
canonical diff SHA-256.

The output directory receives exactly these release files:

- `hot.bin.gz`: sections 1–5 in fixed order: surface index, root payload,
  morphology, resident analyzer support, and compressed random-access annotations
  plus the generated physical-member overlay;
- `details.bin.gz`: complete root forms, senses, glosses, and sense properties;
- `manifest.json`: transport and installed byte counts and SHA-256 digests;
- `stats.json`: deterministic component counts, section digests, measured morphology
  relation attestation, and size report.

The browser host must serve `hot.bin.gz` and `details.bin.gz` as opaque files (for
example `Content-Type: application/gzip`) **without** a `Content-Encoding` response
header. Browser fetch automatically decodes HTTP content encodings, which would make
the downloaded bytes disagree with the signed manifest before the installer performs
its own streaming decompression.

The alpha browser baseline is Safari 26+ or a current Chromium browser. The installer
requires a dedicated Worker, OPFS (`navigator.storage.getDirectory()`), IndexedDB,
Web Locks, `FileSystemFileHandle.createWritable()`, and `DecompressionStream`; the UI
checks these capabilities before offering installation. The offline PWA shell additionally
uses a Service Worker. Safari added the writable-file stream used by this deliberately
simple streaming installer in
[Safari 26](https://developer.apple.com/documentation/safari-release-notes/safari-26-release-notes).
Supporting older Safari would require a separate sync-access write path and is not part
of this milestone. The Chromium gate pins the whole browser to one Linux CPU, adds five
same-affinity contention peers, and accepts the run only when the exact analyzer Worker
measures a 5.0-7.5x slowdown. It is a repeatable phone-performance proxy, not a
substitute for the deferred physical-iPhone check.

Generated entries are not a shallow copy of PostgreSQL rows. Section 4 keeps the
morphology-rule-to-semantic-alias table and the narrower real-lexical-target collision
facts resident. Section 5 stores count-only exceptions and exact physical-member rows,
keyed by root and one- or two-rule aliases. Every member-bearing exception retains all
physical conjugations, ordered `conj_prop` rows, tri-state properties, and exact
two-stage via-member binding. Its decoded rows are 10 bytes each; repeated semantic
keys are intentional. Consecutive roots are packed into roughly 256 KiB decoded gzip
blocks, and a resident root index supports preload. Worker startup inflates and verifies
the pinned pack's generated blocks in one pass. Split/hint annotations remain lazy
behind a 16-entry LRU. `stats.json` records the final block count, decoded bytes, LRU
source-payload bound, exact index,
raw/internal-compressed totals, block/member counts, and largest compressed and decoded
blocks.

Files are staged and verified before the four final names are atomically replaced.
The output directory must be below the repository root.

## Verify

Verification takes the same measured shell-byte input used by the build:

```bash
bun run alpha:release:verify -- \
  --out dist/browser-alpha \
  --shell-bytes <the-same-measured-byte-count>
```

It rechecks the checkout, source lock, toolchain, release manifest digest, download
and installed hashes and lengths, fixed section set, every section checksum and
reader header, the details header, stats identity, and all three release gates:

- always-resident uncompressed `hot.bin` at most 24 MiB;
- persisted hot + details + precached shell + cached manifest + compact
  `install.json` + the 36-byte logical IndexedDB install ID at most 64 MiB;
- first-install shell + manifest + compressed hot + compressed details at most
  25 MiB.

The passed shell size is deliberately not inferred from a directory or a source
map convention. The caller owns one exact production-shell measurement, and the
same integer is recorded in `stats.json` and required during verification.

Record accepted component sizes and hashes from the release's own `stats.json`; do not
reuse measurements from an earlier generated-overlay format.

## Updating a pinned pack

Refresh the lock from the qualified read-only database instead of editing expected
values by hand:

```bash
bun run alpha:release:refresh-lock -- \
  --database 'postgresql:///ichiran_oracle_ea958336?host=%2Fvar%2Frun%2Fpostgresql'
```

This command does not consume artifact identities from the old lock. It compiles
and verifies the target database, then atomically writes a deterministic v2 lock
with the upstream Lisp revision, frozen PostgreSQL reference revision, release-dump
identity, database/toolchain identity, raw inputs, artifact counts and exact artifact
digests. Normal `build` and `verify` remain strict consumers of that lock.

Review the lock diff and pass the exhaustive parity jobs before publishing. The
alpha has no update channel, migration, delta, or fallback pack.
