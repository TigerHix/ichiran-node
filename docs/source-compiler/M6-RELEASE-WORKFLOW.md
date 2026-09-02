# M6 source-native release workflow

The release command has two concrete modes. Both compile the same TypeScript
semantic model, stream generated conjugations through bounded binary spools,
invoke the existing Rust surface-index compiler, and write pack format v1.
Neither mode resolves or loads the PostgreSQL reference package or configures a
database. Separate browser-pack `*-oracle.ts` modules retain the migration
loaders, while the release imports only compiler-owned semantic-input builders.

## Qualified January baseline

The baseline mode verifies every file in
`data/source-compiler-sources.lock.json`, then compares section bytes and counts
against `work/m2-baseline`. Root payload ordering has its independent review in
`data/source-compiler-direct-order-attestation.json`. Analyzer support and
annotations may differ only when their complete source and qualified section
identities and count groups exactly equal the generated-order release gate.
Every other difference is a hard failure. The baseline manifest, compressed
assets, and `stats.json` must
also match the pinned SHA-256 checksum index from the immutable qualified
release; a self-consistent local replacement cannot become a new baseline.

A fresh clone acquires that comparison pack directly from the immutable GitHub
release. The acquisition command pins the checksum-index identity, then verifies
the manifest, hot pack, details pack and producer statistics named by that index:

```sh
bun scripts/acquire-qualified-source-compiler-baseline.ts work/m2-baseline
```

An existing directory is verified in place; it is never silently refreshed. A
missing directory is staged, verified and renamed into place only after all five
published files pass. This makes `work/m2-baseline` a reproducible cache rather
than an undeclared prerequisite.

The source lock assigns exactly one verified file to each of nine compiler roles:
JMdict, Kanjidic2, extra entries, municipalities, wards, chronological errata,
compatibility, `kwpos.csv`, and `conjo.csv`. Release code receives those verified
paths directly. Missing, duplicate, ambiguous and unknown role assignments fail,
and `stats.json` records the exact byte count and SHA-256 of every consumed file.
Lock verification also checks the expanded JMdict and Kanjidic2 byte identities
and embedded upstream version markers, and loads both semantic ledgers through
their canonical parsers to verify the declared row counts. Archive-patch hashes
are acquisition provenance: the named acquisition command verifies them before
creating the pinned compiler input, while releases consume only that pinned file.

Baseline mode also reads and validates
`data/source-compiler-generated-order-attestation.json`. That compact input
requires the exhaustive generated candidate universe to close with zero
source-only, qualified-only, reverse-only, rank or collision gaps, and records
the reviewed source-native scheduling deltas. Its `releaseGate` is mandatory and
pins both analyzer sections on both sides together with every artifact count. A
mismatch in one byte identity or one count hard-fails; there is no section-name
allowlist. The attestation is the sole numeric source of truth for these section
identities and count groups. Tests validate its schema and closure invariants
without copying those values into a second fixture or table.

```sh
bun run source:release -- baseline \
  --out work/m6-source-release \
  --pack-version ichiran-260118-source
```

The command requires a clean checkout. There is no release option that can
bypass this rule: the manifest's full 40-character commit describes the code
and tracked inputs, while the verified source-lock digest describes every
pinned external input. The command checks the checkout before work, then checks
the same commit and clean state again immediately before atomically activating
the finished generation. Full releases use Bun's `--smol` mode so garbage
collection bounds the compiler's large transient object graph. The
separate `source:release:isolated` command applies the same mode automatically.
The two
determinism-check surface-index builds run sequentially so their multi-million-
row Rust working sets do not overlap. The compiler also finishes the surface
TSV and releases the physical-target graph before binary pack assembly; the
phase boundary changes object lifetime, not the surface bytes or pack format.

## PostgreSQL-unavailable proof

The package's default test command also succeeds on a machine with no database
configuration:

```sh
env -u ICHIRAN_DB_URL -u DATABASE_URL ICHIRAN_RUN_DATABASE_TESTS=false \
  bun test packages/data/tests
```

At the qualification checkpoint this runs 138 tests and skips 12 explicit
database cases. Nine are legacy load/conjugation tests that run only when
`ICHIRAN_RUN_DATABASE_TESTS=true`; three are separately invoked exhaustive
PostgreSQL-oracle comparisons. To prove the legacy coverage remains available,
run the same suite against the test database:

```sh
ICHIRAN_RUN_DATABASE_TESTS=true \
  ICHIRAN_DB_URL='postgresql:///ichiran_test?host=%2Fvar%2Frun%2Fpostgresql' \
  bun test packages/data/tests
```

That run passes 147 tests and skips only the three exhaustive oracle cases.
These test modes do not replace the isolated full release proof below.

The focused import test rejects any attempt to resolve the PostgreSQL oracle
package or the PostgreSQL client from the source release module graph:

```sh
bun test packages/data/tests/source-compiler-release-evidence.test.ts \
  --test-name-pattern 'reference modules are blocked'
```

For the milestone gate, run the complete baseline through the Linux-only
isolation wrapper. It creates an unprivileged user, mount and network namespace.
An empty bind mount hides the host's PostgreSQL Unix socket directory, while the
new network namespace starts with loopback down. The wrapper proves both
transports are unavailable on ports 5432 and 5433, clears database connection
environment variables, and only then starts the compiler:

```sh
bun run source:release:isolated -- baseline \
  --out work/m6-source-release-no-postgres \
  --pack-version ichiran-260118-source
```

This is the final availability proof, not merely an invalid connection string:
neither a local socket nor a network route exists inside the build namespace.
The wrapper never stops or reconfigures the host PostgreSQL service. Its mount
and network changes disappear when the child exits. A low-cost capability probe
can be run without starting the compiler:

```sh
bun run source:release:isolated -- --probe-only
```

The command still requires all non-PostgreSQL build dependencies to have been
installed before entering the network namespace.

After building the isolated baseline, run the complete 1,241-operation
chosen-authority corpus and the independent 301-operation fallback comparison as
documented in `M6-PACKED-PARITY-AUDIT.md`. Every non-exact observation must have
one concrete review row; the diagnostic report must retain an empty runtime
allowlist. This corpus audit is separate from release generation, so PostgreSQL
remains an oracle and never becomes a pack input.

## Genuine 2026-01-02 JMdict update

The qualified transition is the first official daily JMdict snapshot after the
January 1 baseline. It changes only JMdict. The lock keeps Kanjidic2, custom sources,
chronological errata, and the reviewed compatibility ledger identical to the
qualified baseline. It pins the deterministic gzip and uncompressed XML identities,
the `JMdict created: 2026-01-02` header, the EDRDG URL and license, and the
Jitendex archive commit containing the exact January 2 daily patch.

Acquire the official snapshot while the authoritative URL has the pinned bytes:

```sh
bun scripts/acquire-source-compiler-update-2026-01-02.ts
```

The acquisition command downloads and verifies the 3,086-byte historical patch,
applies it to the pinned January 1 JMdict, and verifies the patch payload,
uncompressed XML, deterministic gzip and creation header before exclusively
creating the ignored file `work/m6-transition/JMdict_e-2026-01-02.gz`. The
historical authority is Jitendex archive commit
`fbc4afb4c786b7f4c304c173a475553279bbb528`, patch
`JMdict_e/patches/2026/01/02.patch.br`.

Build the updated release through the same compiler:

```sh
bun run source:release -- update \
  --source-lock data/source-compiler-update-2026-01-02.lock.json \
  --out work/m6-update-release \
  --pack-version jmdict-2026-01-02-source
```

Update mode deliberately does not compare its changed dictionary bytes to the
January artifact. It still verifies all pinned inputs, rebuilds every output
twice where representation is encoded, enforces release size limits, verifies
the staged pack, and records every section digest and count in `stats.json`.
The CLI requires an explicit update lock. That verified lock names the one
JMdict file and identity used by the compiler; the path and identity cannot be
overridden separately. Update-versus-baseline is decided from the pinned file's
SHA-256, so copying or renaming the January bytes cannot turn off comparison.

A later September 1 snapshot was also tested as a transition candidate. Its
source semantics compiled, but its installed hot pack was 25,273,024 bytes,
107,200 bytes above the unchanged 24 MiB product gate. Publication correctly
stopped. The gate was not raised and pack format v1 was not changed; qualifying
the first genuine post-baseline daily update keeps the transition focused while
leaving that later capacity issue for a separately reviewed product decision.

Before the full build, run the bounded semantic witness:

```sh
bun scripts/source-compiler-update-witness.ts
```

It verifies the complete transition lock, proves that seq 2868547 is absent
from the January 1 JMdict and present in the January 2 source as `パオーン`, and
passes that canonical entry through the real surface, root-payload and detail
encoders twice. The command is a low-memory source/encoder check; it is not a
substitute for the complete release.

The final update gate builds the same clean commit twice into independent
release roots:

```sh
bun run source:release -- update \
  --source-lock data/source-compiler-update-2026-01-02.lock.json \
  --out work/m6-update-release-a \
  --pack-version jmdict-2026-01-02-source

bun run source:release -- update \
  --source-lock data/source-compiler-update-2026-01-02.lock.json \
  --out work/m6-update-release-b \
  --pack-version jmdict-2026-01-02-source
```

Compare the active generation inventories and every published payload byte.
`manifest.json` and `stats.json` intentionally include the output pack version,
so use the same `--pack-version` in both commands as shown above:

```sh
release_a=$(readlink -f work/m6-update-release-a)
release_b=$(readlink -f work/m6-update-release-b)

(cd "$release_a" && find . -maxdepth 1 -type f -printf '%f\0' \
  | sort -z | xargs -0 sha256sum) > /tmp/ichiran-update-a.sha256
(cd "$release_b" && find . -maxdepth 1 -type f -printf '%f\0' \
  | sort -z | xargs -0 sha256sum) > /tmp/ichiran-update-b.sha256
diff -u /tmp/ichiran-update-a.sha256 /tmp/ichiran-update-b.sha256
```

An empty diff is the two-release determinism proof. Both runs must record the
same clean 40-character source commit and the same verified update-lock digest.
