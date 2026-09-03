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
Baseline mode always uses that tracked lock and rejects `--source-lock`; only update
mode accepts—and requires—an explicit transition lock.

A fresh clone acquires that comparison pack directly from the immutable GitHub
release. The acquisition command pins the checksum-index identity, then verifies
the manifest, hot pack, details pack and producer statistics named by that index:

```sh
bun scripts/acquire-qualified-source-compiler-baseline.ts work/m2-baseline
```

An existing directory is verified in place; it is never silently refreshed. A
missing directory is staged, verified and renamed into place only after all five
published files pass. This makes `work/m2-baseline` a reproducible cache rather
than an undeclared prerequisite. Release comparison reads each of the four named
artifacts once, verifies those captured buffers, and parses only those same buffers;
replacing the ignored cache after capture cannot change the comparison authority.

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

Release compilation requires Bun 1.3.5 and a rustup installation that provides
Rust 1.92.0. These are the pinned prerequisites on Linux, macOS, and WSL; native
Windows is not supported. The root wrapper rejects any other Bun version before
building, and the private surface-compiler build fails if `cargo +1.92.0` is absent.

The command requires a clean checkout. The root wrapper captures HEAD before building,
checks the same clean HEAD after building, and passes that commit to the built CLI; the
CLI requires that matching internal launch value. Direct CLI/dist invocation is
unsupported; compiler provenance comes from the wrapper's mandatory fresh build and
same-clean-HEAD checks, not from a self-identifying compiled binary.
There is no release option that can
bypass this rule: the manifest's full 40-character commit describes the code
and tracked inputs, while the verified source-lock digest describes every
pinned external input. The command checks the checkout before work, then checks
the same commit and clean state again immediately before atomically activating
the finished generation and after the CLI returns. Each locked source is copied from
the bytes verified against the lock into the private release temporary directory, and
every parser consumes that immutable snapshot. The Rust surface compiler is built with
toolchain 1.92.0 into a fresh target directory under the same private temporary root.
Full releases use Bun's `--smol` mode so garbage
collection bounds the compiler's large transient object graph. The
separate `source:release:isolated` command applies the same mode automatically.
The two
determinism-check surface-index builds run sequentially so their multi-million-
row Rust working sets do not overlap. The compiler also finishes the surface
TSV and releases the physical-target graph before binary pack assembly; the
phase boundary changes object lifetime, not the surface bytes or pack format.
The root command first performs a TypeScript-only full package build of
`@ichiran/core`, copying the already generated and separately reproducibility-checked
WASM, then builds the private `@ichiran/data` compiler. It does not run the Rust/WASM
toolchain or import `@ichiran/reference-postgres`, and it leaves the core production
entry point usable. The built compiler runs from and stamps that same clean repository
root. It supports Linux/macOS and WSL; native Windows is not supported because the
surface compiler and atomic release activation use POSIX facilities.
The output must be absent or an atomic symlink. Its sibling `.generations` root must
be a real directory, never a symlink, and published artifacts must be regular
non-symlink files. Cooperating identical first publications remain idempotent, but
the caller exclusively owns the output path, its `.generations` root, and all ancestors
throughout publication. First activation never overwrites an existing path, and state
changes observed immediately before replacement are rejected. The source compiler also
compares the initial lexical and physical output identity immediately before the
publisher creates its generation root or writes bytes, so even a swapped `work/`
ancestor that still resolves elsewhere under `work/` is rejected.

## PostgreSQL-unavailable proof

The package's source test command explicitly excludes the five database/reference
files and runs every other data test on a machine with no database configuration:

```sh
bun run --cwd packages/data test:source
```

At the integration checkpoint this runs 160 tests with no skip. The excluded files
contain nine legacy load/conjugation cases and three exhaustive PostgreSQL-oracle
comparisons. To prove that migration coverage remains available, run the complete
suite against the test database:

```sh
ICHIRAN_RUN_DATABASE_TESTS=true \
  ICHIRAN_DB_URL='postgresql:///ichiran_test?host=%2Fvar%2Frun%2Fpostgresql' \
  bun test packages/data/tests
```

The three exhaustive oracle cases retain their separately named opt-in variables.
These test modes do not replace the isolated full release proof below.

The focused import test rejects any attempt to resolve the PostgreSQL oracle
package or the PostgreSQL client from the source release module graph:

```sh
bun test packages/data/tests/source-compiler-release-evidence.test.ts \
  --test-name-pattern 'reference modules are blocked'
```

For the milestone gate, run the complete baseline through the Linux-only
isolation wrapper. It creates an unprivileged user, mount and network namespace.
An empty bind mount hides `/run/postgresql` (and its `/var/run` alias), an owned
disk-backed directory under `/var/tmp` is bind-mounted as the private mode-1777
`/tmp`, and the new network namespace starts with loopback down. The wrapper verifies
that ports 5432 and 5433 have no sockets in any of those locations, clears database
connection environment variables, and only then starts the compiler:

```sh
bun run source:release:isolated -- baseline \
  --out work/m6-source-release-no-postgres \
  --pack-version ichiran-260118-source
```

This is the final availability proof, not merely an invalid connection string:
neither a host local socket nor a network route exists inside the build namespace,
while large compiler spools and the fresh Cargo target remain on the private
disk-backed `/tmp` bind rather than consuming RAM. Because that owned directory is
removed after the namespace exits, the isolation wrapper rejects a
repository physically located under `/tmp` and any normalized or symlink-resolved
`--out` destination under `/tmp`. Use the checkout's ignored `work/` directory or a
persistent external path instead.
The `/var/tmp` backing filesystem must permit executable files; the namespace wrapper
creates and runs a tiny private probe before reporting capability or starting a build,
so a `noexec` host fails immediately rather than at the later Rust compiler launch.
The wrapper never stops or reconfigures the host PostgreSQL service. Its mount
and network changes disappear when the child exits, and it removes only the exact
private `/var/tmp/ichiran-source-private-tmp.*` directory that it created. A low-cost capability probe
can be run without starting the compiler:

```sh
bun run source:release:isolated -- --probe-only
```

The capability probe performs no build. A full release requires dependencies to have
been installed before entering the network namespace; its core/data build preparation
then runs inside that namespace, so future build hooks cannot silently escape the
PostgreSQL-unavailable proof.

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
107,200 bytes above the former 24 MiB product gate. Publication correctly stopped.
The September 2 product decision raised the hot limit by one MiB, from 24 to 25 MiB,
and the complete first-install limit by one MiB, from 25 to 26 MiB. The 64 MiB
ready-state persisted limit is unchanged. These are explicit capacity decisions for
the current dictionary, not a weakening or removal of the release gates.

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

The publisher already verifies the exact active generation inventory. Compare every
named published payload byte. `manifest.json` and `stats.json` intentionally include
the output pack version, so use the same `--pack-version` in both commands as shown:

```sh
set -eu
release_a=work/m6-update-release-a
release_b=work/m6-update-release-b
for artifact in manifest.json hot.bin.gz details.bin.gz stats.json; do
  cmp "$release_a/$artifact" "$release_b/$artifact"
done
```

The successful byte-comparison loop is the two-release determinism proof. Both runs
must record the same clean 40-character source commit and the same verified update-lock
digest.

## Current 2026-09-02 JMdict update

`data/source-compiler-update-2026-09-02.lock.json` pins the current English JMdict
snapshot. On 2026-09-02 the official EDRDG URL served a 10,565,350-byte gzip with
SHA-256 `20b8e5c25d1f3755422fe1f704dee703e64d4d2e0580f9b918c1073eadcd86d9`.
It expands to 63,077,282 bytes with SHA-256
`3ffd03dd326e2d2a35d307fcac3307a6dab3abd0818dde6cb2657962d3025196`,
contains 218,683 entries, and identifies itself as `JMdict created: 2026-09-02`.
The authoritative URL is `https://www.edrdg.org/pub/Nihongo/JMdict_e.gz`; the data
is attributed to EDRDG under CC BY-SA 4.0.

The lock also gives a durable reconstruction authority: Jitendex EDRDG archive
commit `3ad579211fc38f01048b2704d93974eff13372dd`, whose
`JMdict_e/patches/2026/09/02.patch.br` is 1,501 bytes with SHA-256
`1416eb49f8b6fd89034cd7263089badca141a158a51ec2c4d48fc32d28768f7f`.
The generic acquisition command reconstructs the target from that immutable archive,
verifies the expanded official XML identity, and writes a deterministic gzip. The
compiler input is 10,565,341 bytes with SHA-256
`7cd74020d4669eed9276fb34ba767c670be509db1d8fede59c92ddf1debb3c0a`.

```sh
bun scripts/acquire-source-compiler-jmdict.ts \
  data/source-compiler-update-2026-09-02.lock.json

bun run source:release:isolated -- update \
  --source-lock data/source-compiler-update-2026-09-02.lock.json \
  --out work/jmdict-2026-09-02-release-a \
  --pack-version jmdict-2026-09-02-source

bun run source:release:isolated -- update \
  --source-lock data/source-compiler-update-2026-09-02.lock.json \
  --out work/jmdict-2026-09-02-release-b \
  --pack-version jmdict-2026-09-02-source

bun run qualify:rust-same-pack -- work/jmdict-2026-09-02-release-a \
  --source-lock data/source-compiler-update-2026-09-02.lock.json
bun run qualify:native-same-pack -- work/jmdict-2026-09-02-release-a \
  --source-lock data/source-compiler-update-2026-09-02.lock.json
bun run qualify:source-hosts -- work/jmdict-2026-09-02-release-a \
  --source-lock data/source-compiler-update-2026-09-02.lock.json
bun run --cwd packages/browser-demo qualify -- \
  --release work/jmdict-2026-09-02-release-a \
  --source-lock data/source-compiler-update-2026-09-02.lock.json
```

Compare all four published artifacts exactly as above. Because JMdict content changed,
the frozen January PostgreSQL oracle cannot judge the changed rows. Qualification uses
source-lock verification, two-build byte identity, complete pack verification, Rust
versus frozen-TypeScript same-pack parity, native C same-pack coverage, and the Node,
CLI, HTTP, and browser gates. Any retained January behavioral fixture remains a
regression assertion for unaffected inputs; it is not an allowlist for dictionary data
changes.

One retained probe changes for a concrete source-data reason, recorded in
`data/source-compiler-update-2026-09-02-behavior.json`. JMdict entry 1859020 now
includes the kana form `一本とる`, so its passive-past surface `一本とられた` collides
with the unchanged expression entry 2268020. The selected entry, segmentation, and
score stay unchanged; the result now carries `Past (~ta) via Passive`. The regression
test accepts that value only for the exact September 2 source-lock digest.
