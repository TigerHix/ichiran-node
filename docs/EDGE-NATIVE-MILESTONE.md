# Edge-native analyzer milestone

Status: TypeScript alpha implementation complete for the pinned `ichiran-260118`
analyzer. Generated release and browser-benchmark evidence stays outside Git and is
reproduced by the qualification commands; physical-iPhone measurement remains
deferred.

This document is the as-built alpha contract. It does not define permanent
implementation ownership. The accepted post-alpha direction—one Rust kernel for
browser WASM, Node, and native iOS plus a PostgreSQL-free source compiler—is
authoritative in
[SOURCE-COMPILER-RUST-KERNEL-ROADMAP.md](./SOURCE-COMPILER-RUST-KERNEL-ROADMAP.md).

## Alpha product decision

Ship one analyzer implementation that runs locally in a browser Worker and in Node.js
from the same immutable data release. A phone user installs the complete release once;
after installation, analysis, romanization, dictionary details, and app restart work
without a network connection.

This milestone is a standalone mobile-first demo. Integration into Komi or Nemu is a
later product decision. The demo may borrow Nemu's useful interaction pattern—furigana,
surface text, POS, and tap-to-inspect details—but owns a simpler analyzer-only UI.

"Edge" here means the user's browser, not an edge Worker or hosted API. The browser
must not send analyzed text anywhere.

## Definition of done

The demo milestone is complete when all of the following are true:

- a static PWA installs one pinned analyzer release with visible progress, integrity
  verification, incomplete-install recovery, and an explicit clear/reinstall action;
- the PWA restarts with networking disabled and exposes segmentation, top-N paths,
  entity hints, romanization, full dictionary details, compounds, counters, suffixes,
  and conjugation information;
- all analyzer work runs in a dedicated Worker and none runs on the main thread;
- the browser, Node adapter, CLI, and API use the same `@ichiran/core` behavior;
- PostgreSQL, SQL clients, Node built-ins, and server calls are absent from the browser
  runtime and generated web bundle;
- the packed runtime passes the upstream, legacy, corruption, offline, artifact-size,
  and calibrated performance gates;
- the complete generated pack is published as a release artifact rather than committed
  to Git.

The first release has one immutable version. Automatic updates, deltas, migrations,
and multiple installed versions are deliberately deferred.

## Source target

The analyzer target is pinned rather than described as "latest":

| Source | Identity |
|---|---|
| Upstream Ichiran | `ea9583368e67cad22d94abae8dbcc8df96d99bcd` (`ichiran-260118-3-gea95833`) |
| Upstream tree | `5352f7641feaeeb1c3db04ea80ced31ca117dbe3` |
| Data release | `ichiran-260118` |
| Release dump | 200,012,956 bytes; SHA-256 `98a44e2cc88a65677da8b1f7124e7d6c904253eb1aae0ef16d2c7cc1dacdba82` |
| jmdictdb inputs | `02dc4aabd185a5b02c29fa6bc685bd78296741b3` |
| Frozen Node/PostgreSQL reference | `d583720572fbf26ee201166ac47034c50380a571` |

The qualified upstream Lisp checkout passes 782 / 782 analyzer assertions. Its
restored and errata-applied database has 214,700 root entries, 251,648 senses,
434,112 glosses, 407,620 sense properties, and 6,332 reading restrictions. These
numbers identify the compiler input; they are not final packed-release measurements.

The authoritative source metadata is captured in
`browser-alpha/upstream-oracle.json`. Final compiler counts and artifact digests belong
in the deterministic release lock and the release's own `stats.json`.

## Alpha architecture

```text
              build and qualification only

upstream Lisp + release dump + PostgreSQL
                    |
                    v
       @ichiran/reference-postgres
                    |
                    v
             @ichiran/data
                    |
         deterministic release
          /                   \
         v                     v
browser OPFS + IDB + Worker  @ichiran/node
         \                     /
          v                   v
                 @ichiran/core
          lookup -> candidates -> score -> top-N
```

`@ichiran/core` is the alpha product and executable migration oracle. It owns packed
readers, route-aware lookup, reverse morphology, suffix/counter/number construction,
analyzer rules, scoring, stable top-N selection, details, romanization, and legacy
serialization.

Hosts own only I/O:

- the browser adapter installs verified bytes in OPFS, keeps one per-install commit
  ID in IndexedDB to linearize cross-tab lifecycle changes, and serves random-access
  blocks to a dedicated Worker;
- `@ichiran/node` reads the same release from a directory, verifies it, decompresses
  it, and supplies the bytes to core;
- CLI and API are presentation/transport adapters over the Node host.

This boundary prevents four nearly-identical analyzers from drifting apart.

## Why this is not a database reskin

The former runtime asks PostgreSQL to discover dictionary rows and relies on a large
materialized conjugation closure. Replacing PostgreSQL with SQLite or embedding the
same tables would preserve the database-shaped hot path, ship substantial redundant
rows, and remain poorly matched to a phone browser.

The packed analyzer changes the representation and the runtime algorithm:

- a deterministic minimal byte automaton scans route-aware surface keys;
- compact root payloads store only analyzer-resident facts;
- reverse morphology reconstructs valid conjugations from canonical roots and rules
  rather than loading millions of generated entry rows;
- a small exception overlay preserves physical-member multiplicity, ordering, and
  properties where reconstruction alone would change observable behavior;
- analyzer support stores suffixes, counters, split/hint facts, and scoring aggregates
  in purpose-built indexed sections;
- complete senses and glosses live in a separately blocked detail store and are opened
  only when presentation needs them;
- the hot path works on numeric IDs and typed-array views instead of expanding the
  dictionary into object graphs.

The scoring formula, analyzer filters, pair synergies, and top-N semantics remain the
accepted Ichiran algorithm because parity is a product requirement. Retaining those
semantics is not the same as retaining the SQL implementation.

## Installed data

One release contains:

- `hot.bin.gz`: surface index, root payload, reverse morphology, resident support, and
  random-access analyzer annotations;
- `details.bin.gz`: complete dictionary forms, senses, glosses, and metadata;
- `manifest.json`: download and installed lengths and SHA-256 identities;
- `stats.json`: deterministic compiler, artifact, relation, and size evidence.

The installer downloads all analyzer and dictionary content needed by the demo. There
is no network fallback. The hot data is memory-oriented; details remain installed
offline but are decoded lazily.

The deterministic `ichiran-260118` assets are:

| Measure | Bytes | MiB |
|---|---:|---:|
| `hot.bin.gz` one-time download | 12,662,917 | 12.076 |
| `details.bin.gz` one-time download | 12,317,325 | 11.747 |
| resident `hot.bin` | 24,857,288 | 23.706 |
| installed `details.bin` | 13,555,874 | 12.928 |

Production-shell, complete first-install, and complete persisted totals are derived
from the final browser build and bound in the release's generated `stats.json`.
`manifest.json` and `stats.json` remain the source of truth; the data assets
themselves are release artifacts, not repository contents.

## Parity policy

Parity covers analyzer behavior, not merely top-one token boundaries:

- segmentation spans, alternatives, and integer scores;
- readings, kana, romanization, and punctuation normalization;
- dictionary entries, ordered senses and glosses, and metadata;
- compounds, counters, suffix descriptions, and components;
- conjugation paths, properties, descriptions, and `via` chains;
- entity hints, gaps, tie order, and top-N behavior;
- historical CLI info text and full legacy JSON structure.

The clean model uses canonical root identity plus an ordered semantic inflection path.
The legacy serializer preserves the historical presentation contract; any intentional
identity normalization is explicit in the acceptance comparator rather than hidden in
a broad exception list.

Qualification is three-way:

1. the pinned upstream Lisp suite and freshly captured CLI fixtures define the current
   source behavior;
2. the frozen PostgreSQL Node implementation identifies unintended migration drift
   during one transition cycle;
3. browser and Node execute the same packed core, so exact host-to-host comparison is
   expected rather than sampled equivalence.

Where a pinned current-Lisp snapshot exists, it is the release authority. The
PostgreSQL reference is diagnostic for those cases and cannot veto a current-Lisp
match. Counters, entity hints, and deterministic probes have no Lisp snapshot, so the
frozen reference remains their temporary detailed-and-clean fallback authority. The
portable result is never normalized through the database resolver: committed
canonical Lisp fixtures are compared directly, so a leaked generated sequence ID is
a release failure.

Known upstream changes from the older Node baseline are named regression cases, not an
allowlist. The upstream probes include seven corrected top-one segmentations, two former
JSON crash cases, and the `食べがたい` suffix result. A release is blocked by any
unexplained output difference.

## Performance contract

The design target is instant-feeling interaction on an iPhone 17 Pro Max and very fast
interaction on an iPhone 13-class device. Until physical-device testing is available,
the repeatable release proxy is a production Chromium Worker under calibrated CPU
contention.

| Gate | Requirement |
|---|---:|
| One-time compressed shell + data transfer | at most 26 MiB |
| Installed shell + analyzer data | at most 64 MiB |
| Always-resident hot image | at most 25 MiB |
| Ordinary top-one p95 at calibrated 6x contention | at most 75 ms |
| Pathological morphology p95 at calibrated 6x contention | at most 250 ms |
| Dense contiguous 64-256-unit top-1/5/10 p95 at calibrated 6x contention | at most 500 ms |
| Main-thread analyzer work | none |

The production qualification command writes raw samples and environment metadata to
`work/browser-benchmark.json`; timing samples are intentionally not committed because
they describe the machine running the gate, not the immutable analyzer artifact.
Physical iPhone 13-class validation remains a production gate when the device is
available.

## Post-alpha optimization direction

The packed architecture is the necessary foundation for deeper optimization because
it removes database latency and gives the analyzer compact, deterministic structures
with clear ownership. The earlier alpha plan treated focused WASM kernels as an
optional optimization. The accepted product direction now goes further: port the
complete analyzer kernel to Rust so browser WASM and native iOS share one semantic
implementation. That decision does not remove the requirement to measure the boundary
before browser cutover.

Likely next steps, in order of evidence:

1. Profile full Worker requests on the final pack, separating open, lookup,
   morphology, scoring/path search, detail decode, and serialization.
2. Reduce allocations and fuse candidate construction with scoring where profiles show
   object churn or repeated decoding.
3. Profile the remaining adjacent pair-rule resolver and frontier bookkeeping. The
   default kernel already uses an exact three-frontier sweep for non-adjacent
   predecessors; adjacent pairs retain the complete rule resolver, while custom
   initial/transition hooks intentionally use the exhaustive fallback.
4. Revisit block boundaries, integer widths, compression, and resident-versus-lazy
   placement using real phone memory and decode traces.
5. Port differential slices to Rust, keeping browser installation and asynchronous I/O
   in TypeScript; cut over only when boundary copying, startup, memory, and final code
   size pass the release gates.

Other viable work includes automaton-guided scanning that avoids temporary substrings,
precomputed transitions for common adjacent analyzer-rule cases, compact arenas for
candidates, and persistent Worker reuse. Every change must reproduce the same ordered
results and scores before it can replace the current exact implementation.

WASM is the browser compilation target for the shared Rust kernel, not a database
strategy and not an assumed speedup. A database compiled to WASM would still be the
wrong data model.

## Grammar and Kanjidic boundary

The separate `@ichiran/grammar` package is experimental and excluded. The analyzer's
own suffix DSL, segmentation filters, penalties, and synergies remain in core because
removing them changes segmentation and scores. `POST /v1/analyze` returns the analyzer
result directly; it has no grammar placeholder or implied grammar parity.

Kanjidic is used only during compilation by the legacy `matchReadings`/easy-hint path.
The compiler resolves the needed reading facts and stores those results in the analyzer
pack. No current Komi or Nemu integration path was found to consume a general Kanjidic
runtime through this analyzer, and the standalone demo does not expose one. If a future
product needs character metadata,
that should be a separately scoped dataset and API rather than hidden analyzer weight.

## PostgreSQL lifecycle

PostgreSQL is build infrastructure, never a product dependency.

During this migration cycle:

- `@ichiran/reference-postgres` remains private and frozen at the old Node analyzer;
- `@ichiran/data` may reuse its database/data-authoring code to compile a release;
- upstream and differential qualification may query the read-only database;
- normal core, browser, Node, CLI, and API use no database.

The old implementation can be deleted after compiler-owned projections no longer
import its internals and upstream Lisp plus packed-runtime tests cover the required
behavior independently. That cleanup removes code rather than wrapping it behind a
permanent repository interface.

The next compiler milestone will ingest pinned source XML/CSV and ordered compatibility
data directly, then remove PostgreSQL from normal release builds. This is independent
of the already-complete runtime removal and may proceed in parallel with the Rust
kernel.

## Explicit non-goals

- experimental grammar analysis;
- a general Kanjidic character service;
- Komi or Nemu integration in this milestone;
- server-side or edge-Worker analysis;
- SQL emulation or an embedded general-purpose database;
- automatic updates, delta packs, or multiple installed versions;
- an unmeasured browser WASM cutover;
- claiming final phone performance from desktop microbenchmarks.
