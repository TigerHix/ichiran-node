# Edge-native analyzer milestone

Status: implementation complete for the pinned `ichiran-260118` analyzer. Generated
release and browser-benchmark evidence stays outside Git and is reproduced by the
qualification commands; physical-iPhone measurement remains deferred.

## Product decision

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

## Architecture

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
browser OPFS + Worker      @ichiran/node
         \                     /
          v                   v
                 @ichiran/core
          lookup -> candidates -> score -> top-N
```

`@ichiran/core` is the product. It owns packed readers, route-aware lookup, reverse
morphology, suffix/counter/number construction, analyzer rules, scoring, stable top-N
selection, details, romanization, and legacy serialization.

Hosts own only I/O:

- the browser adapter installs verified bytes in OPFS and serves random-access blocks
  to a dedicated Worker;
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
| production shell | 618,844 | 0.590 |
| complete first-install transfer | 25,600,013 | 24.414 |
| complete persisted installation | 39,033,822 | 37.226 |

The release's generated `manifest.json` and `stats.json` remain the source of truth;
the data assets themselves are release artifacts, not repository contents.

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
| One-time compressed shell + data transfer | at most 25 MiB |
| Installed shell + analyzer data | at most 64 MiB |
| Always-resident hot image | at most 24 MiB |
| Ordinary top-one p95 at calibrated 6x contention | at most 75 ms |
| Pathological morphology p95 at calibrated 6x contention | at most 250 ms |
| Main-thread analyzer work | none |

The production qualification command writes raw samples and environment metadata to
`work/browser-benchmark.json`; timing samples are intentionally not committed because
they describe the machine running the gate, not the immutable analyzer artifact.
Physical iPhone 13-class validation remains a production gate when the device is
available.

## Is this the final optimization ceiling?

No. This architecture is the necessary foundation for deeper optimization because it
removes database latency and gives the analyzer compact, deterministic structures with
clear ownership. It intentionally leaves room for measured improvements without
weakening parity.

Likely next steps, in order of evidence:

1. Profile full Worker requests on the final pack, separating open, lookup,
   morphology, scoring/path search, detail decode, and serialization.
2. Reduce allocations and fuse candidate construction with scoring where profiles show
   object churn or repeated decoding.
3. Specialize the path search from the current parity-first transition scan toward an
   interval/sweep top-N algorithm where pair-dependent rules permit it.
4. Revisit block boundaries, integer widths, compression, and resident-versus-lazy
   placement using real phone memory and decode traces.
5. Move only proven CPU kernels to WASM, and keep JavaScript when boundary copying,
   startup, or code size erases the gain.

Other viable work includes automaton-guided scanning that avoids temporary substrings,
precomputed transitions for common analyzer-rule cases, compact arenas for candidates,
and persistent Worker reuse. Every change must reproduce the same ordered results and
scores before it can replace the parity-first implementation.

WASM is therefore an optimization option, not the architecture. A database compiled to
WASM would still be the wrong data model; focused WASM kernels may be useful after
profiling.

## Grammar and Kanjidic boundary

The separate `@ichiran/grammar` package is experimental and excluded. The analyzer's
own suffix DSL, segmentation filters, penalties, and synergies remain in core because
removing them changes segmentation and scores. `/api/analyze` returns analyzer output
with `grammars: {}` and `grammarExcluded: true` instead of pretending grammar parity.

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

A later compiler may ingest source XML/CSV or a normalized export directly and remove
PostgreSQL from the build too. That is useful maintenance work, but it is not required
to make the shipped product self-contained.

## Explicit non-goals

- experimental grammar analysis;
- a general Kanjidic character service;
- Komi or Nemu integration in this milestone;
- server-side or edge-Worker analysis;
- SQL emulation or an embedded general-purpose database;
- automatic updates, delta packs, or multiple installed versions;
- mandatory WASM before profiling;
- claiming final phone performance from desktop microbenchmarks.
