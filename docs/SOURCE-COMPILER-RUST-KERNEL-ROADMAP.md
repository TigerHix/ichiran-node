# Source compiler and Rust kernel roadmap

Status: integrated implementation and qualification recipe complete; exact-head
Linux/WSL qualification pending. Last updated 2026-09-02.

This document is authoritative for work after the completed TypeScript browser alpha.
The browser-alpha documents remain the as-built specification and qualification record
for that baseline.

## Integrated checkpoint

The two reviewed workstreams are integrated on `codex/integrated-edge-cutover`.
The qualified revision will be the immutable commit named by the external final
handoff after all gates pass and that exact commit is pushed. No tracked document or
floating branch head identifies itself as qualified. The candidate builds a
PostgreSQL-isolated source release and runs the canonical Rust kernel and native C ABI
on that exact pack. The qualification recipe and accepted identities are in
[`INTEGRATED-EDGE-CUTOVER-REPORT.md`](./INTEGRATED-EDGE-CUTOVER-REPORT.md).

| Milestone | Status |
| --- | --- |
| M0 immutable baseline | PASS |
| M1/M3 Rust feasibility and full semantics | PASS |
| M4 Linux/Chromium browser cutover | IMPLEMENTED; FINAL QUALIFICATION PENDING |
| M4 physical Safari/iPhone | PENDING |
| M4N Node/CLI/API cutover | IMPLEMENTED; FINAL QUALIFICATION PENDING |
| M5A Linux/WSL C ABI and Mac handoff | IMPLEMENTED; FINAL QUALIFICATION PENDING |
| M5B Apple packaging, Swift, simulator, and device | PENDING |
| M6 PostgreSQL-free source compiler and same-pack Rust gate | IMPLEMENTED; FINAL QUALIFICATION PENDING |

Rust now owns production analyzer semantics. TypeScript owns host adapters, browser
installation/lifecycle, release verification, and the source compiler. The frozen
TypeScript and PostgreSQL analyzers remain qualification-only for the transition
release. They are not two production analyzers and must not acquire new runtime
callers.

The canonical `@ichiran/core` entry point exposes the Rust facade and public/shared
data contracts. The post-cutover product API is the small `Analyzer` contract in
[`MIGRATION.md`](../MIGRATION.md); historical compatibility surfaces described by
the browser-alpha record are not product exports. TypeScript analyzer execution is confined to the explicit
qualification subpath. The canonical `@ichiran/data` entry point and executable are
the source compiler; the old PostgreSQL loader is private migration tooling. Direct
compilation and the Linux namespace isolation proof are separate commands. The
fresh-pack qualification recipe covers Rust/WASM, the C ABI, Node, CLI, API, and the
production browser Worker; its exact-head final run remains pending.

## Decision

The final product has one analyzer kernel implemented in Rust and one versioned packed
data format:

```text
pinned JMdict + Kanjidic2 + CSV/custom data + ordered errata
                              |
                              v
                TypeScript source-data compiler
                              |
                    deterministic packed release -------------------+
                                                                    |
 Rust analyzer crate                                                |
       |                                                            |
       +--> WASM + thin TypeScript host --> browser Worker/PWA <----+
       |                                                            |
       +--> WASM Node adapter -----------> Node / CLI / API <--------+
       |                                                            |
       +--> native static library --------> C ABI / Swift <----------+
```

The compiler remains TypeScript because it runs only during data production and its
existing binary writers are qualified. Rewriting it in Rust would not improve an
on-device product. The runtime kernel moves to Rust because browser WASM and native iOS
are both intended product targets and should not maintain separate analyzer semantics.

This is a one-way, parity-gated replacement:

- the current TypeScript kernel remains frozen as an executable oracle until Rust is
  qualified;
- the PostgreSQL-backed producer/query/reference path remains frozen as a data oracle
  until the source compiler is qualified; `@ichiran/data` and the pack encoders remain
  active implementation code;
- upstream Lisp remains a behavior/fixture authority during updates, not a product or
  normal release-build dependency;
- grammar remains separate and out of scope.

No current implementation is deleted merely to make the architecture look finished.
Each reference is retired only after its replacement has independently passed the
required gates.

## Product intent

The end state is:

- full analyzer behavior on the user's device with no server lookup;
- one complete, integrity-verified installation that remains usable offline;
- the same immutable pack in browser, native iOS, and Node;
- one canonical Rust implementation of lookup, morphology, scoring, top-N selection,
  romanization, details, and analyzer-owned suffix/counter/split behavior;
- a deterministic source compiler that does not require PostgreSQL;
- exact current-Ichiran behavior unless a named, reviewed change intentionally replaces
  accidental database ordering;
- no experimental grammar port and no general Kanjidic runtime API.

The standalone browser is the first reference host. This Windows/WSL workstream will
deliver the qualified Rust kernel, versioned C ABI contract, C-boundary tests, and an
iOS integration handoff. XCFramework/Swift packaging and a validation app belong to a
Mac agent with Xcode; Komi and Nemu integration remain later product work.

## Qualified baseline

The migration begins from a working product rather than an inferred specification.

The qualified TypeScript artifact commit is
`29ec534ede2b4c90dcddb18f87a84089c24df9de`. It is published on branch
`portable-core-260118` and annotated tag `portable-core-260118-baseline`. Two independent
clean-tree release builds reproduced every locked component digest and the complete
9,173,122-key morphology relation. Both builds produced generation
`b4d958a390b77e458d14a6ecdbdb42921a22ba1815f5cb67708a88586a0ce38f` and all four
release files were byte-identical. The files and qualification evidence are published
in the [immutable baseline release](https://github.com/TigerHix/ichiran-node/releases/tag/portable-core-260118-baseline).

| Evidence | Accepted result |
|---|---:|
| Default product tests | 159 passed; 10 opt-in skips; 3,800 assertions; zero failures |
| Compiler unit tests | 36 passed; 464 assertions; zero failures |
| Package parity | 139 passed; one deliberate scorer skip; 3,703 assertions; zero failures |
| Frozen PostgreSQL reference suite | 824 passed; two documented JMdict-version skips; 881 assertions; zero failures |
| Current-Lisp comparisons | 1,241 / 1,241 exact |
| Frozen PostgreSQL fallback comparisons | 301 / 301 exact |
| PostgreSQL scorer differential | 1,297 assertions passed |
| Exhaustive PostgreSQL/cache witness | 390,582 assertions passed |
| Browser E2E | 11 / 11 passed |
| Morphology relation | 9,173,122 legacy keys; zero packed-only or duplicate keys |
| Ordinary browser p95 at calibrated 6.01x contention | 33.8 ms |
| Pathological morphology browser p95 | 96.5 ms |
| Dense-boundary browser p95 | 112.5 ms |
| Random-access details p95 | 65.7 ms |
| Worker ready / first analysis | 696.8 ms / 53.1 ms |
| Resident hot image | 24,857,288 bytes |
| Lazy detail store | 13,555,874 bytes |
| One-time shell and data total | 25,662,818 bytes |
| Persisted logical total | 39,096,725 bytes |

The complete commit-bound matrix, raw logs, oracle report, and browser benchmark are
sealed in qualification evidence archive SHA-256
`0c2542a6ecd3f61c917211b8aae8f6e0c83c0177bdbe73d5b32dfe3a436045fa`. Locked
component hashes and counts remain owned by
[`browser-alpha/sources.lock.json`](../browser-alpha/sources.lock.json). The benchmark
used single-core calibrated contention in headless Chromium under WSL; it is a stable
regression gate, not a claim of physical iPhone measurement. Safari testing on the
iPhone 13 baseline and current target device remains open for M4/M5B.

This roadmap-status update is intentionally a docs-only descendant of the tagged
artifact commit. It does not change the pack or move the baseline tag.

The Rust analyzer crate, browser/Node WASM host, C ABI, and source-native compiler now
exist in the integrated implementation. Their final exact-head Linux/WSL qualification
is pending. Swift/XCFramework packaging and physical Apple validation do not yet exist
and remain M5B work.

## Dependency boundary

| Stage | Transition baseline | Integrated candidate |
|---|---|---|
| Browser analysis | TypeScript core + pack | Rust WASM kernel + source-built pack |
| Native iOS analysis | Not implemented | Native Rust kernel + same pack; M5B pending |
| Node/CLI/API analysis | TypeScript core + pack | Rust through the same WASM artifact |
| Normal pack build | PostgreSQL + frozen reference TypeScript | Pinned sources + TypeScript-owned compiler and deterministic tools |
| Migration qualification | PostgreSQL reference + recorded/current Lisp evidence | Frozen fixtures plus both kernels for this transition release only |
| Upstream behavior update | Lisp and its database used as an external authority | Same external authority when needed; no normal build dependency |

PostgreSQL and Lisp are already absent from browser, Node, CLI, and API runtime paths.
Remaining PostgreSQL work is limited to maintaining and then retiring the frozen
transition oracle; it is not part of normal compilation or release production.

## Ownership

### Rust kernel

Rust owns all analyzer behavior that must agree across hosts:

- strict pack, section, index, checksum, and compressed-block readers;
- surface scanning and root lookup;
- reverse morphology and generated physical-member behavior;
- suffixes, counters, numbers, splits, hints, and analyzer-internal rules;
- scoring, stable candidate ordering, and exact top-N path search;
- clean result projection, romanization, details, and any retained legacy serializer.

The platform-neutral crate must not perform browser, Swift, Node, network, or release
installation work.

### TypeScript browser host

TypeScript remains responsible for:

- the React UI and Worker RPC/lifecycle;
- manifest download, hashing, and release compatibility;
- A/B OPFS installation and IndexedDB commit identity;
- Web Locks, Service Worker behavior, update recovery, and offline UX;
- streaming decompression of the outer release files.

The Worker will call Rust in coarse operations. It must not marshal individual
candidates or dictionary objects across the JavaScript/WASM boundary.

### Native iOS host

The native adapter will expose a small C ABI over the same Rust crate. Swift owns pack
download/bundling, atomic installation, version presentation, and calls off the main
actor. The initial boundary should use opaque handles and one owned UTF-8 result buffer
per operation; Swift can decode that buffer with `Codable`.

iOS should link native Rust rather than embed a WASM runtime. Browser WASM and native
iOS share source and semantics, not necessarily the same host ABI.

### TypeScript source compiler

The compiler owns:

- locked acquisition and validation of every raw input;
- direct JMdict/custom-data parsing and chronological errata application;
- best-reading derivation;
- primary and secondary conjugation generation and physical-target reuse;
- source-native suffix, counter, split, hint, collision, and generated-member facts;
- explicit stable ordering and provenance;
- the existing deterministic binary encoders and release publication.

The permanent compiler boundary is pack-ready semantic input, not an in-memory clone
of PostgreSQL tables. Use direct in-memory structures by default and bounded
streams/spools where measured volume requires them. Do not create a generic repository
interface, ORM, query language, or provider framework.

## Contracts that must not drift

### Pack contract

The Rust port initially reads the existing format and current pack unchanged. A format
redesign during the kernel port would make failures ambiguous between encoding,
decoding, and analyzer behavior. Any later format change is an explicit version bump
with both old and new readers covered during its transition.

### Text and ordering contract

Observable offsets and lengths currently use JavaScript UTF-16 code units. Rust must
preserve that behavior, including supplementary characters and malformed-surrogate
fixtures, rather than silently replacing it with Unicode-scalar or UTF-8-byte offsets.

JavaScript `Map`, `Set`, and stable sort behavior is also observable in ties. Rust hash
map iteration must never define output. Ordered vectors, explicit ordinals, or stable
semantic sorts own all observable traversal.

### Host boundary

The browser should pass UTF-16 input and receive one serialized output buffer per
operation. The 24.9 MiB hot image may live in WASM linear memory, but JavaScript must
release its temporary copy. Memory must be reserved deliberately because WASM linear
memory cannot shrink.

Details remain random-access. On a cache miss, Rust should return a small requested
range; the TypeScript host reads the range asynchronously, supplies it, and retries.
Long-lived synchronous OPFS access handles are an optional measured optimization, not
the initial architecture.

### Performance and size contract

Rust is not presumed faster in the browser. The existing TypeScript Worker already
passes its calibrated performance gates. Rust must earn cutover through measurement.

The current one-time total is 25,662,818 bytes against a 26,214,400-byte gate, leaving
551,582 bytes. The present JavaScript Worker is about 178 KiB and would be replaced,
but a WASM module plus glue may still exceed the remaining allowance. The first spike
must measure raw finalized-shell bytes, startup, transient memory, steady memory, and
latency; CDN compression is not a substitute for the release's measured contract.

## Source-data prerequisite

The checked-in `packages/data/JMdict_e.gz` cannot reproduce the current release:

- it identifies itself as the 2025-10-13 JMdict;
- the qualified `ichiran-260118` database records the 2026-01-01 JMdict;
- 1,480 shared original entries differ;
- 50 checked-in XML entries are absent from PostgreSQL;
- PostgreSQL retains 1,019 roots absent from the checked-in XML;
- after custom data and errata, 2,105 shared detail entries differ.

The present source lock hashes the October file even though the PostgreSQL compiler
does not consume it. That proves file co-location, not pack provenance. Kanjidic2 is
also not yet pinned outside the database, although a reading subset is used to compile
easy hints.

Before source-built parity can be claimed, choose and document one path:

1. recover and pin the exact January 1 JMdict and matching Kanjidic2 source; or
2. pin the best available raw sources plus small, explicit compatibility ledgers for
   historical roots, custom sequence identities, and direct tie order.

A query-shaped database export is not the final source format. Compatibility data must
name the behavior it preserves and remain reviewable; it cannot become a broad opaque
allowlist.

## Milestones and gates

### M0 — Qualified TypeScript baseline

Status: complete. Qualification, push, annotated tag, independently verified asset
upload, and immutable GitHub Release all passed. Physical-device checks belong to M4
and M5B, not this entry gate. M1 and M2 may start.

- reviewed source committed;
- current browser/Node runtime qualified;
- first committed clean-tree release reproduced all locked sections;
- publish the exact `manifest.json`, `hot.bin.gz`, `details.bin.gz`, and `stats.json` at
  a durable immutable location before either port depends on the local ignored build;
- roadmap committed, branch/tag pushed, two clean builds compared byte for byte, and
  the commit-bound verification matrix and evidence archive completed;
- exact release assets and qualification evidence published under the baseline tag
  with GitHub reporting `immutable: true`.

### M1 — Rust feasibility spike

Deliver a narrow vertical slice, not a scaffold-only crate:

- native and WASM builds from one host-neutral crate;
- pack/header/CRC reader and surface automaton over the real release;
- UTF-16 normalization/scanning fixtures, including astral and lone-surrogate cases;
- one direct lexical result and one morphology result through a bulk JSON boundary;
- generated-block inflation and one lazy detail read;
- browser Worker integration without changing installer ownership;
- draft versioned C header, integration README, and Linux C harness over the same
  host-neutral crate;
- measured WASM bytes, open time, transient/steady memory, and representative latency.

Gate: exact slice parity, no fine-grained JS/WASM calls, no eager detail-store load,
credible peak memory, and an explicit decision on any size/performance regression.
Rust need not beat the current JavaScript engine to pass the spike, but it must meet
the product gates without a material interaction regression. Failure of a first
implementation is evidence to adjust the ABI or memory plan, not permission to weaken
analyzer parity.

### M2 — Source provenance lock

- acquire the exact January inputs or approve explicit compatibility ledgers;
- lock byte counts, hashes, URLs, and upstream identities;
- separate permanent source provenance from temporary PostgreSQL-oracle identity;
- add a pure JMdict-to-canonical-entry slice and semantic-digest comparison;
- reproduce one primary conjugation, one secondary chain, and one generated-target
  reuse case without a database-shaped compatibility layer;
- inventory every `ctid`-owned observable order and define its deterministic
  replacement before full conjugation work begins;
- prove the existing pack encoders accept compiler-owned semantic input.

M1 and M2 can proceed in parallel.

### M3 — Full Rust semantic parity

- port readers, utilities, morphology, lexical materialization, scoring/path search,
  projection, and required serialization in small differential slices;
- pass corruption/format fixtures against the real pack;
- pass all 1,241 current-Lisp comparisons and all 301 fallback cases through both
  TypeScript and Rust;
- capture the 301 PostgreSQL-only fallback expectations as provenance-bound canonical
  fixtures before PostgreSQL retirement, or replace each with current-Lisp coverage;
- prove exact stable ties, scores, UTF-16 spans, and top-N output;
- keep the TypeScript implementation frozen except for specification corrections.

### M4 — Browser WASM cutover candidate

- replace the TypeScript analyzer inside the existing Worker;
- retain the PWA installer and lifecycle implementation;
- pass the complete unit, E2E, offline, corruption, update, responsive, size, and
  calibrated performance suites;
- perform physical Safari testing on the iPhone 13 baseline and current target device;
- retain a build-time switch to the TypeScript oracle during qualification only.

### M4N — Node, CLI, and API Rust cutover

- load the same WASM kernel from Node unless profiling proves that a native library is
  worth an additional binary-distribution matrix;
- cut Node, CLI, HTTP, and browser clients to the same clean analyze, romanize, and
  dictionary-entry model; keep legacy serialization only in differential qualification;
- keep filesystem verification and release loading in the thin Node adapter;
- do not carry a second TypeScript analyzer after the Rust transition cycle.

### M5A — Windows/WSL native handoff

- expose a small versioned C ABI with explicit error and allocator/free ownership;
- prevent Rust panics from unwinding across C and execute the parity corpus through a
  C caller on Linux, not only through Rust-native tests;
- document target triples, exported symbols, threading/call-serialization rules, pack
  ownership, result-buffer lifetime, and the exact Mac build/validation commands;
- provide a standalone integration README and header that a Mac agent can consume
  without reverse-engineering the Rust crate;
- do not claim XCFramework, Swift, simulator, or device validation from WSL.

Gate: the Rust kernel, C header, ABI tests, and handoff README are complete and tied to
the M3 parity-qualified revision. M4, M4N, and M5A may then proceed in parallel and are
revalidated together at the chosen cutover revision.

### M5B — Mac-owned iOS packaging

The Mac agent will build the Apple device/simulator static libraries, XCFramework,
Swift package/wrapper, and minimal validation app. It will run the corpus through the
actual C/Swift boundary and verify background execution, pack installation, offline
restart, leak-free ownership, and memory/performance on physical devices. Komi
integration begins only after that reusable boundary is qualified.

M4, M5A, and M5B share the Rust kernel but none may weaken another host's semantics.

### M6 — PostgreSQL-free source compiler

- build canonical roots/details/direct surfaces from source;
- replay custom data, chronological errata, best readings, and conjugation phases;
- preserve generated target reuse, property/lineage cross-products, ghost lineage,
  suffix/counter/split/hint facts, and reviewed order compatibility;
- compare semantic input digests and final bytes with the frozen PostgreSQL producer;
- require byte equality wherever representation is unchanged and individually review
  every unavoidable ordering delta;
- independently enumerate source-native forward conjugation relations and compare them
  exhaustively with the packed reverse relation, recording counts, digest, duplicates,
  and every explained omission or packed-only result;
- build and publish a pack with PostgreSQL physically unavailable;
- process at least one real upstream data/behavior update through the new path.

### M7 — Default cutover and retirement

- make Rust the default analyzer in accepted product hosts;
- make the source compiler the normal release path;
- retain TypeScript and PostgreSQL references for one update/release cycle;
- require every behavior formerly covered only by a live PostgreSQL query to have a
  durable fixture or source-native invariant before removing that query;
- then archive or delete them rather than maintaining two supported analyzers;
- keep upstream fixtures and compact migration evidence needed to explain history.

The genuine source-native update required by M6 is the PostgreSQL transition cycle;
another extra update is not required before retirement. Likewise, the first accepted
Rust-default release is the TypeScript transition cycle. Each reference remains
available through its transition release and is removed only after that release's
evidence is accepted.

The workstreams and retirement gates are:

```text
M0 baseline
  |-- M1 Rust spike --> M3 Rust parity --+--> M4 browser WASM --+
  |                                      +--> M4N Node/CLI/API --+--> retire TS kernel
  |                                      +--> M5A WSL C handoff -+
  |                                                   |
  |                                                   +--> M5B Mac iOS
  |
  +-- M2 source provenance --> M6 source compiler --> real update --> retire PostgreSQL
```

M1 and M2 begin in parallel, and each full implementation begins as soon as its own
gate passes. Neither track waits for the other. Only cross-kernel validation of the
source-built pack and final retirement are gated by both tracks.

## Exact work order

There is one short shared prerequisite before the two implementation tracks: close M0
so every agent works from the same durable code, pack, fixtures, and behavioral
contract. This is qualification and publication work, not another architecture phase.

### Phase 0 — Close and publish the baseline

All five steps are complete. The tag remains on the qualified artifact commit; later
status documentation is a docs-only descendant.

1. **Complete:** commit the accepted roadmap locally.
2. **Complete:** from that final clean HEAD, run two independent release builds, verify
   each, and compare the complete four-file inventories byte for byte.
3. **Complete:** rerun typechecks, unit/integration tests, chosen-authority parity,
   PostgreSQL scorer differential, browser E2E, and browser performance.
4. **Complete:** push the commit and create annotated tag
   `portable-core-260118-baseline` on that unchanged commit.
5. **Complete:** create the matching immutable GitHub Release in `TigerHix/ichiran-node`,
   attaching the exact four release files plus commit-bound qualification
   logs/attestations. Evidence is external to Git so recording it cannot change the
   qualified commit.

Read-only recovery of January source files may overlap Phase 0, but neither port should
silently adopt a different pack or fixture corpus while the baseline is moving.

### Phase 1 — Launch two entry tracks in parallel

**Track A: Rust feasibility.** Read format v1 unchanged; open the published real pack
in native Rust and browser WASM; prove representative lexical, morphology, generated,
details, UTF-16, ordering, and bulk-boundary behavior; measure final-shell bytes,
startup, latency, and peak/steady memory.

**Track B: source provenance.** Recover and pin the coherent January JMdict/Kanjidic2
inputs or propose explicit compatibility ledgers; implement pure parsing/projection
tests; identify every ordering dependency, including PostgreSQL `ctid`; define the
source-native exhaustive conjugation relation proof.

The tracks share only the frozen pack contract and fixture corpus. Track A does not
wait for raw-source recovery, and Track B does not wait for Rust.

### Phase 2 — Review each go/no-go gate independently

- Rust proceeds only if format v1 is usable, exact semantics are credible, the host
  boundary is coarse, lazy details remain lazy, and browser size/memory/performance has
  a viable path through the existing gates.
- The compiler proceeds only after coherent source identity is locked and accidental
  database ordering has an explicit deterministic replacement policy. Any remaining
  compatibility data must be small, named, reviewable, and sourced; no opaque
  PostgreSQL projection may become a permanent input.

Scaffolds, partial parsers, or one happy-path lookup do not satisfy either gate.
Each track moves to Phase 3 immediately when its own gate passes; a blocked source
snapshot does not idle Rust, and a Rust boundary iteration does not idle the compiler.

### Phase 3 — Run the full ports in parallel

**Track A — Rust kernel:**

1. Port formats, character/number utilities, and romanization.
2. Port surface, root, morphology, support, annotation, and detail readers.
3. Port candidate materialization, lexicon, suffixes, and counters.
4. Port analyzer rules, scoring, and exact top-N path search.
5. Port result projection, runtime orchestration, and retained legacy serialization.
6. Prove native/WASM parity, then run browser cutover, Node cutover, and WSL C ABI/
   Linux-harness/Mac-handoff work in parallel.

The 301 PostgreSQL-only fallback outputs must become provenance-bound fixtures before
the full Rust parity report stops depending on live queries.

**Track B — source compiler:**

1. Build canonical roots, details, and direct surfaces.
2. Replay custom sources, chronological errata, and best readings.
3. Generate primary and secondary conjugations.
4. Rebuild generated reuse, suffix/counter/split/hint/collision facts, and explicit
   stable order.
5. Feed compiler-owned semantic input into the existing encoders and release path.
6. Reproduce `ichiran-260118` exhaustively, then qualify one genuine update with
   PostgreSQL physically unavailable from the new build path.

Do not redesign the pack in either track. If measurements prove format v1 is a real
blocker, stop at the gate and version the producer and reader together as a separate
reviewed change.

### Phase 4 — Cut over and retire references

Make Rust and the source compiler the defaults. The M6 genuine update is the
PostgreSQL transition cycle, and the first accepted Rust-default release is the
TypeScript transition cycle; no extra shadow cycle is implied. Delete each reference
after its transition evidence and durable replacements pass. The Mac agent can perform
M5B in parallel once M5A is ready. Physical iPhone qualification remains an iOS release
gate, not a blocker for the WSL workstreams.

## Effort model

These are engineering-effort ranges, not calendar promises:

| Workstream | Expected effort |
|---|---:|
| Rust feasibility spike | roughly 1–2 focused weeks |
| Release-quality Rust kernel and browser integration | roughly 8–14 engineer-weeks |
| WSL C ABI, boundary tests, and Mac handoff | roughly 3–7 focused days |
| Mac-owned XCFramework, Swift wrapper, and Apple validation | roughly 1–3 additional weeks |
| Complete source-native compiler | roughly 8–12+ engineer-weeks |

The kernel and compiler should run as independent parallel workstreams after their
respective M1/M2 entry gates. The critical path is the slower stream plus integration,
not the sum of both ranges.

## Non-goals

- porting experimental grammar;
- shipping PostgreSQL, SQLite, Lisp, or a database-shaped runtime;
- rewriting the source compiler in Rust solely for language uniformity;
- running browser WASM inside iOS instead of compiling Rust natively;
- deleting the qualified TypeScript implementation before Rust parity;
- redesigning the pack while simultaneously replacing its producer and reader;
- assuming WASM performance without measuring the final browser boundary;
- broad output allowlists or opaque compatibility snapshots;
- Komi/Nemu integration before the reusable browser/native boundaries are proven.

## Accepted product decisions

- Browser, Node, and native iOS ultimately execute the same Rust analyzer source. Node
  uses the WASM artifact unless measurement justifies native packaging.
- The source compiler first recreates `ichiran-260118`, with exact behavior and byte
  equality wherever representation is unchanged, then qualifies one genuine update.
  Official pinned raw sources are permanent inputs; PostgreSQL exports are diagnostic
  evidence only.
- Windows/WSL owns the Rust kernel, versioned C ABI, C-boundary tests, and excellent Mac
  handoff documentation. A Mac agent owns XCFramework/Swift packaging, the validation
  app, Xcode/simulator/device work, and later Komi integration.
- Rust need not beat TypeScript in raw browser latency. It must preserve exact parity,
  meet the latency/memory gates, and avoid a material interaction regression. The
  25 MiB first-install limit remains the target; any exception requires explicit
  approval from final-shell measurements.
- The accepted baseline is pushed, clean-build-qualified, published as immutable
  artifacts, and tagged before full implementation begins.
- Pack format v1, parallel Rust/compiler work, one-cycle reference retention, one-time
  integrity-verified offline installation, and physical iPhone 13 / 17 Pro Max
  qualification remain accepted constraints.
