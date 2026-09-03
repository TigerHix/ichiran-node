# Browser Analyzer Alpha

This directory is the as-built specification and qualification record for the
TypeScript browser alpha. It is intentionally preserved as historical evidence.

> The alpha originally included a PWA shell. That ownership was superseded after
> cutover: the current browser package persists only analyzer data in OPFS and runs
> the kernel in a Web Worker. It does not ship or register a Service Worker. Shell
> caching, installability, and shell updates belong to the consuming application.

The alpha product boundary is defined in
[`../EDGE-NATIVE-MILESTONE.md`](../EDGE-NATIVE-MILESTONE.md). The authoritative
post-alpha architecture is the
[`source-compiler and Rust-kernel roadmap`](../SOURCE-COMPILER-RUST-KERNEL-ROADMAP.md).

The current demo is an analyzer-only browser host that uses no PostgreSQL, Node.js
service, or analysis-time network lookup after one data installation. PostgreSQL was
a read-only build-time oracle for the first compiler; it is not shipped to the
browser and is not used by the current source compiler.

The accepted scope is:

- a zero-runtime-dependency portable analyzer over immutable binary data;
- a Node-only compiler owned by `@ichiran/data`;
- a dedicated browser Worker and OPFS installer;
- a mobile-first browser demo derived from Komi's token and detail interaction model;
- top-N, entity hints, romanization, full offline dictionary details, and a legacy
  serializer;
- exact normalized oracle parity with an empty result-difference allowlist.

The separate experimental `@ichiran/grammar` package, GiNZA/Bunpro, a general
Kanjidic character API, Komi/Nemu integration, automatic analyzer-data updates, SQL emulation,
and mandatory WASM are outside this milestone. Analyzer-internal suffix handling,
segmentation filters, penalties, and pair synergies remain in scope because the
analyzer already observes them.

## Runtime architecture

The browser installs one pinned hot image and one pinned detail store into an OPFS
A/B slot. A 36-byte per-install ID in IndexedDB is the atomic cross-tab commit
record; the active `install-{a,b}.json` marker mirrors that ID so cold inspection can
reject mismatched state and stale corruption cannot target a same-release reinstall.
`hot.bin` contains five deterministic sections: a route-aware surface automaton,
root payload, reverse morphology, resident analyzer support, and random-access
annotations/generated facts. Complete forms, senses, glosses, and sense metadata
live in `details.bin` and are opened lazily.

`@ichiran/core` returns the clean lexical model: canonical root identity,
semantic inflection paths, top-N sentence paths, token alternatives, compounds,
counters, and entity hints. `serializeLegacyDetailed` is a thin presentation bridge
over that result and lazy details. It preserves the legacy shape without making
PostgreSQL-generated sequence IDs part of the new public model.

Ordinary conjugations are reconstructed by reverse morphology. Exceptional
generated-row behavior is not represented by a scalar property or a copied SQL row:
section 5 retains every relevant physical conjugation member and every ordered
`conj_prop` row in 10-byte records. Count-only exceptions use the same record format,
and generated records are packed in independently compressed root blocks that are
prewarmed once when the Worker opens the pack. The `ichiran-260118` release has 37
generated blocks. Split/hint annotation blocks remain
lazy behind a 16-entry LRU. See `ANALYZER-SUPPORT.md`.

The initial browser capability floor is Safari 26+ or a current Chromium browser.
The installer needs a dedicated Worker, OPFS, IndexedDB, writable file streams, Web
Locks, and `DecompressionStream`. Older Safari does not implement the deliberately
simple writable-stream install path used here.

## Gates

| Metric | Required |
|---|---:|
| Compressed analyzer manifest + data transfer | no more than 26 MiB |
| Installed analyzer data + commit metadata | no more than 64 MiB |
| Resident hot image | no more than 25 MiB |
| Ordinary top-one p95 at calibrated 6x Worker contention | no more than 75 ms |
| Pathological morphology p95 at calibrated 6x Worker contention | no more than 250 ms |
| Dense contiguous 64-256-unit top-1/5/10 p95 at calibrated 6x Worker contention | no more than 500 ms |
| Main-thread analyzer work | none |

Actual iPhone 13-class validation is a production gate after this alpha. The alpha uses
repeatable desktop Chromium runs with a measured 5.0-7.5x slowdown on the exact analyzer
Worker as the agreed provisional proxy.

## Build and verification

The product parity target is upstream Ichiran
`ea9583368e67cad22d94abae8dbcc8df96d99bcd` with data release
`ichiran-260118`. The release dump is 200,012,956 bytes with SHA-256
`98a44e2cc88a65677da8b1f7124e7d6c904253eb1aae0ef16d2c7cc1dacdba82`;
the pinned upstream suite passes 782 / 782 analyzer assertions. Exact source evidence
is recorded in `browser-alpha/upstream-oracle.json`.

The former PostgreSQL-backed Node analyzer remains frozen at
`d583720572fbf26ee201166ac47034c50380a571` as a private compiler and migration
reference. Release and differential commands require the checked-out
`packages/reference-postgres` source to match that reference. The v2
`browser-alpha/sources.lock.json` was produced from the qualified database with
`alpha:release:refresh-lock`. It locks every `ichiran-260118` component count and
digest, including the 9,173,122-row exhaustive morphology relation and its zero
alpha-only/duplicate result.

Run database-backed baseline tests against the local oracle with:

```bash
ICHIRAN_DB_URL='postgresql:///ichiran_oracle_ea958336?host=%2Fvar%2Frun%2Fpostgresql' \
  bun test --timeout 30000 --max-concurrency 1 packages/reference-postgres/tests
```

The supported alpha commands are:

```bash
bun run alpha:release:typecheck
bun run alpha:release:refresh-lock -- \
  --database "$ICHIRAN_DB_URL"
bun run alpha:release:build -- \
  --database "$ICHIRAN_DB_URL" \
  --out dist/browser-alpha \
  --pack-version ichiran-260118
bun run alpha:release:verify -- \
  --out dist/browser-alpha

bun run alpha:demo:stage
bun run alpha:demo:build
bun run alpha:demo:test
bun run alpha:demo:e2e
```

The deterministic `ichiran-260118` data assets are 12,662,917 compressed hot bytes and
12,317,325 compressed detail bytes. They install as a 24,857,288-byte resident hot
image and 13,555,874-byte lazy detail store. Analyzer transfer and persisted totals
are derived from the signed release and bound in `stats.json`; qualification requires
all three analyzer size gates to pass. IndexedDB allocation overhead is browser
managed and not included in that logical payload total.

`stats.json`, the exhaustive oracle report, and `work/browser-benchmark.json` are
generated qualification evidence and remain outside Git with the release artifacts.
The browser report records its exact Chromium version, CPU-affinity calibration, raw
Worker samples, offline assertions, and responsive checks. Physical-iPhone timing is
still deliberately deferred.
