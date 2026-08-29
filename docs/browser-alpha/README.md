# Browser Analyzer Alpha

The alpha is an installable, analyzer-only PWA that uses no PostgreSQL, Node.js
service, or network lookup after one data installation. PostgreSQL is a read-only
build-time oracle for the first compiler; it is not shipped to the browser.

The accepted scope is:

- a zero-runtime-dependency portable analyzer over immutable binary data;
- a Node-only compiler owned by `@ichiran/data`;
- a dedicated browser Worker and OPFS installer;
- a mobile-first PWA derived from Nemu's token and detail interaction model;
- top-N, entity hints, romanization, full offline dictionary details, and a legacy
  serializer;
- exact normalized oracle parity with an empty result-difference allowlist.

The separate experimental `@ichiran/grammar` package, GiNZA/Bunpro, a general
Kanjidic character API, Komi/Nemu integration, automatic updates, SQL emulation,
and mandatory WASM are outside this milestone. Analyzer-internal suffix handling,
segmentation filters, penalties, and pair synergies remain in scope because the
analyzer already observes them.

## Runtime architecture

The browser installs one pinned `hot.bin` and one pinned `details.bin` into OPFS.
`hot.bin` contains five deterministic sections: a route-aware surface automaton,
root payload, reverse morphology, resident analyzer support, and random-access
annotations/generated facts. Complete forms, senses, glosses, and sense metadata
live in `details.bin` and are opened lazily.

`PortableAnalyzer.analyze` returns the clean lexical model: canonical root identity,
semantic inflection paths, top-N sentence paths, token alternatives, compounds,
counters, and entity hints. `serializeLegacyDetailed` is a thin presentation bridge
over that result and lazy details. It preserves the legacy shape without making
PostgreSQL-generated sequence IDs part of the new public model.

Ordinary conjugations are reconstructed by reverse morphology. Exceptional
generated-row behavior is not represented by a scalar property or a copied SQL row:
section 5 retains every relevant physical conjugation member and every ordered
`conj_prop` row in 10-byte records. Count-only exceptions use the same record format,
and generated records are packed in 36 independently compressed root blocks that are
prewarmed once when the Worker opens the pack. Split/hint annotation blocks remain
lazy behind a 16-entry LRU. See `ANALYZER-SUPPORT.md`.

The initial browser capability floor is Safari 26+ or a current Chromium browser.
The installer needs a dedicated Worker, OPFS, writable file streams, Web Locks, and
`DecompressionStream`; the PWA shell additionally uses a Service Worker. Older Safari
does not implement the deliberately simple writable-stream install path used here.

## Gates

| Metric | Required |
|---|---:|
| Compressed one-time transfer | no more than 25 MiB |
| Installed shell + analyzer data | no more than 64 MiB |
| Resident hot image | no more than 24 MiB |
| Ordinary top-one p95 at calibrated 6x Worker contention | no more than 75 ms |
| Pathological morphology p95 at calibrated 6x Worker contention | no more than 250 ms |
| Main-thread analyzer work | none |

Actual iPhone 13-class validation is a production gate after this alpha. The alpha uses
repeatable desktop Chromium runs with a measured 5.0-7.5x slowdown on the exact analyzer
Worker as the agreed provisional proxy.

## Build and verification

The PostgreSQL oracle is pinned to
`d583720572fbf26ee201166ac47034c50380a571`. It contains the original analyzer plus a
build-only shared-transaction seam and the reviewed easy-hint tuple fix documented in
the acceptance contract. Snapshot and toolchain metadata are frozen in
`browser-alpha/sources.lock.json`; `oracle.json` records the measured database identity.
Release and differential commands also require the checked-out `packages/core` tree to
match that exact ancestor commit.

Run database-backed baseline tests against the local oracle with:

```bash
ICHIRAN_DB_URL='postgresql:///ichiran_test?host=%2Fvar%2Frun%2Fpostgresql' \
  bun test --timeout 30000 --max-concurrency 1 packages/core/tests
```

The supported alpha commands are:

```bash
bun run alpha:release:typecheck
bun run alpha:release:build -- \
  --database "$ICHIRAN_DB_URL" \
  --out dist/browser-alpha \
  --pack-version alpha.1 \
  --shell-bytes <measured-production-shell-bytes>
bun run alpha:release:verify -- \
  --out dist/browser-alpha \
  --shell-bytes <the-same-measured-byte-count>

bun run alpha:demo:stage
bun run alpha:demo:build
bun run alpha:demo:test
bun run alpha:demo:e2e
```

The current `alpha.1-dev` qualification pack passes all three size gates: 25,055,731
wire bytes, 38,249,770 persisted bytes, and a 24,422,280-byte hot image when paired with
the measured 607,732-byte production shell. Its strict fresh-PostgreSQL differential is
1,241 / 1,241 exact with an empty result-difference allowlist.

The production offline Playwright suite passes 5 / 5 tests. On Chrome 151.0.7922.34,
an AMD Ryzen 9 9950X pinned to CPU 31 with five contention peers produced an exact
Worker calibration ratio of 6.134353741858198x (58.8 ms baseline, 360.7 ms contended).
Ordinary analysis measured p50 25.5 ms, p95 50.8 ms, and max 128.3 ms over 990 samples;
pathological morphology measured p50 51.4 ms, p95 118.9 ms, and max 185.6 ms over 500
samples. Worker ready/first-analysis times were 342.9/65.3 ms, and the main-thread
long-task list was empty. The clean final build must reproduce this dev qualification;
its own `stats.json`, oracle report, and browser benchmark report remain the release of
record.
