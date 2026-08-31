# Browser Analyzer Alpha: parity and acceptance contract

Status: pinned analyzer implementation complete; release and browser evidence is
generated locally and kept outside Git, 2026-08-29
Authoritative analyzer target: upstream Ichiran at
`ea9583368e67cad22d94abae8dbcc8df96d99bcd`, data release `ichiran-260118`
Frozen transition reference: `ichiran-node` at
`d583720572fbf26ee201166ac47034c50380a571`
Scope: analyzer only. The experimental `@ichiran/grammar` package and a general
Kanjidic character API are excluded.

## 1. Definition of done

The alpha is complete when a production-built static PWA can install one pinned
data version, restart with networking disabled, and run the clean analyzer API and
legacy-shaped serializer in a dedicated Worker. It must pass the correctness,
offline, artifact-size, and throttled-browser gates below.

The implementation language is not a gate. TypeScript, WASM, or a mixture is
acceptable. WASM is added only when profiling is evidence that it helps.

The first alpha has deliberately simple data lifecycle behavior: one immutable
version, integrity checking, incomplete/corrupt-install detection, and an explicit
clear/reinstall action. Background update checks, deltas, migrations, and multiple
installed versions are not required.

## 2. Observable compatibility policy

The canonical browser result uses lexical identity, never generated-entry identity:

```text
root = canonical JMdict/custom seq + source form + source reading
inflection = ordered [first rule, optional second rule]
```

The thin legacy serializer preserves the current nesting and observable fields, but
a conjugated token's synthetic PostgreSQL `seq` is intentionally replaced by its
canonical root `seq`. Direct lexical `seq` values remain unchanged. Synthetic entity
segments continue to omit `seq`.

After that identity normalization, compatibility is exact for:

- text spans and path segmentation;
- path and token scores (integer equality, no tolerance);
- reading, kana, and romanization strings;
- ordered inflection properties and conjugation descriptions;
- compounds, components, suffix descriptions, and alternatives;
- senses, ordered glosses, POS, field, misc, dialect, and info metadata;
- entity-hint behavior and top-N behavior.

Object keys are canonicalized lexicographically for comparison. Arrays remain
ordered. Only a run of alternatives or complete paths with exactly equal score may
be sorted, using the semantic candidate key below; differently scored results may
never be reordered. Strings compare as exact UTF-8 after the public analyzer has
performed its normal input normalization. `null`, an omitted property, and an empty
array remain distinct.

Semantic candidate key:

```text
route, surface, rootSeq, sourceForm, sourceReading,
ordered (pos, conjType, negative, formal, ruleOrdinal) path,
ordered recursive component semantic keys
```

## 3. Existing fixture oracle

The checked-in fixture files are already substantial and must not be replaced with
only hand-picked examples.

| Fixture | Coverage | SHA-256 |
|---|---:|---|
| `packages/reference-postgres/tests/data/segmentation.json` | 534 segmentation cases; 2 historical JMdict-version skips | `a3df8f66132c50d3f78d68632ed8d3477717f8e95b0730e89f05e588252e4944` |
| `packages/cli/tests/data/cli.json` | 5 romanization, 3 info, 252 full JSON requests (250 top-1, one top-3, one top-5) | `bc611dcf11e4b271ca2775a58f8c6615130fa2d42782cc1a679fb34eb8d73f5a` |
| `packages/cli/tests/data/cli-lisp-outputs.json` | fresh upstream `ea958336` output for the preceding requests | `a092f07a2b7337c3a790b0d93808213adf2e89eef1750aeaed54160b90856bb8` |
| `packages/cli/tests/data/cli-canonical-outputs.json` | preceding Lisp output after compiler-only canonical identity normalization | `d9ef666af3be61c8bf9987f84a7efa9728b21566a98b6cca63d1d83463868080` |
| `packages/cli/tests/data/hard-cli.json` | 149 top-1 full JSON requests; entries 0-49 are complex morphology chains | `5e8a910314843a25c4bf2dd4663db0211fecc31a031b38c88c9880780115be69` |
| `packages/cli/tests/data/hard-cli-lisp-outputs.json` | fresh upstream `ea958336` output for the hard requests | `d82f5d5e9ef3b858209ea63a1ea5b448c6460e4ea0fddfc6b811ebb7c3756a85` |
| `packages/cli/tests/data/hard-cli-canonical-outputs.json` | preceding hard Lisp output after compiler-only canonical identity normalization | `235d873cae56cde282feccef577b7b6314b69a2bc4197016167f9e0fe609a7d1` |

The request corpora under `packages/reference-postgres/tests/data` and
`packages/cli/tests/data` have identical hashes. Fresh upstream Lisp output is captured
under the CLI package; the reference copy remains the historical baseline.

The complete differential corpus additionally includes:

- 200 counter combinations in `packages/reference-postgres/tests/counters.test.ts`;
- 54 entity-hint cases in `packages/reference-postgres/tests/entity-hints.test.ts`;
- 17 presentation round trips in `packages/reference-postgres/tests/json-consistency.test.ts`;
- six number cases and one numeric `basicSplit` case;
- conjugation tombstone/special-form assertions alongside the database-backed
  conjugation checks.

The 13 standalone `matchReadings` tests do not make the general Kanjidic API part of
the browser product. Instead, the compiler records the resolved hinted-kana output
for every root reading reached by the registered definitions in
`packages/reference-postgres/src/dict/splitDefinitions.ts`; the refreshed lock records
the exact registration and resolved-output counts.

Existing PostgreSQL baseline commands, run from the frozen repository with a valid
`ICHIRAN_DB_URL`:

```bash
bun test --timeout 30000 --max-concurrency 1 packages/reference-postgres/tests/

bun test --timeout 60000 \
  packages/reference-postgres/tests/conjugation.test.ts \
  packages/reference-postgres/tests/counters.test.ts \
  packages/reference-postgres/tests/segmentation.test.ts

RUN_PARITY_TESTS=true bun test --timeout 60000 \
  packages/cli/tests/cli-parity.test.ts \
  packages/cli/tests/hard-cli-parity.test.ts
```

The complete `packages/reference-postgres/tests` baseline is 824 passes, two documented skips, and
zero failures when run serially against the Unix-socket oracle. The narrower
conjugation + counters + segmentation command is 733 passes, two skips, and zero
failures; it is retained because it is the historical analyzer-boundary measurement
in the architecture investigation, not because it is the full core suite. The CLI
parity command comprises 409 comparisons: five romanizations, three info outputs,
252 ordinary/full outputs, and 149 hard/full outputs.

For the local Unix-socket oracle used during this investigation, the test helper's
URL parser accepts:

```bash
export ICHIRAN_DB_URL='postgresql:///ichiran_test?host=%2Fvar%2Frun%2Fpostgresql'
```

The checked-in CLI outputs are fresh recordings from the pinned upstream Lisp CLI.
The copies under `packages/reference-postgres` retain the older Node/PostgreSQL
baseline so intended upstream changes can be named rather than hidden. The packed
release must match upstream; the frozen Node reference is a migration diagnostic.

The raw Lisp recordings remain verbatim and therefore retain snapshot-specific
generated PostgreSQL sequence IDs. The pack-only gate reads the separate canonical
files, generated by `alpha:fixtures:canonicalize` against the locked read-only
database. Each canonical file embeds and verifies the raw recording's SHA-256, the
complete `sources.lock.json` SHA-256, oracle identities, request count, and normalized
output digest. Expected canonical identities are therefore independent of the pack
under test; a pack cannot rewrite its own expected result.

## 4. Source and database snapshot

Every artifact manifest points to the digest of committed
`browser-alpha/sources.lock.json`. The compiler refuses a dirty source checkout or a
source/oracle mismatch. The lock contains:

- frozen oracle repository commit;
- Bun, Node, PostgreSQL server, compiler format, and binary format versions;
- SHA-256 and byte size of every raw source file;
- hashes/counts of canonical PostgreSQL projections and generated physical-member
  projection;
- exact artifact counts/digests and the reviewed morphology relation attestation.

The current upstream inputs are explicit:

| Input | Identity |
|---|---|
| Ichiran source | commit `ea9583368e67cad22d94abae8dbcc8df96d99bcd`, tree `5352f7641feaeeb1c3db04ea80ced31ca117dbe3` |
| `ichiran-260118.pgdump` | 200,012,956 bytes; SHA-256 `98a44e2cc88a65677da8b1f7124e7d6c904253eb1aae0ef16d2c7cc1dacdba82` |
| jmdictdb source | commit `02dc4aabd185a5b02c29fa6bc685bd78296741b3`, data tree `529a73e7e4ae91842ac3b280f51a920f15e38105` |
| `conj.csv` | `46a915afc385e05dfc8174226a5ee7bb4231e30870068e77d52e10a3433b5852` |
| `conjo.csv` | `b2919a24ecc7f3cdf9b9478eb13526b76cb086c53000316ecb1781ff947c14f3` |
| `kwpos.csv` | `485b5ac5d8a23a8bcd18e69ca2fdf21d231fb740780a6c7d7ccf40d65ed831e4` |
| `data/sources/extra.xml` | `4a056ebe608cb7bb5284e412688ff634d3516f3249a54d37b33645d7a266093b` |
| `data/sources/gyoseiku.csv` | `23f706ff84b5c27da86be1d7a6066630d34617e1769edb0673c91d258851ce3f` |
| `data/sources/jichitai.csv` | `328bd0779b1bb69a4c1bc3773c5ff71cffb59c825377ca7f31eafda3860e3af8` |

The qualified database is PostgreSQL 16.15, UTF-8, `ja_JP.utf8`, with normalized
schema SHA-256
`481fe143a39d53ff6393ec83a77623f8824e1322c9ef7b8ded4503204bd0be98`.
After upstream errata its known root-data counts are:

| Projection | Rows |
|---|---:|
| root entries | 214,700 |
| root kanji / kana forms | 227,054 / 259,364 |
| senses | 251,648 |
| glosses | 434,112 |
| sense properties | 407,620 |
| restricted readings | 6,332 |
| Kanjidic characters / readings used by the build | 13,108 / 38,977 |

The lock must cover every compiled artifact: route-specific direct and morphology
surfaces, root/scoring facts, senses and glosses, suffix/counter caches, resolved
reading hints, physical-member exceptions, and direct/generated lookup order. It
stores semantic identities rather than surrogate database IDs, CTIDs, timestamps, or
machine paths.

`alpha:release:refresh-lock` compiled and verified the qualified database, then wrote
the deterministic v2 lock. Key packed counts are:

| Component | Locked result |
|---|---:|
| accepted surface endpoints / states / edges | 8,393,704 / 589,125 / 971,845 |
| root payload surfaces / forms / entries | 443,275 / 476,178 / 217,967 |
| morphology rules / templates / root keys | 1,161 / 7,211 / 40,882 |
| split / hint annotation facts | 38,032 / 36,885 |
| generated blocks / roots / records | 37 / 20,347 / 764,828 |
| detail entries / forms / senses / glosses / properties | 217,967 / 492,913 / 251,648 / 434,112 / 407,620 |

The lock also records the exact byte length and SHA-256 of every component.
Maintainers do not edit expected artifact values by hand.

Code-defined errata, suffixes, counters, split rules, hints, scoring, penalties, and
synergies are locked by the clean repository commit. The private
`packages/reference-postgres` source remains frozen at
`d583720572fbf26ee201166ac47034c50380a571` except for its build-only package boundary.
`--allow-dirty` does not relax source, database, artifact, or parity checks.

## 5. Exhaustive parity gates

All gates operate on semantic keys, never generated IDs.

### 5.1 Artifact/data equality

- Exactly 214,700 root entries, 227,054 root kanji forms, and 259,364 root kana
  forms are accounted for by the refreshed source projection.
- Every scoring-critical field and all 6,332 restriction rows equal the refreshed
  root projection.
- All 251,648 senses, 434,112 glosses, and 407,620 sense properties decode exactly.
- The route-aware scanner accepts exactly the runtime-active endpoints recorded by
  the refreshed lock and accounts for every inactive physical source row.
- Suffixes, counters, and their sequence/class mappings compare by exact logical
  value, not only by count.
- Every applicable easy-hint result is compiled exactly without a runtime Kanjidic
  table; the refreshed lock records the final registration and output counts.
- Every selected generated exception round-trips its count fact and every physical
  member/property row in stable order. Two-stage members bind to the exact intermediate
  `viaMemberOrd`; count-only exceptions decode with no fabricated member.

### 5.2 Morphology relation

Run reverse morphology against every installed route/form/path match recorded by the
refreshed lock. For each candidate compare route, surface, root, source form/reading,
ordered one- or two-stage properties, inherited ord/common facts, and exact forward
equality. Compare sorted semantic diff JSONL byte-for-byte with the frozen relation
attestation. An unexpected diff or a stale attestation row fails.

The final relation measurement expands 9,173,122 legacy keys across 7,959,271
surface groups and compares them with 8,774,911 packed keys. It records exactly
398,211 legacy-only rows, zero alpha-only rows, and zero duplicate rows on either
side. The canonical diff SHA-256 is
`6eb1cbae46eb2df0c67570764d0cf408bf4d7b8873eb4b53ce694cd17549d421`.
Those exact rows—not a broad predicate—capture removed ghost readings and invalid
property cross-products. Fifty explicit compatibility patches and four exact
tombstones preserve reviewed installed behavior.

### 5.3 End-to-end output

Run both APIs against the complete frozen corpus:

- clean `analyze`/`describe`/`romanize` output;
- legacy-shaped serialization;
- top-1, the checked top-3/top-5 cases, all entity fixtures, counters, numbers, and
  every recorded hinted reading.

Canonical output must be byte-identical to the pinned upstream oracle. Ordinary and
hard current-Lisp captures are normalized once by the compiler into independently
hashed fixtures; the portable output is compared directly and is never rewritten by
the PostgreSQL identity resolver. This makes a leaked generated sequence ID a release
failure.

Current-Lisp snapshots are authoritative for 534 segmentation, five standalone
romanization, 252 ordinary detailed, and 149 hard detailed comparisons. The frozen
PostgreSQL implementation remains the detailed-and-clean fallback authority for 200
counter requests, 54 entity-hint requests, and 47 deterministic probes. Its output is
diagnostic-only where a current-Lisp snapshot exists. The alpha has no result
allowlist: any chosen-authority divergence fails the release gate.

Supported release and end-to-end commands:

```bash
bun run alpha:release:build -- \
  --database "$ICHIRAN_DB_URL" \
  --out dist/browser-alpha \
  --pack-version ichiran-260118 \
  --shell-dir packages/browser-demo/dist

bun packages/core/tools/oracle-parity.ts \
  --repository "$PWD" \
  --release dist/browser-alpha \
  --database "$ICHIRAN_DB_URL" \
  --out work/oracle-full-final.json

bun run alpha:release:verify -- \
  --out dist/browser-alpha \
  --shell-dir packages/browser-demo/dist
```

`oracle-parity.ts` runs the full corpus by default and exits non-zero on any current
difference. `--smoke` is for development only; `--allow-failures` is diagnostic and is
never an acceptance command. `alpha:release:build` itself performs the exhaustive
morphology gate in the build's read-only snapshot. The equivalent standalone
reproduction command is documented in `MORPHOLOGY.md`.

## 6. Morphology relation-attestation format

The morphology relation diff is build proof, not runtime policy and not permission
for an analyzer-output difference. Runtime patches/tombstones remain small explicit
compiler inputs. The attestation contains no regex, range, glob, SQL predicate, or
“ignore field” switch.

Each morphology JSONL row is canonical JSON with this shape:

```json
{"route":"kana","surface":"...","side":"legacy-only","key":"[123,\"source\",\"form\",\"reading\",null,[[\"v1\",5,false,false]],0,null]"}
```

`key` is the canonical tuple of root sequence, source text/form/reading, optional
intermediate, ordered semantic property path, inherited ordinal, and commonness.
`side` is `legacy-only` or `alpha-only`. Every semantic key is a separate row; the
release gate requires zero alpha-only rows and locks the complete emitted JSONL by
count and SHA-256. The reviewed reason classes in `MORPHOLOGY.md` describe this exact
set but are not executable predicates. The release compiler independently recomputes
and checks the row count, digest, all relation totals, and database-artifact totals
from the exact compiled morphology bytes. End-to-end output has a separate strict
gate with zero allowed differences.

Generated-`seq` replacement is performed by canonicalization and documented as a
global API change; it is not an analyzer result difference.

## 7. Artifact contract and hard size gates

The installed release consists of one small manifest, one directly readable
`hot.bin`, one complete offline `details.bin` (with sequence-to-offset directory),
and the static PWA shell. A different physical split is acceptable only if the same
totals and atomic install behavior hold.

`manifest.json` contains format version, data version, `sources.lock.json` hash,
and for every file its SHA-256, stored bytes, and transport bytes. The compiler emits
no timestamp or absolute path. Two builds in fresh temporary directories from the
same snapshot must produce identical file hashes and bytes.

Binary-size pass/fail definitions use powers of two:

- `hot.bin` raw bytes: **at most 24 MiB = 25,165,824 bytes**;
- all persisted browser payload bytes plus precached app shell: **at most 64 MiB = 67,108,864
  bytes**;
- fresh-install wire payload for manifest, app shell, and all analyzer data using
  the release content encoding: **at most 25 MiB = 26,214,400 bytes**.

The `ichiran-260118` analyzer data measures 24,857,288 resident hot bytes; its
compressed hot/detail assets are 12,662,917 and 12,317,325 bytes respectively. The
production release report supplies the shell-dependent persisted and first-install
wire totals, and qualification requires all three gates to pass without excluding
dictionary details.

The size checker sums the manifest's actual payload lengths and deterministic
release-encoded lengths, including one committed `install-{a,b}.json` slot marker;
it does not count the temporary inactive upgrade slot or use filesystem or IndexedDB
allocation size. The IndexedDB contribution is the 36-byte logical UUID;
browser-managed schema, key, structured-clone, and page overhead are implementation
defined. Full dictionary details are mandatory in these totals. Full Kanjidic
meanings/radicals/strokes are absent because no alpha analyzer operation consumes
them.

The browser production dependency graph must contain no `postgres`, Node builtin,
SQL engine, or Node compatibility shim. The Node-only compiler may depend on
PostgreSQL.

## 8. Browser benchmark corpus

Materialize the following exact texts into a committed `browser-alpha/bench/corpus.json`;
do not slice fixture files dynamically at benchmark time. Record each item's source
fixture and index.

Hard-gated groups:

- `ordinary`: `packages/cli/tests/data/cli.json` `fullJson[3..101]`, 99 natural
  13-36-character sentences;
- `pathological-morphology`: `packages/cli/tests/data/hard-cli.json`
  `fullJson[0..49]`, 50 complex auxiliary/morphology chains;
- `dense-contiguous-boundary`: repeated `あ` at 64, 128, 192, and the accepted
  256-unit contiguous-word boundary, each measured at top-1, top-5, and top-10.

Report-only diagnostic groups:

- 459 segmentation fixtures of at most 12 characters;
- 50 long noun/compound cases, CLI indices 102-151;
- 50 hiragana/colloquial cases, CLI indices 152-201;
- 50 modern mixed-script/slang cases, CLI indices 202-251;
- the top-3/top-5 fixture calls, all 54 entity-hint cases, counters/numbers;
- five committed top-10 paragraph-scaling probes at 128, 512, 1,024, 2,048,
  and the accepted 4,096 UTF-16-unit boundary;
- `describe` random-access latency and Worker ready/first-call latency.

## 9. Performance procedure and gates

Benchmark the production PWA through its public Worker RPC, not a Node reader or an
internal function. Page-target CDP throttling is not an acceptable proxy because it
does not throttle the dedicated analyzer Worker. Pin the complete Playwright/Chromium
process group and five CPU-contention peers to one Linux CPU instead. Calibrate the
exact `ichiran-analyzer` Worker target immediately before the measured run and require
its contended-to-baseline median ratio to be 5.0-7.5. The run records the calibration
samples, browser revision, OS, CPU model, CPU affinity, repository/artifact hashes, and
raw analyzer timings.

Procedure:

1. Install the complete pack, close the page, reopen it offline, and await Worker
   readiness.
2. Call clean `analyze` with the committed request limit: top-1 for ordinary and
   pathological inputs, and top-1/5/10 for the dense boundary. Do not call `describe`
   or hydrate gloss details.
3. Do not add a whole-result memo/cache for the benchmark. Packed indexes and JIT state
   remain warm after the two warmup passes.
4. Start the five same-affinity contention peers only after the Worker calibration
   baseline. Make two unmeasured shuffled passes over each corpus, then ten measured
   passes using a fixed seed. Include `postMessage`, Worker computation, and response
   clone in each duration; exclude UI result rendering.
5. Compute p95 by nearest rank over all per-request samples and retain each raw
   sample in the report.

Hard gates on the designated reference desktop:

- ordinary top-1 p95 at the calibrated 6x proxy: **at most 75 ms**;
- pathological-morphology top-1 p95 at the calibrated 6x proxy: **at most 250 ms**;
- dense contiguous 64-256-unit top-1/5/10 p95 at the calibrated 6x proxy:
  **at most 500 ms**;
- zero main-thread `longtask` entries over 50 ms during ordinary analyzer UI work,
  and no analyzer-kernel import in the window bundle.

Performance gates say nothing about whether the kernel is JS or WASM. Top-N outside
the explicit dense-boundary gate, `describe`, initial install, Worker startup, and peak
browser memory are reported but not assigned an unagreed alpha threshold.

The calibrated contention ratio is a multiplier, not a hardware normalization. The
first accepted benchmark must designate and record the reference workstation/runner;
subsequent hard comparisons use that same runner class and pinned browser revision.

## 10. Offline/PWA functional gates

The supported browser floor is Safari 26+ or current Chromium. Required runtime
capabilities are Worker, OPFS, IndexedDB, Web Locks,
`FileSystemFileHandle.createWritable()`, and `DecompressionStream`; the offline app
shell uses a Service Worker.

Automated browser tests must prove:

- manifest/data hashes are checked before marking an install complete;
- interruption midway through every artifact never exposes a partial active pack;
- a failed staged reinstall removes only its inactive slot and leaves the prior
  verified generation selected and usable;
- the shell-cached deployed manifest is authenticated before status/runtime open,
  and an older installed manifest is reported as requiring reinstall;
- a queued cross-tab read observes the new per-install commit ID after clear or
  reinstall, and stale corruption cannot mark a same-release reinstall corrupt;
- restart after an incomplete or corrupted first install offers a clear reinstall;
- after a successful install, browser restart plus network blocking still supports
  top-1/top-N analysis, romanization, entities, and full `describe` gloss details;
- analysis and `describe` cause zero HTTP requests after installation;
- all analyzer CPU work runs in the dedicated Worker;
- persistence-request denial is handled and surfaced without corrupting data;
- explicit clear/reinstall removes only this app's stored pack and can reinstall it.
- a waiting shell update leaves old lazy chunks available offline, activates only
  after every old tab closes, reopens offline, and then removes the unused cache.

No update scheduler, delta update, background migration, Komi/Nemu integration, or
production hosting is required for this alpha.

## 11. Qualification status

The required implementation now exists: exhaustive morphology verification, pinned
canonical Lisp fixtures, one portable analyzer, an exact physical-member overlay, the
OPFS PWA, and the production Worker benchmark harness.

The clean `ichiran-260118` oracle report contains 1,241 chosen-authority comparisons:
534 segmentation, five standalone romanization, and 702 detailed analyzer requests.
It also checks 301 clean fallback projections for counters, entity hints, and probes.
The acceptance result is exact throughout with an empty result allowlist. Frozen
PostgreSQL differences on snapshot-covered requests remain named diagnostics and are
not silently accepted as portable-output differences.

The production Playwright qualification installs the complete release, analyzes and
opens details through the public Worker API, restarts with networking blocked, proves
zero post-install analyzer requests, exercises integrity/interruption recovery and
cross-tab mutation locking, and checks the required desktop and phone layouts. Its
calibrated contention run must satisfy the 75 ms ordinary, 250 ms pathological, and
500 ms dense-boundary p95 gates and report no main-thread analyzer work.

Exact machine-dependent browser samples, Chromium/CPU metadata, calibration, and
long-task evidence are written to `work/browser-benchmark.json` by
`alpha:demo:qualify`; they are deliberately generated rather than committed. The
immutable artifact identities and sizes are recorded by the release's `manifest.json`
and `stats.json`.

The release provenance is the pinned upstream commit, qualified database, source lock,
and `ichiran-260118.pgdump` identity recorded above. A future source-only compiler may
additionally pin the original Kanjidic2 XML; this does not require shipping Kanjidic at
runtime.

Actual iPhone 13 and iPhone 17 Pro Max measurements remain deliberately deferred.
Passing this alpha is a conservative desktop proxy, not production mobile signoff.
