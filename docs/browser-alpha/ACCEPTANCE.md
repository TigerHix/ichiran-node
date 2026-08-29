# Browser Analyzer Alpha: parity and acceptance contract

Status: proposed implementation contract, 2026-08-28
Frozen implementation oracle: `work/ichiran-node` `main` at
`ba1966a0699e4aec9b5cfe2f18b448c21adcc590`
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
ordered (pos, conjType, negative, formal, ruleOrdinal) path
```

## 3. Existing fixture oracle

The checked-in fixture files are already substantial and must not be replaced with
only hand-picked examples.

| Fixture | Coverage | SHA-256 |
|---|---:|---|
| `packages/core/tests/data/segmentation.json` | 534 segmentation cases; 2 historical JMdict-version skips | `a3df8f66132c50d3f78d68632ed8d3477717f8e95b0730e89f05e588252e4944` |
| `packages/cli/tests/data/cli.json` | 5 romanization, 3 info, 252 full JSON requests (250 top-1, one top-3, one top-5) | `bc611dcf11e4b271ca2775a58f8c6615130fa2d42782cc1a679fb34eb8d73f5a` |
| `packages/cli/tests/data/cli-lisp-outputs.json` | historical expected output for the preceding requests | `f9857e5f8294c79a74d4c7769ff5b5fc9dad7a1e3f879720ca3f7e77d3227c99` |
| `packages/cli/tests/data/hard-cli.json` | 149 top-1 full JSON requests; entries 0-49 are complex morphology chains | `5e8a910314843a25c4bf2dd4663db0211fecc31a031b38c88c9880780115be69` |
| `packages/cli/tests/data/hard-cli-lisp-outputs.json` | historical expected output for the hard requests | `8e02980887f2c088349b50f41e43d33c51d6a7c741d66e40896cbfb2ba54f3d7` |

The copies under `packages/core/tests/data` and `packages/cli/tests/data` currently
have identical hashes. The CLI copies are canonical for full-output fixtures; the
core copy is canonical for segmentation.

Additional current tests to port to the portable kernel:

- 200 counter combinations in `packages/core/tests/counters.test.ts`;
- 54 entity-hint cases in `packages/core/tests/entity-hints.test.ts`;
- 17 presentation round trips in `packages/core/tests/json-consistency.test.ts`;
- six number cases and one numeric `basicSplit` case;
- the conjugation tombstone/special-form assertions replacing the current
  database-row-only conjugation test.

The 13 standalone `matchReadings` tests do not make the general Kanjidic API part of
the browser product. Instead, the compiler records the resolved hinted-kana output
for every root reading reached by the 420 `defEasyHint` registrations in
`packages/core/src/dict/splitDefinitions.ts`; those outputs compare exactly.

Existing PostgreSQL baseline commands, run from the frozen repository with a valid
`ICHIRAN_DB_URL`:

```bash
bun test --timeout 30000 --max-concurrency 1 packages/core/tests/

bun test --timeout 60000 \
  packages/core/tests/conjugation.test.ts \
  packages/core/tests/counters.test.ts \
  packages/core/tests/segmentation.test.ts

RUN_PARITY_TESTS=true bun test --timeout 60000 \
  packages/cli/tests/cli-parity.test.ts \
  packages/cli/tests/hard-cli-parity.test.ts
```

The complete `packages/core/tests` baseline is 824 passes, two documented skips, and
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

The checked-in Lisp outputs remain a historical guard, but the authoritative alpha
golden is a fresh canonical recording from the frozen current PostgreSQL analyzer.
It includes the current outputs for the two historical JMdict-version skips rather
than perpetuating skips in the browser suite.

## 4. Source and database snapshot

Every artifact manifest points to a committed `sources.lock.json`. The compiler
refuses a dirty source checkout or a source/oracle hash mismatch. The lock contains:

- repository commit and clean tree state;
- Bun, Node, PostgreSQL server, compiler format, and binary format versions;
- SHA-256 and byte size of every raw source file;
- hashes/counts of canonical PostgreSQL projections;
- hashes of the current semantic output golden and intentional-diff files.

Raw inputs currently available and frozen:

| Input | SHA-256 |
|---|---|
| `packages/data/JMdict_e.gz` | `391ec340c994cc82809ff667de74530d320bff9b3d062243b03d40b4826b1fe3` |
| `data/conj.csv` | `46a915afc385e05dfc8174226a5ee7bb4231e30870068e77d52e10a3433b5852` |
| `data/conjo.csv` | `b2919a24ecc7f3cdf9b9478eb13526b76cb086c53000316ecb1781ff947c14f3` |
| `data/kwpos.csv` | `485b5ac5d8a23a8bcd18e69ca2fdf21d231fb740780a6c7d7ccf40d65ed831e4` |
| `data/sources/extra.xml` | `4a056ebe608cb7bb5284e412688ff634d3516f3249a54d37b33645d7a266093b` |
| `data/sources/gyoseiku.csv` | `23f706ff84b5c27da86be1d7a6066630d34617e1769edb0673c91d258851ce3f` |
| `data/sources/jichitai.csv` | `328bd0779b1bb69a4c1bc3773c5ff71cffb59c825377ca7f31eafda3860e3af8` |

Code-defined errata, suffixes, counters, split rules, hints, scoring, penalties, and
synergies are locked by the clean repository commit. They must not be treated as
unversioned configuration.

The current oracle is PostgreSQL 16.15, database `ichiran_test`, UTF-8,
`C.UTF-8` collation. It has no build-metadata table, so normalized projections—not
surrogate IDs or a database file checksum—are the authoritative snapshot. Each
projection uses PostgreSQL text `COPY`, explicit `NULL '\N'`, explicit columns, and
an explicit bytewise order (`COLLATE "C"` for text). Surrogate row IDs, generated
target `seq`, timestamps, and absolute paths are excluded.

Required logical projections:

1. root entries and all root kana/kanji forms, including ord/common/tags,
   conjugatable/nokanji flags and best counterpart;
2. root score facts, POS sets, archived/prefer-kana flags, and restrictions;
3. all senses, glosses, and sense properties keyed by `(root seq, sense ord)`;
4. route-specific direct/morphology surface acceptance;
5. all 8,270,527 installed lookup-row/path matches, canonicalized to root and
   ordered rule tuples;
6. compiled logical suffix and counter caches;
7. resolved output of all analyzer reading hints.

Useful current projection checks:

| Projection | Rows | SHA-256 |
|---|---:|---|
| root entry `(seq,n_kanji,n_kana,primary_nokanji)` | 213,732 | `ff0e59236a2033efc2e8cef6bba6d9bec0f482b3cf5e6e01b1b3403d54dcb806` |
| all root form fields | 480,480 | `123bfd9ad25292005bf713c0ab9efa1a1186622125adb628a8dace19ff7f3537` |
| root senses | 246,494 | `55a15829c75f08dddbd1db4da851958ea7ed788ed46534d79d09cfed08144c70` |
| root glosses | 423,974 | `6efd2c73878b2b0f7e704c6f633fd089272b51ea55642586469b5a6f5a3618a6` |
| root sense properties | 396,408 | `67cdd9d4bb8a2203b00ac51766a8ffd3f0c04244751cfd7dbab1f34930dc56f1` |
| restricted readings | 6,732 | `700b35f550fb5e15bb9f8ba5401c61777343afae5e3e0b9092a5fe54cb43b093` |
| non-name Kanjidic `(character,reading,type)` | 35,542 | `0bea1400b8c60ec3949a20f6d7c1e11264c96e1216a32ace3722a837e3f6e8d7` |

The snapshot command to add to the repository is:

```bash
bun run alpha:oracle:snapshot -- \
  --database "$ICHIRAN_DB_URL" \
  --out browser-alpha/oracle/snapshot
```

It writes the lock, canonical projection hashes/counts, and canonical current-output
goldens. It performs no database writes.

## 5. Exhaustive parity gates

All gates operate on semantic keys, never generated IDs.

### 5.1 Artifact/data equality

- Exactly 213,732 root entries and 480,480 root form rows decode from the artifact.
- Every scoring-critical field and all 6,732 restriction rows equal the root
  projections.
- All 246,494 senses, 423,974 glosses, and 396,408 sense properties decode exactly.
- The route-aware scanner accepts exactly the same endpoints as current lookup.
  Current reference counts are 448,323 direct keys, 7,850,940 morphology keys,
  9,395 overlapping keys, and 8,289,868 keys in their union.
- Suffix output has 5,531 keys and 3,543 sequence/class mappings; counters have 748
  keys and 787 variants, with exact logical equality, not only equal counts.
- All 420 easy-hint definitions produce the recorded final hinted-kana output without
  a runtime Kanjidic table.

### 5.2 Morphology relation

Run reverse morphology against every one of the 8,270,527 installed route/form/path
matches. For each candidate compare route, surface, root, source form/reading,
ordered one- or two-stage properties, inherited ord/common facts, and exact forward
equality. Compare sorted semantic diff JSONL byte-for-byte with the intentional
allowlist. An unexpected diff or a stale allowlist row fails.

Known classes that must be enumerated, not matched by a broad predicate:

- remove secondary ghost-reading contamination: 18,944 legacy rows / 18,926
  `(root,finalSurface)` associations;
- remove potential/passive property cross-products: the snapshot has 15,331
  multi-property links; the exact false-property rows must be emitted by the
  exhaustive tool rather than accepted by the approximate 43,543 estimate;
- repair or remove the 55 stale non-root kana surfaces (54 rewritten
  `右に出る者はいない` forms and `じゃないで`) according to clean forward lineage;
- preserve the two explicit negative tombstones;
- preserve intentional copula/`じゃ` and `ございます` forms;
- preserve installed root/surface behavior for the 5,698 restriction-filter-divergent
  pairs unless a separate exact correction is reviewed.

### 5.3 End-to-end output

Run both APIs against the complete frozen corpus:

- clean `analyze`/`describe`/`romanize` output;
- legacy-shaped serialization;
- top-1, the checked top-3/top-5 cases, all entity fixtures, counters, numbers, and
  every recorded hinted reading.

Canonical output must be byte-identical to the frozen current oracle except exact
requests listed in the output-diff allowlist. A winning segmentation or any score
change is forbidden unless that exact request has reviewed legacy and alpha result
hashes and a dedicated regression fixture.

Proposed commands:

```bash
bun run alpha:compile -- \
  --snapshot browser-alpha/oracle/snapshot \
  --out dist/browser-alpha

bun run alpha:verify:data -- \
  --snapshot browser-alpha/oracle/snapshot \
  --manifest dist/browser-alpha/manifest.json

bun run alpha:verify:morphology -- \
  --snapshot browser-alpha/oracle/snapshot \
  --allow browser-alpha/oracle/intentional-morphology-diffs.jsonl

bun run alpha:verify:outputs -- \
  --snapshot browser-alpha/oracle/snapshot \
  --allow browser-alpha/oracle/intentional-output-diffs.jsonl
```

## 6. Intentional-difference format

Allowlisting is proof data, not runtime policy. Runtime patches/tombstones remain
small explicit compiler inputs. The allowlist contains no regex, range, glob, SQL
predicate, or “ignore field” switch.

Each morphology JSONL row is canonical JSON with this shape:

```json
{"alpha":null,"change":"remove-candidate","key":{"path":[{"formal":false,"negative":false,"pos":"v1","ruleOrdinal":0,"type":5}],"rootSeq":123,"route":"kana","sourceForm":"...","sourceReading":"...","surface":"..."},"legacy":{"common":null,"ord":0},"reason":"ghost-secondary-reading"}
```

`change` is one of `remove-candidate`, `add-candidate`, `remove-property`,
`repair-lineage`, or `change-score-fact`. `legacy` and `alpha` contain the exact
changed semantic fields. Repeated `reason` values are allowed, but every semantic
key is a separate row.

Each end-to-end output JSONL row is exact-request scoped:

```json
{"alphaResultSha256":"...","legacyResultSha256":"...","reason":"remove-ghost-secondary-reading","request":{"entities":[],"limit":1,"op":"analyze","text":"..."}}
```

The alpha result itself is a normal golden fixture. CI requires:

- actual diff keys equal allowlisted keys exactly;
- actual before/after values or hashes equal the row exactly;
- no duplicate rows and no unused/stale rows;
- each output diff has a dedicated named regression test.

Generated-`seq` replacement is performed by canonicalization and documented as a
global API change; it is not repeated as millions of allowlist rows.

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
- all persisted OPFS data plus precached app shell: **at most 64 MiB = 67,108,864
  bytes**;
- fresh-install wire payload for manifest, app shell, and all analyzer data using
  the release content encoding: **at most 25 MiB = 26,214,400 bytes**.

The size checker sums the manifest's actual persisted lengths and deterministic
release-encoded lengths; it does not use filesystem allocation size or estimate from
source TSV. Full dictionary details are mandatory in these totals. Full Kanjidic
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
  `fullJson[0..49]`, 50 complex auxiliary/morphology chains.

Report-only diagnostic groups:

- 459 segmentation fixtures of at most 12 characters;
- 50 long noun/compound cases, CLI indices 102-151;
- 50 hiragana/colloquial cases, CLI indices 152-201;
- 50 modern mixed-script/slang cases, CLI indices 202-251;
- the top-3/top-5 fixture calls, all 54 entity-hint cases, counters/numbers;
- `describe` random-access latency and Worker ready/first-call latency.

## 9. Performance procedure and gates

Benchmark the production PWA through its public Worker RPC, not a Node reader or an
internal function. Use pinned Playwright Chromium and CDP
`Emulation.setCPUThrottlingRate` with rate 6. The run records browser revision, OS,
CPU model, repository/artifact hashes, and raw sample timings.

Procedure:

1. Install the complete pack, close the page, reopen it offline, and await Worker
   readiness.
2. Call compact `analyze(text, {limit: 1})`; do not hydrate gloss details.
3. Disable only the whole-result memo/cache in benchmark mode. Dictionary/index and
   JIT state remain warm.
4. Make two unmeasured shuffled passes over each corpus, then ten measured passes
   using a fixed seed. Include `postMessage`, Worker computation, and response clone
   in each duration; exclude UI result rendering.
5. Compute p95 by nearest rank over all per-request samples and retain each raw
   sample in the report.

Hard gates on the designated reference desktop:

- ordinary top-1 p95 at 6x throttle: **at most 75 ms**;
- pathological-morphology top-1 p95 at 6x throttle: **at most 250 ms**;
- zero main-thread `longtask` entries over 50 ms during the RPC-only measured
  windows, and no analyzer-kernel import in the window bundle.

Performance gates say nothing about whether the kernel is JS or WASM. Top-N,
`describe`, initial install, Worker startup, and peak browser memory are reported but
not assigned an unagreed alpha threshold.

CPU throttling is a multiplier, not a hardware normalization. The first accepted
benchmark must designate and record the reference workstation/runner; subsequent
hard comparisons use that same runner class and pinned browser revision.

## 10. Offline/PWA functional gates

Automated browser tests must prove:

- manifest/data hashes are checked before marking an install complete;
- interruption midway through every artifact never exposes a partial active pack;
- restart after an incomplete or corrupted first install offers a clear reinstall;
- after a successful install, browser restart plus network blocking still supports
  top-1/top-N analysis, romanization, entities, and full `describe` gloss details;
- analysis and `describe` cause zero HTTP requests after installation;
- all analyzer CPU work runs in the dedicated Worker;
- persistence-request denial is handled and surfaced without corrupting data;
- explicit clear/reinstall removes only this app's stored pack and can reinstall it.

No update scheduler, delta update, background migration, Komi/Nemu integration, or
production hosting is required for this alpha.

## 11. Remaining blockers before the contract can pass

1. The exhaustive 8.27-million-path reverse differential does not exist yet. The
   current reverse prototype is sampled; implementing this verifier is a correctness
   prerequisite, not optional follow-up optimization.
2. The clean checkout does not contain the Kanjidic2 source file used to build the
   current database. The alpha can freeze the resolved 420 hint outputs and the
   normalized non-name reading projection above, but a from-scratch source-only
   rebuild later needs the original XML and its hash.
3. The current database has no provenance metadata proving which raw files built it.
   The normalized projection lock is therefore authoritative for alpha.
4. The existing CLI fixture normalizer sorts alternatives by generated `seq`; it
   cannot be reused. The semantic canonicalizer in this contract must be implemented.
5. A fixed reference benchmark runner and pinned Playwright Chromium revision have
   not yet been named. Until they are, latency results are informative but cannot be
   a stable cross-machine CI gate.
6. Actual iPhone 13 and iPhone 17 Pro Max measurements are deliberately deferred.
   Passing this alpha is a conservative desktop proxy, not production mobile signoff.
