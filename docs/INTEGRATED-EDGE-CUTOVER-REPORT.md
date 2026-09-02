# Integrated source-compiler and Rust-kernel cutover report

Date: 2026-09-02  
Branch: `codex/integrated-edge-cutover`  
Qualified code commit: `e7565078c8c7e0a29890328b491cb4ad701df73b`  
Baseline: `effd10f1cd4cfd6780760c8130030d287df35ca9`

## Decision

The combined Linux/WSL release candidate passes. The PostgreSQL-free source compiler
and the canonical Rust analyzer kernel now meet on one fresh format-v1 source release.
Browser, Node, CLI, and API use the same checked-in Rust WASM module through thin host
adapters. Native clients use the same Rust crate through ABI v3.

TypeScript owns the source compiler, pack/release verification, host I/O, browser
installation, and qualification orchestration. Its former analyzer is available only
from the explicit qualification entry point. PostgreSQL and upstream Lisp are frozen
migration authorities for this transition release and are not runtime or normal
source-build dependencies. No runtime allowlist or second production analyzer was
added. Grammar was not changed.

This report is a documentation-only descendant of the qualified code commit. The
release manifest and all executed code-bearing gates bind
`e7565078c8c7e0a29890328b491cb4ad701df73b`.

## Integrated history

All refs were fetched and matched the recorded heads. Five registered live worktrees
were clean. Stale prunable temporary worktree registrations were not treated as live
checkouts and were not deleted.

| Ref | Verified commit |
| --- | --- |
| `origin/main` | `effd10f1cd4cfd6780760c8130030d287df35ca9` |
| `origin/source-compiler-m2` | `4def0dba30c82c186b765048d42cdf4a5e7231d1` |
| `origin/codex/rust-kernel-m1` | `957d25862d3caee53b152775eb9079778afb172a` |

The source branch was merged first in
`437b8b254ea983fed8fa1da5c27de693e3d59d01`; its parents are the exact baseline and
source head. The Rust branch was merged second in
`cb16d102df53c3f4e6662eb740a84b2fb03921bd`; its second parent is the exact Rust head.
Both heads remain ancestors of the candidate.

The expected `README.md` and `packages/core/tools/oracle-parity.ts` conflicts were
resolved semantically. The combined oracle retains source-compiler pack verification,
v4 tested-release/report/attestation provenance, clean-release requirements, Rust
same-pack qualification, and `--fallback-out`. The fallback fixture continues to bind
the frozen oracle lock rather than the source compiler input lock.

## Qualified source release

Two output directories that did not exist before the final code commit were built
through `scripts/source-compiler-release-no-postgres.sh`:

- `work/integrated-source-e756507-a`
- `work/integrated-source-e756507-b`

The isolation probe reported Unix sockets hidden, loopback down, and ports 5432/5433
unavailable. Both builds activated generation
`7447e2d507decf4143b3e7f9e092504e878810f67f5676779597b7119a102796`.
`manifest.json`, `hot.bin.gz`, `details.bin.gz`, and `stats.json` were byte-identical
between the two directories.

| Artifact | Bytes | SHA-256 |
| --- | ---: | --- |
| `manifest.json` | 938 | `19338081fcaf47f44392ec21e657acf93f412f3eddfd26efac01b8c2c01065c8` |
| manifest authenticated body | — | `66e5987d73565204b1ea952d355d8a4eac42bef727e593dc01fc1337a04a33f4` |
| `hot.bin.gz` | 12,607,002 | `8648acfde1b3bc685bcd786a7718d2d980103c87fade3c3fdbdf416960859b9f` |
| installed `hot.bin` | 24,747,944 | `eb9c58204c624b1220bc257b910fc5df7e092133af09760ce6800b672b4bcd96` |
| `details.bin.gz` | 12,317,325 | `ad10bc4876d9a05224f62f5b438080ea1ff4e6a88ab3090be0f871035e95918a` |
| installed `details.bin` | 13,555,874 | `0fc45731d84fbb7c2ccf3ef5692d2f1ab01e538325f0ed50135da38e621aa151` |
| `stats.json` | 37,343 | `13443b5e5d50a34ce216d3ba440261cc3784de168ea9205e0c5df15b32bc39f2` |

The manifest records source lock SHA-256
`16f11739978e91922cf43337c6b801765214dbb0945509dec94b85321952b9cd`.
The release contains 8,393,704 accepted surfaces and 217,967 roots. All runtime
lookup-order exception surface, class, locator, and byte counts are zero.

The database-free data suite passed 138 tests with 12 explicit database-only skips,
861 assertions, and zero failures. It validated the source lock, manifest, direct
order attestation, generated-order release gate, complete semantic locator/candidate
closure, and blocked migration-oracle module boundary. The retained full ordering
spools were not regenerated; their compact, digest-bound direct/generated
attestations were validated by the clean release and test gates.

## Source compiler parity

The final v4 oracle report was generated against the fresh source release. PostgreSQL
was used only by this separately invoked migration-oracle process. After removing
`generatedAt` and the three expected candidate-specific tested-release provenance
fields, its sorted bytes are identical to the retained v4 report; both normalized
files have SHA-256
`05cceb0b78d4437038a8c12917e1d69badcf65699b667ec1af5f018df79e1111`.

| Comparison | Result |
| --- | ---: |
| Chosen authority | 1,225/1,241 byte exact; 16 individually attested order changes |
| Chosen path | 1,229/1,241 exact; 12 analyzer-order and 4 presentation-order changes |
| Fallback clean semantics | 296/301 exact; 5 individually attested order changes |
| Standalone romanization | 5/5 exact |
| Missing semantic locators or candidates | 0 |
| Errors | 0 |
| Retained reviewed samples | 21 |
| Runtime allowlist entries | 0 |

The newly emitted `--fallback-out` file is byte-identical to
`packages/rust-kernel/tests/fixtures/m3-fallback.json`, SHA-256
`dbc13ead615b8d70d2f3ecf38aeb7042361459856700a86844c5fe0db6706843`.
The retained-report validator passed against the new release: all 16 chosen rows and
five fallback rows form an exact bijection with the attestation. The regenerated
report intentionally cannot replace the retained report because the attestation also
pins the historical report's clean source commit; this provenance check was preserved.

All 21 intentional order changes, including exact inputs and old/new values, remain
the durable record in
[`source-compiler/M6-PACKED-PARITY-AUDIT.md`](source-compiler/M6-PACKED-PARITY-AUDIT.md).
Integration introduced no additional analyzer difference.

One additional browser witness exposed an opaque request-local ID consequence of the
new pack: for `analyze("猫", { limit: 1 })`, `candidateId` is `1` in the immutable pack
and `2` in the source-built pack in the chosen token and its repeated alternative;
every semantic field is unchanged. Both TypeScript and Rust produce `2` on the new
pack. Browser qualification now derives six exact DTO witnesses from the frozen
TypeScript oracle reading the verified active pack, then compares the Worker bytes
exactly. This is test-only same-pack evidence, not a runtime normalization or exception.

## Rust same-pack and native qualification

The source-release adapter verifies the manifest and compressed files first, streams
the two installed images into an exact temporary directory, and invokes the existing
same-pack differential. It does not create another release representation or analyzer.

| Gate | Result |
| --- | ---: |
| Raw analyzer operations | 1,236/1,236 exact |
| Standalone romanization | 5/5 exact |
| Retained detailed operations | 702/702 exact |
| Detail reads | 4,434; 116,459,085 total bytes |
| Largest detail read | 1,755,112 bytes |
| Eager whole-details-store read | false |
| Allowlist entries | 0 |

The checked-in WASM is 1,119,555 bytes, SHA-256
`f4d17d3a406c1c8269acfc54cd4b08fcaaee795f1d273f8af93be6b25331fe5d`.
The immutable-baseline differential also remained 1,236/1,236 raw, 5/5 romanization,
and 702/702 detailed exact, with 4,430 bounded detail reads and no allowlist.

Rustfmt, warning-denied Clippy over all targets/features, ordinary Cargo tests, and a
fresh temporary WASM/glue/declaration reproduction passed. Ordinary Rust totals are
104 passed, zero failed, and 19 explicit real-pack tests ignored; all 19/19 real-pack
tests passed when invoked against the digest-locked installed baseline.

The strict C11 ABI v3 callers passed:

- 1,236/1,236 clean operations plus 3/3 astral/lone-surrogate UTF-16 cases;
- 702/702 detailed operations (401 current-Lisp and 301 fallback authority);
- four canonical-tie witnesses, five romanizations, four lazy describes, and two
  corrupt-block recovery cases;
- owned success/error buffers and 14-symbol ownership contract;
- 128/128 concurrent clean calls and 32/32 concurrent detailed operations.

## Host and browser qualification

The immutable compatibility matrix passed 35/35 Node/CLI/API tests: Node release
verification and lazy lifecycle, legacy CLI text/JSON behavior, API response shapes,
concurrency, input bounds, and upstream regressions. The source pack is not run through
the old raw-Lisp CLI exact gate because that gate is designed to fail on the 16 M6
order changes; the full v4 source attestation above is its non-weakened replacement for
this pack. Rust/TypeScript agreement on that same source pack remains exact.

The default root command passed 164 tests, skipped 24 explicit opt-in real-pack or
PostgreSQL cases, and failed zero after the production package build and Rust artifact
reproduction. Root and compiler typechecks passed.

Browser unit tests passed 30/30. The production Vite build and Worker-only audit
passed. Playwright installed Chromium and passed all 13 serial scenarios, covering:

- first install, OPFS persistence, offline restart, and shell activation;
- corrupt manifest/transfer/installed bytes and interrupted installs;
- cross-tab ABA repair, runtime corruption recovery, update/downgrade behavior, and
  old-shell cleanup;
- unsupported environments and responsive behavior;
- exact Rust Worker same-pack DTO/UTF-16 witnesses;
- the calibrated exhaustive performance corpus.

| Browser measurement | Result |
| --- | ---: |
| Calibration ratio | 6.0519x |
| Worker ready / Rust open | 512.8 / 328.5 ms |
| First analysis | 29.1 ms |
| Lexical p50 / p95 | 0.7 / 24.9 ms |
| Morphology p50 / p95 | 1.0 / 24.1 ms |
| First detail | 3.1 ms |
| Transient bytes | 59,598,736 |
| WASM linear memory | 34,078,720 |
| Resident Rust kernel payload | 29,194,814 |
| Detail resident before / after | 1,755,112 / 1,820,470 bytes |
| Release download | 24,925,265 bytes |
| Production shell | 882,045 bytes |
| First install | 25,807,310 bytes (407,090 below 25 MiB) |

## Dependency and maintainability audit

- Product runtime packages contain no PostgreSQL client, database URL, Lisp process,
  or reference-package import. PostgreSQL remains a dev dependency because the frozen
  reference and migration tools still exist for one transition release.
- The source release module graph rejects both `@ichiran/reference-postgres` and the
  PostgreSQL client. The isolation wrapper independently makes sockets and networking
  unavailable.
- `TypeScriptOracleRuntime` is exported only by `@ichiran/core/qualification` and is
  used by qualification code. Production browser audit confirms it is not bundled.
- Rust owns analyzer semantics; TypeScript changes are host, release, compiler, and
  qualification boundaries only.
- No runtime allowlist, policy framework, database-shaped compiler adapter, queue,
  state-machine layer, or second production analyzer was introduced.
- Integration glue is small: a 25-line same-pack release adapter and a 16-line release
  verifier. The merged 1,621-line oracle tool predates integration; conflict resolution
  retained both workstreams rather than wrapping it in another tool.
- No new source file crosses 1,000 lines. Generated/binary source inputs are not
  abstraction findings. No `work`, `target`, `dist`, release pack, or test-result blob
  is tracked.
- `packages/grammar` has no diff from the baseline.

## Failures diagnosed and repaired

No failing gate was skipped or relabeled.

1. The merged root same-pack command initially expected installed files while the
   source compiler emits compressed release files. A thin verifier/streaming adapter
   now joins those existing contracts and cleans its exact temporary directory.
2. The first oracle invocation inherited a stale `.env` TCP port. The retained frozen
   database identity was verified and subsequent oracle runs used its explicit Unix
   socket URL. No build command used it.
3. One native detailed fixture serialized `counter` before `seq`/`gloss`; frozen
   TypeScript constructs `seq`, `gloss`, then `counter`, and the C product corpus
   already required that order. Only the stale fixture changed; a trial production
   Rust reorder was reverted, the checked-in WASM stayed byte-identical, and the full
   Rust/C gates passed.
4. A browser M1 witness assumed the immutable pack's request-local `candidateId`.
   Qualification now generates exact same-pack witnesses from the frozen TypeScript
   oracle; the complete 13-test browser run then passed.
5. Diagnostic commands aimed once at an unpacked differential directory and once at a
   relative C-harness path. Both failed with explicit missing-file errors and were
   rerun against the correct compressed release and absolute installed-pack path.
6. A regenerated v4 report was deliberately rejected as a replacement for the
   historical attestation because its source commit differs. The historical
   attestation was kept intact, validated against the new release, and the new report
   was independently proven content-identical after only candidate provenance fields
   were removed.

## Command ledger

The release qualification used these commands (output paths abbreviated only here):

```sh
git fetch --all --prune
git switch -c codex/integrated-edge-cutover origin/main
git merge --no-ff origin/source-compiler-m2
git merge --no-ff origin/codex/rust-kernel-m1

bun install --frozen-lockfile
bun run build:compiler
bun run typecheck:compiler
bun run typecheck

bun scripts/acquire-qualified-source-compiler-baseline.ts work/m2-baseline
sh scripts/source-compiler-release-no-postgres.sh --probe-only
bun run source:release -- baseline --out work/integrated-source-e756507-a \
  --pack-version ichiran-260118-integrated
bun run source:release -- baseline --out work/integrated-source-e756507-b \
  --pack-version ichiran-260118-integrated
cmp <release-a-file> <release-b-file> # all four files

env -u ICHIRAN_DB_URL -u DATABASE_URL ICHIRAN_RUN_DATABASE_TESTS=false \
  bun test packages/data/tests
bun run source:attestation -- --report data/source-compiler-parity-report.json \
  --release work/integrated-source-e756507-a
bun packages/core/tools/oracle-parity.ts --repository "$PWD" \
  --release work/integrated-source-e756507-a --source-compiler-pack \
  --allow-failures --out /tmp/integrated-source-e756507-parity.json \
  --fallback-out /tmp/integrated-source-e756507-fallback.json --samples 1241

bun run qualify:rust-same-pack -- work/integrated-source-e756507-a
bun packages/core/tools/rust-kernel-wasm-differential.ts \
  work/integrated-qualified-installed
bun run verify:rust-kernel
cargo test --release --locked --all-targets --all-features \
  --manifest-path packages/rust-kernel/Cargo.toml -- --ignored --test-threads=1
bash packages/rust-kernel/tests/run_c_harness.sh \
  /home/tiger/ichiran-node/work/integrated-qualified-installed

ICHIRAN_PACK_DIR=/home/tiger/ichiran-node/work/m2-baseline \
  RUN_PARITY_TESTS=true \
  bun test packages/node/tests packages/cli/tests packages/api/tests
bun run test
bunx playwright install chromium
ICHIRAN_E2E_NODE="$(command -v node)" \
  bun run --cwd packages/browser-demo qualify -- \
  --release /home/tiger/ichiran-node/work/integrated-source-e756507-a

bun test packages/data/tests/source-compiler-oracle-boundary.test.ts \
  packages/data/tests/source-compiler-release-evidence.test.ts \
  packages/core/tests/public-api.test.ts
git diff --check
```

## Remaining risks and handoff

The Linux/WSL candidate does not claim physical Safari/iPhone qualification. M4's
physical Safari checks and M5B's Mac-owned Apple targets, XCFramework, Swift wrapper,
simulator, device, leak, background-thread, and packaging validation remain pending.
The PostgreSQL and TypeScript analyzers must remain frozen and qualification-only for
the transition release; removing them is a later retirement change, not part of this
candidate.

The concise M5B procedure and exact ABI contract are in
[`packages/rust-kernel/MAC-HANDOFF.md`](../packages/rust-kernel/MAC-HANDOFF.md).
