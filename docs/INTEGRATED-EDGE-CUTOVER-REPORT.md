# Integrated source-compiler and Rust-kernel cutover report

Date: 2026-09-02
Branch: `codex/integrated-edge-cutover`
Qualification revision: the commit containing this report, which must equal the
branch head and every generated manifest's `sourceCommit`
Baseline: `effd10f1cd4cfd6780760c8130030d287df35ca9`

## Decision

This branch joins the reviewed PostgreSQL-free source compiler and the canonical Rust
analyzer kernel without replacing either implementation. Rust owns analyzer semantics.
TypeScript owns the source compiler, pack and release verification, browser lifecycle,
host adapters, and qualification orchestration. The frozen TypeScript and PostgreSQL
analyzers are available only to explicitly named transition qualification tools.
Upstream Lisp is fixture authority only. Grammar is unchanged.

The release candidate is qualified only when all commands in this report run from a
clean checkout of the report-bearing branch head. Documentation-only descendants are
not qualified revisions: the manifest, code, report, and pushed branch must all name
the same commit.

## Preserved merge ancestry

All input refs matched their recorded heads before integration:

| Ref | Verified commit |
| --- | --- |
| `origin/main` | `effd10f1cd4cfd6780760c8130030d287df35ca9` |
| `origin/source-compiler-m2` | `4def0dba30c82c186b765048d42cdf4a5e7231d1` |
| `origin/codex/rust-kernel-m1` | `957d25862d3caee53b152775eb9079778afb172a` |

The source branch was merged first by
`437b8b254ea983fed8fa1da5c27de693e3d59d01`. The Rust branch was merged second by
`cb16d102df53c3f4e6662eb740a84b2fb03921bd`. Both reviewed heads are ancestors of
the candidate.

The expected `README.md` and `packages/core/tools/oracle-parity.ts` conflicts were
resolved semantically. The oracle retains source-pack verification, v4 retained
report/attestation and clean-release validation, Rust same-pack qualification, and
`--fallback-out` generation.

## Release ownership and entry points

- `@ichiran/core` exports the Rust/WASM facade, public DTO/options, release/pack
  readers, and shared host/compiler utilities. TypeScript analyzer execution helpers
  are excluded from the main entry point and live under `@ichiran/core/qualification`.
- The canonical browser qualifier rejects `ICHIRAN_TYPESCRIPT_ORACLE=1`, removes the
  variable from production build/audit/E2E child environments, and requires a Rust
  build audit. The separately named `build:qualification-typescript-oracle` command
  exists only for frozen-oracle diagnostics.
- `@ichiran/data` exports and executes the source compiler. Its legacy PostgreSQL
  loader is private `migration:data` tooling. PostgreSQL and the reference package are
  development dependencies, not normal data-package dependencies.
- `source:release` runs the compiler directly. Linux namespace isolation is the
  separate `source:release:isolated` proof and has no `pg_isready` dependency.

`candidateId` is a request-local reference. It may change across calls, packs, or
runtimes and must not be persisted. Full Rust same-pack comparison includes it exactly;
the PostgreSQL/Lisp clean projection deliberately omits it because those authorities
have no corresponding field. A regression test locks both halves of that contract.

## Source compiler qualification

Two previously nonexistent output directories must be compiled independently from the
same clean report-bearing commit with `source:release:isolated`. All four release files
must compare byte-for-byte. Both manifests must contain the current 40-character HEAD.
The qualified January source semantics produce these stable installed identities:

| Installed asset | Bytes | SHA-256 |
| --- | ---: | --- |
| `hot.bin` | 24,747,944 | `eb9c58204c624b1220bc257b910fc5df7e092133af09760ce6800b672b4bcd96` |
| `details.bin` | 13,555,874 | `0fc45731d84fbb7c2ccf3ef5692d2f1ab01e538325f0ed50135da38e621aa151` |

The direct compiler and isolation wrapper invoke the same source entry point. Isolation
uses an empty bind mount over the PostgreSQL socket directory and a new network
namespace with loopback down. The source-release module-graph test rejects imports of
the PostgreSQL client, the frozen reference package, and browser-pack oracle loaders.

Required source results are:

| Comparison | Accepted result |
| --- | ---: |
| Chosen authority | 1,225/1,241 byte exact; 16 attested ordering changes |
| Chosen path | 1,229/1,241 exact; 12 analyzer-order and 4 presentation-order changes |
| Fallback clean semantics | 296/301 exact; 5 attested ordering changes |
| Standalone romanization | 5/5 exact |
| Missing semantic locators or candidates | 0 |
| Runtime allowlist entries | 0 |

The generated fallback file must remain byte-identical to
`packages/rust-kernel/tests/fixtures/m3-fallback.json`. Direct/generated ordering
attestations must validate with no additional or unreviewed difference.

## Rust, native, host, and browser qualification

The Rust/WASM same-pack differential runs on the fresh source release and requires:

- 1,236/1,236 raw analyzer operations exact against the frozen TypeScript same-pack
  oracle;
- 5/5 standalone romanizations exact;
- 702/702 retained detailed operations exact;
- zero allowlist entries and no eager whole-details-store read.

The native same-pack command materializes that same verified source release and drives
the real C ABI. It requires 1,236 clean operations, three UTF-16 edge cases, 702
detailed operations, five romanizations, four lazy describes, two corruption/recovery
cases, owned success/error buffers, 128 concurrent clean calls, and 32 concurrent
detailed operations. The immutable baseline C corpus remains a separate regression
gate; source-pack qualification no longer relies on it as a substitute.

`qualify:source-hosts` verifies the source release, rebuilds product packages, and runs
durable Node runtime, CLI executable, and HTTP API tests without the raw historical
ordering fixtures. Intentional M6 ordering changes remain governed by the v4 source
attestation, not by an exception in a production host test.

The canonical browser command builds only the Rust Worker, audits the production
bundle, installs through OPFS, restarts offline, exercises corruption/recovery and
updates, runs exact active-pack Rust witnesses, and executes the calibrated exhaustive
performance gate. Browser artifact bytes and measured timings are emitted by that
exact-revision run and belong in the final handoff report; they are not copied forward
from an earlier commit.

## Canonical qualification commands

These are the successful release gates. Diagnostic attempts are not represented as a
complete shell history.

```sh
git status --short
test "$(git rev-parse HEAD)" = "$(git rev-parse origin/codex/integrated-edge-cutover)"

bun install --frozen-lockfile
bun run typecheck:compiler
bun run typecheck
bun run test

bun run source:release:isolated -- baseline --out <release-a> --pack-version <version>
bun run source:release:isolated -- baseline --out <release-b> --pack-version <version>
cmp <release-a>/manifest.json <release-b>/manifest.json
cmp <release-a>/hot.bin.gz <release-b>/hot.bin.gz
cmp <release-a>/details.bin.gz <release-b>/details.bin.gz
cmp <release-a>/stats.json <release-b>/stats.json

env -u ICHIRAN_DB_URL -u DATABASE_URL ICHIRAN_RUN_DATABASE_TESTS=false \
  bun test packages/data/tests
bun run source:attestation -- --report data/source-compiler-parity-report.json \
  --release <release-a>
bun packages/core/tools/oracle-parity.ts --repository "$PWD" \
  --release <release-a> --source-compiler-pack --allow-failures \
  --out <temporary-report> --fallback-out <temporary-fallback> --samples 1241

bun run qualify:rust-same-pack -- <release-a>
bun run qualify:native-same-pack -- <release-a>
bun run qualify:source-hosts -- <release-a>
bun run verify:rust-kernel
bash packages/rust-kernel/tests/run_c_harness.sh <installed-immutable-baseline>

bun test packages/browser-demo/tests
bunx playwright install chromium
bun run --cwd packages/browser-demo qualify -- --release <release-a>

git diff --check origin/main...HEAD
git diff --exit-code effd10f1cd4cfd6780760c8130030d287df35ca9 -- packages/grammar
git status --short --branch
```

## Remaining gates and handoff

This Linux/WSL qualification does not claim physical Safari/iPhone, XCFramework,
Swift, simulator, Apple-device, leak, or Apple lifecycle results. M4 physical Safari
and M5B Apple packaging remain pending. The checked-in PostgreSQL and TypeScript
references must remain frozen and qualification-only for this transition release.

Source release directories are ignored generated artifacts and are not transferred by
Git. The Mac owner must obtain an attached/published copy of the exact four-file
release, verify its manifest against the checked-out candidate commit, and run both the
immutable and source same-pack native commands before Swift work. See
[`packages/rust-kernel/MAC-HANDOFF.md`](../packages/rust-kernel/MAC-HANDOFF.md).
