# Rust analyzer kernel cutover qualification

Date: 2026-09-01
Starting commit: `effd10f1cd4cfd6780760c8130030d287df35ca9`
Starting ref: `origin/main`
Branch: `codex/rust-kernel-m1`
Qualified artifact: `portable-core-260118-baseline`
Initial cutover code revision: `384bfa27277f0d832ae444b24f8d52c520d71ee2`
Review-remediation code revision: `a276cb5157bba1c25201a1b1909e89743b212638`

## Decision

The Linux/WSL Rust analyzer workstream passes. One host-neutral Rust crate now owns
the complete analyzer semantics, compiles natively and to browser WASM, is the default
kernel behind the existing browser Worker and Node/CLI/API adapters, and exports the
qualified C ABI handoff. Pack format v1, the immutable release, the TypeScript
differential oracle, the source compiler, the PostgreSQL producer, and grammar were
not changed.

This is a cutover candidate, not approval to merge to `main`. Complete roadmap M4
acceptance remains pending physical Safari runs. M5B remains pending the documented
Mac-owned Apple build, C/Swift boundary, simulator, and device qualification. Those
are the only remaining Rust-host acceptance risks; no Linux/WSL implementation or
parity work remains.

## Coherent checkpoints

| Revision | Checkpoint |
| --- | --- |
| `c3bec2a7c1e57151bdd0006c656cedacfa0e2e4e` | M1 genuine vertical slice |
| `79c7cd02a0179dcfa85f8ad97bad520399d94524` | M3 semantic foundations |
| `41ae665009d9034c84be512f817aa67b22ac3b85` | Complete M3 semantic parity |
| `4ad665beb64cbbc0eece754ff1eda6d665cd1a0a` | Browser Worker shared-WASM cutover |
| `a2e75a8e87b53ed932c66b93848a646ce78171aa` | Node lazy random-access detail source |
| `aa0d2d21375d7c7713d210026dc24da3ef5a5379` | M5A versioned C ABI and Mac handoff |
| `384bfa27277f0d832ae444b24f8d52c520d71ee2` | Bounded WSL exhaustive-browser teardown |
| `a276cb5157bba1c25201a1b1909e89743b212638` | Concurrent WASM sessions, ABI v3 product parity, and deterministic artifact gates |

All checkpoints were pushed only to `origin/codex/rust-kernel-m1`. Nothing was merged
to `main`.

## Milestone gates

| Milestone | Decision | Evidence |
| --- | --- | --- |
| M0 qualified TypeScript baseline | COMPLETE before this workstream | Immutable release supplied by the roadmap |
| M1 Rust feasibility spike | **PASS** | Native/WASM crate, real-pack slice, UTF-16, generated/detail paths, Worker, C draft, measurements |
| M2 source provenance lock | NOT PART OF THIS WORKSTREAM | Parallel source-compiler milestone; no producer change attempted |
| M3 full Rust semantic parity | **PASS** | 1,241/1,241 authoritative and 301/301 fallback exact |
| M4 browser WASM cutover, Linux/WSL portion | **PASS** | Default Rust Worker, complete Chromium matrix, size/memory/performance gates |
| M4 complete roadmap gate | **PENDING** | Physical Safari on iPhone 13 baseline and current target device |
| M4N Node/CLI/API cutover | **PASS** | Same WASM, preserved public behavior, lazy file-backed details |
| M5A Windows/WSL native handoff | **PASS** | ABI v3, complete Linux C product corpus, ownership/panic/thread checks, Mac README |
| M5B Mac-owned iOS packaging | **PENDING** | Requires Mac, XCFramework, Swift, simulator, and physical-device execution |
| M6 PostgreSQL-free source compiler | OUT OF SCOPE | Source compiler and producer were explicitly frozen |
| M7 full retirement | OUT OF SCOPE | Requires M2/M5B/M6 and an accepted transition release |
| **Rust analyzer Linux/WSL cutover candidate** | **PASS** | No remaining WSL parity or implementation gap |

## Exact parity evidence

| Gate | Result |
| --- | ---: |
| Current-Lisp-authoritative total | **1,241/1,241 exact** |
| Segmentation | 534/534 exact |
| Standalone romanization | 5/5 exact |
| Retained detailed output | 702/702 exact |
| Detailed current-Lisp authority | 401/401 exact |
| Detailed provenance-bound fallback | 301/301 exact |
| Raw presentation-free WASM operations | **1,236/1,236 exact** |
| Raw segmentation / CLI / hard | 534/534, 252/252, 149/149 |
| Raw counters / entities / probes | 200/200, 54/54, 47/47 |
| Provenance-bound fallback clean output | **301/301 exact** |
| Linux C clean serialized operations | **1,236/1,236 exact** |
| C explicit astral/lone-surrogate witnesses | 3/3 exact |
| C retained detailed operations | **702/702 exact** |
| C romanization / describe / corrupt recovery | 5/5, 4/4, 2/2 |
| C concurrent clean / detailed operations | 128/128, 32/32 |
| C owned error buffers | 4/4 |
| Equal-score differential allowlist | **0** |

The `ﾊｼ` equal-score witness retains exact root order
`[1581610, 1237410, 1476410]`. Scores, stable top-N paths, serialized field and array
order, and original UTF-16 code units are exact. Astral and lone high/low surrogate
fixtures pass through native, WASM, Worker, and C boundaries.

The final lazy-detail differential made 4,430 exact reads totaling 116,354,330 bytes;
the maximum individual read was 1,755,112 bytes and no whole-store read occurred.
The installed `details.bin` is 13,555,874 bytes.

## Final verification totals

| Gate | Result |
| --- | ---: |
| Ordinary Rust tests | 104 passed, 0 failed, 19 real-pack tests ignored |
| Qualified real-pack Rust tests | 19/19 passed |
| Node/CLI/API qualified tests | 35/35 passed |
| Core + Node focused matrix | 134 passed, 1 PostgreSQL-only skip, 0 failed |
| Browser unit tests | 30/30 passed |
| Browser non-benchmark Playwright | 12/12 passed |
| Browser exhaustive benchmark Playwright | 1/1 passed |
| Default root test (without opt-in real-pack/PostgreSQL suites) | 163 passed, 24 skipped, 0 failed |
| Root typecheck and production build | PASS |
| Strict rustfmt and all-target/all-feature Clippy | PASS |
| Pedantic Linux C11 build and symbol audit | PASS |
| Fresh temporary WASM/glue/declaration reproduction | PASS |
| Source/compiler/grammar scope audit | PASS |

The exhaustive Playwright qualification completed all assertions and exited cleanly
with Node v22.18.0 hosting Playwright. It used a calibrated 6.1632x single-core
contention proxy on CPU 31. The browser integration has no main-thread long task over
50 ms in the uncontended check.

The root `bun run test` gate now starts with `bun run verify:rust-kernel`. That gate
runs rustfmt, warning-denied Clippy, and ordinary Cargo tests, then builds the WASM,
JavaScript glue, and declarations in a fresh temporary target directory and compares
all four files byte-for-byte with the checked-in artifact. Rust 1.92.0,
`wasm-bindgen-cli` 0.2.127, and Binaryen 132.0.0 are pinned or strictly checked. A Rust
edit with stale generated output therefore fails the default verification path.
The root parity and browser-qualification commands use the same prerequisite.

The immutable-baseline differential remains the default and retains its hard-coded
hot, detail, WASM, and fallback-fixture identities. A separate explicit `--same-pack`
mode verifies an arbitrary format-v1 release against its own manifest and compares
both Rust clean output and retained detailed output with the frozen TypeScript oracle
reading those same bytes. This prepares cross-kernel validation of a later
source-compiler release without weakening the immutable baseline gate.

Both modes passed at the repaired revision. The immutable mode remained
1,236/1,236 raw exact, 5/5 standalone romanization exact, 301/301 fallback-clean
exact, and 702/702 retained-detail exact. The same-pack mode was independently
1,236/1,236 raw exact, 5/5 standalone romanization exact, and 702/702
retained-detail exact, with zero allowlist entries. Both identified hot SHA-256
`61f2882e086be7e0e1b6ba9000e76e0e735b22ea443146f628f04cf877ff6ae0`, detail
SHA-256 `0fc45731d84fbb7c2ccf3ef5692d2f1ab01e538325f0ed50135da38e621aa151`,
and WASM SHA-256
`f4d17d3a406c1c8269acfc54cd4b08fcaaee795f1d273f8af93be6b25331fe5d`.

The same-pack mode also passed against the source compiler's clean `6fbad4a740a7a616b5ae3c808961478a5179775c`
format-v1 pack: 1,236/1,236 raw, 5/5 standalone romanization, and 702/702
retained-detail operations were exact, with zero allowlist entries and no
whole-detail-store read. That pack's hot image is
24,747,944 bytes with SHA-256
`eb9c58204c624b1220bc257b910fc5df7e092133af09760ce6800b672b4bcd96`.
This is cross-kernel reader validation only; it does not claim or modify the separate
M2/M6 source-compiler gate.

## Artifact and first-install bytes

| Artifact | Bytes |
| --- | ---: |
| Final optimized WASM | **1,119,555** |
| Browser-distributed gzip WASM | **437,459** |
| Final application shell, including gzip WASM | **882,045** |
| Qualified release download | **24,981,169** |
| First-install total | **25,863,214** |
| 25 MiB target | 26,214,400 |
| Remaining margin | **351,186** |
| Linux static library | 32,315,664 |
| Linux shared library | 1,648,112 |
| Public C header | 5,056 |

The raw WASM SHA-256 is
`f4d17d3a406c1c8269acfc54cd4b08fcaaee795f1d273f8af93be6b25331fe5d`.
The immutable hot/detail SHA-256 values remain
`61f2882e086be7e0e1b6ba9000e76e0e735b22ea443146f628f04cf877ff6ae0` and
`0fc45731d84fbb7c2ccf3ef5692d2f1ab01e538325f0ed50135da38e621aa151`.

## Browser measurements

| Measurement | Result |
| --- | ---: |
| Worker ready / pack open / first analysis | 708.3 / 421.8 / 24.6 ms |
| Lexical p50 / p95 | 3.5 / 27.3 ms |
| Morphology p50 / p95 | 1.5 / 22.7 ms |
| First lazy detail request | 5.6 ms |
| Exhaustive ordinary p50 / p95, 990 samples | 27.3 / 53.0 ms |
| Exhaustive morphology p50 / p95, 500 samples | 51.8 / 91.6 ms |
| Exhaustive dense-boundary p50 / p95, 120 samples | 103.5 / 264.6 ms |
| Conservative transient bytes | 59,839,152 |
| WASM linear memory | 34,537,472 |
| Rust kernel payload | 29,301,346 |
| Resident detail before / after one read | 1,755,112 / 1,820,470 |
| Worker JS heap used | 756,940 |
| Steady JS heap used / total | 737,180 / 1,572,864 |

The calibrated interaction gates were 75 ms ordinary p95, 250 ms morphology p95,
and 500 ms dense-boundary p95; all pass without hiding WASM linear or transient
memory. The final exhaustive run used a 6.1632x single-core contention proxy on CPU
31 and measured 53.0, 91.6, and 264.6 ms p95 respectively.

## Node measurements

| Measurement | Result |
| --- | ---: |
| Verified open / Rust runtime open / first analysis | 465.6 / 128.7 / 64.6 ms |
| Lexical p50 / p95, 100 samples | 0.55 / 0.92 ms |
| Morphology p50 / p95, 100 samples | 1.87 / 2.59 ms |
| Representative sentence p50 / p95 | 3.09 / 4.11 ms |
| First lazy describe | 4.64 ms |
| Process RSS / peak RSS | 210,628,608 / 218,402,816 bytes |
| JS heap used | 8,331,872 bytes |
| WASM linear memory | 38,469,632 bytes |
| Rust kernel payload / resident detail | 32,964,572 / 1,820,470 bytes |

Node performs two small detail reads at open, zero during analysis, and one exact
block read for the first describe. Its verified temporary detail file is mode 0600,
is not retained in JS memory, and is removed by `dispose()`.

## Ownership and quality decision

TypeScript remains the single owner of browser release verification, installation,
OPFS generations, locking/recovery, and Service Worker lifecycle. Node owns only
release verification, file I/O, gzip spooling, and temporary-file lifetime. Rust is
the single owner of pack parsing, caches, candidates, scoring, search, projection,
serialization, and C-visible mutation. All host calls are coarse serialized
operations; there is no per-candidate JS/WASM or C traffic.

`TypeScriptOracleRuntime` is not exported from the normal `@ichiran/core` entry point;
it is available only from `@ichiran/core/qualification`. The Rust cutover also makes
the earlier experimental `IchiranRuntime.surface`, `.roots`, `.morphology`, `.support`,
and `.annotations` reader objects an explicit breaking change. The supported runtime
surface is `analyze`, `romanize`, `legacy`, `describe`, metrics/lifecycle operations,
and the narrow `entryIndexForSequence` compatibility lookup. No partial TypeScript
reader shim remains on the Rust runtime.

The ordinary `@ichiran/core` build runs the pinned Rust-to-WASM build before copying
generated output, and its `prepack` lifecycle runs that same build. A package dry run
confirmed the packed WASM has the qualified SHA-256 above. The stricter root
verification independently compiles all four generated files in a fresh temporary
Cargo target and rejects any byte drift.

The requested thermo-nuclear quality review was applied at every bulky milestone.
Superseded slice code, the duplicate browser runtime/generated WASM tree, eager Node
detail loading, and the unrepresentative v1 C analyze signature were deleted rather
than wrapped. No new Rust production file exceeds 1,000 lines, and no policy layer,
queue, state machine, or second analyzer was introduced.

The frozen TypeScript oracle files are byte-identical to the starting commit. The
branch contains no change under `packages/grammar`, no source-compiler or PostgreSQL
producer change, and no grammar commit. The protected local branches and worktrees
were not modified, renamed, merged, reset, deleted, pruned, or pushed.

## Remaining external risks

- Physical Safari behavior and memory must still be qualified on the iPhone 13
  baseline and the current target device before complete M4 acceptance.
- A Mac agent must build the Apple static libraries/XCFramework, exercise the actual
  C/Swift boundary, and validate simulator and physical-device ownership,
  backgrounding, offline restart, latency, and memory before M5B passes.
- Merge, release, TypeScript-oracle retirement, and source-compiler retirement remain
  review/transition decisions. This branch makes none of them autonomously.
