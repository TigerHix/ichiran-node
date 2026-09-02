# Rust analyzer kernel cutover qualification

Date: 2026-09-01
Starting commit: `effd10f1cd4cfd6780760c8130030d287df35ca9`
Starting ref: `origin/main`
Branch: `codex/rust-kernel-m1`
Qualified artifact: `portable-core-260118-baseline`
Cutover code revision: `384bfa27277f0d832ae444b24f8d52c520d71ee2`

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
| M5A Windows/WSL native handoff | **PASS** | ABI v2, Linux C corpus, ownership/panic/thread checks, Mac README |
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
| Linux C serialized operations | **1,236/1,236 exact** |
| C concurrent calls / owned errors | 128/128, 1/1 |
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
| Ordinary Rust tests | 102 passed, 0 failed, 19 real-pack tests ignored |
| Qualified real-pack Rust tests | 19/19 passed |
| Node/CLI/API qualified tests | 33/33 passed |
| Core + Node focused matrix | 134 passed, 1 PostgreSQL-only skip, 0 failed |
| Browser unit tests | 30/30 passed |
| Browser non-benchmark Playwright | 12/12 passed |
| Browser exhaustive benchmark Playwright | 1/1 passed |
| Root typecheck and production build | PASS |
| Strict rustfmt and all-target/all-feature Clippy | PASS |
| Pedantic Linux C11 build and symbol audit | PASS |
| Source/compiler/grammar scope audit | PASS |

The exhaustive Playwright qualification completed all assertions and exited cleanly
with Node v22.18.0 hosting Playwright. It used a calibrated 6.2013x single-core
contention proxy on CPU 15. The browser integration has no main-thread long task over
50 ms in the uncontended check.

## Artifact and first-install bytes

| Artifact | Bytes |
| --- | ---: |
| Final optimized WASM | **1,119,198** |
| Browser-distributed gzip WASM | **436,666** |
| Final application shell, including gzip WASM | **881,041** |
| Qualified release download | **24,981,169** |
| First-install total | **25,862,210** |
| 25 MiB target | 26,214,400 |
| Remaining margin | **352,190** |
| Linux static library | 32,140,340 |
| Linux shared library | 1,473,016 |
| Public C header | 2,945 |

The raw WASM SHA-256 is
`d8b35fbd8f3d62ef63724f4df833deb8c40a76053d1b3ce84459a81ff04d55eb`.
The immutable hot/detail SHA-256 values remain
`61f2882e086be7e0e1b6ba9000e76e0e735b22ea443146f628f04cf877ff6ae0` and
`0fc45731d84fbb7c2ccf3ef5692d2f1ab01e538325f0ed50135da38e621aa151`.

## Browser measurements

| Measurement | Result |
| --- | ---: |
| Worker ready / pack open / first analysis | 621.2 / 358.1 / 5.5 ms |
| Lexical p50 / p95 | 3.4 / 27.7 ms |
| Morphology p50 / p95 | 1.7 / 24.4 ms |
| First lazy detail request | 2.3 ms |
| Exhaustive ordinary p50 / p95, 990 samples | 27.5 / 52.3 ms |
| Exhaustive morphology p50 / p95, 500 samples | 54.9 / 91.2 ms |
| Exhaustive dense-boundary p50 / p95, 120 samples | 94.8 / 236.9 ms |
| Conservative transient bytes | 59,839,152 |
| WASM linear memory | 34,537,472 |
| Rust kernel payload | 29,301,346 |
| Resident detail before / after one read | 1,755,112 / 1,820,470 |
| Worker JS heap used | 754,792 |
| Steady JS heap used / total | 734,004 / 1,572,864 |

The calibrated interaction gates were 75 ms ordinary p95, 250 ms morphology p95,
and 500 ms dense-boundary p95; all pass without hiding WASM linear or transient
memory.

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
