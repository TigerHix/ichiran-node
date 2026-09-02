# Rust analyzer kernel M4 browser report

Date: 2026-09-01
Starting commit: `effd10f1cd4cfd6780760c8130030d287df35ca9`
Branch: `codex/rust-kernel-m1`
Qualified artifact: `portable-core-260118-baseline`

## Decision

The WSL browser cutover candidate is complete. The existing TypeScript PWA remains
the sole owner of installation, OPFS generations, Service Worker updates, and release
verification; its Worker now opens the shared Rust-generated WASM kernel by default.
`ICHIRAN_TYPESCRIPT_ORACLE=1` retains the frozen TypeScript analyzer only as a
qualification build switch.

The WSL portion of M4 passes its parity, size, memory, lifecycle, and measured
performance requirements, including a clean final-tree exhaustive benchmark run.
The complete roadmap M4 gate remains pending because the required physical Safari
runs on the iPhone 13 baseline and current target device cannot be performed from WSL.

## Final artifacts

| Artifact | Bytes |
| --- | ---: |
| Final optimized WASM | 1,119,198 |
| Browser-distributed gzip WASM | 436,666 |
| Final application shell, including gzip WASM | 881,041 |
| Qualified release download | 24,981,169 |
| First-install total | **25,862,210** |
| Margin below 25 MiB | **352,190** |

The raw WASM SHA-256 is
`d8b35fbd8f3d62ef63724f4df833deb8c40a76053d1b3ce84459a81ff04d55eb`.
The production shell contains exactly one opaque `.wasm.gz.bin` asset and no raw WASM
asset. The benchmark corpus remains a separate 74,082-byte lazy chunk.

## WASM parity

| Gate | Result |
| --- | ---: |
| Raw presentation-free operations | **1,236/1,236 exact** |
| Segmentation | 534/534 exact |
| CLI corpus | 252/252 exact |
| Hard corpus | 149/149 exact |
| Counters | 200/200 exact |
| Entities | 54/54 exact |
| Probes | 47/47 exact |
| Provenance-bound fallback clean output | **301/301 exact** |
| Retained detailed output | **702/702 exact** |
| Detailed current-Lisp authority | 401/401 exact |
| Detailed frozen fallback | 301/301 exact |
| Equal-score allowlist | 0 |

The `ﾊｼ` witness preserves exact root order
`[1581610, 1237410, 1476410]`. The lazy detail differential performed 4,430 exact
range reads. Its largest request was 1,755,112 bytes and it never read the complete
13,555,874-byte detail store.

## Browser measurements

The final Rust Worker measurement reported:

| Measurement | Result |
| --- | ---: |
| Worker ready | 621.2 ms |
| Pack open | 358.1 ms |
| First analysis | 5.5 ms |
| Lexical p50 / p95 | 3.4 / 27.7 ms |
| Morphology p50 / p95 | 1.7 / 24.4 ms |
| Lazy detail request | 2.3 ms |
| Transient bytes | 59,839,152 |
| WASM linear memory | 34,537,472 |
| Kernel payload | 29,301,346 |
| Detail resident before / after | 1,755,112 / 1,820,470 |
| Worker JS heap used | 754,792 |
| Worker embedder heap | 31,904 |
| Worker backing storage | 38,694 |

The independent steady-memory test reported 734,004 used JS-heap bytes, 1,572,864
total JS-heap bytes, 31,824 embedder bytes, and 38,694 backing-store bytes.

The final completed exhaustive browser report used a calibrated 6.2013x single-core
contention proxy and passed all interaction thresholds:

| Corpus | Samples | p50 | p95 | Gate |
| --- | ---: | ---: | ---: | ---: |
| Ordinary | 990 | 27.5 ms | **52.3 ms** | <= 75 ms |
| Pathological morphology | 500 | 54.9 ms | **91.2 ms** | <= 250 ms |
| Dense contiguous boundary | 120 | 94.8 ms | **236.9 ms** | <= 500 ms |

The uncontended main-thread long-task list was empty. The final run completed the
benchmark and all assertions in roughly seven minutes on CPU 15, wrote the complete
report, and exited cleanly. Earlier diagnostics proved that WSL could delay an offline
persistent-context close or a killed Bun child indefinitely after every assertion had
already passed. Teardown is now bounded, stale child handles are unreferenced after
the bounded reap window, and the launcher can run Playwright with a real Node host via
`ICHIRAN_E2E_NODE`; the final qualification used Node v22.18.0.

## Browser verification

| Gate | Result |
| --- | ---: |
| Browser unit tests | 30/30 passed |
| Non-benchmark Playwright tests | 12/12 passed |
| Exhaustive benchmark Playwright test | 1/1 passed |
| Integrity and interrupted-install recovery | PASS |
| Cross-tab ABA and runtime corruption | PASS |
| Offline restart and shell upgrade | PASS |
| Responsive/installable PWA checks | PASS |
| Main-thread long tasks over 50 ms | 0 |
| Production release/build audit | PASS |
| TypeScript application/Worker/tool typechecks | PASS |

The Worker boundary remains coarse: analyze, retained serialization, romanization,
and detail requests cross as serialized operations. Candidate discovery and scoring
never chatter across JS/WASM. Chromium-invalidated file snapshots were removed; each
lazy detail range reacquires the committed OPFS file and performs one exact read.

## Quality gate

The requested thermo-nuclear review passes after centralizing the shared runtime in
`packages/core`, deleting the browser-only duplicate runtime and duplicate generated
WASM tree, replacing nullable install-state combinations with a discriminated ready
state, caching immutable Rust path facts, and compiling WASM-only legacy metadata out
of native builds. No new production file crosses 1,000 lines.

`cargo fmt --check`, `cargo clippy --all-targets --all-features -- -D warnings`,
browser typechecks, the production build audit, and `git diff --check` pass. The
final teardown changes are confined to the E2E harness; no production bundle or
analyzer behavior changed.

## Gate status

| Requirement | Decision |
| --- | --- |
| Rust replaces TypeScript inside the default Worker | PASS |
| TypeScript retains installer/lifecycle ownership | PASS |
| Exact WASM parity and stable ties | PASS |
| Lazy details and coarse boundary | PASS |
| 25 MiB first-install target | PASS |
| WSL size, memory, and performance evidence | PASS |
| Browser unit and non-benchmark E2E matrix | PASS |
| Idle-host exhaustive rerun | PASS |
| Physical Safari: iPhone 13 and current target | PENDING (Mac-owned) |
| **WSL M4 candidate** | **PASS** |
| **Complete roadmap M4 gate** | **PENDING** |
