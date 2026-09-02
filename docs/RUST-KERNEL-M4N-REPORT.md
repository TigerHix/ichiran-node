# Rust analyzer kernel M4N Node report

Date: 2026-09-01
Starting commit: `effd10f1cd4cfd6780760c8130030d287df35ca9`
Branch: `codex/rust-kernel-m1`
Qualified artifact: `portable-core-260118-baseline`
WASM SHA-256: `f4d17d3a406c1c8269acfc54cd4b08fcaaee795f1d273f8af93be6b25331fe5d`

## Decision

M4N passes. Node, CLI, and API load the same 1,119,555-byte WASM kernel as the
browser build. Their public analysis, romanization, retained legacy, CLI text/JSON,
and HTTP response shapes are unchanged. No native-library distribution matrix was
introduced.

The Node adapter owns only release verification, filesystem I/O, gzip streaming, and
temporary-file lifetime. It verifies compressed and decoded detail lengths and
SHA-256 digests while streaming `details.bin.gz` into a unique mode-0600 temporary
file. The runtime opens the detail header/index and later performs exact positional
reads without retaining a file descriptor. `dispose()` removes the owned file.

## Compatibility gates

| Gate | Result |
| --- | ---: |
| Qualified Node/CLI/API tests | **35/35 passed** |
| Hard CLI full JSON | 148/149 raw exact, 1 canonical tie, 0 mismatches |
| Ordinary CLI full JSON | 250/252 raw exact, 2 canonical ties, 0 mismatches |
| Standalone romanization | 5/5 exact |
| Info output | 3/3 exact |
| Upstream 260118 regressions | 27/27 exact |
| Real API response-shape tests | 2/2 passed |
| File verification/lifecycle tests | 5/5 passed |
| Concurrent Node legacy / HTTP detail sessions | 1/1, 1/1 passed |
| Core + Node focused matrix | 134 passed, 1 PostgreSQL-only skip, 0 failed |

The three canonical-only CLI witnesses use the repository's existing documented
equal-score path policy; they are not a new allowlist. The lower-level final WASM
differential remains 1,236/1,236 raw exact, 301/301 fallback exact, and 702/702
retained-detail exact with no equal-score canonicalization.

## Lazy detail proof

The qualified read audit records exactly two reads during open: the 96-byte header
and validated index prefix. Analysis performs zero additional detail reads. The first
`describe` performs one exact compressed-block read. Disposal removes the temporary
file, and each positional read opens and closes its own file handle.

Failure tests cover compressed checksum mismatch, an authenticated but truncated gzip
stream, decoded checksum mismatch, decoded-length overflow, identity assets, exact
subranges, partial-spool cleanup, sibling hot/WASM load failures, and runtime-open
cleanup. Source-commit rejection still occurs before asset reads.

## Node measurements

Measured with Node v22.18.0 and the qualified release:

| Measurement | Result |
| --- | ---: |
| End-to-end verified open | 465.6 ms |
| Rust runtime open portion | 128.7 ms |
| First analysis | 64.6 ms |
| Lexical p50 / p95, 100 samples | 0.55 / 0.92 ms |
| Morphology p50 / p95, 100 samples | 1.87 / 2.59 ms |
| Representative sentence p50 / p95, 100 samples | 3.09 / 4.11 ms |
| First lazy describe | 4.64 ms |
| Process RSS after representative work | 210,628,608 bytes |
| JS heap used after representative work | 8,331,872 bytes |
| WASM linear memory after representative work | 38,469,632 bytes |
| Rust resident kernel payload | 32,964,572 bytes |
| Rust resident detail data | 1,820,470 bytes |
| Peak RSS reported by `/usr/bin/time` | 218,402,816 bytes |

The complete 13,555,874-byte installed detail store is not retained in JS memory.
Temporary open-time bytes remain explicit in the shared runtime metric
(59,839,152 bytes), and the spool occupies only disk space after verification.

## Ownership

Embedders call `runtime.dispose()` after outstanding operations complete. The CLI
executable releases its process-wide runtime in `finally`. The API disposes exactly
once on server close, listen failure, or process exit. Real-release tests also own and
dispose their runtime explicitly.

## Quality gate

The requested thermo-nuclear review passes. The eager detail loader was deleted
instead of wrapped. The new Node file source is 172 lines, owns one concrete concern,
and does not add caching, queues, policy objects, or a second analyzer. Mutation of
temporary-file ownership stays in that source, while kernel disposal stays in the
shared runtime.

All package typechecks, `git diff --check`, and the qualified compatibility matrix
pass.

## Gate status

| Requirement | Decision |
| --- | --- |
| Same browser WASM kernel | PASS |
| Existing Node API and CLI behavior | PASS |
| Existing HTTP behavior | PASS |
| Thin filesystem/release adapter | PASS |
| Lazy random-access details | PASS |
| Explicit CLI/API/embedder lifetime | PASS |
| Size, latency, and memory measured | PASS |
| **Overall M4N gate** | **PASS** |
