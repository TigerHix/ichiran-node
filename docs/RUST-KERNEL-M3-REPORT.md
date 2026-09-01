# Rust analyzer kernel M3 report

Date: 2026-08-31
Starting commit: `effd10f1cd4cfd6780760c8130030d287df35ca9`
Branch: `codex/rust-kernel-m1`
Qualified artifact: `portable-core-260118-baseline`

## Decision

M3 passes. The host-neutral Rust crate now owns the complete analyzer semantics:
utilities and readers, lexical and generated materialization, recursive suffixes,
counters, rules, scoring, stable top-N search, projection, standalone romanization,
and retained compact/detailed serialization. Pack format v1 and the TypeScript oracle
remain unchanged.

## Parity gates

| Gate | Result |
| --- | ---: |
| Current-Lisp-authoritative total | **1,241/1,241 exact** |
| Segmentation | 534/534 exact |
| Standalone romanization | 5/5 exact |
| Retained detailed output | 702/702 exact |
| Detailed current-Lisp snapshots | 401/401 exact |
| Detailed provenance-bound fallback | 301/301 exact |
| Raw presentation-free TypeScript/Rust operations | 1,236/1,236 exact |
| Provenance-bound fallback clean comparisons | 301/301 exact |
| Equal-score/path ordering allowlist | 0 |
| Ordinary Rust tests | 99 passed, 0 failed |
| Qualified real-pack Rust tests | 19 passed, 0 failed |

The 1,241 authoritative total is the roadmap corpus: 534 segmentation comparisons,
five standalone romanization comparisons, and 702 detailed legacy comparisons. The
raw 1,236-operation differential independently covers all 534 segmentation requests
and all 702 analyzer requests with exact object values and exact array order, without
equal-score canonicalization. The `ﾊｼ` tie preserves root order
`[1581610, 1237410, 1476410]` in both implementations.

The 301 PostgreSQL-only cases are stored in
`packages/rust-kernel/tests/fixtures/m3-fallback.json`, bound to the immutable release,
and use only the repository's documented canonical equal-score policy. Its SHA-256 is
`dbc13ead615b8d70d2f3ecf38aeb7042361459856700a86844c5fe0db6706843`.

## Lazy details and boundaries

The detailed differential opens only the 96-byte header and validated detail index
prefix. A retained operation asks the host for one exact compressed range at a time;
the existing detail reader checks compressed length, decoded length, and checksum.
The per-operation session retains parsed entries across one-block cache eviction.
Analysis is not rerun while details are hydrated, and `details.bin` is never eagerly
loaded by the kernel.

Analysis and retained output each cross the host boundary as one serialized buffer.
Candidate discovery, recursive materialization, scoring, search, projection, and
serialization stay in Rust; there is no per-candidate host chatter.

## Quality gate

The explicit thermo-nuclear review passes after deleting the superseded M1 scorer,
the old suffix-selection implementation, broad dead-code suppressions, unused reader
APIs, and the only production `unreachable!` in the retained renderer. New semantic
ownership is split into focused modules; no production file crosses 1,000 lines.

`cargo clippy --all-targets --all-features -- -D warnings`, `cargo fmt --check`, and
`git diff --check` pass. The two fail-closed differential tools retain every mismatch,
enforce corpus/result counts, and contain no exception allowlist.

## Gate status

| Requirement | Decision |
| --- | --- |
| Readers and utilities | PASS |
| Lexical/generated/suffix/counter materialization | PASS |
| Rules, scoring, stable top-N search | PASS |
| Clean projection, details, legacy serialization | PASS |
| UTF-16 spans, malformed surrogates, stable ties | PASS |
| Native TypeScript/Rust differential | PASS |
| 1,241 authoritative comparisons | PASS |
| 301 fallback comparisons | PASS |
| Strict maintainability review | PASS |
| **Overall M3 gate** | **PASS** |

Browser WASM cutover, Node/CLI/API adapter cutover, and the full C-corpus handoff are
separate M4, M4N, and M5A gates and are not claimed by this report.
