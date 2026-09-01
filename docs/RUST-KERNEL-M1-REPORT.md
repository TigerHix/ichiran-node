# Rust analyzer kernel M1 report

Date: 2026-08-31
Starting commit: `effd10f1cd4cfd6780760c8130030d287df35ca9`
Branch: `codex/rust-kernel-m1`
Qualified artifact: `portable-core-260118-baseline`

## Decision

M1 passes. The vertical slice uses one host-neutral Rust crate for native, static/C,
and browser-WASM builds. It reads pack format v1 without changing the producer,
returns exact frozen-oracle output across the coarse Worker/WASM boundary, leaves
OPFS/installation/Service Worker ownership in TypeScript, and stays within the
existing product size, latency, and memory gates.

This is an M1 decision only. The experimental runtime deliberately supports only the
qualified slice. It is not the default browser analyzer and is not a claim of M3
semantic parity.

## Immutable inputs

| File | Bytes | SHA-256 |
| --- | ---: | --- |
| `hot.bin` | 24,857,288 | `61f2882e086be7e0e1b6ba9000e76e0e735b22ea443146f628f04cf877ff6ae0` |
| `details.bin` | 13,555,874 | `0fc45731d84fbb7c2ccf3ef5692d2f1ab01e538325f0ed50135da38e621aa151` |
| `hot.bin.gz` | 12,662,917 | `35d02c84d4cc531d299d7d5530994351b75bdba429d5276c20bc2f67cdc8d6d7` |
| `details.bin.gz` | 12,317,325 | `ad10bc4876d9a05224f62f5b438080ea1ff4e6a88ab3090be0f871035e95918a` |
| `manifest.json` | 927 | `1885e36e1485561b0d3528598df4c1422d7f5c2f3483a6a6f8a226f13d3e2ebd` |

The qualified tests verify the uncompressed byte counts and digests before using the
release. No release data is committed in this branch.

## Delivered slice

- Strict outer header, directory, alignment, zero-padding, version, and CRC checks.
- Strict surface, root, morphology, support, annotation, generated-block, and detail
  index readers over the real pack, with checked hostile-count arithmetic.
- Direct lexical (`猫`), genuine morphology (`食べた`), and generated-block
  (`忘れた`) analysis with exact scores and serialized DTOs.
- Exact astral, lone-high-surrogate, and lone-low-surrogate UTF-16 behavior.
- Generated blocks decode on demand (zero decoded at open); the explicit full
  validation path covers all 37 blocks and 9,417,412 decoded bytes.
- Details retain only the 1,755,112-byte prefix at open and fetch/decode one compressed
  block on demand. `details.bin` is never eagerly loaded.
- One serialized analysis call crosses JS/WASM. A detail lookup uses one coarse range
  request followed by one compressed-block/result call. Candidate enumeration and
  block decoding stay inside Rust.
- Experimental Worker integration under `ICHIRAN_RUST_M1=1`; TypeScript retains all
  release verification, OPFS, installation, locking, recovery, and Service Worker
  ownership. Runtime-module loading overlaps install inspection but cannot quarantine
  valid data; only explicit immutable-artifact integrity codes can do that.
- Versioned C ABI v1, explicit Rust-owned result buffers/free function, serialized
  mutation behind one kernel mutex, panic containment, Linux pthread harness, and
  Mac-agent handoff documentation.

## Exact verification totals

| Gate | Result |
| --- | --- |
| Rust unit/strict tests | 19/19 exact, 0 failed |
| Qualified real-pack tests | 5/5 exact, 0 failed |
| Qualified morphology corruption tests | 2/2 exact, 0 failed |
| Total executed Rust tests | 26/26, 0 failed |
| Live native TypeScript/Rust witnesses | 6/6 full DTOs exact |
| Browser Worker/WASM witnesses | 6/6 serialized full DTOs exact |
| Browser unit tests | 30/30, 100 assertions, 0 failed |
| Ordinary repository tests | 157 passed, 19 intentionally skipped, 0 failed, 3,800 assertions |
| Default TypeScript browser E2E | 12 passed, 1 Rust-only skip, 0 failed |
| Experimental Rust browser E2E | 1/1 slice gate passed; separate steady-memory probe passed |
| Linux C ABI | lexical 1, morphology 1, generated 1, UTF-16 3, concurrent calls 128 |
| Formatting / Clippy / typecheck | all pass; Clippy warnings denied |

The six differential inputs are `猫`, `食べた`, `忘れた`, U+1F600, one lone high
surrogate, and one lone low surrogate. The browser builds the last three from numeric
UTF-16 code units and verifies both request/result units and the entire serialized DTO.

## Final browser size

Built with Rust/Cargo 1.92.0, `wasm32-unknown-unknown`, and wasm-bindgen 0.2.127.

| Artifact | Bytes |
| --- | ---: |
| Final WASM (`db2c01c3...f447f`) | 313,621 |
| Rust runtime glue chunk | 12,686 |
| Analyzer Worker chunk | 21,025 |
| Final shell excluding analyzer release files | 850,656 |
| Compressed release files plus manifest | 24,981,169 |
| First-install total | **25,831,825** |
| 25 MiB target | 26,214,400 |
| Headroom | **382,575** |

The decision uses the finalized Vite output and final Service Worker, not an
intermediate `.wasm` file.

## Final browser measurements

The interaction run used the repository's pinned-CPU harness with 6.16x calibrated
contention. Values are Worker RPC wall times, not direct main-thread calls.

| Measurement | Rust M1 |
| --- | ---: |
| Worker ready | 710.6 ms |
| Runtime open | 539.8 ms |
| First analysis | 1.1 ms |
| Lexical p50 / p95 | 0.4 / 15.0 ms |
| Morphology p50 / p95 | 0.6 / 14.7 ms |
| Lazy detail read | 19.6 ms |
| Conservative open transient | 59,773,616 bytes |
| WASM linear memory after detail | 33,161,216 bytes |
| Kernel payload accounting | 25,903,006 bytes |
| Detail resident before / after one read | 1,755,112 / 1,820,470 bytes |
| Measured Worker JS heap during interaction run | 700,808 bytes |
| Separate post-GC Worker JS heap / backing storage | 684,060 / 33,792 bytes |

The clean default TypeScript post-GC probe measured 2,065,212 bytes of Worker JS heap
and 36,047,847 bytes of backing storage. The counters are not allocation-identical
between JS and WASM, so the decision keeps the explicit 33,161,216-byte WASM linear
memory visible rather than folding it into a misleading single number. Rust remains
below the 96 MiB steady and 128 MiB conservative transient gates and does not introduce
a material memory or interaction regression.

## M1 gate decisions

| Requirement | Decision |
| --- | --- |
| One native/browser-WASM crate | PASS |
| Strict real-pack header/format/checksum | PASS |
| Surface automaton lookup | PASS |
| Direct lexical exact parity | PASS |
| Genuine morphology exact parity | PASS |
| Generated inflation and public result | PASS |
| Lazy random-access detail retrieval | PASS |
| Astral and malformed-surrogate fixtures | PASS |
| Coarse serialized operation boundary | PASS |
| TypeScript-owned Worker/OPFS/SW/install lifecycle | PASS |
| Draft C header, Linux harness, Mac handoff | PASS |
| Final size/latency/transient/steady measurements | PASS |
| **Overall M1 gate** | **PASS** |

## Remaining risks and next gate

- M1 analysis is intentionally a whole-input, top-one slice. General segmentation,
  stable top-N path search, full scoring/rules, suffixes, counters, projection,
  retained legacy serialization, and all utilities remain M3 work.
- Browser Safari and physical iPhone validation belong to M4/M5B and cannot be claimed
  from WSL.
- The C ABI is a draft at M1. It must be rerun over the complete M3 corpus and tied to
  the M3-qualified revision before the native handoff gate passes.
- No broad allowlist exists. M3 must reach 1,241/1,241 current-Lisp-authoritative and
  301/301 fallback comparisons with exact ties, scores, UTF-16 spans, and output.
