# Rust analyzer kernel M5A native handoff report

Date: 2026-09-01
Starting commit: `effd10f1cd4cfd6780760c8130030d287df35ca9`
M3 semantic checkpoint: `41ae665`
Branch: `codex/rust-kernel-m1`
Qualified artifact: `portable-core-260118-baseline`

## Decision

M5A passes. The native Rust crate exports one versioned C ABI for opening the
qualified pack and executing a complete analysis. The Linux C caller runs the same
six-suite analyzer corpus used for M3 qualification and compares every serialized
result byte-for-byte with the frozen TypeScript oracle.

The draft limit-only ABI could not represent the 54 entity cases or the corpus's
punctuation-normalization choices. ABI v2 replaces that argument with one borrowed
UTF-8 options JSON object. The operation remains coarse: one borrowed UTF-16 input
and one borrowed options document enter Rust, and one Rust-owned UTF-8 JSON buffer
returns. There is no candidate-level C traffic or second native analyzer.

## C parity gate

| Suite | Exact results |
| --- | ---: |
| Segmentation | 534/534 |
| CLI | 252/252 |
| Hard | 149/149 |
| Counters | 200/200 |
| Entities | 54/54 |
| Probes | 47/47 |
| **Total** | **1,236/1,236** |
| Concurrent exact calls | 128/128 |
| Owned error buffers | 1/1 |

The corpus generator reads the repository's canonical fixture definitions and obtains
expected bytes from the unchanged TypeScript analyzer. It normalizes only the
nondeterministic timing field to the kernel contract's `computeMs: 0`. The transport
preserves original UTF-16 code units, including astral and malformed-surrogate
fixtures, complete options, field order, scores, paths, alternatives, and ties.

The generator and C caller both lock the qualified `hot.bin` SHA-256:
`61f2882e086be7e0e1b6ba9000e76e0e735b22ea443146f628f04cf877ff6ae0`.
The TypeScript oracle source files are byte-unchanged from the exact starting commit.

## ABI and ownership

ABI v2 exports exactly these five symbols:

- `ichiran_kernel_abi_version`
- `ichiran_kernel_open`
- `ichiran_kernel_analyze_utf16`
- `ichiran_kernel_free`
- `ichiran_buffer_free`

The caller owns the hot-pack, UTF-16, and options buffers and may release them after
each call returns. Rust copies the pack into an opaque kernel owner. Every success or
error buffer is allocated by Rust and must be returned exactly once to
`ichiran_buffer_free`; every non-null kernel must be returned exactly once to
`ichiran_kernel_free` after outstanding calls finish.

One kernel may be shared across threads. Analysis calls serialize at its mutex so the
lazy caches retain one mutation owner. The fallible open and analyze entry points use
`catch_unwind`; a regression test proves a panic becomes an owned
`ICHIRAN_INTERNAL` JSON result rather than unwinding through C.

## Native artifacts

The qualified Linux release build produced:

| Artifact | Bytes |
| --- | ---: |
| `libichiran_kernel.a` | 32,140,340 |
| `libichiran_kernel.so` | 1,473,016 |
| Public C header | 2,945 |

These are host-link artifacts, not browser download bytes. Pack format v1 and the
shared analyzer semantics are unchanged.

## Mac handoff

`packages/rust-kernel/MAC-HANDOFF.md` documents the Linux, macOS, iOS device, and iOS
simulator target triples; locked cargo commands; symbol audit; simulator-only `lipo`;
XCFramework construction; Swift UTF-16 requirements; threading; pack ownership; and
result lifetime. The parity runner selects Linux or Darwin link flags and gives the
Mac agent one exact validation command.

No Mac, Swift, XCFramework, simulator, or physical-device execution is claimed from
WSL. Those validations remain M5B work.

## Quality gate

The requested thermo-nuclear review passes. The v1 limit-only signature was deleted
rather than retained as a compatibility layer. Format parsing and state mutation stay
inside the Rust kernel; C sees only a small opaque owner and one bulk operation. The
new test files are focused, no file crosses 1,000 lines, and no policy objects,
queues, state machines, or speculative host abstractions were added.

Strict rustfmt, all-target/all-feature clippy with warnings denied, pedantic C11
compilation, Bun bundling, Bash syntax, exported-symbol inspection, and
`git diff --check` pass. The Rust library reports 91 passed, 0 failed, with 9
real-pack tests ignored in the ordinary run.

## Gate status

| Requirement | Decision |
| --- | --- |
| Small versioned C ABI | PASS |
| Explicit allocation and free ownership | PASS |
| No Rust unwind through C | PASS |
| Full analyzer corpus through Linux C | PASS |
| Exact serialized output and UTF-16 behavior | PASS |
| Thread-sharing contract exercised | PASS |
| M3 parity lineage recorded | PASS |
| Standalone Mac handoff documentation | PASS |
| Mac/XCFramework/device execution | PENDING (M5B, Mac-owned) |
| **Overall M5A gate** | **PASS** |
