# Rust analyzer kernel M5A native handoff report

Date: 2026-09-01
Starting commit: `effd10f1cd4cfd6780760c8130030d287df35ca9`
Branch: `codex/rust-kernel-m1`
Qualified artifact: `portable-core-260118-baseline`

## Decision

M5A passes on Linux/WSL. ABI v3 exposes the complete agreed native analyzer surface:
clean analysis, analyzer-backed romanization, lazy random-access details/describe, and
retained detailed legacy output. Swift can remain a pack/file/lifecycle host and does
not need to reconstruct analyzer or presentation semantics.

The ABI keeps three obvious opaque owners: one kernel, one detail index/cache, and one
independent legacy operation. It does not introduce callbacks, queues, per-candidate
traffic, or a second analyzer implementation.

## Native C qualification

| Gate | Exact result |
| --- | ---: |
| Clean segmentation | 534/534 |
| Clean CLI | 252/252 |
| Clean hard | 149/149 |
| Clean counters | 200/200 |
| Clean entities | 54/54 |
| Clean probes | 47/47 |
| **Clean total** | **1,236/1,236** |
| Explicit astral/lone-surrogate UTF-16 | 3/3 |
| Detailed current-Lisp authority | 401/401 |
| Detailed provenance fallback | 301/301 |
| **Detailed total** | **702/702** |
| Current-Lisp canonical ties | 3/3 |
| Fallback canonical ties | 1/1 |
| Standalone romanization | 5/5 |
| Lazy describe | 4/4 |
| Corrupt-block rejection and retry | 2/2 |
| Owned error buffers | 4/4 |
| Concurrent clean calls | 128/128 |
| Concurrent detailed operations | 32/32 |

The clean C caller compares Rust bytes directly with the frozen TypeScript oracle and
adds explicit astral-pair, lone-high-surrogate, and lone-low-surrogate witnesses
without changing the 1,236 corpus accounting. For
detailed output, the generator first requires every portable TypeScript result to be
canonical-exact against its current-Lisp or provenance-bound fallback authority and
requires the exact identities of the three known current-Lisp equal-score tie-order
cases and the one known fallback tie (`probes:26`). The generator then preserves the
TypeScript oracle's raw object-field and array order, and the C caller compares Rust
bytes exactly. There is no broad allowlist;
any new, removed, or renamed tie divergence fails corpus generation.

The pack inputs are locked to:

- `hot.bin` SHA-256:
  `61f2882e086be7e0e1b6ba9000e76e0e735b22ea443146f628f04cf877ff6ae0`
- `details.bin` SHA-256:
  `0fc45731d84fbb7c2ccf3ef5692d2f1ab01e538325f0ed50135da38e621aa151`

## ABI v3 and ownership

ABI v3 exports 14 symbols:

- `ichiran_kernel_abi_version`
- `ichiran_kernel_open`
- `ichiran_kernel_analyze_utf16`
- `ichiran_kernel_romanize_utf16`
- `ichiran_detail_prefix_length`
- `ichiran_detail_store_open`
- `ichiran_detail_store_range`
- `ichiran_detail_store_decode`
- `ichiran_kernel_legacy_begin_utf16`
- `ichiran_kernel_legacy_step`
- `ichiran_kernel_free`
- `ichiran_detail_store_free`
- `ichiran_legacy_operation_free`
- `ichiran_buffer_free`

The host owns installation and all file reads. Rust copies the hot image and the
verified detail prefix, retains at most one decoded detail block in the shared store,
and never eagerly loads `details.bin`. Describe is range/read/decode. Detailed legacy
is begin/step: a missing step returns one typed `IchiranDetailRange`, and the next step
accepts exactly that compressed range.

Each legacy handle owns its analysis, serializer session, romanization method, and
pending request. Supplied-block decode and serializer retry are atomic under the fixed
operation → kernel → detail-store lock order. This prevents concurrent operations from
overwriting another operation's state while retaining one mutation owner for kernel
and detail caches.

All input buffers are borrowed only for their call. Every result and step buffer,
including empty successes and errors, is Rust-owned and returned once through
`ichiran_buffer_free`. Handles have matching free functions and cannot be freed while
in flight. UTF-16 input, entity offsets, result spans, astral pairs, and lone
surrogates remain lossless. All fallible entries use panic containment; focused unit
tests prove both ordinary and typed-step panics become owned `ICHIRAN_INTERNAL`
results rather than unwinding through C.

## Native artifacts

| Artifact | Bytes |
| --- | ---: |
| `libichiran_kernel.a` | 32,315,664 |
| `libichiran_kernel.so` | 1,648,112 |
| `include/ichiran_kernel.h` | 5,056 |

These are native link artifacts, not browser installation bytes. Pack format v1 and
the analyzer semantics are unchanged.

## Quality and handoff

The thermo-nuclear maintainability review passes. `ffi.rs` is 779 lines, the focused
product C caller is 583 lines, and no changed file crosses 1,000 lines. Detail retry
state lives in its opaque operation instead of scattered flags or a runtime-wide
queue. The C boundary reuses `Kernel`, `DetailStore`, and `LegacyDetailSession`; it
does not duplicate analyzer rules.

Rustfmt, all-target/all-feature Clippy with warnings denied, Rust tests, strict C11
compilation, the complete Linux C qualification, exported-symbol inspection, and
`git diff --check` pass. The ordinary Rust run has 104 passed and 19 ignored
real-pack tests; the qualified release run passes all 19/19 ignored tests.
`packages/rust-kernel/MAC-HANDOFF.md` now gives the Mac agent
the exact 14-symbol contract, target commands, Swift call flow, ownership rules, and
the complete corpus gate without requiring an ABI redesign.

## Gate status

| Requirement | Decision |
| --- | --- |
| Complete small versioned C ABI | PASS |
| Clean and detailed C corpus | PASS |
| Romanization and lazy describe | PASS |
| Explicit allocation/free ownership | PASS |
| Panic and corrupt-block containment | PASS |
| Independent concurrent legacy sessions | PASS |
| Host-owned pack installation and file reads | PASS |
| Standalone Mac handoff | PASS |
| Mac/XCFramework/Swift/simulator/device execution | PENDING (M5B, Mac-owned) |
| **Overall M5A gate** | **PASS** |
