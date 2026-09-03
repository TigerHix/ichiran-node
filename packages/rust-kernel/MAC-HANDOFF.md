# Mac native handoff

## Integrated candidate

Use the final pushed head named in the integration task handoff. This tracked guide is
a recipe, not post-commit evidence. Pin the remote head before building and keep the
checkout detached at that exact commit:

```sh
git fetch origin codex/integrated-edge-cutover
candidate=$(git rev-parse origin/codex/integrated-edge-cutover)
git switch --detach "$candidate"
test "$(git rev-parse HEAD)" = "$candidate"
test -z "$(git status --porcelain=v1 --untracked-files=all)"
```

The integrated Linux/WSL report is
[`docs/INTEGRATED-EDGE-CUTOVER-REPORT.md`](../../docs/INTEGRATED-EDGE-CUTOVER-REPORT.md).

The source-built pack qualified with this code has installed hot SHA-256
`eb9c58204c624b1220bc257b910fc5df7e092133af09760ce6800b672b4bcd96`
and details SHA-256
`0fc45731d84fbb7c2ccf3ef5692d2f1ab01e538325f0ed50135da38e621aa151`.
The source release lives under an ignored `work/` directory and is not contained in or
transferred by Git. Obtain the exact attached or published `manifest.json`,
`hot.bin.gz`, `details.bin.gz`, and `stats.json` from the integration handoff. Verify
that `manifest.sourceCommit` equals `candidate` and that the decoded identities above
match before using it. Do not substitute an unverified local pack. Run the immutable C
corpus first, then the source same-pack C corpus, then exercise that source pack through
the Swift wrapper for install/open, analysis, romanization, describe, retained legacy,
restart, corruption/recovery, and concurrent background calls. Do not port analyzer or
presentation logic into Swift.

M5B remains open: build the device and simulator archives, create the XCFramework,
write the thin Swift ownership/file adapter, and run simulator plus physical-device
memory, leak, lifecycle, and UTF-16 tests. Physical Safari qualification is a separate
remaining M4 gate.

This crate is the sole analyzer implementation for native and browser builds. ABI v3
exposes clean analysis, analyzer-backed romanization, lazy detail lookup, and retained
detailed/legacy presentation. Swift owns pack installation and file reads; it must not
recreate analyzer, detail, or presentation semantics.

## Build targets

Install Bun 1.3.5 for source-release and host-qualification commands. Use rustup
with the checked-in Rust 1.92.0 toolchain and locked dependencies for native builds:

```sh
rustup target add aarch64-apple-darwin x86_64-apple-darwin
cargo build --release --locked --target aarch64-apple-darwin
cargo build --release --locked --target x86_64-apple-darwin
lipo -create \
  target/aarch64-apple-darwin/release/libichiran_kernel.a \
  target/x86_64-apple-darwin/release/libichiran_kernel.a \
  -output libichiran_kernel.a
```

For the later M5B iOS package:

```sh
rustup target add aarch64-apple-ios aarch64-apple-ios-sim x86_64-apple-ios
cargo build --release --locked --target aarch64-apple-ios
cargo build --release --locked --target aarch64-apple-ios-sim
cargo build --release --locked --target x86_64-apple-ios
lipo -create \
  target/aarch64-apple-ios-sim/release/libichiran_kernel.a \
  target/x86_64-apple-ios/release/libichiran_kernel.a \
  -output target/libichiran_kernel-simulator.a
xcodebuild -create-xcframework \
  -library target/aarch64-apple-ios/release/libichiran_kernel.a -headers include \
  -library target/libichiran_kernel-simulator.a -headers include \
  -output target/IchiranKernel.xcframework
```

Never combine device and simulator archives with `lipo`. The XCFramework owns those
separate platform slices.

## ABI v3 audit

Import `include/ichiran_kernel.h` through a module map. Confirm the universal archive
exports these 14 symbols:

```sh
nm -gU libichiran_kernel.a | awk '/_ichiran_/ { print $3 }' | sort -u
```

```text
_ichiran_buffer_free
_ichiran_detail_prefix_length
_ichiran_detail_store_decode
_ichiran_detail_store_free
_ichiran_detail_store_open
_ichiran_detail_store_range
_ichiran_kernel_abi_version
_ichiran_kernel_analyze_utf16
_ichiran_kernel_free
_ichiran_kernel_legacy_begin_utf16
_ichiran_kernel_legacy_step
_ichiran_kernel_open
_ichiran_kernel_romanize_utf16
_ichiran_legacy_operation_free
```

Reject the library at integration time unless `ichiran_kernel_abi_version()` equals
`ICHIRAN_KERNEL_ABI_VERSION`.

## Swift wrapper flow

The host verifies and installs format-v1 `hot.bin` and `details.bin`. Pass the complete
hot image once to `ichiran_kernel_open`; Rust copies it. For details, read the first 96
bytes, call `ichiran_detail_prefix_length`, read exactly that prefix, then call
`ichiran_detail_store_open`. Rust copies only the prefix and one decoded block; it
never owns the file or eagerly loads `details.bin`.

- Clean analysis calls `ichiran_kernel_analyze_utf16` with one UTF-16 buffer and one
  `{limit, entities, normalizePunctuation}` JSON document.
- Romanization calls `ichiran_kernel_romanize_utf16`; its JSON string preserves lone
  surrogates as escapes.
- Describe calls `ichiran_detail_store_range`, reads that exact file range, and passes
  it to `ichiran_detail_store_decode`. Rust returns UTF-8 `DetailEntry` JSON in the
  established TypeScript field order.
- Retained legacy calls `ichiran_kernel_legacy_begin_utf16` for an independent opaque
  operation, then calls `ichiran_kernel_legacy_step`. On `MISSING_DETAIL`, read the
  returned range and supply it on the next step. `READY` returns the established
  detailed legacy JSON bytes and is terminal. Always release the operation.

Input lengths, entity offsets, and analyzer spans are UTF-16 code units. Construct the
input from `String.UTF16View`; do not pass UTF-8 offsets. Keep explicit `[UInt16]` tests
for astral pairs and unpaired high and low surrogates.

Every `IchiranResult.buffer` and `IchiranStepResult.buffer`, including empty success
and error buffers, is Rust-owned. Return the unchanged value exactly once with
`ichiran_buffer_free`. Release every non-null kernel, detail store, and legacy
operation exactly once with its matching free function. Rust borrows input, options,
method, and compressed-range buffers only until the call returns.

One kernel and detail store may be shared across native threads. Calls serialize at
their owner; each legacy operation has independent analysis, retry, and detail-session
state. The only multi-owner path locks operation, kernel, then detail store, and
atomically decodes a supplied block before retrying serialization. Do not free a
handle while any call using it is in flight. Run analyzer work off the main actor.

All fallible entries catch Rust panics and return an owned `ICHIRAN_INTERNAL` error;
no unwind crosses C. Swift should map nonzero status codes to one error type and decode
only successful JSON. Pack download, hashing, atomic installation, release identity,
and file lifetime remain host responsibilities.

## Required Mac validation

Before writing the Swift adapter, run the same C caller on macOS:

```sh
bash tests/run_c_harness.sh /path/to/portable-core-260118-baseline
(cd ../.. && bun run qualify:native-same-pack -- /path/to/attached-source-release)
```

The required output covers 1,236 clean analyses plus three raw astral/lone-surrogate
UTF-16 witnesses, 702 retained detailed results (401 current-Lisp authority and 301
provenance-bound fallback) plus the same three UTF-16 cases, five retained
romanizations plus three UTF-16 cases using explicit non-default methods, four lazy
describes, corrupt-block recovery, owned errors, 128 concurrent clean calls, and 32
concurrent detailed operations. Re-run the same corpus through the final
C/Swift wrapper, then validate background execution, restart, leaks, and memory on
simulator and physical devices.

WSL qualification does not claim XCFramework, Swift, simulator, Safari, or physical
device validation. Those remain M5B Mac-owned gates.

Before signing off the XCFramework/Swift package and its test evidence, repeat:

```sh
test "$(git rev-parse HEAD)" = "$candidate"
test -z "$(git status --porcelain=v1 --untracked-files=all)"
```
