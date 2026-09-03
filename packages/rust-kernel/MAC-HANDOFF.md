# Mac native handoff

## Current candidate

Start from the current pushed `main`. Record that base before creating the Mac-owned
Swift branch; final qualification must be rerun from its own clean committed head:

```sh
git fetch origin main
base=$(git rev-parse origin/main)
git switch -c codex/swift-native-host "$base"
test "$(git merge-base HEAD "$base")" = "$base"
test -z "$(git status --porcelain=v1 --untracked-files=all)"
```

The integrated Linux/WSL report is
[`docs/INTEGRATED-EDGE-CUTOVER-REPORT.md`](../../docs/INTEGRATED-EDGE-CUTOVER-REPORT.md).

The current September source-built pack has installed hot SHA-256
`0641cad9cd6b3719e95f6d731c60350806e79f4c534a10d59852eb17b22c6e23`
and details SHA-256
`29abbd909261a04c3d76b3844339d276a5c6dbd02d4493629b88d15f39b57560`.
It is selected by `data/source-compiler-update-2026-09-02.lock.json` and uses pack
version `jmdict-2026-09-02-source`. Release files live under ignored `work/` output and
are not transferred by Git. Build the release from the final clean Swift-branch commit
so `manifest.sourceCommit` equals that exact head:

```sh
bun install --frozen-lockfile
bun scripts/acquire-source-compiler-jmdict.ts \
  data/source-compiler-update-2026-09-02.lock.json
bun run source:release -- update \
  --source-lock data/source-compiler-update-2026-09-02.lock.json \
  --out work/swift-source-release \
  --pack-version jmdict-2026-09-02-source
```

Verify the manifest identity and decoded hashes above before using it. Run the native
C same-pack corpus, then exercise that source pack through the Swift wrapper for
install/open, analysis, romanization, entry lookup, restart, corruption/recovery, and
concurrent background calls. Do not port analyzer or presentation logic into Swift.

M5B remains open: build the device and simulator archives, create the XCFramework,
write the thin Swift ownership/file adapter, and run simulator plus physical-device
memory, leak, lifecycle, and UTF-16 tests. Physical Safari qualification is a separate
remaining M4 gate.

This crate is the sole analyzer implementation for native and browser builds. ABI v3
exposes clean analysis, analyzer-backed romanization, and lazy detail lookup. Its
legacy session symbols remain for qualification only and must not become public Swift
API. Swift owns pack installation and file reads; it must not recreate analyzer,
detail, or presentation semantics.

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

Before writing the Swift adapter, run the immutable C caller on macOS. After the final
Swift implementation is committed, build the September source pack as described above
and run the source same-pack caller from that same clean head:

```sh
bash tests/run_c_harness.sh /path/to/portable-core-260118-baseline
(cd ../.. && bun run qualify:native-same-pack -- work/swift-source-release \
  --source-lock data/source-compiler-update-2026-09-02.lock.json)
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

Before signing off the XCFramework/Swift package and its test evidence, repeat with
the final committed head recorded in the generated manifest:

```sh
manifest_commit=$(bun -e "console.log(JSON.parse(await Bun.file(\
  'work/swift-source-release/manifest.json').text()).sourceCommit)")
test "$(git rev-parse HEAD)" = "$manifest_commit"
test -z "$(git status --porcelain=v1 --untracked-files=all)"
```
