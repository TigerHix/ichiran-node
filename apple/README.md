# Ichiran native Apple integration

This directory packages the existing Rust analyzer for iOS and exposes it through a
small Swift API. Rust remains the only implementation of segmentation, morphology,
scoring, romanization, dictionary presentation, and fallback behavior.

## Prerequisites

The qualified toolchain is macOS 26.7, Xcode 26.6 (iOS 26.5 SDK), Swift 6.3.3,
Rust/rustup with the repository's Rust 1.92.0 toolchain, Bun 1.3.5, and XcodeGen
2.45.4. Install the Rust targets once:

```sh
rustup target add --toolchain 1.92.0 \
  aarch64-apple-ios aarch64-apple-ios-sim x86_64-apple-ios \
  aarch64-apple-darwin x86_64-apple-darwin
```

The scripts fail with a specific message when the host architecture, Apple tools,
Rust toolchain, target set, ABI, symbol set, or archive architecture is wrong.

## Build and test

Build and audit the complete XCFramework with one command:

```sh
apple/scripts/build-xcframework.sh
```

This creates the device and universal-simulator slices at
`apple/IchiranSwift/Artifacts/IchiranKernel.xcframework`, a distributable zip beside
it, and `work/apple/xcframework/audit.txt`. These generated files are ignored. The
audit records the 14 exported `ichiran_*` symbols, runtime ABI version 3,
architectures, sizes, and SHA-256 values. The script combines only arm64 and x86_64
simulator archives; it never combines device and simulator code.

Run the Swift package, app, exact repository parity corpora, pack failure tests, and
lifetime tests on a simulator with one command:

```sh
apple/scripts/run-tests.sh /absolute/path/to/release
```

For a source-compiler pack, bind corpus generation to its source lock:

```sh
apple/scripts/run-tests.sh work/swift-source-release \
  --same-pack --source-lock data/source-compiler-update-2026-09-02.lock.json
```

The same-pack source lock must be a tracked path inside this repository because the
qualification corpus binds it to the current Git checkout.

Override the default simulator with `ICHIRAN_IOS_DESTINATION`, for example
`platform=iOS Simulator,name=iPhone 17 Pro,OS=26.5`. Xcode's Address Sanitizer and
Thread Sanitizer can be enabled on the same generated project with
`-enableAddressSanitizer YES` and `-enableThreadSanitizer YES`, respectively.

## Package integration

After generating the XCFramework, add `apple/IchiranSwift` as a local Swift package
or publish `IchiranKernel.xcframework.zip` and point a binary target at its audited
checksum. Link the `IchiranSwift` product. Generated archives, XCFrameworks, test
fixtures, and packs must remain outside Git.

The product API is deliberately limited to `IchiranAnalyzer.analyze`, `romanize`,
`entry`, and `dispose`, plus `IchiranPackStore` installation/opening. The retained C
session API is visible only to `@testable` qualification code and is not public Swift
API.

## Installing and opening a pack

Install either a bundled/local release directory or a remote `manifest.json`:

```swift
let store = IchiranPackStore(baseDirectory: applicationSupportURL)
try await store.install(from: .directory(bundleReleaseURL))
// Or: try await store.install(from: .remote(manifestURL))

let analyzer = try await store.openAnalyzer()
let result = try await analyzer.analyze("庭には二羽鶏がいる")
let latin = try await analyzer.romanize("日本語", options: .init(method: .kunreiSiki))
let entry = try await analyzer.entry(entryIndex)
await analyzer.dispose()
```

`manifest.json` is authenticated before installation. Compressed and installed byte
counts and SHA-256 identities are checked while files stream to a staging generation.
The active marker is replaced atomically only after the candidate opens successfully,
so a failed replacement leaves the previous verified generation active. A later
process can call `openAnalyzer()` without networking.

`details.bin` stays file-backed. Opening reads its 96-byte header and then exactly the
resident prefix requested by `ichiran_detail_prefix_length`; lookup reads only the
compressed range returned by `ichiran_detail_store_range`.

## Ownership and threading

`IchiranAnalyzer` is an actor and the sole owner of the Rust kernel handle, detail-store
handle, and details file. Its isolation serializes calls and makes `dispose()` wait for
an in-flight call. Handles are destroyed exactly once, and every Rust-owned result
buffer is returned exactly once, including empty and error buffers. Opening runs in a
detached task, and analyzer calls execute on the analyzer actor rather than the main
actor. String input and result spans use UTF-16 code units.

## Release artifacts

Run `apple/scripts/build-xcframework.sh`, archive the generated zip and
`work/apple/xcframework/audit.txt` in the release workspace, and verify the recorded
hash before distribution. Do not commit either generated binary or pack data. A
physical iPhone run of the validation app remains required for a device-qualified
release whenever a connected device is available.
