# Mac native handoff

This crate is the sole analyzer source for native and browser builds. The C surface is
versioned by `ICHIRAN_KERNEL_ABI_VERSION` in `include/ichiran_kernel.h`; Swift should
import that header through a module map and treat `IchiranKernel` as an opaque owner.
ABI v2 accepts one UTF-16 input plus a borrowed UTF-8 options JSON document and
returns one Rust-owned UTF-8 JSON buffer. Do not mirror analyzer semantics in Swift.

Build a universal static library on a Mac with the same locked Rust toolchain:

```sh
rustup target add aarch64-apple-darwin x86_64-apple-darwin
cargo build --release --locked --target aarch64-apple-darwin
cargo build --release --locked --target x86_64-apple-darwin
lipo -create \
  target/aarch64-apple-darwin/release/libichiran_kernel.a \
  target/x86_64-apple-darwin/release/libichiran_kernel.a \
  -output libichiran_kernel.a
```

The Linux qualification triple is `x86_64-unknown-linux-gnu`. The commands above
produce `aarch64-apple-darwin` and `x86_64-apple-darwin`. Confirm that the universal
archive exports exactly the five documented C entry points before integrating it:

```sh
nm -gU libichiran_kernel.a | awk '/_ichiran_(kernel|buffer)_/ { print $3 }' | sort -u
```

The expected names are `_ichiran_kernel_abi_version`, `_ichiran_kernel_open`,
`_ichiran_kernel_analyze_utf16`, `_ichiran_kernel_free`, and
`_ichiran_buffer_free`.

The later iOS handoff uses the same crate and header, not a separate kernel. Install
the device and simulator targets and build each architecture on the Mac agent:

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
  -library target/aarch64-apple-ios/release/libichiran_kernel.a \
  -headers include \
  -library target/libichiran_kernel-simulator.a \
  -headers include \
  -output target/IchiranKernel.xcframework
```

Use `aarch64-apple-ios` for devices. The `lipo` command combines only the two
simulator architectures; `xcodebuild` keeps device and simulator libraries in
separate XCFramework slices. Never `lipo` a device library and a simulator library
into one archive.

The host owns pack installation, file access, and concurrency. It passes the complete
verified `hot.bin` once to `ichiran_kernel_open`, then submits one UTF-16 buffer per
analysis with the JSON fields `limit`, `entities`, and `normalizePunctuation`.
Entity offsets and all public spans are UTF-16 code units. Never convert the input
through Swift `String.UTF8View`: unpaired surrogate fixtures must survive unchanged.

One opaque kernel may be shared across native threads. The Rust handle serializes
analysis calls so its lazy caches retain a single owner. Do not call
`ichiran_kernel_free` until every in-flight analysis has returned; the host still owns
that lifetime boundary.

Every `IchiranResult.buffer`, including an error buffer and a zero-length success
buffer, is Rust-owned and must be passed exactly once to `ichiran_buffer_free`.
Release the opaque kernel exactly once with `ichiran_kernel_free`. Calls do not borrow
the caller's hot, input, or options buffers after returning. The fallible open and
analyze entries catch Rust panics and return `ICHIRAN_INTERNAL`; no unwind is permitted
to cross the ABI.

Before building a Swift adapter, run the Linux harness documented in the crate README,
then reproduce its exact 1,236-operation, six-suite corpus on the Mac agent with the
same qualified pack and source revision recorded by the fixture stream. Do not
introduce a second native semantic implementation or change pack v1. WSL qualification
does not claim that the XCFramework, Swift wrapper, simulator, or physical-device
integration has passed; those remain Mac-agent work.

On the Mac agent, the native C-boundary qualification command is identical; the
runner selects the Darwin link flags explicitly:

```sh
bash tests/run_c_harness.sh /path/to/portable-core-260118-baseline
```
