# Mac native handoff

This crate is the sole analyzer source for native and browser builds. The C surface is
versioned by `ICHIRAN_KERNEL_ABI_VERSION` in `include/ichiran_kernel.h`; Swift should
import that header through a module map and treat `IchiranKernel` as an opaque owner.

Build a universal static library on a Mac with the same locked Rust toolchain:

```sh
rustup target add aarch64-apple-darwin x86_64-apple-darwin
cargo build --release --target aarch64-apple-darwin
cargo build --release --target x86_64-apple-darwin
lipo -create \
  target/aarch64-apple-darwin/release/libichiran_kernel.a \
  target/x86_64-apple-darwin/release/libichiran_kernel.a \
  -output libichiran_kernel.a
```

The later iOS handoff uses the same crate and header, not a separate kernel. Install
the device and simulator targets and build each architecture on the Mac agent:

```sh
rustup target add aarch64-apple-ios aarch64-apple-ios-sim x86_64-apple-ios
cargo build --release --target aarch64-apple-ios
cargo build --release --target aarch64-apple-ios-sim
cargo build --release --target x86_64-apple-ios
```

Use `aarch64-apple-ios` for devices. Combine the two simulator libraries into an
XCFramework slice as required by the consuming Xcode project; do not `lipo` a device
library and a simulator library into one archive.

The host owns pack installation, file access, and concurrency. It passes the complete
verified `hot.bin` once to `ichiran_kernel_open`, then submits one UTF-16 buffer per
analysis. Never convert the input through Swift `String.UTF8View`: unpaired surrogate
fixtures and all public spans are defined in UTF-16 code units.

One opaque kernel may be shared across native threads. The Rust handle serializes
analysis calls so its lazy caches retain a single owner. Do not call
`ichiran_kernel_free` until every in-flight analysis has returned; the host still owns
that lifetime boundary.

Every `IchiranResult.buffer`, including an error buffer and a zero-length success
buffer, is Rust-owned and must be passed exactly once to `ichiran_buffer_free`.
Release the opaque kernel exactly once with `ichiran_kernel_free`. Calls do not borrow
the caller's hot or input buffers after returning. Rust catches panics at each C entry;
no unwind is permitted to cross the ABI.

Before building a Swift adapter, run the Linux harness documented in the crate README,
then reproduce the same two M1 JSON witnesses and the full differential corpus on the
Mac agent. Do not introduce a second native semantic implementation or change pack v1.
