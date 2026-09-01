# Ichiran Rust kernel

One host-neutral crate reads pack format v1 and exposes coarse native, browser-WASM,
and versioned C operations. The TypeScript analyzer remains the frozen differential
oracle during the transition.

Qualified real-pack checks:

```sh
ICHIRAN_M1_PACK_DIR=/path/to/portable-core-260118-baseline \
  cargo test --release --test qualified_pack -- --ignored --test-threads=1
```

Live differential against the frozen TypeScript kernel in this checkout:

```sh
ICHIRAN_M1_PACK_DIR=/path/to/portable-core-260118-baseline \
  bun tests/m1-differential.ts
```

Linux C boundary check:

```sh
cargo build --release
cc -std=c11 -Wall -Wextra -Werror -Iinclude tests/c_harness.c \
  target/release/libichiran_kernel.a -ldl -lpthread -lm \
  -o target/c_harness
target/c_harness /path/to/portable-core-260118-baseline/hot.bin
```

The harness also shares one kernel across four pthreads. Native analysis calls are
safe from concurrent threads and serialize at the opaque kernel handle because the
analyzer's lazy caches have one owner.

Browser artifact:

```sh
bun install
bun run build:rust-wasm
```

The build uses the crate's `wasm-release` profile, wasm-bindgen, and the pinned
Binaryen 132 optimizer. It writes the one shared artifact consumed by browser and
Node into `packages/core/src/rust-kernel/generated`.

See `MAC-HANDOFF.md` for buffer ownership and the native handoff contract.
