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
bash tests/run_c_harness.sh /path/to/portable-core-260118-baseline
```

The generator locks the six-suite, 1,236-operation corpus and obtains expected bytes
from the frozen TypeScript analyzer in this checkout. The C caller sends every input
and options document through ABI v2 and compares the complete serialized result
byte-for-byte. It also checks a Rust-owned error buffer and shares one kernel across
four pthreads for 128 more exact calls. Native calls serialize at the opaque handle
because the analyzer's lazy caches have one owner.

Browser artifact:

```sh
bun install
bun run build:rust-wasm
```

The build uses the crate's `wasm-release` profile, wasm-bindgen, and the pinned
Binaryen 132 optimizer. It writes the one shared artifact consumed by browser and
Node into `packages/core/src/rust-kernel/generated`.

See `MAC-HANDOFF.md` for buffer ownership and the native handoff contract.
