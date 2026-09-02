# Ichiran Rust kernel

One host-neutral crate reads pack format v1 and exposes coarse native, browser-WASM,
and versioned C operations. Rust is the product analyzer; the frozen TypeScript
implementation is retained only in qualification tools.

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

ABI v3 runs two native callers. The clean caller locks all 1,236 six-suite analyses,
three explicit astral/lone-surrogate witnesses, one owned error, and 128 concurrent
calls. The product caller additionally locks 702
detailed legacy results, five romanizations, four lazy describes, two corrupt-block
recovery paths, three more owned errors, and 32 concurrent detailed operations. The
702 expected byte streams come from the frozen portable TypeScript oracle only after
each is independently canonical-exact against its 401 current-Lisp or 301
provenance-bound fallback authority.

The host keeps `details.bin` and installation ownership. Rust holds only the verified
detail prefix, a one-block cache, and independent opaque legacy-operation sessions.
See `include/ichiran_kernel.h` and `MAC-HANDOFF.md` for ABI v3, UTF-16, threading,
panic containment, and allocation ownership.

Browser artifact:

```sh
bun install
bun run build:rust-wasm
```

The build uses the crate's `wasm-release` profile, wasm-bindgen, and the pinned
Binaryen 132 optimizer. It writes the one shared artifact consumed by browser and
Node into `packages/core/src/rust-kernel/generated`.

The checked-in WASM/glue/declarations are generated artifacts; the repository verifier
rebuilds them in a temporary directory and requires byte-for-byte identity.
