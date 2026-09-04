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
  bun ../core/tools/rust-kernel-wasm-differential.ts --same-pack "$ICHIRAN_M1_PACK_DIR"
```

Linux C boundary check:

```sh
bash tests/run_c_harness.sh /path/to/portable-core-260118-baseline
```

ABI v7 runs two native callers. The clean caller locks all 1,236 six-suite analyses,
three explicit astral/lone-surrogate witnesses, one owned error, and 128 concurrent
calls. The multilingual product caller opens the language-neutral lexicon and the
English and Simplified Chinese locale stores, then exercises lazy decoding, localized
legacy output, canonical token details, English fallback, and corrupt-block recovery.

The host keeps `lexicon.bin`, the manifest-declared `gloss.<locale>.bin` files, and
installation ownership. Rust holds verified store prefixes, small block caches, and
independent opaque operation sessions. See `include/ichiran_kernel.h` for ABI v7, UTF-16, threading,
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
