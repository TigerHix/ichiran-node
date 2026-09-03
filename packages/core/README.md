# @ichiran/core

Browser-safe host facade and public model for the canonical Rust analyzer kernel. It
loads the checked-in WASM artifact and never performs filesystem, network, Node.js, or
PostgreSQL I/O. Browser and Node execute the same emitted module.

`TypeScriptOracleRuntime`, the wrapper used to execute the frozen TypeScript oracle,
is not part of the normal package entry point. Release qualification may import it
explicitly from `@ichiran/core/qualification` for same-pack differential checks.
Physical Safari/iPhone qualification and Mac-owned native Apple packaging remain
pending; see
[the forward roadmap](../../docs/SOURCE-COMPILER-RUST-KERNEL-ROADMAP.md).

The Rust kernel owns:

- strict readers for the immutable hot pack and random-access detail store;
- route-aware surface lookup and direct dictionary roots;
- reverse morphology, suffixes, counters, numbers, splits, and entity hints;
- scoring, analyzer-internal filters and synergies, and stable top-N paths;
- dictionary details, romanization, and legacy-compatible serialization.

Core owns the shared public model and `IchiranRuntime`, the asynchronous WASM facade
used by every host.

Hosts provide installed hot bytes and a random-access detail source. In a browser the
generated WASM URL is fetched by default:

```ts
const runtime = await IchiranRuntime.open({ hot, details });

const clean = await runtime.analyze('食べました');
const romanized = await runtime.romanize('食べました');
const legacy = await runtime.legacy('食べました', { limit: 3 });
const entryIndex = runtime.entryIndexForSequence(1358280);
```

Node callers must also pass the emitted WASM bytes as `wasm`; the `@ichiran/node`
adapter reads those bytes and the verified release assets from disk automatically.

The browser Worker provides OPFS-backed sources. `@ichiran/node` provides filesystem
loading and manifest verification. Neither adapter implements analyzer behavior.

The Rust cutover intentionally removed the former experimental reader fields
`surface`, `roots`, `morphology`, `support`, and `annotations` from `IchiranRuntime`.
They exposed TypeScript implementation objects that cannot be a stable cross-host
contract. Use `analyze`, `romanize`, `legacy`, `describe`, and the narrow
`entryIndexForSequence` compatibility operation instead. This is an explicit breaking
cutover, not a partial reader shim.

The immutable-baseline differential remains hash-pinned by default. The explicit
same-pack mode verifies an arbitrary format-v1 manifest and compares both kernels on
that release without changing the baseline gate:

```sh
bun tools/rust-kernel-wasm-differential.ts /path/to/portable-core-260118-baseline
bun tools/rust-kernel-wasm-differential.ts --same-pack /path/to/installed-format-v1-release
```

## Data layout

The installed release has a resident hot pack and a lazy detail store. The hot pack
contains five deterministic sections:

1. route-aware surface index;
2. root payload;
3. reverse morphology;
4. resident analyzer support;
5. block-compressed annotations and generated physical-member facts.

Complete forms, senses, glosses, and sense properties live in the separate detail
store and are opened on demand. All persisted and downloaded identities are verified
by the host before use; individual packed sections and blocks have their own structural
and checksum validation.

The binary formats are internal release contracts, not a database abstraction. Rust
works directly on compact packed data and canonical root identities. Generated
PostgreSQL sequence IDs are not part of the clean public model.

## Scope

The analyzer-internal rules needed for segmentation parity are included. The separate
experimental `@ichiran/grammar` package and a general Kanjidic API are not. Kanjidic
readings used for analyzer hints are resolved by the compiler and stored as analyzer
facts.

See [../../docs/EDGE-NATIVE-MILESTONE.md](../../docs/EDGE-NATIVE-MILESTONE.md) for the
as-built alpha boundary and [../../docs/browser-alpha/README.md](../../docs/browser-alpha/README.md)
for artifact and performance gates.
