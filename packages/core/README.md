# @ichiran/core

Current canonical self-contained Ichiran analyzer for JavaScript. It is browser-safe,
has no runtime dependencies, and never performs filesystem, network, Node.js, or
PostgreSQL I/O.

This implementation is the qualified executable oracle for the planned Rust kernel.
It remains supported during parity migration, then retires after browser, Node, and
iOS use the same Rust source. The pack and clean analyzer model remain the contract; see
[the forward roadmap](../../docs/SOURCE-COMPILER-RUST-KERNEL-ROADMAP.md).

Core owns:

- strict readers for the immutable hot pack and random-access detail store;
- route-aware surface lookup and direct dictionary roots;
- reverse morphology, suffixes, counters, numbers, splits, and entity hints;
- scoring, analyzer-internal filters and synergies, and stable top-N paths;
- dictionary details, romanization, and legacy-compatible serialization;
- `IchiranRuntime`, the shared asynchronous facade used by every host.

Hosts provide installed bytes and a gzip block decoder:

```ts
const runtime = await IchiranRuntime.open({ hot, details, decodeGzip });

const clean = await runtime.analyze('食べました');
const romanized = await runtime.romanize('食べました');
const legacy = await runtime.legacy('食べました', { limit: 3 });
```

The browser Worker provides OPFS-backed sources. `@ichiran/node` provides filesystem
loading and manifest verification. Neither adapter implements analyzer behavior.

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

The binary formats are internal release contracts, not a database abstraction. Core
works directly on compact typed-array views and canonical root identities. Generated
PostgreSQL sequence IDs are not part of the clean public model.

## Scope

The analyzer-internal rules needed for segmentation parity are included. The separate
experimental `@ichiran/grammar` package and a general Kanjidic API are not. Kanjidic
readings used for analyzer hints are resolved by the compiler and stored as analyzer
facts.

See [../../docs/EDGE-NATIVE-MILESTONE.md](../../docs/EDGE-NATIVE-MILESTONE.md) for the
as-built alpha boundary and [../../docs/browser-alpha/README.md](../../docs/browser-alpha/README.md)
for artifact and performance gates.
