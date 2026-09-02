# ichiran-node

Self-contained Japanese segmentation, dictionary analysis, and romanization for
JavaScript. The production analyzer runs from immutable binary data in a browser
Worker or Node.js. It does not connect to PostgreSQL, call a server, or fetch text
for analysis.

The current milestone is an analyzer-only offline demo. A user downloads one pinned
data release, installs it in browser storage, and can then analyze entirely on the
device. The same runtime powers the browser demo, Node adapter, CLI, and HTTP API.

The TypeScript runtime remains the qualified migration baseline for the pending Rust
kernel. Release data is now owned by a PostgreSQL-free TypeScript source compiler.
The accepted post-alpha direction is one Rust analyzer crate compiled to browser
WASM, Node, and native iOS while retaining that source-native compiler.
See
[the source-compiler and Rust-kernel roadmap](./docs/SOURCE-COMPILER-RUST-KERNEL-ROADMAP.md).

## Quick start

```bash
bun install
bun run build
```

The CLI and API need an installed analyzer release containing `manifest.json`,
`hot.bin.gz`, and `details.bin.gz`:

```bash
export ICHIRAN_PACK_DIR=/absolute/path/to/analyzer-release

bun run cli "今日はいい天気です"
bun run cli -- -i "食べました"
bun run cli -- -f -l 3 "みんな土足でおいで"

bun run dev
```

See [CLI.md](./CLI.md) and [API.md](./API.md) for the compatibility surfaces.

To run the offline browser demo with a built release:

```bash
bun run alpha:demo:stage
bun run --cwd packages/browser-demo dev
```

Building a new pack is a maintainer workflow documented in
[docs/browser-alpha/RELEASE.md](./docs/browser-alpha/RELEASE.md).

## Current qualified architecture

```text
immutable analyzer release
        |
        v
@ichiran/core  <--- browser Worker
        ^
        |
@ichiran/node  <--- CLI / HTTP API

pinned semantic sources ---> @ichiran/data source compiler ---> immutable release
                                  ^
                                  |
                    PostgreSQL migration oracle only
```

- `@ichiran/core` is the current canonical analyzer and executable oracle for the Rust
  port. It owns packed readers, lookup, morphology, scoring, top-N paths, dictionary
  details, romanization, and the legacy serializer. It is browser-safe and has no
  runtime dependencies.
- `@ichiran/node` verifies and decompresses release files, then opens the same core
  runtime used in the browser.
- `@ichiran/reference-postgres` is the frozen former implementation. It is private
  and retained temporarily as a compiler/oracle dependency, not a product runtime.
- `@ichiran/data` owns the TypeScript source compiler and pack-v1 encoders. Its
  verified source lock selects JMdict, Kanjidic2, custom XML/CSV, conjugation CSVs,
  chronological errata, and the narrow compatibility ledger.

The data files are generated release artifacts rather than Git contents. PostgreSQL
is not a release input. The frozen database and `@ichiran/reference-postgres` remain
read-only migration oracles for qualification and can be physically unavailable
during a complete source release build.

## Scope and source pins

The parity target is upstream Ichiran
`ea9583368e67cad22d94abae8dbcc8df96d99bcd` with data release
`ichiran-260118`. The pinned January 1 JMdict gzip has SHA-256
`92eb77d60e5b949585e41a777ff3857c412bc97ea75444d14497a5156b6264b7`;
the matching qualified Kanjidic2 gzip has SHA-256
`1861f294b187d491dd127a972d59dfe92117df536466562a0f2a44abf98a7d03`.
The 200,012,956-byte PostgreSQL dump, SHA-256
`98a44e2cc88a65677da8b1f7124e7d6c904253eb1aae0ef16d2c7cc1dacdba82`,
is retained only as an oracle. The complete release corpus contains 1,241
chosen-authority comparisons (940 current-Lisp snapshots plus 301 frozen
PostgreSQL fallbacks), and independently checks clean semantics for all 301
fallback operations.

The separate experimental `@ichiran/grammar` package is outside this product
milestone. Analyzer-internal suffix rules, segmentation filters, penalties, and
synergies remain in scope because they affect analyzer output.

Kanjidic is not a runtime product feature. The compiler uses its readings only to
resolve analyzer easy-hint data; those resolved facts are packed into the release.
The browser and Node runtimes contain no general Kanjidic lookup API.

The as-built product boundary and parity contract are in
[docs/EDGE-NATIVE-MILESTONE.md](./docs/EDGE-NATIVE-MILESTONE.md). The authoritative
post-alpha architecture and retirement gates are in
[docs/SOURCE-COMPILER-RUST-KERNEL-ROADMAP.md](./docs/SOURCE-COMPILER-RUST-KERNEL-ROADMAP.md).

## Development

```bash
bun run typecheck
bun run test
```

Compiler and PostgreSQL-reference checks are explicit and separate:

```bash
bun run typecheck:compiler
bun run build:compiler
```

See [PACKAGES.md](./PACKAGES.md) for package ownership and
[docs/browser-alpha/README.md](./docs/browser-alpha/README.md) for browser gates.

## License

[FSL-1.1-MIT](./LICENSE)
