# ichiran-node

Self-contained Japanese segmentation, dictionary analysis, and romanization for
JavaScript. The production analyzer runs from immutable binary data in a browser
Worker or Node.js. It does not connect to PostgreSQL, call a server, or fetch text
for analysis.

The default analyzer is one host-neutral Rust kernel compiled to WASM. The same emitted
module powers the browser Worker, Node adapter, CLI, and HTTP API. TypeScript owns host
installation and I/O; `TypeScriptOracleRuntime` remains available only through an
explicit qualification entry point for frozen differential checks.

Native Apple packaging is the next host integration of the same Rust source. Physical
Safari/iPhone qualification and the Mac-owned XCFramework, Swift, simulator, and
device gates remain pending. Release data is owned by the PostgreSQL-free TypeScript
source compiler; the frozen PostgreSQL and TypeScript analyzers remain temporary
qualification oracles only. See
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
Rust WASM kernel + @ichiran/core host facade <--- browser Worker
        ^
        |
@ichiran/node  <--- CLI / HTTP API

pinned semantic sources ---> @ichiran/data source compiler ---> immutable release
                                  ^
                                  |
                    PostgreSQL migration oracle only
```

- The Rust crate is the canonical analyzer. It owns packed readers, lookup, morphology,
  scoring, top-N paths, dictionary details, romanization, and retained serialization.
- `@ichiran/core` is the browser-safe WASM host facade and shared public model. Its
  `TypeScriptOracleRuntime` is exposed only from `@ichiran/core/qualification`.
- `@ichiran/node` verifies and decompresses release files, then opens the same core
  WASM runtime used in the browser.
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

The default test command runs rustfmt, Clippy, ordinary Cargo tests, and a fresh
temporary Rust/WASM build before the TypeScript suites. It fails if the checked-in
WASM, JavaScript glue, or declarations differ. The build requires
`wasm-bindgen-cli 0.2.127`; install that exact tool when needed with
`cargo install wasm-bindgen-cli --version 0.2.127 --locked`.
The root parity and browser-qualification commands run the same check first.

Compiler and PostgreSQL-reference checks are explicit and separate:

```bash
bun run typecheck:compiler
bun run build:compiler
```

See [PACKAGES.md](./PACKAGES.md) for package ownership and
[docs/browser-alpha/README.md](./docs/browser-alpha/README.md) for browser gates.

## License

[FSL-1.1-MIT](./LICENSE)
