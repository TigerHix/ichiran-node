# ichiran-node

Self-contained Japanese segmentation, dictionary analysis, and romanization for
JavaScript. The production analyzer runs from immutable binary data in a browser
Worker or Node.js. It does not connect to PostgreSQL, call a server, or fetch text
for analysis.

The current milestone is an analyzer-only offline demo. A user downloads one pinned
data release, installs it in browser storage, and can then analyze entirely on the
device. The same runtime powers the browser demo, Node adapter, CLI, and HTTP API.

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

## Architecture

```text
immutable analyzer release
        |
        v
@ichiran/core  <--- browser Worker
        ^
        |
@ichiran/node  <--- CLI / HTTP API

PostgreSQL + @ichiran/reference-postgres ---> @ichiran/data compiler only
```

- `@ichiran/core` is the canonical analyzer. It owns packed readers, lookup,
  morphology, scoring, top-N paths, dictionary details, romanization, and the
  legacy serializer. It is browser-safe and has no runtime dependencies.
- `@ichiran/node` verifies and decompresses release files, then opens the same core
  runtime used in the browser.
- `@ichiran/reference-postgres` is the frozen former implementation. It is private
  and retained temporarily as a compiler/oracle dependency, not a product runtime.
- `@ichiran/data` is the Node/PostgreSQL release compiler.

The data files are generated release artifacts rather than Git contents. PostgreSQL
is allowed only while compiling or qualifying a release; it is never part of the
browser, CLI, API, or normal core test path.

## Scope and source pins

The parity target is upstream Ichiran
`ea9583368e67cad22d94abae8dbcc8df96d99bcd` with data release
`ichiran-260118`. The pinned dump is 200,012,956 bytes with SHA-256
`98a44e2cc88a65677da8b1f7124e7d6c904253eb1aae0ef16d2c7cc1dacdba82`.
The captured upstream suite passes 782 / 782 analyzer assertions.

The separate experimental `@ichiran/grammar` package is outside this product
milestone. Analyzer-internal suffix rules, segmentation filters, penalties, and
synergies remain in scope because they affect analyzer output.

Kanjidic is not a runtime product feature. The compiler uses its readings only to
resolve analyzer easy-hint data; those resolved facts are packed into the release.
The browser and Node runtimes contain no general Kanjidic lookup API.

The complete product boundary, parity contract, and optimization roadmap are in
[docs/EDGE-NATIVE-MILESTONE.md](./docs/EDGE-NATIVE-MILESTONE.md).

## Development

```bash
bun run typecheck
bun test
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
