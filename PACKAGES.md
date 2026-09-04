# Packages

The default product has one host-neutral Rust analyzer kernel and thin TypeScript host
adapters. The frozen TypeScript and PostgreSQL analyzers are qualification-only and
remain outside the runtime and normal source-compiler dependency graphs. See
[docs/SOURCE-COMPILER-RUST-KERNEL-ROADMAP.md](./docs/SOURCE-COMPILER-RUST-KERNEL-ROADMAP.md).

## Product packages

| Package | Ownership |
|---|---|
| `ichiran-kernel` | Canonical packed readers and analyzer semantics for WASM and the native C ABI |
| `@ichiran/core` | Browser-safe `Analyzer` facade, public result model, and explicit release/compiler/qualification subpaths |
| `@ichiran/presentation` | Small validated `en`/`zh-Hans` catalogs for analyzer terminology and UI copy, independent of dictionary gloss locale |
| `@ichiran/node` | The single `openAnalyzer` filesystem loader with manifest verification and gzip decoding |
| `@ichiran/cli` | Explicit `ichiran analyze`, `romanize`, and `entry` commands |
| `@ichiran/api` | Analyzer-only Node HTTP server over `@ichiran/node` |
| `@ichiran/browser-demo` | OPFS analyzer-pack installer, Worker transport, and mobile-first integration demo; consumer applications own their shell and Service Worker policy |

Runtime dependencies are intentionally direct:

```text
browser-demo ----------> presentation catalogs
       |---------------> core facade ----------> Rust WASM
node ------------------> core facade ----------> Rust WASM
cli -------------------> node
api -------------------> node
```

`@ichiran/core` has no Node or PostgreSQL runtime dependency. Host packages own I/O;
analyzer behavior stays in Rust. The frozen TypeScript runtime is not exported from
the normal entry point and is available only as `@ichiran/core/qualification`.

## Compiler and reference packages

| Package | Ownership |
|---|---|
| `@ichiran/data` | Node-only deterministic source compiler, release-format-v2 lexicon/locale encoders, and frozen migration-authoring utilities |
| `@ichiran/reference-postgres` | Private frozen PostgreSQL analyzer used only by transition qualification and migration maintenance |
| `@ichiran/testing` | PostgreSQL-reference test setup |

```text
pinned sources ----------> source compiler ----------> immutable pack

qualification tools -----> reference-postgres -------> PostgreSQL
testing -----------------> reference-postgres -------> PostgreSQL
```

These packages never enter a shipped runtime bundle. The normal release entry point
imports source-compiler modules directly and neither imports nor connects to
PostgreSQL. The legacy database loader and frozen reference remain for the transition
release only; they are not alternate production compilers or analyzers.

## Experimental package

`@ichiran/grammar` is a separate experiment. It is not a dependency of core, Node,
CLI, API, or the browser demo and is outside the edge-native milestone. Do not
confuse it with analyzer-internal suffix handling, filters, penalties, or synergies.

## Data release

The compiler emits six files for the current English and Simplified Chinese release:

```text
manifest.json
hot.bin.gz
lexicon.bin.gz
gloss.en.bin.gz
gloss.zh-Hans.bin.gz
stats.json
```

The compressed data files are release artifacts, not source-controlled package
contents. Browser installation persists their verified decoded forms in OPFS. Node
loads and verifies the same release through `ICHIRAN_PACK_DIR`.

## Commands

```bash
# Product
bun run build
bun run typecheck
bun test

# Direct source release, isolation proof, and cross-kernel qualification
bun run source:release -- baseline --out /absolute/path/to/release --pack-version <version>
bun run source:release:isolated -- baseline --out /absolute/path/to/isolated-release --pack-version <version>
bun run qualify:rust-same-pack -- /absolute/path/to/release
bun run qualify:native-same-pack -- /absolute/path/to/release
bun run qualify:source-hosts -- /absolute/path/to/release

# Frozen compiler/reference and format-1 evidence maintenance
bun run build:compiler
bun run typecheck:compiler
bun run source:attestation -- --report data/source-compiler-parity-report.json --release /absolute/path/to/format-1-release

# Browser demo
bun run alpha:demo:stage
bun run alpha:demo:build
bun run alpha:demo:test
bun run alpha:demo:e2e
```

Normal product commands and `source:release` do not read `ICHIRAN_DB_URL`. Only
explicit migration-oracle and reference tests do.
