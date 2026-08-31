# Packages

The qualified alpha has one TypeScript analyzer implementation and thin host adapters.
Compiler and reference code are kept outside the runtime dependency graph.

The accepted post-alpha direction replaces the analyzer kernel with one Rust crate for
browser WASM, Node, and native iOS. TypeScript continues to own the browser host and
source-data compiler. See
[docs/SOURCE-COMPILER-RUST-KERNEL-ROADMAP.md](./docs/SOURCE-COMPILER-RUST-KERNEL-ROADMAP.md).

## Product packages

| Package | Ownership |
|---|---|
| `@ichiran/core` | Browser-safe packed readers, analyzer, morphology, scoring, details, romanization, and legacy serialization |
| `@ichiran/node` | Filesystem loading, manifest verification, gzip decoding, and legacy info formatting for Node |
| `@ichiran/cli` | Historical `ichiran-cli` command surface over `@ichiran/node` |
| `@ichiran/api` | Analyzer-only Node HTTP server over `@ichiran/node` |
| `@ichiran/browser-demo` | Offline PWA, OPFS installer, Worker transport, and mobile-first demo UI |

Runtime dependencies are intentionally direct:

```text
browser-demo ----------> core
node ------------------> core
cli -------------------> node
api -------------------> node
```

`@ichiran/core` has no Node, PostgreSQL, or third-party runtime dependency. Host
packages own I/O; analyzer behavior stays in core.

## Compiler and reference packages

| Package | Ownership |
|---|---|
| `@ichiran/data` | Node-only deterministic release compiler and source-data maintenance |
| `@ichiran/reference-postgres` | Private frozen PostgreSQL analyzer used by the compiler and transition oracle |
| `@ichiran/testing` | PostgreSQL-reference test setup |

```text
data ----------> reference-postgres ----------> PostgreSQL
testing -------> reference-postgres ----------> PostgreSQL
```

These packages never enter a shipped runtime bundle. The reference package remains
for one migration cycle because the compiler still reuses its data-authoring logic.
It can be deleted after that logic has been moved into the compiler and the upstream
Lisp plus packed-runtime parity gates independently cover the product.

## Experimental package

`@ichiran/grammar` is a separate experiment. It is not a dependency of core, Node,
CLI, API, or the browser demo and is outside the edge-native milestone. Do not
confuse it with analyzer-internal suffix handling, filters, penalties, or synergies.

## Data release

The compiler emits four files:

```text
manifest.json
hot.bin.gz
details.bin.gz
stats.json
```

The two compressed data files are release artifacts, not source-controlled package
contents. Browser installation persists their verified decoded forms in OPFS. Node
loads and verifies the same release through `ICHIRAN_PACK_DIR`.

## Commands

```bash
# Product
bun run build
bun run typecheck
bun test

# Compiler/reference
bun run build:compiler
bun run typecheck:compiler

# Release
bun run alpha:release:build -- --database "$ICHIRAN_DB_URL" --out dist/browser-alpha --pack-version <version> --shell-dir packages/browser-demo/dist
bun run alpha:release:verify -- --out dist/browser-alpha --shell-dir packages/browser-demo/dist

# Browser demo
bun run alpha:demo:stage
bun run alpha:demo:build
bun run alpha:demo:test
bun run alpha:demo:e2e
```

Normal product commands do not read `ICHIRAN_DB_URL`. Release compilation and
reference-only tests do.
