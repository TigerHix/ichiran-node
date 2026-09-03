# Product API migration

This is a one-way cutover from the historical Lisp-shaped compatibility surface to
the packed analyzer product contract. The Rust kernel and pack semantics are
unchanged; names, ownership, transport shapes, and supported presentation formats are
intentionally smaller.

## Core

Before:

```ts
import { IchiranRuntime } from '@ichiran/core';

const runtime = await IchiranRuntime.open(source);
await runtime.analyze(text, options);
await runtime.romanize(text, options);
await runtime.describe(entryIndex);
await runtime.legacy(text, options);
runtime.metrics();
```

After:

```ts
import { Analyzer } from '@ichiran/core';

const analyzer = await Analyzer.open(source);
await analyzer.analyze(text, options);
await analyzer.details(text, { ...options, pathIndex: 0, tokenIndex: 0 });
await analyzer.romanize(text, options);
await analyzer.entry(entryIndex);
analyzer.dispose();
```

| Removed or renamed | Replacement |
|---|---|
| `IchiranRuntime` | `Analyzer` |
| `IchiranRuntimeSource` | `AnalyzerSource` |
| `DetailRandomAccessSource` | `RandomAccessSource` |
| `PortableAnalyzeOptions` | `AnalyzeOptions` |
| `PortableAnalysis*` | corresponding `Analysis*` type |
| `AnalyzerEntityHint` | `EntityHint` |
| `DetailEntry`, `DetailForm`, `DetailSense`, etc. | `DictionaryEntry`, `DictionaryForm`, `DictionarySense`, etc. |
| `RomanizationName` | `RomanizationScheme` |
| `RUST_KERNEL_WASM_URL` | `ANALYZER_WASM_URL` |
| `describe(entryIndex)` | `entry(entryIndex)` |
| consumer-side sense/restriction/conjugation reconstruction | `details(text, { pathIndex, tokenIndex, ...options })` |
| `AnalyzerInputError`, `DetailStoreError`, Rust format errors | `AnalyzerError` |
| root manifest exports | import from `@ichiran/core/release` |
| `legacy()` | no product equivalent |
| `entryIndexForSequence()` | no equivalent; retain the `entryIndex` returned by `analyze()` |
| `metrics()` | no product equivalent; release tooling may use `@ichiran/core/qualification/runtime` |
| validation functions and public limit constants | no equivalent; call the analyzer and handle `AnalyzerError` |
| `PORTABLE_LEGACY_INFO` and legacy DTO types | no equivalent |
| `joinRomanizedParts` | no equivalent; `romanize()` returns the complete string |

`RomanizeOptions` deliberately has no `limit`: romanization selects the best path and
does not compute unused alternatives. It supports `method`, `entities`, and
`normalizePunctuation`.

Product error handling should switch on four stable codes only:

```ts
try {
  await analyzer.analyze(text, options);
} catch (error) {
  if (error instanceof AnalyzerError && error.code === 'invalid-input') {
    // Correct the request.
  }
}
```

The other codes are `invalid-pack`, `not-found`, and `internal`. Fine-grained kernel
corruption codes were implementation details and are no longer a caller contract.

## Node

```diff
- import { openNodeRuntime, romanizeWithInfo } from '@ichiran/node';
- const runtime = await openNodeRuntime(directory, { expectedSourceCommit });
+ import { openAnalyzer } from '@ichiran/node';
+ const analyzer = await openAnalyzer(directory);
```

`openAnalyzer` is the package's only export. With no argument it reads
`ICHIRAN_PACK_DIR`. The `expectedSourceCommit` option was removed; deployments that
need this gate set `ICHIRAN_SOURCE_COMMIT`.

`romanizeWithInfo` and the legacy word-info formatter have no equivalent. A product
that needs dictionary presentation should call `analyze`, retain the selected path
and token indexes, then call `details` with the same input and analysis options.
The kernel owns sense restrictions, counters, suffix descriptions, compound
components, conjugation lineage, entities, and alternative selection. UI code owns
only labels and layout. `entry()` remains available for raw dictionary tooling; it
must not be used to reconstruct analyzer presentation semantics.

## CLI

| Old invocation | New invocation |
|---|---|
| `ichiran-cli TEXT` | `ichiran romanize TEXT` |
| `ichiran-cli -f -l 3 TEXT` | `ichiran analyze --limit 3 TEXT` |
| `ichiran-cli -i TEXT` | `ichiran details TEXT` |
| `ichiran-cli --eval ...` | no equivalent |

`analyze` prints `AnalysisResult` JSON, not legacy nested JSON. `details` accepts text
plus `--path` and `--token` indexes and prints the canonical presentation tree.
`entry` accepts an `entryIndex` and prints the raw dictionary row. `romanize` prints a
string and supports `--method` and `--normalize-punctuation`. There is no implicit
default command. Unlike the old CLI, punctuation is not normalized unless requested.

The package is executable-only; the former programmatic cache and `runCli` helpers
were removed. Applications should depend on `@ichiran/node`.

## HTTP

| Removed route | Replacement |
|---|---|
| `POST /api/romanize` | `POST /v1/romanize` |
| `POST /api/segment` | `POST /v1/analyze` |
| `POST /api/analyze` | `POST /v1/analyze` |
| `POST /api/romanize/info` | no equivalent |
| `GET /health/db` | `GET /health` |
| `GET /api`, `POST /api/test` | no equivalent |

Analyze options moved under `options`:

```diff
- { "text": "日本語", "limit": 3, "entities": [] }
+ { "text": "日本語", "options": { "limit": 3, "entities": [] } }
```

`/v1/analyze` returns `AnalysisResult` directly. `/v1/romanize` returns only
`{ "romanized": "..." }`; it does not echo input. Dictionary details are available
through `POST /v1/details` with `{ text, options: { pathIndex, tokenIndex, ... } }`.
Raw dictionary rows are available at `GET /v1/entries/:entryIndex`. Errors changed from a string to
`{ "error": { "code": string, "message": string } }`.

Grammar placeholders were removed. Grammar remains a separate experimental package.

## Browser client and Worker

The product operations are `expect-release`, `status`, `install`, `clear`, `analyze`,
`details`, `romanize`, and `entry`. `details` returns the same canonical tree as Node
and native hosts. `describe` became `entry`; its result is a typed `DictionaryEntry`.

The `legacy`, public benchmark, and public kernel-metrics operations were removed.
Performance measurement remains available only in qualification builds and reuses the
same product `AnalyzerClient` and Worker, so the release gate still measures the
runtime that ships.

Browser lifecycle errors remain host-local (`not-installed`, `stale-install`, storage,
Worker, and release errors). Analyzer failures use the same four core product codes.

The browser adapter owns analyzer-pack download, verification, OPFS persistence, and
Worker execution only. It does not register a Service Worker, provide a PWA manifest,
cache the application shell, or manage shell updates. Consumers own those choices.
A verified installed pack can open without fetching the published manifest; when the
manifest is reachable, it is still used to detect a newer pack.

## Qualification-only compatibility

The frozen TypeScript/PostgreSQL behavior and detailed legacy projection remain only
where differential migration tests require them. Maintainer tools import explicit
qualification entry points; product code must not depend on them. There is no second
supported analyzer or compatibility release line.
