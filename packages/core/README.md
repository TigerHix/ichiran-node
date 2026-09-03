# @ichiran/core

The browser-safe product API for the canonical Rust analyzer. It loads one immutable
pack and exposes four operations: open, analyze, romanize, and read a dictionary
entry. It performs no filesystem, network, Node.js, or PostgreSQL I/O.

```ts
import { Analyzer, type AnalyzerSource } from '@ichiran/core';

const source: AnalyzerSource = { hot, details };
const analyzer = await Analyzer.open(source);
try {
  const result = await analyzer.analyze('食べました', { limit: 3 });
  const romanized = await analyzer.romanize('食べました');
  const index = result.paths[0]?.tokens.find(token => token.entryIndex !== null)?.entryIndex;
  const entry = index === undefined ? null : await analyzer.entry(index);
} finally {
  analyzer.dispose();
}
```

`Analyzer.open` takes ownership of the random-access detail source only after it
succeeds. `dispose()` is idempotent and releases WASM resources and that source.

The stable failure contract is `AnalyzerError` with one of four codes:
`invalid-input`, `invalid-pack`, `not-found`, or `internal`. Rust format and corruption
codes are intentionally not exposed as product API.

Production exports are deliberately small. Release loaders use
`@ichiran/core/release`. Pack compilers use `@ichiran/core/compiler`. Frozen
differential tooling uses `@ichiran/core/qualification` and
`@ichiran/core/qualification/runtime`; neither qualification path belongs in a
shipped application.

The installed release contains a resident hot pack and a lazy detail store. The hot
pack owns lookup, morphology, scoring, suffix, counter, number, and annotation facts.
Complete dictionary forms and senses are decoded from the detail store only when
`entry()` is called. Browser and Node hosts load the same bytes and WASM module.

Grammar and a general Kanjidic API are out of scope. See
[MIGRATION.md](../../MIGRATION.md) for the one-way API cutover.
