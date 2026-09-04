# @ichiran/core

The browser-safe product API for the canonical Rust analyzer. It loads one immutable
pack and exposes five operations: open, analyze, resolve one token's canonical
details, romanize, and read a raw dictionary entry. It performs no filesystem,
network, Node.js, or PostgreSQL I/O.

```ts
import { Analyzer, type AnalyzerSource } from '@ichiran/core';

const source: AnalyzerSource = {
  hot,
  lexicon: { source: lexiconSource, sha256: lexiconSha256 },
  locales: { en: englishSource, 'zh-Hans': simplifiedChineseSource }
};
const analyzer = await Analyzer.open(source);
try {
  const result = await analyzer.analyze('食べました', { limit: 3 });
  const details = await analyzer.details('食べました', {
    limit: 3,
    pathIndex: 0,
    tokenIndex: 0,
    locale: 'zh-Hans'
  });
  const romanized = await analyzer.romanize('食べました');
  const index = result.paths[0]?.tokens.find(token => token.entryIndex !== null)?.entryIndex;
  const entry = index === undefined ? null : await analyzer.entry(index, { locale: 'zh-Hans' });
} finally {
  analyzer.dispose();
}
```

`Analyzer.open` takes ownership of the random-access lexicon and locale sources only
after it succeeds. `dispose()` is idempotent and releases WASM resources and those sources.

The stable failure contract is `AnalyzerError` with one of four codes:
`invalid-input`, `invalid-pack`, `not-found`, or `internal`. Rust format and corruption
codes are intentionally not exposed as product API.

Production exports are deliberately small. Release loaders use
`@ichiran/core/release`. Pack compilers use `@ichiran/core/compiler`. Frozen
differential tooling uses `@ichiran/core/qualification` and
`@ichiran/core/qualification/runtime`; neither qualification path belongs in a
shipped application.

The installed release contains a resident hot pack, a language-neutral lazy lexicon,
and lazy locale gloss stores. The hot
pack owns lookup, morphology, scoring, suffix, counter, number, and annotation facts.
Complete dictionary forms, senses, and localized glosses are decoded only when
`details()` or `entry()` is called. English is the default locale; `zh-Hans` selects
Simplified Chinese, with per-sense English fallback when the Chinese source is missing.
`details()` is the normal presentation API: it
applies exact JMdict restrictions, counter filtering, suffix semantics, conjugation
lineage, compounds, entities, and ranked alternatives in the kernel. `entry()` is a
lower-level escape hatch for consumers that explicitly need a raw dictionary row.
Browser and Node hosts load the same bytes and WASM module.

Grammar and a general Kanjidic API are out of scope. See
[MIGRATION.md](../../MIGRATION.md) for the one-way API cutover.
