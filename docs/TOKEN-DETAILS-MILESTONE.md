# Canonical token details milestone

## Outcome

Consumers receive one lazy, context-aware `TokenDetails` tree from the analyzer.
Browser, Node, and native hosts render the same semantic result without rebuilding
Ichiran's presentation rules from a raw dictionary entry. Grammar is unchanged.

## Verifiable goals

1. A primary token is represented once. `alternatives` contains only genuinely
   secondary retained candidates, in analyzer order.
2. Inflected words expose the exact conjugation forest, including POS-filtered
   meanings, negative/formal flags, and recursive `via` stages. Root meanings are
   not duplicated at the top level.
3. Compounds expose component details and never combine one component's root with
   another component's inflection. Non-primary suffix components retain their
   suffix descriptions.
4. Counters expose value and ordinal status and include only counter senses.
5. Sense restrictions use the analyzer's exact spelling/reading restriction
   relation, including kana normalization and `nokanji` handling.
6. Synthetic entities retain their proper-noun meaning. Field and usage metadata
   survive in the clean model; scores and pack internals remain outside the learner
   presentation contract.
7. Detail hydration remains random-access and cold: opening or analyzing does not
   eagerly read `details.bin`; requesting one token reads only the blocks required
   by that token tree.
8. The same request and pack produce byte-equivalent JSON through Rust native,
   WASM/Core, Browser Worker, and the C ABI.
9. The existing analyzer, romanization, pack-size, reproducibility, and PostgreSQL-
   free runtime gates remain green.

## Product API

The stateless operation is conceptually:

```ts
analyzer.details(text, {
  pathIndex,
  tokenIndex,
  limit,
  entities,
  normalizePunctuation,
})
```

Details are addressed by their original analysis context. The first implementation
may repeat the hot analysis when a token is selected. This keeps ownership obvious,
avoids opaque result handles and mutable last-result state, and is acceptable only
if the measured selection latency remains within the existing interaction budget.

The result is a recursive clean tree containing surface, reading, context-filtered
meanings, components, conjugations, secondary alternatives, suffix information,
counter information, and entity status. Consumers own layout, localization, and
learner-facing labels; the analyzer owns semantic selection and tree construction.

## Work order

1. Define Rust and TypeScript contracts plus normalization fixtures.
2. Reuse the qualified lazy detailed hydrator to construct one clean token tree.
3. Add WASM/Core and C/native operations, preserving the existing missing-detail
   handshake.
4. Add Browser Worker/client plumbing and migrate the demo renderer.
5. Add differential fixtures against the qualified detailed authority and focused
   regressions for alternatives, inflections, compounds, suffixes, counters,
   restrictions, and entities.
6. Run package tests, Rust formatting/lints/tests, C parity, reproducible WASM,
   browser tests, Playwright, size audit, and repository qualification.

## Non-goals

- Restoring the historical public legacy JSON API.
- Moving UI layout, labels, OPFS, or application-shell ownership into the kernel.
- Changing segmentation, scoring, dictionary compilation, or grammar.
- Introducing persistent analysis sessions before measurement proves re-analysis
  is a device bottleneck.
