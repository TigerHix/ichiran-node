# Ichiran surface index compiler

This build-time tool turns a bytewise-sorted TSV stream into the browser
analyzer's compact route-aware surface automaton. It has no crate dependencies.

Each input row has exactly five tab-separated fields:

```text
surface  kana_direct  kana_morph  kanji_direct  kanji_morph
```

Flags are `0` or `1`. Input surfaces must be unique and ascending by UTF-8
bytes (`ORDER BY text COLLATE "C"` in PostgreSQL). The compiler applies the
same route as the analyzer: a non-empty surface containing only Ichiran's kana
ranges uses the kana flags; every other surface uses the kanji flags. A row
whose active route has neither flag is deliberately omitted.

By default the tool reads stdin and writes the binary index to stdout. Optional
`--input` and `--output` paths make local diagnostics less awkward.
