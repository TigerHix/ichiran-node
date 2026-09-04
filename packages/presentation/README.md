# `@ichiran/presentation`

This package owns learner-facing analyzer terminology. It is deliberately separate from dictionary gloss data:

- `definitionLocale` selects a large dictionary gloss asset (`en`, `zh-Hans`, and future locales).
- `uiLocale` selects this small bundled catalog for POS, fields, conjugations, suffix semantics, entity labels, counters, actions, errors, and accessibility text.
- The analyzer emits stable codes and flags. It never selects a presentation language.

## Translation and LQA workflow

1. Add or revise the English source and its namespace context in `src/catalogs/en.ts` and `src/schema.ts`.
2. A translator updates the exact matching keys in `src/catalogs/zh-Hans.ts`.
3. An LQA reviewer checks terminology in analyzer context, records their review in `review/zh-Hans.json`, and updates `sourceHash` using `bun scripts/check-catalogs.ts --print-source-hash`.
4. `bun run check:catalogs` rejects missing/extra keys, placeholder drift, empty translations, and stale LQA review hashes. CI runs it before compilation.

Unknown future analyzer codes are displayed as their raw stable code, never silently translated through English. A shipping locale must have exact catalog coverage; there is no runtime locale fallback.
