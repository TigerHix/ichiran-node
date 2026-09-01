# M2 ordering audit and replacement policy

## Gate result

The qualified direct-root order is physical, not semantic. `maintenance.ts`
performs unordered whole-table best-reading updates, producing new heap tuples.
`root-payload.ts` then freezes `ctid DESC` as lookup order.

| Comparison with qualified `ctid DESC` | Delta surfaces | Forms | First winners | Evidence SHA-256 |
|---|---:|---:|---:|---|
| January JMdict document events only | 3,391 | 14,214 | 3,115 | `9a75e5d6011cfdd6c4a1c882ca0d16a5f92f88266d01d36e2247b6813212cd19` |
| Complete final canonical entries | 3,435 | 14,387 | 3,148 | `5f4660a0afbc1a21021f3c4db49014554a3b7991a48960c2238a050ae05a1854` |

The complete lossless direct-order evidence has 3,435 surface rows and 4,076,458
JSONL bytes. It is a broad baseline export, not small reviewable compatibility
data. Only 179 of 3,391 base deltas are confined to one ten-entry parallel load
batch; unordered maintenance heap placement is the dominant cause.

The user explicitly chose source-native precedence and an intentional rebaseline
on 2026-08-31. The broad compatibility ledger is rejected. The 3,148 changed first
candidates are now the named M6 review domain, not an allowlist.

## Permanent deterministic rules

1. Mutations run in one order: JMdict document order, primary generation,
   secondary generation, custom declarations, errata declarations in code order,
   then best-reading derivation. Mutating phases never run in parallel.
2. A form or property owns `(creationEvent, ordinalWithinEvent)`. Replacing a
   value retains identity unless the declaration explicitly deletes and recreates
   it. Best-reading derivation never changes creation order.
3. Direct precedence is mutation phase descending, creation event descending,
   form ordinal descending, seq descending.
4. Detail forms are kanji before kana, then ordinal and UTF-8 text. Sense and
   gloss ordinals must be unique. Properties use tag, ordinal and source order.
5. Best reading selects the first restriction-eligible form by ordinal, creation
   event and UTF-8 text. Qualified roots have no equal form-ordinal ties.
6. Primary conjugation orders root source, POS source, route/form, rule declaration
   and FF/FT/TF/TT variant explicitly. One central interner owns target mutation.
7. Secondary conjugation follows explicit first-stage lineage. It never chooses an
   arbitrary `DISTINCT ON` row or chains every form on a reused physical target.
8. Target reuse preserves the qualified superset rule and selects the lowest
   compatible lexical seq. New physical identities are assigned by complete
   semantic signature; members, properties and `via` have semantic ordinals.
9. Suffix ordinary declarations replace and `join` declarations prepend. Counter,
   split and hint declaration order is explicit; undeclared duplicate keys fail.
10. Maps and sets may deduplicate and index only. Observable vectors use a complete
    semantic comparator; equal keys must have equal values.
11. Surface TSV remains UTF-8 byte ordered. Pack sections remain 1 through 5.
    Release filename comparisons must not use locale-sensitive order.

## Other observed partial orders

- Secondary `DISTINCT ON` has 57,376 pairs; 3,855 have two eligible first-stage
  types and 2,015 differ from minimum physical order. The choices are currently
  output-equivalent because none selects type 53, but the permanent rule must not
  depend on that accident.
- Details contain 75 equal `(tag,ord)` property groups (150 rows).
- Qualified generated lookup has 208,352 surfaces and 492,547 physical classes.
  Its 1,623 encoded local-exception surfaces are graph compression facts, not a
  substitute for the 3,435 direct-root ordering-evidence rows.
- Qualified counter roots have no equal form ordinals. Kanjidic non-name readings
  have no duplicate `(literal,reading)` keys.
