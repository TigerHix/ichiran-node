# M6 source-native analyzer support facts

This slice moves counter, suffix and collision facts onto the canonical
TypeScript model. PostgreSQL is not imported by any source compiler module. The
qualified `ichiran_oracle_ea958336` database was used read-only to compare the
counter result and to record the remaining full-corpus targets.

## PostgreSQL projections replaced

| M2 projection | Previous database fact | Source-native owner |
|---:|---|---|
| 18 | Lexical collision paths in conjugation/property ID order | `ConjugationEmission`, physical bindings and morphology rule identities |
| 19 | Collision target flags, POS and form counts | Final `CanonicalEntry` senses and forms |
| 20 | First-stage collision lineage | The emission's explicit primary key and physical `viaTargetSeq` |
| 21 | Suffix conjugation hydration | Semantic emissions joined to physical bindings and suffix declarations |
| 22 | Direct split/hint forms | Final canonical forms plus the explicit split and hint declaration ledgers |
| 23 | Generated split locators | Forward emissions, physical bindings and semantic member properties |

The indirect counter queries are also replaced. Counter roots come from final
`pos=ctr` sense properties, `stagk`/`stagr` restrict the canonical forms, and 93
reviewed special-counter rows preserve analyzer behavior. The declaration table
is intended behavior, not a database export: every row names its root and
resulting counter semantics.

## Deterministic rules

- Counter roots are visited by sequence number. Forms use ordinal, creation
  event, event ordinal and text. Variants keep declaration order within a key.
- Accepted counter suffixes and generated ordinal variants are added while
  iterating the live insertion-ordered cache, matching the qualified behavior.
- Ordinary suffix declarations replace a cache key; the one `join` declaration
  prepends. Suffix classes are assigned at the same declaration point.
- Generated suffixes admit a physical target surface only when a strong
  source-owned occurrence reaches it. The admitted surface then inherits the
  complete semantic-property cross-product on that root/target. Reused lexical
  targets keep canonical metadata, and kana-normalization-equivalent lexical
  spellings such as `がり`/`ガリ` are included without inventing conjugations.
  Unrelated spellings and weak-only properties remain excluded.
- Collision facts are keyed and sorted by root, first/second rule, route and
  surface. Canonical target facts are derived once from final senses. A conflicting
  duplicate is rejected.
- Split declarations are evaluated in their source order, while encoded facts
  are sorted by definition, route, surface and kind. Custom hard guards reject
  a candidate; the legacy `def-simple-split` test form keeps its upstream
  early-exit behavior. Direct part ambiguity uses canonical form-creation order,
  the explicit replacement for PostgreSQL text-row insertion order. Part lookup
  is deliberately narrow: exact direct forms are tried before generated forms
  from named roots and from explicitly named lexical targets' real lineage.
- Hint declarations are keyed by definition sequence. Simple hints operate on
  the canonical reading; easy hints alone receive the narrow Kanjidic reading
  map and emit only the zero-width analyzer markers.

No generic repository, query language, ORM-shaped record set, policy object or
SQLite substitute was introduced.

## Evidence

The counter compiler produces 760 keys and 799 variants. A normalized semantic
set comparison against the qualified cache found zero missing and zero extra
rows. Its sorted-record SHA-256 is
`22a711feb7d0395e1c880c5b0012e25e62ee0eb5868d33cafc157a376f888376`.
Global cache insertion order differs because the source compiler visits complete
entries by sequence instead of relying on PostgreSQL's separate kanji-then-kana
query insertion. This order is not encoded: runtime variant order is the explicit
per-key `order`, which is identical in the semantic comparison.

With generated forms deliberately absent, the suffix declaration replay has 76
keys, 77 values and 47 direct classes. This small proof covers replacement, the
joined `ちゃ` bucket, custom roots, explicit `ください` root hydration and null
hydration for ordinary direct declarations. The v6 full projection has exactly
the qualified 5,532 keys, 5,533 values and 3,586 classes. It has no missing or
extra semantic keys. Its normalized SHA-256 is
`876eb57e6399ab0c8d313b3b299a415b884aa4faddb37a1fdb2ec71a7eed6cea`
versus qualified
`7431c7c581f6315fa5102b416317cf221691a211e849a0d2200082a87cc24a33`.
The 46 changed values are preserved as central evidence rather than hidden by
the equal counts: 42 belong to root `1577980` physical candidate order, and four
have identical locators but different physical-target `conjugatable` metadata.
The 36 source/qualified class-identity swaps are the same root-`1577980` family.
Those are allocator/order facts, not suffix-declaration exceptions.

The collision compiler replaces projections 18–20 structurally. Its tests cover
lexical target reuse, direct and secondary lineage, target sense facts,
tombstones, and input-order independence. The corrected-route v8 comparison has
5,442 source and 5,442 qualified facts with zero missing or extra keys. Its
normalized semantic SHA-256 is
`47c273c20d1325a5b8dc02365780b6059aa7053c2e578c39ade43c8423aa5599`
on both sides. Eighty-eight raw rows differ only in the transient generated
`viaSeq`; their target, route, surface, rules and canonical collision facts are
identical. This is exact normalized collision semantics; a final raw identity
attestation remains coupled to the central physical projection.

The first complete annotation enumeration produced all 36,885 qualified hints
with identical encoded semantic rows. Both sides have SHA-256
`55e44040b944c3c3b2742f4831d90cb2aff2a9c686ee7922f5190ff58702236e`.
The v6 split enumeration has exactly 38,032 source and 38,032 qualified facts,
with no missing or extra keys. Four custom conditions that had produced 1,756
empty splits are now typed hard guards and reject; legacy test early-exit remains
unchanged. Generated split form metadata follows physical target order and the
qualified null-best rule. The raw rows still contain 33,354 generated-ID and
metadata changes. After replacing generated IDs with canonical lexical identity,
2,560 observable rows remain: 2,259 locator, 257 ordinal and 100 flag changes,
with overlaps.

The bounded resolver had omitted lineage when a declaration named a lexical
target rather than its conjugation root. Seeding the locator scan with both the
declared roots and every selected path target fixes this generally without
synthesizing a locator. The focused `1405800` target / `1405790` root / `v5k`
type-5 regression passes. Exactly 888 of the v6 residual rows have this shape,
so the next full run is expected to reduce 2,560 to 1,672 before central changes.

The remaining 293 rows that the central normalized record audit could not
classify are now exhaustive and individually categorized. Their sorted semantic
SHA-256 is
`d90d26f3385a6e85d6bfbde10003fba8cf255a3418314d2a7e01efd4370e3d1f`:

| Definition | Rows | Central observable |
|---:|---:|---|
| 1551500 | 9 | target `conjugatable`, identical locator set |
| 1591980 | 12 | target `conjugatable`, identical locator set |
| 1591980 | 4 | locator order only, identical locator multiset |
| 1601010 | 250 | physical target form ordinal |
| 2253080 | 12 | target `conjugatable`; six also have form ordinal |
| 2668400 | 6 | target `conjugatable`, identical locator set |

For definition `1601010`, source target `12392145` groups `上がった` and
`上った`; qualified target `10600203` also groups `騰がった`, shifting ordinals
without changing the selected lineage. The 39 flag rows similarly reflect which
compatible physical target was created first. The four locator-order rows expose
source semantic-property order versus qualified `conjugation.id` insertion order.
They are central allocator/form/property-order differences, not support-owned
membership gaps.

The compact complete support join is
`work/m6-evidence/support-central-join-v6.json` (SHA-256
`894e3ef3528a3c223ecb8c1d113f784abd97ea2ba85f2a4b12267af608332f7e`),
and its central/lookup classification is
`work/m6-evidence/support-central-classification-v6.json` (SHA-256
`dade82efa48ac9080615d114373fd476894536c569a83210a80e5bb7feb82c4b`).
Both retain locator order, ordinal and flag fields.

The annotation declaration proof compares every source-native definition
identity with the frozen reference and the reviewed 260118 overlays:

| Declaration set | Source | Oracle | Missing | Extra | Sorted sequence SHA-256 |
|---|---:|---:|---:|---:|---|
| Legacy split | 172 | 172 | 0 | 0 | `896f42769d22c43d95f24d2001e5c097957c53d24ff38630727048676dd8b202` |
| Qualified split | 174 | 174 | 0 | 0 | `f05314aaa8ba56ae9293c4deebdd1580f957f879b813d8a219f6b8bc67bc0049` |
| Segment split | 18 | 18 | 0 | 0 | `3ae821a6049874e6b59ed3e28d2e5395b52c8f37cbf076b845b417cca98af0a3` |
| Legacy hint | 645 | 645 | 0 | 0 | `62cd88b14d37a0cb340324eed53643cdc5c1de38ce03a0e06f59c04c25199e9d` |
| Qualified hint | 658 | 658 | 0 | 0 | `91c41f4060ebc6619e2de3069bc6fe270cb55e2d7677acfdadefab79f67ffde2` |

The two extra split and thirteen extra hint declarations are the named
`analyzer-upstream-260118` behavior additions, not compatibility exports. Four
focused compiler tests cover declaration identity, direct split and segment
split evaluation, simple and Kanjidic-backed hints, and a generated split part
resolved by ancestor plus semantic lineage. All four pass without PostgreSQL.

Declaration provenance is upstream `tshatrov/ichiran` commit
`ea9583368e67cad22d94abae8dbcc8df96d99bcd`, whose `dict-split.lisp` SHA-256 is
`0b71c312317ab8cc90b0828b868f6aed8742ff8a114bdeb3d4265f4c2302c7a6`.
The frozen TypeScript migration oracle `splitDefinitions.ts` has SHA-256
`8b93dc07041507ca3b50560a3c017761f0d8c00ce3188ce1a88af7093bdc9fab`;
the reviewed 260118 overlay source has SHA-256
`3423711c6ad8fe2ae6dbdd706f850dfb291eb634cadd7330ee2a72e88b4d5e40`.

The v6 full report is `work/m6-evidence/annotation-diff-v6.json`, SHA-256
`cfbdcd8e75e94fdffb407f527a72d5b3beca72452eaaa92cd6b010dfe3e726e6`.
It completed in 2:11.14 with peak RSS 13,462,748 KiB. Its generated path spool
SHA-256 is
`6b90759d51e242633e836ffc068a4c065f240f0d39703ebfac7b852b50c598a9`;
its occurrence spool SHA-256 is
`0d40955b57b64d3dbbfb46feb1e0d10d69ee84811b9c60a27dfc2cbb86a23b80`.
The focused PostgreSQL-free suite includes the existing Kanjidic easy-hint
witness. Its final support slice run passed 11 tests and 47 assertions across
the split/hint, bounded suffix, counter/direct-suffix and collision suites. The
release TypeScript build and `git diff --check` also pass. No source-native
module imports `reference-postgres` or attempts a connection.

## Exact remaining gaps

This slice is a **conditional PASS**, not the whole M6 analyzer-support gate.

- The complete 36,885 hints, 760 counter keys/799 variants, and normalized 5,442
  collision facts are exact. Split and suffix cardinalities are exact.
- The bounded lexical-target locator omission is fixed and focused-green. Its
  exact 888-row effect is derived from the exhaustive v6 join, but a high-memory
  full rerun is intentionally deferred until the central projection stabilizes.
- Every remaining support residual is joined to central physical grouping,
  creator, form-order or property-order behavior. Final unconditional passage
  requires the central attestation and release build to prove or individually
  review those observable ord/flag/order rows; support output must not patch
  them locally.
- Generated annotation records and lookup-order facts are owned by the separate
  generated-support work. They are not claimed here.
- Chronological dictionary errata are owned by `chronological-errata.ts`; the
  earlier custom-root slice itself implements only custom-data chronology.

Accordingly, the support-owned gate passes conditionally: source ownership,
bounded semantic rules, focused regressions and exact cardinalities are in
place, with no database-shaped permanent layer. It becomes an unconditional
PASS only when the final central attestation and PostgreSQL-unavailable release
run confirm the predicted locator reduction and account for the central
ordering/metadata rows without a broad allowlist.
