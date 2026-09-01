# M2 PostgreSQL projection inventory

This is the complete qualified-producer inventory at `effd10f1`. PostgreSQL is a
migration oracle for these comparisons, not a permanent source-compiler input.
The replacement column names the compiler-owned semantic fact that must replace
the query.

## Direct qualified-producer projections

| # | Location | Projection and observable use | Source-native replacement |
|---:|---|---|---|
| 1 | `scripts/browser-alpha-release.ts` | Server/database/schema identity and `pg_dump` schema hash. Qualification only. | Release records source-lock and compiler identities; schema proof remains oracle-only evidence. |
| 2 | `surface-index.ts` | Union of direct root forms and generated morphology surfaces, PostgreSQL C order. | Canonical direct forms plus forward conjugation emissions, sorted by UTF-8 bytes. |
| 3 | `root-payload-oracle.ts:42` | Root seq, form counts, primary-no-kanji, archived and prefer-kana flags. | `CanonicalEntry` plus final chronological mutations. |
| 4 | `root-payload-oracle.ts:42` | Active root POS values. | Ordered sense properties after archived-sense filtering. |
| 5 | `root-payload-oracle.ts:42` | Direct kanji/kana forms; `row_number` by `ctid DESC` becomes lookup precedence. | Canonical forms with explicit creation-event order; baseline CTID conflict is the M2 blocker. |
| 6 | `root-payload-oracle.ts:42` | Restricted reading/written pairs. | JMdict `re_restr` facts. |
| 7 | `root-payload-oracle.ts:42` | Unordered legacy lookup followed by `unshift`, used to prove the direct order. | Oracle-only parity check. |
| 8 | `details-oracle.ts:37` | Root entry sequence list. | Canonical roots ordered by seq. |
| 9 | `details-oracle.ts:37` | Direct forms and form metadata. | Canonical forms. |
| 10 | `details-oracle.ts:37` | Senses and ordinals. | Canonical senses. |
| 11 | `details-oracle.ts:37` | Glosses and ordinals. | Canonical glosses. |
| 12 | `details-oracle.ts:37` | Sense properties ordered by tag, ord and physical id. | `(tag, ordinal, propertySourceOrder)`; 75 qualified equal-tag/ord groups require the last field. |
| 13 | `morphology-compiler-oracle.ts:13` | Conjugating root/POS pairs. | Forward emission roots and ordered POS facts. |
| 14 | `morphology-compiler-oracle.ts:40` | Root form sets. | Canonical form sets. |
| 15 | `morphology-compiler-oracle.ts:52` | Two manual patch families (`では` and expression cases). | Named compatibility declarations with upstream code provenance. |
| 16 | relation verifier | Full PostgreSQL reverse-conjugation relation. | Migration oracle only; M6 independently enumerates the source-forward relation. |
| 17 | relation verifier | Relation counts and digest. | Evidence only. |
| 18 | `analyzer-support-oracle.ts:228` | Generated/lexical collision paths ordered by physical conjugation/property ids. | Canonical lineage keys, including `via`. |
| 19 | `analyzer-support-oracle.ts:228` | Collision-entry flags and POS. | Canonical lexical entries. |
| 20 | `analyzer-support-oracle.ts:228` | First-stage collision lineage. | Explicit first emission/member lineage. |
| 21 | `analyzer-support-oracle.ts:469` | Suffix conjugation/property hydration. | Suffix declarations joined to semantic member/property ordinals. |
| 22 | `analyzer-support-oracle.ts:571` | Direct forms required by split/hint annotations. | Canonical forms. |
| 23 | `analyzer-support-oracle.ts:664` | Generated split locators. | Forward emission locators. |
| 24 | `analyzer-generated-oracle.ts:64` | Primary/secondary paths, rule aliases, manual patches, physical target reuse, member/property/via ordinals and overlay facts. | One forward `ConjugationEmission` stream folded into a separate physical-target model. |
| 25 | `analyzer-generated-oracle.ts:255` | Generated lookup classes ranked through `max(form.ctid)`. | Complete semantic class key and explicit creation-event precedence; remaining CTID baseline conflicts require a reviewed choice. |

## Indirect helper projections

| Helper | Current database dependency | Replacement |
|---|---|---|
| Suffix registry | Unordered unions of direct/generated forms and unordered properties. | Freeze declaration vectors, then semantic member/property order. |
| Counter compiler | Roots/forms ordered by seq and ord; equal ord was implicit. | Seq, route, form ordinal, creation event and text. Qualified counter roots have no equal form ordinals. |
| Split registry | `ANY` queries whose first row was sometimes consumed. | Resolve exact canonical seq/form keys and reject ambiguous declarations. |
| Hint registry | Direct/generated readings and Kanjidic reading queries. | Canonical forms/emissions plus compiler-only Kanjidic document-order readings. |
| Best readings | Unordered bulk updates and partial `ORDER BY ord`. | Restriction-aware derivation over the final canonical entries. |
| Conjugation target lookup | Form-superset query choosing the lowest seq. | Central target interner; preserve lowest compatible lexical seq. |
| Primary lineage | Conjugation/property/source-reading joins. | First-stage emissions. |
| Secondary lineage | Unordered `DISTINCT ON (from,seq)`. | Join only explicit first-stage lineage; never all forms on a reused physical target. |
| Archived/prefer-kana helpers | Derived tables and correlated sense queries. | Final sense properties. |
| Kanjidic | `kanji`/`reading` rows; unordered first-match behavior. | Narrow easy-hint input preserving XML character/reading order. |

There are no application-defined database functions, views, ORM models, or
repository abstractions in this path. SQL built-ins (`jsonb_to_recordset`, window
functions, arrays and regular expressions) shape the projections but add no
authoritative semantics.
