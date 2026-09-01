# M6 chronological conjugation errata

`conjugation-errata.ts` consumes the conjugation-affecting declarations selected
from the pinned 601-row `data/source-compiler-errata.json` ledger. The ledger is
the chronology and provenance authority; the fold does not query PostgreSQL and
does not preserve database rows as compiler input.

The qualified ledger contains 13 conjugation-affecting declarations:

- one `conjugateDa` declaration and one `addDehaJaReadings` declaration;
- one `addGozaimasuConjs` declaration;
- two `deleteConjugation` declarations;
- four `addConjReading` declarations;
- three `rearrangeReadingsConj` declarations;
- one `replaceReadingConj` declaration.

Direct root edits are applied first by `applyQualifiedErrata`. The conjugation
fold then uses the final canonical roots and the source-native forward emission
APIs to validate that added, reordered and replaced readings generate the
declared lineages. The two historical physical-target deletions are translated
to semantic suppressions:

| Root | Source | Surface | Property | Oracle target provenance |
|---:|---|---|---|---:|
| 2257550 | `ない` | `な` | `adj-i`, type 51; named retained ghost | 2029110 |
| 2684620 | `しい` | `し` | `adj-i`, type 51 | 2086640 |

The oracle target ids are retained only inside provenance. The permanent
compiler result is expressed by root, source surface, generated surface and
semantic property.

Root 2257550 is intentionally asymmetric: its final source POS is
`aux-adj`/`suf`, so a fresh forward fold no longer emits the historical
`adj-i` type-51 row. The qualified database had retained that older generated
lineage and the chronological ledger explicitly deleted it. The fold therefore
accepts the absence only for the exact reviewed `2029110/2257550` identity.
Every other deletion, including `2086640/2684620`, must match a live
source-native emission.

## Qualified manual patches

The fold derives the `じゃ` variants from the ordinary `だ` forward emissions,
then applies the upstream `では` to `じゃ` transformation. It emits the six
declared `ございます` replacements for every canonical form of roots 1612690
and 2253080.

| Source declaration | Rows |
|---|---:|
| `addDehaJaReadings` | 8 |
| `addGozaimasuConjs` | 42 |
| Total | 50 |

The deterministic `MorphologyManualPatchSource` projection has SHA-256
`ec2957e97afd0421a567febdd181ec1743450874a1cc3fd0aa8d3148d4e7e022`.
Rows are ordered by route, surface, root sequence, source text, POS, type,
negative and formal values, serialized as compact JSON plus LF, and hashed in
that order.

The qualified PostgreSQL comparison used exactly the existing migration-oracle
projection from `loadManualPatches`: conjugations from root 2089020 whose source
reading begins `じゃ`, plus `exp` conjugations from roots 1612690 and 2253080.
Its 50 compact JSON rows have the same digest. There are no source-only or
oracle-only rows.

## `addConjReading` differential

The four `addConjReading` declarations are enumerated independently from the
forward rules rather than copied from physical generated entries:

| Root/source | Source-native rows |
|---|---:|
| 1008370 `デカい` | 22 |
| 1566420 `ハメる` | 59 |
| 1572760 `クドい` | 22 |
| 1593170 `コケる` | 59 |
| Total | 162 |

The ordered source-native projection digest is
`04d77a77d0113eea9f878f2468c0833e9c26fe1bf42865adbd952f25795dd408`.
The PostgreSQL projection contains 159 rows with digest
`b03fd816a1529bf7738f549c513454c5c56b778bea4e03ee52a90840345e4a79`.
They share 155 exact rows. Every delta is individually accounted for:

- Source-only: `コケないで`; `コケますならば`; `コケませんならば`; and
  the four type-5 `コケれる` variants.
- PostgreSQL-only: the four type-6 `ハメれる` variants.

These are the known result of the historical `add-conj-reading` loop deriving
one reading from each physical target's ordinal-zero member. Merged physical
targets and their insertion order therefore selected or omitted lineages. The
source fold instead enumerates the declared source reading through the rule
relation. This is an explicit seven-versus-four semantic differential, not an
allowlist or a permanent database input.

`chronologicalMorphologySource(entries, rows)` is the pack boundary. It returns
the existing compiler-owned `MorphologySource`, with these 50 manual patches
attached, for direct use by the unchanged format-v1 writer.

The adapter also accepts `extraPositions`. Root orchestration passes the named
`conjugation-position` rows from the central compatibility ledger through this
option. The chronological fold does not hardcode their sequence numbers or
broaden ordinary `cop` roots into historical copula roots.

## PostgreSQL-unavailable proof

The targeted test sets `DATABASE_URL` to an unresolvable host, builds a complete
format-v1 morphology section from compiler-owned roots, and verifies:

- 50 compiled manual patches;
- all four qualified tombstones;
- lookup of representative `じゃない` and `ございません` patches;
- absence of the deleted `ない -> な` and `しい -> し` lineages.

Run it with:

```bash
bun test packages/data/tests/source-compiler-conjugation-errata.test.ts
```

No PostgreSQL server, dump, SQL query or database-shaped repository layer is
used by the fold or adapter.

The complete 217,967-root integration was also run with both `DATABASE_URL` and
`ICHIRAN_DB_URL` set to `postgres-unavailable.invalid`. With the two named
compatibility positions supplied by root orchestration, it reproduces the
qualified morphology section byte for byte:

| Measurement | Source-native | Qualified lock |
|---|---:|---:|
| bytes | 2,688,176 | 2,688,176 |
| SHA-256 | `1614d150f3609b9de4f93de5ad0e33e12aec41211dc9096870a8d019eab9c0f3` | same |
| positions | 22 | 22 |
| rules | 1,161 | 1,161 |
| templates | 7,211 | 7,211 |
| suffixes | 2,568 | 2,568 |
| root keys | 40,882 | 40,882 |
| root groups | 14,608 | 14,608 |
| manual patches | 50 | 50 |
| tombstones | 4 | 4 |
