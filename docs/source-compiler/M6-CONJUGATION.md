# M6 source-native conjugation emissions

## Boundary

`conjugation-emissions.ts` enumerates the forward semantic relation directly
from `CanonicalEntry` values and the qualified `kwpos.csv` / `conjo.csv` rule
declarations. It imports no PostgreSQL type, query or export. PostgreSQL is used
only below as a read-only migration oracle.

The compiler keeps two concepts separate:

1. A `ConjugationEmission` says which root form and exact rule lineage produces
   a surface. This is analyzer behavior.
2. `assignPhysicalTargets` chooses a lexical or generated entry that can store
   the emitted form set. This is representation and reuse.

A physical member can carry several property memberships. Those memberships do
not get multiplied back into the semantic emission relation.

## Stable order and identity

Primary emission order is explicit:

1. POS order in the canonical senses;
2. kanji forms in canonical ordinal order, then kana forms;
3. qualified rule declaration order;
4. the first occurrence of each `(pos,type,negative,formal)` group.

Secondary emission order follows the primary emission, its exact emitted forms,
and the qualified secondary rule order. Only positive, non-formal primary types
5, 6, 7, 8 and 53 can chain. Types 5–8 chain as `v1`; type 53 chains as
`v5s`. `vs-i` and `vs-s` never chain. The allowed final types are 2, 3, 4, 9,
10, 11, 12 and 13.

Every secondary semantic emission names the complete primary emission key as
its `via` lineage. Installed reverse candidates use only that explicit lineage,
so an unrelated member of a superset target cannot become analyzer behavior.
The separate physical/CSR projection reproduces the historical target matrix:
lexical targets expose every conjugatable row, while generated targets expose
only direct one-hop reading/form pairs for the current root. Late
`addConjReading` mutations occur after secondary generation and never become
secondary sources.

One forward relation key contains:

```text
root seq
route
surface
source text
first semantic property (including explicit nulls)
second semantic property or explicit null
intermediate surface or explicit null
```

Physical target ids and rule-table row ids are deliberately absent. The digest
uses sorted, length-prefixed UTF-8 keys, so embedded separators cannot make two
identities collide.

Physical allocation is also explicit. `conjugation-scheduler.ts` owns one dense
global stream: base JMdict primary, base JMdict secondary, custom-created
primary, custom-created secondary, then chronological position additions. The
allocator consumes that stream and is deliberately ignorant of phase, source
ids and sequence-number conventions. The
lowest-seq compatible target wins. A target is compatible when its forms are a
superset of the emitted kanji and kana sets; kana-only emissions require a
kana-only target. The root and the exact `via` target are excluded. A new target
is allocated only when no compatible target exists.

## Locked production witnesses

| Root | Source-native result | Qualified evidence |
| --- | --- | --- |
| 1519210 忘れる | Direct type 2/FF emits 忘れた and わすれた in source-form order. | Exact semantic path and source-key match. |
| 1337800 熟す | Type 53/FF emits 熟さす; its explicit `v5s` type 10/FT child emits 熟さしなさい. | Exact semantic path, surface and `via` witness. |
| 1358280 食べる | Potential emits both 食べられる and 食べれる; Passive emits only 食べられる. Both reuse one superset physical target while retaining separate semantic candidates. | PostgreSQL physically merged Potential and Passive before secondary generation. The source relation restores 29 legitimate lineages and 84 route/surface keys. |
| 2089020 だ | Type 1/FT emits です and reuses lexical seq 1628500. `cop-da` is normalized to the qualified semantic POS `cop`. | Exact lexical-reuse witness. Eight colloquial `じゃ` variants belong to the later chronological errata fold, not to the CSV rule core. |
| 1253900 欠く | Potential emits 欠ける, 闕ける and かける and reuses lexical seq 1253920. Its secondary past emits 欠けた, 闕けた and かけた, never 缺けた. | The physical target also contains unrelated lexical form 缺ける. PostgreSQL propagated it into 32 ghost route/surface keys; the source relation intentionally excludes them. |

The five source/rule witnesses produce:

| Root | Emissions | Relation keys | SHA-256 |
| --- | ---: | ---: | --- |
| 1519210 | 197 | 502 | `db957cf652092c5bc7fe78583cec3c226511c6f0e1804d146134d02c9705286a` |
| 1337800 | 198 | 432 | `f7a8c2754d57b6c069af72737c018f8fe0152b78a4bb51422521587a536e3fc0` |
| 1358280 | 197 | 753 | `3bc6bd934e1c44c76f7a5b6963a68132373ff346e33c200de3ab7f08216c4991` |
| 2089020 | 19 | 22 | `a3de52c824e67e08cad5026f9d16c9b92c5dc0fdab0528b85cf350869ffaadd2` |
| 1253900 | 198 | 756 | `28594648a190b7054eef682584e333366325068ed80b78c4965b686898fbca65` |
| Combined | 809 | 2,465 | `75e98fec671acd3e46a806ea8d68c6a254d4444d5fb6e0eb42554b2db915f924` |

All 2,465 complete keys are unique. The read-only PostgreSQL oracle has 780
physical semantic paths and 2,421 joined route/surface keys for the same roots.
That focused rule-core differential is fully accounted for by the three
reviewed cases above: `+29/+84` from restored 食べる lineages, `0/-32` from
removed 欠く ghosts and the eight だ forms supplied by the later chronological
overlay. It is not the release gate.

## Verification and remaining integration

`source-compiler-conjugation-emissions.test.ts` fixes all five witnesses,
superset target reuse, separate property memberships, lexical reuse, secondary
lineage and the combined digest. It runs without a database.

`configured-conjugation-relation.ts` now writes the exact configured forward
relation: only `MorphologySource.roots` positions, chronological suppressions,
all four compiled tombstones and all 50 compiled manual patches. Its output is
accepted directly by the independent packed-reverse proof CLI.

The complete configured gate passes exactly: 8,774,912 unique forward keys and
8,774,912 unique packed-reverse keys, zero duplicates, zero omissions and zero
packed-only results. Both sorted files have SHA-256
`47ff056b9055a21c36971acd1178e08a375c1158150e977c7fe3a8e5ceda9c76`.

## Full bounded physical and surface proof

`writeScheduledGeneratedProjection` replays the five scheduler phases and
writes fixed path rows plus variable UTF-8 occurrence rows. It retains physical
targets and compact numeric lineage only; it never retains the 8.7-million-key
relation in a JavaScript string map. The v14 PostgreSQL-unavailable corpus run
is recorded in `work/m6-evidence/surface-probe-v14.json`; its independent pack
set comparison is `work/m6-evidence/surface-diff-v14.json`.

| Measurement | Source-native | Qualified |
|---|---:|---:|
| semantic paths | 2,470,643 | compiler-owned stream |
| physical occurrences | 8,787,816 | compiler-owned CSR matrix |
| installed reverse occurrences | 8,774,912 | 8,774,912 |
| direct surfaces | 443,275 | 443,275 |
| morphology surfaces | 7,959,940 | 7,959,940 |
| direct/morphology overlap | 9,511 | 9,511 |
| surface input rows | 8,411,378 | 8,411,378 |
| zero-flag omitted rows | 17,674 | 17,674 |

Both accepted surface sets have zero missing and zero extra rows. The v14 run
finished in 3:35.18 with peak RSS 11,303,072 KiB. The report SHA-256 is
`11a91f1b80216a594e556d3b0b6edc9c03025a7131c3fea674b3b3cd3bc03e39`;
the exact-diff report SHA-256 is
`8de344b2410e9f2583ee8ee571307db0057b89afee6d7ebf7edd5c1778d6e603`.

Four provenance-bearing compatibility rows preserve the only irreducible
target-order difference: qualified PostgreSQL allocated root 2410170's narrow
plain-positive type 6, 7, 8 and 53 targets before root 1587490's wider `佚`
targets, while permanent source declaration order encounters 1587490 first.
The rows name both target identities and preserved behavior; there is no
surface list or broad target allowlist.
