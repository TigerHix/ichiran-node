# Analyzer support and generated-fact pack

This pack removes the analyzer's remaining PostgreSQL and Kanjidic reads without
shipping the separate experimental `@ichiran/grammar` package. It freezes the small
analyzer programs and exceptional facts that cannot be reconstructed from the surface,
root, and reverse-morphology sections alone.

## Ownership and storage

`hot.bin` owns two analyzer-specific sections:

| section | ID | reader | ownership |
|---|---:|---|---|
| analyzer support | 4 | `AnalyzerSupportReader` | resident suffix/counter tables, morphology rule aliases, and lexical-target collision facts |
| analyzer annotations | 5 | `AnalyzerAnnotationsReader` | lazy split/hint blocks, generated physical-member overlay, and exact lookup order |

Section 4 is directly readable from the hot image. Section 5 has a resident header and
sorted indexes followed by independently checksummed gzip blocks. It is still part of
`hot.bin`; it is not a third installed asset and it is not part of `details.bin`.

For split and hint data, one definition sequence is one compressed block. For generated
facts, consecutive root sequences are grouped into blocks targeting about 256 KiB
decoded. The Worker uses native asynchronous gzip decompression. At startup it performs
one complete prewarm of the pinned pack's 36 generated blocks, then exposes synchronous
`PreloadedAnalyzerAnnotations` views to analyzer requests.

A lookup for a known but unloaded split/hint block throws
`AnalyzerAnnotationNotLoadedError`. The Worker loads that exact annotation block and
restarts the analysis. It never treats an unloaded fact as absent and never keeps
candidates from the incomplete attempt. Per-request preloaded state is cleared at the
end. The Reader retains all 36 prewarmed generated blocks (9,336,624 decoded bytes) and
a 16-entry split/hint annotation LRU with a 3,603,164-byte source-payload upper bound.

The same section stores physical lookup precedence without retaining PostgreSQL row or
generated-target identifiers. Most semantic locators use a six-bit level from the
SCC-condensed global precedence graph. A small resident table stores complete dense
local ranks for the exact `(route,surface)` keys where global levels cannot reproduce
the observed order. Exception selection is atomic: once a surface is in that table,
every direct/generated locator must resolve locally and a missing locator is an error,
never a fallback to its global level.

## Frozen analyzer inputs

The read-only PostgreSQL compiler freezes:

- the ordered suffix cache, materialized suffix forms, suffix classes, and exact
  scoring conjugations;
- the ordered counter cache, its eleven runtime counter classes, and digit,
  phonology, validation, ordinal, and foreign-number options;
- compiled outputs for registered split, segsplit, and easy-hint definitions over
  every applicable direct or morphology form;
- the morphology rule-to-semantic-alias map used by the generated overlay;
- lexical-target collision facts for the small class where a generated target reuses
  a real root entry and therefore observes that entry's scorer flags; and
- exact generated target count and physical conjugation-member exceptions described
  below; and
- the semantic direct/generated lookup-order relation observed by the legacy bulk
  text lookup followed by `unshift`.

Counter and suffix ordering is explicit and observable. The suffix reader indexes only
known UTF-16 key lengths and returns longest matches first. The runtime retains the
small suffix-handler program because handler eligibility is recursive and algorithmic;
precomputing every stacked suffix surface would recreate the database closure.

## Why a physical-member overlay is required

Reverse morphology provides a semantic one- or two-rule path, which is the right public
identity. The legacy analyzer, however, can observe physical multiplicity inside a
generated target:

- several physical `conjugation` rows can implement the same semantic rule path;
- one physical conjugation can own several ordered `conj_prop` rows;
- a two-stage row selects one particular physical member of its intermediate target;
- several semantic paths can share one physical target; and
- a generated target's `n_kanji` or `n_kana` can differ from its lexical root.

A scalar property per semantic path loses this information. In particular it collapses
Potential/Passive and other multi-property rows, changes presentation order, and can
bind a secondary path to the wrong intermediate member.

The final overlay therefore keeps every exceptional physical member. It is keyed by
canonical root sequence plus one or two compact semantic rule aliases. Ordinary paths
whose counts, multiplicity, and exact property all equal the semantic default have
no record and use reverse morphology directly.

`physicalGroup` is a dense, pack-local identity shared only when several semantic paths
reach the same physical target. `memberOrd` is the stable order of a physical
conjugation within that target. `propOrd` preserves the physical `conj_prop` order
within the member. `viaMemberOrd` binds a two-stage final member to the exact member of
the intermediate target. None of these values exposes a PostgreSQL generated `seq` in
the clean API.

The pre-existing lexical-target collision table in section 4 has a narrower job. When a
generated target is also a real lexical entry, scorer flags such as POS, archive,
prefer-kana, particle/copula membership, and split definition identity cannot be
replaced with root facts. That table preserves those exact exceptional facts. It does
not stand in for the general physical-member overlay.

## Ten-byte generated record

Each generated row in a decoded section-5 block is exactly 10 bytes:

| bytes | field | meaning |
|---:|---|---|
| 4 | semantic key + `propOrd` | low 22 bits encode the one/two-rule alias key; high 10 bits preserve property order |
| 1 | count fact | `0` means root counts; otherwise indexes a resident `(nKanji,nKana)` pair |
| 3 | physical identity | 18-bit `physicalGroup`, 3-bit `memberOrd`, and 3-bit `viaMemberOrd`; `7` means no via member |
| 2 | exact property | 5-bit morphology POS index, 6-bit type, two 2-bit false/true/null negative/formal values; bit 15 stays clear and `0xffff` is the count-only sentinel |

Repeated low-22-bit semantic keys are intentional. The reader aggregates all contiguous
rows for the key into an ordered `members[]`; it must not insert them into a scalar map
that overwrites duplicates.

A count-only exception has a non-zero count fact, zero physical identity, and property
`0xffff`. It decodes to `members: null`. A member-bearing exception may also carry a
count fact, and decodes every physical row with its exact tri-state property. The writer and
reader reject out-of-range aliases, ordinals, non-dense groups, empty exceptions,
invalid tri-state flag codes, malformed ordering, and inconsistent source statistics.

## Lookup-order provenance and encoding

The compiler observes heap-tuple order only inside its read-only snapshot, then discards
it. Its locked source digest covers the complete sorted semantic relation
`(route,surface,physicalRank,rootSeq,firstAlias,secondAlias)`. A second digest covers
the emitted global levels plus complete local exceptions. Neither relation includes a
CTID, text-row ID, physical generated sequence, or other surrogate identifier.

Global locator levels remain in the root-keyed generated blocks as one packed `u32`
per locator. Section 5 format 4 also has a resident, exact-string exception directory:
a 16-byte span per `(route,surface)`, an 8-byte `(rootSeq,semanticKey,localRank)` row per
locator, and concatenated UTF-8 surface bytes. Local ranks are dense physical-class
ranks. The build proves every source surface through the same atomic local/global
selection used at runtime, including uniform ranks for all semantic locators merged
into one physical candidate.

## Split, hint, and Kanjidic boundary

Registered split and hint functions execute at build time against every applicable
form. The runtime stores the resolved split parts and final hinted reading; it contains
no split-definition closures and performs no Kanjidic lookup.

Kanjidic is used only by the build-time `matchReadings` path needed to resolve easy
hints. The alpha does not ship characters, meanings, radicals, strokes, or a general
Kanjidic API. Komi and Nemu integration are outside this standalone milestone.

## Suffix handler boundary

The pack replaces suffix cache initialization and all database facts used by suffix
candidates. It deliberately does not encode the active handler registry as an
unbounded surface-to-compound table. Numeric modifiers remain score multipliers;
function modifiers compile to constants; abbreviation handlers create proxy or
retargeted candidates; and the `:suru` break reuses the frozen suffix cache form.

This code is analyzer-internal morphology and scoring behavior. It is not the excluded
experimental grammar package.

## Build and verification APIs

- `loadAnalyzerSupportSource(sql)` loads suffix, counter, annotation, collision, and
  generated projection inputs in the release transaction.
- `buildAnalyzerSupportCore(source)` emits section 4.
- `buildAnalyzerAnnotations(source.splits, source.hints, source.generated)` emits
  section 5.
- `AnalyzerSupportReader` and `AnalyzerAnnotationsReader` have no database or Node
  runtime dependency.

Focused round-trip, determinism, ordering, lazy-load, and corruption tests live in
`packages/portable/tests/analyzer-support.test.ts` and
`packages/portable/tests/analyzer-annotations.test.ts`. Analyzer integration tests also
cover multiple physical `conj_prop` rows and exact two-stage via-member binding.

## Current qualification measurements

The deterministic `alpha.1-dev` qualification pack records the following values. The
clean final release must reproduce these data sections; its own `stats.json` remains the
release of record.

| Measure | Result |
|---|---:|
| Section 4 bytes / SHA-256 | 946,872 / `3777d5a6f9c35c6165e0324adb6e9e53ec24b1df3af15036e198980f8fa51d92` |
| Section 5 bytes / SHA-256 | 3,491,312 / `ddea80b011220a310515cfd3d6a8536857a4f60d78772b8f5d47b86d0504acda` |
| Annotation blocks / splits / hints | 827 / 37,594 / 36,388 |
| Generated blocks / roots / 10-byte records | 36 / 20,115 / 759,040 |
| Count exceptions / physical groups / physical members | 646,450 / 169,714 / 383,470 |
| Property overrides | 30,730 |
| Maximum member / via-member / property ordinal | 5 / 2 / 3 |
| Resident index bytes | 258,496 |
| Section-5 internal compressed / decoded bytes | 3,232,807 / 23,012,698 |
| Largest generated compressed / decoded block | 77,823 / 262,104 |
| Generated startup prewarm / exact decoded bytes | 36 blocks / 9,336,624 |
| Split/hint annotation LRU / source-payload upper bound | 16 blocks / 3,603,164 |

The hybrid lookup-order proof adds 335,873 stored rank records plus 1,494 exact
exception surfaces (3,725 classes and 4,123 locators). Its canonical projection SHA-256
is `016dae9f069ad984e23cd495df7cb79e2e0685c54ffe011bf3b70c0ad283b122`.
