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
decoded. The Worker uses native asynchronous gzip decompression. At startup it inflates
and verifies every generated block once. The reader's generated cache retains at most
36 decoded blocks; a request that needs an evicted block reloads it before retrying the
synchronous analyzer operation.

A lookup for a known but unloaded split/hint block throws
`AnalyzerAnnotationNotLoadedError`. The Worker loads that exact annotation block and
restarts the analysis. It never treats an unloaded fact as absent and never keeps
candidates from the incomplete attempt. Per-request preloaded state is cleared at the
end. The Reader also retains a 16-entry split/hint annotation LRU. Block counts and
cache bounds below are locked generated-release measurements.

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
`packages/core/tests/analyzer-support.test.ts` and
`packages/core/tests/analyzer-annotations.test.ts`. Analyzer integration tests also
cover multiple physical `conj_prop` rows and exact two-stage via-member binding.

## Locked `ichiran-260118` measurements

These values come from `dist/browser-alpha/stats.json`, checked against
`browser-alpha/sources.lock.json`; those generated files are the source of truth for
the release. They are artifact and cache-capacity measurements, not timing claims.

| Measure | Result |
|---|---:|
| Section 4 bytes / SHA-256 | 949,424 / `24632918fa8b5116b983946281107e53ad6e8ac728b517121e6aa9c4955a14f0` |
| Suffix keys / values / classes | 5,532 / 5,533 / 3,586 |
| Counter keys / variants | 760 / 799 |
| Lexical collisions | 5,442 |
| Generated rules / semantic aliases | 1,161 / 1,030 |
| Section 5 bytes / SHA-256 | 3,531,024 / `2ba1615e1a08dbfe458dd8a4ca89201e25aed58844531769dd7bbc0ac26de592` |
| Annotation blocks / splits / hints | 842 / 38,032 / 36,885 |
| Generated blocks / roots / 10-byte records | 37 / 20,347 / 764,828 |
| Generated physical groups / fact-pair entries | 170,717 / 80 |
| Lookup-order records / roots / bytes | 340,437 / 9,635 / 1,361,748 |
| Exact exception surfaces / classes / locators | 1,623 / 3,895 / 4,212 |
| Exact exception bytes | 79,908 |
| Resident index bytes | 264,128 |
| Internal compressed / decoded bytes | 3,266,889 / 23,255,989 |
| Annotation compressed / decoded bytes | 816,578 / 13,838,577 |
| Generated compressed / decoded bytes | 2,450,311 / 9,417,412 |
| Largest generated compressed / decoded block | 78,022 / 262,144 |
| Generated decoded-cache capacity / upper bound | 36 blocks / 9,437,184 bytes |

Release verification reopens both sections and checks their byte lengths, digests,
counts, block bounds, and empty unresolved-support issue set before publication. No
physical-phone performance measurement is implied by these artifact statistics.
