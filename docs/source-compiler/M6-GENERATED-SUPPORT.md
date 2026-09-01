# M6 source-native generated support

## Permanent boundary

`writeScheduledGeneratedProjection` and
`compileBoundedSourceNativeAnalyzerSupport` build the complete generated
analyzer-support projection from:

- final canonical entries;
- the exact positions selected by `MorphologySource.roots`;
- explicit custom-root identities and the first errata event;
- the named chronological copula position addition;
- chronological suppressions;
- the seven reviewed physical-target order splits;
- compiled format-v1 morphology rules, patches and tombstones.

It imports no PostgreSQL query, dump or export. The output is the existing
`AnalyzerSupportGeneratedSource`, so the qualified annotation writer accepts it
without a database-shaped adapter or a pack-format change. The same build also
returns collision facts from `compileAnalyzerSupportCollisions`.

## Scheduler and physical identity

The replayable central scheduler owns the permanent M2 order:

1. base JMdict primary globally;
2. base JMdict secondary globally;
3. custom-created primary;
4. custom-created secondary;
5. chronological conjugation positions and additions.

Within a phase, canonical source ordinal, configured POS order, form ordinal,
qualified rule declaration and rule flag order are explicit. The scheduler
emits dense numeric ordinals and aliases while holding only one root's forms.
`StreamingPhysicalTargetAllocator` only consumes the ordered stream; it cannot
infer phase from a sequence number or source label.

The allocator keeps semantic candidates separate from physical storage. It
reuses the lowest compatible lexical or generated form superset, records exact
primary lineage for each secondary, and assigns semantic member and property
ordinals. A multi-property physical member never creates extra semantic
candidates.

## Generated projection

The two concrete generated-projection spools and their bounded reducers produce:

- canonical semantic rule aliases;
- root/first/second semantic records;
- target count exceptions, including chronological patch forms;
- dense physical groups for shared targets;
- exact final members, prefix members and property cross-products;
- semantic projection and coverage counts/digests.

Target sequence numbers are transient joins. They do not enter generated
records or their semantic digest.

`analyzer-generated-stream.ts` combines direct lexical locators and generated
locators from the bounded occurrence spool on active ambiguous surfaces.
Physical class precedence is derived
from canonical lexical creation events, generated target allocation and manual
patch addition. Higher creation precedence receives lower lookup rank. Those
exact source rows feed the same SCC-condensed graph projection and runtime
replay validator as the qualified producer. Global ranks are used wherever
exact; sparse complete local exceptions cover only cyclic or otherwise
non-global surfaces.

## Focused regression proof

The focused 食べる emission/allocator fixture proves one generated superset
target shared by Potential and Passive while retaining separate semantic
properties. The bounded 忘れる fixture then runs the production scheduler,
physical allocator, spools and reducers and proves the unchanged annotation
writer encodes that compiler-owned result directly.

The PostgreSQL-unavailable test points both database URLs at an unreachable
endpoint. Scheduling, allocation, generated projection, collision projection
and annotation encoding still complete.

## Full bounded projection proof

The v14 full-corpus run writes 2,470,643 fixed semantic path rows and 8,787,816
occurrence rows, of which exactly 8,774,912 are installed reverse candidates.
The surface reducer independently reproduces all 443,275 direct and 7,959,940
morphology surfaces, with 9,511 overlaps and no accepted missing or extra
surface. It preserves the qualified 17,674 zero-flag input rows rather than
silently deleting them. Raw evidence is in
`work/m6-evidence/surface-probe-v14.json` and
`work/m6-evidence/surface-diff-v14.json`.

The run completes with PostgreSQL unavailable in 3:35.18 and peaks at
11,303,072 KiB RSS. Its occurrence volume is bounded because secondary target
reuse writes only physical sources absent from the explicit primary matrix;
it does not replay the whole target superset into every semantic path.

Run:

```sh
bun test packages/data/tests/source-compiler-generated-stream.test.ts \
  packages/data/tests/source-compiler-conjugation-emissions.test.ts \
  packages/data/tests/source-compiler-support-collisions.test.ts
bun run typecheck:compiler
```

The qualified comparison targets are 2,468,434 semantic paths, 2,468,441
matched paths, 733,880 overlay records, 651,728 count exceptions, 170,717
physical groups, 385,967 physical members, 30,948 property overrides and
projection digest
`141db8fe3fc7923f2652ef23459a8a4812471f28435f82702bcd36cd347c9340`.
Every difference from these semantic facts must be named; lookup-order bytes
are separately rebaselined under the approved source-native precedence.

## Qualified generated-order attestation

The final source projection has exactly 2,468,434 semantic paths and 2,468,441
matched paths. The seven additional matches are the distinct manual patch paths
that overlap rule-derived paths; twelve manual-only patch paths are semantic
paths but are not double-counted as matches. The source projection contains
733,451 generated records, 651,013 count exceptions, 169,649 physical groups,
384,226 members and 30,792 property overrides. Its projection SHA-256 is
`218e3cb001d8b4efde96e1ded7a3bd9bd56cbf34d3cc0855a28318bdbc670004`.

The qualified projection has 733,880 records, 651,728 count exceptions,
170,717 physical groups, 385,967 members and 30,948 overrides. Comparing every
normalized record, rather than only totals, leaves 2,478 qualified-only, 2,049
source-only and 8,479 changed records. Dense target and group identifiers are
excluded from this semantic comparison. The physical-group mapping has 167,265
common signatures, no ambiguous member signatures, and digest
`71cab95ba79f2eebaa1783ee77f9b5a0b3501ff191a027eb5404c9c47030822d`.

The exhaustive lookup proof then joins every active source occurrence with the
qualified generated records and physical groups, lexical collisions, direct
forms, global and local lookup ranks, and the immutable packed reverse
morphology relation. Across 212,198 compared surfaces it finds 173,111 exact
rows, 9,799 physical grouping changes and 29,288 ordering-only changes. All
548,607 semantic locators exist on both sides. Source-only, qualified-only,
reverse-source-only and reverse-packed-only locator counts are all zero, as are
collision changes, missing ranks, rank conflicts and incomplete rank
partitions. The 35,306 first-candidate changes are therefore completely
enumerated consequences of replacing the qualified producer's unordered,
concurrently batched root scheduling with the explicit source-native scheduler;
they are not candidate omissions or a broad allowlist.

The qualified SQL inventory reports 208,352 ambiguous surfaces, while the
reachable qualified pack has 208,351. The one-row distinction is the
unreachable `コケさせ` surface: both of its packed reverse candidates are
explicit tombstones, and runtime lookup returns no candidate.

The compact, release-validated record is
`data/source-compiler-generated-order-attestation.json`. It pins the final
producer report, raw and normalized row files, binary spools, exhaustive lookup
report, wall times and peak RSS by byte count and SHA-256. The large NDJSON
evidence remains under `work/m6-evidence`; it is not committed.

The attestation's release gate is mandatory. It names the exact source and
qualified analyzer-support and analyzer-annotations section identities and
their complete artifact count groups. The release comparison validates all
four identities and both pairs of count groups directly; unrelated sections
and count groups remain exact. The attestation itself is the sole numeric source
of truth for the reviewed section identities and count pairs.

Reproduce the lookup proof from the exact final spools without PostgreSQL:

```sh
bun scripts/source-compiler-generated-order-proof.ts \
  work/m2-baseline/hot.bin.gz \
  work/m6-evidence/generated-diff-final-v4-run/generated-paths.bin \
  work/m6-evidence/generated-diff-final-v4-run/generated-occurrences.bin \
  work/m6-evidence/full-locator-universe-final-v2-temp \
  work/m6-evidence/full-locator-universe-final-v2.json \
  work/m6-evidence/full-locator-universe-final-v2.ndjson
```

The producer run took 322.87 seconds and peaked at 17,305,324 KiB RSS. The
lookup proof took 102.44 seconds and peaked at 5,474,320 KiB RSS. The proof-only
qualified decoder lives beside the proof script and is not imported by the
permanent compiler or the release producer.
