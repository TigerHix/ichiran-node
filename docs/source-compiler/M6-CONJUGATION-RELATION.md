# M6 independent conjugation relation proof

## What is proved

The proof compares two relations that are constructed independently:

- **Forward:** compiler-owned canonical roots and qualified source rules emit
  complete semantic keys. The final compiler may instead supply its completed
  key stream after chronological conjugation edits.
- **Packed reverse:** the proof walks every morphology terminal in section 1 of
  the qualified hot pack, then asks the production section-3 reverse matcher for
  every candidate at that terminal.

Neither side reads PostgreSQL. A PostgreSQL relation can still be retained as a
migration oracle, but it is not accepted by this API or CLI.

The complete key is:

```text
root seq, route, surface, source text, source form, source reading,
first (pos,type,negative,formal),
second (pos,type,negative,formal) or null,
intermediate surface or null, source ordinal, source common rank
```

Physical target ids, database ids and pack-local rule ids are absent. Explicit
nulls prevent the nullable-composite omission that affected the old verifier.

## Scale and determinism

Forward and packed keys are written as canonical NDJSON. GNU `sort` runs with
`LC_ALL=C` and can spill into a dedicated temporary directory. The verifier then
merges the sorted files one group at a time. Heap use is independent of the
number of roots and relation rows; disk use grows with the relation.

Each side reports raw rows, unique keys, duplicates and a sorted length-prefixed
SHA-256 digest. The merge reports common keys, omissions, packed-only keys, an
ordered difference digest and per-category counts/digests/examples.

An exact reviewed delta row contains its side, complete canonical key, category,
provenance and preserved behavior. Stale review rows fail the gate. Categories
do not act as broad allowlists: the structural `inactive-route` category is
reported but remains unreviewed until exact rows are supplied. The gate passes
only with no duplicates, no unreviewed differences and no unused review rows.

## CLI

For the final gate, the completed source compiler writes its full unsorted
relation and invokes:

```sh
bun scripts/source-compiler-configured-forward.ts \
  --out work/source-forward.ndjson
bun scripts/source-compiler-conjugation-proof.ts \
  --forward work/source-forward.ndjson \
  --pack work/qualified/hot.bin.gz \
  --reviewed data/source-compiler-conjugation-deltas.json \
  --out work/conjugation-proof.json
```

The source writer is `writeConfiguredConjugationRelation`. It requires the
source-owned position map, the chronological suppression fold and the compiled
morphology artifact. It reports applied and unmatched suppressions/tombstones,
so the live 2684620 deletion and the historical 2257550 ghost cannot disappear
inside a broad difference category. Manual patches are taken from the compiled
artifact; the qualified build therefore writes exactly 50 patch rows.

`--roots canonical-roots.ndjson` is also available for rule-only integration.
It reads one validated `CanonicalEntry` per line and emits every primary and
allowed secondary rule. This mode deliberately sets `complete=false`, because a
root stream alone cannot prove that chronological emission edits such as the
`では` to `じゃ` additions were folded. `--root-limit` and `--surface-limit`
are diagnostic only and also cannot pass the complete gate.

The CLI accepts compressed or installed hot packs, records both hashes, creates
an isolated temporary work directory, refuses to overwrite its report and
deletes only its own temporary directory unless `--keep-work` is supplied.

## Final locked evidence

The final report is
`work/m6-evidence/conjugation-report-exact-20260831.json`. It passed without a
review file or allowed delta:

- forward: 8,774,912 rows, 8,774,912 unique, zero duplicates;
- packed reverse: 7,959,940 morphology terminals, 8,774,912 candidates,
  669 terminals without a candidate, zero duplicates;
- common: 8,774,912; omissions: zero; packed-only: zero;
- relation digest on both sides:
  `acc916310b1547a1af217fec1fba0f8520982eed232dd7ec8cb728bf490cf8fc`;
- byte SHA-256 of both sorted NDJSON files:
  `47ff056b9055a21c36971acd1178e08a375c1158150e977c7fe3a8e5ceda9c76`;
- empty difference digest:
  `e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855`.

The seven named コケる compatibility rows were introduced only after the
unfiltered relation exposed their exact historical physical-lineage closure.
They suppress 54 candidates; the live 2684620 suppression, the explained
2257550 ghost and all 50 manual patches remain separately visible compiler
inputs. No root allowlist or reviewed form list is used.

The smaller synthetic regression comparison also locks:

- forward: 3 rows, 2 unique, 1 duplicate, digest
  `eb2befb13356bc79df5627691e37c0955ce1b9e1a6e855c29b88d60667b8b2b7`;
- packed: 2 rows, 2 unique, 0 duplicates, digest
  `37d33dd703b0e9e3b20569d6b309b875e0b094aff657e48f5a0d5b638317f588`;
- one common key, one inactive-route omission and one exact reviewed
  chronological-errata packed-only key;
- difference digest
  `f46141eed73771f0220ee57183699b93897c71918fd21c633f862f06ccbf3193`.

## Final gate

The configured gate passed. A raw `--roots` run remains a diagnostic and cannot
replace it because it intentionally lacks the configured
position/suppression/manual-patch inputs.
