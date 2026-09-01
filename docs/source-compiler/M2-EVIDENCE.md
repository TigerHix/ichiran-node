# M2 feasibility evidence

## JMdict canonical slice

The source-native parser reads the pinned gzip without PostgreSQL and projects
typed entries, forms, senses, glosses, properties, restrictions, source ordinals
and explicit creation order. A length-prefixed semantic digest comparison against
the qualified producer found:

- source entries: 214,698;
- qualified producer root XML entries: 214,697;
- equal entries: 214,697;
- mismatched common entries: 0;
- source-only: JMdict seq 2611370;
- source digest: `b1ee4927144c2a3c65fa5e50f4bc5238875af6534c019dbde95432318e6a857b`;
- producer digest: `88b1b9feb43ab89760711c99f2b50e52bbd69cb1d6536c26e83a1675f3f02375`;
- comparison JSON digest: `ad4f05b621846295fde08640805e00fc97f3a7f67e1759f96e1dc3d9c3571110`.

Seq 2611370 is not an opaque discrepancy: upstream `dict-errata.lisp` deletes all
senses, clears `root-p`, and removes written form `為り`. It is the single JMdict
compatibility row.

## Representative behavior

- Primary: 1519210 忘れる/わすれる (`v1`) produces
  忘れた/わすれた, qualified rule 99 and path type 2/FF.
- Secondary: 1337800 熟す/じゅくす produces 熟さす then
  熟さしなさい through the explicit first-stage member.
- Physical target reuse: 1358280 食べる shares the 食べられる physical target
  between Potential and Passive while 食べれる remains semantically Potential
  only. Physical property cross-products are not forward semantic candidates.
- Lexical reuse: 2089020 だ produces です and reuses lexical seq 1628500.
- Negative lineage: 1253900 欠く must not acquire 缺け from an unrelated member
  of its reused physical intermediate target.
- Best readings: the JMdict fixture proves restriction-aware reciprocal kanji/kana
  selection.
- Kanjidic: the narrow compiler input reproduces the `def-easy-hint` result for
  2140350 時は金なり as `とき<space><modifier>は<space>かね<space>なり`.
- The qualified root and detail encoders accept compiler-owned semantic structures
  directly and produce deterministic bytes; no database-shaped adapter is used.

## Locked conjugation evidence for M6

- PostgreSQL relation: 9,173,122 keys in 7,959,271 route/surface groups.
- Qualified packed candidates over that database-selected domain: 8,774,911.
- Legacy-only: 398,211; packed-only on that restricted domain: 0.
- Difference digest: `6eb1cbae46eb2df0c67570764d0cf408bf4d7b8873eb4b53ce694cd17549d421`.
- Generated groups: 170,717; physical members: 385,967; property overrides: 30,948.
- Generated projection digest: `141db8fe3fc7923f2652ef23459a8a4812471f28435f82702bcd36cd347c9340`.
- Ghost source rows: 19,456; ghost root/surface pairs: 19,437.

An independent traversal of packed section-1 found 7,959,940 terminal surfaces,
8,774,912 candidates and 669 terminals with no candidates. The extra candidate
proves the existing database-selected verifier cannot establish global
`packed-only = 0`. M6 must enumerate both domains independently and compare both
runtime identity and fuller provenance evidence keys.

## Gate

Source identity, the canonical slice, representative conjugation, best readings,
Kanjidic easy hints, encoder ownership and every SQL projection are established.
The direct-root ordering conflict in `M2-ORDERING.md` leaves the required
compatibility data broad and non-reviewable. M2 is **FAIL** pending the explicit
behavior choice, so M6 has not started.

## Verification run

- Exact-source acquisition: PASS; reconstructed gzip hashes are
  `92eb77d6…` (JMdict) and `1861f294…` (Kanjidic2).
- Compiler typecheck: PASS.
- Compiler build: PASS.
- Source-compiler tests: 8 pass, 0 fail, 26 assertions.
- Full data-package run: 44 pass, 3 PostgreSQL suites skipped, and 2 legacy
  database test files stopped in shared setup because `ICHIRAN_DB_URL` was not
  configured. No assertion in those files ran or failed. The qualified oracle was
  deliberately not exposed to mutating legacy tests.
