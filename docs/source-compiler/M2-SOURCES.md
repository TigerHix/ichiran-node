# M2 source provenance

The machine-readable lock is `data/source-compiler-sources.lock.json`.
`data/source-compiler-compatibility.json` contains the small, individually
reviewed historical compatibility facts.

## Qualified producer

- Portable baseline tag: `portable-core-260118-baseline`.
- Ichiran data release: `ichiran-260118`, tag commit
  `0c37354003840076a32459d56e9e91c69d7df119`.
- Qualified upstream code: `ea9583368e67cad22d94abae8dbcc8df96d99bcd`.
- Historical dump: 200,012,956 bytes, SHA-256
  `98a44e2cc88a65677da8b1f7124e7d6c904253eb1aae0ef16d2c7cc1dacdba82`.
- The release states that it is based on JMdict from January 1, 2026.

## JMdict

Exact `JMdict_e` for 2026-01-01 is reconstructed from the Jitendex daily EDRDG
archive at commit `2bdfbdcadaf38a7da3000f68f93ce711c7d5a878`. The target patch is
`JMdict_e/patches/2026/01/01.patch.br` (2,058 bytes; Git blob
`30567dfa46bfb2b124b2cdabe04d7e344f4b3919`; decompressed-payload SHA-256
`53bfb4a8de7f16b3242def83c6e72368a068bdc796054727c9aec62f3e07947f`).

- Identity: `JMdict created: 2026-01-01`.
- XML: 61,494,891 bytes; SHA-256
  `a21b13e465060d1bedd497b5b5d4b603e8ab8130663afb3a5a5c60b4250ef2ca`.
- Entries: 214,698.
- Locally deterministic gzip: 10,260,701 bytes; SHA-256
  `92eb77d60e5b949585e41a777ff3857c412bc97ea75444d14497a5156b6264b7`.
- License: EDRDG CC BY-SA 4.0.

The gzip is a deterministic repository transport, not a claim about the bytes of
the original EDRDG gzip. The uncompressed XML hash is authoritative.

The sole source-only seq, 2611370, is explained by chronological upstream errata:
delete every sense, mark non-root, and delete `為り`. The 3,270 qualified
database-only roots are all named custom input results: five `extra.xml` rows and
3,265 municipality/ward rows. No JMdict compatibility row is needed.

## Custom and errata sources

All are pinned at upstream commit `ea9583368e67cad22d94abae8dbcc8df96d99bcd`.

| Input | Bytes | SHA-256 |
|---|---:|---|
| `data/sources/extra.xml` | 2,134 | `4a056ebe608cb7bb5284e412688ff634d3516f3249a54d37b33645d7a266093b` |
| `data/sources/jichitai.csv` | 116,379 | `328bd0779b1bb69a4c1bc3773c5ff71cffb59c825377ca7f31eafda3860e3af8` |
| `data/sources/gyoseiku.csv` | 10,266 | `23f706ff84b5c27da86be1d7a6066630d34617e1769edb0673c91d258851ce3f` |
| `dict-custom.lisp` | 13,483 | `f54a68fee54e3f850043202f272590d102a8e152817b9319c95c96942a1d7035` |
| `dict-errata.lisp` | 53,989 | `44b37171b95f7b0e40181ee5ea0edd77439871363c7dfdb0d71bf9187538cdb7` |

These are first-class semantic inputs/intended-behavior authorities, not ledger
rows. Upstream Lisp establishes the baseline meaning of the chronological edits;
it is not a normal compiler dependency.

The source compiler mechanically extracts 601 chronological declarations to
`data/source-compiler-errata.json`: 186,000 bytes, SHA-256
`7f78b244955c14e23afc5474b03c66554cfba189bf0383856afd8a00bd279f24`.
The generated ledger names its upstream authority, source line and preserved
behavior for every row.

The final compatibility ledger has 11 rows: one historical Kanjidic reading,
one January `もの` presentation correction, seven qualified noun properties on
named municipality senses, and two historical copula conjugation positions. It
is 7,952 bytes with SHA-256
`6e867889e87d43999163d3fd6fa4630a2c39253cc5d63b2484af3aad5e01c51e`.
Every row names both provenance and the exact behavior retained; none is an
export, query result set or broad allowlist.

## Kanjidic2

January 2026 Kanjidic was tested and rejected: its 39,012 readings, 8,349
okurigana and 24,821 English meanings do not match the qualified producer.

The qualified semantics match the official EDRDG 2015-03-17 capture:

- identity: file version 4, database version 2015-076, creation 2015-03-17;
- original gzip: 1,372,016 bytes; SHA-256
  `1861f294b187d491dd127a972d59dfe92117df536466562a0f2a44abf98a7d03`;
- XML: 14,652,660 bytes; SHA-256
  `d16ceffeddd0089ae2b4833d937fa34a1216805422165701e0236b7da5afa68f`;
- license at that date: EDRDG CC BY-SA 3.0.

The source and qualified producer match exactly for 13,108 character metadata
rows, 8,326 okurigana rows and 24,646 English meanings. Readings differ by one
row: normal kun reading `かわ` for `楊`. A Kanjidic-derived 2015-03-02 artifact at
Top-Ranger/harbour-kanji commit
`cad2171c6907d3e72505b45c2c14b2b2f6648f73` independently contains that reading.
Its `kanjidb.sqlite3` is 733,184 bytes with SHA-256
`cf3fbbcc155687c04937d3218912a349e35260b436755c8a065f02894c43c930`.
The original March 2 XML was not recovered, so the single provenance-backed row
is the smallest Kanjidic compatibility fact. Adding it makes the 38,977-row reading
projection byte-identical, SHA-256
`f6e98da3f4679ba6076cd3ae761e5763ec24e0246778bc5188ee3858d68d4d8f`.

Kanjidic remains a compiler-only input for easy hints. No general Kanjidic runtime
API is introduced.

## Reproduction

From repository root:

```sh
bun scripts/acquire-source-compiler-inputs.ts work/acquired-source-compiler-inputs
sha256sum work/acquired-source-compiler-inputs/JMdict_e.gz
sha256sum work/acquired-source-compiler-inputs/kanjidic2.xml.gz
```

The script clones the EDRDG archive at the exact January commit, applies patches
through 2026-01-01, validates both uncompressed and deterministic-gzip JMdict
identities, downloads the fixed Wayback Kanjidic capture, and validates its gzip
and XML identities before writing outputs.
