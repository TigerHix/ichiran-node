# M6 source-native release workflow

The release command has two concrete modes. Both compile the same TypeScript
semantic model, stream generated conjugations through bounded binary spools,
invoke the existing Rust surface-index compiler, and write pack format v1.
Neither mode imports the PostgreSQL reference package or configures a database.

## Qualified January baseline

The baseline mode verifies every file in
`data/source-compiler-sources.lock.json`, then compares section bytes and counts
against `work/m2-baseline`. Root payload ordering has its independent review in
`data/source-compiler-direct-order-attestation.json`. Analyzer support and
annotations may differ only when their complete source and qualified section
identities and count groups exactly equal the generated-order release gate.
Every other difference is a hard failure. The baseline manifest, compressed
assets, and `stats.json` must
also match the pinned SHA-256 checksum index from the immutable qualified
release; a self-consistent local replacement cannot become a new baseline.

Baseline mode also reads and validates
`data/source-compiler-generated-order-attestation.json`. That compact input
requires the exhaustive generated candidate universe to close with zero
source-only, qualified-only, reverse-only, rank or collision gaps, and records
the reviewed source-native scheduling deltas. Its `releaseGate` is atomic: it is
either `null`, which always fails, or it pins both analyzer sections on both
sides together with every artifact count. A mismatch in one byte identity or
one count hard-fails; there is no section-name allowlist or update-mode bypass.

The completed development probe failed before publication as designed and
printed the complete candidate. It took 7:28.92, peaked at 22,020,276 KiB RSS,
and used no swaps. The stderr and timing evidence have SHA-256 identities
`1e2c0db376f69c39093653ba2aac118b8fd0d53708c69c9cb0eb9224adad13b3`
and `b1598a2970ca1b05fbc13499be30f4a9050a2195afb6ef0279876683b5a19bc9`.

The final analyzer-support sections are both 949,424 bytes. The source SHA-256
is `f600a57d489a4745184f6cc620a808d7d622e6078e778dbed50f145590a574bb`;
the qualified SHA-256 is
`24632918fa8b5116b983946281107e53ad6e8ac728b517121e6aa9c4955a14f0`.
Every support count is exact:

| Count | Source | Qualified | Delta |
| --- | ---: | ---: | ---: |
| suffixKeys | 5,532 | 5,532 | 0 |
| suffixValues | 5,533 | 5,533 | 0 |
| suffixClasses | 3,586 | 3,586 | 0 |
| counterKeys | 760 | 760 | 0 |
| counterVariants | 799 | 799 | 0 |
| collisions | 5,442 | 5,442 | 0 |
| generatedRules | 1,161 | 1,161 | 0 |
| generatedAliases | 1,030 | 1,030 | 0 |

The source analyzer-annotations section is 3,421,680 bytes with SHA-256
`6b4078d0ae47c0081cfc8db6e9c7f0f10c7c933e8e9ec5158cabe85f5983444e`.
The qualified section is 3,531,024 bytes with SHA-256
`2ba1615e1a08dbfe458dd8a4ca89201e25aed58844531769dd7bbc0ac26de592`.
The gate pins the complete count pair, not only the nonzero deltas:

| Count | Source | Qualified | Delta |
| --- | ---: | ---: | ---: |
| blocks | 842 | 842 | 0 |
| splits | 38,032 | 38,032 | 0 |
| hints | 36,885 | 36,885 | 0 |
| generatedBlocks | 36 | 37 | -1 |
| generatedRoots | 20,347 | 20,347 | 0 |
| generatedRecords | 764,243 | 764,828 | -585 |
| lookupOrderRecords | 341,506 | 340,437 | 1,069 |
| lookupOrderRoots | 9,630 | 9,635 | -5 |
| lookupOrderBytes | 1,366,024 | 1,361,748 | 4,276 |
| lookupOrderExceptionSurfaces | 0 | 1,623 | -1,623 |
| lookupOrderExceptionClasses | 0 | 3,895 | -3,895 |
| lookupOrderExceptionLocators | 0 | 4,212 | -4,212 |
| lookupOrderExceptionBytes | 0 | 79,908 | -79,908 |
| generatedPhysicalGroups | 169,649 | 170,717 | -1,068 |
| generatedFactPairs | 81 | 80 | 1 |
| indexBytes | 184,200 | 264,128 | -79,928 |
| uncompressedBytes | 23,220,023 | 23,255,989 | -35,966 |
| compressedBytes | 3,237,473 | 3,266,889 | -29,416 |
| annotationUncompressedBytes | 13,804,197 | 13,838,577 | -34,380 |
| annotationCompressedBytes | 811,988 | 816,578 | -4,590 |
| generatedUncompressedBytes | 9,415,826 | 9,417,412 | -1,586 |
| generatedCompressedBytes | 2,425,485 | 2,450,311 | -24,826 |
| totalBytes | 3,421,680 | 3,531,024 | -109,344 |
| largestUncompressedBlock | 326,926 | 326,926 | 0 |
| largestGeneratedBlock | 262,130 | 262,144 | -14 |
| largestGeneratedCompressedBlock | 77,596 | 78,022 | -426 |

```sh
bun scripts/source-compiler-release.ts baseline \
  --out work/m6-source-release \
  --pack-version ichiran-260118-source
```

The command normally requires a clean checkout. `--allow-dirty` exists only for
development runs whose manifest commit cannot describe all local edits. The
command records the full 40-character commit, checks the checkout before work,
and checks the same commit and clean state again immediately before atomically
activating the finished generation.

## PostgreSQL-unavailable proof

The focused import test rejects any attempt to resolve the PostgreSQL oracle
package or the PostgreSQL client from the source release module graph:

```sh
bun test packages/data/tests/source-compiler-release-evidence.test.ts \
  --test-name-pattern 'reference modules are blocked'
```

For the milestone gate, run the complete baseline in a Linux user, mount, and
network namespace. The temporary mount hides the host's PostgreSQL Unix socket;
the isolated network namespace makes TCP PostgreSQL unreachable. Both probes
must fail before the compiler starts:

```sh
unshare --user --map-root-user --mount --net sh -eu -c '
  mount -t tmpfs -o mode=700 tmpfs /var/run/postgresql
  ! pg_isready -q
  ! pg_isready -q -h 127.0.0.1 -p 5432
  exec env -u DATABASE_URL -u ICHIRAN_DB_URL \
    bun scripts/source-compiler-release.ts baseline \
      --out work/m6-source-release-no-postgres \
      --pack-version ichiran-260118-source
'
```

This is the final availability proof, not merely an invalid connection string:
neither a local socket nor a network route exists inside the build namespace.
The command still requires all non-PostgreSQL build dependencies to have been
installed before entering the network namespace.

## Genuine 2026-09-01 JMdict update

The transition changes only JMdict. The lock keeps Kanjidic2, custom sources,
chronological errata, and the reviewed compatibility ledger identical to the
qualified baseline. It pins the official gzip and uncompressed XML identities,
the `JMdict created: 2026-09-01` header, the EDRDG URL and license, and the
Jitendex archive commit containing the final 2026-08-31 patch.

Acquire the official snapshot while the authoritative URL has the pinned bytes:

```sh
bun scripts/acquire-source-compiler-update-2026-09-01.ts
```

The acquisition command verifies compressed and uncompressed SHA-256 digests
and the creation header before exclusively creating the ignored file
`work/m6-transition/JMdict_e-2026-09-01.gz`. If the URL has advanced, it fails
instead of relabeling a newer dictionary. The uncompressed identity can also be
reconstructed from Jitendex archive commit
`8c5c132d1f587aa02786a401be8c58f0b0b4185f` through patch
`JMdict_e/patches/2026/08/31.patch.br`.

Build the updated release through the same compiler:

```sh
bun scripts/source-compiler-release.ts update \
  --source-lock data/source-compiler-update-2026-09-01.lock.json \
  --jmdict work/m6-transition/JMdict_e-2026-09-01.gz \
  --jmdict-source-id edrdg-jmdict-e-2026-09-01 \
  --out work/m6-update-release \
  --pack-version jmdict-2026-09-01-source
```

Update mode deliberately does not compare its changed dictionary bytes to the
January artifact. It still verifies all pinned inputs, rebuilds every output
twice where representation is encoded, enforces release size limits, verifies
the staged pack, and records every section digest and count in `stats.json`.
The CLI requires an explicit update lock and rejects the baseline JMdict path or
identity in this mode, so `update` cannot be used merely to turn off the January
comparison.
