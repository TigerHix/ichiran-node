# M6 canonical roots and direct surfaces

## What the compiler now owns

`compileCanonicalRoots()` builds the complete dictionary model from the pinned
January JMdict, the three custom source files, the chronological errata ledger,
and the eighteen-row compatibility ledger. It then derives best readings. None of
those steps opens a database or reads a PostgreSQL export.

The chronological order is fixed: 214,698 JMdict declarations, 3,703 custom
mutations, 601 declared errata rows, the compatibility rows, and finally best
reading derivation. Errata removes the one obsolete JMdict root, seq 2611370.
The finished model has 217,967 roots.

## Qualified baseline comparison

`scripts/source-compiler-root-proof.ts` compares the finished semantic model to
the immutable `ichiran-260118` PostgreSQL oracle. PostgreSQL is used only by this
migration proof.

| Projection | Result |
|---|---:|
| Canonical detail records | 217,967 / 217,967 exact |
| Detail store | 13,555,874 bytes, byte equal |
| Detail SHA-256 | `0fc45731d84fbb7c2ccf3ef5692d2f1ab01e538325f0ed50135da38e621aa151` |
| Root metadata records | exact |
| Direct form semantic sets | exact |
| Restriction rows | 6,332 / 6,332 exact |
| Direct surface classes | 443,275 |
| Source-order delta classes | 3,435 |
| Forms in those classes | 14,387 |
| Changed first candidates | 3,148 |

The root section remains 9,088,056 bytes. Its source-native SHA-256 is
`19204bdae9ec44f7a5240aa7b74e83cf302a8f8da09b4a1748445ef0dd5dc8d2`;
the qualified physical-order SHA-256 is
`2bd83550fc67ae90dcaed1db37dc0b596091ea49081c00f34f2325f846b9aafa`.
This is the approved direct-order rebaseline, not a semantic-content delta.

The proof writes every changed class with both complete orders to migration
evidence. The current evidence digest is
`5f4660a0afbc1a21021f3c4db49014554a3b7991a48960c2238a050ae05a1854`.
That evidence is deliberately not read by the compiler and is not a broad
compatibility allowlist.

## Reproduction

From the repository root, with the qualified oracle available only for the
comparison:

```sh
mkdir -p work/m6-evidence
bun scripts/source-compiler-root-proof.ts \
  ichiran_oracle_ea958336 \
  work/m6-evidence/direct-order.jsonl
```

The normal source compiler and its tests do not need or inspect that database.
