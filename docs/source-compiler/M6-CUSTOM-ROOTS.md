# M6 custom roots and custom detail edits

This slice compiles the three pinned Ichiran custom sources directly into the
canonical TypeScript model. PostgreSQL is not an input. The qualified database
was used only once, read-only, to verify the resulting identities and semantic
rows.

## Inputs and provenance

All three files are repository inputs pinned with upstream Ichiran commit
`ea9583368e67cad22d94abae8dbcc8df96d99bcd`. Their authoritative byte identities
are:

| Source ID | File | SHA-256 |
|---|---|---|
| `ichiran-extra-260118` | `data/sources/extra.xml` | `4a056ebe608cb7bb5284e412688ff634d3516f3249a54d37b33645d7a266093b` |
| `ichiran-jichitai-260118` | `data/sources/jichitai.csv` | `328bd0779b1bb69a4c1bc3773c5ff71cffb59c825377ca7f31eafda3860e3af8` |
| `ichiran-gyoseiku-260118` | `data/sources/gyoseiku.csv` | `23f706ff84b5c27da86be1d7a6066630d34617e1769edb0673c91d258851ce3f` |

`custom-sources.ts` parses only these formats. It does not expose a generic
loader, repository, query API, or database-shaped model. Geographic romanization
uses the canonical core Hepburn implementation; the source reader supplies the
small Common Lisp `string-capitalize` behavior needed after apostrophes.

## Explicit chronological rules

The custom phase begins after the 214,698 January JMdict source events.

1. Create the five non-duplicate `extra.xml` roots in file order. The sixth row,
   `せず`, already names an existing canonical root and is skipped.
2. Expand the municipality CSV into full and short proposals, then stable-sort
   them by the upstream semantic type order `都道府県市区町村`.
3. Apply ward proposals in file order beneath their preceding city header.
4. For each proposal, match ordinal-zero spelling and reading against the current
   canonical roots, with candidates ordered by sequence number.
5. A matching definition is skipped. A reviewed old city/prefecture gloss is
   replaced. Any other existing candidate receives a new sense. Otherwise a new
   root is created.

Only an actual mutation consumes an event number. Every event records source ID,
source ordinal, mutation kind, target sequence, and—where applicable—the old and
new gloss. Forms and new POS properties carry the same event in `SourceOrder`.
This is the single deterministic ordering input for later direct-form and detail
pack compilation.

The non-numeric `extra.xml` identities and the first geographic allocation are
small, explicit baseline identities:

| Source entry | Qualified sequence |
|---|---:|
| `お掛け` | 12,294,525 |
| `甲斐もない` | 12,294,526 |
| `観了` | 12,294,576 |
| `たそう` | 900,000 |
| `もいい` | 900,001 |
| First municipality/ward creation | 12,294,577 |

The 3,265 geographic creations then occupy the contiguous range 12,294,577
through 12,297,841. No per-place identity ledger is stored.

## Results

The pinned January JMdict plus the three custom sources produce:

| Result | Count |
|---|---:|
| Geographic proposals | 3,750 |
| Extra root creations | 5 |
| Municipality root creations | 3,157 |
| Ward root creations | 108 |
| Existing-root sense additions | 358 |
| Existing-root gloss replacements | 75 |
| Proposals already represented | 52 |
| Existing roots changed | 220 |
| Total chronological mutations | 3,703 |

Evidence digests:

| Evidence | SHA-256 |
|---|---|
| Sorted 3,270-root identity list | `e992430575166c17f7ba8e8bc82386f271005ecf9203a9ed2a851ab2d160695a` |
| Final canonical custom roots | `628ca94479064b50b13f1ecdac6294d0707eb426da1e16619126c84865915923` |
| Final 220 existing-root replacements | `fd9bb40e39e48e7588ddf542abe6681cf86682f7e8ee7eaf14f6ae6356e01572` |
| Ordered edit log | `000c103b84daef02b85b00128acd149c4ef42e8dab7fede85f0eb86b4841e304` |

Read-only comparison with `ichiran_oracle_ea958336` found all 3,270 root
identities, exact equality for all custom-owned columns of the 6,499 direct form
rows, exact equality for all 3,406 new-root sense rows, and all 433 custom
definitions on their intended updated roots. The later `best` links are excluded
from the form comparison because best-reading derivation is a separate phase.

## Boundary of this slice

This implements the chronological **custom-data load phase only**. It implements
no action from `dict-errata.lisp` or `packages/data/src/data/errata.ts`.
Specifically still missing from the full M6 compiler are chronological errata for
root demotion (including seq 2611370), entry/form additions and deletions, form
reordering, sense/gloss/property edits, restriction edits, conjugation and source-
reading edits, and the later suffix/counter/split facts. The M2 seq-2611370 witness
documents that behavior but does not apply it here. Best-reading derivation is
also a later phase and is intentionally absent from this custom compiler.
