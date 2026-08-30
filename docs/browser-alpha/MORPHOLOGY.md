# Browser alpha reverse morphology

The design remains current. Counts and digests below are measurements of the previous
`d583` alpha pack; the refreshed `ichiran-260118` release must publish new values in
its lock and `stats.json`.

The alpha replaces PostgreSQL's generated conjugation closure with a small
reverse matcher. Grammar is not part of this component.

## Representation

The compiler reads the frozen PostgreSQL snapshot and the pinned conjugation
CSV rules. It emits one deterministic `ICHIMOR1` section containing:

- canonical rule records and direct or two-stage reverse suffix templates;
- an open-addressed index from `(route, POS, reconstructed source text)` to
  canonical root/source facts;
- route-independent root-form lists used by the materializer's suppression
  checks;
- 38 intentional `じゃ` / `ございます` compatibility records;
- three exact tombstones: the two installed negative exceptions and one
  post-secondary-closure artifact.

`MorphologyReader.lookup(surface, route)` reverses only suffixes that occur in
the installed root/rule cross-product, confirms each match by applying the
forward rule(s), and returns the canonical root plus the ordered property path.
`ruleIds` are stable within the pinned section. Section 4 maps them to compact
semantic aliases used by section 5's generated physical-member overlay, without
exposing generated PostgreSQL IDs.

Candidate ordering and compiler ordering use JavaScript code-unit comparison,
not `localeCompare`, so output is independent of the browser locale.

## Route and suppression parity

The analyzer route is the same all-kana classifier used by core. The compiler
filters four malformed, classifier-inactive `kana_text` root spellings; there
are no all-kana `kanji_text` roots. Those four rows have zero active direct or
secondary lookup relations.

Root-form suppression is intentionally route-independent. The PostgreSQL
materializer checks the union returned by `getAllReadings()`, so making this
check route-specific would admit generated candidates that the installed
closure suppresses. The exhaustive relation comparison below has no alpha-only
candidate.

Snapshot route accounting is:

| Measure | Rows |
|---|---:|
| `conj_source_reading` | 8,270,561 |
| physical target-table route matches | 8,270,527 |
| classifier-active route matches | 8,269,155 |
| installed but classifier-inactive route matches | 1,372 |
| missing from both physical target tables | 55 |
| present in both target tables | 21 |

The 8,270,527 figure is a sum across physical kana/kanji routes. It is not the
runtime-active count. The portable analyzer exposes only the classifier-active
route; the normalized surface index supplies the reviewed 55 replacements for
stale/missing generated forms.

## Previous measured artifact

Two complete database builds produced byte-identical files.

| Measure | Result |
|---|---:|
| Raw bytes | 2,664,344 |
| gzip-9 bytes | 1,132,372 |
| POS / rules | 22 / 1,161 |
| Direct / secondary templates | 2,211 / 5,568 |
| Distinct suffixes | 2,568 |
| Root rows / keys / groups | 41,383 / 40,321 / 14,491 |
| Manual patches / tombstones | 38 / 3 |
| Build wall / peak RSS | 18.8 s / 323,088 KiB |
| SHA-256 | `186e24e74168d838bebbfab2d6c5061da79f2906ff66f98f65f120b845852757` |

On desktop Bun, strict open/validation took about 13.5 ms. A 200,000-lookup
ambiguous hit sample averaged 20.8 microseconds per lookup; a common two-stage
hit averaged 11.1 microseconds over 100,000 lookups. These are engineering
measurements, not mobile latency claims.

## Exhaustive relation result

The verifier streams the lineage-valid, classifier-active PostgreSQL relation,
expands every property path, and compares exact semantic keys. The final run
covered 9,045,688 expanded relation rows in 7,849,201 surface/route groups:

| Measure | Result |
|---|---:|
| Legacy semantic keys | 9,045,688 |
| Alpha semantic keys | 8,653,352 |
| Legacy-only | 392,336 |
| Alpha-only | **0** |
| Duplicate legacy / alpha keys | **0 / 0** |

The exact 392,336-row JSONL is a frozen relation-difference attestation. It is
deliberately not checked in: it is 91,164,882 bytes. Its SHA-256 is
`e97c7eeea1e54145b8a8dc406c079d787049739a20aefb4d4229d2fe65b467e0`.
The rows classify as:

| Reviewed legacy artifact | Rows |
|---|---:|
| Potential/passive property cross-product, direct | 43,596 |
| Potential/passive property cross-product, secondary | 348,736 |
| Other merged-link property cross-products (`せんといて`, `せんとき`, `とあって`, `とあり`) | 4 |

No broad runtime predicate accepts these rows. The exhaustive tool emits their
exact semantic keys. The release build hashes those canonical JSONL lines while
streaming this complete relation inside the same repeatable-read transaction that
compiled the morphology section. It refuses any changed count or digest, any
alpha-only candidate, or any duplicate on either side before publishing. This is
not an end-to-end result allowlist: the strict analyzer oracle still permits zero
normalized output differences.

## Reproduction

Run from `packages/data`:

```bash
export ICHIRAN_DB_URL='postgresql:///ichiran_oracle_ea958336?host=%2Fvar%2Frun%2Fpostgresql'

bun src/browser-pack/morphology-build.ts \
  --data ../../data \
  --out /tmp/ichiran-morphology.bin

bun src/browser-pack/morphology-verify.ts \
  --artifact /tmp/ichiran-morphology.bin \
  --diff /tmp/ichiran-morphology-diff.jsonl \
  > /tmp/ichiran-morphology-verify.json
```

The standalone verifier loads `browser-alpha/sources.lock.json` by default and exits
non-zero unless the artifact digest, measured relation totals, and streamed JSONL
digest match it exactly. `--lock <path>` selects another reviewed lock. The release build performs
the same measurement directly on the exact section it is about to package; writing
the 91 MB JSONL is optional proof output and is not required during release.

The morphology candidate intentionally carries canonical `rootSeq`, not a
generated entry ID. Its semantic property path is sufficient for the ordinary
single-member case. For exceptional targets, the analyzer annotation reader adds
ordered physical members: every physical conjugation, every `conj_prop` row,
exact tri-state properties, count exceptions, shared-target identity, and exact secondary
`viaMemberOrd` binding. These facts use lazy 10-byte records keyed by root plus the
one- or two-rule semantic alias path.

The separate resident collision table is narrower: it preserves scorer state when
a generated target also reuses a real lexical entry. It does not replace the general
physical-member overlay. Both mechanisms remain analyzer-private; the clean result
continues to expose canonical root identity and semantic inflections.
