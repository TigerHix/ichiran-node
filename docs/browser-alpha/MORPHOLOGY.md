# Browser alpha reverse morphology

The design remains current. Counts and digests below are locked to upstream Ichiran
`ea9583368e67cad22d94abae8dbcc8df96d99bcd` and data release `ichiran-260118`.
The generated release's `dist/browser-alpha/stats.json`, checked against
`browser-alpha/sources.lock.json`, is the source of truth for measurements.

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
- 50 locked manual compatibility records; and
- four exact tombstones for reverse-rule candidates excluded by the installed
  closure.

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

Locked `ichiran-260118` route accounting is:

| Measure | Rows |
|---|---:|
| `conj_source_reading` | 8,386,607 |
| installed physical target-route matches | 8,386,641 |
| classifier-active route matches | 8,385,507 |
| installed but classifier-inactive route matches | 1,134 |
| not installed in either target route | 1 |
| additional matches from dual-route rows | 35 |

The 8,386,641 figure is a sum across physical kana/kanji routes, so dual-route
rows contribute twice. It is not the runtime-active count. The portable analyzer
exposes only the 8,385,507 classifier-active route matches.

## Locked `ichiran-260118` artifact

These are generated-release values, not standalone timing or lookup benchmarks:

| Measure | Result |
|---|---:|
| Raw section bytes | 2,688,176 (2.56 MiB) |
| POS / rules | 22 / 1,161 |
| Reverse templates | 7,211 |
| Distinct suffixes | 2,568 |
| Root keys / groups | 40,882 / 14,608 |
| Manual patches / tombstones | 50 / 4 |
| SHA-256 | `1614d150f3609b9de4f93de5ad0e33e12aec41211dc9096870a8d019eab9c0f3` |

## Exhaustive relation result

The verifier streams the lineage-valid, classifier-active PostgreSQL relation,
expands every property path, and compares exact semantic keys. The locked run
covered 9,173,122 legacy relation keys in 7,959,271 surface/route groups;
7,571,395 groups were exact:

| Measure | Result |
|---|---:|
| Legacy semantic keys | 9,173,122 |
| Alpha semantic keys | 8,774,911 |
| Legacy-only | 398,211 |
| Alpha-only | **0** |
| Duplicate legacy / alpha keys | **0 / 0** |

The exact 398,211-row legacy-only set is a frozen relation-difference attestation
with SHA-256
`6eb1cbae46eb2df0c67570764d0cf408bf4d7b8873eb4b53ce694cd17549d421`.
No broad runtime predicate accepts these rows. The exhaustive tool emits their
canonical semantic keys. The release build hashes those canonical JSONL lines while
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
the JSONL is optional proof output and is not required during release.

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
