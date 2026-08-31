# Portable analyzer scoring and path kernel

Status: implemented and integrated in the locked `ichiran-260118` analyzer
Scope: scoring, analyzer-internal pair rules, culling, and top-N path selection

The scoring kernel has no separately emitted dictionary section. Artifact sizes and
release-level measurements come from the generated `dist/browser-alpha/stats.json`,
checked against `browser-alpha/sources.lock.json`; this document does not attribute a
speculative share of the JavaScript shell or packed data to the scorer.

## What this implementation is

`packages/core/src/analyzer-scoring.ts` is a synchronous, DB-free translation of
the current analyzer's `calcScore`, identical-candidate cull, lookup score cutoff, and
presentation alternative cutoff. `packages/core/src/analyzer-rules.ts` contains the
analyzer's registered segmentation filters, penalties, and pair synergies.
`packages/core/src/analyzer-paths.ts` owns gap penalties, entity boosts, stable top-N
insertion, and path accumulation. These modules do not import PostgreSQL, Node APIs, the
separate experimental `@ichiran/grammar` package, surface lookup, morphology generation,
suffix/counter generation, details, or presentation code.

This is intentionally not a SQL-shaped compatibility layer. Lookup resolves a small
`AnalyzerScoreCandidate`, the scorer consumes it once, and the path kernel works on
candidate IDs and scored span groups. Packed readers remain responsible for packed
storage; the hot loop does not expand the whole dictionary into objects.

The arithmetic remains the accepted parity algorithm, but the default path search is
no longer the original quadratic scan over every non-adjacent predecessor. It uses an
exact sweep whose non-adjacent result can only come from the union of three size-N
affine frontiers: ordinary left score, short-word-penalized left score, and score with
the left group removed. Adjacent predecessors still run the complete pair-rule
resolver because filters, replacements, and synergy adjustments apply there. Passing
a custom initial or transition resolver deliberately selects the exhaustive dynamic
program so extension hooks cannot silently inherit assumptions proved only for the
built-in rules. Persistent backpointers defer path materialization until the final
top-N results.

This is not the optimization ceiling. Remaining measured opportunities include the
adjacent rule resolver, candidate/allocation churn, packed-reader locality, and focused
WASM kernels where boundary costs justify them. The public candidate/result types keep
those changes possible without coupling scoring to a database model.

## Concrete runtime boundary

A word candidate supplies:

- scoring text, source text, route, current sequence, ordinal, common rank, and
  `nokanji`;
- current-entry `root`, `nKanji`, and `primaryNokanji` facts;
- the selected conjugation lineage with `(seq, from, via, pos, type, negative, formal)`;
- non-archived POS union over the current sequence plus selected roots;
- archived/prefer-kana aggregates for both `[currentSeq]` and
  `[currentSeq, ...from]` (legacy scoring chooses between them based on root/use-length);
- pre-resolved original-form common/ordinal inheritance;
- a pre-resolved score split, if registered;
- a materialized `:suru` suffix candidate only when kanji-break scoring can ask for it.

A compound supplies a base candidate, its complete text, flattened score modifier, and
final-component conjugations. Numeric suffix modifiers and suffix score functions are
not interchangeable: the former scale by property score and added morae; every score
function in the current suffix DSL resolves to a one-time constant. The portable form
stores both `multiplier` and `constant`, so no JavaScript function enters the runtime
artifact.

The scorer returns the integer score plus the exact data needed by analyzer filters and
presentation: POS, lineage sequence set, conjugations, inherited common rank, property
score/use-length/split breakdown, and four packed flags for strong, primary, common, and
long.

## Exact legacy behavior deliberately retained

- Score cutoff is `5`; identical candidates retain scores at least half the best score.
- Presentation retains alternatives at least two-thirds of the winning candidate.
- Stable common-rank ordering runs before stable descending-score ordering.
- `0` common rank follows the current comparator semantics; it was not "fixed" based on
  comments in the old source.
- Mixed primary/secondary conjugation lists discard `via` rows exactly as current code
  does; an all-`via` list is the secondary-conjugation case.
- Final, semi-final, weak-form, skip-word, copula, archived, prefer-kana, particle,
  pronoun, counter, split, and kanji-break branches retain their current truthiness and
  rounding behavior.
- Proportional splits update property score but leave split-info null, matching current
  output.
- Compound scoring does not forward `final` to its score base, matching current code.
- Gap penalty is `-500` per UTF-16 input position.
- Top-N ties retain registration order. The all-gap path is registered first.
- A dictionary candidate on an entity span receives the entity boost once. A synthetic
  entity candidate is created with that boost as its score and receives it again in path
  scoring. This double application is current observable behavior and is retained.

## Pairwise analyzer rules

The core default includes all registered analyzer segmentation filters and penalties,
plus the registered pair synergies. These originate in
`packages/reference-postgres/src/grammar`; they are part of the analyzer parity
contract and are distinct from the separate experimental `@ichiran/grammar` package.

Lookup populates the compact `AnalyzerSegment.rules` facts: exact candidate text,
simple/proxy/compound/counter kind, score info, and (for compounds) the final simple
component's sequence and text. A proxy final component deliberately yields no sequence,
matching the current compound-sequence helper.

The path API retains two narrow optional resolver overrides for differential testing and
future experiments. Initial filtering maps one span group to zero or more filtered
groups; a pair transition returns `{right, adjustment?, left}` records in observable
order. Production callers need not provide them. The DP—not the rule resolver—owns score
accumulation, replacement of the prior path head, gaps, top-N truncation, and final
reversal.

## Generated-entry parity boundary

The PostgreSQL scorer uses generated physical identity internally even though the clean
browser API exposes canonical root identity. It cannot be replaced blindly with root
facts. In `packages/reference-postgres/src/dict/scoring.ts`, current/generated identity affects:

- entry lookup (`root_p`, `n_kanji`, and `primary_nokanji`);
- the `[seq, ...from]` lineage and the root-only `[seq]` aggregate selection;
- all-archived, prefer-kana, prefer-kana-on-sense-zero, and non-archived POS queries;
- `SKIP_WORDS`, `FINAL_PRT`, `SEMI_FINAL_PRT`, `NON_FINAL_PRT`, `COPULAE`, and
  `NO_KANJI_BREAK_PENALTY` membership;
- split-definition dispatch and analyzer pair-rule predicates that inspect `seqSet`;
- original-text inheritance and the legacy diagnostic score info.

The integrated adapter resolves that dependency with two deliberately small mechanisms:

1. The resident lexical-target collision table supplies exact entry aggregates and
   scorer memberships when a generated target is also a real lexical entry.
2. The lazy generated physical-member overlay supplies count differences, shared-target
   identity, every physical conjugation member, every ordered `conj_prop` property, and
   exact two-stage via-member binding for the broader exceptional set.

The second mechanism uses repeated 10-byte records for one semantic key. Treating it as
a scalar map would drop valid members. Ordinary one-member/default-property paths need
no record and are derived from canonical root plus reverse morphology. The adapter uses
pack-local groups and ordinals to reproduce physical selection and ordering, while the
clean result keeps only root identity and semantic inflections.

The analyzer integration now owns compound and suffix construction, optional `:suru`
breaks, counters, registered split/segsplit/hint results, truncation behavior, and rule
fact population. These are implementation details behind `PortableAnalyzer`; they are
not additional public subsystems.

## Verification

Normal core tests include frozen arithmetic fixtures and differential tests against
the frozen PostgreSQL reference for common ordering, culling, gap penalty, and
neutral-transition path DP. They also lock top-N ties, filtered replacement
transitions, adjustments, and entity
double boost. Analyzer-rule tests compare every registered synergy, representative
filter/penalty/non-adjacent cases, and 250 deterministic mixed-feature pairs against the
frozen reference implementation.

An opt-in PostgreSQL differential materializes real scoring facts for deterministic
root and generated forms, then compares ordinary, final, and extended-use-length
modes against the compiler-only reference `calcScore`:

```bash
cd packages/core
RUN_ANALYZER_SCORING_POSTGRES=true \
  bun test tests/analyzer-scoring-postgres.test.ts
```

The core suite also covers end-to-end direct/morphology lookup, suffixes, counters,
numbers, splits, generated multi-property expansion, two-stage via-member binding,
legacy compact/detailed serialization, and corruption/lazy-load behavior. Release
qualification compares the portable analyzer with the locked current-Lisp fixtures
where those exist and uses the frozen PostgreSQL implementation for the remaining
fallback probes. The gate requires zero chosen-authority differences and an empty
result allowlist; parity totals belong to that release run, not to a stale component
snapshot.

Production Chromium performance remains the browser release gate. Do not substitute Bun
microbenchmarks for the public Worker RPC benchmark. No physical-iPhone test has been
performed for this release, so desktop browser results must not be presented as phone
measurements.
