# GiNZA + Ichiran Analysis for Grammar Detection

## TL;DR

- **GiNZA**: 8x faster, provides dependency trees for structural pattern matching
- **Ichiran**: Rich dictionary (glosses, readings), learner-friendly chunking
- **Proposed**: GiNZA for grammar detection, Ichiran for enrichment

---

## Latency

| | GiNZA | Ichiran |
|---|---|---|
| Mean | **12ms** | 101ms |
| P95 | 16ms | 190ms |

GiNZA includes full dependency parse. Ichiran is tokenization only.

---

## What GiNZA Gives Us

Dependency tree for each sentence:

```
Sentence: 素人からみるとかなりうまい人でも、プロの世界では全然通用しないらしい。

Token      Dep        Head
素人        obl        みる       ← 素人 is oblique argument of みる
から        case       素人       ← から marks 素人
みる        advcl      通用       ← みると clause modifies main verb
と          mark       みる       ← と marks conditional
```

This lets us write grammar rules like:

```
からみると:
  - case(X, から) where X is noun
  - advcl(みる, main_verb)
  - mark(みる, と)
```

Instead of just "contains からみると".

---

## What Ichiran Gives Us

- **Glosses**: "to eat" for 食べる
- **Readings**: たべる for 食べる
- **Compound structure**: ことになっている as single semantic unit
- **Conjugation info**: 食べた = past tense of 食べる

GiNZA has lemmas but not dictionary definitions.

---

## Proposed Architecture

```
Input
  │
  ▼
GiNZA (12ms)
  │ - tokenization
  │ - dependency tree
  │ - POS tags
  ▼
Grammar Matcher
  │ - match structural patterns
  │ - use deps for disambiguation
  ▼
Ichiran (lazy, only matched spans)
  │ - enrich with glosses
  │ - enrich with readings
  ▼
Output
```

---

## Open Questions

1. **Alignment**: How to map GiNZA tokens to Ichiran segments?
   - Character span based? (GiNZA oversegments)
   
2. **Pattern language**: How to express grammar rules as dependency patterns?
   - DSL? JSON schema? Direct code?

3. **Coverage**: Does GiNZA handle all grammatical constructs we need?
   - Need to test on edge cases

---

## Files

- `scripts/dependency-pattern-demo.py` - demo of dependency-based matching
- `scripts/grammar-classification-eval.py` - latency benchmarks
- `scripts/compare-tokenizers.py` - token-level comparison
