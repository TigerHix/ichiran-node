# GiNZA vs Ichiran Tokenization: Detailed Analysis for Grammar Matching

## Executive Summary

This analysis compares GiNZA and Ichiran tokenization on 20 JLPT grammar sentences (10 N5, 10 N3) to determine how each can contribute to an improved grammar matching system.

**Key Finding**: Neither tokenizer alone is ideal. A hybrid approach using GiNZA's structural analysis with Ichiran's semantic data offers the best path forward.

---

## Tokenization Philosophy Comparison

| Aspect | GiNZA | Ichiran |
|--------|-------|---------|
| **Design goal** | NLP/UD parsing | Learner-friendly display |
| **Granularity** | Morphological (fine) | Semantic (coarse) |
| **Consistency** | Very consistent | Context-dependent |
| **Dependency parse** | ✅ Yes | ❌ No |
| **Rich dictionary** | Basic (lemma, POS) | Extensive (gloss, readings) |
| **Compound handling** | Splits all | Groups meaningfully |

---

## Detailed Observations

### 1. Verb + Auxiliary Patterns

**GiNZA splits, Ichiran groups:**

| Sentence | GiNZA | Ichiran |
|----------|-------|---------|
| 解散します | `解散 \| し \| ます` (3) | `解散します` (1) |
| 出発する | `出発 \| する` (2) | `出発する` (1) |
| 分かりません | `分かり \| ませ \| ん` (3) | `分かりません` (1) |
| 歩きません | `歩き \| ませ \| ん` (3) | `歩きません` (1) |

**Implication**: For grammar like 〜ません, GiNZA's granularity makes pattern matching trivial (look for `ませ + ん`), while Ichiran requires parsing the compound.

### 2. Grammar Point Compounds

**Ichiran excels at grouping grammar patterns:**

| Pattern | GiNZA | Ichiran |
|---------|-------|---------|
| ことになっている | `こと \| に \| なっ \| て \| いる` (5) | `ことになっている` (1) |
| ことはない | `こと \| は \| ない` (3) | `ことはない` (1) |
| ようになった | `よう \| に \| なっ \| た` (4) | `ようになった` (1) |
| してください | `し \| て \| ください` (3) | `してください` (1) |

**Implication**: Ichiran already "detects" these N3 grammar patterns by grouping them! This is valuable metadata.

### 3. Particle Attachment

**Ichiran sometimes attaches particles:**

| Input | GiNZA | Ichiran |
|-------|-------|---------|
| いっしょに | `いっしょ \| に` | `いっしょに` |
| では | `で \| は` | `では` |
| ですが | `です \| が` | `ですが` |
| この後 | `この \| 後` | `この後` |

**Implication**: Ichiran treats these as lexical units (dictionary entries), which is semantically meaningful.

### 4. Dependency Parsing (GiNZA Only)

GiNZA provides Universal Dependencies relations:

```
図書館では、声を小さくしてください。
  図書館 → obl → し     (location)
  声    → obj → し     (object)
  小さく → advcl → し   (adverbial clause)
```

```
日本語が話せるようになった。
  日本語 → nsubj → 話せる  (subject)
  話せる → advcl → なっ    (subordinate clause)
```

**Implication**: Dependency relations enable structural rules like "find verbs whose advcl modifier contains ようになる pattern".

### 5. Token Information Quality

| Field | GiNZA | Ichiran |
|-------|-------|---------|
| Surface | ✅ | ✅ |
| Lemma | ✅ | ❌ (only via component) |
| Reading/Kana | ❌ | ✅ |
| Romaji | ❌ | ✅ |
| Gloss/Definition | ❌ | ✅ |
| POS | ✅ (UD) | ✅ (JMdict style) |
| Dependency head | ✅ | ❌ |
| Dependency relation | ✅ | ❌ |
| Compound components | ❌ | ✅ |
| Conjugation chain | ❌ | ✅ |

---

## Alignment Strategy Analysis

### Approach 1: Character-Span Alignment

Map GiNZA tokens to Ichiran tokens via character positions:

```
Text: 図書館では、声を小さくしてください。

GiNZA: [図書館][で][は][、][声][を][小さく][し][て][ください][。]
        0-3    3-4 4-5 5-6 6-7 7-8 8-11  11-12 12-13 13-17   17-18

Ichiran: [図書館][では][、][声][を][小さく][してください][。]
          0-3    3-5  5-6 6-7 7-8 8-11  11-17       17-18

Mapping:
  GiNZA[で,は] → Ichiran[では]     ✓ (合併)
  GiNZA[し,て,ください] → Ichiran[してください] ✓ (合併)
```

**Pros**: Works mechanically, preserves both analyses
**Cons**: Character positions may drift with normalization; ambiguous boundaries

### Approach 2: Token Subsequence Matching

For each Ichiran compound, find the GiNZA subsequence that covers the same span:

```python
def align(ginza_tokens, ichiran_token):
    target_text = ichiran_token.text  # "してください"
    buffer = []
    for g in ginza_tokens:
        buffer.append(g)
        if "".join(t.text for t in buffer) == target_text:
            return buffer  # [し, て, ください]
    return None
```

**Pros**: Robust to minor tokenization differences
**Cons**: Requires exact match; fails on reading normalization issues

### Approach 3: Hybrid Token Stream

Create unified tokens that have both GiNZA and Ichiran data:

```typescript
interface HybridToken {
  // From GiNZA
  ginzaTokens: GinzaToken[];  // The constituent morphemes
  dep: string;                 // Dependency relation
  head: number;                // Head token index
  
  // From Ichiran  
  text: string;               // Surface (from Ichiran grouping)
  reading: string;
  gloss: string[];
  compound?: string[];        // If this is a compound
  components?: Component[];   // Component details
}
```

**Pros**: Full data from both; enables rich pattern matching
**Cons**: Complexity; alignment errors compound

---

## Recommended Architecture

### Phase 1: GiNZA as Primary Tokenizer

Use GiNZA for:
- Consistent, predictable token stream
- Dependency parsing for structural rules
- Pattern matching against morpheme sequences

**Grammar rule example** (declarative):
```yaml
id: n3.koto-ni-natte-iru
pattern:
  - token: {lemma: "こと"}
  - token: {text: "に"}
  - token: {lemma: "なる", dep: "fixed"}
  - token: {text: "て"}
  - token: {lemma: "いる"}
```

### Phase 2: Ichiran for Enrichment

After GiNZA tokenization, query Ichiran to get:
- Readings and glosses for matched tokens
- Compound detection validation
- Alternative interpretations

### Phase 3: Rule Compiler

Compile human-readable grammar rules into optimized matchers:

```yaml
# High-level rule (human-authored)
n3.you-ni-naru:
  meaning: "come to; become able to"
  pattern: "V-dictionary + ようになる"

# Compiled pattern (auto-generated)
n3.you-ni-naru:
  sequence:
    - {pos: VERB, dep: advcl}
    - {lemma: よう, tag: 形状詞-助動詞語幹}
    - {text: に}
    - {lemma: なる}
```

### Phase 4: DP-Based Matching

Build a trie or state machine from all grammar patterns:

```
        [V]
         |
       [よう]
         |
        [に]
       /    \
   [なる]   [する]
      |        |
   [た]     [ようにする]
      |
[ようになった]
```

All N5-N1 patterns can be matched in a single pass through the token stream.

---

## What Do We Need Ichiran For?

Based on this analysis:

| Use Case | Use GiNZA | Use Ichiran | Notes |
|----------|-----------|-------------|-------|
| Tokenization | ✅ Primary | ❌ | Consistency > semantic grouping |
| Dependency parse | ✅ | ❌ | Only GiNZA has this |
| Pattern matching | ✅ | ❌ | Granular tokens easier to match |
| Readings/Glosses | ❌ | ✅ | Ichiran has richer data |
| Compound validation | ❌ | ✅ | Ichiran groups compounds |
| Presentation | ❌ | ✅ | Learner-friendly units |

**Conclusion**: Use GiNZA for structural analysis and pattern matching; use Ichiran for enrichment and presentation.

---

## Open Questions (Need More Analysis)

1. **Performance**: How much slower is running both tokenizers?
2. **Alignment accuracy**: What percentage of sentences have clean 1:1 or N:1 mappings?
3. **Edge cases**: How do both handle ambiguous sentences, slang, rare kanji?
4. **Coverage**: Does GiNZA handle all conjugation patterns Ichiran does?

---

## Next Steps

1. **Implement character-span alignment** between GiNZA and Ichiran
2. **Test on full grammar corpus** (all N5-N1 patterns)
3. **Measure alignment success rate**
4. **Design declarative rule format** that compiles to GiNZA patterns
5. **Build pattern trie** for O(n) matching complexity

