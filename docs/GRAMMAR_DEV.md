## Grammar Pattern Development Guide

### 1) Define the target
- Identify the core construction, intended scope, and level (N5–N1).
- Collect 5–15 positive examples and 5–10 negative examples (common confusions).

### 2) Choose a structural backbone
- Prefer macros to encode structure and tokenizer variance:
  - `NP`, `NPCase`, `NPTopic`
  - `PrePredicateAdjuncts` (adverbs, NP+case/topic, simple `exp`)
  - `IAdjKu`, `IAdjKute`, `IAdjKunai`, `IAdjKunakute`
  - `NaAdjDe`
- Compose with pattern nodes:
  - `sequence`, `alt`, `optional`, `repeat`, `capture`, `token`
- Tips:
  - Model fused and split tokenizations using `alt`.
  - For optional contrastive は: `optional` + `token: ["text:は"]`.

### 3) Constrain atomic pieces with predicates
- Common:
  - `text:…`, `lemma:…`, `lemmaNeg:…`, `pos:…`, `kana:/…/`
  - `hasVerbalConjugation` (replacement for `isVerb`): POS is verb OR has conjugation data
  - `isIAdjective`, `isNaAdjective`, `isAuxiliary`, `isTopicMarker`, `isCaseParticle`, `isPredicate`
- Lexically sensitive helper:
  - `prefixHasPos:suffix:pos1,pos2` (e.g., `prefixHasPos:んです:v,adj-i,adj-na,aux`)
- Guardrails:
  - Use `not` to block false-friends (e.g., exclude `ので` for sentence‑final の).
  - Use `peek`/`anchor` for lookahead or boundaries when needed.

### 4) Handle tokenizer variance intentionally
- Provide `alt` branches for fused vs split forms (e.g., くない vs く + ない).
- Ordering matters: `alt` now deduplicates identical outcomes and prefers higher `preference`. When equal, earlier branches win—place more specific/single‑token branches first.

### 5) Add captures sparingly
- Capture only spans used in UX/explanations.
- Use clear labels (e.g., `"capture": "predicate"`, `"subject"`, `"adjective"`).

### 6) Author the JSON
- Location: `src/grammarMatcher/grammars/<level>/`.
- Include: `id`, `level`, `priority`, `formation`, `pattern`, `explanation`, `examples`, `negativeExamples`.
- Skeleton:
```json
{
  "id": "n5.example-pattern",
  "level": "N5",
  "formation": "Template the learner sees",
  "priority": 6,
  "pattern": {
    "sequence": [
      { "macro": "PrePredicateAdjuncts" },
      { "alt": [
        { "macro": "IAdjKu" },
        { "sequence": [
          { "macro": "NP" },
          { "token": ["isCaseParticle"] }
        ]}
      ]},
      { "token": ["hasVerbalConjugation"] }
    ]
  },
  "explanation": "What it means, how it’s used, key constraints.",
  "examples": [{ "jp": "よく勉強します。", "en": "I study well." }],
  "negativeExamples": [{ "jp": "よくて行きます。", "en": "This links clauses; not adverbial modification." }]
}
```

### 7) Test and iterate
- Build: `bun run build`
- Tests: `bun test examples -t <grammarId>` or `bun test examples`
- Debug a sentence (see `DEBUG.md`):
  - Inspect tokens
  - Probe predicates:
    - `resolvePredicate('hasVerbalConjugation')`
    - `resolvePredicate('prefixHasPos:んです:v,adj-i,adj-na,aux')`
- If too broad/narrow, refine with `not`, add a `peek`, or split/rewire `alt` branches.

### 8) Guard common pitfalls
- Use `hasVerbalConjugation` (not `isVerb`).
- For ありません系, prefer `lemmaNeg:ある`.
- For sentence‑final の/ん, block `ので`, `のは` explicitly when needed.
- Punctuation: stop marks are kept; other punctuation is dropped pre‑match—don’t rely on commas.
- Prefer macros over ad‑hoc `repeat` to cover tokenizer variance consistently.

### 9) Prioritization guidance
- Higher `priority` for frequent/teachable patterns or those with cleaner detection.
- Separate close siblings so the better pedagogical fit surfaces first.

### 10) Advanced tools (when appropriate)
- `AttributiveClause` for relative‑like modifiers.
- `NPCase`/`NPTopic` to attach particles robustly.
- `PrePredicateAdjuncts` before predicates for natural clause adjuncts.

### Macros vs Predicates: when and why
- Use a MACRO when:
  - You are encoding structure across multiple tokens (e.g., NP + particle, adjective splits like 〜く + て/ない).
  - You need to normalize tokenizer variance (fused vs split) in one reusable place.
  - The pattern is reused across multiple grammars (e.g., `PrePredicateAdjuncts`, `NP`, `IAdjKute`).
  - You want to compose bigger patterns from tested building blocks.
- Avoid a MACRO when:
  - The pattern is highly idiosyncratic to one grammar and unlikely to recur.
  - A single `sequence/alt` inside the grammar JSON is enough and clearer.
  - The behavior would be better expressed as a single‑token constraint (that’s a predicate).

- Use a PREDICATE when:
  - You need an atomic check on a single token: text/lemma/pos/reading/conjugation flags.
  - You need lexical sensitivity that requires DB lookup (e.g., `prefixHasPos`).
  - The condition is orthogonal and reusable across grammars (e.g., `isTopicMarker`, `hasVerbalConjugation`).
- Avoid a PREDICATE when:
  - The logic spans multiple tokens or ordering (that’s a macro/pattern), not a single token.
  - You would need to query context extensively—prefer a macro with `peek/not/optional/repeat`.

### Creating new macros/predicates: guidelines
- Create a NEW MACRO if:
  - You repeated the same multi‑token `sequence/alt/optional/repeat` in ≥2 grammars.
  - It expresses a linguistic unit taught as a chunk (e.g., い‑adj 〜くて, NP+case, clause adjuncts).
  - It encapsulates tokenizer variance you don’t want duplicated everywhere.
- Don’t create a new macro if:
  - It’s a one‑off for a single pattern or adds marginal abstraction.
  - It’s just a thin wrapper around a single `token`—that should be a predicate.

- Create a NEW PREDICATE if:
  - You need a reusable single‑token property missing from current predicates.
  - Detection requires lexical normalization, conjugation flags, or DB‑assisted checks.
  - Multiple grammars would benefit from the same atomic test.
- Don’t create a new predicate if:
  - Existing `text/lemma/lemmaNeg/pos/kana` suffice with a simple regex/value.
  - The rule relies on adjacent tokens—make it a macro/pattern instead.

### Quick summary
- Start with macro‑based structure → constrain with predicates → model fused/split via ordered `alt` → add minimal captures → iterate with `DEBUG.md` tools and tests.


