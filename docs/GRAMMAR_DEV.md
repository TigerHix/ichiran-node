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
- Location: `@ichiran/grammar/grammars/<level>/`.
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

---

## Runtime Architecture

### File Structure

The grammar runtime is split into focused modules:

- **`runtime.ts`** - Orchestration layer
  - `matchText(text, defs, options)` - match grammars, select best segmentation per sentence
  - `analyzeText(text, defs, options)` - match + full segmentation data
  - `compileGrammars(defs)` - compile and sort grammars by priority

- **`matcher.ts`** - Pattern matching and ranking
  - `matchGrammars(tokens, grammars, options)` - match compiled grammars against tokens
  - `selectBestOutcome(outcomes, startIndex)` - deterministic outcome ranking (span > preference > captures)
  - Start gate precomputation for O(n) performance

- **`cache.ts`** - Compiled pattern LRU cache
  - `getCompiledMatcher(def)` - get or compile a matcher with caching
  - `clearCompiledGrammarCache()` - reset cache (useful for tests)
  - `setCompiledGrammarCacheCapacity(n)` - adjust cache size (default: 500)

- **`segmentation.ts`** - Tokenization alternatives
  - `segmentText(text, limit, normalizePunctuation)` - generate tokenization alternatives
  - `splitTextBySentences(text)` - split by sentence boundaries

- **`segments.ts`** - Capture segment builder
  - `buildSegmentsFromTokens(tokens, captures)` - build alternating raw/capture segments for rendering

- **`compiler.ts`** - Pattern compilation
  - `compilePattern(node)` - compile pattern nodes into matchers
  - Handles sequence, alt, token, repeat, optional, capture, peek, not, anchor, macro

- **`predicates.ts`** - Token predicates
  - `resolvePredicate(spec)` - resolve predicate string to function
  - `evaluatePredicates(token, ctx, preds)` - evaluate predicates with AND semantics
  - Built-in predicates: `text:`, `lemma:`, `pos:`, `kana:`, `isNaAdjective`, `hasVerbalConjugation`, etc.

- **`profile.ts`** - Performance profiling helpers
  - `time(label, fn)` - synchronous timing
  - `timeAsync(label, fn)` - asynchronous timing
  - Controlled by `GRAMMAR_PROFILE` env var

- **`macros.ts`** - Macro min-token registry
  - `MACRO_MIN_TOKENS` - conservative minimum token counts for built-in macros
  - Used to skip futile match attempts when insufficient tokens remain

### Public APIs

#### Core Matching

```typescript
import { matchText, analyzeText } from '@ichiran/grammar';

// Match patterns and get hits from best segmentation per sentence
const hits = await matchText(text, grammarDefs, {
  maxMatches: 100,
  limit: 5,                    // segmentation alternatives per sentence
  normalizePunctuation: false
});

// Match + full segmentation data
const result = await analyzeText(text, grammarDefs, options);
// result: { grammarMatches, segments, tokens, grammarDetails }
```

#### Cache Management

```typescript
import { clearCompiledGrammarCache, setCompiledGrammarCacheCapacity } from '@ichiran/grammar';

// Clear cache (useful for tests or when grammar defs change at runtime)
clearCompiledGrammarCache();

// Adjust cache capacity (default: 500 entries)
setCompiledGrammarCacheCapacity(1000);
```

### Performance Optimizations

#### Profiling

Set `GRAMMAR_PROFILE=1` to enable detailed timing logs:

```bash
GRAMMAR_PROFILE=1 bun run examples/analyze.ts
```

Output includes:
- Total time, sentences, alternatives tried
- Grammar count, matcher calls/ms
- selectBestOutcome ms, buildSegments ms
- Compile time, cache hits/misses
- Token counts (filtered/unfiltered)

#### Start Gates

Start gates reduce unnecessary matcher invocations using a DSL:

- **`firstToken`** - predicates that must hold at the match start position (AND semantics)
- **`anyToken`** - predicates that must match *somewhere* in the token array (OR semantics, precomputed once)
- **`near`** - predicates that must match within a window around start position (precomputed as boolean array)

Precomputation avoids O(n²) predicate evaluations during matching.

Example:
```json
{
  "id": "n2.grammar-pattern",
  "startGate": {
    "firstToken": ["text:一番"],
    "anyToken": ["pos:v", "pos:adj-i"],
    "near": {
      "predicates": ["text:より"],
      "window": { "left": 3, "right": 3 }
    }
  }
}
```

#### Min-Token Estimation

Each pattern and macro has a conservative lower bound on required tokens. The matcher skips attempts when `tokens.length - startIndex < minTokens`.

Macro estimates (from `macros.ts`):
- `NP`: 1, `NPCase`: 2, `NPTopic`: 2
- `IAdjKu`: 1, `IAdjKunakute`: 2, `IAdjKuSuru`: 2
- `NaAdjDe`: 2, `NaAdjNiNaru`: 2
- `PrePredicateAdjuncts`: 0 (optional)

#### Compiled Pattern Cache

Patterns are compiled once and cached (LRU, default 500 entries). Cache key: `${grammarId}|${JSON.stringify(pattern)}` to detect changes. On cache hit, compilation is skipped entirely.


