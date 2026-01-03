# Writing Grammar Rules

Rules match Japanese grammar patterns using GiNZA's dependency parse and morphology.

## Project Structure

```
src/
├── engine/              # Core matching engine
│   ├── dsl.ts           # Low-level DSL primitives
│   ├── lang.ts          # High-level LinguisticRuleBuilder
│   └── compiler.ts      # Rule compilation and matching
├── rules/
│   └── bunpro/          # Bunpro rulesets
│       ├── _test/       # Shared test utilities
│       │   ├── engine.ts    # Shared GinZA engine (globalThis singleton)
│       │   └── helpers.ts   # describeRule() and test utilities
│       ├── jlpt1/       # One folder per JLPT level
│       │   ├── index.ts     # Exports BUNPRO_JLPT1 ruleset
│       │   ├── を経て.ts     # Rule definition (one rule per file)
│       │   └── を経て.test.ts # Tests (side-by-side with rule)
│       ├── jlpt2/ ... jlpt5/
│       ├── index.ts     # Re-exports all rulesets
│       └── nonJlpt.ts
├── data/bunpro/         # Bunpro JSON test data (positive examples)
└── ruleset.ts           # Ruleset types and matching
```

**File naming**: Rule files use Japanese UTF-8 filenames matching the rule ID (e.g., `だけでなく.ts`). Use `export default` to avoid Japanese variable names.

---

## Creating a New Rule

### Step 1: Create the rule file

Create `src/rules/bunpro/jlpt4/だけでなく.ts`:

```typescript
import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('だけでなく', (r) => {
  // 1. Declare tokens with constraints
  const dake = r.tok({ lemma: 'だけ' }, 'dake');
  const de = r.tok({ text: 'で' }, 'de');
  const nai = r.aux({ lemma: 'ない' }, 'nai');

  // 2. Add structural constraints
  r.inOrder(dake, de, 1).inOrder(de, nai, 2);

  // 3. Define captures (what to highlight)
  r.captureSpan('だけでなく', dake, nai);
});
```

### Step 2: Add to the ruleset index

Update `src/rules/bunpro/jlpt4/index.ts`:

```typescript
import type { Ruleset } from '../../../ruleset.js';
import dakedenaku from './だけでなく.js';
// ... other imports

export const BUNPRO_JLPT4: Ruleset = {
  id: 'bunpro.jlpt4',
  rules: [dakedenaku, /* ... */],
};
```

### Step 3: Create the test file

Create `src/rules/bunpro/jlpt4/だけでなく.test.ts`:

```typescript
import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './だけでなく.js';
import { BUNPRO_JLPT4 } from './index.js';

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get);
});
```

### Step 4: Run tests and iterate

```bash
bun test src/rules/bunpro/jlpt4/だけでなく.test.ts
```

Failures show detailed explain output:

```
❌ Rule 'だけでなく' failed on: 日本語だけでなく英語も話せる。
   Reason: No candidates for variable 'nai'
   Failed clause: node
   Partial bindings: {"dake":3,"de":4}
```

This tells you `nai` didn't match—adjust constraints and repeat.

---

## Testing: Positives vs Negatives

### Positive examples (automatic)

Positive test sentences come from Bunpro JSON data in `data/bunpro/`. The `describeRule()` helper automatically loads and tests all sentences for the rule.

### Negative examples (manual)

**Critical**: Rules that pass positives may still overcapture. Add negative examples—sentences that look similar but should NOT match:

```typescript
// jlpt3/では-それでは-じゃあ.test.ts
const negatives = [
  // Locative では (で=case marker, not conjunction)
  '東京では電車が便利です。',
  '日本では桜が有名です。',
  // ではない negation (different grammar)
  '彼は学生ではない。',
  // Instrumental で + topic は
  '車では行けない場所です。',
];

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get, { negatives });
});
```

Test output:
- `✓` positives that correctly match
- `✗` negatives that correctly don't match
- `❌ FALSE POSITIVE` if a negative incorrectly matches (test fails)

### Finding negative examples

1. **Similar surface forms**: Same particles/words but different grammar
2. **Different dependency structures**: Same words but separate clauses
3. **Different auxiliary chains**: Same verb but different tense/aspect

### Skipping unmatchable positives (GiNZA limitations)

**Use `skipPositives` only as a last resort** when a Bunpro sentence genuinely cannot be matched due to GiNZA parsing limitations—not because the rule is hard to write.

```typescript
const skipPositives = [
  'クエンティンさんは映画監督で俳優です。',
  '彼はアメリカの数学者で大学教授だった。',
];

describe('bunpro.jlpt5', () => {
  const engine = useSharedEngine([BUNPRO_JLPT5]);
  describeRule(rule, 'JLPT5', BUNPRO_JLPT5.id, engine.get, { negatives, skipPositives });
});
```

#### Strict requirements

Each skipped sentence **must** have a documented justification proving:

1. **GiNZA parses identical grammar inconsistently** — show parse output for both working and failing cases
2. **No discriminator exists** — matching would cause overcapture on unrelated patterns
3. **The limitation is in GiNZA, not the rule** — other similar sentences DO match

#### Required analysis format

Document each skip in the test file with concrete evidence:

```typescript
// Sentences that can't be matched due to GiNZA parsing limitations:
//
// ANALYSIS: Noun + で conjunction (copula te-form)
//
// GiNZA parses this pattern inconsistently:
//   漫画家で  → lemma=だ (copula) ✓ WORKS
//   映画監督で → lemma=で (particle) ✗ INDISTINGUISHABLE
//   数学者で  → lemma=で (particle) ✗ INDISTINGUISHABLE
//
// The discriminator `lemma=だ` identifies copula で vs locative で.
// But GiNZA only assigns lemma=だ to SOME Noun+で conjunctions.
// When lemma=で, it's identical to locative で (東京で働く).
//
// Matching all で with lemma=で would overcapture:
//   ❌ 東京で働く (locative: work IN Tokyo)
//   ❌ 鉛筆で書く (instrumental: write WITH pencil)
//
// CONCLUSION: No reliable discriminator. GiNZA limitation.
const skipPositives = [
  'クエンティンさんは映画監督で俳優です。',
  '彼はアメリカの数学者で大学教授だった。',
];
```

#### When NOT to use skipPositives

- ❌ "The rule is complex" — simplify or use `r.either()`
- ❌ "I can't figure out the pattern" — use `engine.analyze()` to inspect
- ❌ "It would need a new DSL feature" — consider adding the feature
- ❌ "Only one variant fails" — that means a discriminator likely exists

#### Verification checklist

Before adding a skip, confirm:

- [ ] You've inspected `engine.analyze(sentence)` output for failing AND working cases
- [ ] You've identified what differs (pos, dep, lemma, head, etc.)
- [ ] You've proven that using looser constraints would match unrelated grammar
- [ ] You've documented the full analysis in the test file

---

## DSL Reference

### Token Helpers

| Method | Creates |
|--------|---------|
| `r.tok({ ... }, name?)` | Any token |
| `r.verb({ ... }, name?)` | VERB token |
| `r.aux({ ... }, name?)` | AUX token |
| `r.noun({ ... }, name?)` | NOUN token |
| `r.adj({ ... }, name?)` | ADJ token |
| `r.adv({ ... }, name?)` | ADV token |
| `r.particle('text', name?)` | Particle by surface |

### Token Conditions

```typescript
r.tok({
  text: 'じゃ',                    // exact surface
  textOneOf: ['じゃ', 'では'],      // surface alternatives
  lemma: 'する',                   // dictionary form
  lemmaOneOf: ['へる', '経る'],     // lemma alternatives
  pos: 'VERB',                     // POS tag
  posOneOf: ['VERB', 'AUX'],       // POS alternatives
  dep: 'obj',                      // dependency label
  depOneOf: ['obj', 'nsubj'],      // dep alternatives
  inflectionForm: '意志推量形',     // conjugation form
  conjugationClass: '五段-ラ行',    // verb class
});
```

**Requirement**: Every rule must have at least one `text` or `lemma` constraint (used for dispatch).

### Dependency Edges

```typescript
r.headChild(head, child, 'depLabel');  // child --dep--> head
r.auxOf(verb, aux);                     // aux --aux--> verb (shortcut)
r.caseMarker(noun, particle);           // particle --case--> noun
r.objectOf(verb, obj);                  // obj --obj--> verb
r.copulaOf(head, copula);               // copula --cop--> head
```

### Order Constraints

```typescript
r.inOrder(a, b);      // a appears before b (any distance)
r.inOrder(a, b, 2);   // a before b, at most 2 tokens apart
```

### Alternative Patterns

```typescript
r.either(
  (b) => {
    const shimau = b.tok({ lemma: 'しまう', dep: 'fixed' }, 'shimau');
    b.capture(shimau);
  },
  (b) => {
    const chau = b.tok({ lemmaOneOf: ['ちゃう', 'じゃう'], pos: 'AUX' }, 'shimau');
    b.capture(chau);
  }
);
```

Each branch is expanded at compile time into separate rule variants—zero runtime overhead.

### Optional Clauses

```typescript
r.optional((b) => {
  const mo = b.particle('も', 'mo');
  b.inOrder(dake, mo, 1);
});
```

Optional clauses don't cause match failure if unmet.

### Captures

```typescript
r.capture(tok);                    // default name 'match'
r.captureAs('verb', tok);          // named token capture
r.captureSpan('pattern', a, b);    // span from a to b (char offsets)
```

Output: `{ captures: { 'pattern': { start: 5, end: 12, text: 'だけでなく' } } }`

---

## Avoiding Overcapture

### Common mistakes

**1. Surface text without structural constraints**

```typescript
// BAD: matches any では including locative 東京では
const de = r.tok({ text: 'で', pos: 'ADP' });
const wa = r.tok({ text: 'は', pos: 'ADP' });

// GOOD: conjunction では has dep=dep or dep=cc, not dep=case
const de = r.tok({ text: 'で', pos: 'ADP', dep: 'dep' }, 'de');
```

**2. Missing dependency edges**

```typescript
// BAD: matches volitional + しない even in separate clauses
const vol = r.verb({ inflectionForm: '意志推量形' });
const suru = r.verb({ lemma: 'する' });

// GOOD: require volitional to be syntactic child of する
r.headChild(suru, vol, 'advcl');
```

**3. Missing auxiliary constraints**

```typescript
// BAD: matches ようとした, ようとしている too
const suru = r.verb({ lemma: 'する' });

// GOOD: require specific auxiliary
const nai = r.aux({ lemma: 'ない' });
r.auxOf(suru, nai);
```

### Key discriminators

| Pattern | Risk | Discriminator |
|---------|------|---------------|
| Particles (では, には) | Locative vs conjunction | `dep=dep`/`dep=cc` vs `dep=case` |
| Aux chains (しない, している) | Wrong auxiliary | `auxOf()` with specific lemma |
| Multi-clause (ようとしない) | Separate clauses | `headChild()` edges |
| Fixed expressions | Parsed as single token | Check GiNZA output first |

---

## Debugging

### Explain mode

```typescript
const result = await engine.explainMatch('彼は諦めようとしない', '-ようとしない');
// → { matched: true, captures: {...} }
// → { matched: false, reason: '...', partialBinding: {...}, failedClause: {...} }
```

### Inspect GiNZA parse

```typescript
const doc = await engine.analyze('彼は諦めようとしない');
console.log(JSON.stringify(doc, null, 2));
```

Look at:
- `pos` (POS tag)
- `dep` (dependency label)
- `head` (index of syntactic head)
- `lemma` (dictionary form)
- `inflectionForm` (conjugation)

---

## Extending the DSL/Compiler

Before adding new DSL methods or compiler features, ask:

### Is it justified?

**Add if**:
- The pattern appears in multiple rules (reusable)
- It represents a genuine linguistic abstraction (not a workaround)
- It simplifies rule writing without sacrificing clarity

**Don't add if**:
- It's a one-off hack for a single edge case
- It can be expressed with existing primitives
- It's an escape hatch to avoid understanding GiNZA's parse

### Example: Good extension

```typescript
// Adding auxOf() was justified because:
// 1. Aux attachment is extremely common in Japanese grammar
// 2. It's a well-defined UD relation (aux)
// 3. Many rules need it: てしまう, ている, てある, etc.

r.auxOf(verb, aux);  // instead of r.headChild(verb, aux, 'aux')
```

### Example: Bad extension

```typescript
// DON'T add: "match any token between a and b"
// Why: This is an escape hatch that bypasses structural matching.
// Instead: Figure out the actual dependency structure and match it.
```

### Process for new primitives

1. Implement the rule using existing DSL
2. If awkward, identify the repeated pattern across 3+ rules
3. Propose the abstraction with examples of reuse
4. Add to `lang.ts` (high-level) or `dsl.ts` (low-level)
5. Document in this guide

---

## Reference

### POS Tags (Universal Dependencies)

`VERB`, `NOUN`, `AUX`, `ADJ`, `ADV`, `ADP`, `NUM`, `PRON`, `DET`, `CCONJ`, `SCONJ`, `PART`, `PUNCT`, `SYM`, `X`, `INTJ`

### Dependency Labels

`aux`, `case`, `obj`, `nsubj`, `acl`, `advcl`, `advmod`, `amod`, `mark`, `compound`, `fixed`, `flat`, `nmod`, `obl`, `cop`, `det`, `nummod`, `punct`, `root`, `dep`, `cc`, `conj`, `dislocated`, `discourse`, `csubj`, `expl`, `iobj`, `parataxis`, `vocative`, `ccomp`, `xcomp`

### Common Inflection Forms

`終止形-一般`, `連用形-一般`, `連体形-一般`, `仮定形-一般`, `意志推量形`, `未然形-一般`, `命令形`

See `src/ginza/generated.ts` for the full corpus-derived type lists.

---

## Quick Reference

```bash
# Run all tests for a rule
bun test src/rules/bunpro/jlpt4/だけでなく.test.ts

# Run all tests for a level
bun test src/rules/bunpro/jlpt4/

# Typecheck
bun typecheck

# Benchmark
bun scripts/benchmark.ts
```
