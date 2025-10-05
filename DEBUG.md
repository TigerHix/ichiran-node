# Grammar Matcher Debugging Guide

## Quick Test Commands

```bash
# Test specific grammar pattern
bun test examples -t n5.na-adj-ni-narimasu

# Test all patterns
bun test examples

# Build after changes
bun run build
```

## Inspect Tokenization

Create temp script `debug.ts`:

```typescript
import { segmentSentenceWithAlternatives } from './src/grammarMatcher/runtime.js';
import './tests/load-env.js';
import { getConnectionFromEnv, setConnection } from './src/conn.js';
import { initializeIchiran } from './src/init.js';
import { initSuffixes } from './src/grammar/suffixCache.js';

const conn = getConnectionFromEnv();
setConnection(conn!);
initializeIchiran();
await initSuffixes({ blocking: true });

const alternatives = await segmentSentenceWithAlternatives('好きになりました。');
const token = alternatives[0].tokens[0];

console.log('Text:', token.text);
console.log('POS:', token.grammarInfo?.partOfSpeech);
console.log('Kana:', token.wordInfo?.kana);
console.log('Conjugations:', JSON.stringify(token.grammarInfo?.conjugations, null, 2));
```

Run: `bun run debug.ts`

## Check Dictionary Lookups

```typescript
import { findWordWithPos } from './src/dict/suffixHelpers.js';
// ... setup code ...

// Check if word has specific POS
const results = await findWordWithPos('好き', 'adj-na');
console.log('Found:', results.length > 0);
console.log('Entries:', results.map(r => ({ text: r.text, seq: r.seq })));
```

## Test Pattern Matching

```typescript
import { matchSentence } from './src/grammarMatcher/runtime.js';
import { grammarCatalog } from './src/grammarMatcher/catalog.js';

const matches = await matchSentence('好きになりました。', grammarCatalog);
console.log('Matches:', matches.map(m => m.grammarId));
console.log('Captures:', matches[0]?.captures.map(c => c.tokens.map(t => t.text).join('')));
```

## Common Debug Patterns

### View full token structure
```typescript
console.log(JSON.stringify(token, null, 2));
```

### Check predicate matching
```typescript
import { resolvePredicate } from './src/grammarMatcher/predicates.js';

const predicate = resolvePredicate('pos:exp');
const matches = await predicate(token, { tokens: [token], index: 0 });
console.log('Matches:', matches);
```

### Test async predicate
```typescript
const predicate = resolvePredicate('prefixHasPos:になる:adj-na');
const result = await predicate(token, { tokens: [token], index: 0 });
```

## Database Queries

```typescript
import { getConnection } from './src/conn.js';
const sql = getConnection();

// Find word by text
const words = await sql`SELECT * FROM kanji_text WHERE text = '好き'`;

// Get POS tags for word
const pos = await sql`
  SELECT sp.text FROM sense_prop sp 
  WHERE sp.seq = ${seq} AND sp.tag = 'pos'
`;
```

## Grammar Pattern Structure

```json
{
  "token": [
    "pos:exp",                    // Check POS
    "prefixHasPos:になる:adj-na"  // Check prefix after stripping suffix
  ]
}
```

### Pattern nodes:
- `sequence`: Match all in order
- `alt`: Match any one
- `optional`: Match 0 or 1
- `repeat`: Match min-max times
- `capture`: Save matched tokens
- `token`: Match single token with predicates

## Predicate Format

Sync predicates (all are async now but don't need await):
- `text:好き` - exact text match
- `pos:adj-na` - POS tag
- `kana:/pattern/` - regex on kana
- `isNaAdjective` - helper predicate

Async predicates (need DB lookup):
- `prefixHasPos:suffix:pos` - strip suffix, check prefix POS

## Adding New Predicates

1. Add to `predicateFactories` in `predicates.ts`
2. All predicates return `AsyncPredicateFn` (returns `Promise<boolean>`)
3. Use `async` keyword: `() => async (token) => { ... }`

## Tips

- Token text is in original form (conjugated)
- Base form in `token.grammarInfo.conjugations[0].word`
- POS is array, use `.some()` to check
- Cache DB lookups to avoid slowdown
- Test both positive AND negative examples

## Updates (concise)

### Punctuation filtering
- We KEEP stop marks, DROP other punctuation/whitespace before matching.
- Stop marks kept (split sequences): `。 ! ? 「 」 『 』 （ ） 【 】 ［ ］ " '`
- Code: `filterNonStopMarkPunctuation()` in `src/grammarMatcher/runtime.ts`.
- Symptom: comma blocks 〜くて → fix by ensuring commas are filtered.

### Prefer macros over ad-hoc repeats
- Use these in grammar JSON:
  - `PrePredicateAdjuncts` – adverbs, NP+case/topic, simple exp
  - `NP`, `NPCase`, `NPTopic`
  - `IAdjKu`, `IAdjKute`, `IAdjKunai`, `IAdjKunakute`
  - `NaAdjDe` (excludes である)

Examples
```json
{ "macro": "IAdjKute" }
{ "macro": "NaAdjDe" }
{ "optional": { "macro": "PrePredicateAdjuncts" } }
```

### Tiny predicates, structural macros
- Atomic: `isCopulaDe` (で as copula conjunctive)
- Structure via macros to handle token splits: よくて vs よく + て, くない vs く + ない

### Common fixes
- Matching inside quotes? Keep quotes as stop marks so patterns don’t span `「…」` or ASCII quotes.
- Use `isPredicate` (any conjugation) instead of `isPredicateHead` (root only) for real sentences.

### Quick probes
- Show tokens with POS presence (helps see punctuation kept/dropped):
```ts
for (const t of tokens) console.log(t.text, !!t.grammarInfo?.partOfSpeech, !!t.wordInfo);
```
- Predicate check:
```ts
const p = resolvePredicate('isCopulaDe'); await p(token, { tokens, index });
```

### Testing
- Single grammar: `bun test examples -t n5.i-adj-kute`
- Broad sanity: `bun test examples`
