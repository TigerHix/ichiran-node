# Grammar Rule Creation & Debugging Guide

## Philosophy: Linguistic Structure Over Test Cases

**Golden Rule:** Encode the grammatical structure, not the test cases.

### ❌ Wrong Approach
1. Look at failing test
2. Add specific pattern to match it
3. Repeat until all tests pass
4. Result: Overfitted, brittle patterns coupled to test data

### ✅ Right Approach
1. Understand the **linguistic principle** behind the grammar
2. Identify what **structurally distinguishes** valid from invalid uses
3. Encode those structural constraints
4. Validate with tests (they should pass naturally)

---

## Pattern Design Principles

### 1. Prefer Whitelisting Over Blacklisting

**Bad:** Enumerate all the things that CANNOT follow
```json
{
  "not": { "token": ["text:町"] },
  "not": { "token": ["text:星"] },
  "not": { "token": ["text:線"] }
  // ... infinite list
}
```

**Good:** Define what MUST follow
```json
{
  "sequence": [
    { "token": ["text:一番"] },
    { "token": ["isGradableElement"] }  // Structural requirement
  ]
}
```

### 2. Use Semantic Predicates

Create predicates that capture **linguistic concepts**, not surface patterns.

**Bad:** Hardcode specific words in patterns
```json
{
  "alt": [
    { "token": ["text:前"] },
    { "token": ["text:後"] },
    { "token": ["text:上"] }
    // ... 50 more
  ]
}
```

**Good:** Create semantic predicates
```typescript
isSpatialNoun: () => (token) => {
  // Nouns denoting absolute positions (not gradable)
  const spatialNouns = ['前', '後', '上', '下', ...];
  return spatialNouns.includes(token.text) && isNoun(token);
}
```

### 3. Structural Proximity Matters

If X must modify Y, ensure they're **structurally adjacent** in the pattern.

**Example:** Superlative 一番 must directly modify a gradable element
```json
{
  "sequence": [
    { "token": ["text:一番"] },
    { "token": ["isGradableElement"] }  // Must be next (or very close)
  ]
}
```

This naturally rejects compounds like `一番町` where 一番 is part of a name, not modifying anything.

### 4. Separate Linguistic Concerns

Don't mix unrelated concepts in one predicate.

**Bad:**
```typescript
isSpatialOrCompoundNoun: () => ...  // Conflates two concepts
```

**Good:**
```typescript
isSpatialNoun: () => ...      // Positional nouns
isCompoundSuffix: () => ...   // Name-forming suffixes
```

---

## Debugging Workflow

### Step 1: Understand the Grammar Linguistically

Before touching code, answer:
- What is the **functional meaning** of this grammar?
- What **structural features** distinguish it from similar patterns?
- What are **edge cases** (non-prototypical uses)?

**Example (一番):**
- Function: Marks superlative degree
- Structure: Modifies gradable predicates (adj/verb/adv)
- Edge cases: Ordinal use (一番に), spatial use (一番前), compounds (一番町)

### Step 2: Identify Why Tests Fail

For each failing example, ask:
- Is this a **structural issue** (pattern doesn't capture valid forms)?
- Is this a **semantic issue** (need better predicates)?
- Is this an **overfitting issue** (pattern too specific to other tests)?

### Step 3: Fix Structurally, Not Superficially

**Bad fixes:**
- "Test has を particle, so I'll allow を"
- "Test has this word, so I'll match this word"

**Good fixes:**
- "Verbs can take object arguments marked by を, so allow [noun を gradable]"
- "Compound predicates have [noun + adjective], so add that alternative"

### Step 4: Validate Generalizability

After fixes, test with examples NOT in the test suite:
```javascript
// Add variations
"一番楽しみにしている"  // Different particle (に)
"一番期待されている"    // Passive form
```

If these fail despite being valid, your pattern is still overfitted.

---

## Common Pitfalls

### 1. Hardcoding Test Cases
**Symptom:** Each test failure adds a specific token match
**Fix:** Identify the underlying pattern class

### 2. Over-Broad Patterns
**Symptom:** Negative examples incorrectly match
**Fix:** Add structural constraints (not just token blacklists)

### 3. Mixing Concerns
**Symptom:** One predicate checks multiple unrelated properties
**Fix:** Split into focused predicates with clear purposes

### 4. Ignoring Word Order
**Symptom:** Pattern matches but at wrong positions
**Fix:** Use sequences and lookaheads to enforce structure

### 5. Missing Linguistic Distinctions
**Symptom:** Can't distinguish valid from invalid without enumerating all cases
**Fix:** Find the structural/semantic property that makes the distinction

---

## Pattern Alternatives Strategy

When a grammar has multiple valid forms, structure alternatives by their **grammatical role**, not surface similarity.

**Good structure:**
```json
{
  "alt": [
    { "sequence": [...] },  // Alternative 1: の + noun + copula (predicative nominal)
    { "sequence": [...] },  // Alternative 2: Direct modification of adjective/adverb
    { "sequence": [...] }   // Alternative 3: Modification of verb with object
  ]
}
```

Each alternative should represent a distinct grammatical construction.

---

## When to Create New Predicates

Create a new predicate when:
1. ✅ You find yourself repeating the same token alternatives
2. ✅ The pattern represents a clear linguistic concept
3. ✅ It could be reused in other grammars
4. ✅ It separates semantic from structural concerns

Don't create a predicate when:
1. ❌ It's just a list of test case words
2. ❌ It's only used once and very specific
3. ❌ It's a structural constraint (use pattern sequences instead)

---

## Validation Checklist

Before considering a grammar complete:

- [ ] Pattern is based on linguistic structure, not test shapes
- [ ] Each alternative has a clear grammatical purpose
- [ ] Predicates capture semantic concepts, not word lists
- [ ] Can explain why each negative example should fail
- [ ] Can explain why each positive example should pass
- [ ] Pattern doesn't overfit to specific test vocabulary
- [ ] Structural constraints prevent non-grammatical matches

---

## Example: Debugging Session

**Problem:** `一番町に新しいカフェができた` incorrectly matches

**Bad analysis:** "Add 町 to blacklist"

**Good analysis:**
1. Why should this fail? 一番 here is part of a place name, not modifying anything
2. What's the structural difference? In valid uses, 一番 is followed by/near a gradable element
3. How to encode this? Require that gradable elements appear within 1-2 tokens after 一番
4. Result: Naturally rejects all compound uses without enumerating them

---

## Remember

> "The best pattern is the one that captures the grammar, not the test cases."

A well-designed pattern will pass tests as a side effect of correctly encoding the linguistic structure.

