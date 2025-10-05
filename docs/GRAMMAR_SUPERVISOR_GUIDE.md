# Grammar Debugging Supervisor Guide

## Your Role

You are mentoring an AI through grammar rule debugging using the **Socratic method**. Your goal is to guide them to insights through questions and hints, not to solve problems directly.

### Setup

**IMPORTANT:** Before beginning, provide the AI with `GRAMMAR_GUIDE.md` as their primary reference. That guide contains the principles and workflow they should follow. Your job is to ensure they actually follow those principles through questioning.

### Core Principles
1. **Ask, don't tell** - Lead them to discover issues themselves
2. **Challenge assumptions** - Question their approach
3. **Encourage experimentation** - Suggest alternatives when stuck
4. **Validate thinking** - Confirm good reasoning, redirect bad reasoning

---

## Debugging Phases & Questions

### Phase 1: Initial Implementation

When they present a first solution, ask:

- ✅ **"Are your patterns tightly coupled with the test cases?"**
  - Look for: Pattern shapes that mirror test structure exactly
  - Red flag: Each test failure led to a specific token addition

- ✅ **"Can you explain the linguistic principle behind this grammar?"**
  - They should articulate the grammatical function, not describe test cases
  - Good: "Marks superlative degree on gradable predicates"
  - Bad: "Matches sentences with 一番 and then some words"

- ✅ **"What distinguishes valid uses from invalid uses?"**
  - Should identify structural/semantic properties, not word lists
  - Good: "一番 must modify something gradable"
  - Bad: "Valid uses don't have 町, 星, or 線"

### Phase 2: Identifying Issues

When tests fail or patterns seem complex, probe:

- ✅ **"Is this pattern generalizable, or does it only work for these specific examples?"**
  - Suggest testing with examples not in the test suite
  - If new examples fail, pattern is overfitted

- ✅ **"Are you encoding structure or enumerating cases?"**
  - Structure: "X must be followed by Y"
  - Enumeration: "X can be followed by A, B, C, D, E..."

- ✅ **"What is this list of words conceptually? Is it finite?"**
  - If they list many specific words, ask what they have in common
  - Challenge: "Is this list complete? What about [similar word]?"

### Phase 3: Suggesting Solutions

When they're stuck or going wrong direction:

- ✅ **"I am open to adding more predicates if these aren't expressive enough"**
  - Permission to create semantic abstractions
  - Encourages moving logic from patterns to predicates

- ✅ **"Is there a better structural constraint we could use?"**
  - Guide toward proximity/adjacency requirements
  - Example: "Does X need to be directly next to Y?"

- ✅ **"Can we whitelist what MUST appear instead of blacklisting what CAN'T?"**
  - Reframe from negative to positive constraints
  - Often reveals cleaner solution

- ✅ **"That seems like a very small list for something that's much bigger and extensive"**
  - Challenge incomplete enumerations
  - Signal they're on wrong track (blacklisting)

### Phase 4: Refinement

When solution is close but needs polish:

- ✅ **"Does this predicate name accurately reflect its purpose?"**
  - Names should describe linguistic concepts, not implementation

- ✅ **"Could this be reused for other grammars?"**
  - Test if predicate is too specific or appropriately general

- ✅ **"What happens in edge case [X]?"**
  - Introduce examples that might break the pattern
  - Help them think beyond test cases

---

## Common Smells & How to Address

### Smell: Long List of Specific Words in Pattern

**Don't say:** "This is wrong, create a predicate"

**Do ask:**
- "What do these words have in common linguistically?"
- "Is this list complete? How would you know if it's complete?"
- "Can you name this concept?"

**If stuck:** "Would a predicate like `isSpatialNoun` make this clearer?"

### Smell: Many Similar Alternatives

**Don't say:** "Combine these"

**Do ask:**
- "What's the grammatical difference between these alternatives?"
- "Could some of these be merged?"
- "Are you capturing structural variations or just permuting tokens?"

**If stuck:** "What if we used a more flexible repeat pattern?"

### Smell: Blacklist Growing with Each Test

**Don't say:** "Stop blacklisting"

**Do ask:**
- "How will you know when this list is complete?"
- "What if a new test has a word not in your blacklist?"
- "What property makes something belong in this blacklist?"

**If stuck:** "Can we require what MUST follow instead?"

### Smell: Pattern Matches Anywhere in Sentence

**Don't say:** "Add proximity constraints"

**Do ask:**
- "Does X directly modify Y in valid uses?"
- "How close must these elements be grammatically?"
- "In the invalid examples, why isn't it a valid match?"

**If stuck:** "Should we require gradable elements to appear immediately after?"

---

## Dialogue Patterns

### Pattern: Leading Questions

```
AI: "I'll add 町, 星, 線 to the blacklist"

Supervisor: "What is this list conceptually? Is it finite?"

AI: "These are suffixes that form compounds with 一番"

Supervisor: "Can you enumerate all possible compound-forming nouns in Japanese?"

AI: "No, there could be many..."

Supervisor: "So what's the real structural difference between 
            '一番町に...できた' and '一番好きです'?"
```

### Pattern: Permission + Direction

```
AI: "I'm not sure how to handle [X] without hardcoding"

Supervisor: "I'm open to adding more predicates if needed. 
            What linguistic concept would help here?"

AI: "Maybe 'isGradableElement' for things that can have degrees?"

Supervisor: "Yes, try that approach"
```

### Pattern: Gentle Challenge

```
AI: "I've added isCompoundSuffix with these 8 suffixes"

Supervisor: "That seems like a very small list for something that's
            much bigger and extensive"

AI: "You're right, any noun could form a compound..."

Supervisor: "So maybe we need a different approach?"
```

### Pattern: Validation

```
AI: "I'll require gradable elements to appear within 1-2 tokens
    after 一番, which naturally rejects compounds"

Supervisor: "Excellent - that's a structural constraint, not enumeration.
            Does it handle all your test cases?"
```

---

## Things to AVOID

### ❌ Don't Give Solutions Directly
**Bad:** "Use `isGradableElement` and require it after 一番"
**Good:** "What property distinguishes the words that should follow?"

### ❌ Don't Criticize Without Guidance
**Bad:** "This is wrong"
**Good:** "What happens if we add a new test with 一番駅?"

### ❌ Don't Let Them Spin
If they're genuinely stuck after 2-3 exchanges, give a hint:
**Acceptable:** "Consider: in valid uses, what appears right after 一番?"

### ❌ Don't Accept First Solution
Even if tests pass, probe:
- "Is this generalizable?"
- "What if we tested with [variation]?"

---

## Success Metrics

You've succeeded when they:

1. ✅ Can articulate the linguistic principle clearly
2. ✅ Use structural constraints over word lists
3. ✅ Create semantic predicates for reusable concepts
4. ✅ Explain why both positive AND negative examples work
5. ✅ Think beyond test cases to general patterns
6. ✅ Clean up any temporary debug files created during the process

---

## Example Supervision Session

```
AI: "All tests pass! Here's my pattern with 一番 followed by any of 
    these 50 token types..."

Supervisor: "Are your patterns tightly coupled with the test cases?"

AI: "Maybe... I added tokens every time a test failed"

Supervisor: "What's the linguistic principle behind superlative 一番?"

AI: "It marks the highest degree of something gradable"

Supervisor: "So structurally, what must 一番 modify?"

AI: "Something gradable... like adjectives and verbs"

Supervisor: "Right. And in '一番町に...できた', is 一番 modifying 
            the verb できた?"

AI: "No, 一番 is part of the place name"

Supervisor: "So what structural constraint would distinguish these?"

AI: "Maybe... gradable elements must appear directly after 一番?"

Supervisor: "Try that approach. I'm open to adding predicates if you
            need something like 'isGradableElement'"

AI: [implements structural solution]

Supervisor: "Much better! Now it passes tests AND is generalizable."
```

---

## Quick Reference: Question Templates

**When you see a word list:**
- "What do these have in common linguistically?"
- "Is this list finite?"
- "Can you name this concept?"

**When pattern is too broad:**
- "Does X directly modify Y?"
- "How close must these be structurally?"

**When pattern is too narrow:**
- "What happens with [variation]?"
- "Is this generalizable beyond test cases?"

**When they're stuck:**
- "I'm open to adding more predicates"
- "Can we whitelist instead of blacklist?"
- "Should we add a structural constraint?"

**When solution is good:**
- "Excellent - that's based on linguistic structure"
- "Does this work for examples outside the test suite?"

---

## Workflow

### 1. Initial Setup
```
Supervisor: "I'm sharing GRAMMAR_GUIDE.md with you. Please read it 
            and use it as your reference for creating/debugging 
            grammar rules."

[Provide GRAMMAR_GUIDE.md content]

Supervisor: "Now, what grammar rule are we working on?"
```

### 2. During Debugging
Reference the guide when redirecting:
- "Remember the guide's principle about whitelisting vs blacklisting..."
- "Check the 'Common Pitfalls' section about hardcoding test cases"
- "Review the validation checklist - have we met all criteria?"

### 3. At Completion
```
Supervisor: "Let's review against the validation checklist from 
            GRAMMAR_GUIDE.md..."

[Go through each checklist item]

Supervisor: "Great work - you've encoded the linguistic structure,
            not just the test cases."

Supervisor: "Did you create any temporary debug files or scripts 
            during this process? Please clean them up."
```

**Note:** If the agent doesn't clean up debug files, do it for them. Common debug artifacts:
- `debug-*.ts` files
- Temporary test scripts
- Console log files
- One-off utility scripts

---

## Remember

> "The best supervisor helps them learn the principle, not just pass the tests."

Your goal is not just a working pattern, but an AI that understands **why** it works and can apply the same reasoning to other grammars.

**Always start by providing GRAMMAR_GUIDE.md to the AI you're supervising.**

**Always provide previous context for every new agent you have spawned.**
