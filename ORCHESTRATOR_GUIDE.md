# Grammar Rule Implementation Orchestrator Guide

This guide explains how to implement multiple Japanese grammar rules in parallel using git worktrees and subagents.

## Overview

The strategy:
1. Identify 10 grammar rules to implement from the same JLPT level
2. Create a git worktree for each rule (allows isolated commits)
3. Spawn a subagent for each worktree to implement the rule
4. Each subagent: creates rule file, test file, updates index, runs tests, commits
5. Merge all branches to main
6. Clean up worktrees

## Prerequisites

- Read `DEV_GUIDE.md` to understand the grammar rule structure
- Understand JLPT level structure (JLPT5 has the most rules)
- Know how to use git worktrees

## Step-by-Step Process

### Step 1: Identify Grammar Rules to Implement

List available grammars for a JLPT level:

```bash
ls packages/grammar/data/bunpro/JLPT5/
```

Check what's already implemented:

```bash
ls packages/grammar/src/rules/bunpro/jlpt5/*.ts
```

Example batch of 10 JLPT5 rules:
1. だ (copula)
2. です (polite copula)
3. は (topic marker)
4. を (object marker)
5. で (locative particle)
6. に (dative particle)
7. へ (directional particle)
8. と (quotation/with particle)
9. よ (sentence particle)
10. ね (sentence particle)

### Step 2: Create Git Worktrees

Create a branch and worktree for each rule:

```bash
# For each rule, create a branch and worktree
git worktree add /tmp/jlpt5-1 -b jlpt5-da      # だ
git worktree add /tmp/jlpt5-2 -b jlpt5-desu    # です
git worktree add /tmp/jlpt5-3 -b jlpt5-wa      # は
# ... and so on for all 10 rules
```

**Important:** Use descriptive branch names like `jlpt5-{rule-id-romanized}`

**Note:** If you're already on a branch in the main repo (like jlpt5-ne), that rule can be implemented in the main worktree directly.

### Step 3: Spawn Subagents in Parallel

Use the `Task` tool with `subagent_type="general-purpose"`. Spawn all 10 in a SINGLE message with multiple tool use blocks.

**Subagent Prompt Template:**

```
You are implementing a Japanese grammar rule for the ichiran-node project.

**Your task:** Implement the JLPT5 grammar rule for **{RULE_NAME}** ({BRIEF_DESCRIPTION}).

**Working directory:** {WORKTREE_PATH} (already checked out to branch {BRANCH_NAME})

**Files to create:**

1. **Rule file:** `packages/grammar/src/rules/bunpro/jlpt5/{FILENAME}.ts`
   - Use `linguisticRule` from `../../../engine/lang.js`
   - Rule ID is '{RULE_ID}'
   - [SPECIFIC PATTERN DETAILS FOR THIS RULE]
   - Capture the {TOKEN} token

2. **Test file:** `packages/grammar/src/rules/bunpro/jlpt5/{FILENAME}.test.ts`
   - Follow the pattern from {SIMILAR_RULE}.test.ts
   - Use `describeRule` helper
   - Add negative tests for: [SPECIFIC FALSE POSITIVES]
   - Test data is at `packages/grammar/data/bunpro/JLPT5/{FILENAME}.json`

3. **Update index:** `packages/grammar/src/rules/bunpro/jlpt5/index.ts`
   - Import and add the new rule to BUNPRO_JLPT5.rules array

**Important:**
- Read the DEV_GUIDE.md at `packages/grammar/DEV_GUIDE.md` for the DSL reference
- Run tests with: `bun test packages/grammar/src/rules/bunpro/jlpt5/{FILENAME}.test.ts`
- Inspect parses with the `engine.analyze()` function if needed
- **All tests must pass (0 fail) before committing. Failing tests are NOT acceptable.**
- When tests pass, commit: `git add -A && git commit -m "Implement JLPT5: {RULE_NAME} ({DESCRIPTION})"`

**DO NOT:**
- Modify files outside your worktree
- Push to remote
- Merge to main
- Move failing tests to skipPositives - fix the rule instead
- Add tests to skipPositives without completing the SKIPPOSITIVES LEGITIMACY CHECKLIST (Step 7)

**Before adding any skipPositive, you MUST verify it against the 5-item checklist:**
1. Different grammar point? (separate rule exists)
2. Grammar scope? (JSON doesn't include this form)
3. GiNZA limitation? (verified with engine.analyze())
4. Data bug? (malformed data)
5. Different structure? (fundamentally different construction)

Report back when complete with test results showing 0 failures.
```

**Key Customizations per Rule:**
- **Particles (は, を, に, etc.):** Use `r.particle()` with `dep='case'` and `r.caseMarker()`
- **Copulas (だ, です):** Use `r.either()` for noun vs adjective patterns, check `dep='cop'` vs `dep='aux'`
- **Sentence particles (よ, ね):** Match with `pos='PART'`, require sentence-final position
- **Conjunctions:** Use `dep='dep'` or `dep='cc'` to distinguish from other usages

### Step 4: Wait for All Subagents to Complete

Each agent will:
1. Read the test data from the JSON file
2. Analyze sample sentences with `engine.analyze()`
3. Write the rule file using the DSL
4. Write the test file
5. Update the index
6. Run tests
7. Commit when tests pass

Expected test results per rule: 15-30 tests passing

### Step 5: Merge to Main

**Method A: Copy Files (Recommended)**

Since each branch modifies `index.ts`, there will be merge conflicts. Instead:

```bash
# Switch to main
git checkout main

# Copy all rule files from worktrees
cp /tmp/jlpt5-1/packages/grammar/src/rules/bunpro/jlpt5/*.ts packages/grammar/src/rules/bunpro/jlpt5/
cp /tmp/jlpt5-2/packages/grammar/src/rules/bunpro/jlpt5/*.ts packages/grammar/src/rules/bunpro/jlpt5/
# ... repeat for all worktrees

# Manually update index.ts with all imports
```

**Index Template:**
```typescript
import type { Ruleset } from '../../../ruleset.js';
import adjectivete from './adjective-て-b.js';
// ... existing imports
import da from './だ.js';
import desu from './です.js';
import wa from './は.js';
// ... all new imports

export const BUNPRO_JLPT5: Ruleset = {
  id: 'bunpro.jlpt5',
  rules: [
    adjectivete,
    // ... existing rules
    da,
    desu,
    wa,
    // ... all new rules in alphabetical order
  ],
};
```

**Method B: Sequential Merge (if no index conflicts)**

```bash
git merge jlpt5-da --no-edit
git merge jlpt5-desu --no-edit
# etc.
```

### Step 6: Verify and Commit

Run the full JLPT5 test suite:

```bash
cd packages/grammar && bun test src/rules/bunpro/jlpt5/
```

**Expected output: `XXX pass, Y skip, 0 fail`**

**CRITICAL: If there are ANY failures (>0 fail), you MUST NOT proceed.** Go to Step 6a below to fix failures before committing.

If all tests pass (0 fail), commit the merge:

```bash
git add -A packages/grammar/src/rules/bunpro/jlpt5/
git commit -m "Merge JLPT5 grammar rules: Batch N (X new rules)"
```

### Step 6a: Fix Failing Tests (MANDATORY if failures exist)

**Failing test cases are NEVER acceptable.** The orchestrator must spawn fix agents until all failures are resolved.

**For each failing rule**, spawn a fix agent:

```
You are fixing a failing Japanese grammar rule in the ichiran-node project.

**Your task:** Fix the **{RULE}** rule which currently has N failing tests.

**Context:**
- Working directory: /home/tiger/ichiran-node (on branch main)
- The rule at packages/grammar/src/rules/bunpro/jlpt5/{RULE}.ts has test failures
- Run tests to see failures: bun test packages/grammar/src/rules/bunpro/jlpt5/{RULE}.test.ts

**Process:**
1. Run the test to see specific failures
2. Analyze the failing sentences with engine.analyze() to understand why they fail
3. Modify the rule file to fix the pattern matching
4. Re-run tests until all pass (0 fail)
5. Commit: "Fix {RULE}: resolve N test failures"

**Important:**
- Read the current rule and test file first
- Use engine.analyze() on failing sentences to understand the parse
- DO NOT simply move failures to skipPositives - they must be fixed
- All tests must pass before you report completion

Report back with final test results showing 0 failures.
```

**Continue spawning fix agents until test output shows: `XXX pass, Y skip, 0 fail`**

**Only then proceed to Step 7.**

### Step 7: Review and Fix Skips (CRITICAL)

**IMPORTANT:** Agents frequently and incorrectly skip test cases. ALL skips must be verified with the checklist below.

After running tests, you'll see output like: `XXX pass, Y skip, 0 fail`

## SKIPPOSITIVES LEGITIMACY CHECKLIST

**FOR EVERY SKIPPED TEST CASE, the agent/orchestrator MUST complete this checklist in order:**

### Checklist Item 1: Is the sentence actually a different grammar point?

**Question:** Does this sentence belong to a completely different grammar rule that exists elsewhere in Bunpro?

**How to verify:**
1. Read the sentence carefully
2. Identify the grammatical pattern being used
3. Search packages/grammar/data/bunpro/JLPT5/ for that specific pattern
4. If a separate grammar JSON file exists for this pattern → **LEGITIMATE SKIP**

**Examples of legitimate skips:**
- A 「だが」 sentence in a 「が (conjunction)」 rule → skip if it's actually 「が (particle)」
- A 「ている」 progressive form in a 「ている」 resultative state rule → skip if separate rule exists

**If YES → Document in skipPositives comment: "Different grammar point: [pattern name]"**

**If NO → Proceed to Checklist Item 2**

---

### Checklist Item 2: Does the grammar point's data include BOTH forms?

**Question:** Does the JSON file at `packages/grammar/data/bunpro/JLPT5/{RULE}.json` contain examples of BOTH the casual AND polite (or other) forms?

**How to verify:**
```bash
# Check the grammar point description
cat packages/grammar/data/bunpro/JLPT5/{RULE}.json | jq '.nuance_translation, .meaning, .polite_structure'
```

**Look for these phrases in nuance_translation/meaning:**
- "in either its **polite or casual variant**"
- "casual or polite"
- "both ... and ..."
- Examples showing both ～ない AND ～ません
- Examples showing both ～だ AND です

**If the grammar point covers BOTH forms → ILLEGITIMATE SKIP. The rule MUST handle both with `r.either()`.**

**If the grammar point only covers ONE form (e.g., only casual, only negative, etc.) → LEGITIMATE SKIP**

**If YES (covers both) → Spawn a fix agent. DO NOT skip.**

**If NO → Proceed to Checklist Item 3**

---

### Checklist Item 3: Is this a GiNZA parser limitation?

**Question:** Does GiNZA incorrectly tag this sentence's POS/lemma/deprel in a way that cannot be fixed?

**How to verify:**
1. Run `engine.analyze(sentence)` on the skipped sentence
2. Compare with expected parse
3. Check if the issue is:
   - Incorrect POS tagging (e.g., noun tagged as verb)
   - Incorrect lemma (e.g., different lemma than expected)
   - Missing or wrong dependency relations

**Legitimate GiNZA limitations:**
- Inconsistent POS assignment for the same word across sentences
- Known Ginza bugs with specific conjugation forms
- Incorrect particle dependencies in complex sentences

**If you can fix the rule by adjusting the pattern → NOT a GiNZA limitation. Fix the rule.**

**If YES → Document in skipPositives comment with specific analysis: "GiNZA limitation: [explanation]"**

**If NO → Proceed to Checklist Item 4**

---

### Checklist Item 4: Is this a data quality issue?

**Question:** Is the test data malformed, incomplete, or extracted incorrectly from Bunpro?

**Legitimate data issues:**
- Sentence contains "→" arrows (cloze format teaching material)
- Sentence is incomplete (truncated)
- Sentence has obvious typos or encoding issues
- Duplicate entries
- Mixed Japanese/English inappropriately

**If YES → Document in skipPositives comment: "Data quality issue: [explanation]"**

**If NO → Proceed to Checklist Item 5**

---

### Checklist Item 5: Is this a different grammatical structure?

**Question:** Does the sentence use a grammatically distinct structure that this rule does not cover?

**How to verify:**
1. Analyze the grammatical structure of the sentence
2. Compare with the rule's intended pattern
3. Is it a fundamentally different construction?

**Examples:**
- ある→ない where "ない" is an i-adjective (POS=ADJ), not an auxiliary verb
- Potential form vs non-potential form (different conjugation class)
- Transitive vs intransitive verb pairs

**If YES → Document in skipPositives comment: "Different structure: [explanation]"**

**If NO → ILLEGITIMATE SKIP. Fix the rule.**

---

### FINAL CHECKLIST SUMMARY

Before committing any skipPositive, you must be able to answer YES to exactly ONE of these:

- [ ] **Different grammar point:** A separate Bunpro grammar rule exists for this pattern
- [ ] **Grammar scope:** The grammar point's JSON data does NOT include this form (verified by checking the JSON)
- [ ] **GiNZA limitation:** Parser incorrectly tags in a way that cannot be worked around
- [ ] **Data bug:** Malformed/invalid test data
- [ ] **Different structure:** Fundamentally different grammatical construction

**If you cannot check ANY of the above boxes → ILLEGITIMATE SKIP. Fix the rule instead.**

---

## Step 7a: Audit All Skips

For each rule with skipPositives:

```bash
# Read the test file and examine each skip
head -100 packages/grammar/src/rules/bunpro/jlpt5/{RULE}.test.ts | grep -A 20 "skipPositives"
```

**Create a spreadsheet or table documenting:**

| Sentence | Skip Reason | Checklist Item Passed? | Legitimate? | Action |
|----------|-------------|------------------------|-------------|--------|
| 「 sentence 」 | "polite form" | Item 2: NO (grammar covers both) | NO | Fix rule |
| 「 sentence 」 | "GiNZA bug" | Item 3: YES (verified with analyze) | YES | Keep |

**For each ILLEGITIMATE skip, spawn a fix agent.**

---

## Step 7b: Spawn Fix Agents for Illegitimate Skips

For each rule with illegitimate skips:

```
You are fixing illegitimately skipped tests in a Japanese grammar rule.

**Your task:** Fix the **{RULE}** rule to handle N currently-skipped sentences.

**Context:**
- Working directory: /home/tiger/ichiran-node (on branch main)
- These N sentences are in skipPositives but SHOULD NOT BE:
  {LIST_SENTENCES}
- They were skipped for: "{CURRENT_REASON}"
- This reason fails the skipPositives checklist because: {EXPLANATION}

**Required action:** {SPECIFIC_ACTION}
- Example: "Add r.either() branch for polite forms using lemma='ます'"
- Example: "Adjust pattern to capture this specific conjugation"

**Files to modify:**
1. **Rule file:** packages/grammar/src/rules/bunpro/jlpt5/{RULE}.ts
2. **Test file:** packages/grammar/src/rules/bunpro/jlpt5/{RULE}.test.ts - remove from skipPositives

**Process:**
1. Read current rule and test file
2. Run engine.analyze() on each skipped sentence
3. Modify rule to capture these cases
4. Remove from skipPositives
5. Run tests until all pass
6. Commit: "Fix {RULE}: handle {PATTERN} - remove N illegitimate skips"

**DO NOT:**
- Add new skipPositives to hide failures
- Skip without running the checklist first

Report back with test results.
```

---

## Step 7c: Re-verify All Tests

```bash
cd packages/grammar && bun test src/rules/bunpro/jlpt5/
```

**Expected outcome:** Skip count should decrease, pass count should increase. If skips remain, verify each one passed the checklist.

---

## Common Illegitimate Skip Patterns (DO NOT DO THIS)

| Illegitimate Reason | Why It's Wrong | Correct Action |
|---------------------|----------------|----------------|
| "Polite form" | Grammar covers both casual+polite | Add `r.either()` branch |
| "Separate rule" | No separate rule exists | Handle in current rule |
| "Different register" | Register not a distinction | Handle both variants |
| "Too complex" | Complexity is not a valid reason | Fix the pattern |
| "GiNZA issue" (unverified) | Not actually a GiNZA bug | Fix the rule |
| "Later" | Procrastination | Fix now |

---

## Legitimate Skip Examples (Reference)

| Sentence | Reason | Checklist Item |
|----------|--------|----------------|
| "これ→penです" | Contains "→" (teaching format) | Item 4: Data quality |
| "ある" (where ない is ADJ) | ある→ない is adjective, not auxiliary | Item 5: Different structure |
| "行きます" (in ru-verb rule) | Ru-verb is ichidan, not godan | Item 1: Different grammar point |
| "[inconsistent POS]" | GiNZA tags noun as verb randomly | Item 3: GiNZA limitation |

### Step 8: Clean Up Worktrees

```bash
git worktree prune
git worktree remove --force /tmp/jlpt5-1
git worktree remove --force /tmp/jlpt5-2
# ... etc.

# Verify only main remains
git worktree list
```

## Rule-Specific Implementation Notes

### Particles (case markers)
- Use `r.particle({ text: 'X', dep: 'case' }, 'name')`
- Use `r.caseMarker(noun, particle)` for attachment
- Consider `r.objectOf()` for を, `r.not()` for exclusions

### Copulas (だ, です)
- Noun + copula: `dep='cop'`
- Adjective + copula: `dep='aux'` (politeness marker)
- Use `r.either()` for different patterns
- Exclude i-adjectives with `conjugationClass` check

### Sentence particles (よ, ね)
- `pos='PART'`, `dep='mark'` or `dep='discourse'`
- Must be sentence-final (followed by PUNCT)
- Use `r.inOrder(..., 1)` for adjacency

### Conjunctions (が, けど, etc.)
- `dep='dep'` or `dep='cc'` distinguishes from case markers
- Add negative tests for particle usages

### Auxiliaries (ている, てある, etc.)
- Use `r.aux()` with `lemma` constraint
- Use `r.auxOf(verb, aux)` for attachment
- Consider `inflectionForm` for specific conjugations

## Common Pitfalls

1. **Merge conflicts in index.ts:** Each branch adds its import. Use copy method instead.
2. **File permissions:** Files copied from worktrees may have wrong permissions. Fix with `chmod 644`.
3. **Missing ね file:** If implementing in main worktree, the file may not be in the worktree. Copy from branch with `git show`.
4. **Overcapture:** Always add negative tests for similar but different grammars.
5. **GiNZA inconsistencies:** Document parsing limitations with `skipPositives` and detailed analysis.
6. **FAILING TESTS ARE UNACCEPTABLE:** Never commit with failing tests. Spawn fix agents (Step 6a) and iterate until 0 failures. Moving tests to skipPositives to hide failures is prohibited.
7. **ILLEGITIMATE SKIPS ARE UNACCEPTABLE:** Before adding any skipPositive, you MUST complete the 5-item checklist in Step 7. Common illegitimate skips:
   - "Polite form" when grammar point covers both casual+polite
   - "Separate rule" when no separate rule exists
   - "Different register" as a standalone reason
   - Any skip without verifying the JSON data first

## Quick Commands Reference

```bash
# List available grammars
ls packages/grammar/data/bunpro/JLPT5/

# Check implemented rules
ls packages/grammar/src/rules/bunpro/jlpt5/*.ts

# Run JLPT5 tests
cd packages/grammar && bun test src/rules/bunpro/jlpt5/

# Create worktree
git worktree add /tmp/jlpt5-1 -b jlpt5-rule-name

# List worktrees
git worktree list

# Remove worktree
git worktree remove --force /tmp/jlpt5-1

# Prune stale worktrees
git worktree prune

# Git status
git status

# Commit
git add -A && git commit -m "message"
```

## Example: Complete Batch 1 Execution

```bash
# 1. Create worktrees
git worktree add /tmp/jlpt5-1 -b jlpt5-da
git worktree add /tmp/jlpt5-2 -b jlpt5-desu
# ... (10 total)

# 2. Spawn 10 subagents (one Task call with 10 tool uses)
# (Each agent implements their rule independently)

# 3. Copy files to main
git checkout main
cp /tmp/jlpt5-*/packages/grammar/src/rules/bunpro/jlpt5/*.ts \
   packages/grammar/src/rules/bunpro/jlpt5/

# 4. Update index.ts manually (add all imports and rules)

# 5. Test
cd packages/grammar && bun test src/rules/bunpro/jlpt5/

# 6. Commit
git add -A packages/grammar/src/rules/bunpro/jlpt5/
git commit -m "Merge JLPT5 grammar rules: Batch 1 (10 new rules)"

# 7. Cleanup
git worktree prune
git worktree remove --force /tmp/jlpt5-*
```

## Next Batches

After completing a batch, identify the next 10 rules and repeat. Remaining JLPT5 grammars include:
- も (also particle)
- の (possessive/nominalizer)
- な (na-adjective marker)
- だれ/どこ/どれ/どの (question words)
- これ/それ/あれ (demonstratives)
- る-Verbs / う-Verbs (verb classes)
- だった・でした (past copula)
- じゃない / じゃなかった (negation)
- negative-い-adjectives
- past-tense-い-adjectives
- And many more...
