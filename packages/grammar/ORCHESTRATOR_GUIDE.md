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
- When tests pass, commit: `git add -A && git commit -m "Implement JLPT5: {RULE_NAME} ({DESCRIPTION})"`

**DO NOT:**
- Modify files outside your worktree
- Push to remote
- Merge to main

Report back when complete with test results.
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

Expected output: `XXX pass, Y skip, 0 fail`

Commit the merge:

```bash
git add -A packages/grammar/src/rules/bunpro/jlpt5/
git commit -m "Merge JLPT5 grammar rules: Batch N (X new rules)"
```

### Step 7: Clean Up Worktrees

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
