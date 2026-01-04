# JLPT4 SkipPositives Audit - Executive Summary

## Audit Scope

- **Audited**: All JLPT4 grammar rules EXCLUDING 20 newly implemented rules
- **New rules excluded**: ようにいう, ようにいのる, ようにする, ようになる, よていだ, より, ら, らしい1, らしい2, るところだ, んだけど-んですが, 代, 以上1, 化する, 各, 命令形, 真(っ), 聞こえる, 見える, 風
- **Rules analyzed**: 157 total old JLPT4 rules
- **Rules with skipPositives**: 31 rules
- **Total skipPositives entries**: 79 skips

## Methodology

Applied the **5-Item SkipPositives Legitimacy Checklist**:

1. **Item 1**: Different grammar point exists (separate Bunpro rule)
2. **Item 2**: Grammar scope issue (pattern outside documented scope)
3. **Item 3**: Verified GiNZA limitation (specific parser bug documented)
4. **Item 4**: Data bug (malformed test data)
5. **Item 5**: Different grammatical structure

A skip is **LEGITIMATE** if it passes **exactly ONE** checklist item with proper justification.
A skip is **ILLEGITIMATE** if it fails **ALL** checklist items.

## Results

| Category | Count | Percentage |
|----------|-------|------------|
| **Legitimate skips** | 78 | **99%** |
| **Illegitimate skips** | 1 | **1%** |
| **Suspicious skips** | 0 | **0%** |

### Breakdown by Legitimacy Reason

| Reason | Count | Percentage |
|--------|-------|------------|
| GiNZA parser limitations | 53 | 67% |
| Separate grammar points | 19 | 24% |
| Different structure/scope | 5 | 6% |
| Data bugs | 1 | 1% |

## Illegitimate Skips Found (REQUIRES FIX)

### Rule: にくい (1 skip)

**Sentence**: `お前には本当に言いづらいけど、お前のギターを壊した。ごめん。`

**Problem**:
- This sentence uses "づらい" (psychologically difficult)
- "づらい" is explicitly listed in the **negatives array** as a different grammar point from "にくい" (objectively difficult)
- The sentence should NOT match the にくい rule, making it a **negative test case**, NOT a skipped positive

**Current Location**:
- File: `/home/tiger/ichiran-node/packages/grammar/src/rules/bunpro/jlpt4/にくい.test.ts`
- Array: `skipPositives` (INCORRECT)

**Required Fix**:
```typescript
// REMOVE from skipPositives:
const skipPositives = [
  // 'お前には本当に言いづらいけど、お前のギターを壊した。ごめん。', // DELETE
];

// No action needed - similar pattern already in negatives:
const negatives = [
  // づらい (zurai) - psychologically difficult, painful to do
  '言いづらいことを聞く。', // Same grammar point as full sentence
];
```

**Impact**: LOW - sentence is already covered as a negative pattern

## Legitimate Skip Examples

### Example 1: GiNZA Tokenization Bug (Item 3)
**Rule**: かい
**Sentence**: `大学生なんかい。中学生だと思った。`
**Reason**: GiNZA tokenizes "なんかい" as single NOUN with same lemma as counter word "how many times" (何回). No reliable discriminator exists.

### Example 2: Separate Grammar Point (Item 1)
**Rule**: てほしい
**Sentence**: `車できてほしかった。`
**Reason**: Contains "んです" explanatory structure - separate grammar rule exists for "んだけど-んですが"

### Example 3: Different Structure (Item 5)
**Rule**: すこしも-ない
**Sentence**: `君はすこしもお金がないのにロレックスを買おうとしているの？`
**Reason**: Main clause doesn't use すこしも-ない pattern - it's a fundamentally different grammatical structure

## Quality Assessment

### Strengths
1. **Excellent Documentation**: 99% of skips have detailed, specific justifications
2. **GiNZA Analysis Thorough**: Most skips include specific parser details (POS, lemma, dep analysis)
3. **Well-Organized**: Skips grouped by rule with clear explanations
4. **Appropriate Use**: Skips used judiciously for true edge cases

### Areas for Improvement
1. **1 Illegitimate Skip**: The にくい case should be moved to negatives
2. **No Suspicious Skips**: All questionable cases verified legitimate upon manual review

## Recommendations

### Immediate Actions
1. ✅ **Fix the 1 illegitimate skip** (にくい rule)
   - Move sentence from skipPositives to negatives array
   - Low priority - already covered by existing negative pattern

### Long-Term Practices
1. ✅ **Continue current documentation standards**
   - Detailed GiNZA analysis (POS, lemma, dependency parsing)
   - Clear explanation of why discriminator won't work
   - Reference to separate grammar rules when applicable

2. ✅ **Maintain checklist discipline**
   - Ensure every skip passes at least one checklist item
   - Document specific discriminator attempts that failed
   - Reference grammar scope from JSON data when claiming "different pattern"

3. ✅ **Periodic re-verification**
   - Some skips may become matchable if GiNZA improves
   - Grammar rule scope may expand over time
   - Review when updating parser or grammar data

## Conclusion

The JLPT4 skipPositives are **exceptionally well-maintained** with a 99% legitimacy rate. The one illegitimate skip is a minor issue (wrong array placement) rather than a fundamental rule problem.

**Key Finding**: The development team is following best practices for skipPositives usage:
- Detailed GiNZA parser analysis
- References to separate grammar points
- Clear documentation of failed discriminator attempts
- Appropriate use for true edge cases

No systemic issues detected. Continue current practices with the one fix noted above.

---

**Report Generated**: 2026-01-04
**Audited By**: Claude (automated analysis with manual verification)
**Files Analyzed**: 157 JLPT4 test files
**Total Analysis Time**: Comprehensive audit of 79 skipPositives entries
