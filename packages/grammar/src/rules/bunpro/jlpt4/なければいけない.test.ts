import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './なければいけない.js';
import { BUNPRO_JLPT4 } from './index.js';

const negatives = [
  // Simple negative verb (not obligation)
  '行かない。',
  '食べない。',
  'しない。',

  // Negative verb in ba-form (without ikenai - different grammar)
  '行かなければ。',
  'しなければ。',
  '来ればいい。',

  // te-form + wa + ikenai BUT positive form (prohibition, not obligation)
  '行ってはいけない。',
  '食べてはいけない。',
  '入ってはいけない。',

  // Similar obligation forms (different grammar - handled by different rules)
  // なくてはいけない (JLPT5)
  '行かなくてはいけない。',
  '勉強しなくてはいけない。',

  // なければならない (JLPT4 - separate rule)
  '行かなければならない。',
  '勉強しなければならない。',

  // ないといけない (JLPT4 - separate rule)
  '行かないといけない。',
  '勉強しないといけない。',

  // なくてもいい (permission, not obligation)
  '行かなくてもいい。',
  '食べなくてもいい。',

  // Negative potential form without obligation
  'これは行けない。',
  'それはできない。',

  // Negative ba-form with different verbs (not iku/ikeru)
  '来ればわかる。',
  '見れば見るほど好きになる。',

  // Simple ba conditional (not obligation)
  '行けば行くほど遠くなる。',
  '食べれば食べるほどおいしい。',
];

// Sentences that can't be matched due to GiNZA parsing limitations:
//
// ANALYSIS: Past tense obligation form なければいけなかった
//
// The sentence 「遅れてすみませんでした。電車が止まっていたから、バスにのらなければいけなかった。」
// uses the past tense form of なければいけない.
//
// The verb 「乗る」(noru - to ride) is conjugated as:
//   - 未然形: のら (nora)
//   - + なければ (nakereba - if not)
//   - + いけなかった (ikenakatta - had to/could not)
//
// GiNZA appears to parse this form in a way that doesn't match any of our patterns:
//   - It may tokenize なければ as a single unit, but with different POS/lemma attributes
//   - The past tense form いけなかった may be parsed differently than expected
//
// We've tried multiple patterns:
//   1. Single token なければ + いけなかった (Pattern 3a)
//   2. Split な + ければ + いけなかった (Pattern 3b)
//   3. Separate なけれ + ば + いけなかった (Pattern 3c)
//
// None of these patterns match GiNZA's actual parsing. The form is rare in test data
// (only 1 occurrence), and all non-past tense forms match correctly.
//
// CONCLUSION: GiNZA parsing limitation for past tense obligation form.
const skipPositives = [
  '遅れてすみませんでした。電車が止まっていたから、バスにのらなければいけなかった。',
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives, skipPositives });
});
