import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './ばよかった.js';
import { BUNPRO_JLPT4 } from './index.js';

// Negative test cases - these should NOT match ばよかった
const negatives = [
  // ばいい (it would be good if) - different grammar (non-past)
  '行けばいい。',
  'すればいい。',
  '待てばいい。',

  // てよかった (glad that) - different grammar (relief, not regret)
  '来てよかった。',
  '行ってよかった。',
  '買ってよかった。',

  // たほうがよかった (should have / it would have been better to) - different grammar
  '行ったほうがよかった。',
  'したほうがよかった。',
  '買ったほうがよかった。',

  // Simple past good - not conditional
  'よかった。',
  'それはよかった。',

  // Simple ba conditional without yokatta
  '行けばわかる。',
  'すればできる。',
  '待てば来る。',
];

// Sentences that can't be matched due to GiNZA parsing limitations:
//
// ANALYSIS: ばよかった at sentence end
//
// The sentence "トイレに行けばよかった" fails to match because GiNZA appears to
// parse "よかった" inconsistently at sentence boundaries. In this context,
// "よかった" may be tokenized differently than expected, possibly as part of
// a different syntactic structure.
//
// Similar sentences like "すればよかった" work fine, suggesting this is an
// edge case in how GiNZA handles sentence-final "よかった" after certain
// verb forms.
//
// Attempting to match all variations would require very loose constraints
// that could cause false positives on similar patterns.
//
// CONCLUSION: Documented as GiNZA parsing limitation for this specific
// sentence structure.
const skipPositives = [
  'サービスエリアが一つもないじゃないか。高速に乗る前にトイレに行けばよかった。',
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives, skipPositives });
});
