import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './なにか-なにも.js';
import { BUNPRO_JLPT5 } from './index.js';

// Negative tests: sentences that should NOT match this rule.
//
// なにか/なにも are indefinite pronouns meaning "something/nothing".
// We want to avoid false positives from:
// 1. Just 何 (なに) alone without the particle
// 2. か/も particles used with other words
// 3. Other question words (だれ, どこ, etc.) which have their own rules
const negatives = [
  // 何 (なに) without particle - different grammar
  '何を食べますか。',
  '何が好きですか。',
  '何時に来ますか。',
  // か particle with other question words (covered by different rules)
  'どこか行きたい。',
  'だれか来ました。',
  // も particle with other words (inclusive も)
  '私も行きます。',
  '彼も食べる。',
  // Regular use of 何+が in questions (not なにか)
  '何が美味しいですか。',
  '何をするつもりですか。',
];

describe('bunpro.jlpt5', () => {
  const engine = useSharedEngine([BUNPRO_JLPT5]);
  describeRule(rule, 'JLPT5', BUNPRO_JLPT5.id, engine.get, { negatives });
});
