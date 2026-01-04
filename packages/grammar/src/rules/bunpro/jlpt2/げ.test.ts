import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './げ.js';
import { BUNPRO_JLPT2 } from './index.js';

// False positives: sentences that should NOT match
const negatives = [
  // げ (particle/sound effects that are NOT the suffix)
  'げげげ！', // sound of laughter

  // 下 (げ - "down/under") as unrelated reading
  '下にある。', // が (particle) + 下にある (is below)

  // 気 (き - "spirit/mind") without げ suffix
  '気がつく。', // 気がつく (notice) - different grammar
  '気になる。', // 気になる (worry about) - different grammar
  '気をつけて。', // 気をつけて (be careful) - different grammar
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
