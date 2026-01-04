import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './いわゆる.js';
import { BUNPRO_JLPT2 } from './index.js';

// Negative examples: sentences that should NOT match
const negatives = [
  // 言う (to say) - different verb
  '彼はそう言いました。',
  '何と言っていますか？',

  // 言われる (passive of iu) - different conjugation
  '彼は天才だと言われている。',

  // いわば (so to speak) - slightly different nuance, not pre-nominal
  'それは、いわば賭けだ。',

  // いわく (to say / to state) - classical verb form
  'いわくつきがある。',

  // いう (to say / called) - different word
  'それを東京という。',

  // 所為 (sei - cause/because) - same kanji "所為" but different reading
  '雨の所為で遅れました。',
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
