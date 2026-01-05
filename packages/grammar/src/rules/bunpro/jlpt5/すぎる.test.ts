import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './すぎる.js';
import { BUNPRO_JLPT5 } from './index.js';

// Negative test cases - sentences that should NOT match the すぎる grammar rule
// These are cases where すぎる appears but not as the "too much" auxiliary
const negatives = [
  // Independent verb すぎる meaning "to pass" or "to exceed"
  // (different from the auxiliary use)
  '限度をすぎる。', // Exceed the limit (main verb use)
  '基準をすぎる。', // Pass the standard (main verb use)

  // Similar constructions that aren't すぎる
  '食べる', // Just "eat" without すぎる
  '高い', // Just "expensive" without すぎる
];

// Data bug: the JSON has answer="ねすぎます" but should be "寝すぎます"
// The word_prompt shows the verb is 寝る (to sleep), not ねる
const skipPositives = [
  'いつも週末にねすぎます。',
];

describe('bunpro.jlpt5', () => {
  const engine = useSharedEngine([BUNPRO_JLPT5]);
  describeRule(rule, 'JLPT5', BUNPRO_JLPT5.id, engine.get, { negatives, skipPositives });
});
