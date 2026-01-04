import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './くらい1.js';
import { BUNPRO_JLPT5 } from './index.js';

// Negative test cases: sentences that should NOT match this rule
const negatives = [
  // ごろ/ころ (time expressions, not degree/extent)
  '８時ごろにもう帰りました。',
  '子供のころによく遊びました。',
  '１０時ごろに寝ます。',
  // Different usages of くらい that belong to different grammar points
  // (e.g., くらい2 "to the extent that" - JLPT3)
  // These would be handled by higher-level rules
];

describe('bunpro.jlpt5', () => {
  const engine = useSharedEngine([BUNPRO_JLPT5]);
  describeRule(rule, 'JLPT5', BUNPRO_JLPT5.id, engine.get, { negatives });
});
