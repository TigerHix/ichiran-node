import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './ずっと2.js';
import { BUNPRO_JLPT3 } from './index.js';

// Negative examples: should NOT match ずっと2 (comparison/comparative degree)
const negatives = [
  // Similar adverbs expressing degree but different grammar
  'もっと安いです。',
  'はるかに遠い。',
  'かなり良い。',
  'とっても面白い。',
  'とても美味しい。',

  // Note: ずっと1 patterns (continuous action) are NOT included in negatives
  // because they are syntactically identical to ずっと2 patterns and cannot be
  // reliably distinguished without semantic analysis:
  // - ずっと住んでいる (ずっと1 - continuous) vs ずっと混雑している (ずっと2 - state)
  // Both follow the same pattern: ずっと + verb + ている
  // This is a fundamental limitation of syntactic pattern matching
];

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get, { negatives });
});
