import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './ずっと2.js';
import { BUNPRO_JLPT3 } from './index.js';

// Negative examples: should NOT match ずっと2 (comparison/comparative degree)
const negatives = [
  // ずっと1 - "continuously/always" (different grammar point)
  'ずっと待っていた。',
  'ずっと好きです。',
  'ずっと一緒にいる。',
  'ずっと住んでいます。',
  'ずっと勉強している。',

  // Similar adverbs expressing degree but different grammar
  'もっと安いです。',
  'はるかに遠い。',
  'かなり良い。',
  'とっても面白い。',
  'とても美味しい。',
];

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get, { negatives });
});
