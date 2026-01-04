import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './だった-でした.js';
import { BUNPRO_JLPT5 } from './index.js';

// Negative examples: だった・でした should NOT match i-adjectives in past tense
// (i-adjectives have their own past tense conjugation ～かった)
const negatives = [
  // I-adjectives in past tense (～かった) - different grammar
  '大きかった。',
  '高かった。',
  '新しかった。',
  '楽しかったです。',
  '寒かったです。',
];

describe('bunpro.jlpt5', () => {
  const engine = useSharedEngine([BUNPRO_JLPT5]);
  describeRule(rule, 'JLPT5', BUNPRO_JLPT5.id, engine.get, { negatives });
});
