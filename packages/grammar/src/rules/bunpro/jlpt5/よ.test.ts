import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './よ.js';
import { BUNPRO_JLPT5 } from './index.js';

// Negative test cases - sentences that should NOT match the よ rule
const negatives = [
  // よ appearing as part of words (not sentence-ending particle)
  '良い天気です。',
  '四月よふみは誰ですか。',
  '今日はよい日です。',
  '彼女はよかったと言いました。',
  // よ as locative (not in this dataset, but for completeness)
  // Most uses of よ are sentence-ending particles
];

describe('bunpro.jlpt5', () => {
  const engine = useSharedEngine([BUNPRO_JLPT5]);
  describeRule(rule, 'JLPT5', BUNPRO_JLPT5.id, engine.get, { negatives });
});
