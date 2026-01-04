import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './たがる.js';
import { BUNPRO_JLPT4 } from './index.js';

// Negative examples: sentences that should NOT match たがる
const negatives = [
  // たい (first-person desire - different grammar)
  '私は行きたい。',
  '彼が行きたいと言っている。',
  // てほしい (want someone to do something - different grammar)
  '私に見てほしい。',
  '彼氏に大事にしてほしい。',
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
