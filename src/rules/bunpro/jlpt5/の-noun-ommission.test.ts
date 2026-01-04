import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './の-noun-ommission.js';
import { BUNPRO_JLPT5 } from './index.js';

// Negative tests: sentences that should NOT match this rule
const negatives = [
  // Standard possessive: noun + の + noun (no omission)
  '私の本はこれです。',
  'たけしさんの車は新しいです。',
  '日本の寿司は美味しい。',
  // の as nominalizer for verb phrases (different grammar)
  '買うのは高い。',
  '行くのが好きです。',
  // Relative clause + noun (の is inside, not at end)
  '昨日買った本は面白い。',
  // Particle の followed by topic marker は in copula (possession pattern)
  'これは私の本です。',
  'それは田中さんのかばんです。',
];

describe('bunpro.jlpt5', () => {
  const engine = useSharedEngine([BUNPRO_JLPT5]);
  describeRule(rule, 'JLPT5', BUNPRO_JLPT5.id, engine.get, { negatives });
});
