import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './っこない.js';
import { BUNPRO_JLPT2 } from './index.js';

// Negative examples: sentences that should NOT match っこない
const negatives = [
  // Simple ない negation (not emphatic っこない)
  'できない。',
  'わからない。',
  '勝てない。',
  '読めない。',

  // そうにない (unlikely to, showing no signs of - different grammar)
  'できそうにない。',
  'わかりそうにない。',

  // わけがない (no reason to, can't be - different grammar)
  'できるわけがない。',
  'わかるわけがない。',

  // ようがない (no way to do - no method, different grammar)
  'しようがない。',
  'やりようがない。',

  // Separate っこ + ない not connected as suffix
  // (rare but possible in other contexts)
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
