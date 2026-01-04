import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './のは.js';
import { BUNPRO_JLPT5 } from './index.js';

// Negative tests: sentences that should NOT match this rule
const negatives = [
  // Noun + のは (possessive/attribute, not nominalizer)
  '私の本はここにあります。',
  '日本の車は高いです。',
  '彼女の写真はきれいです。',
  // Verb + のを (object marker)
  '泳ぐのをやめた。',
  '勉強するのを忘れた。',
  // Simple の particle without following particle
  'これは私のです。',
  'それは彼のです。',
  // Adjective + のは (different rule - adjective-の-は)
  '高いのは買わない。',
  '赤いのはいい。',
];

describe('bunpro.jlpt5', () => {
  const engine = useSharedEngine([BUNPRO_JLPT5]);
  describeRule(rule, 'JLPT5', BUNPRO_JLPT5.id, engine.get, { negatives });
});
