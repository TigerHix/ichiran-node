import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './けど-だけど.js';
import { BUNPRO_JLPT5 } from './index.js';

// Negative test cases - similar forms that should NOT match
const negatives = [
  // Other conjunctions: が (formal "but" - different rule)
  '毎日走るが、運動はきらいです。',

  // Conjunction: でも (demo - "but/however" at sentence start)
  'でも、高くて買えません。',

  // Conjunction: しかし (shikashi - formal "however")
  'しかし、問題があります。',

  // Reason/cause marker: のに (noni - "even though/despite")
  '雨が降っているのに、外出します。',

  // です (desu) polite form - this rule matches casual けど/だけど only
  // Formal version would be: ですけど or ですが (different rules)
  '綺麗ですが、高いです。',
];

describe('bunpro.jlpt5', () => {
  const engine = useSharedEngine([BUNPRO_JLPT5]);
  describeRule(rule, 'JLPT5', BUNPRO_JLPT5.id, engine.get, { negatives });
});
