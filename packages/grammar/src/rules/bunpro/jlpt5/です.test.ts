import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './です.js';
import { BUNPRO_JLPT5 } from './index.js';

// Sentences that should NOT match the です rule.
// False positives would include:
// - でございます (very polite copula - different grammar)
// - です used in different grammatical constructions
const negatives = [
  // でございます (more polite form - different grammar point)
  // 'こちらでございます。',  // This might not be in test data, but illustrates the pattern
  // Examples where です is part of a different grammar pattern:
  // ～んです (explanatory - different grammar)
  'どうして遅れたんですか。',  // explanatory no + desu
  'そうなんです。',             // explanatory no + desu
];

describe('bunpro.jlpt5', () => {
  const engine = useSharedEngine([BUNPRO_JLPT5]);
  describeRule(rule, 'JLPT5', BUNPRO_JLPT5.id, engine.get, { negatives });
});
