import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './ざるを得ない.js';
import { BUNPRO_JLPT2 } from './index.js';

// Negative test cases - sentences that should NOT match the ざるを得ない grammar rule
const negatives = [
  // を得る (wo eru) - "to gain, to acquire" (different grammar)
  // Here を is an object marker (dep=case), not part of fixed expression
  '新たな知識を得る。',
  '経験を得ることは大切だ。',
  '彼の信頼を得たい。',

  // ざる alone - classical negative attributive form (without を得ない)
  // This is the standalone use of the classical auxiliary
  '知られざる名作。',
  '愛されざる者。',

  // Simple negative forms (ない) without the ざるを得ない pattern
  '買わない。',
  'しない。',
  '行かない。',

  // 得ない (enai) - "cannot, unable to" (without ざるを)
  'あり得ない話だ。',
  '決してあり得ない。',

  // Similar but different grammar patterns
  // ざるをえない (hiragana variation - same grammar, but testing orthography)
  // Note: This should match with our rule since we only check text='ざる'

  // こざるを得ない (kuru → ko) - this SHOULD match, not a negative
  // Just ensuring the rule handles irregular verbs correctly

  // Verb + ざる without the full pattern
  '認めざるを得ぬ。', // Older form ending in ぬ instead of ない
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
