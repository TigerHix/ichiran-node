import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './ら.js';
import { BUNPRO_JLPT4 } from './index.js';

const negatives = [
  // Single pronoun without ら
  '彼は学生です。',
  '私は日本語を勉強しています。',
  'これは本です。',

  // Similar but different suffixes
  '彼たちは学生です。',
  '子供たちが遊んでいます。',

  // など instead of ら
  '本などを読んでいます。',
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
