import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './である.js';
import { BUNPRO_JLPT3 } from './index.js';

// Negative examples: casual copula だ and polite です should NOT match
const negatives = [
  // Casual copula だ (different grammar point - JLPT5)
  '彼は学生だ。',
  '彼はパイロットだ。',
  '今日は良い天気だ。',
  '吾輩は猫だ。',
  'とても面白いドラマだ。',

  // Polite copula です (different grammar point - JLPT5)
  '彼は学生です。',
  '彼はパイロットです。',
  '今日は良い天気です。',
  'とても面白いドラマです。',

  // Past tense casual/polite forms
  '彼は学生だった。',
  '彼は学生でした。',

  // で as instrumental/locative particle (different grammar)
  '鉛筆で書く。',
  '東京で働く。',
  '日本で買う。',

  // ではない (negative form - different grammar)
  '彼は学生ではない。',
  'これは本ではない。',

  // Negation じゃない (different grammar point)
  '彼は学生じゃない。',
  'これは本じゃない。',

  // Verb + てある (transitive verb + state - different grammar)
  '窓が開けてある。',
  '準備してある。',

  // でございます (very polite copula - different grammar)
  // Note: This could potentially match, but it's a different level/register
];

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get, { negatives });
});
