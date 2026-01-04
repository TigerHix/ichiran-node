import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './ように-ような.js';
import { BUNPRO_JLPT4 } from './index.js';

// Negative examples: similar patterns that should NOT match
const negatives = [
  // みたいに/みたいな (casual version, different lemma)
  'サンダルみたいに履きやすい。',
  'お金持ちの人みたいな生活。',
  // そうに/そうな (seeming/appearing, attaches to verb/adj stems)
  '楽しそうに笑う。',
  'つまらなそうな顔。',
  '雨が降りそうな天気。',
  // ようだ (sentence-final form, different grammar)
  '雨が降るようだ。',
  // らしい (hearsay/typical, different grammar)
  '雨らしい天気。',
  '彼は学生らしい。',
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
