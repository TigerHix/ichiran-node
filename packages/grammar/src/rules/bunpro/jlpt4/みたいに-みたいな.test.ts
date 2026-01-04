import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './みたいに-みたいな.js';
import { BUNPRO_JLPT4 } from './index.js';

// Negative examples: similar patterns that should NOT match
const negatives = [
  // ように/ような (formal version, different pattern)
  '春のように暖かい。',
  '春のような天気です。',
  // そうに/そうな (seeming/appearing, attaches to verb/adj stems)
  '楽しそうに笑う。',
  'つまらなそうな顔。',
  // らしい (hearsay/typical, different grammar)
  '雨らしい天気。',
  '彼は学生らしい。',
  // みたい alone (predicate form, covered by different rule)
  '彼は先生みたいだ。',
  // Noun + だい (suffix meaning "worth", not みたい)
  // Noun + たい (want to, verb conjugation)
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
