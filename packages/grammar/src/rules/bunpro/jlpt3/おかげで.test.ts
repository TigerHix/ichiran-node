import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './おかげで.js';
import { BUNPRO_JLPT3 } from './index.js';

// Negative examples: similar patterns that should NOT match
const negatives = [
  // Instrumental で (means "using/by means of", not "thanks to")
  '鉛筆で書く。',
  '日本語で話してください。',
  '電車で行きました。',
  // Locative で (means "at/in", not "thanks to")
  '公園で遊びます。',
  '家で勉強する。',
  '図書館で本を読む。',
  // Cause marker で (but different grammar - not おかげで)
  '雨で試合が中止になった。',
  '病気で学校を休んだ。',
  // Similar forms with different kanji/meaning
  '彼は陰で文句を言っている。', // 陰 (shadow - different word)
];

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get, { negatives });
});
