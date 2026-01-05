import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './げ.js';
import { BUNPRO_JLPT2 } from './index.js';

// Negative test cases - sentences that should NOT match the げ grammar rule
const negatives = [
  // そう (seems/looks) - different grammar (less subjective)
  '彼は悲しそうです。',
  '雨が降りそうです。',

  // らしい (typical of) - different grammar
  '彼は学生らしい。',
  '子供らしい遊び。',

  // っぽい (-ish/-like) - different grammar (innate traits)
  '彼は子供っぽい。',
  '黒っぽい服を着ている。',

  // み (abstract noun suffix) - different grammar
  '山の深さがすごい。',
  'この美しさが好きだ。',

  // ぎみ (slight tendency) - different grammar
  '少し疲れ気味です。',
  '風邪気味だ。',

  // Just け (not げ) - different character
  '竹がたくさんある。',
  '気をつけてください。',

  // Adjectives ending in げ but not using the suffix
  // (very rare, most げ words use this suffix)
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
