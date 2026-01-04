import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './は-の一つだ.js';
import { BUNPRO_JLPT4 } from './index.js';

// Negative examples: sentences that should NOT match this pattern
const negatives = [
  // Simple Noun + の + Noun without the counter pattern
  'これは私の本です。',
  '彼は日本の学生です。',
  // の中で (within/among) - different grammar
  '三人の中で一人が合格した。',
  'クラスの中で彼が一番です。',
  // Noun + の + 一つ without は topic marker
  'このセットの一つだ。',
  '本の一つを読む。',
  // Simple existential "there is one" pattern
  'そこに一つあります。',
  // Counter usage without category membership meaning
  'リンゴを三つ買いました。',
  'もう一つ飲みたい。',
];

// Sentences that can't be matched due to GiNZA parsing limitations:
//
// ANALYSIS: Noun + は/も + NounPhrase + の + [counter] + だ/です
//
// The rule requires POS=NUM for the counter to distinguish from regular nouns (e.g., "私の本" vs "乗り物のひとつ").
//
// GiNZA inconsistently tags counters:
//   - ひと (from ひとつ) → NUM ✓ WORKS
//   - ひとり (one person) → NOUN ✗ INDISTINGUISHABLE from regular nouns
//   - いっぽん (one long object) → NOUN ✗ INDISTINGUISHABLE from regular nouns
//   - いっしゅ (one type) → NOUN ✗ INDISTINGUISHABLE from regular nouns
//
// When counters are tagged as NOUN instead of NUM, we can't distinguish them from regular nouns
// using only POS information. Using a looser constraint (POS=NOUN) would cause overcapture:
//   ❌ これは私の本です (possessive: this is my book)
//   ❌ 彼は日本の学生です (noun modification: he is a Japanese student)
//
// CONCLUSION: No reliable discriminator. GiNZA limitation.
const skipPositives = [
  '彼も家族のひとりだ。',          // ひとり tagged as NOUN, not NUM
  'このクレヨンはこのセットのいっぽんだ。',  // いっぽん tagged as NOUN, not NUM
  'トマトはフルーツのいっしゅだ。',  // いっしゅ tagged as NOUN, not NUM
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives, skipPositives });
});
