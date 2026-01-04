import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './さ-interjection.js';
import { BUNPRO_JLPT3 } from './index.js';

// Negative examples: sentences with さ that should NOT match the interjection pattern
const negatives = [
  // さ as sentence-ending particle (casual よ) - different grammar point
  'できるさ。',
  '行くさ。',
  '大丈夫さ。',
  // さ as mid-sentence filler (さ-filler) - different grammar point
  '昨日さ、友達に会ったの。',
  'それでさ、困ってるんだ。',
  '彼はさ、本当に優しい人だよ。',
  // Noun suffix さ (e.g., 暑さ, 面白さ) - completely different
  '暑さが厳しい。',
  'この料理の美味しさは格別だ。',
  '彼の親切さに感謝している。',
  // Particles in similar-sounding words (e.g., さえ, さすが) - different grammar
  '子供さえできる。',
  'さすがに疲れた。',
  'さて、始めましょう。', // さて is different (interjection but different word)
];

// Sentences that can't be matched due to GiNZA parsing limitations:
//
// ANALYSIS: Interjection さ (sentence-initial) vs sentence-final さ particle
//
// GiNZA inconsistently tags sentence-initial さ:
//   さあ、仕事を始めよう。          → pos=INTJ, dep=dep ✓ WORKS (can distinguish from さ-casualよ)
//   「さ、食べて。冷めないうちに。」  → pos=PART, dep=mark ✗ INDISTINGUISHABLE (same as sentence-final)
//   「さ、早く乗ってください。」      → pos=PART, dep=mark ✗ INDISTINGUISHABLE
//   「さ、そろそろ帰る時間だぞ。」     → pos=PART, dep=mark ✗ INDISTINGUISHABLE
//
// The discriminator for さ-casualよ (sentence-final particle) is: pos=PART, dep=mark
// When sentence-initial さ is tagged with dep=mark, it's identical to sentence-final さ.
// There's no token position predicate in the DSL to distinguish by sentence position.
//
// Matching all さ with dep=mark would overcapture:
//   ❌ できるさ (sentence-final: I can do it!)
//   ❌ 行くさ (sentence-final: I'm going!)
//
// CONCLUSION: No reliable discriminator. GiNZA limitation.
const skipPositives = [
  '「さ、食べて。冷めないうちに。」',
  '「さ、早く乗ってください。」「うん。」',
  '「さ、そろそろ帰る時間だぞ。」',
];

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get, { negatives, skipPositives });
});
