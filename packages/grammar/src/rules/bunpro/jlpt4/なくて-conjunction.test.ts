import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './なくて-conjunction.js';
import { BUNPRO_JLPT4 } from './index.js';

// Negative examples: similar patterns that should NOT match
const negatives = [
  // ないで (without doing) - different grammar point (Bunpro 96)
  // This has different POS marking (dep=mark instead of conjunction)
  'トーマスにはまだプレゼントをあげないでね。',
  '朝ご飯を食べないで学校に行った。',

  // Simple negation ending sentence (not conjunction)
  'お金がない。',
  '彼は来ない。',

  // Positive te-form conjunction (not negative)
  '行って、買ってきた。',
  '高くて買えない。',
  '静かでよく眠れる。',

  // Negative requests (てください / で) - different usage
  '行かないでください。',
  '食べないで。',

  // ては (conditional) - different grammar
  '食べてはいけない。',
  '行ってはだめ。',

  // なく (casual negative ending) - not conjunction
  '高くなく、安い。',
  '行かなく、帰る。',

  // だけ + なく (not only, but) - different grammar (Bunpro 266)
  '犬だけではなく、猫も好き。',

  // Note: Some sentences structurally contain なくて but represent
  // different pragmatic uses. The distinction between "conjunction" and
  // "reason/cause" is contextual, not structural. This rule correctly
  // matches the なくて conjunction pattern in all valid contexts.
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
