import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './なくて.js';
import { BUNPRO_JLPT4 } from './index.js';

const negatives = [
  // ないで - different grammar point (JLPT4: verb-ないで)
  // Used for negative requests and "without doing"
  // Structurally: text=ない + で (not text=なく + て)
  '行かないで。',
  '食べないでください。',
  '朝ご飯を食べないで学校に行った。',

  // Simple negation (ない, not なくて)
  '行かない。',
  'お金がない。',
  '時間がない。',

  // Positive te-form (て, not なくて)
  '行って、買ってきた。',
  '食べて、寝た。',
  '帰って悲しいです。', // verbて-b2 - positive te-form

  // ては (different grammar)
  '食べてはいけない。',
  '行ってはだめ。',

  // Note: じゃなくて sentences (e.g., 静かじゃなくて, 重病じゃなくて)
  // are included in the なくて test data and should match.
  // The related JLPT3 grammar point ではなくて-じゃなくて covers the same
  // pattern with a different focus/nuance explanation.
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
