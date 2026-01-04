import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './な-adjective-predicate.js';
import { BUNPRO_JLPT5 } from './index.js';

// Negative test cases: sentences that should NOT match this rule
const negatives = [
  // い-adjective predicates (different grammar)
  '寒い。',
  '寒いです。',
  '楽しい。',
  '楽しいです。',
  '高い。',
  '高いです。',

  // な-adjective modifying a noun (not predicate)
  '静かな人',
  'きれいな部屋',
  '元気な子供',
  '親切な人',
  '便利な道具',

  // Noun + だ (noun predicate, not na-adjective)
  '学生だ',
  '先生です',
  '会社員だ',

  // な-adjective past tense (different grammar rule)
  '静かだった。',
  '静かでした。',
  '元気だった。',
  '元気でした。',

  // な-adjective negative (different grammar rule)
  '静かではない。',
  '静かじゃない。',
  '静かではありません。',
  '静かじゃありません。',

  // な-adjective modifying noun with な
  '有名な駅',
  '親切な人',
  '綺麗な海',

  // Various nouns with copula
  '私は学生です。',
  '彼は会社員だ。',
];

describe('bunpro.jlpt5', () => {
  const engine = useSharedEngine([BUNPRO_JLPT5]);

  // DATA ERROR: The Bunpro data includes explanation/writup examples that are NOT
  // actual na-adjective predicates:
  // 1. "有名な駅は汚いです。" - "有名" modifies "駅" (attributive), "汚い" is the actual predicate
  // 2. "今、暇か。今、ひまですか。" - Compound sentence with copula-drop + proper form
  // These are marked as question_type="readonly" and used_in="writeups" in the source data.
  describeRule(rule, 'JLPT5', BUNPRO_JLPT5.id, engine.get, {
    negatives,
    skipPositives: [
      '有名な駅は汚いです。',
      'この有名な駅が汚いです。',
      '今、暇か。今、ひまですか。',
    ],
  });
});
