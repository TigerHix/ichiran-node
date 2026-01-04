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

// Sentences that can't be matched due to Bunpro test data including unrelated examples:
//
// 1. "有名な駅は汚いです。" and "この有名な駅が汚いです。"
//    These demonstrate na-adjective + な (attributive form) modifying a noun, NOT
//    na-adjective as predicate. The い-adjective "汚い" is the predicate, not "有名".
//    Our rule correctly doesn't match because "有名" has dep=acl (not root) and
//    "な" has inflectionForm=連体形-一般 (attributive), not 終止形-一般 (terminal).
//
// 2. "今、暇か。今、ひまですか。"
//    First part "今、暇か" is casual copula-drop form (no copula). The second part
//    "今、ひまですか." should match, but the Bunpro data combines them as one test.
//    The loader doesn't split on period, so our rule sees the compound sentence.
//
const skipPositives = [
  '有名な駅は汚いです。',
  'この有名な駅が汚いです。',
  '今、暇か。今、ひまですか。',
];

describe('bunpro.jlpt5', () => {
  const engine = useSharedEngine([BUNPRO_JLPT5]);
  describeRule(rule, 'JLPT5', BUNPRO_JLPT5.id, engine.get, { negatives, skipPositives });
});
