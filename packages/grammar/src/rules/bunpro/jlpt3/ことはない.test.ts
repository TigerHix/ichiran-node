import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './ことはない.js';
import { BUNPRO_JLPT3 } from './index.js';

// Negative tests: similar patterns that should NOT match
const negatives = [
  // たことがない (JLPT5 - past experience "have never done")
  // This uses verb in past tense (た-form) + ことがない = "have never done before"
  // Our rule only matches non-past forms with は particle
  '日本に行ったことがない。',
  'このラーメンは食べたことがないと思う。',
  '寿司を食べたことがありません。',

  // ことがある (JLPT3 - "sometimes do" - opposite meaning)
  // Uses が particle instead of は
  'この馬は人を蹴ることがある。',
  'たまに楽しいことがある。',
  '大変なこともある。',

  // ことになる (JLPT3 - "it is decided that")
  '来月日本に行くことになった。',

  // ことにする (JLPT3 - "decide to")
  '毎日運動することにしました。',

  // ことだ (JLPT2 - advice "should")
  '健康のためには運動することだ。',

  // Simple こと + は without ない (incomplete pattern)
  'これは大切なことです。',
  '私のことは忘れてください。',

  // 〜ない (plain negation without こと nominalizer)
  '彼は来ない。',
  '私は行かない。',

  // Different grammar with はない (noun + wa nai, not nominalized verb)
  'この町には駅はない。',
  'お金はないが時間はある。',
];

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get, { negatives });
});
