import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './おおよそ.js';
import { BUNPRO_JLPT2 } from './index.js';

// Negative examples: similar adverbs that should NOT match
const negatives = [
  // Similar "approximately" adverbs (different grammar points)
  // だいたい - "mostly/roughly" (JLPT4)
  'だいたい百人来た。',
  'だいたい理解した。',
  'だいたいの人が賛成している。',

  // ほぼ - "almost/nearly"
  'ほぼ完成している。',
  'ほぼ百人参加した。',
  'ほぼ毎日行っている。',

  // あらかじめ - "beforehand/in advance"
  'あらかじめ準備しておく。',

  // かつて - "once/formerly"
  'かつてこの場所に住んでいた。',

  // すべて - "all/everything"
  'すべての人が集まった。',
  'すべて準備できた。',

  // ことごとく - "entirely/without exception"
  'ことごとく失敗した。',

  // すでに - "already"
  'すでに知っている。',

  // まさに - "just/exactly"
  'まさにその通りです。',

  // わずかに - "slightly/barely"
  'わずかに残っている。',

  // たっぷり - "plentifully/fully"
  'たっぷり時間がある。',

  // たった - "only/merely"
  'たった百円だ。',

  // せいぜい - "at most/at best"
  'せいぜい百人だ。',

  // 少なくとも - "at least"
  '少なくとも百人はいる。',

  // 多くとも - "at most" (literary)
  '多くとも百人だろう。',
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
