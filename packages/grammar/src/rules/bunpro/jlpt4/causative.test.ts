import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './causative.js';
import { BUNPRO_JLPT4 } from './index.js';

// Negative examples: sentences that should NOT match causative
const negatives = [
  // Passive form (different grammar - uses れる/られる)
  '彼は先生に褒められた。',
  '私は兄にケーキを食べられた。',
  // Potential form (different grammar)
  '私は日本語が話せる。',
  '彼は来られる。',
  // Causative-passive (different grammar - combines causative and passive)
  '私は母に勉強させられた。',
  '私は友達に待たされた。',
  // Volitional form (different grammar)
  'みんなで行こう。',
  '一緒に食べましょう。',
  // 〜たい (desire - different grammar)
  '私は行きたい。',
  '彼が何か食べたがっている。',
  // 〜てしまう (completion - different grammar)
  '食べてしまった。',
  '忘れてしまった。',
];

// Sentences that can't be matched due to GiNZA parsing limitations:
//
// 1. "学校に行きたくない子をいかせる。"
//    GiNZA parses "いかせる" as a single VERB token (lemma=いかせる, inflection=終止形-一般)
//    instead of verb stem + causative auxiliary. This is a GiNZA tokenization limitation.
//
// 2. "赤ちゃんはまだ自分で食べることが出来ないから、たべさせなきゃいけない。"
//    GiNZA may parse hiragana "たべ" differently from kanji "食べ", causing inconsistent
//    tokenization patterns that can't be reliably matched.
const skipPositives = [
  '学校に行きたくない子をいかせる。',
  '赤ちゃんはまだ自分で食べることが出来ないから、たべさせなきゃいけない。',
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives, skipPositives });
});
