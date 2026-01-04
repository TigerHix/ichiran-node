import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './ことになる.js';
import { BUNPRO_JLPT3 } from './index.js';

// Negative test cases - things that should NOT match ことになる
//
// Key distinction: ことになる (non-volitional) vs ことにする (volitional)
// - ことになる: "it turns out that / it was decided (by others/circumstance)"
// - ことにする: "I decide to / I make it my policy to"
const negatives = [
  // ことにする - volitional decision (speaker's choice)
  // Should NOT match because it uses する instead of なる
  '来月からダイエットすることにする。',
  '毎日運動することにしました。',
  '英語を勉強することにした。',

  // ことにしている - ongoing decision/habit
  '朝はコーヒーを飲むことにしている。',
  '健康のために野菜をたくさん食べることにしている。',

  // Note: ことになっている (JLPT2) intentionally excluded from negatives
  // It contains ことになって (te-form of ことになる) as a substring,
  // which is a valid match for this rule.

  // Simple になる (become) without こと
  '彼は医者になった。',
  '春になりました。',
  'もっと上手になる。',

  // ことだけに (only the fact that...)
  '彼が来なかったことだけに、心配だ。',
];

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get, { negatives });
});
