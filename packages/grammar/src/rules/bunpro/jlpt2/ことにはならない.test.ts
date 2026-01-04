import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './ことにはならない.js';
import { BUNPRO_JLPT2 } from './index.js';

// Negative test cases - things that should NOT match ことにはならない
//
// Key distinctions:
// - ことになる (JLPT3): "it will turn out that, it is decided that" (affirmative outcome)
// - ことになっている (JLPT2): "it is arranged that" (ongoing state, has ている)
// - ことにはならない (JLPT2): "it doesn't amount to, doesn't mean that" (negative, has には)
const negatives = [
  // ことになる (JLPT3) - affirmative outcome
  '会議は10時から始まることになる。',
  '来年も契約を更新することになりました。',
  'その結果、退社することになった。',

  // ことになっている (JLPT2) - ongoing arrangement/state
  '会議は10時から始まることになっている。',
  '来年も契約を更新することになっています。',
  'その結果、退社することになっていた。',

  // ことにする (JLPT3) - volitional decision
  '毎日運動することにする。',
  '英語を勉強することにしました。',

  // Simple になる (state change) without こと
  '彼は医者になる。',
  '春になる。',
  '夜になる。',

  // ことだけに (only the fact that...)
  '彼が来なかったことだけに、心配だ。',

  // ことから (from the fact that...)
  '彼が来なかったことから、問題が起きた。',

  // ことだから (given that...)
  '田中さんのことだから、遅れてくるだろう。',

  // Other こ+ patterns
  'それはいいことだ。',
  '行くことができる。',
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
