import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './ことにはならない.js';
import { BUNPRO_JLPT2 } from './index.js';

// Negative test cases - things that should NOT match ことにはならない
//
// Key distinctions:
// - ことになる (JLPT3): decision/outcome without negation (it becomes that...)
// - ことになっている (JLPT2): ongoing arrangement/state (it is arranged that...)
// - ことにはならない (JLPT2): negative, denies necessary outcome (won't result in)
// - ことはない (different grammar): no need to, there's no need to...
const negatives = [
  // ことになる (JLPT3) - positive outcome (becomes that way)
  '会議は10時から始まることになる。',
  '来年も契約を更新することになりました。',
  'その結果、退社することになった。',

  // ことになっている (JLPT2) - ongoing state/arrangement
  '会議は10時から始まることになっている。',
  '来年も契約を更新することになっています。',
  '決まりによって、試合に行くことになっている。',

  // ことにする (JLPT3) - volitional decision
  '毎日運動することにする。',
  '英語を勉強することにしました。',

  // ことはない - different grammar (no need to)
  '心配することはない。',
  '急ぐことはない。',
  '彼に言うことはない。',

  // Simple になる (state change) without こと
  '春になる。',
  '彼は医者になった。',
  '部屋がきれいになっている。',

  // ことだけに (only the fact that...)
  '彼が来なかったことだけに、心配だ。',
  '試合に勝ったことだけに、悔しい。',
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
