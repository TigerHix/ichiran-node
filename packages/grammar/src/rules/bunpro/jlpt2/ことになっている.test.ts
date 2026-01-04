import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './ことになっている.js';
import { BUNPRO_JLPT2 } from './index.js';

// Negative test cases - things that should NOT match ことになっている
//
// Key distinctions:
// - ことになる (JLPT3): focuses on the decision/outcome itself (present/past, not progressive)
// - ことになっている (JLPT2): ongoing state of being arranged/expected (has ている)
const negatives = [
  // ことになる (JLPT3) - decision/outcome without progressive aspect
  '会議は10時から始まることになる。',
  '来年も契約を更新することになりました。',
  'その結果、退社することになった。',

  // ことにする (JLPT3) - volitional decision by speaker
  '毎日運動することにする。',
  '英語を勉強することにしました。',

  // Simple になっている (state change) without こと
  'ドアが開いている。',
  '部屋がきれいになっている。',
  '彼は医者になっている。',

  // ことだけに (only the fact that...)
  '彼が来なかったことだけに、心配だ。',
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
