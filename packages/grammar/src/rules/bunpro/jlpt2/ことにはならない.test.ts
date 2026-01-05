import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './ことにはならない.js';
import { BUNPRO_JLPT2 } from './index.js';

// Negative test cases - things that should NOT match ことにはならない
//
// Key distinctions:
// - ことになる (JLPT3): positive outcome/arrangement (will happen)
// - ことになっている (JLPT2): ongoing state of being arranged (progressive)
// - ことにはならない: negative - denies that something necessarily follows
// - ことになる (positive): 会議は10時から始まることになる。
// - ことにはならない (negative): 勉強したことにはならない。
const negatives = [
  // ことになる (JLPT3) - positive outcome
  '会議は10時から始まることになる。',
  '来年も契約を更新することになりました。',
  'その結果、退社することになった。',

  // ことになっている (JLPT2) - ongoing arrangement/progressive
  '会議は10時から始まることになっている。',
  '来年も契約を更新することになっています。',
  'その結果、退社することになっていた。',

  // ことにする (JLPT3) - volitional decision
  '毎日運動することにする。',
  '英語を勉強することにしました。',

  // Simple になっている (state change) without こと
  'ドアが開いている。',
  '部屋がきれいになっている。',
  '彼は医者になっている。',

  // ことだけに (only the fact that...)
  '彼が来なかったことだけに、心配だ。',

  // ことになんて (more colloquial, different structure)
  'そんなことになんてならない。',
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
