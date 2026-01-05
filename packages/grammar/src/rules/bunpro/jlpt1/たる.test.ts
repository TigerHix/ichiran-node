import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './たる.js';
import { BUNPRO_JLPT1 } from './index.js';

// Negative examples: sentences that should NOT match
const negatives = [
  // 足る (verb "to be sufficient") - different word
  'この条件で足ります。',
  '信頼に足る友人です。',

  // だ (copula) - different form
  '彼は学生だ。',
  'これは素晴らしいことです。',

  // である (formal copula) - different form
  '彼は学生である。',
  'それは真実である。',

  // として (as/for) - different grammar
  '学生として勉強する。',
  '友人として接する。',

  // たり (tari-form) - different auxiliary
  '行ったり来たりする。',
  '読んだり書いたりする。',

  // 達 (tachi - plural marker) - different word
  '子供たちが遊んでいる。',
  '私たちの学校です。',

  // たり (classical copula stem) - wrong form
  // Note: This is a valid classical form, but not the pattern we're matching
  // Our rule specifically matches the attributive form たる
];

describe('bunpro.jlpt1', () => {
  const engine = useSharedEngine([BUNPRO_JLPT1]);
  describeRule(rule, 'JLPT1', BUNPRO_JLPT1.id, engine.get, { negatives });
});
