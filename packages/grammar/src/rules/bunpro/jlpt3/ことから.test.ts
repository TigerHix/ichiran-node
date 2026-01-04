import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './ことから.js';
import { BUNPRO_JLPT3 } from './index.js';

// False positives: sentences with こと+から that should NOT match this pattern
const negatives = [
  // ことだから (different grammar - judgment based on characteristics)
  '田中さんのことだから、きっと来る。',
  '彼のことだから、大丈夫だろう。',
  // こと + が + から (different particle order)
  'ことがからわかる。',
  // こと + に + から (different particle order)
  'ことにはからがある。',
  // Simple だ + から (copula + because, no こと)
  'これは本だから読める。',
  '彼は学生だから勉強する。',
];

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get, { negatives });
});
