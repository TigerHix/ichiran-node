import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './おまけに.js';
import { BUNPRO_JLPT2 } from './index.js';

const negatives = [
  // Other conjunctions that should NOT match
  'それに、今日は忙しい。', // それに is a different conjunction
  'その上、雨も降っている。', // その上 is a different conjunction
  'さらに、問題が発生した。', // さらに is a different conjunction
  'しかも、値段も安い。', // しかも is a different conjunction
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
