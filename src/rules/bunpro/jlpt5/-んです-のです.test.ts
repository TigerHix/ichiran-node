import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './-んです-のです.js';
import { BUNPRO_JLPT5 } from './index.js';

const negatives = [
  // Plain です without ん/の
  '学生です。',
  '元気です。',
  // のは/のが nominalizer (not explanatory の)
  '走るのは楽しい。',
  '食べるのが好きです。',
  // ん as part of word, not explanatory
  '本を読んでいる。',
  'みかんです。',
  // のだ without です
  '行くのだ。',
];

describe('bunpro.jlpt5', () => {
  const engine = useSharedEngine([BUNPRO_JLPT5]);
  describeRule(rule, 'JLPT5', BUNPRO_JLPT5.id, engine.get, { negatives });
});
