import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './でしょう.js';
import { BUNPRO_JLPT5 } from './index.js';

const negatives = [
  // です (plain copula, not conjecture)
  '学生です。',
  'これは本です。',
  'いいです。',

  // だろう (casual form - different grammar)
  // Note: だろう is the casual equivalent of でしょう, treated as separate grammar
  '学生だろう。',
  '寒いだろう。',
];

describe('bunpro.jlpt5', () => {
  const engine = useSharedEngine([BUNPRO_JLPT5]);
  describeRule(rule, 'JLPT5', BUNPRO_JLPT5.id, engine.get, { negatives });
});
