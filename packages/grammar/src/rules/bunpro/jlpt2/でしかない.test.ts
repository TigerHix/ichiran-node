import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './でしかない.js';
import { BUNPRO_JLPT2 } from './index.js';

// Negative test cases - sentences that should NOT match the でしかない grammar rule
const negatives = [
  // でない (de nai) - simple negation with copula, not the restrictive pattern
  '彼は学生ではない。',
  'それは難しい問題ではない。',
  '今日は休日ではない。',

  // しかない (shika nai) - "only X" without the copula で
  'やるしかない。',
  '待つしかない。',
  'これしかない。',

  // にすぎない (ni suginai) - similar but different grammar point
  'これは個人の意見にすぎない。',
  '彼は単なる友達にすぎない。',

  // にはかならない (ni hoka naranai) - different restrictive pattern
  '成功は努力にはかならない。',

  // Instrumental/locative で + potential verb (not the copula pattern)
  // "これでしかできない" = "can only do with this" (instrumental で + できる)
  'これでしかできない。',
  'そこでしか見られない。',
  '鉛筆でしか書けない。',

  // で + その他 (de + sonohoka) - different pattern
  'これでその他のものも作れる。',

  // Similar surface forms but different grammar
  'これはできる。',
  'あそこで行った。',
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
