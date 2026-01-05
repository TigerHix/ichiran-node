import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './い-adj-く-もなんともない.js';
import { BUNPRO_JLPT1 } from './index.js';

// Negative test cases: simple negation without もなんとも
const negatives = [
  // Simple i-adjective negation (ない directly after く-form)
  'これは難しくない。',
  'おいしくないです。',
  '珍しくないだろう。',
  // Similar patterns but missing the full もなんとも sequence
  '難しくもないから、問題ない。',
  'おいしくもないけど食べた。',
];

describe('bunpro.jlpt1', () => {
  const engine = useSharedEngine([BUNPRO_JLPT1]);
  describeRule(rule, 'JLPT1', BUNPRO_JLPT1.id, engine.get, { negatives });
});
