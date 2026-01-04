import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './あまり.js';
import { BUNPRO_JLPT3 } from './index.js';

const negatives = [
  // あまり-ない (JLPT4) - "not very, not much"
  // This is a DIFFERENT grammar point from JLPT3 あまり
  'あまり食べない。',
  'あまり美味しくない。',
  'あまり好きではない。',

  // あまり at end of sentence as noun (remainder/surplus)
  'まだ時間があまりある。',

  // あまりに (intensifier, different grammar)
  // Note: Some surface forms may overlap but usage differs
];

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get, { negatives });
});
