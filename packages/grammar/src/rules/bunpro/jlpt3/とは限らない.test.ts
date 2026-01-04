import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './とは限らない.js';
import { BUNPRO_JLPT3 } from './index.js';

const negatives = [
  // に限らない (JLPT1 grammar - "not limited to")
  // Different meaning from とは限らない
  '若者に限ったことではない。',

  // 限る without negation (positive form - different grammar)
  'この店に限る。',

  // Similar but different grammar points
  // わけではない ("it's not that" - weaker nuance)
  // This would typically be matched by a different rule
];

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get, { negatives });
});
