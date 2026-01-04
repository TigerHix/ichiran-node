import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './noun-まで.js';
import { BUNPRO_JLPT5 } from './index.js';

const negatives = [
  // までに - "by" (deadline), not "until" (duration)
  // Different grammar point, same particle but different usage
  // These should be handled by a separate までに rule
];

describe('bunpro.jlpt5', () => {
  const engine = useSharedEngine([BUNPRO_JLPT5]);
  describeRule(rule, 'JLPT5', BUNPRO_JLPT5.id, engine.get, { negatives });
});
