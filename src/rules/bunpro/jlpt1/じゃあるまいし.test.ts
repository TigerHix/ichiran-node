import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './じゃあるまいし.js';
import { BUNPRO_JLPT1 } from './index.js';

const negatives = [
  // であるまい without し (incomplete pattern)
  '嘘であるまい。',
  '彼が犯人であるまいか。',
  // ではあるまい without し
  'そんなことではあるまい。',
];

describe('bunpro.jlpt1', () => {
  const engine = useSharedEngine([BUNPRO_JLPT1]);
  describeRule(rule, 'JLPT1', BUNPRO_JLPT1.id, engine.get, { negatives });
});
