import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './がある-noun.js';
import { BUNPRO_JLPT5 } from './index.js';

const negatives = [
  // がある at end of sentence (different grammar: simple existence)
  'ベッドは部屋にある。',
  '部屋にベッドがある。',
  '机の上に本がある。',
  'お金がある。',
  // Simple noun + がある (not modifying another noun)
  '彼には車がある。',
];

describe('bunpro.jlpt5', () => {
  const engine = useSharedEngine([BUNPRO_JLPT5]);
  describeRule(rule, 'JLPT5', BUNPRO_JLPT5.id, engine.get, { negatives });
});
