import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './すくなくない.js';
import { BUNPRO_JLPT4 } from './index.js';

const negatives = [
  // 少ない (few) without negation - positive form, not double negative
  '時間が少ない。',
  'お金が少ないです。',
  '友達が少ない人',

  // 少しも～ない (not at all) - different grammar with different meaning
  '少しも面白くない。',
  '少しもわからない。',

  // 少なく (adverbial "at least") followed by different grammar
  // Note: This is actually 少なくとも (at least), not our pattern
  // '少なくとも3人はいる', // This should be handled by a different rule
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
