import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './くらい2.js';
import { BUNPRO_JLPT3 } from './index.js';

// Negative test cases: sentences that should NOT match this rule
const negatives = [
  // ほど - formal variant expressing degree/extent
  // Different grammar point, should be handled by separate rule
  '驚くほど静かだった。',
  '死ぬほど疲れている。',
  // など - "and so on" / "things like that"
  // Different particle with different meaning
  'リンゴやみかんなどを買った。',
  '映画など見たくない。',
];

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get, { negatives });
});
