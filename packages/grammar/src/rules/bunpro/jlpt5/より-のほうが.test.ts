import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './より-のほうが.js';
import { BUNPRO_JLPT5 } from './index.js';

// Negative test cases: sentences that should NOT match
// These look similar but are different grammar patterns
const negatives = [
  // Simple comparison without ほう
  '東京より大阪が大きい。',

  // "の方が" meaning "his side/the side" not comparison
  '彼の方が来た。',

  // "の方" as direction not comparison
  '右の方に行きました。',

  // より道 as compound noun, not comparison particle
  'より道をしました。',
];

describe('bunpro.jlpt5', () => {
  const engine = useSharedEngine([BUNPRO_JLPT5]);
  describeRule(rule, 'JLPT5', BUNPRO_JLPT5.id, engine.get, { negatives });
});
