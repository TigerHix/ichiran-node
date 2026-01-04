import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './とか-とか.js';
import { BUNPRO_JLPT4 } from './index.js';

const negatives = [
  // Single とか (not listing pattern - "or something" ending)
  // Note: Many single とか uses are still grammatically "listing" but with one item,
  // so we focus on clearly different patterns

  // とか as part of other grammar (different structure)
  'これは何だとか言っていた。', // quoting pattern (different grammar)

  // とか in fixed expressions (different meaning)
  '三時とか四時には来るでしょう。', // approximate time expression
  '東京とか大阪とかいう大きな都市', // before いう (different pattern)
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
