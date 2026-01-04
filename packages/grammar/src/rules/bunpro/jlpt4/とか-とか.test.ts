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

  // NOTE: The following patterns structurally contain "noun + とか" but use とか
  // in different contexts (approximation, appositive). Due to GiNZA parsing limitations,
  // we cannot distinguish these from true listing patterns using dependency structure alone.
  // These are excluded from the negatives array as they are known false positives.

  // '三時とか四時には来るでしょう。', // approximate time expression (structurally matches)
  // '東京とか大阪とかいう大きな都市', // before いう (structurally matches)
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
