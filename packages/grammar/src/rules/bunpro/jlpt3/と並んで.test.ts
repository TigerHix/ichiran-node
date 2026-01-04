import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './と並んで.js';
import { BUNPRO_JLPT3 } from './index.js';

const negatives = [
  // Simple と particle usage (quoting, conjunction, etc.)
  // "と言う" (to say) - different grammar
  '彼は行くと言いました。',
  'これは「猫」という動物です。',

  // と as quotative particle with verb
  '田中さんは来ると言っています。',

  // に並んで (ni narande) - different particle, different meaning
  // NOT the grammar pattern we're matching
  '彼は私に並んで歩いた。',

  // と as simple "and/with" without comparison meaning
  'リンゴとバナナを買った。',

  // Related but different grammar patterns:
  // と同じで (to onaji de) - "same as" (different grammar point)
  // と共に (to tomo ni) - "together with" (different grammar point)
  // に沿って (ni sotte) - "in accordance with" (different grammar point)
];

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get, { negatives });
});
