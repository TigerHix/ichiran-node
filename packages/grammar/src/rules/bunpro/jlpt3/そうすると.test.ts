import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './そうすると.js';
import { BUNPRO_JLPT3 } from './index.js';

// False positives: sentences that should NOT match
const negatives = [
  // そう as "so" in different contexts (not the conjunction そうすると)
  'そう、そうだと思います。',
  // すると as conditional in mid-sentence (not sentence-initial)
  '勉強すると成績が上がる。',
  '早く起きると間に合う。',
  // と as quotative particle
  '彼は来ると言った。',
  // そうする as volitional + verb (different grammar)
  'そうする人が多いです。',
];

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get, { negatives });
});
