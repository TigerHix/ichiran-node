import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './てもらう.js';
import { BUNPRO_JLPT4 } from './index.js';

// Negative examples: sentences that should NOT match てもらう
const negatives = [
  // あげる as standalone verb meaning "to give"
  '彼にプレゼントをもらう。',
  '妹にお菓子をもらった。',
  // てあげる (doing something for someone else - different direction)
  '彼に本を貸してあげた。',
  '友達に教えてあげましょう。',
  // てくれる (someone does something for me - different subject)
  '彼が私に本を貸してくれた。',
  '友達が教えてくれました。',
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
