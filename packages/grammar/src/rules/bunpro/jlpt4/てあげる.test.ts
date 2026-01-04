import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './てあげる.js';
import { BUNPRO_JLPT4 } from './index.js';

// Negative examples: sentences that should NOT match てあげる
const negatives = [
  // あげる as standalone verb meaning "to give"
  '彼にプレゼントをあげる。',
  '妹にお菓子をあげた。',
  // てくれる (someone does something for me - different direction)
  '彼が私に本を貸してくれた。',
  '友達が教えてくれました。',
  // てもらう (I receive favor - different perspective)
  '彼に日本語を教えてもらった。',
  '田中さんに手伝ってもらいました。',
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
