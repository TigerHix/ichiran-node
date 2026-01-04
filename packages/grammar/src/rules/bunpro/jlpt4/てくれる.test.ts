import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './てくれる.js';
import { BUNPRO_JLPT4 } from './index.js';

// Negative examples: sentences that should NOT match てくれる
const negatives = [
  // くれる as standalone verb meaning "to give"
  '彼がプレゼントをくれる。',
  '妹がお菓子をくれた。',
  // てあげる (I do something for someone - different direction)
  '彼に本を貸してあげた。',
  '友達に教えてあげました。',
  // てもらう (I receive favor - different perspective)
  '彼に日本語を教えてもらった。',
  '田中さんに手伝ってもらいました。',
  // てある (state of being)
  '黒板に字が書いてある。',
  '窓が開けてある。',
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
