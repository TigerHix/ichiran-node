import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './と-with.js';
import { BUNPRO_JLPT5 } from './index.js';

// Sentences that should NOT match the "with" と rule
const negatives = [
  // Conditional と (SCONJ + mark dep) - different grammar point
  '春になると桜が咲く。',
  '右に行くと駅があります。',
  'このボタンを押すと、動きます。',
  '雨が降ると行きません。',
  '駅に行くと、友達に会った。',

  // Quotation と - quotative marker (different grammar point)
  '「こんにちは」と言った。',
  '「そうです」と思いました。',
  '「行きたい」と言っています。',
  '彼は「大丈夫だ」と答えた。',
  '危ないとかいた。',

  // Other particles
  '友達に行く。',  // に (direction)
  '友達で行く。',  // で (means)
  '友達や行く。',  // や (and - partial listing)
];

describe('bunpro.jlpt5', () => {
  const engine = useSharedEngine([BUNPRO_JLPT5]);
  describeRule(rule, 'JLPT5', BUNPRO_JLPT5.id, engine.get, { negatives });
});
