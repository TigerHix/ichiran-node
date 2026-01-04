import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './と言っても.js';
import { BUNPRO_JLPT3 } from './index.js';

const negatives = [
  // といえば (different grammar - "speaking of which")
  '夏といえば海ですね。',
  'カレーといえばこの店です。',

  // としても (different grammar - "even as/though")
  '先生としても反対です。',
  '冗談としても言い過ぎです。',

  // Simple と particle (quotation without 言っても)
  '彼は行くと言った。',
  'こんにちはと言いました。',

  // と言っていい (different grammar - "you could say that")
  'これは素晴らしいと言っていい。',
];

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get, { negatives });
});
