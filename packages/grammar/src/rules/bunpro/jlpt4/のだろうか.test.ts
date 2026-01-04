import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './のだろうか.js';
import { BUNPRO_JLPT4 } from './index.js';

const negatives = [
  // だろう without か (conjecture, not wondering)
  '明日は雨だろう。',
  '彼も来るだろう。',

  // か as question particle without だろう
  '行きますか。',
  'これですか。',

  // だけか (only + question)
  'これだけか。',
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
