import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './っけ.js';
import { BUNPRO_JLPT3 } from './index.js';

const negatives = [
  // Standard question particle か (different from っけ)
  '明日は雨ですか。',

  // Confirmation particle ね (different from っけ)
  '明日は雨ですね。',

  // Emphatic particle よ (different from っけ)
  '明日は雨ですよ。',

  // っ at the end of words (not っけ)
  '学校に行った。',
  '先生に会った。',

  // Similar-looking but different grammar
  // って as quotation particle (not sentence-final)
  '彼は行くと言った。',
];

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get, { negatives });
});
