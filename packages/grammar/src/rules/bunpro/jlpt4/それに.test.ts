import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './それに.js';
import { BUNPRO_JLPT4 } from './index.js';

// Negative examples: similar patterns that should NOT match
const negatives = [
  // Similar words that are NOT それに
  'そこには何もない。', // There's nothing THERE (そこ not それ)
  'その中に入っている。', // Inside THAT (その not それに)
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
