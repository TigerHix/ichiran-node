import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './どころか.js';
import { BUNPRO_JLPT3 } from './index.js';

const negatives = [
  // Different grammar: どころではない (JLPT2) - "out of the question"
  // These should NOT match as they're a different grammar point
  '忙しすぎて年休を取るどころではない。',
  'そんな冗談を言っている場合ではない。',

  // Similar-looking particles that aren't どころか
  // どころで (different particle)
  // Note: May be hard to find clear negative examples since どころか is fairly unique

  // Noun + ところ (place) + か (question particle) - different meaning
  // These should not match our どころか pattern
  '行くところか分からない。',  // "I don't know the place to go" (not "far from going")
];

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get, { negatives });
});
