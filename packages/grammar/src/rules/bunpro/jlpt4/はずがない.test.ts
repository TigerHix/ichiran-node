import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './はずがない.js';
import { BUNPRO_JLPT4 } from './index.js';

const negatives = [
  // Similar patterns that should NOT match:
  // ないはずだ (negative expectation - different grammar point)
  // This expresses "bound not to be" rather than "not bound to be"
  '彼の車は高くないはずだ。',
  '雨は降らないはずです。',

  // はずだ (positive expectation - opposite meaning)
  '彼は来るはずだ。',
  'テストに合格するはずです。',

  // Just "はず" as a noun meaning "expectation"
  'はずが違う。',

  // がはず (ungrammatical combination)
  // Not testing as this wouldn't appear in natural Japanese
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
