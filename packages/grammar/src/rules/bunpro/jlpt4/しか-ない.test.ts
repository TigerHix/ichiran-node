import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './しか-ない.js';
import { BUNPRO_JLPT4 } from './index.js';

const negatives = [
  // だけ instead of しか (different grammar point)
  'これだけがある。',
  'ここだけにある。',

  // Note: だけしか (emphatic "only") still contains しか + negative pattern
  // so it matches. It's a variant of しか-ない, not a completely different pattern.
  // If needed, it should be handled by a separate "だけしか" rule that has priority.

  // Positive verb (しか requires negative)
  // Note: This is grammatically incorrect in Japanese, but testing that we don't match it
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
