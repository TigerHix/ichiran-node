import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './さて.js';
import { BUNPRO_JLPT3 } from './index.js';

// Negative examples: sentences with similar-looking patterns that should NOT match
const negatives = [
  // さて used as a compound verb component (not discourse marker)
  // Note: This is unlikely as さて is primarily a conjunction
];

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get, { negatives });
});
