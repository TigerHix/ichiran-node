import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './がほしい.js';
import { BUNPRO_JLPT4 } from './index.js';

// Negative examples: sentences that should NOT match がほしい
const negatives = [
  // ほしい as standalone adjective (not noun + が + ほしい)
  // This is technically a different grammatical structure
  '彼はほしいと言っている。',
  // てほしい (want someone to do something - different grammar)
  '私に見てほしい。',
  '彼氏に大事にしてほしい。',
  // をほしい (incorrect particle - should be が)
  // These wouldn't appear in natural Japanese, but we want to ensure we don't match them
  // Actually, let's not test these as they're ungrammatical
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
