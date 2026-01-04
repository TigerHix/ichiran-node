import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './る-verb-past.js';
import { BUNPRO_JLPT5 } from './index.js';

// Sentences that can't be matched due to test data issues:
const skipPositives = [
  // "ドアを開ける。" (I open the door) - This is PRESENT tense, not PAST tense.
  // This sentence appears in the る-verb-past test data but is not in past tense.
  // Translation: "I open the door." (present)
  // The rule correctly matches only past tense forms (～た, ～ました).
  'ドアを開ける。',
];

describe('bunpro.jlpt5', () => {
  const engine = useSharedEngine([BUNPRO_JLPT5]);
  describeRule(rule, 'JLPT5', BUNPRO_JLPT5.id, engine.get, { skipPositives });
});
