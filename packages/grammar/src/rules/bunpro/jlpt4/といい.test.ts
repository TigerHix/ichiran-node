import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './といい.js';
import { BUNPRO_JLPT4 } from './index.js';

// Negative examples - sentences that look similar but should NOT match
const negatives = [
  // Quotation と + いい (different meaning - "said 'good'")
  '彼は「いい」と言った。',
  '田中さんは「行こう」と言っています。',
  '先生は「明日は晴れる」と言いました。',

  // と + other adjectives (not expressing hope with "good")
  '勉強すると楽しい。',        // Studying is fun (conditional, but not "hope")
  '彼が来ると嬉しい。',        // I'm happy when he comes (state, not hope)

  // Quotation patterns (different meaning)
  '彼は行くと言っている。',      // quotation (no いい)

  // Note: We intentionally don't test "雨が降ったといい人がいた" as negative
  // because the surface form といい does appear there - it's just a different meaning.
  // Distinguishing these requires deeper context analysis beyond simple patterns.
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
