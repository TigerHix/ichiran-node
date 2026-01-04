import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './ほかにも-ほかには.js';
import { BUNPRO_JLPT4 } from './index.js';

const negatives = [
  // 外 as noun meaning "outside" (not 他/ほか = other/another)
  // The key discriminator is text: 外 vs 他/ほか
  '家の外で遊びます。',
  '外を見てください。',
  '外は寒いです。',
];

// Sentences that can't be matched due to GiNZA parsing limitations:
//
// "いいえ、ほかはいりません。" (No, I don't need anything else.)
//
// ANALYSIS:
// GiNZA parses "ほかは" (other + topic marker) inconsistently.
// Similar sentences that work:
//   - "ほかは？" (Anything else?) ✓ WORKS
//   - "今日はスーパーに行くほかは予定がない。" (Other than going to the supermarket...) ✓ WORKS
//
// The failing sentence has a special structure with:
//   - Sentence-initial "いいえ、" (No,) which might affect parsing
//   - "ほかは" followed immediately by "いりません" (don't need)
//
// The pattern ほか + は is successfully matched in other contexts,
// suggesting a GiNZA inconsistency in how it parses this specific sentence structure.
//
// CONCLUSION: No reliable discriminator. GiNZA limitation.
const skipPositives = [
  'いいえ、ほかはいりません。',
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives, skipPositives });
});
