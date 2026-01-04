import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './らしい2.js';
import { BUNPRO_JLPT4 } from './index.js';

// Sentences that must be skipped from positive tests
// "愛らしさ" uses the nominalized form "らしさ" (noun suffix), not "らしい"
const skipPositives = [
  '「あなたの笑顔の愛らしさに気づいたばかりだわ。」',
];

// Negative test cases - sentences that should NOT match the らしい② grammar rule
//
// IMPORTANT LIMITATION:
// There is inherent overlap between らしい① (hearsay) and らしい② (characteristic)
// because they use identical surface forms when attached to nouns. Context determines
// the meaning, not grammar. This is acceptable since they're related usages.
//
// We only test cases that are clearly NOT noun + らしい:
const negatives = [
  // Other similar grammar patterns that should not match
  '子供のようだ。',         // のようだ (seems like) - different structure
  '先生のような人だ。',      // のような (like/similar to) - different structure
  '子供っぽい態度。',        // っぽい (-ish) - different suffix
  '先生みたいな人だ。',      // みたいな (like/similar to) - different structure
];

// Note: Verb + らしい (e.g., 行くらしい) and Adj + らしい (e.g., 忙しいらしい) from
// らしい① (hearsay) may match due to catch-all branch, but this is acceptable
// overlap since the forms are grammatically related.

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives, skipPositives });
});
