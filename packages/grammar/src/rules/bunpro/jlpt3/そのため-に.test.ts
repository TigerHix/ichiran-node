import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './そのため-に.js';
import { BUNPRO_JLPT3 } from './index.js';

// Negative examples: similar patterns that should NOT match
const negatives = [
  // Simple possession pattern (その + noun, not conjunction)
  'そのための方法を教えてください。',
  'そのための準備が必要です。',
  // Similar but different conjunctions
  'それで遅れました。',
  'だから行けません。',
  // ため in different grammatical contexts
  '勉強のため、日本に行きます。',
  '何のために働くのですか。',
  // Different words starting with その
  'その後、彼は来ませんでした。',
  'そのうちに行きます。',
];

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get, { negatives });
});
