import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './だが・ですが.js';
import { BUNPRO_JLPT4 } from './index.js';

// Negative examples: similar patterns that should NOT match
const negatives = [
  // Subject particle が (not conjunction)
  '私が学生です。',
  '彼が来ました。',
  // が attached to verb/i-adj (not copula + が)
  '行きますが、時間がない。',
  // です/だ followed by subject が
  '彼です。が、私は違う。', // Separate sentences, です is predication, が is subject
  // だけど (casual form - different grammar)
  'それは簡単だけど、時間がかかる。',
  // けれど/けれども (different grammar)
  'それは簡単けれど、時間がかかる。',
  'それは簡単けれども、時間がかかる。',
  // しかし (different grammar)
  'それは簡単。しかし、時間がかかる。',
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
