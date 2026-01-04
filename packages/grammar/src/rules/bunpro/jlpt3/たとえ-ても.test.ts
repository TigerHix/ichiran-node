import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './たとえ-ても.js';
import { BUNPRO_JLPT3 } from './index.js';

// Sentences with たとえ that should NOT match the pattern
const negatives = [
  // たとえ without ても/でも (incomplete pattern)
  'たとえ雨が降ります。',
  'たとえ彼が来るでしょう。',
  // Different grammar using たとえ (if combined differently)
  // Note: Most negative cases would require contextual discrimination
  // since たとえ almost always appears with ても/でも
];

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get, { negatives });
});
