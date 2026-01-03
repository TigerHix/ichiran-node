import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './この.js';
import { BUNPRO_JLPT5 } from './index.js';

// Negative examples: この should NOT match in these cases
const negatives = [
  // Other demonstratives should not match
  'そのケーキは大きいです。',
  'あの本がいいです。',
  'どの本が好きですか。',
  // Pronouns (not pre-noun adjectivals)
  'これはいい本です。',
  'それは高い本です。',
  'あれは古い本です。',
];

describe('bunpro.jlpt5', () => {
  const engine = useSharedEngine([BUNPRO_JLPT5]);
  describeRule(rule, 'JLPT5', BUNPRO_JLPT5.id, engine.get, { negatives });
});
