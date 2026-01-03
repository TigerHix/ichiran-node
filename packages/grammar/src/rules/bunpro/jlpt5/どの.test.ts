import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './どの.js';
import { BUNPRO_JLPT5 } from './index.js';

// Negative examples: どの should NOT match in these cases
const negatives = [
  // Other demonstratives should not match
  'このケーキは大きいです。',
  'その本がいいです。',
  'あのレストランに行きます。',
  // Pronouns (not pre-noun adjectivals)
  'これはいい本です。',
  'それは高い本です。',
  'あれは古い本です。',
  'どれが好きですか。',
];

describe('bunpro.jlpt5', () => {
  const engine = useSharedEngine([BUNPRO_JLPT5]);
  describeRule(rule, 'JLPT5', BUNPRO_JLPT5.id, engine.get, { negatives });
});
