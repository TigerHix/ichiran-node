import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './だけ.js';
import { BUNPRO_JLPT5 } from './index.js';

// Negative tests: sentences that should NOT match the だけ rule.
//
// だけ means "only/just" and is an adverbial particle.
// We want to avoid false positives from:
// 1. Similar particles like しか (which requires negative verbs)
// 2. のみ (more formal "only", different grammar point)
// 3. Sentences with other grammatical markers
const negatives = [
  // しか～ない (similar meaning but different grammar - requires negative)
  '彼しか来ませんでした。',
  'これしかありません。',
  '一人しかいません。',
  // のみ (more formal version, different grammar)
  '金のみで買える。',
  '会員のみ入可能です。',
  // Other particles that should not match
  '私は学生です。',      // Topic marker は
  '彼が行きます。',      // Subject marker が
  '本を読む。',          // Object marker を
  '東京に行きます。',     // Direction marker に
  '鉛筆で書く。',         // Instrumental marker で
];

describe('bunpro.jlpt5', () => {
  const engine = useSharedEngine([BUNPRO_JLPT5]);
  describeRule(rule, 'JLPT5', BUNPRO_JLPT5.id, engine.get, { negatives });
});
