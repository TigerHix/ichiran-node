import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './も.js';
import { BUNPRO_JLPT5 } from './index.js';

// Negative tests: sentences that should NOT match the も particle rule.
//
// The も particle indicates "also/too" and replaces other case markers.
// We want to avoid false positives from other particles with different
// grammatical functions.
//
// Note: Unlike が (which can be "but"), も doesn't have a completely different
// grammatical function that would cause false positives in standard sentences.
const negatives = [
  // Other particles - should not match も
  '私が学生です。',      // Subject marker が
  '彼を待ってください。',  // Object marker を
  '東京に行きます。',     // Direction marker に
  '私は学生です。',      // Topic marker は
  '本とペン',           // And marker と
  '鉛筆で書く',          // Instrumental marker で
  '京都へ行く',          // Direction marker へ
  '家から来る',          // From marker から
  '日本で',              // At marker で
];

describe('bunpro.jlpt5', () => {
  const engine = useSharedEngine([BUNPRO_JLPT5]);
  describeRule(rule, 'JLPT5', BUNPRO_JLPT5.id, engine.get, { negatives });
});
