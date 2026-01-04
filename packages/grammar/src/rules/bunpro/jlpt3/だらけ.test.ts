import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './だらけ.js';
import { BUNPRO_JLPT3 } from './index.js';

// Negative examples: Similar patterns that should NOT match
const negatives = [
  // まみれ (JLPT1 - covered/stained with, e.g., 泥まみれ, 血まみれ)
  // まみれ emphasizes being soaked/stained, not scattered
  '彼は泥まみれの靴で部屋に入った。',
  '血まみれのシャツを着ていた。',

  // ずくめ (JLPT1 - entirely/all, used with set expressions, e.g., いいことずくめ)
  // ずくめ is usually used with positive connotations
  '今日はいいことずくめの日だ。',
  '黒ずくめの服を着ている。',

  // ばかり (nothing but/only - different grammar)
  // Used for frequency or exclusivity, not being covered with something
  '彼はいつも文句ばかり言っている。',
  'あの店は肉ばかり売っている。',

  // だけ (only - different grammar)
  'これだけあれば十分だ。',
  'あなたにだけ話します。',
];

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get, { negatives });
});
