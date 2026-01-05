import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './が気になる.js';
import { BUNPRO_JLPT2 } from './index.js';

// Negative test cases - sentences that should NOT match the が気になる grammar rule
const negatives = [
  // 気にする (ki ni suru) - "to worry about (intentionally)" - different verb
  '彼の言葉を気にする。',
  '失敗を気にしすぎないでください。',
  '人の目を気にするな。',

  // 気に入る (ki ni iru) - "to like/be pleased with" - different verb
  'その服が気に入った。',
  'このプレゼントが気に入っています。',

  // Simple 気になる without が - intransitive usage (different grammar)
  // Note: Some of these might still match if が appears elsewhere in the sentence

  // 気になる used as "to be curious" without clear subject
  // (These may vary depending on context)

  // Other grammar with 気
  '気がつく。',
  '気をつける。',
  '気になるほどだ。',
  '気にかけないでください。',

  // が + other uses (not followed by 気になる)
  '私が行きます。',
  '彼が来た。',
  '雨が降っている。',

  // Similar sounding but different grammar patterns
  'が気に入る (different verb)',
  'が気がする (different structure)',
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
