import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './ちゃんと-きちんと.js';
import { BUNPRO_JLPT3 } from './index.js';

// Negative examples: similar adverbs that should NOT match
const negatives = [
  // Similar manner adverbs (different grammar points)
  // しっかり - "thoroughly/firmly" (emphasizes effort or thoroughness)
  'しっかり勉強してください。',
  'しっかり掴んで。',
  'しっかりしてください。',

  // よく - "often/well" (different meaning)
  'よく勉強した。',
  'よく食べてください。',
  'よくわからない。',

  // てきとう - "suitably/appropriate" but also "random/careless"
  'てきとうに選んでください。',
  'てきとうな答えです。',

  // じゅうぶん - "sufficiently/enough" (focuses on quantity/degree)
  'じゅうぶん時間がある。',
  'じゅうぶん食べてください。',
  'じゅうぶん準備しました。',

  // ちょうど - "exactly/precisely" (different meaning)
  'ちょうどいい時間です。',
  'ちょうど10時です。',

  // ていねいに - "politely/carefully" (focuses on politeness rather than correctness)
  'ていねいに書いてください。',
  'ていねいに説明します。',

  // Different degree adverbs
  'かなり美味しい。',
  'なかなか良い。',
  'けっこう高い。',
  'だいぶ疲れた。',
];

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get, { negatives });
});
