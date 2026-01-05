import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './なお2.js';
import { BUNPRO_JLPT2 } from './index.js';

// Negative test cases - sentences that should NOT match the なお② grammar rule
const negatives = [
  // Similar conjunctions/additive adverbs with different meanings

  // さらに (sarani) - "furthermore, even more" (progression)
  '電気代がさらに高くなった。',
  'さらに給与が上がり、とても助かります。',

  // その上 (sono ue) - "on top of that, moreover" (emphatic addition)
  '彼は優秀だ。その上、性格もいい。',
  '安い。その上、性能もいい。',

  // また (mata) - "also, again" (less formal, more common)
  'また明日会いましょう。',
  '彼もまた学生です。',

  // それに (soreni) - "and besides" (neutral, less formal)
  '彼は優秀だ。それに性格もいい。',
  'この店は安い。それに美味しい。',

  // しかも (shikamo) - "moreover, furthermore" (emphatic, surprising)
  'この店は安い。しかも美味しい。',
  '彼は頭がいい。しかも努力家だ。',

  // おまけに (omake ni) - "on top of that" (often negative, colloquial)
  '雨だった。おまけに風も強かった。',
  '疲れた。おまけに腹も減った。',

  // ますます (masumasu) - "increasingly, more and more" (intensification)
  'ますます寒くなってきました。',
  'ますますのご活躍をお祈りします。',

  // いっそう (issou) - "all the more, to a greater extent" (comparative)
  'いっそう努力します。',
  '雨でいっそう美しくなった。',

  // いよいよ (iyoiyo) - "at last, finally, truly" (climax/finally)
  'いよいよ明日は卒業です。',
  'いよいよ始まります。',

  // Note: We do NOT include なお① (still/yet) examples as negatives
  // because both なお① and なお② use the same word "なお" and
  // both rules legitimately match it in different contexts.
  // The grammar system correctly identifies both usages of the word.
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
