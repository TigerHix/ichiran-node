import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './さらに.js';
import { BUNPRO_JLPT2 } from './index.js';

// Negative test cases - sentences that should NOT match the さらに grammar rule
const negatives = [
  // また (mata) - "also, again" (less formal, more common)
  'また明日会いましょう。',
  '彼もまた学生です。',

  // その上 (sono ue) - "besides, in addition" (conjunction-focused)
  '彼は優秀だ。その上、性格もいい。',
  '安い。その上、性能もいい。',

  // しかも (shikamo) - "moreover, furthermore" (emphasizes reinforcement)
  'この店は安い。しかも美味しい。',
  '彼は頭がいい。しかも努力家だ。',

  // おまけに (omake ni) - "on top of that" (often negative, colloquial)
  '雨だった。おまけに風も強かった。',
  '疲れた。おまけに腹も減った。',

  // なお (nao) - "furthermore" (neutral, formal, simple addition)
  '詳細はなおお問い合わせください。',
  'なお、ご不明な点があればお問い合わせください。',

  // ますます (masumasu) - "increasingly, more and more" (state intensification)
  'ますます寒くなってきました。',
  'ますますのご活躍をお祈りします。',

  // 再び (futatabi) - "again, once more" (repetition after pause)
  '再び会う日を楽しみにしています。',
  '再び同じ間違いをしないように。',

  // いっそう (issou) - "all the more, to a greater extent" (comparative intensifier)
  'いっそう努力します。',
  '雨でいっそう美しくなった。',

  // Noun + さらに (when さらに is part of a compound, not the grammar point)
  // Note: This is difficult to construct as さらに rarely compounds with nouns

  // さ (sa) - sentence-ending particle (unrelated)
  '今日は暑いさ。',
  '分かったさ。',

  // に (ni) - particle/direction marker alone (not part of さらに)
  '東京に行きます。',
  '学校に遅刻した。',
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
