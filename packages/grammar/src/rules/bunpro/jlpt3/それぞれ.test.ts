import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './それぞれ.js';
import { BUNPRO_JLPT3 } from './index.js';

// Negative examples: similar words that should NOT match
const negatives = [
  // Similar "each" words (different grammar points)
  // 各 - prefix meaning "each/every" (always before noun)
  '各部屋に鍵があります。',
  '各階に地図があります。',
  '各国で大会が行われます。',

  // 各々 - slightly more formal variant of それぞれ
  '各々の意見を聞くべきだ。',

  // おのおの - formal "each/every"
  'おのおのが自分の道を行く。',

  // 一人一人 - "each person" (different structure)
  '一人一人の意見を聞いてください。',
  '一人一人が違う思いを持っている。',

  // 一つ一つ - "one by one/each one"
  '一つ一つ確認してください。',

  // ずつ - "each/per" (after quantities)
  '一つずつ食べる。',
  '一人ずつ入ってください。',
  '千円ずつ払う。',

  // 個々 - suffix meaning "respective" (before noun)
  // それぞれ is casual daily conversation, 個々 is more formal
  '個々の事情がある。',  // individual circumstances
  '個々に対応する。',     // handle individually

  // 当たり - "per/apiece" (after numbers)
  '一個当たり100円です。',
  '一人当たり5000円。',

  // ていてい - "each/every" (archaic/literary)
  // Not commonly used in modern Japanese

  // Sentences that use "each" but with different patterns
  'みんな同じことをしている。',  // everyone doing the same (not each)
  '全員が集まった。',             // everyone gathered (not each)
];

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get, { negatives });
});
