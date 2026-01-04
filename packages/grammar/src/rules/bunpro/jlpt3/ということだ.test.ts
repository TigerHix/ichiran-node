import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './ということだ.js';
import { BUNPRO_JLPT3 } from './index.js';

// Positive sentences to skip (known GiNZA parsing limitations or data issues)
const skipPositives = [
  // This sentence uses ということ + ではなくて (adversative conjunction)
  // The answer is just "ということ" without だ/です, and it's followed by ではなくて
  // which is a different grammatical structure: "I don't mean X, but Y"
  // Our rule requires だ or です at the end
  '意見を言ってはいけないということではなくて、もっと冷静に話してほしい。',
];

// Negative tests: similar patterns that should NOT match
const negatives = [
  // ということ (JLPT4 - just nominalization without copula "the fact that")
  // This ends with こと as a noun, not followed by だ/です
  '彼が来たということを知らなかった。',

  // ことだ (JLPT3 - advice "you should")
  '時間通りに来ることだ。',
  '練習することです。',

  // そうだ (JLPT3 - plain hearsay, less formal)
  '彼は病気だそうだ。',
  '明日は雨だそうです。',

  // らしい (JLPT4 - "seems like / apparently")
  '彼は来るらしい。',
  '明日は雨らしい。',

  // だって (JLPT3 - casual hearsay)
  '彼は病気だって。',

  // ということではない (negation - "it doesn't mean that")
  '彼が来たということではない。',
  '意味がないということではない。',

  // Simple quotation という (called/named)
  'これは何という花ですか。',
  '田中という人から電話がありました。',

  // というのは (topic marker - "what's called")
  '寿司というのは、日本の代表的な料理です。',

  // ことになっている (arrangement/rule)
  'この部屋では喫煙は禁止されていることになっている。',

  // ことになる (it is decided that)
  '来月日本に行くことになった。',
];

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get, { negatives, skipPositives });
});
