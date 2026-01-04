import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './ようにいのる.js';
import { BUNPRO_JLPT4 } from './index.js';

// Negative examples: sentences that should NOT match ようにいのる
const negatives = [
  // Simple ように without 祈る (different grammar point)
  '日本語のように話す。',
  '大人のようになる。',
  '勉強するように言われた。',

  // ようにする (make an effort) - different grammar
  '毎日勉強するようにしている。',
  '健康に気をつけるようにしましょう。',

  // ようになる (change of state) - different grammar
  '日本語を話せるようになった。',
  'できるようになりたい。',

  // 祈る without ように (just praying, not "so that")
  '神様に祈る。',
  '毎日祈っています。',

  // といい (hope/wish) - different grammar point
  '雨が降るといいな。',
  '合格するといいですね。',
];

// Sentences that cannot be matched due to GiNZA parsing or grammar limitations
const skipPositives = [
  // ANALYSIS: Shortened form where 祈る is omitted
  //
  // The sentence "トモちゃんとジュンイチロウくんが結婚するように。" ends
  // with just ように (without 祈る). According to the Bunpro writeup:
  // "However, 'unlike' other ように structures, ように祈る will sometimes
  // be shortened to ように by itself. This is especially true after ます."
  //
  // This is a special elliptical construction where the verb is omitted.
  // Without the verb 祈る explicitly present, this cannot be distinguished
  // from other ように patterns (purpose, manner, etc.) using structural
  // matching alone.
  //
  // The rule requires the verb 祈る to be present for unambiguous matching.
  'トモちゃんとジュンイチロウくんが結婚するように。',
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives, skipPositives });
});
