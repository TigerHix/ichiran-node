import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './-なり-なり.js';
import { BUNPRO_JLPT1 } from './index.js';

// Negative test cases: sentences that should NOT match
const negatives = [
  // Single なり usage (not the listing pattern)
  '彼になりたい。',
  '本当になりたい。',
  // Different grammar: なり as "to become" verb
  '彼は医者になりたい。',
  '春になった。',
  // Different listing particles (とか, や)
  'リンゴとかバナナなどを買った。',
  'リンゴやバナナを買った。',
  // たり-たりする pattern (different listing grammar)
  '行ったり来たりしている。',
  '食べたり飲んだりした。',
  // Separate clauses with なり
  '彼は来た。私はなり行った。',
];

// Sentences that can't be matched due to GiNZA parsing limitations:
//
// ANALYSIS: "大なり小なり" (greater or lesser)
//
// GiNZA parses this as:
//   [0] 大なり   pos=NOUN  lemma=大なり  (compound token)
//   [1] 小       pos=NOUN  lemma=小
//   [2] なり     pos=AUX   lemma=なる
//
// The expected pattern is: 大 + なり + 小 + なり
// But GiNZA treats "大なり" as a single compound NOUN token.
//
// We cannot match this because:
// 1. The first なり is embedded within the "大なり" compound token
// 2. There's no way to match a substring within a token
// 3. This is fundamentally a GiNZA tokenization limitation
//
// Note: This is different from other patterns like "フランスなりイタリアなり"
// where "なり" is correctly tokenized separately.
const skipPositives = [
  '完璧な人はいない。大なり小なり欠点はあるものだ。',
];

describe('bunpro.jlpt1', () => {
  const engine = useSharedEngine([BUNPRO_JLPT1]);
  describeRule(rule, 'JLPT1', BUNPRO_JLPT1.id, engine.get, { negatives, skipPositives });
});
