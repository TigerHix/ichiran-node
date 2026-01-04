import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './と-and.js';
import { BUNPRO_JLPT5 } from './index.js';

// Sentences that can't be matched due to GiNZA parsing limitations or data issues:
//
// 1. ANALYSIS: "and" particle と connecting nouns
//
// GiNZA incorrectly tags ポチ (a dog name) as ADV instead of NOUN/PROPN:
//   ポチとサタケ → pos=ADV (should be NOUN) ✗ INDISTINGUISHABLE
//
// Most other names are correctly tagged:
//   トムとジェリー → pos=NOUN ✓ WORKS
//   メアリーとたけし → pos=PROPN ✓ WORKS
//
// The structure is correct (noun connects to noun via case-marked particle),
// but the POS tag is wrong. Matching all ADV tokens would overcapture:
//   ❌ よく食べる (adverbial usage)
//   ❌ とても大きい (adverbial usage)
//
// CONCLUSION: No reliable discriminator. GiNZA limitation.
//
// 2. Bunpro data includes "with" meaning examples
//
// The sentence "彼女と食べる。" (eat with girlfriend) demonstrates the
// accompaniment meaning of と, not the listing "and" meaning. The structure
// is identical (noun + と), but the semantic meaning is different:
//   - 彼女と食べる → obl → VERB (with)
//   - 川と滝 → nmod → NOUN (and)
//
// This sentence is in the Bunpro writeup section showing contrast between
// "and" vs "with" usages. It should NOT match the "and" rule.
const skipPositives = [
  'ポチとサタケは犬です。',
  '彼女と食べる。',
];

// Sentences where と means something OTHER than "and" (listing particle).
// These should NOT match the と-and rule.
const negatives = [
  // Accompaniment と meaning "with" (different grammar)
  '彼と食べる。',
  '彼と行く。',
  '私は彼と行きました。',
  '友達と遊ぶ。',

  // Quotation particle と (with 言う, 思う, etc.)
  '彼は「これは違う」と言った。',
  '「危ない」と言った。',
  '彼だと思った。',
  '「行く」と言った。',
  '「いいえ」と答えた。',

  // Conditional と (if/when)
  '春になると、桜が咲く。',
  'これを見ると、思い出す。',

  // Other particles (for completeness)
  '彼は行った。',
  '彼が行った。',
  '彼を行く。',
];

describe('bunpro.jlpt5', () => {
  const engine = useSharedEngine([BUNPRO_JLPT5]);
  describeRule(rule, 'JLPT5', BUNPRO_JLPT5.id, engine.get, { negatives, skipPositives });
});
