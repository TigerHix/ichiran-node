import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './かもしれない.js';
import { BUNPRO_JLPT4 } from './index.js';

const negatives = [
  // 知れる as independent verb "can be known" (not part of かもしれない)
  '結果はまだ知れない。',

  // Separate clause ending in 知れる (potential of 知る)
  '彼の気持ちは知れている。', // His feelings can be guessed (lit. can be known)
];

// Sentences that can't be matched due to GiNZA parsing limitations:
//
// 1. Shortened form かも (ambiguous - can be standalone particle or part of かもしれない)
//    - ドライブするかも (could just be "drive + maybe particle")
//    - そうかもね (could just be "so + maybe particle")
//    - 見られるかも (could just be "can see + maybe particle")
//
//    GiNZA parses か as a particle (PART) with dep=mark in all these cases.
//    The full かもしれない has も with dep=fixed, which distinguishes it.
//    But standalone かも lacks the fixed dependency chain.
//
// 2. Colloquial form かもしらん (different structure)
//    - 忘れてきたかもしらん
//
//    GiNZA parses this as:
//    - か (PART, dep=mark)
//    - も (ADP, dep=fixed)
//    - しら (VERB, lemma=しらん)
//    - ん (AUX)
//
//    Different lemma (しらん vs しれる) and structure.
//    Would require a separate rule variant just for this dialectal form.
//
// CONCLUSION: Matching shortened forms would overcapture on standalone かも particles.
// These are legitimate expressions of uncertainty, but structurally indistinguishable
// from other uses of かも without the full かもしれない pattern.
const skipPositives = [
  '明日は休みだからドライブするかも。',
  'そうかもね。',
  '財布を家に忘れてきたかもしらん。',
  'この時期は晴れの日が続くから、富士山を綺麗に見られるかも。',
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives, skipPositives });
});
