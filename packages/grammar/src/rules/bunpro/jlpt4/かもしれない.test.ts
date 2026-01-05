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
// 1. Colloquial form かもしらん (different structure)
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
// Note: The shortened form かも IS now matched (Pattern 4):
// - ドライブするかも
// - そうかもね
// - 見られるかも
//
// These all have か (PART/ADP, dep=mark/case) + も (ADP, dep=case),
// which Pattern 4 matches successfully.
const skipPositives = [
  // Colloquial form かもしらん (different structure - uses しらん instead of しれる)
  '財布を家に忘れてきたかもしらん。',
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives, skipPositives });
});
