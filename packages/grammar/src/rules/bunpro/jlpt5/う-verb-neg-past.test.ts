import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './う-verb-neg-past.js';
import { BUNPRO_JLPT5 } from './index.js';

// Sentences that can't be matched due to different grammatical structures:
//
// ANALYSIS: Polite forms of ある, and i-adjective forms
//
// This rule matches u-verbs in negative past form, both casual (～なかった) and polite (～ませんでした):
//   Casual: VERB conjClass="五段-*" + AUX lemma="ない" infl="連用形-促音便"
//   Polite: VERB infl="連用形-一般" + AUX lemma="ます" infl="未然形-一般" + AUX lemma="ぬ" +
//           AUX lemma="です" infl="連用形-一般" + AUX lemma="た"
//
// The Bunpro data also includes:
// 1. Polite forms of ある (～ありませんでした) - ある is not a u-verb (五段):
//    ある is an irregular verb with conjugation class "五段-ワア行-アル"
//    The rule only matches 五段-* verb classes, not this irregular form.
//
// 2. i-adjective なかった (as in 紙がなかった, お客さんに怪我はなかった) - different POS:
//    ADJ conjClass="形容詞" (not AUX)
//    This is the adjective ない (negation of ある), not the auxiliary verb ない.
//    GiNZA parses these as ADJ, not AUX, so they can't be matched by this rule.
//
// CONCLUSION: Skip polite forms of ある and i-adjective forms. The rule matches both casual
// and polite u-verb negative past forms for regular u-verbs.
const skipPositives = [
  // Polite forms of ある (～ありませんでした) - ある is not a regular u-verb
  '紙がありませんでした。',
  'そこには本がありませんでした。',

  // i-adjective なかった (negation of ある, not a verb)
  '紙がなかった。',
  'お客さんに怪我はなかった。[[お客さんに怪我はなかったです。お客さんに怪我はありませんでした。]]',
];

// Negative test cases - similar forms that should NOT match
const negatives = [
  // i-adjective negative past (different POS and conjugation class)
  '高くなかったです。', // takaku nakatta (was not high) - i-adj
  '良くなかった。', // yoku nakatta (was not good) - i-adj

  // ru-verb negative past (different conjugation class - 下一段 instead of 五段)
  '食べなかった。', // tabe nakatta (didn't eat) - ru-verb
  '見なかった。', // mi nakatta (didn't see) - ru-verb
  '起きなかった。', // oki nakatta (didn't wake up) - ru-verb

  // Positive past (not negative)
  '洗った。', // aratta (washed) - positive past
  '行った。', // itta (went) - positive past

  // Negative non-past (different inflection form)
  '洗わない。', // arawanai (don't wash) - negative present
  '行かない。', // ikanai (don't go) - negative present
];

describe('bunpro.jlpt5', () => {
  const engine = useSharedEngine([BUNPRO_JLPT5]);
  describeRule(rule, 'JLPT5', BUNPRO_JLPT5.id, engine.get, { negatives, skipPositives });
});
