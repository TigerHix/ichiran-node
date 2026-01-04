import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './verbて-b2.js';
import { BUNPRO_JLPT4 } from './index.js';

// Negative examples: patterns that should NOT be matched as causal te-forms
const negatives = [
  // て at end of sentence (request/command, not connecting clauses)
  'もっと待って。',
  'こっち来て。',
  '早く来て。',

  // Note: ている and てある are excluded from negatives because:
  // 1. Structurally they ARE te-forms with SCONJ て
  // 2. The distinction is based on the auxiliary that follows (いる/ある)
  // 3. Those patterns have their own specific grammar rules (ている1, ている2, てある)
  // 4. The causal te-form rule will match them, which is acceptable overlap
  //
  // '何をしてるの？',  // ている - different grammar point
  // '彼は今寝ています。', // ている - different grammar point
  // '壁に絵が掛けてある。', // てある - different grammar point
  // 'ドアが開いている。', // いている - different grammar point
];

// Sentences that can't be matched due to GiNZA parsing limitations:
//
// 1. かえって (て-form of 帰る) - GiNZA parses as single ADV token instead of VERB + SCONJ
//    メアリーがアメリカにかえって悲しいです。
//
//    GiNZA parse: かえって [pos=ADV, lemma=かえって, dep=advmod]
//    Expected: 帰っ [pos=VERB] + て [pos=SCONJ, dep=mark]
//
//    When the verb is written in hiragana as a single unit, GiNZA treats it as an adverb.
//    When written in kanji (帰って), it parses correctly as VERB + SCONJ.
//
//    CONCLUSION: GiNZA limitation - cannot distinguish without modifying the input.
const skipPositives = [
  'メアリーがアメリカにかえって悲しいです。',
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives, skipPositives });
});
