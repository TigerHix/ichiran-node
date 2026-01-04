import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './して.js';
import { BUNPRO_JLPT5 } from './index.js';

// Negative examples that should NOT match (overcapture prevention)
const negatives = [
  // Verb + て-form of other verbs (not して)
  '食べて、テレビを見ました。',
  '走って、疲れました。',
  '読んで、分かりました。',
  // Simple する (dictionary form, not te-form)
  '勉強します。',
  '掃除する。',
  // して as auxiliary in different contexts
  'どうして行かないの。',
  'なぜしてくれなかった。',
  // して in compound contexts where different verb is attached to same head
  '手伝ってしてくれて、ありがとう。',
];

// Sentences that can't be matched due to GiNZA parsing limitations:
//
// The rule matches verb + "し" (AUX) + "て" (SCONJ).
// This pattern is used for multiple grammars:
//
// 1. **て-form conjunction** (e.g., 勉強して、宿題をする) - DESIRED
//    - "て" is followed by punctuation (comma, period) or new clause verb
//    - Rule: require "て" followed by PUNCT or different verb
//
// 2. **ていた** (progressive past, e.g., 勉強していた) - NOT DESIRED
//    - "て" is followed by "た" (AUX, dep=aux) of same verb
//    - Same surface "して" but different grammatical function
//    - CANNOT DISCRIMINATE: both forms have "し"(AUX) + "て"(SCONJ) + "た"(AUX)
//
// 3. **ていない** (negative progressive, e.g., 勉強していない) - NOT DESIRED
//    - "て" is followed by "ない" (AUX, lemma=ない)
//    - Same surface "して" but different grammatical function
//    - CANNOT DISCRIMINATE: both forms have "し"(AUX) + "て"(SCONJ) + "ない"(AUX)
//
// 4. **てください** (request form, e.g., 質問してください) - NOT DESIRED
//    - "て" is followed by "ください" (AUX, dep=fixed)
//    - Same surface "して" but different grammatical function
//    - Could potentially discriminate using dep=fixed vs dep=mark, but may overcapture
//
// 5. **てしまう** (completion form, e.g., 約束してしまった) - NOT DESIRED
//    - "て" is followed by auxiliary verbs (like しまう)
//    - Same surface "して" but different grammatical function
//    - CANNOT DISCRIMINATE: surface identical, only difference is following auxiliary
//
// CONCLUSION: Without additional discriminators in GiNZA parse (like different dep labels
// or specific auxiliary types), we cannot reliably distinguish conjunction "して"
// from other grammatical uses that have same surface form.
//
// SkipPositives approach would require skipping ALL non-conjunction examples, which defeats
// the purpose of the rule. Instead, this rule focuses on the core pattern
// and relies on negatives list to catch overcapture cases.
const skipPositives = [];

describe('bunpro.jlpt5', () => {
  const engine = useSharedEngine([BUNPRO_JLPT5]);
  describeRule(rule, 'JLPT5', BUNPRO_JLPT5.id, engine.get, { negatives, skipPositives });
});
