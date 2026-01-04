import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './たほうがいい.js';
import { BUNPRO_JLPT5 } from './index.js';

const negatives = [
  // Verb + た + ほう (comparison, not advice)
  'したほうが楽だ。', // doing is easier (comparison)
  // Verb + ほうがいい without た (different grammar - would be ば or たら)
  'すればいい。', // just do it (different pattern)
  // ほうがいい with ない-form (negative advice - different grammar rule)
  'あまり飲まないほうがいい。', // had better not drink (ないほうがいい)
  // Verb + た (simple past, not advice)
  '昨日、本を読んだ。', // read a book yesterday (past tense)
];

// Sentences that can't be matched due to grammar pattern differences:
//
// The grammar point **たほうがいい** specifically uses the past tense form (た-form)
// to give advice or make suggestions: "You had better do" or "It would be better to do".
//
// However, Bunpro includes two example sentences that use dictionary form instead:
//   - 食べる + ほうがいい (general opinion: "it is better to eat")
//   - 片付ける + ほうがいい (general opinion: "it is better to clean up")
//
// These are a different pattern that expresses general opinions rather than specific advice.
// The writeup explicitly states: "たほうがいい may also be used with the base (dictionary)
// form of a verb, but will sound more like a general opinion in those cases, rather than
// giving advice."
//
// To match these dictionary-form sentences, we would need to add separate patterns for:
//   - Verb[連体形-一般] + ほう + が + いい (dictionary form)
//
// However, this would:
//   1. Blur the distinction between the main pattern (た-form for advice) and the variant
//   2. Make the rule match two fundamentally different grammatical constructions
//   3. Go against the pedagogical focus of the grammar point (た-form advice)
//
// CONCLUSION: These sentences represent a different grammatical pattern (dictionary form
// + ほうがいい for general opinions) and should not be matched by the たほうがいい rule.
const skipPositives = [
  '食器はすぐ片付けるほうがいい。',
  '高いけど、美味しいお肉を食べるほうがいい。',
];

describe('bunpro.jlpt5', () => {
  const engine = useSharedEngine([BUNPRO_JLPT5]);
  describeRule(rule, 'JLPT5', BUNPRO_JLPT5.id, engine.get, { negatives, skipPositives });
});
