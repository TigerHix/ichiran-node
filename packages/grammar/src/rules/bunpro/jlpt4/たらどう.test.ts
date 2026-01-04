import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './たらどう.js';
import { BUNPRO_JLPT4 } from './index.js';

// Sentences that can't be matched because they use the abbreviated form:
//
// ANALYSIS: たら alone (without どう) as a suggestion
//
// The Bunpro data includes the contracted form where just "たら" is used:
//   暇なら外に行ったら？ (Why don't you go outside if you're free?)
//
// This is an abbreviation of たらどう mentioned in the writeup:
// "Sometimes たら is used by itself, with a tone that implies that it is a question"
//
// However, this sentence LITERALLY contains only "たら" and not "どう", so it
// cannot be matched by a rule looking for the pattern "verb-tara-dou".
//
// Matching all "たら" would overcapture:
//   ❌ 暇なら外に行ったら、家に帰る。(If I'm free, when I go outside, I'll go home.)
//   ❌ 雨が降ったら、行かない。(If it rains, I won't go.)
//
// CONCLUSION: No reliable discriminator. The abbreviated form is context-dependent
// and cannot be distinguished from plain conditional "たら".
const skipPositives = [
  '暇なら外に行ったら？',
];

const negatives = [
  // たら without どう (just conditional "if", not the suggestion pattern)
  'もし雨が降ったら、家にいます。',
  'お金があったら、買いたいです。',
  // Just どう without たら (question "how")
  'どうやって駅へ行きますか？',
  'これはどう思いますか？',
  // どう but with different grammar
  'そうしてもいいです。どうもありがとう。',
  // Ta-form without ら (simple past, not conditional)
  '昨日、映画を見ました。',
  'ご飯を食べました。',
  // Similar patterns but different grammar
  // Verb + て + しまう (different grammar)
  '食べてしまいました。',
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives, skipPositives });
});
