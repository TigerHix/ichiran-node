import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './なさい.js';
import { BUNPRO_JLPT4 } from './index.js';

// Negative tests: sentences that should NOT match
const negatives = [
  // Regular なさい (honorific "to do", not imperative)
  '先生が何もなさらないそうです。',
  'あの方は何もなさいませんでした。',
  '何をなさいますか。',
  '明日は何をなさるつもりですか。',

  // するな (prohibitive "don't do" - different grammar)
  'ここに入るな。',
  'そんなことをするな。',
  '食べるな。',
  '来るな。',
  '行くな。',

  // な as sentence-final particle (not related)
  '何か変な感じだな。',
  'これは難しいな。',
  '今日はいい天気だな。',

  // Negative forms of verbs (～ない)
  '勉強しないでください。',
  '食べない。',
  '行かない。',

  // てください (polite request, different from command)
  '勉強してください。',
  '食べてください。',
  '来てください。',
];

// Sentences that can't be matched due to GiNZA parsing limitations:
//
// ANALYSIS: Casual imperative form (verb stem + な)
//
// For the casual form "な", we need to distinguish:
// - Verb stem + な = "do X!" (imperative) ✓ SHOULD MATCH
// - Verb dictionary form + な = "don't X!" (prohibitive) ✗ SHOULD NOT MATCH
//
// The discriminator is the inflection form of the verb:
// - Stem forms: 連用形-一般, 連用形-促音便, 連用形-イ音便, 語幹-一般
// - Dictionary form: 終止形-一般
//
// However, GiNZA doesn't consistently set inflectionForm for all tokens:
// - 勉強し → inflectionForm=null (should be 連用形-一般)
// - 謝り → inflectionForm=null (should be 連用形-一般 or 語幹-一般)
// - 入る → inflectionForm=終止形-一般 ✓ CORRECT
// - 食べる → inflectionForm=終止形-一般 ✓ CORRECT
//
// When inflectionForm is null, we can't distinguish between stem and dictionary forms.
// This means we can't safely match the casual な form without risking false positives
// on the prohibitive pattern.
//
// CONCLUSION: Can't reliably discriminate casual な vs prohibitive な when GiNZA
// doesn't set inflectionForm. Skip the casual form cases that require this discrimination.
const skipPositives = [
  'お前も悪いから謝りな。',
  '勉強しな。',
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives, skipPositives });
});
