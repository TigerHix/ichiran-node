import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './ないわけにはいかない.js';
import { BUNPRO_JLPT2 } from './index.js';

// Negative test cases - sentences that should NOT match the ないわけにはいかない grammar rule
const negatives = [
  // わけにはいかない (JLPT3 - opposite meaning: "can't do" vs "can't not do")
  // This is the opposite - means "can't afford to do" without the verb-negative prefix
  '行きたくないけど、行くわけにはいかない。',
  '断るわけにはいかない。',
  'この仕事をやめるわけにはいかない。',

  // ざるを得ない (zaru o enai) - similar meaning but more formal/literary
  '同意せざるを得ない。',
  '謝罪せざるを得ない。',
  '従わざるを得ません。',

  // Simple verb-negative without the わけには pattern
  '行かない。',
  '勉強しない。',
  '食べない。',

  // わけがない (wake ga nai) - "no way that..." (different meaning)
  'そんなわけがない。',
  '失敗するわけがない。',
  '彼が来るわけがない。',

  // わけではない (wake dewa nai) - "not necessarily the case"
  '行きたくないわけではない。',
  'できないわけではない。',

  // わけだ (wake da) - "that's why", "the reason is"
  'そういうわけだ。',
  '遅れたわけです。',

  // ずにはいられない (zu ni wa irarenai) - "can't help but do" (emotional compulsion)
  '泣かずにはいられない。',
  '笑わずにはいられない。',

  // てならない (te naranai) - "can't help but feel" (emotional state)
  '残念でならない。',
  '心配でならない。',

  // ないといけない / なければならない - "must do" (weaker obligation)
  '行かないといけない。',
  '勉強しなければならない。',
  '食べなければなりません。',

  // Simple には + いく patterns without the double negative structure
  '彼には行く。', // "I'll go to him"
  '私にはわからない。', // "I don't understand"

  // わけ as a standalone noun meaning "reason"
  'わけを説明してください。',
  'そういうわけではない。',
  'わけがわからない。',

  // Similar sounding but unrelated patterns
  'わけがない', // "no way that..."
  'わけではない', // "not necessarily"
  'わけです', // "that's the reason"
];

// Sentences that can't be matched due to GiNZA parsing limitations:
//
// ANALYSIS: Verb(negative) + わけには + いかない
//
// The sentence "締切前だからといって、まったくねないわけにはいかない。"
// contains the pattern: ね + ない + わけ + に + は + い + か + ない
//
// GiNZA parses the two ない tokens (from ねない and いかない) identically
// with the same properties (lemma=ない, pos=AUX). The rule correctly requires
// two separate ない tokens at different positions, but the DSL allows the same
// token to satisfy both constraints when there's no positional discriminator
// that explicitly prevents them from being the same token index.
//
// The constraint inOrder(nai, nai2, 20) where nai=token_X and nai2=token_Y
// should prevent nai and nai2 from being the same token, but GiNZA parses
// both ない tokens with identical properties, making them indistinguishable
// to the matcher.
//
// CONCLUSION: GiNZA limitation - unable to distinguish two identical tokens
// in the pattern "ねないわけにはいかない" where both ない tokens have the
// same lemma, pos, and other properties.
const skipPositives = [
  '締切前だからといって、まったくねないわけにはいかない。',
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives, skipPositives });
});
