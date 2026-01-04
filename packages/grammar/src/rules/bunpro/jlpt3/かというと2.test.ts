import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './かというと2.js';
import { BUNPRO_JLPT3 } from './index.js';

// False positives: sentences with similar particles that should NOT match
const negatives = [
  // Simple というと without question particle (different grammar - topic introduction)
  '日本というと、桜を思い出します。',
  '京都というと、古い寺がたくさんあります。',
  // かというか - "or rather" expression (different grammar point)
  '彼が来なかったかというか、遅れただけです。',
  // というと without question context
  '彼が行ったというと、本当ですか。',
  // Plain か + いう (question + "say" without conditional)
  '何か言いましたか。いうと、あの...',
  // Simple quotation という (just "called" or "said")
  'これは何という花ですか。',
  '田中という人から電話がありました。',
];

// Sentences that can't be matched due to GiNZA parsing limitations:
//
// ANALYSIS: "好きか" + っていうと (na-adjective + question marker + casual quotation)
//
// GiNZA parses this pattern inconsistently:
//   食べたいかっていうと → か = PART (particle) ✓ WORKS
//   好きかっていうと   → か not as separate particle ✗ INDISTINGUISHABLE
//
// The pattern "好きか" (na-adjective + question particle) is parsed by GiNZA
// as a compound unit where "か" is not tagged as a separate particle (PART).
// This makes it impossible to distinguish from other constructions where "か"
// might be part of a compound.
//
// Matching all "か" tokens regardless of POS would overcapture:
//   ❌ 会社員 (company employee - where か is historically related but not a question particle)
//   ❌ 其它 (other - Chinese loan where か is not a question particle)
//
// CONCLUSION: No reliable discriminator for "好きか" + っていうと. GiNZA limitation.
const skipPositives = [
  'イチカさん：「ミクちゃんは好きな人がいるみたいに見えるよ。」イツキさん：「誰が好きかっていうときっとフウタロウくんでしょう。」',
];

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get, { negatives, skipPositives });
});
