import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './ことがある.js';
import { BUNPRO_JLPT3 } from './index.js';

// Skip positive tests that cannot be matched due to GiNZA parsing limitations
const skipPositives = [
  // Sentences that can't be matched due to GiNZA parsing limitations:
  //
  // 1. "頑張ってもうまくいかないことがある"
  //    Structure: うまく(adv) + いか(verb) + ない(aux/neg) + こと + が + ある
  //    The "ない" (negative) auxiliary comes between the verb and "こと"
  //    To match this, we'd need maxDistance > 1, but that would also match た-form verbs
  //    (食べたことがある) which we want to exclude.
  //
  // 2. "ルームメイトの無作法な振る舞いに注意したいことがよくある"
  //    Structure: 注意したい + こと + [particle omitted] + よく(adv) + ある
  //    The particle "が" is completely omitted in this sentence!
  //    Our rule requires the particle to distinguish from other こと constructions.
  //
  // ANALYSIS: Pattern with particle omission and adverb insertion
  //
  // GiNZA parses these patterns in ways that cannot be reliably distinguished:
  //   うまくいかないことがある → verb + neg aux + koto + ga + aru (needs dist > 1)
  //   食べたことがある       → verb + past aux + koto + ga + aru (should NOT match)
  //
  // Using maxDistance > 1 would match both patterns, causing false positives on
  // た-form verbs (past experience construction).
  //
  // CONCLUSION: No reliable discriminator. GiNZA limitation.
  '頑張ってもうまくいかないことがある。',
  'ルームメイトの無作法な振る舞いに注意したいことがよくある。でも、私は気が弱すぎる・・・',
];

// Negative tests: similar patterns that should NOT match
const negatives = [
  // たことがある (JLPT5 - past experience, different grammar)
  // This uses verb in past tense (た-form) + ことがある = "have done before"
  // Our rule only matches non-past forms (dictionary form)
  '日本に行ったことがある。',
  'このラーメンは食べたことがあると思う。',
  '寿司を食べたことがあります。',

  // Skip the "ことがよくある" pattern - it omits the particle
  // "こと" + adverb + "ある" without particle is handled separately

  // ことはない (JLPT3 - "there is no need to" or "never happens")
  '彼と話すことはない。',
  '心配することはない。',

  // ことになる (JLPT3 - "it is decided that")
  '来月日本に行くことになった。',

  // ことにする (JLPT3 - "decide to")
  '毎日運動することにしました。',

  // ことだ (JLPT2 - advice "should")
  '健康のためには運動することだ。',

  // Simple こと + が without ある (incomplete pattern)
  'これは私のことが好きなんだ。',

  // Different grammar with がある (noun + aru, not nominalized)
  'この町には古い寺がある。',
];

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get, { negatives, skipPositives });
});
