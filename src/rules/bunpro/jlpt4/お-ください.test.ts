import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './お-ください.js';
import { BUNPRO_JLPT4 } from './index.js';

// Negative tests: sentences that should NOT match
const negatives = [
  // Regular てください without honorific prefix
  '書いてください',
  '読んでください',
  '待ってください',

  // お + verb + してください (humble form, not honorific)
  'お待ちしてください',
  'お書きしてください',

  // Just ください alone or with different prefixes
  'ください',
  'どうぞください',
];

// Sentences that can't be matched due to GiNZA parsing limitations:
//
// ANALYSIS: Honorific お-ください with causative verb form (聞かせる)
//
// GiNZA parses hiragana-only text inconsistently:
//   お聞かせください (kanji) → お + 聞か + せ + ください ✓ WORKS
//   おきかせください (hiragana) → おき (lemma=おく) + か + せ + ください ✗ INDISTINGUISHABLE
//
// The kanji version correctly parses:
//   お (NOUN, lemma=お, dep=compound)
//   聞か (VERB, lemma=聞く, inflection=未然形-一般)
//   せ (AUX, lemma=せる)
//   ください (AUX, lemma=くださる)
//
// But the hiragana version incorrectly parses:
//   おき (VERB, lemma=おく, dep=advcl) - completely different lemma!
//   か (PART, lemma=か, dep=mark)
//   せ (VERB, lemma=する)
//   ください (AUX, lemma=くださる)
//
// The hiragana version doesn't have a separate お prefix token (it's merged into おき),
// and the verb lemma is wrong (おく instead of 聞く).
// There's no discriminator that can identify this as お-ください without
// incorrectly matching unrelated patterns.
//
// CONCLUSION: No reliable discriminator for hiragana-only text. GiNZA limitation.
const skipPositives = [
  'ご感想をおきかせください。',
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives, skipPositives });
});
