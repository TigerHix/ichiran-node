import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './そんなに.js';
import { BUNPRO_JLPT4 } from './index.js';

// Negative examples: similar patterns that should NOT match
const negatives = [
  // そんな (such a/that kind of - adjectival, not adverbial)
  'そんなこと言わないで。',

  // Particle に (different grammar - directional/locative)
  'そこに行きたい。',
  '彼に会いたい。',
  '東京に行きました。',

  // そんな + noun (attributive use, not adverbial)
  'そんな人はいない。',

  // Nominal そんな (followed by particle, not copula)
  'そんなをするな。',
];

// Positive sentences to skip (not actually そんなに examples)
//
// The Bunpro JSON for そんなに includes example sentences with related
// kosoado words (こんなに, あんなに, どんなに) to demonstrate the pattern.
// However, this specific rule should only match そんなに, not the variants.
// Each variant is a separate lexical item and would need its own rule if desired.
//
// ANALYSIS:
// - こんなに: 'this much' (close to speaker) - different lemma
// - あんなに: 'that much' (over there) - different lemma
// - どんなに: 'how much' (interrogative) - different lemma
//
// GiNZA parses all variants identically (ADJ + AUX with inflectionForm=連用形-ニ),
// but the discriminator is the lemma of the first token (こんな vs そんな vs あんな vs どんな).
// Matching all of them would require a more generic rule that could overcapture.
const skipPositives = [
  'こんなにもらってもいいんですか？',
  'あんなに食べたのにまだお腹が空いているの？',
  'どんなに頑張っても、私は先輩みたいにはなれないと思う。',
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives, skipPositives });
});
