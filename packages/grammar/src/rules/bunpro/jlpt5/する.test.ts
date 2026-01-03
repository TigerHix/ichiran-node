import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './する.js';
import { BUNPRO_JLPT5 } from './index.js';

// Sentences that can't be matched due to GiNZA parsing limitations:
//
// The sentence "君は文法を勉強します。" (You study grammar)
//
// GiNZA parses this as:
//   君(0) は(1) 文法(2) を(3) 勉強します(4)
//   where token 4 is a single VERB with lemma=勉強する
//
// This is because "勉強します" is the polite form of the compound verb
// "勉強する" (to study), which is a dictionary entry in its own right.
//
// Other similar sentences parse differently:
//   "勉強をします" → 勉強(NOUN) + を(PARTICLE) + し(AUX) + ます(AUX)
//   Here "し" has lemma=する and correctly matches standalone する
//
// The discriminator is:
//   - With particle: NOUN + を + します → separate tokens, し has lemma=する
//   - Without particle: NOUN + します → single token, lemma=NOUN+する
//
// We cannot match all します tokens because that would incorrectly match
// ALL suru-compound verbs (勉強する, 掃除する, 料理する, etc.) which are
// separate grammar points.
//
// CONCLUSION: "君は文法を勉強します。" is a suru-compound verb, not
// standalone する. GiNZA limitation prevents matching it without also
// matching unrelated suru-compounds.
const skipPositives = [
  '君は文法を勉強します。', // Suru-compound 勉強する, not standalone する
];

// Negative tests: sentences that should NOT match this rule
const negatives = [
  // Suru-verb compounds (different grammar - these are dictionary verbs, not "する")
  '勉強する。',      // "study" - standalone verb, not "do" + "study"
  '勉強します。',    // polite "study"
  'サッカーする。',  // "play soccer"
  'サッカーします。', // polite "play soccer"
  '掃除する。',      // "clean"
  '掃除します。',    // polite "clean"
  '料理する。',      // "cook"
  '洗濯する。',      // "do laundry"

  // Similar verbs that are not する
  'なる。',          // "to become" (different verb)
  'やる。',          // "to do" (casual alternative, different lemma)
];

describe('bunpro.jlpt5', () => {
  const engine = useSharedEngine([BUNPRO_JLPT5]);
  describeRule(rule, 'JLPT5', BUNPRO_JLPT5.id, engine.get, { negatives, skipPositives });
});
