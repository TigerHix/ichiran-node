import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './な-adjective-noun.js';
import { BUNPRO_JLPT5 } from './index.js';

const negatives = [
  // i-adjective + noun (no な needed)
  // おいしいピザ (tasty pizza - i-adjective)
  'おいしいピザを食べる。',
  'あついひとがいます。',
  // Adjective at end of sentence (predicative use, not modifying noun)
  'この部屋はきれいだ。',
  'この町は静かです。',
  // Prohibitive な (verb + な = "don't do!")
  '入るな。',
  '走るな。',
  // Noun + noun (no adjective)
  '日本語の本があります。',
  '私の猫です。',
  // i-adjective without な (should not match this rule)
  'あつい日',
  'たかいねだん',
];

// Sentences that can't be matched due to GiNZA parsing limitations:
//
// ANALYSIS: Various na-adjectives in test data
//
// The rule matches the core pattern of na-adjective + な + noun based on
// GiNZA's consistent parsing:
//   - Adjective: pos=ADJ, dep=acl
//   - な: text=な, lemma=だ, pos=AUX, inflectionForm=連体形-一般
//   - Noun: pos=NOUN
//
// All sentences in the test data follow this pattern consistently.
// No skips needed at this time.
const skipPositives: string[] = [];

describe('bunpro.jlpt5', () => {
  const engine = useSharedEngine([BUNPRO_JLPT5]);
  describeRule(rule, 'JLPT5', BUNPRO_JLPT5.id, engine.get, { negatives, skipPositives });
});
