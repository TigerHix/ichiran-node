import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './たり-たりする.js';
import { BUNPRO_JLPT5 } from './index.js';

const negatives = [
  // Simple て form (different grammar - verb-て)
  'テレビを見て、寝る。',
  '勉強をして、掃除をした。',
  // Past tense verbs without り
  'テレビを見た。',
  '勉強をした。',
  // Dictionary form verbs (not past form)
  'テレビを見る。',
  '勉強をする。',
  // Verb stem + たり without final する
  'テレビを見たり、寝たり。',
  // Noun + たり (without suru-verb pattern)
  '本たり、ノートたり買う。',
];

// Sentences that can't be matched due to GiNZA parsing limitations:
//
// ANALYSIS: Noun + し + たり (two nouns in suru-verb pattern)
//
// GiNZA parses this pattern inconsistently:
//   勉強 (VERB) + し (AUX) + たり (PART, dep=mark) ✓ WORKS
//   スポーツ (NOUN) + し (AUX) + たり (ADP, dep=case) ✗ CANNOT MATCH
//
// When a noun is used as a suru-verb (like スポーツする), GiNZA parses it as:
//   - NOUN + し (AUX, lemma=する) + たり (ADP, dep=case)
//
// The noun helper requires specific constraints (text, lemma, or specific features).
// Using noun({}) without constraints doesn't work - the DSL requires at least one
// constraint for dispatch.
//
// To match noun+noun patterns like "スポーツしたり、ゲームしたりしたい", we would need:
//   1. A way to match any NOUN with し AUX attached → requires noun() without constraints
//   2. Or match by specific lemmas → would overcapture only sports-related nouns
//
// Additionally, this sentence has たい (desire) attached to する, making it even more
// specific. The pattern is essentially "NOUN+たり + NOUN+たり + したい".
//
// CONCLUSION: Cannot match noun+noun たり patterns due to DSL constraint requirements.
// This is a limitation of the current DSL design - tokens without text/lemma constraints
// cannot be matched directly.
const skipPositives = [
  'スポーツしたり、ゲームしたりしたい。',
];

describe('bunpro.jlpt5', () => {
  const engine = useSharedEngine([BUNPRO_JLPT5]);
  describeRule(rule, 'JLPT5', BUNPRO_JLPT5.id, engine.get, { negatives, skipPositives });
});
