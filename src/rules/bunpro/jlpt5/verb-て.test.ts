import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './verb-て.js';
import { BUNPRO_JLPT5 } from './index.js';

const negatives = [
  // Dictionary form verbs (終止形, not 連用形)
  '本を読む。',
  '私は行く。',
  // Past tense verbs (連用形 but dep=root with AUX, not SCONJ)
  '本を読んだ。',
  '行った。',
  // Adjective + て (different grammar rule - adjective-て-b)
  '綺麗でいい。',
  '大きくて重い。',
  // Noun + で conjunction (copula, not verb - handled by adjective-て-b)
  '漫画家で俳優です。',
];

// Sentences that can't be matched due to GiNZA parsing limitations:
//
// ANALYSIS: Standalone て form cloze fragments
//
// GiNZA parses standalone て forms inconsistently:
//   およいで (sentence fragment) → VERB [連用形-イ音便] + SCONJ [dep=mark] ✓ WORKS
//   よんで (sentence fragment) → NOUN [連用形-撥音便] + AUX [dep=fixed] ✗ INCORRECT
//
// When GiNZA incorrectly parses the verb as NOUN, we can't match using
// pos=VERB constraint because GiNZA doesn't assign it.
//
// To match these cases, we would need to either:
//   1. Match by surface text ending in "で" → would overcapture other patterns
//   2. Match both VERB and NOUN with 連用形 → would match non-verb patterns
//
// CONCLUSION: GiNZA limitation for standalone cloze fragments with specific parsing.
const skipPositives = [
  'む）→ よんで',
  'ぐ）→ およいで',
  'ぶ） → よんで',
];

describe('bunpro.jlpt5', () => {
  const engine = useSharedEngine([BUNPRO_JLPT5]);
  describeRule(rule, 'JLPT5', BUNPRO_JLPT5.id, engine.get, { negatives, skipPositives });
});
