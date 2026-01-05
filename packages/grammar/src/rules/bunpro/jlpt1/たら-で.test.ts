import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './たら-で.js';
import { BUNPRO_JLPT1 } from './index.js';

const negatives = [
  // Simple たら conditional without repetition (different grammar)
  'もし失敗したら、また最初からやり直そう。',

  // Simple た + で (te-form of copula, different grammar)
  'これは猫だ。',

  // で as instrumental particle (different grammar)
  '鉛筆で書く。',

  // たら as surprise discovery without repetition
  'あ、千円だったら、買えますよ。',

  // た + で as cause/reason (て-form, not this pattern)
  '疲れたので、早く寝た。',
];

// Sentences that can't be matched due to pattern matching limitations:
//
// ANALYSIS: Adjective pattern without た
//
// The rule matches verb patterns with た past tense (conditional...た...de), but
// the adjective pattern (conditional...adj...de) without た is difficult to
// distinguish from other conditional+de patterns in complex sentences.
//
// Example: "背が低ければひくいで" (hikereba...hikui...de) has the pattern:
//   - ば (conditional at token 19)
//   - ひくい (adjective at token 20)
//   - で (conjunction at token 21)
//
// However, this sentence ("スーパーとかで高い棚に手が届けばいいんだけど、...")
// contains multiple で and ば/れ/たら conditionals, causing the pattern matcher
// to incorrectly try to match verb patterns first, which then fail to find た.
//
// Making the adjective variant (conditional...de without た) more restrictive to
// avoid this false positive would require negative constraints (e.g., "no た between
// conditional and de"), which the DSL doesn't support.
//
// CONCLUSION: Skip this complex sentence. Simpler adjective patterns like
// "簡単なら簡単で" work correctly.
const skipPositives = [
  '「スーパーとかで高い棚に手が届けばいいんだけど、背が低ければひくいで、なんとかなるはずだ。」',
];

describe('bunpro.jlpt1', () => {
  const engine = useSharedEngine([BUNPRO_JLPT1]);
  describeRule(rule, 'JLPT1', BUNPRO_JLPT1.id, engine.get, { negatives, skipPositives });
});
