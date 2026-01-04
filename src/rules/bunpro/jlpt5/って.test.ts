import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './って.js';
import { BUNPRO_JLPT5 } from './index.js';

// Sentences that should NOT match the quotation って rule
const negatives = [
  // って as topic marker for hearsay (different grammar - んだって, だって)
  // Note: These might be the same grammar point in casual speech, but the
  // formal version uses different patterns
  '明日は雨だって。',
  '彼が来るんだって。',

  // って as contraction of という in apposition (noun naming)
  // "This thing called 'computer'" vs "He said 'computer'"
  'パソコンって機械。',
  '日本って国。',

  // Avoid capturing quotative と (formal version)
  '「行きます」と言った。',
  '「危ない」と言った。',

  // Conditional って (very rare, but theoretically possible)
  // These should use conditional て forms instead
];

describe('bunpro.jlpt5', () => {
  const engine = useSharedEngine([BUNPRO_JLPT5]);
  describeRule(rule, 'JLPT5', BUNPRO_JLPT5.id, engine.get, { negatives });
});
