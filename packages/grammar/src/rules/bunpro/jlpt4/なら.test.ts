import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './なら.js';
import { BUNPRO_JLPT4 } from './index.js';

// Negative examples: similar patterns that should NOT match
const negatives = [
  // に成る (to become) - different grammar
  '彼は先生になる。',

  // なら as in "なら OK" (conditional "if OK") - actually a valid なら
  // Skipping this as it IS a valid use of なら

  // なら attached to で (locative) without being conditional
  // This is tricky - need to verify with actual parsing

  // Other conditionals that are NOT なら
  // 行けば (if I go) - different conditional
  '行ければ、行きます。',
  // 行ったら (when I go / if I went) - different conditional
  '行ったら、連絡してください。',
  // 行くと (when I go) - different conditional
  '行くと、景色がいい。',

  // Sentence-final なら (not our grammar - we match conditional なら)
  // Actually, sentence-final なら can be valid too (elliptical)

  // なら as part of longer form (のならば, ならば) - handled by separate rules
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
