import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './たって.js';
import { BUNPRO_JLPT2 } from './index.js';

const negatives = [
  // Simple quoting/hearsay って (different grammar - sentence-final quoting)
  '彼は来るって言ってた。',
  // Causative て-form + って (command + quoting)
  '行ってって言った。',
  // だって meaning "because" or "but" at sentence start (different grammar)
  'だって遅刻したんです。',
  'だってお腹が空くから。',
  // Simple te-form + 何でも (not たって pattern)
  '食べて何でもする。',
  // Past tense without conditional meaning
  '彼は言った。それだけだ。',
  '昨日は買った。高いけど。',

  // NOTE: "明日は雨だって。" and "子供だってわかる。" are excluded because:
  // - They have the same structure as positive examples (noun + だって)
  // - The difference between "even if" and "hearsay" is semantic, not structural
  // - "子供だってわかる" could mean either "Even a child understands" (positive) or
  //   "They say a child understands" (negative) - ambiguous without context
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
