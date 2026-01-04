import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './だろう.js';
import { BUNPRO_JLPT5 } from './index.js';

// Negative examples: だろう should NOT match in these cases
const negatives = [
  // Polite form でしょう (not casual だろう)
  'これはペンでしょうか。',
  '今日は雨でしょう。',
  // Different conjecture forms
  'これはペンかもしれない。',
  // Copula だ (different form)
  'これはペンだ。',
  'あなただ。',
];

describe('bunpro.jlpt5', () => {
  const engine = useSharedEngine([BUNPRO_JLPT5]);
  describeRule(rule, 'JLPT5', BUNPRO_JLPT5.id, engine.get, { negatives });
});
