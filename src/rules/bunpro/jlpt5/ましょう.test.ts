import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './ましょう.js';
import { BUNPRO_JLPT5 } from './index.js';

// Negative tests: sentences that should NOT match this rule
const negatives = [
  // Casual volitional よう (not polite ましょう)
  '行こう！',
  '食べよう。',
  'しよう！',
  // Polite present ます (not volitional)
  '行きます。',
  '食べます。',
  'します。',
  // Polite negative ません (not volitional)
  '行きません。',
  '食べません。',
  'しません。',
  // Polite past ました (not volitional)
  '行きました。',
  '食べました。',
  'しました。',
  // Note: ましょうか (e.g., 行きましょうか？) will match this rule since it contains
  // ましょう. The か particle is a separate question marker and doesn't change
  // the grammatical structure of ましょう itself. A separate ましょうか rule can
  // be used to match the question form specifically.
];

describe('bunpro.jlpt5', () => {
  const engine = useSharedEngine([BUNPRO_JLPT5]);
  describeRule(rule, 'JLPT5', BUNPRO_JLPT5.id, engine.get, { negatives });
});
