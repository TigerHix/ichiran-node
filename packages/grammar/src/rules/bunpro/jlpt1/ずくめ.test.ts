import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './ずくめ.js';
import { BUNPRO_JLPT1 } from './index.js';

// Negatives: Sentences that look similar but should NOT match
const negatives = [
  // づくし - different suffix meaning "all sorts of" (should not match)
  '国づくしの偉い人が集まった。',
  // Regular noun compounds without ずくめ suffix
  '黒い服を着た人が来た。',
  '規則を守ることが大切です。',
  '仕事が忙しいです。',
];

describe('bunpro.jlpt1', () => {
  const engine = useSharedEngine([BUNPRO_JLPT1]);
  describeRule(rule, 'JLPT1', BUNPRO_JLPT1.id, engine.get, { negatives });
});
