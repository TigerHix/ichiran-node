import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './のなかで-がいちばん.js';
import { BUNPRO_JLPT5 } from './index.js';

// Negative tests: sentences that should NOT match this rule
const negatives = [
  // Simple location の中で without superlative
  '部屋の中で本を読みます。',
  '袋の中に何がありますか。',
  // Different で usage (instrumental, not scope)
  '一番早く行きました。',
  // 一番 without が subject marker
  'りんごが一番おいしいです。',
  // Noun + で without context
  '日本で一番高い山は富士山です。',
];

describe('bunpro.jlpt5', () => {
  const engine = useSharedEngine([BUNPRO_JLPT5]);
  describeRule(rule, 'JLPT5', BUNPRO_JLPT5.id, engine.get, { negatives });
});
