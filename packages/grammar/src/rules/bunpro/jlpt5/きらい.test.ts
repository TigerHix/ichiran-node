import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './きらい.js';
import { BUNPRO_JLPT5 } from './index.js';

// Negative test cases - sentences that should NOT match
const negatives = [
  // いや (different word, similar meaning)
  'いやな仕事だ。',
  // 大きい (i-adjective ending in い)
  'この家は大きい。',
  // 可愛い (i-adjective ending in い)
  '赤ちゃんは可愛い。',
  // 嫌う (verb "to dislike", not the adjective)
  '彼は野菜を嫌う。',
  // きれい (fake i-adj, actually na-adj ending in い)
  '水はきれいだ。',
];

describe('bunpro.jlpt5', () => {
  const engine = useSharedEngine([BUNPRO_JLPT5]);
  describeRule(rule, 'JLPT5', BUNPRO_JLPT5.id, engine.get, { negatives });
});
