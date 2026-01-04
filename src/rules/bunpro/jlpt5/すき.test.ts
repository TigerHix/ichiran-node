import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './すき.js';
import { BUNPRO_JLPT5 } from './index.js';

// Negative test cases - sentences that should NOT match
const negatives = [
  // 大きい (i-adjective ending in い)
  'この家は大きい。',
  // 可愛い (i-adjective ending in い)
  '赤ちゃんは可愛い。',
  // きれい (fake i-adj, actually na-adj ending in い - different word)
  '水はきれいだ。',
  // すごい (i-adjective ending in い)
  '今日はすごいね。',
  // おいしい (i-adjective ending in い)
  'この料理はおいしい。',
];

describe('bunpro.jlpt5', () => {
  const engine = useSharedEngine([BUNPRO_JLPT5]);
  describeRule(rule, 'JLPT5', BUNPRO_JLPT5.id, engine.get, { negatives });
});
