import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './たくさん.js';
import { BUNPRO_JLPT5 } from './index.js';

// Negative examples: たくさん should NOT match in these cases
const negatives = [
  // Other similar quantity words that aren't たくさん
  'これはとても高いです。',           // とても (very) - different adverb
  'これはかなり高いです。',           // かなり (considerably) - different adverb
  'これはすごく高いです。',           // すごく (extremely) - different adverb
  'これはちょっと高いです。',          // ちょっと (a little) - different adverb
  '多すぎます。',                      // 多い (many) as い-adjective - different word
];

describe('bunpro.jlpt5', () => {
  const engine = useSharedEngine([BUNPRO_JLPT5]);
  describeRule(rule, 'JLPT5', BUNPRO_JLPT5.id, engine.get, { negatives });
});
