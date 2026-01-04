import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './かと思ったら-かと思うと.js';
import { BUNPRO_JLPT2 } from './index.js';

// Negative tests: similar-looking patterns that should NOT match
const negatives = [
  // Simple volitional + と思う (different meaning - "I think that...")
  '彼は来ると思います。',
  '明日は雨が降ると思う。',
  // か as question particle + 思う (different structure)
  '来るかと思いますか？',
  'どう思いますか？',
  // Simple quotation + と思う (not the grammar pattern)
  '彼は「行く」と言いました。',
  // 思う without the conditional context
  '昨日のことを思います。',
  '彼を思う気持ち。',
  // Different grammar: かどうか (whether or not)
  '行くかどうかわかりません。',
  'できるかどうか試してみます。',
  // Different grammar: かというと (if I were to say)
  '理由かというと、簡単です。',
  // Different grammar: かと思えば (on the other hand)
  '日本は狭いかと思えば、広い alsoある。',
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
