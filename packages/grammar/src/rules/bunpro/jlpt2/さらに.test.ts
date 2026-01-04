import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './さらに.js';
import { BUNPRO_JLPT2 } from './index.js';

// Negative examples: similar conjunctions/adverbs that should NOT match
const negatives = [
  // また - "also, again" (less formal)
  'また時間が増えた。',
  'また行きます。',
  'また会いたいですね。',

  // その上 - "besides, in addition, furthermore" (conjunction)
  'その上、給与が上がり、とても助かります。',
  'その上、基準が高くなる。',

  // しかも - "moreover, furthermore" (emphasizing surprising addition)
  'この店は安い。しかも美味しい。',
  '彼は優秀だ。しかも親切だ。',

  // なお - "furthermore, still" (formal conjunction)
  'なお、詳細は後ほどお知らせします。',
  'なお、ご不明な点があればお問い合わせください。',

  // ますます - "more and more, increasingly" (emphasizing progression)
  'ますます寒くなってきた。',
  'ますます面白くなる。',

  // いっそう - "more, even more" (comparative intensifier)
  'いっそう努力する。',
  'いっそう良い結果が出る。',

  // もっと - "more" (simple comparative)
  'もっと詳しく説明します。',
  'もっと美味しくなった。',

  // どんどん - "more and more, rapidly" (emphasizing speed)
  'どんどん増えている。',
  'どんどん良くなる。',
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
