import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './から見ると.js';
import { BUNPRO_JLPT2 } from './index.js';

// Negative test cases: sentences with から or 見る that should NOT match
const negatives = [
  // Simple から as source/from marker
  '東京から大阪まで新幹線で行きます。',
  '彼から電話がありました。',
  // 見る as a regular verb meaning "to see/watch" (without grammar pattern)
  '彼を見ると、怒っているようだ。',
  '映画を見て、感動した。',
  // て form as "and then" (not viewpoint)
  '家を見て、買うことにした。',
  '彼を見ると、笑った。',
  // Different grammar: からして (judging from X)
  '彼の態度からして、嫌そうだ。',
  // Different grammar: からいうと (speaking in terms of)
  '経験からいうと、それは無理だ。',
  // Different grammar: にしてみれば (from X's standpoint)
  '私にしてみれば、彼は正しい。',
  // Different grammar: からすると (considering/judging from)
  '外国人からすると、珍しい習慣だ。',
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
