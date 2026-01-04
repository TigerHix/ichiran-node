import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './からすると-からすれば.js';
import { BUNPRO_JLPT2 } from './index.js';

// Negative examples: sentences that should NOT match
const negatives = [
  // からして (more subjective, different grammar)
  'あの態度からして、彼は怒っているようだ。',
  '彼の話し方からして、田舎の人らしい。',

  // からいうと (speaking from/in terms of)
  '私の経験からいうと、そんなことはありえない。',

  // からみると (from the viewpoint of/looking at)
  '学生からみると、この値段は高すぎる。',

  // からいえば (if one were to say from)
  '彼の性格からいえば、断るだろう。',

  // からみれば (from the perspective of)
  '親からみれば、心配なのは当然だ。',

  // にしたら (from the perspective of - subjective)
  '私にしたら、それは不公平だ。',

  // ところを見る (judging from the situation/action)
  '彼の様子を見ると、疲れているようだ。',

  // Simple から (from/since - causal)
  '雨だから行かない。',

  // してから (after doing)
  '食事をしてから出かけます。',
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
