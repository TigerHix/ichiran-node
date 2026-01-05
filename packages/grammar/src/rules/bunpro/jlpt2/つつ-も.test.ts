import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './つつ-も.js';
import { BUNPRO_JLPT2 } from './index.js';

// Negative test cases - sentences that should NOT match the つつ-も grammar rule
const negatives = [
  // ながら (while) - different grammar, less formal
  '歩きながら話す。',
  '考えながら書く。',
  '食べながらテレビを見る。',

  // ながらも (even while) - similar meaning but less formal
  '忙しくなりながらも、練習を続けた。',
  '瘦せながらも、力強い。',

  // くせに (despite) - more critical/blaming tone
  '知っているくせに教えてくれない。',
  'お金がないくせに高いものを買う。',

  // のに (despite/although) - different pattern
  '知っているのに言わない。',
  '雨が降っているのに出かける。',

  // ものの (although/but) - different structure
  'できたものの、満足できない。',
  '行ったものの、会えなかった。',

  // て-form + も (even if/although) - different grammar
  '行っても大丈夫だ。',
  '読んでもわからない。',

  // Simple verb conjugations not related to つつ
  '行きます。',
  '食べました。',
  '勉強している。',

  // Verb + つ +  auxiliary (different pattern)
  '彼を持たせる。',
  '空を飛ぶ。',

  // つ as counter (not part of つつ)
  'りんごを三つ買う。',

  // つ as auxiliary verb (〜てしまう, etc.)
  '食べてしまった。',
  '忘れてしまった。',

  // て-form verbs (not stem form)
  '歩いて話す。',
  '食べてテレビを見る。',
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
