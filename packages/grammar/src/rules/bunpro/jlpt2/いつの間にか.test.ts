import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './いつの間にか.js';
import { BUNPRO_JLPT2 } from './index.js';

// Negative examples: sentences that should NOT match
const negatives = [
  // あっという間に (attonomani) - similar meaning but different expression
  // Emphasizes speed rather than lack of awareness
  '楽しいことをしているとあっという間に時間が過ぎるよね。',

  // いつ (itsu) - "when" (question word, not the expression)
  'いつ日本に来ましたか？',
  'いつ帰りますか？',

  // 間に (mani) - "during/while" (different grammar)
  '夏休みの間に本を読む。',

  // 突然/急に (suddenly) - different adverb
  '突然雨が降ってきた。',
  '急に停車した。',

  // つい (tui) - "inadvertently" (different nuance)
  'つい食べすぎてしまった。',

  // いきなり (ikinari) - "suddenly/all of a sudden"
  'いきなり泣き出した。',

  // たちまち (tachimachi) - "immediately/in a flash"
  '雨がたちまち降り出した。',
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
