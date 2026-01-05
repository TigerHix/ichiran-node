import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './しかしながら.js';
import { BUNPRO_JLPT2 } from './index.js';

// Negative test cases - sentences that should NOT match the しかしながら grammar rule
const negatives = [
  // Similar conjunctions (less formal variants)
  'しかし、私は行かなかった。',
  'だけど、私は行かなかった。',
  'でも、それは違う。',
  'だが、私は反対だ。',
  'ですが、間違っています。',
  'ですが、それは違うと思います。',
  'ただし、例外があります。',
  'もっとも、例外があります。',
  'ところが、彼は来なかった。',

  // が as particle (conjunction "but/and")
  '彼は学生ですが、アルバイトをしています。',
  '雨が降っていたが、出かけた。',
  '彼は来なかった。が、電話はあった。',

  // しかし without ながら (different grammar - less formal)
  '彼は努力した。しかし、失敗した。',
  'しかし、私は反対です。',
  '予算がない。しかし、計画は進める。',

  // ながら meaning "while" (verb + ながら)
  '食事をしながらテレビを見る。',
  '歩きながら考えた。',
  '働きながら学校に通う。',
  '音楽を聴きながら勉強する。',

  // Noun + ながら (despite/while being)
  '初心者ながら、上手にやる。',
  '子供ながら、立派な意見だ。',
  '日本人ながら、日本語を話せない。',

  // それなのに (conversational "and yet")
  '約束した。それなのに来なかった。',
  '勉強した。それなのにテストが悪かった。',
  'それなのに、彼は来なかった。',

  // それにしても (conversational "even so")
  'それにしても、遅いですね。',
  '高いのは分かった。それにしても、買いすぎだ。',

  // それでも (even so)
  '雨が降っている。それでも、行く。',
  '失敗した。それでも、諦めない。',

  // Note: 然しながら and 併しながら are kanji variants of しかしながら
  // and should be matched by this rule (they are NOT negatives)

  // Negative: しかし as standalone (not しかしながら)
  'しかしですね、これは困った。',

  // Negative: ながら in different contexts
  '彼ながらの意見だ。', // "typical of him"
  '昔ながらのやり方。', // "old-fashioned"
  '生きながら死んでいる。', // "dead while alive"
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
