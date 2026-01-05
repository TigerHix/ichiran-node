import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './どころではない.js';
import { BUNPRO_JLPT2 } from './index.js';

// Negative test cases - sentences that should NOT match the どころではない grammar rule
const negatives = [
  // どころか (far from, on the contrary - JLPT3) - different grammar
  '痩せるどころか、太ってしまった。',
  '静かどころか、うるさいくらいだ。',
  '彼は英語どころか、フランス語も話せる。',
  '雨が止むどころか、もっと激しくなってきた。',

  // Simple ではない copula negation - lacks どころ
  '彼は学生ではない。',
  'これは私の本ではない。',
  '今日は休日ではない。',
  'それは本当ではない。',

  // として (toshite) - "as" (different pattern)
  '学生として勉強する。',
  '友達として助ける。',
  '先生として教える。',
  '彼は医者として働いている。',

  // にしては (nishite) - "considering, for" (different pattern)
  '子供にしては詳しい。',
  '新人にしてはよくやっている。',
  'この製品は安いにしては品質が良い。',
  '彼女にしてはとても静かだ。',

  // Similar sounding but unrelated patterns
  // ところだった (was just about to...)
  '車に乗るところだった。',
  '帰るところだったのに、電話が鳴った。',
  '寝るところだった。',

  // ところで (by the way - conversation starter)
  'ところで、明日の予定は？',
  'ところで、彼は来るのかな？',

  // Simple usage of どころ meaning "place"
  'ここはいいところだ。',
  '私の故郷は静かなところです。',
  'どこかいいところを知っていますか？',

  // Sentence uses どころ but in different grammar pattern
  '彼が来るどころか、連絡もしない。',
  '雨が止むどころか、ひどくなっている。',
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
