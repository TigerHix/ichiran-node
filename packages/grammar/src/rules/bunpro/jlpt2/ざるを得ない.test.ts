import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './ざるを得ない.js';
import { BUNPRO_JLPT2 } from './index.js';

// Negative test cases - sentences that should NOT match the ざるを得ない grammar rule
// These test similar suffixes and related grammar patterns
const negatives = [
  // ざる alone (classical negative attributive form, different grammar)
  // ざる modifying a noun vs ざるを得ない (full grammatical construction)
  '知られざる名作だ。',
  'それは想像せざる事態だ。',
  '過ぎ去りし日の、帰らざる者たち。',

  // 得ない alone (cannot, unable to - objective impossibility)
  // Different from ざるを得ない (have no choice but to)
  'そんなことはあり得ない。',
  '決してあり得ない。',
  '考え得る限りの手段を講じる。',

  // 得ない with verb (different meaning)
  '成功し得ない。',

  // Simple negation with ない (not related to ざるを得ない)
  '行かない。',
  '食べない。',

  // にくい (nikui) - "hard to, difficult to" (less formal)
  'この薬は飲みにくい。',
  'この辞書は使いにくい。',

  // づらい (zurai) - "physically difficult to do"
  'これは食べづらい。',
  'その靴は歩きづらい。',

  // がたい (gatai) - "extremely difficult, nearly impossible"
  '信じがたい話だ。',
  '捨てがたい思い出。',

  // かねる (kaneru) - "unable to, hesitant to" (polite refusal)
  'その提案には賛成しかねます。',
  '判断しかねます。',

  // かねない (kanenai) - "might happen, could be possible"
  'この事故は今後増えるかねない。',
  '彼はそんなことをしかねない。',

  // ずにはいられない - "cannot help but do" (emotional compulsion)
  // vs ざるを得ない (external necessity)
  '泣かずにはいられなかった。',
  '笑わずにはいられない。',

  // ないわけにはいかない - "cannot not do" (moral/social obligation)
  // vs ざるを得ない (external necessity)
  '約束を守らないわけにはいかない。',
  '会社を辞めるわけにはいかない。',

  // てならない - "very, extremely" (emotional intensity)
  '会いたくてならない。',
  '心配でたまらない。',

  // Similar patterns but without ざる
  '変更しなければならない。',
  '従う必要がある。',
  '買うしかない。',
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
