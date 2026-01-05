import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './かねない.js';
import { BUNPRO_JLPT2 } from './index.js';

// Negative test cases - sentences that should NOT match the かねない grammar rule
// These test similar suffixes and related grammar patterns
const negatives = [
  // かねる (kaneru) - "cannot, unable to" (opposite meaning - inability)
  // This is the related grammar with different meaning (hesitation/inability)
  'その提案には賛成しかねます。',
  '判断しかねます。',
  'お答えしかねます。',
  '専門的なプログラムには対応し兼ねる。',
  '彼は社長と編集長を兼ねている。',
  '二つの役職を兼ねる。',
  'この部屋は居間と食堂を兼ねている。',

  // にくい (nikui) - "hard to, difficult to" (less formal, subjective)
  'この薬は飲みにくい。',
  'この辞書は使いにくい。',
  '彼の声は聞こえにくい。',

  // づらい (zurai) - "physically difficult to do"
  'これは食べづらい。',
  'その靴は歩きづらい。',
  '読みづらい字だ。',

  // がたい (gatai) - "extremely difficult, nearly impossible"
  '信じがたい話だ。',
  '捨てがたい思い出。',
  '耐えがたい暑さ。',

  // 得る・得ない (eru/enai) - potential/possibility
  'あり得ない話。',
  '決してあり得ない。',
  'そんなことはあり得る。',

  // おそれがある (osore ga aru) - "there is a fear/risk that" (similar meaning but different structure)
  '台風が上陸するおそれがある。',
  '事故になるおそれがある。',

  // かもしれない (kamoshirenai) - "might, may" (general possibility, less negative)
  '明日は雨かもしれない。',
  '彼は来るかもしれない。',
  'これは間違いかもしれない。',

  // なくはない (naku wa nai) - "not impossible, can be done"
  'できなくはない。',
  'わからなくはない。',

  // わけがない (wake ga nai) - "no way, impossible"
  'そんなわけがない。',
  '失敗するわけがない。',

  // を兼ねて (wo kanete) - "serving two purposes simultaneously"
  '運動を兼ねて散歩してきた。',
  '観光を兼ねて出張に行く。',

  // Independent verb use (not as auxiliary)
  // 兼ねる/兼ねない meaning "to combine, by chance"
  '彼は二つの役職を兼ねている。',
  '偶然にも彼と会った。', // guuzen - by chance (different word)
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
