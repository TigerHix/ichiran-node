import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './かねない.js';
import { BUNPRO_JLPT2 } from './index.js';

// Negative test cases - sentences that should NOT match the かねない grammar rule
// These test similar suffixes and related grammar patterns
const negatives = [
  // かねる (kaneru) - "cannot do, be unable to" (positive form)
  // This is the related grammar with different meaning (inability/hesitation)
  'その提案には賛成しかねます。',
  '判断しかねます。',
  'お答えしかねます。',
  '専門的なプログラムには対応し兼ねる。',
  'ご遠慮しかねます。',
  '納得しかねます。',

  // にくい (nikui) - "hard to, difficult to" (less formal, subjective)
  'この薬は飲みにくい。',
  'この辞書は使いにくい。',
  '彼の声は聞こえにくい。',

  // づらい (zurai) - "physically difficult to do"
  'これは食べづらい。',
  'その靴は歩きづらい。',
  '読みづらい字だ。',

  // がたい (gatai) - "extremely difficult, nearly impossible" (stronger)
  '信じがたい話だ。',
  '捨てがたい思い出。',
  '耐えがたい暑さ。',

  // おそれがある (osore ga aru) - "there is a risk/fear that" (similar meaning but different structure)
  '洪水の恐れがある。',
  '台風の上陸する恐れがある。',

  // かもしれない (kamoshirenai) - "might, may" (neutral possibility)
  '雨が降るかもしれない。',
  '彼は来るかもしれない。',
  '明日は晴れるかもしれない。',

  // 得る/える (eru) - "can, possible to" (as in 〜得る)
  'あり得る話。',
  '起こり得る事故。',
  '成し得ること。',

  // 切れない (kirenai) - "unable to finish/complete"
  '食べきれない。',
  '言いきれない。',

  // を兼ねて (wo kanete) - "serving two purposes simultaneously"
  '運動を兼ねて散歩してきた。',
  '観光を兼ねて出張に行く。',

  // Independent verb use (not as auxiliary)
  // 兼ねる meaning "to combine, to serve as"
  '彼は社長と編集長を兼ねている。',
  '二つの役職を兼ねる。',
  'この部屋は居間と食堂を兼ねている。',

  // Negative forms that aren't かねない
  '兼ねる。',
  '兼ねました。',

  // Similar sounding but unrelated words
  '金がない。',
  '彼にかなう。', // kanau (to match) not kanenai
  '彼女が好きか、ないか。', // ka (question particle) + nai
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
