import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './かねる.js';
import { BUNPRO_JLPT2 } from './index.js';

// Negative test cases - sentences that should NOT match the かねる grammar rule
// These test similar suffixes and related grammar patterns
const negatives = [
  // かねない (kanenai) - "might happen, could be possible" (opposite meaning)
  // This is the related grammar with different meaning (possibility of negative outcome)
  'この事故は今後増えるかねない。',
  '彼はそんなことをしかねない。',
  '台風は上陸するかねない。',
  '誤解を招きかねない発言だ。',
  '危うく事故になりかねなかった。',

  // にくい (nikui) - "hard to, difficult to" (less formal, subjective)
  'この薬は飲みにくい。',
  'この辞書は使いにくい。',
  '彼の声は聞こえにくい。',

  // づらい (zurai) - "physically difficult to do"
  'これは食べづらい。',
  'その靴は歩きづらい。',
  '読みづらい字だ。',

  // がたい (gatai) - "extremely difficult, nearly impossible" (stronger than かねる)
  '信じがたい話だ。',
  '捨てがたい思い出。',
  '耐えがたい暑さ。',

  // 切れない (kirenai) - "unable to finish/complete"
  '食べきれない。',
  '言いきれない。',

  // 得ない (enai) - "unable to, cannot" (objective impossibility)
  'あり得ない話。',
  '決してあり得ない。',

  // わけにはいかない - "cannot afford to, impossible to do" (moral/social reasons)
  '約束を破るわけにはいかない。',
  '会社を辞めるわけにはいかない。',

  // を兼ねて (wo kanete) - "serving two purposes simultaneously" (different grammar)
  '運動を兼ねて散歩してきた。',
  '観光を兼ねて出張に行く。',

  // Independent verb use (not as auxiliary)
  // 兼ねる meaning "to combine, to serve as"
  '彼は社長と編集長を兼ねている。',
  '二つの役職を兼ねる。',
  'この部屋は居間と食堂を兼ねている。',

  // Negative forms that aren't かねる
  '兼ねない。',
  '兼ねなかった。',

  // Similar sounding but unrelated words
  '金がある。',
  '彼にかなう。', // kanau (to match) not kaneru
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
