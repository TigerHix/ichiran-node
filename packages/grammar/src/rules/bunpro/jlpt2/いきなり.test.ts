import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './いきなり.js';
import { BUNPRO_JLPT2 } from './index.js';

// Negative test cases - sentences that should NOT match the いきなり grammar rule
const negatives = [
  // Similar adverbs with different meanings
  // 急に (kyuu ni) - "suddenly" but more neutral/descriptive
  '急に雨が降ってきた。',
  '彼は急に立ち上がった。',
  '急に悲しくなった。',

  // 突然 (totsuzen) - "suddenly, unexpectedly" (more formal)
  '突然の訪問に驚いた。',
  '彼は突然部屋を出て行った。',
  '突然気を失った。',

  // たちまち (tachimachi) - "immediately, in a flash" (emphasizes speed)
  'たちまち広まった。',
  '雨はたちまち止んだ。',
  'たちまち売り切れた。',

  // いよいよ (iyoiyo) - "at last, finally" (anticipation)
  'いよいよ夏が来た。',
  'いよいよ試験だ。',
  'いよいよ始まる。',

  // ついに (tsui ni) - "finally" (focuses on result)
  'ついに成功した。',
  'ついに会えた。',
  'ついに終わった。',

  // やっと (yatto) - "finally" (emphasizes difficulty relieved)
  'やっと終わった。',
  'やっと着いた。',
  'やっと理解できた。',

  // だんだん (dandan) - "gradually"
  'だんだん寒くなってきた。',
  '英語がだんだんわかってきた。',
  'だんだん上手くなる。',

  // どんどん (dondon) - "rapidly, more and more"
  'どんどん食べた。',
  '日本語がどんどん上手くなる。',
  'どんどん進んだ。',

  // ずっと (zutto) - "continuously" or "far" (depending on context)
  'ずっと待っていた。',
  'ずっとそこに住みたい。',
  'ずっと速く走りたい。',

  // とんとん (tonton) - "smoothly" or sound effect
  'とんとん拍子に進んだ。',
  'ドアをとんとん叩いた。',
  '売上がとんとん伸びた。',

  // いちいち (ichiichi) - "one by one, every single"
  'いちいち説明するのは面倒だ。',
  'いちいち気にしないで。',
  'いちいち確認した。',

  // おのおの (onoono) - "each, respectively"
  'おのおの違った道を歩む。',
  'おのおの意見を述べる。',
  'おのおのの責任を果たす。',

  // Related grammar: 〜出す (verb suffix meaning "start to")
  '泣き出した。',
  '動き出した。',
  '笑い出した。',

  // Other unrelated adverbs
  'もっと速く走りたい。',
  'すっと立ち上がった。',
  'さっと隠れた。',
  'ばっと広がった。',
  'はっと気づいた。',
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
