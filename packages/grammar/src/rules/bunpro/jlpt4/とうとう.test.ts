import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './とうとう.js';
import { BUNPRO_JLPT4 } from './index.js';

const negatives = [
  // Similar-sounding adverbs that are not とうとう (finally)
  // どうどう - not a common adverb, would be mispronunciation
  '堂々と演技を見せた。',
  // とんとん - means "smoothly" or "bang bang"
  'とんとん拍子に進んだ。',
  'ドアをとんとん叩いた。',
  // どんどん - means "progressively" or "rapidly"
  'どんどん食べた。',
  '日本語がどんどん上手くなる。',
  // だんだん - means "gradually"
  'だんだん寒くなってきた。',
  '英語がだんだんわかってきた。',
  // Other adverbs with similar meanings but different nuances
  // ずっと - means "continuously"
  'ずっと待っていた。',
  'ずっとそこに住みたい。',
  // いよいよ - means "at last" but with different nuance (anticipation)
  'いよいよ夏が来た。',
  // ついに - means "finally" but focuses on result rather than effort
  'ついに成功した。',
  // やっと - means "finally" but emphasizes difficulty relieved
  'やっと終わった。',
  // もっと - means "more"
  'もっと速く走りたい。',
  // すっと - means "smoothly/quickly"
  'すっと立ち上がった。',
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
