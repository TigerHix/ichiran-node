import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './いよいよ.js';
import { BUNPRO_JLPT2 } from './index.js';

const negatives = [
  // Similar-sounding adverbs that are not いよいよ
  // いよいよ can be written as 愈々, but both have same reading
  // Other adverbs with similar meanings but different nuances:
  // ついに - means "finally" but more formal
  'ついに成功した。',
  'ついに春が来た。',
  // やっと - means "finally" but emphasizes relief
  'やっと終わった。',
  'やっと会えた。',
  // とうとう - means "finally, after all" with effort focus
  'とうとう雨が降った。',
  'とうとう負けた。',
  // だんだん - means "gradually"
  'だんだん寒くなってきた。',
  '英語がだんだんわかってきた。',
  // どんどん - means "rapidly, more and more"
  'どんどん食べた。',
  '日本語がどんどん上手くなる。',
  // ますます - means "more and more" (similar to one meaning of いよいよ)
  'ますます頑張ります。',
  'ますます寒くなる。',
  // いっそう - means "more, even more"
  'いっそう頑張ろう。',
  'いっそう美味しくなった。',
  // もっと - means "more"
  'もっと速く走りたい。',
  'もっと勉強しなければならない。',
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
