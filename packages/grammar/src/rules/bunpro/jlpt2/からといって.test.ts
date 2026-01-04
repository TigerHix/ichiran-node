import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './からといって.js';
import { BUNPRO_JLPT2 } from './index.js';

const negatives = [
  // Similar expressions that are NOT からといって
  // から alone - simple reason marker
  '雨が降ったから、行きませんでした。',
  '疲れたから、早く寝ました。',

  // からして - "from the fact that" / "starting with"
  '彼の態度からして、怒っているのがわかる。',
  '顔つきからして、彼は日本人だ。',

  // からには - "since" / "now that"
  'やるからには、最後までやりなさい。',
  '約束したからには、守るべきだ。',

  // からこそ - "precisely because" (emphatic)
  'あなただからこそ、話したんです。',
  '努力したからこそ、成功できたんだ。',

  // からまで - "even from" (uncommon structure)
  '親からまで反対されている。',

  // Sentences with から as particle in different contexts
  '東京から来ました。',
  '朝から晩まで働きます。',

  // ばかりに - negative result (not same as からといって)
  '遅刻したばかりに、電車に乗り遅れた。',

  // だけに - "precisely because"
  '子供だけに、理解できないだろう。',

  // せいで - "because of" (negative outcome)
  '雨のせいで、行けなくなった。',
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
