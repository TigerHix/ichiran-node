import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './からある.js';
import { BUNPRO_JLPT1 } from './index.js';

const negatives = [
  // から alone (without ある/いる/の) - "because" or "from"
  'あなたがいるから、安心できます。',
  '毎日努力をしたから、一番になれた。',
  '東京から大阪へ行く。',
  '8時から仕事を始めます。',
  '日本から来ました。',
  '彼からもらった。',
  '昨日から雨が降っている。',

  // からして (karashite) - "judging from, based on"
  '彼の性格からして、彼と一緒に住むことは無理だろう。',
  '値段からして、このお店は新鮮な食材を使っているに違いない。',

  // からすると・からすれば (karasuruto/karasureba) - more objective judgment
  '彼の話からすると、嘘をついているようだ。',
  'この結果からすれば、成功は難しいだろう。',

  // 以上① (ijou1) - "more than or equal to" (different grammar pattern)
  '１００人以上が来ました。',
  '３００日以上トレーニングが出来た。',

  // Similar patterns with different meaning
  // からみて (karamite) - "judging from the perspective of"
  '私の立場から見て、それは間違いだ。',

  // にしては (nishite) - "considering, for"
  '子供にしては詳しい。',
  '新人にしてはよくやっている。',

  // から as case marking particle + ある (different dependency structure)
  // These should not match because から and ある are not connected
];

describe('bunpro.jlpt1', () => {
  const engine = useSharedEngine([BUNPRO_JLPT1]);
  describeRule(rule, 'JLPT1', BUNPRO_JLPT1.id, engine.get, { negatives });
});
