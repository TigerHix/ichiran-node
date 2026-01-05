import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './からには.js';
import { BUNPRO_JLPT2 } from './index.js';

// Negative test cases - sentences that should NOT match the からには grammar rule
const negatives = [
  // から alone (without には) - simple "because" (different nuance)
  'あなたがいるから、安心できます。',
  '毎日努力をしたから、一番になれた。',
  '雨が降っているから、傘を持っていった。',
  '疲れていたから、早く寝た。',
  '勉強したから、試験に合格した。',

  // から as "from" (origin/source)
  '東京から大阪へ行く。',
  '8時から仕事を始めます。',
  '日本から来ました。',
  '彼からもらった。',
  '昨日から雨が降っている。',

  // からして (judging from, based on) - different grammar
  '彼の性格からして、彼と一緒に住むことは無理だろう。',
  '値段からして、このお店は新鮮な食材を使っているに違いない。',
  'このゲームは名前からしてつまらなそうだ。',

  // からすると・からすれば (more objective judgment)
  '彼の話からすると、嘘をついているようだ。',
  'この結果からすれば、成功は難しいだろう。',
  '状況からすると、間違いないだろう。',

  // からみて (karamite) - "judging from the perspective of"
  '私の立場から見て、それは間違いだ。',
  '彼の表情から見て、満足しているようだ。',

  // からといって (karatotte) - "just because... doesn't mean..."
  '高いからといって、品質が良いとは限らない。',
  '日本に住んでいるからといって、日本語ができるとは限らない。',

  // Similar sounding but unrelated patterns
  // には as locative "in" + topic
  '日本には美味しいものがたくさんあります。',
  '私の町には公園がありません。',
  'この部屋にはテレビがあります。',

  // として (toshite) - "as"
  '学生として勉強する。',
  '友達として助ける。',

  // にしては (nishite) - "considering, for"
  '子供にしては詳しい。',
  '新人にしてはよくやっている。',
  'この製品は安いにしては品質が良い。',
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
