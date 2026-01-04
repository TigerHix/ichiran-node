import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './からして.js';
import { BUNPRO_JLPT2 } from './index.js';

// Negative test cases - sentences that should NOT match the からして grammar rule
const negatives = [
  // から alone (without して) - "because" or "from"
  'あなたがいるから、安心できます。',
  '毎日努力をしたから、一番になれた。',
  '東京から大阪へ行く。',
  '8時から仕事を始めます。',
  '日本から来ました。',
  '彼からもらった。',
  '昨日から雨が降っている。',

  // からして as independent words (different meaning)
  // から (particle) + して (te-form of suru as "do")
  // Example: 何からして食べる (Eat starting from what?)
  // This is a legitimate parsing but different grammar

  // にしては (nishite) - "considering, for" (different nuance)
  '子供にしては詳しい。',
  '新人にしてはよくやっている。',
  'この製品は安いにしては品質が良い。',

  // からすると・からすれば (karasuruto/karasureba) - more objective judgment
  '彼の話からすると、嘘をついているようだ。',
  'この結果からすれば、成功は難しいだろう。',
  '状況からすると、間違いないだろう。',

  // からみて (karamite) - "judging from the perspective of"
  '私の立場から見て、それは間違いだ。',
  '彼の表情から見て、満足しているようだ。',

  // して (shite) alone - te-form of する
  '勉強してください。',
  '掃除して、寝ました。',
  'ご飯を作って、食べました。',

  // Similar sounding but unrelated patterns
  // がして (gashite) - different particle
  '頭がして痛い。',

  // からして followed immediately by noun (different grammar: suru verb + noun)
  // This is unlikely to occur naturally but keeping as placeholder
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
