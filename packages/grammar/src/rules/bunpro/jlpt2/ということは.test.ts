import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './ということは.js';
import { BUNPRO_JLPT2 } from './index.js';

// Negative test cases - sentences that should NOT match the ということは grammar rule
const negatives = [
  // ということ (JLPT4) - just nominalization without topic marker
  // Ends with こと as a noun object, not marked with topic は
  '彼が来たということを知らなかった。',
  '彼が言ったことを忘れないでください。',
  'これは秘密だということを約束してください。',

  // というのは (JLPT3) - introduces definitions/reasons ("what is called X is Y")
  // Similar but different function - defining rather than drawing conclusion
  '寿司というのは、日本の代表的な料理です。',
  '禅というのは、仏教の一派です。',
  'この店でいう「並」というのは、一番小さいサイズのことです。',

  // ことだ (JLPT3) - giving advice "you should"
  '時間通りに来ることだ。',
  '練習することです。',
  '諦めないことだ。',

  // ことになる (it is decided that / it turns out that)
  '来月日本に行くことになった。',
  '会議は来週に延期されることになりました。',
  '彼が責任を取ることになるだろう。',

  // ことになっている (arrangement/rule)
  'この部屋では喫煙は禁止されていることになっている。',
  '試験は9時に始まることになっています。',
  '日本では車は左側を走ることになっている。',

  // ということではない (negation - "it doesn't mean that")
  '彼が来たということではない。',
  '意味がないということではない。',
  'お金がすべてではないということだ。',

  // ということだ (JLPT3) - hearsay/conclusion ("I hear that / it means that")
  // Different because it ends with copula だ rather than topic は
  '先生によると、この病気は薬では治せないということだ。',
  '人によって考え方が違うということだ。',
  '明日は雨だということだ。',

  // ということです (polite version of hearsay)
  '彼は来ないということです。',
  'この店は美味しいということです。',

  // Simple quotation という (called/named) without こと
  'これは何という花ですか。',
  '田中という人から電話がありました。',
  '彼は「さようなら」という言葉を残して去った。',

  // ということなく (without the fact that / without doing)
  '彼は不安ということなく試合に臨んだ。',
  '遅刻ということなく会議に参加した。',

  // こととして (as a matter of)
  'これを秘密のこととして扱う。',
  '例外のこととして認める。',

  // Simple topic は patterns (not related to quoting)
  '私は学生です。',
  '今日は良い天気ですね。',
  '彼は友達と一緒に行きました。',

  // というと (if we speak of / as for)
  '京都というと、古い寺が思い浮かぶ。',
  '彼というと、いつもあの事件のことを思い出す。',

  // といえば (speaking of which)
  '彼といえば、最近会っていません。',
  'カレーといえば、あの店が一番美味しいです。',

  // といった (such as / things like)
  'りんごといった果物が好きです。',
  '東京といった大都市は住みにくい。',

  // として (as / in the capacity of)
  '学生として参加する。',
  '彼は医者として働いている。',

  // としても (even as / even if)
  '冗談としても言い過ぎだ。',
  '富める者としても幸福とは限らない。',

  // ては (te form + wa - conditional emphasis)
  'そんなに高くては買えない。',
  'ここに住んではいけない。',

  // では (de wa - locative/topic)
  '東京では雨が降っている。',
  'この店ではパンを売っている。',
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
