import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './てからでないと.js';
import { BUNPRO_JLPT2 } from './index.js';

// Negative test cases - sentences that should NOT match the てからでないと grammar rule
const negatives = [
  // てから (tekara) - "after doing" (without でないと/でなければ)
  '昼ご飯を食べてから、昼寝をします。',
  '家に帰ってから、シャワーを浴びます。',
  '大学を卒業してから、就職しました。',
  'よく考えてから、決めてください。',
  '彼が来てから、始めましょう。',

  // ないと (naito) - "if not" (without てからで)
  '勉強しないと、試験に合格できない。',
  '早く行かないと、遅れますよ。',
  'お金がないと、生活できない。',
  '雨が降らないと、花が咲かない。',

  // からして (karashite) - "judging from, even"
  '彼の態度からして、怒っているようだ。',
  'この店は名前からして高そうだ。',
  '親からして反対している。',

  // からすると (karasuruto) - "from the standpoint of"
  '彼の話からすると、嘘をついているようだ。',
  '状況からすると、間違いないだろう。',
  'この結果からすれば、成功は難しいだろう。',

  // Similar patterns that are NOT てからでないと
  // てから + positive (not negative condition)
  '薬を飲んでから、よく寝ました。',

  // Simple て + negative (without からでないと)
  '勉強して、試験に合格したい。',
  '早く起きて、宿題をしよう。',

  // でないと (denaito) without てから
  'これは本でないと、認められません。',
  '18歳以上でないと、入れません。',

  // から + negative (without て)
  '忙しいから、行けない。',
  '雨だから、試合がない。',

  // Negative with だけ (dake) - different pattern
  '練習してからだけ、試合に出られる。',
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
