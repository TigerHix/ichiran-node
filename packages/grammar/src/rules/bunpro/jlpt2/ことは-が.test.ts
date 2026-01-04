import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './ことは-が.js';
import { BUNPRO_JLPT2 } from './index.js';

// Negative test cases - sentences that should NOT match the ことは-が grammar rule
const negatives = [
  // こと alone (without は...が pattern)
  '大切なことを忘れた。',
  '何か言いたいことはありますか。',
  'それはいいことだ。',
  '彼のことが心配だ。',

  // Simple topic marker は (without こと)
  '漢字は読めますが、簡単な漢字しか読めないです。',
  'この家は広いですが、家具が多いから狭く見える。',

  // Simple contrastive が (without ことは pattern)
  '彼は来たが、彼女は来なかった。',
  '勉強したが、試験に落ちた。',

  // ことだ (simple assertion, not concession)
  '勉強することだ。',
  '健康第一にすることだ。',

  // ことにする (decide to)
  '毎日運動することにした。',
  '明日出発することにしよう。',

  // ことになる (it has been decided that)
  '来月日本に行くことになった。',
  '会議は明日開かれることになっている。',

  // ことになっている (it is arranged that)
  '授業は9時に始まることになっている。',
  'ここに駐車してはいけないことになっている。',

  // ことから (from the fact that)
  '彼が来なかったことから、何かあったのだろう。',
  '彼の表情からすると、試験は悪くなかったようだ。',

  // ことだから (given that, it is exactly because)
  '田中さんのことだから、今日も遅れてくるだろう。',
  '真面目な田中さんのことだから、約束は守るだろう。',

  // Different words before and after ことは (not the pattern)
  // The pattern requires the SAME word repeated

  // ことながら (although, but - different grammar)
  // Not a common pattern but could appear

  // Simple repetition with different particles
  '行くことは行く。', // Without が at the end
  'できることはできる。', // Without が at the end

  // Noun + は (topic) + Noun (no こと pattern)
  'これはペンです。',
  '彼は学生です。',

  // Adjective + は (topic) + Adjective (no こと pattern)
  'この部屋は広いです。',
  'その本は面白いです。',

  // Verb + は (topic) - ungrammatical, but testing anyway
  // '食べるは食べます。', // This would be ungrammatical Japanese

  // が as subject marker (not conjunction)
  '私が行きます。',
  '彼が来た。',

  // けど as informal conjunction (without ことは pattern)
  '明日は雨だけど、行きます。',
  '疲れたけど、仕事をした。',

  // が as sentence-ending particle (not conjunction)
  'どうしようかな。',
  'いいですね。',

  // けど as sentence-ending particle (not conjunction)
  'そう思うけど。',
  'ちょっと難しいけど。',

  // ことさ (informal explanatory)
  'どうでもいいことさ。',
  '気にしないことさ。',

  // ことか (emphatic expression)
  'どれほど待ち望んだことか。',
  'どれだけ苦労したことか。',

  // ことなしに (without)
  '努力することなしに成功はできない。',
  '彼の許可することなしに、入室できない。',

  // ことで (by means of, in the matter of)
  '話し合いで解決することで、合意した。',
  'メールで連絡することで、手間が省ける。',

  // ことを (object marker + noun)
  '彼が来たことを知らない。',
  '会議が終わったことを報告する。',
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
