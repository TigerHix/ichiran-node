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
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
