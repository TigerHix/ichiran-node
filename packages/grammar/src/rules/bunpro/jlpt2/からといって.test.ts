import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './からといって.js';
import { BUNPRO_JLPT2 } from './index.js';

// Negative test cases - sentences that should NOT match the からといって grammar rule
const negatives = [
  // から alone (without といって) - "because" or "from"
  'あなたがいるから、安心できます。',
  '毎日努力をしたから、一番になれた。',
  '東京から大阪へ行く。',
  '8時から仕事を始めます。',
  '日本から来ました。',
  '彼からもらった。',
  '昨日から雨が降っている。',

  // と言って (to itte) - "called/say" (not the concessive pattern)
  'これは何と言う意味ですか。',
  '彼は行くと言った。',
  'そうと言って彼は去った。',

  // からして (karashite) - "judging from, even"
  '彼の性格からして、無理だろう。',
  '名前からしてつまらなそうだ。',

  // からすると・からすれば (karasuruto/karasureba) - more objective judgment
  '彼の話からすると、嘘をついているようだ。',
  'この結果からすれば、成功は難しいだろう。',
  '状況からすると、間違いないだろう。',

  // からして - different grammar
  '親からして反対している。',

  // Similar but unrelated patterns
  // だといって (datoitte) - different meaning
  // だ + といって without から

  // 単に + と言って (tanni + to itte) - different structure
  '単に試したと言って済む問題ではない。',

  // という (toiū) - "called/known as"
  'これは何という花ですか。',
  '東京という都市は大きい。',
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
