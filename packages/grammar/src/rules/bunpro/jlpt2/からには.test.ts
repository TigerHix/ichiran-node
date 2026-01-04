import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './からには.js';
import { BUNPRO_JLPT2 } from './index.js';

// Negative test cases - sentences that should NOT match the からには grammar rule
const negatives = [
  // から alone (without には or は) - "because" or "from"
  'あなたがいるから、安心できます。',
  '毎日努力をしたから、一番になれた。',
  '東京から大阪へ行く。',
  '8時から仕事を始めます。',
  '日本から来ました。',
  '昨日から雨が降っている。',

  // からして (judging from, based on)
  '彼の性格からして、無理だろう。',
  '名前からしてつまらなそうだ。',

  // からすると・からすれば (more objective judgment)
  '彼の話からすると、嘘をついているようだ。',
  'この結果からすれば、成功は難しいだろう。',

  // にしては (considering, for)
  '子供にしては詳しい。',
  '新人にしてはよくやっている。',

  // には alone (topic marker with emphasis)
  '東京には行ったことがない。',
  '彼には言えない。',

  // Similar but unrelated patterns
  // から + noun (from + place)
  '家から駅まで歩く。',

  // Verb + から + verb (from/to relationship)
  '大阪から来る人。',
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
