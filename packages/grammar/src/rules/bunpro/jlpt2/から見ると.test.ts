import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './から見ると.js';
import { BUNPRO_JLPT2 } from './index.js';

// Negative test cases - sentences that should NOT match the から見ると grammar rule
const negatives = [
  // 見る alone (without から) - "to see"
  '映画を見ました。',
  '富士山が見える。',
  '私は彼を見たことがある。',

  // から alone (without 見る) - "because" or "from"
  'あなたがいるから、安心できます。',
  '東京から大阪へ行く。',
  '8時から仕事を始めます。',
  '日本から来ました。',

  // にしたら (nishitara) - more subjective/emphatic perspective
  '子供にしたら大問題だ。',
  '私にしたら、それは間違いだ。',

  // にしては (nishite) - "considering, for"
  '子供にしては詳しい。',
  '新人にしてはよくやっている。',

  // からして (karashite) - more subjective/emphatic judgment
  '彼の性格からして、彼と一緒に住むことは無理だろう。',
  '名前からしてつまらなそうだ。',

  // からすると・からすれば (karasuruto/karasureba) - more objective judgment
  '彼の話からすると、嘘をついているようだ。',
  'この結果からすれば、成功は難しいだろう。',

  // からいうと・からいえば (karaiuto/karaieba) - "speaking in terms of"
  '経験からいうとそれは無理だ。',
  '安全の点からいえば、この車は優れている。',

  // 見る with different particles (not から)
  '彼を見てもらう。',
  '私を見てください。',
  '彼を見ながら話す。',

  // Similar but unrelated patterns
  // 見 (mi) + other particles
  '山を見る。',
  '彼を見るたびに思い出す。',

  // から + other verbs (not 見る)
  '彼から聞いた。',
  '彼からもらった本。',
  '彼から送られたメール。',
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
